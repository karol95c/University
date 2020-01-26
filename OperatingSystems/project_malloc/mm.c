/*
 * Karol Cidyło 292419
 * My solution is based on boundary tags with headers and footers,
 * free blocks have also encoded pointers to prev and next free blocks
 *
 * Minimum size of block is 16, sizes of block are rounded up to multiple of 16
 *
 * Header and footer - 4 bytes
 * [ssssssss|ssssssss|ssssssss|sssss00a]
 * s - size bit
 * a - allocation flag (1 - block is allocated, 0 - block is free)
 * we don't need last 3 bits for size(because size is rounded up to 16
 *
 * Free blocks:
 * header(4bytes) | [ next free(4bytes) | prev free (4 bytes) ] | footer(4bytes)
 *
 * Allocated blocks:
 * header(4bytes) | [                 payload                 ] | footer (4
 * bytes)
 *
 * There is implemented decoding end encoding addresses of next and prev free
 * blocks In free blocks offsets of addresses are saved (offsets related to
 * heap_listp which is initialized in mm_init). So addresses are encoded to 4
 * byte uint32_t.
 *
 * Free list root pointer is initialized in mm_init.
 *
 * Finding free blocks are based on mixed next-fit and best-fit algorithms.
 * There is saved last_searched free block. Next find_free will start from this
 * block. But algorithm still search best fit block. Newly allocated blocks are
 * removed from list.
 *
 * In adding block to free blocks list coalesce function check if it can be
 * extended of sizes of adjacent free blocks (algorithm do it earnestly). Newly
 * added free block is set at the beggining of the list.
 *
 *
 */
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "memlib.h"
#include "mm.h"

/* If you want debugging output, use the following macro.  When you hand
 * in, remove the #define DEBUG line. */
#define DEBUG
#ifdef DEBUG
#define debug(...) printf(__VA_ARGS__)
#else
#define debug(...)
#endif

/* do not change the following! */
#ifdef DRIVER
/* create aliases for driver tests */
#define malloc mm_malloc
#define free mm_free
#define realloc mm_realloc
#define calloc mm_calloc
#endif /* def DRIVER */

/* Macros used from CSAPP book. */
/* Basic constants and macros */
#define WORDSIZE 4           /* word size (bytes) */
#define DSIZE (2 * WORDSIZE) /* doubleword size (bytes) */
#define CHUNKSIZE (1 << 7)   /* initial heap size (bytes) */

#define MAX(x, y) ((x) > (y) ? (x) : (y))

/* Pack a size and allocated bit into a word */
#define PACK(size, alloc) ((size) | (alloc))

/* Read and write a word at address p */
#define GET(p) (*(unsigned int *)(p))
#define PUT(p, val) (*(unsigned int *)(p) = (val))

/* Read the size and allocated fields from address p */
#define GET_SIZE(p) (GET(p) & ~0x7)
#define GET_ALLOC(p) (GET(p) & 0x1)

/* Given block ptr bp, compute address of its header and footer */
#define HDRP(bp) ((char *)(bp)-WORDSIZE)
#define FTRP(bp) ((char *)(bp) + GET_SIZE(HDRP(bp)) - DSIZE)

/* Given block ptr bp, compute address of next and previous blocks */
#define NEXT_BLKP(bp) ((char *)(bp) + GET_SIZE(((char *)(bp)-WORDSIZE)))
#define PREV_BLKP(bp) ((char *)(bp)-GET_SIZE(((char *)(bp)-DSIZE)))

#define MIN_BLKSIZE 16

/* My macros fo free blocks */
/* Write encoded address to proper space of block */
#define WRITE_ADDR_TO(p, val)                                                  \
  (*(uint32_t *)(p) = (uint32_t)(unsigned long)(val))
#define NEXT_FREE_PTR(bp) ((uint32_t *)((char *)(bp)))
#define PREV_FREE_PTR(bp) ((uint32_t *)((char *)(bp) + WORDSIZE))

static char *heap_listp;
static char *free_listp;
static char *last_search;

#define VERBOSE 0
static size_t round_up(size_t size) {
  return (size + ALIGNMENT - 1) & -ALIGNMENT;
}

/* encode_ptr - get offset of pointer related to heap_listp (heap start) */
static inline uint32_t encode_ptr(void *bp) {
  if (!bp) {
    return 0;
  }
  uint32_t encoded = (char *)bp - heap_listp;
  return encoded;
}

/*
 * decode_ptr - get pointer from offset at given bp pointer
 */
static inline void *decode_ptr(void *bp) {
  uint32_t offset = *(uint32_t *)bp;
  if (offset == 0) {
    return NULL;
  }
  void *decoded = heap_listp + offset;
  return decoded;
}

/*
 * next_free_block_addr - return decoded address of next free block
 * on free blocks list
 */
static inline void *next_free_block_addr(void *bp) {
  return decode_ptr(NEXT_FREE_PTR(bp));
}

/*
 * prev_free_block_addr - return decoded address of previous free block
 * on free blocks list
 */
static inline void *prev_free_block_addr(void *bp) {
  return decode_ptr(PREV_FREE_PTR(bp));
}

/*
 * set_nextptr - encode and set next free pointer on free blocks list
 */
static inline void set_nextptr(void *ptr, void *next) {
  WRITE_ADDR_TO(NEXT_FREE_PTR(ptr), encode_ptr(next));
}

/*
 * set_prevptr - encode and set previous free pointer on free blocks list
 */
static inline void set_prevptr(void *ptr, void *prev) {
  WRITE_ADDR_TO(PREV_FREE_PTR(ptr), encode_ptr(prev));
}

/*
 * prints block information
 */
void print_block_info(void *bp) {
  if (!VERBOSE) {
    return;
  }
  size_t size = GET_SIZE(HDRP(bp));
  int32_t alloc = GET_ALLOC(HDRP(bp));

  if (alloc) {
    printf("block = %p, size = %ld, next_block = %p, prev_block = %p\n", bp,
           size, NEXT_BLKP(bp), PREV_BLKP(bp));
  } else {
    printf("block = %p, size = %ld, next_block = %p, prev_block = %p, "
           "next_free = %p, prev_free = %p\n",
           bp, size, NEXT_BLKP(bp), PREV_BLKP(bp), next_free_block_addr(bp),
           prev_free_block_addr(bp));
  }
}

/*
 * add_to_free_list - add block at the beggining of free blocks list
 */
static inline void add_to_free_list(void *bp) {
  if (!free_listp) {
    free_listp = bp;
    set_nextptr(free_listp, NULL);
    set_prevptr(free_listp, NULL);
    return;
  }

  void *root = free_listp;
  // set_nextptr(bp, next);
  set_nextptr(bp, root);
  set_prevptr(root, bp);
  set_prevptr(bp, NULL);
  free_listp = bp;
}

/*
 * remove_from_free_list - remove block from free blocks list
 * set proper next and previous free blocks for adjacent free blocks
 * on list
 */
static inline void remove_from_free_list(void *bp) {
  void *next = next_free_block_addr(bp);
  void *prev = prev_free_block_addr(bp);
  if (bp == last_search) {
    last_search = prev_free_block_addr(bp);
  }
  if (prev) {
    set_nextptr(prev, next);
  } else {
    /* it means that pointer was root */
    free_listp = next;
    if (next) {
      set_prevptr(next, NULL);
    }
    return;
  }

  if (next) {
    set_prevptr(next, prev);
  } else {
    set_nextptr(prev, NULL);
  }
  set_nextptr(bp, NULL);
  set_prevptr(bp, NULL);
}

/*
 * coalesce_next_free - coalesce free blocks when next block is free
 */
static inline void coalesce_next_free(size_t size, void *bp) {
  remove_from_free_list(NEXT_BLKP(bp));
  PUT(HDRP(bp), PACK(size, 0));
  PUT(FTRP(bp), PACK(size, 0));
  add_to_free_list(bp);
}

/*
 * coalesce_prev_free - coalesce free blocks when previous block is free
 */
static inline void coalesce_prev_free(size_t size, void *bp) {
  remove_from_free_list(PREV_BLKP(bp));
  PUT(FTRP(bp), PACK(size, 0));
  PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
  add_to_free_list(PREV_BLKP(bp));
}

/*
 * coalesce_two_free - coalesce free blocks where next and previous
 * adjacent blocks are free
 */
static inline void coalesce_two_free(size_t size, void *bp) {
  remove_from_free_list(NEXT_BLKP(bp));
  remove_from_free_list(PREV_BLKP(bp));
  PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
  PUT(FTRP(NEXT_BLKP(bp)), PACK(size, 0));
  add_to_free_list(PREV_BLKP(bp));
}

/*
 * coalesce - find case of possible coalescing of free blocks and apply
 */
static inline void *coalesce(void *bp) {
  unsigned int prev_allocated = GET_ALLOC(FTRP(PREV_BLKP(bp)));
  unsigned int next_allocated = GET_ALLOC(HDRP(NEXT_BLKP(bp)));
  size_t size = GET_SIZE(HDRP(bp));
  if (VERBOSE) {
    printf("coalesce ");
  }
  if (prev_allocated && next_allocated) {
    if (VERBOSE) {
      printf("case 1\n");
    } /* Case 1 */
    add_to_free_list(bp);
    return bp;
  }

  else if (prev_allocated && !next_allocated) {
    if (VERBOSE) {
      printf("case 2\n");
    } /* Case 2 */
    size += GET_SIZE(HDRP(NEXT_BLKP(bp)));
    coalesce_next_free(size, bp);
    return (bp);
  }

  else if (!prev_allocated && next_allocated) {
    if (VERBOSE) {
      printf("case 3\n");
    } /* Case 3 */
    size += GET_SIZE(HDRP(PREV_BLKP(bp)));
    coalesce_prev_free(size, bp);
    return (PREV_BLKP(bp));
  }

  else { /* Case 4 */
    if (VERBOSE) {
      printf("case 4\n");
    }
    size += GET_SIZE(HDRP(PREV_BLKP(bp))) + GET_SIZE(FTRP(NEXT_BLKP(bp)));
    coalesce_two_free(size, bp);
    return (PREV_BLKP(bp));
  }
}

/*
 * extend_heap - add more heap space
 */
static inline void *extend_heap(size_t words) {
  if (VERBOSE) {
    printf("extend_heap\n");
  }
  void *bp;
  size_t size;

  /* Allocate an even number of words to maintain alignment */
  size = (words % 2) ? (words + 1) : words;
  size *= WORDSIZE;
  if ((bp = mem_sbrk(size)) == (void *)-1)
    return NULL;
  /* Prologue header and footer */
  PUT(HDRP(bp), PACK(size, 0)); /* free block header */
  PUT(FTRP(bp), PACK(size, 0)); /* free block footer */
                                /* epilogue header */
  PUT(HDRP(NEXT_BLKP(bp)), PACK(0, 1));

  return coalesce(bp);
}

/*
 * mm_init - Called when a new trace starts.
 */
int mm_init(void) {
  // code copied from CSAPP
  if ((heap_listp = mem_sbrk(4 * WORDSIZE)) == (void *)-1)
    return -1;
  void *heap;
  PUT(heap_listp, 0);
  /* Alignment padding */
  PUT(heap_listp + (1 * WORDSIZE), PACK(DSIZE, 1)); /* Prologue header */
  PUT(heap_listp + (2 * WORDSIZE), PACK(DSIZE, 1)); /* Prologue footer */
  PUT(heap_listp + (3 * WORDSIZE), PACK(0, 1));     /* Epilogue header */
  // heap_listp += (2 * WORDSIZE);

  /* Extend the empty heap with a free block of CHUNKSIZE bytes */
  free_listp = NULL;
  last_search = NULL;
  heap = extend_heap(CHUNKSIZE / WORDSIZE);
  if (heap == NULL)
    return -1;

  // heap_listp += WORDSIZE;
  // dbg_printf("mm_init heap_listp %lx > ", (long)heap_listp);

  return 0;
}

/*
 * adjust_size - check if is possible to save space from
 * allocated block and make new free block if it's true.
 */
static inline void adjust_size(void *bp, size_t size) {
  void *next = NULL;
  print_block_info(bp);
  size_t old_size = GET_SIZE(HDRP(bp));
  size_t new_size = old_size - size;
  if (new_size < MIN_BLKSIZE) {
    remove_from_free_list(bp);
    PUT(HDRP(bp), PACK(old_size, 1));
    PUT(FTRP(bp), PACK(old_size, 1));

    return;
  }

  remove_from_free_list(bp);
  PUT(HDRP(bp), PACK(size, 1));
  PUT(FTRP(bp), PACK(size, 1));

  next = NEXT_BLKP(bp);

  PUT(HDRP(next), PACK(old_size - size, 0));
  PUT(FTRP(next), PACK(new_size, 0));
  add_to_free_list(next);
  mm_checkheap(VERBOSE);
}

/*
 * find_free - find free block with sufficient size
 * function start search from last searched block
 * and try to fit best sufficient size
 * After job, function saves last_searched block pointer
 * for future search.
 */
static inline void *find_free(size_t size) {
  if (!last_search) {
    last_search = free_listp;
  }
  if (!free_listp) {
    return NULL;
  }
  void *next = last_search;
  void *best = NULL;
  size_t min = SIZE_MAX;
  size_t cand_size;
  while (next) {
    cand_size = GET_SIZE(HDRP(next));
    if (cand_size >= size) {
      if (cand_size - size <= MIN_BLKSIZE) {
        last_search = prev_free_block_addr(next);
        return next;
      }
      if (cand_size - size < min && cand_size - size > MIN_BLKSIZE) {
        best = next;
        min = cand_size - size;
      }
    }
    next = next_free_block_addr(next);
    if (next == NULL) {
      next = free_listp;
    }
    if (next == last_search) {
      next = NULL;
    }
  }
  if (best) {
    last_search = prev_free_block_addr(best);
  }

  return best;
}

/*
 * malloc - allocate block of requested size rounded up to multiply of 16
 *  or allocate block of MIN_BLKSIZE if requested size is less
 *  When free block from free block list cannot satisfy size
 *  malloc extend heap space
 */
void *malloc(size_t size) {
  size_t new_size;

  void *bp;
  size_t extend_size;

  new_size = MAX(round_up(size + MIN_BLKSIZE), MIN_BLKSIZE);
  if (new_size <= 0) {
    return NULL;
  }

  bp = find_free(new_size);
  if (bp) {
    adjust_size(bp, new_size);
    return bp;
  }

  extend_size = MAX(new_size, CHUNKSIZE / 8);
  if ((bp = extend_heap(extend_size / WORDSIZE)) == NULL)
    return NULL;

  adjust_size(bp, new_size);

  return bp;
}

/*
 * free - add block to free block list and coalesce if possible
 */
void free(void *ptr) {
  if (VERBOSE) {
    printf("FREE: %p\n", ptr);
  }

  if (!ptr || !GET_ALLOC(HDRP(ptr)))
    return;
  size_t size = GET_SIZE(HDRP(ptr));

  PUT(HDRP(ptr), PACK(size, 0));
  PUT(FTRP(ptr), PACK(size, 0));
  coalesce(ptr);
}

/*
 * realloc - more comments in code
 **/
void *realloc(void *old_ptr, size_t size) {
  size_t old_size;
  size_t new_size;
  void *next;

  /* If size == 0 then this is just free, and we return NULL. */
  if (size == 0) {
    free(old_ptr);
    return NULL;
  }

  /* If old_ptr is NULL, then this is just malloc. */
  if (!old_ptr)
    return malloc(size);

  old_size = GET_SIZE(HDRP(old_ptr));
  new_size = MAX(round_up(size + MIN_BLKSIZE), MIN_BLKSIZE);
  if (old_size > new_size) {
    /* if size is smaller and diffrence is bigger than MIN_BLKSIZE
     * we just add remainder as free block
     */
    if (old_size - new_size <= MIN_BLKSIZE) {
      return old_ptr;
    }
    PUT(HDRP(old_ptr), PACK(new_size, 1));
    PUT(FTRP(old_ptr), PACK(new_size, 1));

    next = NEXT_BLKP(old_ptr);

    PUT(HDRP(next), PACK(old_size - new_size, 0));
    PUT(FTRP(next), PACK(old_size - new_size, 0));
    add_to_free_list(next);

    return old_ptr;
  } else if (old_size < new_size) {
    void *next_ptr = NEXT_BLKP(old_ptr);
    size_t next_size = GET_SIZE(HDRP(next_ptr));

    /* check if next free, if it is we can just extend block */
    if (!GET_ALLOC(HDRP(next_ptr)) && (old_size + next_size >= new_size)) {
      PUT(HDRP(old_ptr), PACK(next_size + old_size, 1));
      PUT(FTRP(old_ptr), PACK(next_size + old_size, 1));
      remove_from_free_list(next_ptr);
      return old_ptr;
    }

    /* if next is not free or space is not sufficient we need
     * to allocate new block and free previous
     */
    void *new_ptr = malloc(new_size);
    memcpy(new_ptr, old_ptr, old_size);
    free(old_ptr);
    return new_ptr;
  }
  return old_ptr;
}

/*
 * calloc - Allocate the block and set it to zero.
 */
void *calloc(size_t nmemb, size_t size) {
  size_t bytes = nmemb * size;
  void *new_ptr = malloc(bytes);

  /* If malloc() fails, skip zeroing out the memory. */
  if (new_ptr)
    memset(new_ptr, 0, bytes);

  return new_ptr;
}

static void check_free_list(int verbose) {
  void *next;
  void *next_prev;
  void *current;
  void *prev;
  if (!free_listp) {
    if (verbose) {
      printf("Start of free list is null\n");
    }
    return;
  }
  assert(GET_SIZE(HDRP(free_listp)) > 0);

  next = free_listp;
  current = free_listp;
  prev = prev_free_block_addr(next);
  next = next_free_block_addr(next);
  while (next) {
    size_t size = GET_SIZE(HDRP(next));
    assert(size >= MIN_BLKSIZE);
    next_prev = prev_free_block_addr(next);
    if (verbose) {
      // printblock(current);
      printf("current = %p, size = %ld, next = %p, prev = %p, next_prev = %p\n",
             current, size, next, prev, next_prev);
    }
    if (next_prev) {
      assert(current == next_prev);
    }
    if (prev) {
      assert(current != prev);
    }
    assert(current != next);
    prev = prev_free_block_addr(next);
    current = next;
    next = next_free_block_addr(next);
    if (verbose) {
      printf("----------------------------------------------\n");
    }
  }
}

/*
 * mm_checkheap - So simple, it doesn't need a checker!
 */
void mm_checkheap(int verbose) { check_free_list(verbose); }
