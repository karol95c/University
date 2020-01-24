/*
 * mm-naive.c - The fastest, least memory-efficient malloc package.
 *
 * In this naive approach, a block is allocated by simply incrementing
 * the brk pointer.  Blocks are never coalesced or reused.  The size of
 * a block is found at the first aligned word before the block (we need
 * it for realloc).
 *
 * This code is correct and blazingly fast, but very bad usage-wise since
 * it never frees anything.
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

/* Basic constants and macros */
#define WSIZE 4             /* word size (bytes) */
#define DSIZE 8             /* doubleword size (bytes) */
#define CHUNKSIZE (1 << 12) /* initial heap size (bytes) */
#define OVERHEAD 16         /* overhead of header and footer (bytes) */
#define PTR_SIZE 8

#define MAX(x, y) ((x) > (y) ? (x) : (y))

/* Pack a size and allocated bit into a word */
#define PACK(size, alloc) ((size) | (alloc))

/* Read and write a word at address p */
#define GET(p) (*(unsigned int *)(p))
#define PUT(p, val) (*(unsigned int *)(p) = (val))
// #define PUT_ADDR(p, addr) (*(uint64_t *)(p) = (addr))
#define PUT_ADDR(p, val) (*(int *)(p) = (int)(long)(val))

// #define PUT_ADDR(p, val)    (*(int *)(p) = (int)(long)(val))

/* Read the size and allocated fields from address p */
#define GET_SIZE(p) (GET(p) & ~0x7)
#define GET_ALLOC(p) (GET(p) & 0x1)

/* Given block ptr bp, compute address of its header and footer */
#define HDRP(bp) ((char *)(bp)-WSIZE)
#define FTRP(bp) ((char *)(bp) + GET_SIZE(HDRP(bp)) - DSIZE)

/* Given block ptr bp, read address of its next/prev free block pointer */
#define NEXTP(bp) ((int *)((char *)(bp)))
#define PREVP(bp) ((int *)((char *)(bp) + WSIZE))

/* Given block ptr bp, compute address of next and previous blocks */
#define NEXT_BLKP(bp) ((char *)(bp) + GET_SIZE(((char *)(bp)-WSIZE)))
#define PREV_BLKP(bp) ((char *)(bp)-GET_SIZE(((char *)(bp)-DSIZE)))

#define MIN_BLKSIZE 24

static char *heap_listp;
static char *free_listp;

typedef struct {
  int32_t header;
  /*
   * We don't know what the size of the payload will be, so we will
   * declare it as a zero-length array.  This allow us to obtain a
   * pointer to the start of the payload.
   */
  uint8_t payload[];
} block_t;

#define VERBOSE 0
static size_t round_up(size_t size) {
  return (size + ALIGNMENT - 1) & -ALIGNMENT;
}

static inline void *offset2addr(int offset) {
  if (offset) {
    return (void *)((long)offset | 0x800000000);
  } else {
    return NULL;
  }
}

static inline void *next_free_blck(void *bp) {
  int offset = *NEXTP(bp);
  return offset2addr(offset);
}

static inline void *prev_free_blck(void *bp) {
  int offset = *PREVP(bp);
  return offset2addr(offset);
}

// static size_t get_size(block_t *block) { return block->header & -4; }

static inline void set_nextptr(void *ptr, void *next) {
  PUT_ADDR(NEXTP(ptr), next);
  // char_ptr = (char *)next;
  // (void) char_ptr;
}
static inline void set_prevptr(void *ptr, void *prev) {
  // char *char_ptr = ((char *)ptr);
  // char_ptr = (char *)next;
  // (void) char_ptr;
  PUT_ADDR(PREVP(ptr), prev);
}

static inline bool is_epilogue(void *ptr) {
  return GET_ALLOC(HDRP(ptr)) == 1 && GET_SIZE(HDRP(ptr)) == 0;
}

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
           bp, size, NEXT_BLKP(bp), PREV_BLKP(bp), next_free_blck(bp),
           prev_free_blck(bp));
  }
}

static inline void add_to_free_list(void *bp) {
  if (!free_listp) {
    free_listp = bp;
    set_nextptr(free_listp, NULL);
    set_prevptr(free_listp, NULL);
    return;
  }

  assert(free_listp != bp);
  void *root = free_listp;
  // set_nextptr(bp, next);
  set_nextptr(bp, root);
  set_prevptr(root, bp);
  set_prevptr(bp, NULL);
  free_listp = bp;

  // while (nextp != NULL && GET_SIZE(HDRP(nextp)) < size){
  //   prevp = nextp;
  //   nextp = NEXTP(nextp);
  // }
  // for (; nextp != NULL && GET_SIZE(HDRP(nextp)) < size; prevp = nextp, nextp
  // = (char *)next_free_blck(nextp)) {
  // }

  // PUT_ADDR(NEXTP(bp), nextp);
  // PUT_ADDR(PREVP(bp), prevp);
  // PUT_ADDR(NEXTP(prevp), bp);
  // if (nextp != NULL) {
  //     PUT_ADDR(PREVP(nextp), bp);
  // }
  mm_checkheap(VERBOSE);
}

static inline void remove_from_free_list(void *bp) {
  void *next = next_free_blck(bp);
  void *prev = prev_free_blck(bp);
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
  mm_checkheap(VERBOSE);
}

static inline void coalesce_next_free(size_t size, void *bp) {
  remove_from_free_list(NEXT_BLKP(bp));
  PUT(HDRP(bp), PACK(size, 0));
  PUT(FTRP(bp), PACK(size, 0));
  add_to_free_list(bp);
}

static inline void coalesce_prev_free(size_t size, void *bp) {
  remove_from_free_list(PREV_BLKP(bp));
  PUT(FTRP(bp), PACK(size, 0));
  PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
  add_to_free_list(PREV_BLKP(bp));
}

static inline void coalesce_two_free(size_t size, void *bp) {
  remove_from_free_list(NEXT_BLKP(bp));
  remove_from_free_list(PREV_BLKP(bp));
  PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
  PUT(FTRP(NEXT_BLKP(bp)), PACK(size, 0));
  add_to_free_list(PREV_BLKP(bp));
}
/*
 * coalesce - boundary tag coalescing. Return ptr to coalesced block
 */
static inline void *coalesce(void *bp) {
  print_block_info(bp);
  unsigned int prev_alloc = GET_ALLOC(FTRP(PREV_BLKP(bp)));
  unsigned int next_alloc = GET_ALLOC(HDRP(NEXT_BLKP(bp)));
  size_t size = GET_SIZE(HDRP(bp));
  if (VERBOSE) {
    printf("coalesce ");
  }
  // dbg_printblock(bp);
  if (prev_alloc && next_alloc) {
    if (VERBOSE) {
      printf("case 1\n");
    } /* Case 1 */
    add_to_free_list(bp);
    return bp;
  }

  else if (prev_alloc && !next_alloc) {
    if (VERBOSE) {
      printf("case 2\n");
    } /* Case 2 */
    size += GET_SIZE(HDRP(NEXT_BLKP(bp)));
    coalesce_next_free(size, bp);
    return (bp);
  }

  else if (!prev_alloc && next_alloc) {
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
 * extend_heap - Extend heap with free block and return its block pointer
 */
static inline void *extend_heap(size_t words) {
  if (VERBOSE) {
    printf("extend_heap\n");
  }
  void *bp;
  size_t size;

  /* Allocate an even number of words to maintain alignment */
  size = (words % 2) ? (words + 1) * WSIZE : words * WSIZE;
  if ((bp = mem_sbrk(size)) == (void *)-1)
    return NULL;
  /* Initialize free block header/footer and the epilogue header */
  PUT(HDRP(bp), PACK(size, 0)); /* free block header */
  PUT(FTRP(bp), PACK(size, 0)); /* free block footer */

  PUT(HDRP(NEXT_BLKP(bp)), PACK(0, 1)); /* new epilogue header */

  /* Coalesce if the previous block was free */
  return coalesce(bp);
}

/*
 * mm_init - Called when a new trace starts.
 */
int mm_init(void) {
  // code copied from CSAPP
  if ((heap_listp = mem_sbrk(4 * WSIZE)) == (void *)-1)
    return -1;
  void *heap;
  PUT(heap_listp, 0);
  /* Alignment padding */
  PUT(heap_listp + (1 * WSIZE), PACK(DSIZE, 1)); /* Prologue header */
  PUT(heap_listp + (2 * WSIZE), PACK(DSIZE, 1)); /* Prologue footer */
  PUT(heap_listp + (3 * WSIZE), PACK(0, 1));     /* Epilogue header */
  heap_listp += (2 * WSIZE);

  /* Extend the empty heap with a free block of CHUNKSIZE bytes */
  free_listp = NULL;
  heap = extend_heap(CHUNKSIZE / WSIZE);
  if (heap == NULL)
    return -1;

  heap_listp += WSIZE;
  // dbg_printf("mm_init heap_listp %lx > ", (long)heap_listp);

  return 0;
}

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

static inline void *find_free(size_t size) {
  void *next = free_listp;
  while (next && size > GET_SIZE(HDRP(next))) {
    next = next_free_blck(next);
  }
  return next;
}

/*
 * malloc - Allocate a block by incrementing the brk pointer.
 *      Always allocate a block whose size is a multiple of the alignment.
 */
void *malloc(size_t size) {
  /*
   * Find free block to allocate memory, if prologue rached
   * more memory is needed.
   */
  size_t new_size;
  if (VERBOSE) {
    printf("malloc size: %ld ", size);
  }

  void *bp;
  size_t extend_size;

  new_size = MAX(round_up(size + MIN_BLKSIZE), MIN_BLKSIZE);
  if (new_size <= 0) {
    return NULL;
  }

  if (VERBOSE) {
    printf("rounded: %ld\n", new_size);
  }

  bp = find_free(new_size);
  if (bp) {
    adjust_size(bp, new_size);
    return bp;
  }

  extend_size = MAX(new_size, CHUNKSIZE / 8);
  if ((bp = extend_heap(extend_size / WSIZE)) == NULL)
    return NULL;

  adjust_size(bp, new_size);

  return bp;
}

/*
 * free - We don't know how to free a block.  So we ignore this call.
 *      Computers have big memories; surely it won't be a problem.
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
 * realloc - Change the size of the block by mallocing a new block,
 *      copying its data, and freeing the old block.
 **/
void *realloc(void *old_ptr, size_t size) {
  size_t old_size;
  size_t new_size;
  void *next;
  if (VERBOSE) {
    printf("REALLOC\n");
  }
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
    /* check if next free */
    void *next_ptr = NEXT_BLKP(old_ptr);
    size_t next_size = GET_SIZE(HDRP(next_ptr));

    if (!GET_ALLOC(HDRP(next_ptr)) && (old_size + next_size >= new_size)) {
      // size_t rescue = old_size + next_size - new_size;
      // if (rescue >= MIN_BLKSIZE)
      // {
      //   PUT(HDRP(next_ptr), PACK(next_size, 1));
      //   PUT(FTRP(next_ptr), PACK(next_size, 1));
      //   remove_from_free_list(next_ptr);
      //   PUT(HDRP(old_ptr), PACK(new_size, 1));
      //   PUT(FTRP(old_ptr), PACK(new_size, 1));
      //   next_ptr = NEXT_BLKP(old_ptr);
      //   PUT(HDRP(next_ptr), PACK(rescue, 0));
      //   PUT(FTRP(next_ptr), PACK(rescue, 0));
      //   add_to_free_list(next_ptr);
      //   return old_ptr;
      // }
      PUT(HDRP(old_ptr), PACK(next_size + old_size, 1));
      PUT(FTRP(old_ptr), PACK(next_size + old_size, 1));
      remove_from_free_list(next_ptr);
      return old_ptr;
    }
    void *new_ptr = malloc(new_size);
    memcpy(new_ptr, old_ptr, old_size);
    free(old_ptr);
    return new_ptr;
  }
  return old_ptr;

  /* If malloc() fails, the original block is left untouched. */
  // if (!new_ptr)
  //   return NULL;

  // /* Copy the old data. */
  // // block_t *block = old_ptr - offsetof(block_t, payload);
  // // size_t old_size = get_size(block);
  // // if (size < old_size)
  // //   old_size = size;
  // // memcpy(new_ptr, old_ptr, old_size);

  // /* Free the old block. */
  // free(old_ptr);

  // return new_ptr;
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

/*
 * mm_checkheap - So simple, it doesn't need a checker!
 */

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
  prev = prev_free_blck(next);
  next = next_free_blck(next);
  while (next) {
    size_t size = GET_SIZE(HDRP(next));
    assert(size >= MIN_BLKSIZE);
    next_prev = prev_free_blck(next);
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
    prev = prev_free_blck(next);
    current = next;
    next = next_free_blck(next);
    if (verbose) {
      printf("----------------------------------------------\n");
    }
  }
}
void mm_checkheap(int verbose) { check_free_list(verbose); }
