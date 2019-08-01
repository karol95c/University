	.text
	.global	insert_sort
	.type	insert_sort, @function

insert_sort:

    cmp %rdi, %rsi
    je exit
    movq %rdi, %rbx
    movq %rdi, %rcx
    addq $8, %rcx
    movq %rcx, %r9

process:
    subq $8, %r9
    movq (%r9), %r10
    cmp (%rcx), %r10
    jle process_next
    movq (%rcx), %r8
    movq %r10, (%rcx)
    movq %r8, (%r9)
    subq $8, %rcx
    cmp %rcx, %rdi
    je process_next
    jmp process

process_next:
    cmp %rbx, %rsi
    je exit
    addq $8, %rbx
    movq %rbx, %rcx
    movq %rbx, %r9
    jmp process

exit:
    ret

	.size	insert_sort, . - insert_sort
