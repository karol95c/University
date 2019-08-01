/* Solution idea - change in order neighbour bits, next change in order
    two bits with neighbour two bits. Next steps 4 bits, 8 bits etc.
    Part of masks used are in data section, others are in code and copied
    to %rdx to reach memory limits. */

    .text
	.global	bitrev
	.type	bitrev, @function
bitrev:
    movq %rdi,%rax
    movq %rdi,%r9
    andq magic1, %rax
    shlq $1,%rax
    andq magic2, %r9
    shrq $1,%r9 
    orq %r9,%rax

    movq %rax,%r9
    andq magic3, %rax
    shlq $2,%rax
    andq magic4, %r9
    shrq $2,%r9 
    orq %r9,%rax

    movq %rax,%r9
    movq $0x0F0F0F0F0F0F0F0F, %rdx
    andq %rdx, %rax
    shlq $4,%rax
    movq $0xF0F0F0F0F0F0F0F0, %rdx
    andq %rdx, %r9
    shrq $4,%r9 
    orq %r9,%rax

    movq %rax,%r9
    movq $0x00FF00FF00FF00FF, %rdx
    andq %rdx, %rax
    shlq $8,%rax
    movq $0xFF00FF00FF00FF00, %rdx
    andq %rdx, %r9
    shrq $8,%r9 
    orq %r9,%rax

    movq %rax,%r9
    movq $0x0000FFFF0000FFFF, %rdx
    andq %rdx, %rax
    shlq $16,%rax
    movq $0xFFFF0000FFFF0000, %rdx
    andq %rdx, %r9
    shrq $16,%r9 
    orq %r9,%rax

    movq %rax,%r9
    movq $0x00000000FFFFFFFF, %rdx
    andq %rdx, %rax
    shlq $32,%rax
    movq $0xFFFFFFFF00000000, %rdx
    andq %rdx, %r9
    shrq $32,%r9 
    orq %r9,%rax

    ret
    .size	bitrev, .-bitrev
.data
magic1: .quad   0x5555555555555555
magic2: .quad   0xAAAAAAAAAAAAAAAA
magic3: .quad   0x3333333333333333
magic4: .quad   0xCCCCCCCCCCCCCCCC



	
