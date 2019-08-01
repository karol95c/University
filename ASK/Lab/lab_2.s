
	.text
	.global	clz
	.type	clz, @function

/* Solution idea - divide word to left and right part,
if left is zero it means there is no 1 in this part.
    Add number of bits this part has.
Next step i to divide right part for two and do the same.
When right part is not zero then we need to check left part
to get know prefix length.*/


clz:
    xorl %eax, %eax
    movq %rdi, %rcx
    movq %rdi, %r9

check32:
    shrq $32, %rcx
    test %ecx, %ecx
    jnz check16
    add $32, %eax
    movl %r9d, %ecx

check16:
    movl %ecx, %r9d
    shrl $16, %ecx
    andl magic1, %ecx
    test %ecx, %ecx
    jnz check8
    add $16, %eax
    movl %r9d, %ecx
    
check8:
    movl %ecx, %r9d
    shrl $8, %ecx
    andl magic2, %ecx
    test %ecx, %ecx
    jnz check4
    add $8, %eax
    movl %r9d, %ecx

    
check4:
    movl %ecx, %r9d
    shrl $4, %ecx
    andl magic3, %ecx
    test %ecx, %ecx
    jnz check2
    add $4, %eax
    movl %r9d, %ecx

check2:
    movl %ecx, %r9d
    shrl $2, %ecx
    andl magic4, %ecx
    test %ecx, %ecx
    jnz check1
    add $2, %eax
    movl %r9d, %ecx

check1:
    movl %ecx, %r9d
    shrl $1, %ecx
    andl magic1, %ecx
    test %ecx, %ecx
    jnz checklast
    add $1, %eax
    movl %r9d, %ecx

checklast:
    andl magic1, %ecx
    test %ecx, %ecx
    jnz done
    add $1, %eax

done:
    ret

	.size	clz, .-clz

.data
magic1:     .quad   0xFFFF
magic2:     .quad   0xFF
magic3:     .quad   0xF
magic4:     .quad   0x3
magic5:     .quad   0x1
