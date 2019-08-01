	.text
	.global	add_bignum
	.type	add_bignum, @function
add_bignum:

    clc
	movq $0, %r8			/* counter */

	movl (%rdi), %r10d		/* length of a */
	movl (%rsi), %r11d		/* length of b */

	xorl %eax, %eax			/* CF flag */
	xorl %r9d, %r9d			/* previous CF flag*/
loop:
	cmp %r8d, %r10d
	je setsecond

	cmp %r8d, %r11d
	je handlerest

	mov 0x4(%rdi, %r8, 1), %bl
	mov 0x4(%rsi, %r8, 1), %cl
	
	add %r9b, %bl
	movl %eax, %r9d				
	xorl %eax, %eax				
	addb %bl, %cl
	adc $0, %eax			
	addl %eax, %r9d

	xorl %eax, %eax

	mov %cl, 0x4(%rdx, %r8, 1)
 
	add $1, %r8d
	jmp loop

setsecond:
	movl %r11d, %r10d
	movq %rsi, %rdi

handlerest:
	cmp %r8d, %r10d
	je checklast
	mov 0x4(%rdi, %r8, 1), %cl

	add %r9b, %cl
	adc $0, %eax

	movl %eax, %r9d

	xorl %eax, %eax

	mov %cl, 0x4(%rdx, %r8, 1)
	add $1, %r8d
	jmp handlerest

checklast:
	add %r9b, 0x4(%rdx, %r8, 1)
	adc $0, 0x5(%rdx, %r8, 1)	

    ret

	.size	add_bignum, . - add_bignum
