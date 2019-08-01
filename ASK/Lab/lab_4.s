read = 0
write = 1
exit  = 60

        .global _start
        .type _start, @function
        .section .text


_start: 
    xorq %r10, %r10

    movq $0, %rdi
    movq $1, %rdx
read2:
    movq $read, %rax
    sub $1, %rsp
    addq $1, %r10
    movq %rsp, %rsi
    syscall
    movq (%rsp), %rbx
    test %rbx, %rbx
    jz write2
     
    jmp read2

write2:
    subq $8, %r10
    add $8, %rsp
    movq $write, %rax
    movq $1, %rdi
    movq %r10, %rdx
    movq %rsp, %rsi
    syscall

    movq    $exit, %rax
    movq    $0, %rdi
    syscall
        
    .size   _start, . - _start
