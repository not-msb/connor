.text
.globl syscall1
syscall1:
    pushq %rbp
    movq %rsp, %rbp
    movl %edi, %eax
    movl %esi, %edi
    syscall
    leave
    ret
.type syscall1, @function
.size syscall1, .-syscall1

.text
.globl exit
exit:
    pushq %rbp
    movq %rsp, %rbp
    movl $60, %eax
    syscall
    leave
    ret
.type exit, @function
.size exit, .-exit
