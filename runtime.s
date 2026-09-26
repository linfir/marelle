.intel_syntax noprefix
.text

.globl scheme_main_stub
scheme_main_stub:
    push rbx
    push rbp
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8
    call scheme_main
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
    ret

.section .note.GNU-stack,"",@progbits
