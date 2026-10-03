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
    xor ebp, ebp
    xor r10d, r10d
    lea r11, [rip + .Lresume]
    jmp scheme_main
.Lresume:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
    ret

// Return scheme_alloc(rax) in rax, preserving the Scheme argument registers,
// r10 and r11. rsp must be 16-byte aligned before the call, as in Scheme code.
.globl safe_scheme_alloc
safe_scheme_alloc:
    push rdi
    push rsi
    push rdx
    push rcx
    push r8
    push r9
    push r10
    push r11
    sub rsp, 8          # return address + 8 pushes: realign to 16
    mov rdi, rax
    call scheme_alloc
    add rsp, 8
    pop r11
    pop r10
    pop r9
    pop r8
    pop rcx
    pop rdx
    pop rsi
    pop rdi
    ret

// Allocate a frame
// Parameters:
//   - rax: length (in words) of the frame
//   - rbp: caller's frame
//   - r11: return address
//   - r10: procedure object
// Output:
//   - rbp: the new frame
// Clobbers: rax, r11
.globl alloc_frame
alloc_frame:
    push rcx            # also realigns rsp to 16 for safe_scheme_alloc
    mov rcx, [rip + scheme_cap]
    sub rcx, [rip + scheme_len]
    cmp rcx, rax
    jb .Lslow
    mov rcx, [rip + scheme_len]
    add [rip + scheme_len], rax
    shl rcx, 3
    add rcx, [rip + scheme_mem]
.Linit:
    mov [rcx + 8], rax
    mov [rcx + 16], rbp
    mov [rcx + 24], r11
    mov [rcx + 32], r10
    lea r11, [rip + frame_type]
    mov [rcx], r11
    mov rbp, rcx
    pop rcx
    ret
.Lslow:
    mov rcx, rax
    call safe_scheme_alloc
    xchg rax, rcx
    jmp .Linit

.section .note.GNU-stack,"",@progbits
