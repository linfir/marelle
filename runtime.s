.intel_syntax noprefix
.text

// Enter Scheme from C, following the System V ABI: call the Scheme function
// scheme_main in the Scheme calling convention and return its value
.globl scheme_enter
scheme_enter:
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

// Allocate an object
// Parameters:
//   - rax: length (in words) of the object
//   - rbp: current frame, the only root
//   - rsp: 8 (mod 16), i.e. aligned before the call
// Output:
//   - rax: the object
//   - rbp: the current frame, moved if a collection happened
// Clobbers: rdi, rsi, rdx, rcx, r8, r9, r10, r11
.globl alloc_object
alloc_object:
    push rbp            # roots[0], also realigns rsp to 16
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call scheme_alloc
    pop rbp
    ret

// Allocate a frame
// Parameters:
//   - rax: length (in words) of the frame
//   - rbp: caller's frame
//   - r11: return address
//   - r10: procedure object, or 0 for scheme_main
//   - argument registers, as many as the procedure's arity
//   - rsp: 8 (mod 16), i.e. aligned before the call
// Output:
//   - rbp: the new frame
//   - r10 and argument registers, moved if a collection happened
// Clobbers: rax, r11
.globl alloc_frame
alloc_frame:
    push rcx            # also realigns rsp to 16
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
    mov rcx, [rsp]      # argument 4
    push r11
    push rax
    push r9             # roots: rbp, r10, then the arguments
    push r8
    push rcx
    push rdx
    push rsi
    push rdi
    push r10
    push rbp
    mov rdi, rax
    mov rsi, rsp
    mov edx, 2
    test r10, r10
    jz .Lroots
    add rdx, [r10 + 8]  # arity
.Lroots:
    call scheme_alloc
    pop rbp
    pop r10
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop r8
    pop r9
    mov [rsp + 16], rcx # replaces the copy popped by .Linit
    mov rcx, rax
    pop rax
    pop r11
    jmp .Linit

.section .note.GNU-stack,"",@progbits
