global WinMain
extern ExitProcess
section .text
WinMain:

    ; Create variable x

    ; Load literal
    mov ax, 255
    push ax

    ; Create variable y

    ; Push variable of type Boolean to the top of the stack
    push WORD [rsp + 0]

    ; Invert boolean
    pop ax
    not al
    push ax

    ; Create variable z

    ; Push variable of type Boolean to the top of the stack
    push WORD [rsp + 2]

    ; Push variable of type Boolean to the top of the stack
    push WORD [rsp + 2]

    ; Do logical xor
    pop bx
    pop ax
    xor ax, bx
    push ax

    ; Invert boolean
    pop ax
    not al
    push ax

    ; Push variable of type Boolean to the top of the stack
    push WORD [rsp + 0]

    ; Exit statement
    pop rcx
    call ExitProcess

    ; Default exit statement
    xor rcx, rcx
    call ExitProcess
