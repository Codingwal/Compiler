default rel
bits 64

extern GetStdHandle
extern WriteFile
extern ReadFile
extern ExitProcess
extern GetLastError

; get_output_handle()
%macro get_output_handle 0
    mov     rcx, -11            ; input device
    call    GetStdHandle
    mov [output_handle], rax
%endmacro

; get_input_handle()
%macro get_input_handle 0
    mov     rcx, -10            ; input device
    call    GetStdHandle
    mov [input_handle], rax
%endmacro

; write_to_console(buffer, numBytesToWrite, numBytesWrittenBuffer)
%macro write_to_console 3
    mov rcx, [output_handle]    ; handle
    lea rdx, [%1]               ; string buffer
    mov r8, %2                  ; number of bytes to write
    lea r9, [%3]                ; number of bytes written buffer
    sub rsp, 48                 ; shadow space (always reserve space for 4 params, even if 0 are used)
    call WriteFile              ; call WinAPI
    add rsp, 48                 ; remove shadow space
%endmacro

; read_from_console(buffer, numBytesToRead, numBytesReadBuffer)
%macro read_from_console 3
    mov rcx, [input_handle]     ; handle
    lea rdx, [%1]               ; string buffer
    mov r8, %2                  ; number of bytes to read
    lea r9, [%3]                ; number of bytes read buffer
    sub rsp, 48                 ; shadow space (always reserve space for 4 params, even if 0 are used)
    call ReadFile               ; call WinAPI
    add rsp, 48                 ; remove shadow space
%endmacro

; await_input(buffer, numBytesToRead, numBytesReadBuffer)
%macro await_input 3
%%await_input_loop_start:
    read_from_console %1, %2, %3
    cmp rax, 0
    je %%await_input_loop_start
%endmacro

; exit(exitCode)
%macro exit 1
    mov rcx, %1
    call ExitProcess
%endmacro

; sourceBuffer.len must be a multiple of 8
; move_qwords(destBuffer, sourcebuffer, sourceBuffer.len)
%macro move_qwords 3
    ; store write index
    mov rbx, %1

    ; store read index
    mov rcx, %2

    ; store end of write buffer index
    mov rdx, %3
    add rdx, rcx

%%move_qword:
    ; copy byte
    mov rax, [rcx]
    mov [rbx], rax

    ; increment pointers
    add rbx, 8
    add rcx, 8

    ; check if end of write buffer has been reached
    cmp rcx, rdx
    jl %%move_qword
%endmacro

; combine_strings(destBuffer, buffer1, buffer1.len, buffer2, buffer2.len)
%macro combine_strings 5
    ; Store first string
    ; move_qwords(destBuffer, sourcebuffer, sourceBuffer.len)
    move_qwords %1, %2, %3

    ; Store second string
    mov rax, %1
    add rax, %3
    ; move_qwords(destBuffer, sourcebuffer, sourceBuffer.len)
    move_qwords rax, %4, %5
%endmacro

%macro setup 0
    mov rbp, rsp
    get_input_handle
    get_output_handle
%endmacro

section .text
global main
main:
    setup                                               ; setup()

    ; char[]* input()
    lea r12, [dynamic_memory + 128]
    push r12
    sub rsp, 8
    await_input r12 + 8, 64, r12                        ; await_input(buffer, numBytesToRead, numBytesReadBuffer)

    ; char[]* input()
    lea r12, [dynamic_memory]
    push r12
    sub rsp, 8  
    await_input r12 + 8, 64, r12                        ; await_input(buffer, numBytesToRead, numBytesReadBuffer)

    ; void write(char[]* string)
    mov r12, [rbp - 8]
    write_to_console r12 + 8, [r12], trash              ; write_to_console(buffer, numBytesToWrite, numBytesWrittenBuffer)

    ; void write(char[]* string)
    mov r12, [rbp - 24]
    write_to_console r12 + 8, [r12], trash              ; write_to_console(buffer, numBytesToWrite, numBytesWrittenBuffer)

    add rsp, 32

    ; void exit(int exit_code)
    exit 0                                              ; exit(exitCode)

section .bss
    input_handle: resb 8
    output_handle: resb 8

    dynamic_memory: resb 256

    trash: resb 8