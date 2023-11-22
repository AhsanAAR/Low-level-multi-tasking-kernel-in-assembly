[org 0x0100]

jmp start

start:
mov al, 0x56
mov bx, 2
int 0x60

mov ax, 0x4c00
int 21h