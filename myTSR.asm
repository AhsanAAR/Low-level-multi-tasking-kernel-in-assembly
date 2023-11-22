; PCB layout:
; ax,bx,cx,dx,si,di,bp,sp,ip,cs,ds,ss,es,flags,next,prev,status
; 0 ,2 ,4 ,6 ,8 ,10,12,14,16,18,20,22,24,26   ,28  ,30  ,31

; status 0xFF active
; status 0xF0 suspended
; status 0x00 deleted

[org 0x0100]
jmp start

AXI EQU 0
BXI EQU 2
CXI EQU 4
DXI EQU 6
SII EQU 8
DII EQU 10
BPI EQU 12
SPI EQU 14
IPI EQU 16
CSI EQU 18
DSI EQU 20
SSI EQU 22
ESInd EQU 24
FLAGSI EQU 26
NEXTI EQU 28
PREVI EQU 30
STATUS EQU 31

PCB:         times 32*32 db 0    ; space for 32 PCB's each of 32 bytes
STACK:       times 32*512 db 0   ; space for 512-byte stcks corresponding to each PCB
               
currentPCB:  dw 0
oldTimerIsr: dw 0,0

timerCount: dw 0


;******************************************************************
;							NEW TIMER ISR 
;******************************************************************


newTimerIsr: 

push ax
mov ax, [timerCount]
inc ax
mov [timerCount], ax
cmp ax, 10
pop ax
jne exitTimer
mov word [timerCount], 0
call StoreContext
call GetNextThread
call RestoreContext
exitTimer:
jmp far [cs:oldTimerIsr] 				; runs the system isr for timer

StoreContext:
		push 	bx
		mov 	bx,[cs:currentPCB] 
		shl 	bx,5 
		mov 	[cs:PCB+bx+AXI],ax
		mov 	[cs:PCB+bx+CXI],cx
		mov 	[cs:PCB+bx+DXI],dx
		mov 	[cs:PCB+bx+SII],si
		mov 	[cs:PCB+bx+DII],di
		mov 	[cs:PCB+bx+BPI],bp
		mov 	[cs:PCB+bx+ESInd],es
		mov 	[cs:PCB+bx+DSI],ds
		pop 	ax
		mov 	[cs:PCB+bx+BXI],ax
		pop 	cx						; saves return addres
		pop 	ax

		; gets the IP CS and FLAGS through the stack
		mov 	[cs:PCB+bx+IPI],ax		
		pop 	ax
		mov 	[cs:PCB+bx+CSI],ax
		pop 	ax
		mov 	[cs:PCB+bx+FLAGSI],ax
		
		mov 	[cs:PCB+bx+SSI],ss
		mov 	[cs:PCB+bx+SPI],sp
		push 	cx						; restore return address for ret statement
		ret

GetNextThread:
		push 	bx							
		push 	cx
		mov 	bx,[cs:currentPCB]
		shl 	bx,5
getNext:
		mov 	ax,[cs:PCB+bx+NEXTI]
		mov 	bx, ax
		shl 	bx, 5
		mov 	cl, [cs:PCB+bx+STATUS]
		cmp 	cl, 0xFF				; loop until we get an active PCB
		jne 	getNext
		pop 	cx
		pop 	bx
		ret								; ax contains the nextPCB index

; ax contains PCB to be restored
RestoreContext:
		mov 	bx,ax
		mov 	[cs:currentPCB],bx
		shl 	bx,5
		pop 	cx						; store the return address for ret statement
		cli
		mov 	ax,[cs:PCB+bx+SSI]
		mov 	ss,ax
		mov 	sp,[cs:PCB+bx+SPI]
		sti

		; restores the stack with flags, cs and ip for the iret
		mov 	ax,[cs:PCB+bx+FLAGSI]	
		push 	ax						
		mov 	ax,[cs:PCB+bx+CSI]
		push 	ax
		mov 	ax,[cs:PCB+bx+IPI]
		push 	ax

		push 	cx  					; restore the return address on stack
		mov 	ax,[cs:PCB+bx+DSI]
		mov 	ds,ax
		mov 	ax,[cs:PCB+bx+ESInd]
		mov 	es,ax
		mov 	cx,[cs:PCB+bx+CXI]
		mov 	dx,[cs:PCB+bx+DXI]
		mov 	si,[cs:PCB+bx+SII]
		mov 	di,[cs:PCB+bx+DII]
		mov 	bp,[cs:PCB+bx+BPI]
		mov 	ax,[cs:PCB+bx+AXI]
		mov 	bx,[cs:PCB+bx+BXI]
		ret


;******************************************************************
;						ISR 60 for PCB control
; subservices:
; 0x55 = create
; 0x56 = suspend
; 0x57 = restore
; 0x58 = delete
;******************************************************************

isr60:

	cmp 	al,0x55
	je 		CreateThread
	cmp 	al,0x56
	je		SuspendThread
	cmp 	al,0x57
	je 		RestoreThread
	cmp 	al,0x58
	je 		DeleteThread
	iret

; bx contains the PCB we want to delete
DeleteThread:
	push ax
	push cx
	push bx
	shl bx, 5
	mov al, [cs:PCB+bx+STATUS]
	cmp al, 0
	je dontDelete
	mov byte [cs:PCB+bx+STATUS], 0x00
	mov al, [cs:PCB+bx+PREVI]
	mov cx, [cs:PCB+bx+NEXTI]
	mov bx, cx
	shl bx, 5
	mov [cs:PCB+bx+PREVI], al
	xor ah, ah
	mov bx, ax
	shl bx, 5
	mov [cs:PCB+bx+NEXTI], cx
dontDelete:
	pop bx
	pop cx
	pop ax
	iret

; bx contains the PCB we want to restore
RestoreThread:
	push bx
	shl bx, 5
	mov cl, [cs:PCB+bx+STATUS]
	cmp cl, 0xF0
	jne dontRestore
	mov byte [cs:PCB+bx+STATUS], 0xFF
dontRestore:
	pop bx
	iret

; bx contains the PCB we want to suspend
SuspendThread:
	push bx
	shl bx, 5
	mov cl, [cs:PCB+bx+STATUS]
	cmp cl, 0xFF
	jne dontSuspend
	mov byte [cs:PCB+bx+STATUS], 0xF0
dontSuspend:
	pop bx
	iret

;parameters:
;ip of process
;cs of process
CreateThread:
	push 	bp
	mov 	bp,sp
	push 	bx
	push 	ax
	push 	cx

	call 	getFreePCB
	; ax contains new PCB index
	or 	ax,0
	jz 	NotPossible					; PCB is filled
	call 	initPCB
	mov 	bx,ax
	shl 	bx,5
	push ax
	mov cx, [bp+12]
	push si
	mov si, bp
	push bx
	push bp
	push di
	mov di, bx
	mov bx, [cs:PCB+bx+SPI]

	cmp cx, 0
	je exitParams

	mov bp, cx
	shl bp, 1
	add bp, si
	add bp, 12


paramsLoop:
	mov ax, [bp]
	sub bx, 2
	mov [cs:bx], ax
	sub bp, 2
	loop paramsLoop
exitParams:
	mov [cs:PCB+di+SPI], bx
	pop di
	pop bp
	pop bx
	pop si
	mov 	ax,[bp+10]
	mov 	[cs:PCB+bx+CSI],ax 		; process CS from stack
	mov 	ax,[bp+8]
	mov 	[cs:PCB+bx+IPI],ax 		; process ip from stack
	pop ax
	call 	InsertPCB
NotPossible:
	pop cx
	pop ax
	pop bx
	pop bp
	ret

; address where a thread will return if it returns
LegalTerm:
	mov ax, 0x4c00
	int 21h

getFreePCB:
	push 	bx
	push 	cx
	mov 	cx,32
	mov 	bx,0
chknext:
	mov 	al,[cs:PCB+bx+STATUS]
	or 	    al,0			
	jz 	    gotFree
	add 	bx,32
	loop  chknext
	xor 	bx,bx
gotFree:
	shr 	bx,5
	mov 	ax,bx
	pop 	cx
	pop 	bx
	ret

initPCB:
	push 	bx
	push 	ax
	mov 	bx,ax
	shl 	bx,5
	push 	si
	mov 	si, bx
	mov 	word [cs:PCB+bx+AXI],0
	mov 	word [cs:PCB+bx+BXI],0
	mov 	word [cs:PCB+bx+CXI],0
	mov 	word [cs:PCB+bx+DXI],0
	mov 	word [cs:PCB+bx+BPI],0
	mov 	word [cs:PCB+bx+SII],0
	mov 	word [cs:PCB+bx+DII],0
	mov 	word [cs:PCB+bx+DSI],ds
	mov 	word [cs:PCB+bx+ESInd],es
	mov 	word [cs:PCB+bx+SSI],cs
	mov 	word [cs:PCB+bx+FLAGSI],0x0200
	mov 	byte [cs:PCB+bx+STATUS],0xFF
	inc 	ax
	shl 	ax, 9
	add 	ax, STACK
	sub		ax, 2
	mov 	bx,ax
	mov 	[cs:bx],cs
	sub 	bx,2
	mov 	word [cs:bx],LegalTerm
	mov 	[cs:PCB+si+SPI],bx
	pop 	si
	pop 	ax
	pop 	bx
	ret

; AX Contains PCB Index to be inserted
; We will insert it on Zero
; ThisPCB[Next] = 0[Next] 
; 0[NEXT]=This PCB
; 0 -> Next -> ax
; ax -> Next -> 0
InsertPCB:
	push 	bx 
	push 	cx
	push 	dx
	cli
	mov 	cx,[cs:PCB+0+NEXTI]			; cs = 0->next
	mov 	bx, cx
	shl 	bx, 5
	mov 	[cs:PCB+bx+PREVI], al		; 0->next->prev = ax
	mov 	bx, ax 						
	shl 	bx,5 ; 
	mov 	[cs:PCB+bx+NEXTI],cx		; ax->next = 0->next
	mov 	byte [cs:PCB+bx+PREVI], 0	; ax->prev = 0
	mov 	[cs:PCB+0+NEXTI],ax			; 0->next = ax
	pop dx
	pop cx
	pop bx
	sti
	ret

;*************************************************************


;parameters:
;num
;col
;row
printnum: push bp
 mov bp, sp
 push es
 push ax
 push bx
 push cx
 push dx
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov ax, [bp+4] ; load number in ax
 mov bx, 10 ; use base 10 for division
 mov cx, 0 ; initialize count of digits
nextdigit: 
 mov dx, 0 ; zero upper half of dividend
 div bx ; divide by 10
 add dl, 0x30 ; convert digit into ascii value
 push dx ; save ascii value on stack
 inc cx ; increment count of values
 cmp ax, 0 ; is the quotient zero
 jnz nextdigit ; if no divide it again
 mov di, [bp+6]
 mov ax, 160
 mul di
 mov di, [bp+8]
 shl di, 1
 add di, ax
 nextpos: pop dx ; remove a digit from the stack
 mov dh, 0x07 ; use normal attribute
 mov [es:di], dx ; print char on screen
 add di, 2 ; move to next screen location
 loop nextpos ; repeat for all digits on stack
 pop di
 pop dx
 pop cx
 pop bx
 pop ax
 pop es
 pop bp
 ret 6

myRoutine:
	mov bp, sp
	xor dx, dx
	mov ax, [bp]
	mov bx, [bp+2]
myLoop:
	push ax
	push bx
	push dx
	call printnum ; call the printnum subroutine
	inc dx
	cmp dx, 1000
	jne noReset
	xor dx, dx
noReset:
	mov cx, 0
	dec cx
	shr cx, 3
delay: 
	loop delay
	jmp myLoop


start: 
	xor ax, ax 
	mov es, ax ; point es to IVT base
	mov word [cs:PCB+STATUS],0xFF
	mov ax, [es:0x08*4]
	mov [oldTimerIsr], ax
	mov ax, [es:0x08*4 + 2] 
	mov [oldTimerIsr+2], ax 
	
	cli 
	mov word [es:0x08*4], newTimerIsr 
	mov [es:0x08*4+2], cs 
	sti 

	mov word [es:0x60*4], isr60
	mov word [es:0x60*4+2], cs

	; insert routine1
	mov al, 0x55
	push word 20 ; col
	push word 20 ; row
	push word 2
	push cs
	push myRoutine
	int 0x60

	; insert routine2
	mov al, 0x55
	push word 10
	push word 10
	push word 2
	push cs
	push myRoutine
	int 0x60

	mov dx, start 
	add dx, 15 
	mov cl, 4 
	shr dx, cl 
	mov ax, 0x3100 
	int 0x21