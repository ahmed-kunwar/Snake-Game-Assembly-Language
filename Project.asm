[org 0x0100]
jmp start
seconds: db 0
timerflag: dw 0
oldkb: dd 0
incre: db 0
snake:times 240 dw 0 
len: db 20
len1:db 20
totallives: db 3
remaininglives: db 3
str1: db 'TLives:', 0
str2: db 'RLives:', 0
str3: db 'Snakesize:', 0
food: db '@','#','$','&','+'
ticks: dw 0
speed: db 6
n: db 1
level: db 0
over: db 'GAME OVER', 0
won: db 'You won'
win:
	pusha
	push es
	push ds
	push cs
	pop ds
	
	push 0x21
	push 1992
	push won
	
	call prtstr
	
	pop ds
	pop es
	popa
	
ret

Checker:
	pusha
	push es
	push ds
	
	cmp byte[cs:len1],240
	jne continue
	mov byte[cs:remaininglives],0
	;call win
	pop ds
	pop es
	popa
	ret
	continue:
	
	cmp byte[cs:level],2
	jne next
	mov ax,[cs:ticks]
	mov dx,0
	mov bx,1440
	div bx
	cmp dx,0
	jne ter
	dec byte[cs:remaininglives]
	mov word[cs:ticks],0
	call initial
	mov byte[cs:len1],20
	mov byte[cs:level],0
	mov byte[cs:len],20
	call clrscr
	call boundary
	call print
	call Fcreate
	call printdata
	mov byte[cs:level],0
	next:
	
	
	mov ax,[cs:ticks]
	mov dx,0
	mov bx,1440
	div bx
	cmp dx,0
	jne ter
	mov byte[cs:speed],6
	mov byte[cs:seconds],0
	call initial
	cmp byte[cs:level],0
	jne level2
	call Stage1
	mov byte[cs:level],1
	jmp no
	level2:
	call stage2
	mov byte[cs:level],2
	jmp no
	ter:
	
	mov ax,[cs:ticks]
	mov bx,360
	mov dx,0
	div bx
	cmp dx,0
	jne no
	mov cl,[cs:speed]
	sub cl,2
	cmp cl,0
	jne w
	mov cl,1
	w:
	mov byte[cs:speed],cl
	mov byte[cs:seconds],0
	no:
	
	pop ds
	pop es
	popa
ret
	
	

Stage1:
	pusha
	push es
	push ds
	
	mov byte[cs:len],20
	call clrscr
	call boundary
	call initial
	call print
	call printdata
	call Fcreate
	
	
	
	mov ax,0xb800
	mov es,ax
	mov di,1608
	mov ah,0x12
	mov al,'|'
	mov cx,8
	
	hur1:
	mov [es:di],ax
	add di,160
	loop hur1
	
	mov di,3406
	mov cx,20
	cld
	rep stosw
	
	mov di,1006
	mov cx,20
	cld
	rep stosw
	
	pop ds
	pop es
	popa
ret

stage2:
	pusha 
	push es
	push ds
	
	call clrscr
	call boundary
	call initial
	call print
	call printdata
	call Fcreate
	
	mov ax,0xb800
	mov es,ax
	mov si,966
	mov di,968
	mov cx,7
	mov ah,0x12
	mov al,'|'
	
	hurLup:
	mov [es:si],ax
	add si,160
	loop hurLup
	cld
	mov cx,34
	rep stosw
	
	add di,6
	mov cx,34
	rep stosw
	
	mov cx,7
	hurRup:
	mov [es:di],ax
	add di,160
	loop hurRup
	
	add di,480
	mov cx,6
	hurRdwn:
	mov [es:di],ax
	add di,160
	loop hurRdwn
	
	std
	mov cx,34
	rep stosw
	
	sub di,6
	mov cx,34
	std
	rep stosw
	
	mov cx,6
	hurLdwn:
	mov [es:di],ax
	sub di,160
	loop hurLdwn
	
	pop ds
	pop es
	popa
ret

Foodcheck:
	push bp 
	mov bp,sp
	pusha
	push es
	push ds
	
	push cs
	pop ds
	
	mov di,[bp+4]
	mov bx,0xb800
	mov es,bx
	mov ax,[es:di]
	cmp ax,0x0720
	je exit5
	mov al,182
	out 43h,al
	mov ax,9121
	out 42h,al
	mov al,ah
	out 42h,al
	in al,61h
	or al,00000011b
	out 61h,al
	
	in al,61h
	and al,11111100b
	out 61h,al
	mov cx,4
	increment:
	inc byte[cs:len]
	push ds
	push cs
	pop ds
	mov bl,[cs:len]
	mov bh,0
	mov si,bx
	sub si,2
	shl si,1
	mov bx,snake
	add si,bx
	mov bx,[ds:si]
	add si,2
	mov word[ds:si],bx
	pop ds
	inc byte[cs:len1]
	call printdata
	loop increment
	call Fcreate
	exit5:
	
	pop ds
	pop es
	popa
	pop bp
ret 2

Fcreate:
	pusha
	push es
	push ds
	push cs
	pop ds
	
	mov ax,[cs:ticks]
	mov bx,2000
	mov dx,0
	add ax,[cs:len]
	div bx
	shl dx,1
	add dx,640
	cmp dx,3840
	jl go
	sub dx,640
	go:
	cmp dx,960
	jg inbound
	mov dx,3840
	inbound:
	;food load
	mov al,byte[cs:len]
	mov ah,0
	push dx
	mov dx,[cs:ticks]
	add ax,dx
	mov dx,0
	mov bx,5
	div bx
	mov si,dx
	pop dx
	mov cl,byte[cs:food+si]
	mov ch,0xc1
	
	mov ax,0xb800
	mov es, ax
	mov di,dx
	mov bx,[es:di]
	cmp bx,0x0720
	je prt
	ag:
	mov bx,[es:di-2]
	cmp bx,0x0720
	je prt
	sub di,2
	jmp ag
	
	prt:
	sub di,2
	mov word[es:di],cx
	
	pop ds
	pop es
	popa
ret
	

clrscr:
	pusha
	
	mov cx,2000
	mov ax,0xb800
	mov es,ax
	mov ax,0x0720
	mov di,0
	cld
	
	rep stosw
	popa
ret

strlen:
	push bp
	mov bp, sp
	push es
	push di
	push cx
	push ds
	pop es
	
	mov di, [bp+4]
	mov cx, 0xffff
	xor al, al
	repne scasb
	mov ax, 0xffff
	sub ax, cx
	dec ax

	pop cx
	pop di
	pop es
	pop bp
ret 2

prtstr:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push si
	push ds
	push bx
	push dx
	push cs
	pop ds
	mov ax, [bp+4]
	push ax
	call strlen
	
	cmp ax, 0
	je exit1
	mov cx, ax
	
	mov ax, 0xb800
	mov es, ax
	mov di, [bp+6]   ;location
	mov si, [bp+4]   ;points to string
	mov ah, [bp+8]   ;loads attribute
	
	nextchar1:	
		cld
		lodsb
		stosw
		loop nextchar1
		
	exit1:
		pop dx
		pop bx
		pop ds
		pop si
		pop cx
		pop es
		pop ax
		pop bp
		
ret 6
	
prtnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	
	mov ax, 0xb800
	mov es, ax
	mov ax, [bp+4]    ;number
	mov bx, 10
	mov cx, 0
	
	nextdigit1:
		mov dx, 0
		div bx
		add dl, 0x30
		push dx
		inc cx
		cmp ax, 0 
		jne nextdigit1
		
	nextpos1:
		pop dx
		mov bx, [bp+6]
		mov dh, bl
		mov word[es:di], dx
		add di, 2
		loop nextpos1
	
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	
ret 4

printdata:
	pusha
	push es
	push ds
	push cs
	pop ds
	xor di, di
	mov ax, 0xb800
	mov es, ax
	
	;prints total lives
	push 0x74
	push di
	push str1
	call prtstr
	mov al, [totallives]
	mov ah, 0   
	push  0x74			;red on white attribute
	push ax
	call prtnum
	add di, 2
	
	;prints remaining lives
	push 0x74
	push di
	push str2
	call prtstr
	mov al, [remaininglives]
	mov ah, 0   
	push  0x74          ;red on white attribute
	push ax
	call prtnum
	add di, 2
	
	;prints the current snake size
	push 0x74
	push di
	push str3
	call prtstr
	mov al, [len1]
	mov ah, 0   
	push  0x74         ;red on white attribute
	push ax
	call prtnum
	
	pop ds
	pop es
	popa
ret

initial:
	pusha
	push cs
	pop ds
	
	
	mov byte[cs:speed],6
	mov byte[cs:incre],0
	mov byte[cs:len],20
	
	mov cx,20
	mov ax,snake
	mov di,ax
	push cs
	pop es
	
	mov ax,2000
	cld
	l1:
	stosw
	add ax,2
	loop l1
	
	popa
ret

check:
	push bp
	mov bp, sp
	
	pusha
	push es
	push ds
	
	mov ax, 0xb800
	mov es,ax
	mov di, [bp+4]
	mov bh, 0x12
	mov bl, '|'
	cmp word[es:di], bx
	jne exit4
	 
	mov word[bp+6], 1
	jmp p1
	exit4:
		mov bh, 0x21
		mov bl, '*'
		cmp word[es:di], bx
		jne exit3
		mov word[bp+6], 1
		jmp p1
	exit3:
		mov word[bp+6], 0
	p1:	
		pop ds
		pop es
		popa
		pop bp
	ret 2

move:
	pusha
	
	push cs
	pop ds
	push cs
	pop es
	mov si,snake
	mov ax,word[ds:si]
	mov cl,byte[cs:incre]
	cmp cl,0
	je la1
	cmp cl,1
	je ra1
	cmp cl,2
	je ua1
	cmp cl,3
	je da1

	la1:
	sub ax,2
	sub sp, 2
	push ax
	call check
	pop bx
	cmp bx, 1
	je link
	push ax
	call Foodcheck
	jmp p
	ra1:
	add ax,2
	sub sp, 2
	push ax
	call check
	pop bx
	cmp bx, 1
	je exit2
	push ax
	call Foodcheck
	jmp p
	ua1:
	sub ax,160
	sub sp, 2
	push ax
	call check
	pop bx
	cmp bx, 1
	je exit2
	push ax
	call Foodcheck
	jmp p
	da1:
	add ax,160
	sub sp, 2
	push ax
	call check
	pop bx
	cmp bx, 1
	je exit2
	push ax
	call Foodcheck
	link:
	cmp bx, 1
	je exit2
	p:
	mov cl,[cs:len]
	mov ch,0
	mov dx,cx
	shl dx,1
	add si,dx
	sub si,4
	mov di,si
	add di,2
	dec cx
	
	mov dx,[es:di]
	std
	rep movsw
	cld
	
	mov cx,0xb800
	mov ds,cx
	mov si,dx
	mov word[ds:si],0x0720
	mov word[es:di],ax
	jmp q
	exit2:
		dec byte[cs:remaininglives]
		mov byte[cs:speed],6
		mov byte[cs:len1],20
		mov byte[cs:level],0
		mov word[cs:ticks],0
		call clrscr
		call printdata
		call boundary
		call initial
		call print
		call Fcreate
	q:
		popa
ret
	

print:
	pusha
	push cs
	pop ds
	
	mov ax,0xb800
	mov es,ax
	push cs
	pop ds
	mov si,snake
	mov cl,[cs:len]
	mov ch,0
	
	lodsw
	mov di,ax
	mov ah,0x21
	mov al, ':'
	stosw
	dec cx
	l2:
		lodsw
		mov di,ax
		mov ax,0x212a
		stosw
	loop l2
	popa
	ret

printnum:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di

	mov ax,0xb800
	mov es,ax
	mov ax,[bp+4]
	mov bx,10
	mov cx,0

nextdigit:
	mov dx,0
	div bx
	add dl,0x30
	push dx
	inc cx
	cmp ax,0
	jnz nextdigit
	
	mov di,140
	
nextpos:
	pop dx
	mov dh,0x74
	mov [es:di],dx
	add di,2
	loop nextpos
	
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
ret 2

boundary:
	pusha
	
	mov ax, 0xb800
	mov es, ax
	xor di, di
	mov al, 4
	mov cl, 160
	mul cl
	mov di, ax
	mov bh, 0x12
	mov bl, '|'
	mov cx, 80
	
	nextloc:
		mov word[es:di], bx
		add di, 2
	loop nextloc

	add ax, 160
	mov di, ax
	mov cx, 24
	
	nextloc1:
		mov word[es:di], bx
		add di, 160
	loop nextloc1

	add ax, 158
	mov di, ax
	mov cx, 24
	
	nextloc2:
		mov word[es:di], bx
		add di, 160
	loop nextloc2
	
	mov di, 3840
	mov cx, 80
	
	nextloc3:
		mov word[es:di], bx
		add di, 2
	loop nextloc3
	
	popa
	ret

kbisr:
	push ax
	in al,0x60
	cmp al,0x4b
	je la
	cmp al,0x4d
	je ra
	cmp al,0x48
	je ua
	cmp al,0x50
	je da

	jmp exit

	la:
	mov byte[cs:incre],0
	jmp exit
	ra:
	mov byte[cs:incre],1
	jmp exit
	ua:
	mov byte[cs:incre],2
	jmp exit
	da:
	mov byte[cs:incre],3
	jmp exit


	jmp exit

	nomatch:
	pop ax
	jmp far[cs:oldkb]

	exit:
	mov al,0x20
	out 0x20,al
	pop ax
	iret

timer:
	pusha
	mov dx,0
	cmp byte[cs:remaininglives],0
	je skip
	inc word[cs:ticks]
	inc byte[cs:seconds]
	call Checker
	mov cl,[cs:speed]
	cmp byte[cs:seconds],cl
	jne skipall
	call move
	call print
	mov byte[cs:seconds],0
	cmp word[cs:timerflag],1
	jne skipall
	skip:
		call clrscr
		call printdata
		call boundary
		push 0xf4
		push 1990
		push over
		call prtstr
	
skipall:
	mov al,0x20
	out 0x20,al
	
	popa
	iret
	
start:
	call clrscr
	call printdata
	call boundary
	call initial
	call print
	call Fcreate
	
	xor ax,ax
	mov es,ax
	mov ax,[es:9*4]
	mov [oldkb],ax
	mov ax,[es:9*4+2]
	mov [oldkb+2],ax
	cli
	mov word[es:9*4],kbisr
	mov [es:9*4+2],cs
	mov word [es:8*4],timer
	mov [es:8*4+2],cs
	sti
	
	mov dx,start
	add dx,15
	mov cl,4
	shr dx,cl
	

mov ax,0x3100
int 21h
