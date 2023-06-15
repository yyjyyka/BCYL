
extern printi
extern printc
extern checkargc
global asm_main
default rel
section .data
glovars: dq 0
section .text
asm_main:
	push rbp
	mov qword [glovars], rsp
	sub qword [glovars], 8
	push rsi ;save asm_main args
	push rdi
	;check arg count:
	sub rsp, 24
	mov rsi, rdi
	mov rdi, 0
	call checkargc
	add rsp, 24
	pop rdi
	pop rsi ;pop asm_main args
	; allocate globals:
	
ldargs:           ;set up command line arguments on stack:
	mov rcx, rdi
	mov rsi, rsi
_args_next:
	cmp rcx, 0
	jz _args_end
	push qword [rsi]
	add rsi, 8
	sub rcx, 1
	jmp _args_next      ;repeat until --ecx == 0
_args_end:
	lea rbp, [rsp--1*8]  ; make rbp point to first arg
	;CALL 0,L1_main
	push rbp 
	call near L1_main
	push rbx
	;STOP
	mov rsp, qword [glovars]
	add rsp, 8          ; restore rsp
	pop rbp
	ret
	
L1_main:
	pop rax			; retaddr
	pop r10			; oldbp  
	sub rsp, 16     ; make space for svm r,bp 
	mov rsi, rsp 
	mov rbp, rsp 
	add rbp, 0	   ; 8*arity 

_L1_main_pro_1:	  ; slide 2 stack slot
	cmp rbp, rsi      
	jz _L1_main_pro_2    
	mov rcx, [rsi+16] 
	mov [rsi], rcx    
	add rsi, 8        
	jmp _L1_main_pro_1    

_L1_main_pro_2: 
	sub rbp, 8 ; rbp pointer to first arg 
	mov [rbp+16], rax ; set retaddr 
	mov [rbp+8], r10  ; set oldbp
	;INCSP 1
	lea rsp, [rsp-8*(1)]
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;CSTI 4
	push 4
	;STI
	pop r10
	pop rax
	mov [rax],r10
	push r10
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 9
	push 9
	;EQ
	pop rax
	pop r10
	cmp rax, r10
	jne .Lasm0
	push 1
	jmp .Lasm1
.Lasm0:
	push 0
.Lasm1:
	;IFZERO L2
	pop rax
	cmp rax,0
	je L2
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;PRINTI
	pop rdi
	push rdi
	sub rsp, 16
	call printi
	add rsp, 16
	;GOTO L3
	jmp L3
	
L2:
	;GETBP
	push rbp
	;OFFSET 0
	push -0
	;ADD
	pop rax
	pop r10
	add rax, r10
	push rax
	;LDI
	pop rax
	mov rax,[rax]
	push rax
	;CSTI 2
	push 2
	;MUL
	pop rax
	pop r10
	imul r10
	push rax
	;PRINTI
	pop rdi
	push rdi
	sub rsp, 16
	call printi
	add rsp, 16
	
L3:
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;INCSP -1
	lea rsp, [rsp-8*(-1)]
	;RET -1
	pop rbx
	add rsp, 8*-1
	pop rbp
	ret
	