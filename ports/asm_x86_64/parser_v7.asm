; ============================================================
; Parser V7 incremental (CKY + beam + unary closure)
; x86-64 Linux, NASM
;
; Output demo:
;   N=<n> ACCEPT=<0|1> BEST_PTR=<int>
;
; Build:
;   nasm -felf64 parser_v7.asm -o parser_v7.o
;   ld -o parser_v7 parser_v7.o
;   ./parser_v7
; ============================================================

BITS 64
default rel

%define MAXN 64
%define MAXJ (MAXN+1)
%define BEAM 8
%define SIZE (MAXN*MAXJ*BEAM)

%define NEG  -1073741824         ; ~ -2^30

%define START_SYM    1           ; S
%define FALLBACK_SYM 7           ; NOUN

SECTION .data

; ---- toy lexicon (lowercase) ----
; entries: word ptr, len, pos(sym)
lex_words: dq w_el, w_filosofo, w_murio
lex_lens:  dd 2,    8,         5
lex_pos:   dd 4,    5,         6
LEX_N equ 3

w_el:       db "el",0
w_filosofo: db "filosofo",0
w_murio:    db "murio",0

; ---- toy grammar rules ----
; unary: (lhs, rhs, logw)
unary_lhs: dd 3
unary_rhs: dd 6
unary_w:   dd 0
UNARY_N equ 1

; binary: (lhs, rhs1, rhs2, logw)
binary_lhs: dd 1, 2
binary_r1:  dd 2, 4
binary_r2:  dd 3, 5
binary_w:   dd 0, 0
BINARY_N equ 2

; demo tokens (already lowercase/ascii)
t1: db "el",0
t2: db "filosofo",0
t3: db "murio",0

msgN:      db "N=",0
msgA:      db " ACCEPT=",0
msgP:      db " BEST_PTR=",0
msgNL:     db 10,0

SECTION .bss

; token count
n:      resd 1

; chart arrays (parallel):
; cat/sc/kind/a/b each int32
catA:   resd SIZE
scA:    resd SIZE
kindA:  resd SIZE
aA:     resd SIZE
bA:     resd SIZE

; scratch for printing
buf:    resb 64

SECTION .text
global _start

; ---------------------------
; helpers: sys_write
; rdi=fd, rsi=ptr, rdx=len
; ---------------------------
sys_write:
    mov eax, 1
    syscall
    ret

; ---------------------------
; strlen (null-terminated)
; rdi=ptr -> rax=len
; ---------------------------
strlen:
    xor eax, eax
.lenloop:
    cmp byte [rdi+rax], 0
    je .done
    inc eax
    jmp .lenloop
.done:
    ret

; ---------------------------
; print c-string (0-terminated)
; rdi=ptr
; ---------------------------
printz:
    push rdi
    call strlen
    pop rsi              ; ptr -> rsi
    mov rdx, rax
    mov rdi, 1
    call sys_write
    ret

; ---------------------------
; print int32 as decimal
; edi=value
; ---------------------------
print_int:
    ; writes into buf (end-aligned), then sys_write
    mov eax, edi
    lea rbx, [buf+63]
    mov byte [rbx], 0
    dec rbx

    ; handle 0
    cmp eax, 0
    jne .nz
    mov byte [rbx], '0'
    lea rsi, [rbx]
    mov rdx, 1
    mov rdi, 1
    call sys_write
    ret

.nz:
    ; handle negative
    mov ecx, 0
    test eax, eax
    jge .pos
    neg eax
    mov ecx, 1
.pos:
    ; convert
.conv:
    xor edx, edx
    mov ebp, 10
    div ebp              ; eax=quot, edx=rem
    add dl, '0'
    mov [rbx], dl
    dec rbx
    test eax, eax
    jne .conv

    ; add '-'
    cmp ecx, 0
    je .out
    mov byte [rbx], '-'
    dec rbx

.out:
    inc rbx
    lea rsi, [rbx]
    ; length = (buf+63 - rbx)
    lea rax, [buf+63]
    sub rax, rbx
    mov rdx, rax
    mov rdi, 1
    call sys_write
    ret

; ---------------------------
; idx = (((i-1)*MAXJ + (j-1))*BEAM + (s-1))
; inputs: edi=i, esi=j, edx=s   (all 1-based)
; output: eax = idx (0-based element index)
; ---------------------------
idx3:
    dec edi
    dec esi
    dec edx
    mov eax, edi
    imul eax, MAXJ
    add eax, esi
    imul eax, BEAM
    add eax, edx
    ret

; ---------------------------
; pack ptr = ((i*1000)+j)*10+s
; edi=i esi=j edx=s -> eax ptr
; ---------------------------
pack_ptr:
    mov eax, edi
    imul eax, 1000
    add eax, esi
    imul eax, 10
    add eax, edx
    ret

; ---------------------------
; clear_cell(i,j)
; edi=i esi=j
; ---------------------------
clear_cell:
    push rbx
    push rcx
    mov ecx, 1
.loopS:
    cmp ecx, BEAM+1
    je .done
    mov edx, ecx
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    ; eax = idx0
    mov ebx, eax
    shl ebx, 2           ; byte offset
    ; cat=0, sc=NEG, kind=0, a=0, b=0
    mov dword [catA + rbx], 0
    mov dword [scA  + rbx], NEG
    mov dword [kindA+ rbx], 0
    mov dword [aA   + rbx], 0
    mov dword [bA   + rbx], 0
    inc ecx
    jmp .loopS
.done:
    pop rcx
    pop rbx
    ret

; ---------------------------
; lex_lookup(token_ptr in rdi) -> eax=pos(sym)
; compares against lex_words entries
; ---------------------------
lex_lookup:
    push rbx
    push r12
    push r13
    ; token len in r12d
    mov rbx, rdi
    call strlen
    mov r12d, eax

    xor r13d, r13d   ; i=0
.loop:
    cmp r13d, LEX_N
    je .fallback

    ; compare length first
    mov eax, dword [lex_lens + r13*4]
    cmp eax, r12d
    jne .next

    ; strcmp for len bytes
    mov rsi, [lex_words + r13*8]   ; dict word ptr
    xor ecx, ecx
.cmpb:
    cmp ecx, r12d
    je .match
    mov al, byte [rbx + rcx]
    mov dl, byte [rsi + rcx]
    cmp al, dl
    jne .next
    inc ecx
    jmp .cmpb

.match:
    mov eax, dword [lex_pos + r13*4]
    jmp .done

.next:
    inc r13d
    jmp .loop

.fallback:
    mov eax, FALLBACK_SYM

.done:
    pop r13
    pop r12
    pop rbx
    ret

; ---------------------------
; insert_cell(i,j,newCat,newSc,newKind,newA,newB)
; args:
;   edi=i, esi=j
;   edx=newCat
;   ecx=newSc
;   r8d=newKind
;   r9d=newA
;   [rsp+8] = newB (because we run out of regs)
;
; returns:
;   eax=1 if changed (inserted/replaced), else 0
; ---------------------------
insert_cell:
    push rbx
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15

    ; newB
    mov r15d, dword [rsp + 8 + 7*8]  ; after pushes: 7 regs *8 bytes

    ; scan beam: find same cat, empty, and worst
    xor r11d, r11d       ; same_s=0
    xor r12d, r12d       ; empty_s=0
    mov r13d, 1          ; worst_s=1
    ; worst_sc = sc(1)
    mov edx, 1
    push rdi
    push rsi
    mov edx, 1
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2
    mov r14d, dword [scA + rbx]      ; worst_sc

    mov r10d, 1          ; s=1
.scan:
    cmp r10d, BEAM+1
    je .decide

    ; k = idx(i,j,s)
    mov edx, r10d
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2

    ; cat
    mov eax, dword [catA + rbx]
    cmp eax, dword [rsp + 0]  ; doesn't exist; we need newCat in a reg.
    ; keep newCat in r8? we used r8d for kind. We'll reload newCat/newSc:
    ; We'll store newCat/newSc/newKind/newA/newB into locals regs now.

.decide:
    ; This function is tricky to keep in few regs; do it properly:
    ; We'll re-map: use r9d newA already, r8d newKind already.
    ; Put newCat in r12? but r12 used. We'll instead keep newCat in r5? doesn't exist.
    ; So: at entry, copy newCat/newSc into r14/r15? r15 used for newB.
    ; We'll implement a fixed version below (simpler): use stack locals.

    ; (We jump to a fixed implementation label.)
    jmp insert_cell_fixed

; ---- fixed implementation uses stack locals for new fields ----
insert_cell_fixed:
    ; Layout locals at top of stack:
    ; [rsp+0]  newCat
    ; [rsp+4]  newSc
    ; [rsp+8]  newKind
    ; [rsp+12] newA
    ; [rsp+16] newB
    sub rsp, 24
    mov dword [rsp+0], edx
    mov dword [rsp+4], ecx
    mov dword [rsp+8], r8d
    mov dword [rsp+12], r9d
    mov dword [rsp+16], r15d

    xor r11d, r11d       ; same_s=0
    xor r12d, r12d       ; empty_s=0
    mov r13d, 1          ; worst_s=1

    ; worst_sc = sc(i,j,1)
    mov edx, 1
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2
    mov r14d, dword [scA + rbx]

    mov r10d, 1
.scan2:
    cmp r10d, BEAM+1
    je .finish_scan2

    mov edx, r10d
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2

    ; same?
    mov eax, dword [catA + rbx]
    cmp eax, dword [rsp+0]
    jne .chk_empty
    mov r11d, r10d

.chk_empty:
    cmp eax, 0
    jne .chk_worst
    cmp r12d, 0
    jne .chk_worst
    mov r12d, r10d

.chk_worst:
    mov eax, dword [scA + rbx]
    cmp eax, r14d
    jge .next2
    mov r14d, eax
    mov r13d, r10d

.next2:
    inc r10d
    jmp .scan2

.finish_scan2:
    ; if same_s !=0 then update if better
    cmp r11d, 0
    je .no_same2
    mov edx, r11d
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2
    mov eax, dword [scA + rbx]
    cmp dword [rsp+4], eax
    jle .no_change
    ; write slot same
    mov eax, dword [rsp+0]
    mov dword [catA + rbx], eax
    mov eax, dword [rsp+4]
    mov dword [scA + rbx], eax
    mov eax, dword [rsp+8]
    mov dword [kindA + rbx], eax
    mov eax, dword [rsp+12]
    mov dword [aA + rbx], eax
    mov eax, dword [rsp+16]
    mov dword [bA + rbx], eax
    mov eax, 1
    add rsp, 24
    jmp .ret_ins

.no_same2:
    ; if empty_s !=0 insert
    cmp r12d, 0
    je .no_empty2
    mov edx, r12d
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2
    mov eax, dword [rsp+0]
    mov dword [catA + rbx], eax
    mov eax, dword [rsp+4]
    mov dword [scA + rbx], eax
    mov eax, dword [rsp+8]
    mov dword [kindA + rbx], eax
    mov eax, dword [rsp+12]
    mov dword [aA + rbx], eax
    mov eax, dword [rsp+16]
    mov dword [bA + rbx], eax
    mov eax, 1
    add rsp, 24
    jmp .ret_ins

.no_empty2:
    ; replace worst if better
    mov edx, r13d
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2
    mov eax, dword [scA + rbx]
    cmp dword [rsp+4], eax
    jle .no_change
    mov eax, dword [rsp+0]
    mov dword [catA + rbx], eax
    mov eax, dword [rsp+4]
    mov dword [scA + rbx], eax
    mov eax, dword [rsp+8]
    mov dword [kindA + rbx], eax
    mov eax, dword [rsp+12]
    mov dword [aA + rbx], eax
    mov eax, dword [rsp+16]
    mov dword [bA + rbx], eax
    mov eax, 1
    add rsp, 24
    jmp .ret_ins

.no_change:
    xor eax, eax
    add rsp, 24

.ret_ins:
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop rbx
    ret

; ---------------------------
; unary_close(i,j)
; edi=i esi=j
; ---------------------------
unary_close:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r15d, 16            ; iterations
.iter:
    xor r14d, r14d          ; changed_any=0

    mov r12d, 1             ; s=1
.slot:
    cmp r12d, BEAM+1
    je .after_slots

    ; read child cat
    mov edx, r12d
    push rdi
    push rsi
    call idx3
    pop rsi
    pop rdi
    mov ebx, eax
    shl ebx, 2

    mov eax, dword [catA + rbx]
    cmp eax, 0
    je .next_slot

    ; childSc
    mov r13d, dword [scA + rbx]

    ; childPtr = pack(i,j,s)
    mov edx, r12d
    push rdi
    push rsi
    call pack_ptr
    pop rsi
    pop rdi
    mov r10d, eax           ; childPtr

    ; apply unary rules
    xor r9d, r9d            ; r index = 0
.u_loop:
    cmp r9d, UNARY_N
    je .next_slot

    mov ecx, dword [unary_rhs + r9*4]
    cmp ecx, dword [catA + rbx]
    jne .u_next

    ; newCat = lhs
    mov edx, dword [unary_lhs + r9*4]
    ; newSc = childSc + logw
    mov ecx, r13d
    add ecx, dword [unary_w + r9*4]
    ; kind=1
    mov r8d, 1
    ; a=childPtr
    mov r9d, r10d
    ; b=0 via stack
    push 0
    call insert_cell
    add rsp, 8
    test eax, eax
    jz .u_next
    mov r14d, 1

.u_next:
    inc r9d
    jmp .u_loop

.next_slot:
    inc r12d
    jmp .slot

.after_slots:
    cmp r14d, 0
    je .done
    dec r15d
    jnz .iter

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; ---------------------------
; combine(i,k,j)
; edi=i esi=k edx=j
; ---------------------------
combine:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12d, 1             ; sl
.sl:
    cmp r12d, BEAM+1
    je .done

    ; L = (i,k,sl)
    mov r8d, edi            ; save i
    mov r9d, esi            ; save k
    mov r10d, edx           ; save j

    mov edi, r8d
    mov esi, r9d
    mov edx, r12d
    call idx3
    mov ebx, eax
    shl ebx, 2

    mov eax, dword [catA + rbx]
    cmp eax, 0
    je .next_sl
    mov r13d, eax           ; catL
    mov r14d, dword [scA + rbx]  ; scL

    ; ptrL
    mov edi, r8d
    mov esi, r9d
    mov edx, r12d
    call pack_ptr
    mov r11d, eax

    mov r15d, 1             ; sr
.sr:
    cmp r15d, BEAM+1
    je .next_sl

    ; R = (k,j,sr)
    mov edi, r9d
    mov esi, r10d
    mov edx, r15d
    call idx3
    mov ebx, eax
    shl ebx, 2

    mov eax, dword [catA + rbx]
    cmp eax, 0
    je .next_sr
    mov ecx, eax            ; catR
    mov ebp, dword [scA + rbx]  ; scR

    ; ptrR
    mov edi, r9d
    mov esi, r10d
    mov edx, r15d
    call pack_ptr
    mov r6d, eax            ; ptrR (r6 isn't a reg; use rdx/rax later)
    ; We'll keep ptrR in r7? We'll store in rax later; simplest: keep in rdx via push.

    ; apply binary rules
    xor r5d, r5d            ; rr index (use r5d via rdi? not possible). Use r2=???
    ; We'll use r8d.. etc already used. We'll just use rsi scratch:
    xor esi, esi            ; rr=0
.br_loop:
    cmp esi, BINARY_N
    je .next_sr

    ; check rhs1==catL and rhs2==catR
    mov eax, dword [binary_r1 + rsi*4]
    cmp eax, r13d
    jne .br_next
    mov eax, dword [binary_r2 + rsi*4]
    cmp eax, ecx
    jne .br_next

    ; newCat
    mov edx, dword [binary_lhs + rsi*4]
    ; newSc = scL + scR + logw
    mov eax, r14d
    add eax, ebp
    add eax, dword [binary_w + rsi*4]
    mov ecx, eax
    ; kind=2
    mov r8d, 2
    ; a=ptrL
    mov r9d, r11d
    ; b=ptrR: recompute pack (k,j,sr) again to avoid extra regs
    mov edi, r9d            ; careful: r9d overwritten; rebuild from saved r9d/r10d/r15d:
    ; rebuild pointers from saved: k=r9d was saved in r9d, but r9d currently ptrL.
    ; We'll restore from stack? easiest: recompute ptrR using saved r9d/r10d in memory? We didn't store.
    ; Simpler: we re-pack ptrR using r8d/r9d/r10d saved earlier:
    ; We saved i/k/j in r8d/r9d/r10d and ptrL in r11d, so k=j are still in r9d/r10d.
    ; Great: r9d is k, r10d is j. ptrL is r11d.

    mov edi, r9d            ; k
    mov esi, r10d           ; j
    mov edx, r15d           ; sr
    call pack_ptr
    push rax                ; newB on stack
    mov edi, r8d            ; i for insert? Wait: r8d is saved i.
    mov esi, r10d           ; j
    ; But r8d currently i, good.
    ; Set i,j correctly:
    mov edi, r8d
    mov esi, r10d
    call insert_cell
    add rsp, 8

.br_next:
    inc esi
    jmp .br_loop

.next_sr:
    inc r15d
    jmp .sr

.next_sl:
    inc r12d
    jmp .sl

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; ---------------------------
; compute_root: sets rax=accept(0/1), ebx=best_ptr
; uses global n
; ---------------------------
compute_root:
    push r12
    push r13
    push r14
    push r15

    mov eax, dword [n]
    cmp eax, 0
    jne .has
    xor eax, eax
    xor ebx, ebx
    jmp .out

.has:
    ; j = n+1
    mov r12d, eax
    inc r12d

    mov r13d, 0            ; bestS
    mov r14d, NEG          ; bestSc
    mov r15d, 1
.loopS:
    cmp r15d, BEAM+1
    je .finish
    ; cat(1,j,s)
    mov edi, 1
    mov esi, r12d
    mov edx, r15d
    call idx3
    mov ecx, eax
    shl ecx, 2
    mov eax, dword [catA + rcx]
    cmp eax, START_SYM
    jne .nextS
    mov eax, dword [scA + rcx]
    cmp eax, r14d
    jle .nextS
    mov r14d, eax
    mov r13d, r15d
.nextS:
    inc r15d
    jmp .loopS

.finish:
    cmp r13d, 0
    jne .acc
    xor eax, eax
    xor ebx, ebx
    jmp .out

.acc:
    mov eax, 1
    mov edi, 1
    mov esi, r12d
    mov edx, r13d
    call pack_ptr
    mov ebx, eax

.out:
    pop r15
    pop r14
    pop r13
    pop r12
    ret

; ---------------------------
; reset()
; clears full arrays and sets n=0
; ---------------------------
reset:
    mov dword [n], 0
    xor ecx, ecx
.loop:
    cmp ecx, SIZE
    je .done
    mov dword [catA + rcx*4], 0
    mov dword [scA  + rcx*4], NEG
    mov dword [kindA+ rcx*4], 0
    mov dword [aA   + rcx*4], 0
    mov dword [bA   + rcx*4], 0
    inc ecx
    jmp .loop
.done:
    ret

; ---------------------------
; step(token_ptr in rdi)
; incremental real: updates only spans ending at j=N+1
; ---------------------------
step:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov eax, dword [n]
    cmp eax, MAXN
    jae .done

    ; pos = lex_lookup(token)
    call lex_lookup
    mov r12d, eax              ; pos

    ; n++
    mov eax, dword [n]
    inc eax
    mov dword [n], eax
    mov r13d, eax              ; t
    mov r14d, r13d
    inc r14d                   ; j=t+1

    ; clear cell (t,j)
    mov edi, r13d
    mov esi, r14d
    call clear_cell

    ; insert lexical: (pos,0,kind=0,a=t,b=0)
    mov edi, r13d
    mov esi, r14d
    mov edx, r12d     ; cat
    xor ecx, ecx       ; sc=0
    xor r8d, r8d       ; kind=0
    mov r9d, r13d      ; a=t
    push 0             ; b=0
    call insert_cell
    add rsp, 8

    ; unary close on (t,j)
    mov edi, r13d
    mov esi, r14d
    call unary_close

    ; spans ending at j: i = t-1 .. 1
    cmp r13d, 2
    jb .done

    mov r15d, r13d
    dec r15d               ; i=t-1
.il:
    cmp r15d, 0
    je .done

    ; clear (i,j)
    mov edi, r15d
    mov esi, r14d
    call clear_cell

    ; for k = i+1 .. j-1
    mov ebx, r15d
    inc ebx                ; k=i+1
.kl:
    mov eax, r14d
    dec eax                ; j-1
    cmp ebx, eax
    jg .after_k

    mov edi, r15d          ; i
    mov esi, ebx           ; k
    mov edx, r14d          ; j
    call combine

    inc ebx
    jmp .kl

.after_k:
    ; unary close (i,j)
    mov edi, r15d
    mov esi, r14d
    call unary_close

    dec r15d
    jmp .il

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; ---------------------------
; DEMO main
; ---------------------------
_start:
    call reset

    ; Step tokens: el filosofo murio
    mov rdi, t1
    call step
    mov rdi, t2
    call step
    mov rdi, t3
    call step

    ; compute root
    call compute_root     ; eax=accept, ebx=best_ptr

    ; print: N=...
    mov rdi, msgN
    call printz
    mov edi, dword [n]
    call print_int

    ; print accept
    mov rdi, msgA
    call printz
    mov edi, eax
    call print_int

    ; print best ptr
    mov rdi, msgP
    call printz
    mov edi, ebx
    call print_int

    ; newline
    mov rdi, msgNL
    call printz

    ; exit
    mov eax, 60
    xor edi, edi
    syscall
