; Assemble with ML /c MINICRT.ASM

        .MODEL LARGE, FORTRAN

initCRT         PROTO   FORTRAN
delay           PROTO   FORTRAN, MS:FAR PTR WORD
sound           PROTO   FORTRAN, HZ:FAR PTR WORD
nosound         PROTO   FAR
KeyPressed      PROTO   FORTRAN
RDK_INT         PROTO   FORTRAN
PLAYNUM         PROTO   FORTRAN, N:FAR PTR WORD
ISANYKEYDOWN    PROTO   FORTRAN
LASTSCAN        PROTO   FORTRAN
PEEKB           PROTO   FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD
PEEKW           PROTO   FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD
PEEKL           PROTO   FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD
POKEB           PROTO   FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD,
                                 X:FAR PTR BYTE
POKEW           PROTO   FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD,
                                 X:FAR PTR WORD
POKEL           PROTO   FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD,
                                 X:FAR PTR DWORD
PORTINB         PROTO   FORTRAN, PORTNUM: FAR PTR WORD
PORTINW         PROTO   FORTRAN, PORTNUM: FAR PTR WORD
PORTOUTB        PROTO   FORTRAN, PORTNUM: FAR PTR WORD, X:FAR PTR BYTE
PORTOUTW        PROTO   FORTRAN, PORTNUM: FAR PTR WORD, X:FAR PTR WORD

        .DATA

Notes   LABEL   WORD
; D
        DW      65009
        DB      +1
        DB      1
; D#, D+, E-
        DW      61360
        DB      0
        DB      0
; E
        DW      57917
        DB      0
        DB      0
; F
        DW      54666
        DB      0
        DB      0
; F#, F+, G-
        DW      51598
        DB      -1
        DB      2
; G
        DW      48702
        DB      -1
        DB      2
; G#, G+, A-
        DW      45968
        DB      +1
        DB      5
; A
        DW      43388
        DB      0
        DB      0
; A#, A+, B-
        DW      40953
        DB      +1
        DB      1
; B
        DW      38655
        DB      -1
        DB      1
; C
        DW      36485
        DB      +1
        DB      1
; C#, C+, D-
        DW      34437
        DB      +1
        DB      1

        .DATA?

        ;PRIVATE         DelayCnt,
        DelayCnt        SDWORD ?          ; 32-bit counter
        keyboard_type   BYTE ?
        is_last         BYTE ?
        last            BYTE ?

        .CODE

OPTION PROLOGUE:NONE
OPTION EPILOGUE:NONE

Delayloop PROC NEAR
; input:
;   es:di = 40h:6ch
;   bx    = value of es:dx before the call
;   dx:ax = counter
LDelayLoop1:
        sub ax, 1
        sbb dx, 0
        jc  LDelayLoop2
        cmp bx, WORD PTR es:[di]
        je  LDelayLoop1
LDelayLoop2:
        ret
Delayloop ENDP

nosound PROC FAR
        in  al, 61h
        and al, 0FCh
        out 61h, al
        ret
nosound ENDP

isize   EQU     2

mbinmul PROC NEAR
        ; DS:SI - pointer to first number
        ; DS:DI - pointer to second number
        ; DS:BX - location of the result
        push    si
        push    di
; clear result buffer
        push    bx              ; save result pointer BX
        mov     ax, 0           ; get a zero
        mov     cx, 2*isize     ; double precision for this number
        cld                     ; forward direction
mbinmul1:
        mov     [bx], ax        ; clear the "digit"
        inc     bx              ; point to next "digit"
        inc     bx
        loop    mbinmul1        ; loop through all "digits"
        pop     bx              ; restore result pointer BX
;
        mov     cx, isize       ; get the number of 16-bit "digits"
        push    bx
mbinmul2:
        push    cx              ; save count for outer loop
        mov     dx, [si]        ; get "digit" from first number
        inc     si              ; point to next "digit"
        inc     si
;
        push    bx              ; save registers during inner loop
        push    di
;
        mov     cx, isize       ; get the number of 16-bit "digits"
mbinmul3:
        push    cx              ; save count for inner loop
        push    dx              ; save multiplier "digit"
        mov     ax, [di]        ; get "digit" from second number
        inc     di              ; point to next "digit"
        inc     di
        mul     dx              ; multiply
        add     [bx], ax        ; add lower "digit" to result
        inc     bx              ; point to next "digit"
        inc     bx
        adc     [bx], dx        ; add upper part to result
        pop     dx              ; restore multiplier
        pop     cx              ; restore count for inner loop
        loop    mbinmul3        ; loop through all "digits" of second
;
        pop     di              ; restore registers
        pop     bx
;
        inc     bx              ; shift by one "digit"
        inc     bx
        pop     cx              ; restore count for outer loop
        loop    mbinmul2        ; loop through all "digits" of first
        pop     bx
        pop     di
        pop     si
        ret
mbinmul ENDP

; LOCAL SUBROUTINE TO COMPARE DIVISOR AGAINST DIVIDEND
;
divcmp  proc    near
;
        push    si              ; save registers
        push    di
        push    cx
;
        std                     ; backward direction
        add     si, 4*isize-2   ; point to end of temp divisor
        add     di, 4*isize-2   ; point to end of quotient
        mov     cx, 2*isize     ; count for double precision
;
        repz    cmpsw           ; compare "digit" by "digit"
;
        pop     cx              ; restore registers
        pop     di
        pop     si
        ret
;
divcmp  endp
;
;
; LOCAL SUBROUTINE TO ARITHMETIC SHIFT DIVISOR LEFT
;
divsal  proc    near
;
        push    si              ; save registers
        push    cx
;
        mov     cx, 2*isize     ; set counter
        clc                     ; clear carry in
divsal1:
        rcl     word ptr [si],1 ; shift one word by one bit
        inc     si              ; point to next word
        inc     si
        loop    divsal1         ; loop through entire divisor
;
        pop     cx              ; restore registers
        pop     si
        ret
;
divsal endp
;
;
; LOCAL SUBROUTINE TO LOGICAL SHIFT DIVISOR RIGHT
;
divslr  proc    near
;
        push    si              ; save registers
        push    cx
;
        add     si, 4*isize-2   ; point to end of temp divisor
        mov     cx, 2*isize     ; count for double precision
        clc                     ; clear carry in
divslr1:
        rcr     word ptr [si],1 ; rotate one word by one bit
        dec     si              ; point to next word
        dec     si
        loop    divslr1         ; loop through entire divisor
;
        pop     cx              ; restore registers
        pop     si
        ret
;
divslr  endp
;
;
; LOCAL SUBROUTINE TO SUBTRACT SHIFTED DIVISOR FROM DIVIDEND
;
divsub  proc    near
;
        push    si              ; save registers
        push    di
        push    cx
;
        clc                     ; clear carry in
        mov     cx, 2*isize     ; set the count for double precision
divsub1:
        mov     ax, [si]        ; get word from shifted divisor
        inc     si              ; point to next word
        inc     si
        sbb     [di], ax        ; subtract from word of dividend
        inc     di              ; point to next word
        inc     di
        loop    divsub1         ; loop through all words
;
        pop     cx              ; restore registers
        pop     di
        pop     si
        ret
;
divsub  endp
;
; LOCAL SUBROUTINE TO SHIFT QUOTIENT LEFT
;
quotshl proc    near
;
        push    bx              ; save registers
        push    cx
;
        mov     cx, isize       ; count for single precision
quotshl1:
        rcl     word ptr [bx],1 ; shift word of quotient left once
        inc     bx              ; point to next word
        inc     bx
        loop    quotshl1        ; loop through entire quotient
;
        pop     cx              ; restore registers
        pop     bx
        ret
;
quotshl endp
;
;
; ROUTINE TO DIVIDE MULTIDIGIT BINARY NUMBERS
;
mbindiv proc    near
        ; DS:SI - divisor
        ; DS:DI - dividend, remainder
        ; DS:BX - quotient
        push    bp
        mov     bp,sp
        sub     sp, isize*4
        push    si              ; Save registers
        push    di
;
; put single precision divisor into double precision location
        push    di                              ; save dividend pointer
        lea     di, WORD PTR [bp-isize*4]       ; point to temporary divisor
        mov     cx, isize                       ; for a count of isize
        cld                                     ; forward direction
        rep     movsw                           ; make the transfer
;
; clear upper part of double precision location
        mov     ax, 0           ; zero word
        mov     cx, isize       ; for a count of isize
        rep     stosw           ; clear the rest of the words
;
; restore dividend pointer and point to temp divisor
        pop     di                              ; restore dividend pointer
        lea     si, WORD PTR [bp-isize*4]       ; point SI to temporary divisor
;
; initialize shift count
        mov     cx, 1           ; initial count of one
;
; normalize divisor
mbindiv1:
        test    WORD PTR [bp-2], 8000h  ; test msb of divisor
        jnz     mbindiv2                ; exit if normalized
        call    divsal                  ; arithmetic shift left if not
        inc     cx                      ; count the shift
        jmp     mbindiv1                ; keep on looping until normalized
;
; compare, subtract, shift loop
mbindiv2:
        call    divcmp          ; compare divisor against dividend
        ja      mbindiv3        ; skip if too large
        call    divsub          ; subtract if ok
        stc                     ; new bit of quotient is 1
        jmp     mbindiv4        ; jump to end of loop
;
mbindiv3:
        clc                     ; new bit of quotient is 0
mbindiv4:
        call    quotshl         ; shift bit into the quotient
        call    divslr          ; logical shift divisor right once
        loop    mbindiv2        ; loop for next digit
;
        pop     di              ; restore registers
        pop     si
        mov     sp, bp
        pop     bp
        ret
;
mbindiv endp

OPTION PROLOGUE:PrologueDef
OPTION EPILOGUE:EpilogueDef

PEEKB   PROC FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD
        les     bx, SEGM
        mov     ax, es:[bx]
        les     bx, OFS
        mov     bx, es:[bx]
        mov     es, ax
        mov     al, es:[bx]
        ret
PEEKB   ENDP

PEEKW   PROC FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD
        les     bx, SEGM
        mov     ax, es:[bx]
        les     bx, OFS
        mov     bx, es:[bx]
        mov     es, ax
        mov     ax, es:[bx]
        ret
PEEKW   ENDP

PEEKL   PROC FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD
        les     bx, SEGM
        mov     ax, es:[bx]
        les     bx, OFS
        mov     bx, es:[bx]
        mov     es, ax
        mov     ax, es:[bx]
        mov     dx, es:[bx+2]
        ret
PEEKL   ENDP

POKEB   PROC FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD, X:FAR PTR BYTE
        les     bx, SEGM
        mov     cx, es:[bx]
        les     bx, X
        mov     al, es:[bx]
        les     bx, OFS
        mov     bx, es:[bx]
        mov     es, cx
        mov     es:[bx], al
        ret
POKEB   ENDP

POKEW   PROC FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD, X:FAR PTR WORD
        les     bx, SEGM
        mov     cx, es:[bx]
        les     bx, X
        mov     ax, es:[bx]
        les     bx, OFS
        mov     bx, es:[bx]
        mov     es, cx
        mov     es:[bx], ax
        ret
POKEW   ENDP

POKEL   PROC FORTRAN, SEGM:FAR PTR WORD, OFS:FAR PTR WORD, X:FAR PTR DWORD
        les     bx, SEGM
        mov     cx, es:[bx]
        les     bx, X
        mov     ax, es:[bx]
        mov     dx, es:[bx+2]
        les     bx, OFS
        mov     bx, es:[bx]
        mov     es, cx
        mov     es:[bx], ax
        mov     es:[bx+2], dx
        ret
POKEL   ENDP

PORTINB PROC FORTRAN, PORTNUM:FAR PTR WORD
        les     bx, PORTNUM
        mov     dx, es:[bx]
        in      al, dx
        ret
PORTINB ENDP

PORTINW PROC FORTRAN, PORTNUM:FAR PTR WORD
        les     bx, PORTNUM
        mov     dx, es:[bx]
        in      ax, dx
        ret
PORTINW ENDP

PORTOUTB        PROC FORTRAN, PORTNUM: FAR PTR WORD, X:FAR PTR BYTE
        les     bx, PORTNUM
        mov     dx, es:[bx]
        les     bx, X
        mov     al, es:[bx]
        out     dx, al
        ret
PORTOUTB        ENDP

PORTOUTW        PROC FORTRAN, PORTNUM: FAR PTR WORD, X:FAR PTR WORD
        les     bx, PORTNUM
        mov     dx, es:[bx]
        les     bx, X
        mov     ax, es:[bx]
        out     dx, ax
        ret
PORTOUTW        ENDP

PLAYNUM PROC FORTRAN, N:FAR PTR WORD
        LOCAL   basefr:WORD
        LOCAL   notefix:SBYTE
        push    si
        les     bx, N
        mov     ax, es:[bx]
        sub     ax, 3
        jnc     LPlaynum1
        call    nosound
        jmp     LPlaynum_exit
LPlaynum1:
        mov     cl, 12
        div     cl
        mov     bx, ax
        xor     ax, ax
        mov     al, bh
        mov     si, ax
        shl     si, 1
        shl     si, 1
        mov     ax, WORD PTR Notes[si]
        mov     basefr, ax
        mov     ax, WORD PTR Notes[si+2]
        cmp     bl, ah
        je      LPlaynum2
        xor     ax, ax
LPlaynum2:
        mov     notefix, al
        xor     cx, cx
        mov     cl, bl
        or      cx, cx
        jz      LPlaynum_play
        mov     ax, basefr
        shr     ax, cl
        xchg    ax, basefr
        dec     cx
        mov     bx, 1
        shl     bx, cl
        mov     dx, bx
        shl     bx, 1
        dec     bx
        inc     cx
        and     ax, bx
        cmp     ax, dx
        jb      LPlaynum_play
        jz      LPlaynum3
        inc     basefr
        jmp     LPlaynum_play
LPlaynum3:
        mov     ax, basefr
        and     ax, 1
        add     basefr, ax
LPlaynum_play:
        in      al, 61h
        mov     ah, al
        and     ah, 3
        cmp     ah, 3
        je      LPlaynum_next
        or      al, 3
        out     61h, al
LPlaynum_next:
        mov     al, notefix
        cbw
        mov     cx, basefr
        add     cx, ax
        mov     al, 0B6h
        out     43h, al
        mov     al, cl
        out     42h, al
        mov     al, ch
        out     42h, al
LPlaynum_exit:
        pop     si
        ret
PLAYNUM ENDP


DetectKeyboard  PROC    NEAR
        mov     keyboard_type, 0
        mov     ax, 40h
        mov     es, ax
        mov     al, es:[96h]
        and     ax, 10h
        test    ax, ax
        jz      LDetKeyb_exit
        mov     ax, 1200h
        int     16h
        cmp     ax, 1200h
        jz      LDetKeyb_exit
        mov     keyboard_type, 10h
LDetKeyb_exit:
        ret
DetectKeyboard  ENDP

initdelay       PROC NEAR
        LOCAL   t1[2]:WORD
        LOCAL   t2[2]:WORD
        LOCAL   t3[4]:WORD
        push    si
        push    di
        pushf
        sti
        mov     ax, 40h
        mov     es, ax
        mov     di, 6Ch
        mov     bx, es:[di]
LInitDel1:
        cmp     bx, es:[di]
        je      LInitDel1
        mov     bx, es:[di]
        mov     ax, 0FFFFh
        mov     dx, 0FFFFh
        call    DelayLoop
        not     dx
        neg     ax
        sbb     dx, 0FFFFh
        sub     ax, 1
        sbb     dx, 0
        push    ss
        pop     es
        mov     t1[0], ax     ; LW
        mov     t1[2], dx   ; HW
        ;now we need to divide this by 54.92541649846559
        mov     t2[0], 0EC85h
        mov     t2[2], 0Ah
        push    ds
        push    ss
        pop     ds
        lea     si, t1[0]
        lea     di, t2[0]
        lea     bx, t3[0]
        call    mbinmul
        mov     t1[0], 0
        mov     t1[2], 258h
        xchg    di, bx
        call    mbindiv
        pop     ds
        ; t2 - result of the division
        ; t3 - remainder
        ; now rounding and shr
        mov     ax, t2[0]
        mov     dx, t2[2]
        cmp     t3[2], 12Ch
        jb      lab1
        ja      lab2            ; Adding 1 (skipping subtracting 1)
        cmp     t3[0], 0
        ja      lab2
        mov     bx, ax
        and     bx, 1
        add     ax, bx
        adc     dx, 0
lab1:
        sub     ax, 1 ; subtracting 1 after division
        sbb     dx, 0
lab2:
        mov     WORD PTR [DelayCnt], ax
        mov     WORD PTR [DelayCnt+2], dx
        pop     bx              ; Pop saved flags
        test    bh, 4           ; Test for direction flag
        jnz     LInitDel5
        cld
        jmp     LInitDel6
LInitDel5:
        std
LInitDel6:
        pop     di
        pop     si
        ret
initdelay ENDP

initCRT PROC    FORTRAN
        call    DetectKeyboard
        call    initdelay
        ret
initCRT ENDP

KeyPressed      PROC    FORTRAN
        cmp     is_last, 0
        jz      LKeyPress1
        mov     al, 1
        jmp     LKeyPress_exit
LKeyPress1:
        mov     ah, keyboard_type
        inc     ah
        int     16h
        mov     al, 0
        jz      LKeyPress_exit
        inc     al
LKeyPress_exit:
        ret
KeyPressed      ENDP

ISANYKEYDOWN    PROC    FORTRAN
        in      al, 60h
        not     al
        mov     cl, 7
        shr     al, cl
        ret
ISANYKEYDOWN    ENDP

LASTSCAN        PROC    FORTRAN
        in      al, 60h
        ret
LASTSCAN        ENDP

RDK_INT PROC    FORTRAN
        cmp     is_last, 0
        jz      LReadKey1
        mov     is_last, 0
        mov     al, last
        jmp     LReadKey_exit
LReadKey1:
        mov     ah, keyboard_type
        int     16h
        cmp     al, 0E0h
        jnz     LReadKey2
        cmp     ah, 0
        jz      LReadKey2
        mov     al, 0
LReadKey2:
        cmp     al, 0
        jnz     LReadKey_exit
        mov     is_last, 1
        mov     last, ah
LReadKey_exit:
        ret
RDK_INT ENDP

delay   PROC FORTRAN, MS:FAR PTR WORD
        push    di
        push    si
        les     bx, MS
        mov     cx, es:[bx]
        mov     ax, 40h
        mov     es, ax
        xor     di, di
        test    cx, cx
        jz      LDelay2
        mov     si, WORD PTR [DelayCnt+2]
        mov     bx, es:[di]
LDelay1:
        mov     ax, WORD PTR [DelayCnt]
        mov     dx, si
        call    DelayLoop
        loop    LDelay1
LDelay2:
        pop     si
        pop     di
        ret
delay   ENDP

sound   PROC FORTRAN, HZ:FAR PTR WORD
        les     bx, HZ
        mov     cx, es:[bx]
        mov     ax, 34DEh
        mov     dx, 12h
        cmp     cx, dx
        ja      LSound1
        call    nosound
        jmp     LSound_exit
LSound1:
        div     cx
        clc
        rcl     dx, 1
        jc      LSound2
        cmp     dx, cx
        jb      LSound4
        jz      LSound3
LSound2:
        inc     ax
        jmp     LSound4
LSound3:
        mov     dx, ax
        and     dx, 1
        add     ax, dx
LSound4:
        mov     cx, ax
        in      al, 61h
        mov     ah, al
        and     ah, 3
        cmp     ah, 3
        je      LSound_next
        or      al, 3
        out     61h, al
LSound_next:
        mov     al, 0B6h
        out     43h, al
        mov     al, cl
        out     42h, al
        mov     al, ch
        out     42h, al
LSound_exit:
        ret
sound   ENDP
        END
