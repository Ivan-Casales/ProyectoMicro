.ORG 0x0000
    jmp start

.DSEG
    random_next: .BYTE 1

.CSEG

start:
    ; Inicializar sistema

    ldi     XL,     LOW(random_next)
    ldi     XH,     HIGH(random_next)
    ldi     r16,    1

loop:
    call    random
    rjmp    loop

random:
    push    r17
    push    XH
    push    XL

    ldi     XL,     LOW(random_next)
    ldi     XH,     HIGH(random_next)
    ld      r16,    X

    rol     r16
    rol     r16

    ldi     r17,    199
    eor     r16,    r17

    rol     r16
    ldi     r17,    101
    eor     r16,    r17

    ldi     r17, 5
    add     r16, r17

    st      X, r16

    pop     XL
    pop     XH
    pop     r17
    ret
