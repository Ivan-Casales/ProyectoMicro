.ORG 0x0000
    jmp start

.DSEG
    random_next:            .BYTE 1
    buffer:                 .BYTE 512
    hamming_7_decode_table: .BYTE 128
    hamming_7_encode_table: .BYTE 16

.CSEG

start:
    ; Inicializar sistema

    ldi     XL,     LOW(random_next)
    ldi     XH,     HIGH(random_next)
    ldi     r16,    1
    st      X,      r16

    ldi     XL,     LOW(hamming_7_decode_table)
    ldi     XH,     HIGH(hamming_7_decode_table)

    ldi     r16, 0b00000000 ; Index: 00000000
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 00000001
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 00000010
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00000011
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 00000100
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00000101
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00000110
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00000111
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 00001000
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00001001
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00001010
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 00001011
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 00001100
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00001101
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00001110
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00001111
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 00010000
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00010001
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 00010010
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00010011
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00010100
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 00010101
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00010110
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00010111
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00011000
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00011001
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00011010
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00011011
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00011100
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00011101
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00011110
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00011111
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 00100000
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 00100001
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00100010
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00100011
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00100100
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00100101
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 00100110
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 00100111
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00101000
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00101001
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00101010
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00101011
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00101100
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00101101
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00101110
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00101111
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00110000
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00110001
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00110010
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00110011
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00110100
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00110101
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00110110
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00110111
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 00111000
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 00111001
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 00111010
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 00111011
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 00111100
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 00111101
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 00111110
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 00111111
    st      X+,     r16
    ldi     r16, 0b00000000 ; Index: 01000000
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01000001
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01000010
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01000011
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01000100
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01000101
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01000110
    st      X+,     r16
    ldi     r16, 0b00000111 ; Index: 01000111
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01001000
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01001001
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01001010
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01001011
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01001100
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01001101
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01001110
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01001111
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01010000
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01010001
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01010010
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01010011
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01010100
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01010101
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01010110
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01010111
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01011000
    st      X+,     r16
    ldi     r16, 0b00001001 ; Index: 01011001
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01011010
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01011011
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01011100
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01011101
    st      X+,     r16
    ldi     r16, 0b00001110 ; Index: 01011110
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01011111
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01100000
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01100001
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01100010
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01100011
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01100100
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01100101
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01100110
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01100111
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01101000
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01101001
    st      X+,     r16
    ldi     r16, 0b00001010 ; Index: 01101010
    st      X+,     r16
    ldi     r16, 0b00001011 ; Index: 01101011
    st      X+,     r16
    ldi     r16, 0b00001100 ; Index: 01101100
    st      X+,     r16
    ldi     r16, 0b00001101 ; Index: 01101101
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01101110
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01101111
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01110000
    st      X+,     r16
    ldi     r16, 0b00000001 ; Index: 01110001
    st      X+,     r16
    ldi     r16, 0b00000010 ; Index: 01110010
    st      X+,     r16
    ldi     r16, 0b00000011 ; Index: 01110011
    st      X+,     r16
    ldi     r16, 0b00000100 ; Index: 01110100
    st      X+,     r16
    ldi     r16, 0b00000101 ; Index: 01110101
    st      X+,     r16
    ldi     r16, 0b00000110 ; Index: 01110110
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01110111
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01111000
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01111001
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01111010
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01111011
    st      X+,     r16
    ldi     r16, 0b00001000 ; Index: 01111100
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01111101
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01111110
    st      X+,     r16
    ldi     r16, 0b00001111 ; Index: 01111111
    st      X+,     r16

    ldi     XL,     LOW(hamming_7_encode_table)
    ldi     XH,     HIGH(hamming_7_encode_table)

    ldi     r16, 0b00000000 ; Valor fuente: 0000
    st      X+,     r16
    ldi     r16, 0b01100001 ; Valor fuente: 0001
    st      X+,     r16
    ldi     r16, 0b01010010 ; Valor fuente: 0010
    st      X+,     r16
    ldi     r16, 0b00110011 ; Valor fuente: 0011
    st      X+,     r16
    ldi     r16, 0b00110100 ; Valor fuente: 0100
    st      X+,     r16
    ldi     r16, 0b01010101 ; Valor fuente: 0101
    st      X+,     r16
    ldi     r16, 0b01100110 ; Valor fuente: 0110
    st      X+,     r16
    ldi     r16, 0b00000111 ; Valor fuente: 0111
    st      X+,     r16
    ldi     r16, 0b01111000 ; Valor fuente: 1000
    st      X+,     r16
    ldi     r16, 0b00011001 ; Valor fuente: 1001
    st      X+,     r16
    ldi     r16, 0b00101010 ; Valor fuente: 1010
    st      X+,     r16
    ldi     r16, 0b01001011 ; Valor fuente: 1011
    st      X+,     r16
    ldi     r16, 0b01001100 ; Valor fuente: 1100
    st      X+,     r16
    ldi     r16, 0b00101101 ; Valor fuente: 1101
    st      X+,     r16
    ldi     r16, 0b00011110 ; Valor fuente: 1110
    st      X+,     r16
    ldi     r16, 0b01111111 ; Valor fuente: 1111
    st      X+,     r16

    ldi     r16,    13
    rcall   hamming_7_encode
	mov		r16,	r17
	rcall	hamming_7_decode

loop:
    rcall   generar_512
    rcall   suma_buffer
    rjmp    loop

suma_buffer:
    push    XH
    push    XL
    push    r17
    push    r18

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)

    ldi     r16,    0
    ldi     r17,    128
_suma_buffer_loop:
    ld      r18,    X+
    add     r16,    r18

    ld      r18,    X+
    add     r16,    r18

    ld      r18,    X+
    add     r16,    r18

    ld      r18,    X+
    add     r16,    r18

    dec     r17
    brne    _suma_buffer_loop

    pop     r18
    pop     r17
    pop     XL
    pop     XH
    ret

generar_512:
    push    XH
    push    XL
    push    r17
    push    r16

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)

    ldi     r17,    128
_generar_512_loop:
    rcall   random
    st      X+,     r16
    rcall   random
    st      X+,     r16
    rcall   random
    st      X+,     r16
    rcall   random
    st      X+,     r16

    dec     r17
    brne    _generar_512_loop

    pop     r16
    pop     r17
    pop     XL
    pop     XH
    ret

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

hamming_7_encode:
    push    XL
    push    XH

    ldi     XL,     LOW(hamming_7_encode_table)
    ldi     XH,     HIGH(hamming_7_encode_table)

    mov     r17,    r16
    andi    r17,    0b00001111
    add     XL,     r17
    ldi     r17,    0
    adc     XH,     r17
    ld      r17,    X

    pop     XH
    pop     XL
    ret

hamming_7_decode:
    push    XL
    push    XH

    ldi     XL,     LOW(hamming_7_decode_table)
    ldi     XH,     HIGH(hamming_7_decode_table)

    mov     r17,    r16
    andi    r17,    0b01111111
    add     XL,     r17
    ldi     r17,    0
    adc     XH,     r17
    ld      r17,    X

    pop     XH
    pop     XL
    ret
