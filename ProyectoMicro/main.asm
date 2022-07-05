; .SET ESCLAVO = 1

.equ F_CPU  = 16000000
.equ BAUD   = 9600
.equ BPS    = (F_CPU / 16 / BAUD) - 1 ;  baud prescale (USART)

.ORG 0x0000
    jmp start
.ORG 0x0024
    jmp USART0_RXC ; USART0, RX Complete Handler
; .ORG 0x0026
;     jmp USART0_UDRE ; USART0, UDR Empty Handler
.ORG 0x0028
    jmp USART0_TXC ; USART0, TX Complete Handle

.DSEG
    random_next:            .BYTE 1
    buffer:                 .BYTE 512
    tx_sec:                 .BYTE 1
    tx_index:               .BYTE 2
    suma:                   .BYTE 1
	digitos: 				.BYTE 10
    hamming_7_decode_table: .BYTE 128
    hamming_7_encode_table: .BYTE 16

.CSEG

start:
    cli
    call inicializar_sistema
.IFNDEF ESCLAVO
	rcall generar_512
.ENDIF
    sei
loop:
    rcall   suma_buffer
    rcall   display_numero
    rjmp    loop

; Suma 'buffer' y retorna el resultado en r16
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

putc:	                    ; La rutina se encarga de enviar un byte
    push    r17
_wait_putc:
    lds	    r17,     UCSR0A	    ; load UCSR0A into r17
	sbrs	r17,    UDRE0		; wait for empty transmit buffer
	rjmp	_wait_putc          ; repeat loop

	sts	    UDR0,   r16			; transmit character

    pop     r17
	ret					        ; return from subroutine

getc:	                ; La rutina se encarga de recibir un byte
    push    r17
_wait_getc:
    lds	    r17,    UCSR0A			; load UCSR0A into r17
	sbrs	r17,    UDRE0			; wait for empty transmit buffer
	rjmp	_wait_getc			; repeat loop

	lds	r16, UDR0			        ; get received character

    pop     r17
	ret					            ; return from subroutine


pasar_buffer:
    push    XH
    push    XL
    push    r16
    push    r17
    push    r18
    push    r19
    push    r20

    ldi     r20,    4
_cuatro_veces_pasar:    ; Esta rutina debe ser ejecutada 4 veces para alcanzar los 512 bytes
    ldi     r19,    128
_pasar_byte_pasar:      ; Dicha rutina pasa 128 bytes
    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)
    ld      r16,    X+
    rcall   hamming_7_encode
    mov     r18,    r17

    lsr     r16                ; Aplico shift a la derecha 4 veces
    lsr     r16                ;   para obtener los 4 bits siguientes
    lsr     r16
    lsr     r16

    rcall   hamming_7_encode
    mov     r16,    r17
	rcall	putc				; transmit character
    mov     r16,    r18
	rcall	putc				; transmit character

    dec     r19
    brne    _pasar_byte_pasar
    dec     r20
    brne    _cuatro_veces_pasar

    pop    XH
    pop    XL
    pop    r20
    pop    r19
    pop    r18
    pop    r17
    pop    r16
    ret

recibir_buffer:
    push    XH
    push    XL
    push    r16
    push    r17
    push    r18
    push    r19
    push    r20

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)

    ldi     r20,    4
_cuatro_veces_recibir:  ; Esta rutina debe ser ejecutada 4 veces para alcanzar los 512 bytes
    ldi     r19,    128
_pasar_byte_recibir:    ; Dicha rutina pasa 128 bytes
    rcall   getc
    rcall   hamming_7_decode
    mov     r18,    r17

    lsl     r18                ; Aplico shift a la derecha 4 veces
    lsl     r18                ;   para obtener los 4 bits siguientes
    lsl     r18
    lsl     r18

    rcall   getc
    rcall   hamming_7_decode
    or      r18, r17
    st      X+, r18

    dec     r19
    brne    _pasar_byte_recibir
    dec     r20
    brne    _cuatro_veces_recibir

    pop    XH
    pop    XL
    pop    r20
    pop    r19
    pop    r18
    pop    r17
    pop    r16
    ret

display_numero:
	rcall 	div_10
	mov   	r16, 	r24
	ldi   	r17, 	0b00010000
	rcall 	display_digit

	mov		r16,	r25
	rcall 	div_10
	mov   	r16, 	r24
	ldi   	r17, 	0b00100000
	rcall 	display_digit

	mov		r16,	r25
	rcall 	div_10
	mov   	r16, 	r24
	ldi   	r17, 	0b01000000
	rcall 	display_digit

	; mov		r16,	r25
	; rcall 	div_10
	; mov   	r16, 	r24
	; ldi   	r17, 	0b10000000
	; rcall 	display_digit
	ret

; dado un número en r16:
;	r24 es r16 % 10
;	r25 es r16 / 10
div_10:
	mov 	r24, 	r16
	ldi 	r25, 	0
	rjmp 	div_10_comparar
div_10_seguir:
	inc 	r25
	subi 	r24, 	10
div_10_comparar:
	cpi		r24, 	10
	brsh 	div_10_seguir
	ret

; Muestra el dígito en r16  0 <= r16 <= 9,
; r17 es el display a usar (ej: 0b00010000)
display_digit:
	push 	XL
	push 	XH

	ldi		XL,		LOW(digitos)
	ldi		XH,		HIGH(digitos)
	add		XL,		r16
	clr		r16
	adc		XH,		r16
	ld 		r16,	X

	pop		XH
	pop		XL
sacanum:
	rcall	dato_serie
	mov		r16, r17
	rcall	dato_serie
	sbi		PORTD, 4		;PD.4 a 1, es LCH el reloj del latch
	cbi		PORTD, 4		;PD.4 a 0,
	ret
	;Voy a sacar un byte por el 7seg
dato_serie:
	push	r18

	ldi		r18, 0x08		; lo utilizo para contar 8 (8 bits)
loop_dato1:
	cbi		PORTD, 7		;SCLK = 0 reloj en 0
	lsr		r16				;roto a la derecha r16 y el bit 0 se pone en el C
	brcs	loop_dato2		;salta si C=1
	cbi		PORTB, 0		;SD = 0 escribo un 0
	rjmp	loop_dato3
loop_dato2:
	sbi		PORTB, 0		;SD = 1 escribo un 1
loop_dato3:
	sbi		PORTD, 7		;SCLK = 1 reloj en 1
	dec		r18
	brne	loop_dato1; cuando r17 llega a 0 corta y vuelve

	pop		r18
	ret

initUSART:
    push    r16

    ldi	    r16,    LOW(bps)			    ; load baud prescale
	sts	    UBRR0L, r16			            ; load baud prescale
    ldi	    r16,    HIGH(bps)			    ; load baud prescale
	sts	    UBRR0H, r16			            ; to UBRR0

	ldi	    r16, (1 << RXEN0) | (1 << TXEN0) | (1 << RXCIE0) | (1 << TXCIE0) ; | (1 << UDRIE0)
	sts	    UCSR0B, r16

    ; ldi     r16,    (1<<USBS0)|(3<<UCSZ00) ; Set frame format: 8data, 2stop bit
    ; sts     UCSR0C, r16

    pop r16
	ret

initDISPLAY:
    push    r16
    push    r17

    ldi		r16,	0b00111101
	out		DDRB,	r16			;4 LEDs del shield son salidas
	out		PORTB,	r16			;apago los LEDs
	ldi		r16,	0b00000000
	out		DDRC,	r16			;3 botones del shield son entradas
	ldi		r16,	0b10010000
	out		DDRD,	r16			;configuro PD.4 y PD.7 como salidas
	cbi		PORTD,	7			;PD.7 a 0, es el reloj serial, inicializo a 0
	cbi		PORTD,	4			;PD.4 a 0, es el reloj del latch, inicializo a 0
	ldi		r16,	0b11111111
	ldi		r17,	0b11110000
	call	sacanum

    pop     r17
    pop     r16
    ret

rx_siguiente_nibble:
	push    r16
	push    r17
	push    r18
    push    XH
    push    XL
    push    YH
    push    YL

    ldi     XL,     LOW(tx_sec)
    ldi     XH,     HIGH(tx_sec)
    ld      r16,    X
    cpi     r16,    0
    breq    _rx_nibble_bajo
_rx_nibble_alto:
    ldi     XL,     LOW(tx_index)
    ldi     XH,     HIGH(tx_index)
    ld      r16,    X+
    ld      r17,    X

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)
    add     XL,     r16
    adc     XH,     r17

    ldi     r18,    1
    add     r16,    r18
    clr     r18
    adc     r17,    r18
    andi    r17,    0b00000001
    ldi     YL,     LOW(tx_index)
    ldi     YH,     HIGH(tx_index)
    st      Y+,     r16
    st      Y,      r17

    ld      r18,    X
    lds     r16,    UDR0
    rcall   hamming_7_decode
    lsl     r17
    lsl     r17
    lsl     r17
    lsl     r17
    or      r18,    r17
    st      X,      r18
    rjmp    _rx_siguiente_nibble_salir
_rx_nibble_bajo:
    ldi     XL,     LOW(tx_index)
    ldi     XH,     HIGH(tx_index)
    ld      r16,    X+
    ld      r17,    X

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)
    add     XL,     r16
    adc     XH,     r17

    lds     r16,    UDR0
    rcall   hamming_7_decode
    st      X,      r17
_rx_siguiente_nibble_salir:
    ldi     XL,     LOW(tx_sec)
    ldi     XH,     HIGH(tx_sec)
    ld      r16,    X
    ldi     r17,    1
    eor     r16,    r17
    st      X,      r16
    pop     YL
    pop     YH
    pop     XL
    pop     XH
	pop     r18
	pop     r17
	pop	    r16
	ret

USART0_RXC:
    push    r16
    in		r16,	SREG

    out		SREG,	r16
    pop     r16
    reti

; USART0_UDRE:
;     push    r16
;     push    r17
;     in		r16,	SREG
;
;     ldi     r17, 156
; 	sts	    UDR0, r17
;
;     out		SREG,	r16
;     pop     r17
;     pop     r16
;     reti

tx_siguiente_nibble:
	push    r16
	push    r17
	push    r18
    push    XH
    push    XL
    push    YH
    push    YL

    ldi     XL,     LOW(tx_sec)
    ldi     XH,     HIGH(tx_sec)
    ld      r16,    X
    cpi     r16,    0
    breq    _nibble_bajo
_nibble_alto:
    ldi     XL,     LOW(tx_index)
    ldi     XH,     HIGH(tx_index)
    ld      r16,    X+
    ld      r17,    X

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)
    add     XL,     r16
    adc     XH,     r17

    ldi     r18,    1
    add     r16,    r18
    clr     r18
    adc     r17,    r18
    andi    r17,    0b00000001
    ldi     YL,     LOW(tx_index)
    ldi     YH,     HIGH(tx_index)
    st      Y+,     r16
    st      Y,      r17

    ld      r16,    X
    lsr     r16
    lsr     r16
    lsr     r16
    lsr     r16
    rcall   hamming_7_encode
    sts     UDR0,   r17

    rjmp    _tx_siguiente_nibble_salir
_nibble_bajo:
    ldi     XL,     LOW(tx_index)
    ldi     XH,     HIGH(tx_index)
    ld      r16,    X+
    ld      r17,    X

    ldi     XL,     LOW(buffer)
    ldi     XH,     HIGH(buffer)
    add     XL,     r16
    adc     XH,     r17

    ld      r16,    X
    rcall   hamming_7_encode
    sts     UDR0,   r17
_tx_siguiente_nibble_salir:
    ldi     XL,     LOW(tx_sec)
    ldi     XH,     HIGH(tx_sec)
    ld      r16,    X
    ldi     r17,    1
    eor     r16,    r17
    st      X,      r16
    pop     YL
    pop     YH
    pop     XL
    pop     XH
	pop     r18
	pop     r17
	pop	    r16
	ret

USART0_TXC:
    push    r16
    in		r16,	SREG

    rcall   tx_siguiente_nibble

    out		SREG,	r16
    pop     r16
    reti

inicializar_sistema:
    push    XL
    push    XH
    push    r16

    rcall   initUSART
    rcall   initDISPLAY

    ldi     XL,     LOW(tx_sec)
    ldi     XH,     HIGH(tx_sec)
    ldi     r16,    0
    st      X,      r16

    ldi     XL,     LOW(tx_index)
    ldi     XH,     HIGH(tx_index)
    ldi     r16,    0
    st      X+,     r16
    st      X+,     r16

    ldi     XL,     LOW(random_next)
    ldi     XH,     HIGH(random_next)
    ldi     r16,    1
    st      X,      r16

	ldi		XL,		LOW(digitos)
	ldi		XH,		HIGH(digitos)
	ldi		r16,	0b00000011 ; 0
	st		X+,		r16
	ldi		r16,	0b10011111 ; 1
	st		X+,		r16
	ldi		r16,	0b00100101 ; 2
	st		X+,		r16
	ldi		r16,	0b00001101 ; 3
	st		X+,		r16
	ldi 	r16,	0b10011001 ; 4
	st		X+,		r16
	ldi		r16,	0b01001001 ; 5
	st		X+,		r16
	ldi		r16,	0b01000001 ; 6
	st		X+,		r16
	ldi		r16,	0b00011111 ; 7
	st		X+,		r16
	ldi		r16,	0b00000001 ; 8
	st		X+,		r16
	ldi		r16,	0b00011001 ; 9
	st		X+,		r16

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

    pop    r16
    pop    XH
    pop    XL
    ret
