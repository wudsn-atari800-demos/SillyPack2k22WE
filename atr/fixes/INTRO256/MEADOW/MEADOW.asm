; "Meadow", Atari XL/XE Intro 256B
;
; code by tr1x / Agenda
; requirements: original Atari XL/XE 64KB PAL
; version: 1.0
; release date: 2022-12-10

scradr	equ $00
btemp	equ $02
growth	equ $0e

fntadr	equ $4c00

rtclok	equ $0012
chbas	equ $02f4
colpf0	equ $d016
colpf2	equ $d018
consol	equ $d01f
random	equ $d20a
wsync	equ $d40a
vcount	equ $d40b
osgraph	equ $ef9c


	org $0080

start	lda #$0c
	jsr osgraph

	sta rtclok+2

	ldx #$4c
	stx chbas			; X=$0c after "jsr osgraph"

	; vertical flip of plants
	ldx #(plantse-plants)-1
loopa1	ldy #$05
	lda plants,x
	sta fntadr+8,x
loopa2	lsr
	php
	lsr
	rol btemp
	plp
	rol btemp
	dey
	bne loopa2
	lda btemp
	sta fntadr+8+64*8,x
	dex
	bpl loopa1

	; set colors of flowers
loopb1	lsr
	lsr
	lsr
	tax
	lda $d86a,x
	sta colpf0
	eor #$50
	sta colpf2
	lda vcount
	bne loopb1

	lda growth
	bne skipb1

	; reset of the meadow
	lda rtclok+2
	beq start

	; initialization of growing a new flower
	lda random
	cmp #$14
	bcs loopb1
	asl
	;clc			; not necessary
	adc #$38-$28
	sta scradr
	lda #$bf
	sta scradr+1
	ldy #$28
	lda (scradr),y
	bne loopb1
	sta rtclok+2
	lda random
	and #%00001110
	;clc			; not necessary
	adc #$08
	sta growth

	; grow stem with leaves
skipb1	lda random
	and #%00000001
	clc
	adc #$01
	sta (scradr),y
	lda random
	and #%00000010
	ora #$40
	iny
	sta (scradr),y
	dey
	lda scradr
	;sec			; not necessary
	sbc #$27
	sta scradr
	bcs skipb2
	dec scradr+1

	; put flower's head
skipb2	dec growth
	bne loopb1
	lda random
	and #%00000011
	asl
	;clc			; not necessary
	adc #$03
	sta (scradr),y
	ora #$40
	tax
	iny
	sta (scradr),y
	tya
	;sec			; not necessary
	sbc #$27
	tay
	inx
	txa
	sta (scradr),y
	and #%00111111
	dey
	sta (scradr),y

	sty consol		; sound

	jmp loopb1


plants	.byte $02,$02,$02,$02,$02,$02,$02,$02	; stem
	.byte $02,$82,$a2,$aa,$2a,$2a,$0a,$02	; stem + left leaf
	.byte $4e,$4f,$53,$53,$14,$14,$05,$01	; flower #1, bottom left
	.byte $00,$05,$15,$14,$54,$53,$53,$4f	; flower #1, top left
	.byte $f4,$06,$01,$01,$0d,$33,$03,$03	; flower #2, bottom left
	.byte $00,$03,$33,$0d,$01,$01,$06,$f4	; flower #2, top left
	.byte $09,$0a,$72,$50,$13,$02,$02,$02	; flower #3, bottom left
	.byte $00,$04,$05,$01,$13,$50,$72,$0a	; flower #3, top left
	.byte $d5,$35,$35,$35,$35,$0d,$0d,$03	; flower #4, bottom left
	.byte $00,$c3,$c3,$f3,$dd,$dd,$dd,$d5	; flower #4, top left
plantse
	run start
