; "Ninja", Atari XL/XE Intro 256B
;
; A tribute to classic video game of the same title.
;
; code by tr1x / Agenda
; requirements: original Atari XL/XE 64KB PAL
; version: 1.0
; release date: 2022-12-10

tmp1	equ $0000
tmp2	equ $0001

rowcrs	equ $0054
colcrs	equ $0055
oldrow	equ $005a
oldcol	equ $005b
sdmctl	equ $022f
gprior	equ $026f
atachr	equ $02fb
hposp0	equ $d000
hposp1	equ $d001
colpf0	equ $d016
colpf1	equ $d017
colbk	equ $d01a
gractl	equ $d01d
pmbase	equ $d407
wsync	equ $d40a
vcount	equ $d40b
osgraph	equ $ef9c
osdraw	equ $f9c2


	org $0082

;	bvc start

bgcolry	.byte $63, $48, $43, $32, $23

bgcolor	.byte $02, $04, $04, $81, $81, $97

dtax0	.byte $08, $2e, $4b, $4c, $4a, $48, $09, $09	; x0
	.byte $09, $01, $70, $4f, $4d, $4d, $4b

fgcolor	.byte ;$00, $00
	.byte $28, $24, $24, $28, $d4

dtay0	.byte $2c, $2c, $19, $19, $1d, $18, $2d, $31	; y0
	.byte $35, $4f, $3a, $3c, $44, $4d, $3d

dtawh	.byte $06, $06, $04, $04, $40, $51, $f0, $f0	; width:height
	.byte $f0, $d0, $bb, $20, $01, $10, $22


start	lda #$07
	jsr osgraph

	sty atachr

	; draw mountains
	lda #$15
	sta rowcrs
loopa1	*
opcoda1	ror $c665
	dec rowcrs
	bcs skipa1
	inc rowcrs
	inc rowcrs
skipa1	inc opcoda1+1
	lda #$14
	sta oldrow
	jsr osdraw
	inc colcrs
	inc oldcol
	bne loopa1

	; draw lines
loopb1	ldx #$0e
	stx loopb2+1
loopb2	ldx #$00
	lda dtax0,x
	sta colcrs
	sta tmp1
	lda dtay0,x
	sta rowcrs
	lda dtawh,x
	lsr
	lsr
	lsr
	lsr
	asl
	adc colcrs
	sta oldcol
	sta tmp2
	lda dtawh,x
	and #%00001111
	asl
	adc rowcrs
	sta oldrow
	pha
	jsr osdraw

	; horizontal flip
	pla
	sta oldrow
	lda #$a0
	sbc tmp2
	sta oldcol
	lda #$a0
	sbc tmp1
	sta colcrs
	jsr osdraw

	dec loopb2+1
	bpl loopb2

skipb1	dec dtax0+1
	dec dtax0

	bpl loopb1

	; PMG setup
	lda #$6e
	sta sdmctl
	sta pmbase
	ror gractl
	rol gprior

	; colors
loopc1	lda vcount
	ldx #$00
loopc2	cmp bgcolry,x
	inx
	bcc loopc2
	sta wsync
	lda bgcolor-1,x
	sta colbk
	lda fgcolor-1,x
	sta colpf0
	bvc loopc1			; infinite loop


	; ninja, left side
	org $6e00+74

	.byte $1c, $3e, $22, $3e, $1c, $2b, $77, $ff
	.byte $fe, $df, $fe, $3f, $3e, $3f, $7f, $77
	.byte $73, $76, $36


	; ninja, right side
	org $6e80+72+8

	.byte $80, $c0, $c0, $c0, $80


	org hposp0

	.byte $7c, $84
	
	run start
