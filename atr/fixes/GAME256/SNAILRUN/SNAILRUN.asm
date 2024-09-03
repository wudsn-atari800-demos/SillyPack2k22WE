; "Snail Run", Atari XL/XE Game 256B
;
; code by tr1x / Agenda
; requirements: original Atari XL/XE 64KB PAL
; version: 1.0
; release date: 2022-12-10

prevst1	equ $0000
prevst2	equ $0001
posx1	equ $0002
posx2	equ $0003

pmgadr	equ $3a00		; PMG address

sdmctl	equ $022f
gprior	equ $026f
stick0	equ $0278
stick1	equ $0279
strig0	equ $0284
hposp0	equ $d000
hposp1	equ $d001
hposp2	equ $d002
hposp3	equ $d003
hposm0	equ $d004
grafm	equ $d011
consol	equ $d01f
colpm0	equ $d012
gractl	equ $d01d
pmbase	equ $d407
vcount	equ $d40b
osgraph	equ $ef9c


	org $0082

;	bvc start

wintxt	.byte "player X won"

colors1	.byte $3a, $72, $3a, $72
colors2	.byte $2a, $a2, $2a, $a2

snail	.byte $10, $24, $92, $92, $df, $40, $ea, $e4, $e2, $d2, $c8, $a8, $46, $2c, $60, $c8
	.byte $1b, $3b, $36, $b6, $bb, $db, $40, $0a, $4c, $0c, $1a, $1a, $3c, $f0, $f8, $f0
	.byte $00, $1f, $3d, $32, $6d, $7f, $5e, $12, $5b, $6a, $77, $36, $1d, $00, $2a, $14
	.byte $00, $0e, $03, $21, $00, $44, $02, $40, $02, $00, $20, $18, $00, $20, $55;, $eb


start	ldx #$3f
loopa1	txa
	and #%00001111
	tay
	php
	lda snail,x
opcoda1	sta pmgadr+$250,y
opcoda2	sta pmgadr+$2a0,y
	plp
	bne skipa1
	inc opcoda1+2
	inc opcoda2+2
skipa1	dex
	bpl loopa1

start2	lda #$02
	jsr osgraph

	lda #>pmgadr
	sta pmbase
	sta gractl
	sta sdmctl
	sta gprior
	sta posx1
	sta posx2
	sta grafm

	lda #$d1
	sta hposm0

	sta opcodb1		; "cmp ($xx),y"

loopb1	lda vcount
	bne loopb1

	lda posx1
	ldy #<colors1
	jsr misc

loopb2	bit vcount
	bvc loopb2

	lda posx2
	ldy #<colors2
	jsr misc

	lda strig0
	beq start2

opcodb1	bne loopb1	

	tax			; "ldx #$01"
loopb3	lda stick0,x
	cmp prevst1,x
	beq skipb1
	inc posx1,x
	sta prevst1,x
	stx consol
	lda posx1,x
	cmp #$c1
	bcc skipb1
	txa
	adc #$10
	sta wintxt+7
	ldy #$0b
loopb4	lda wintxt,y
	sta $be70+20*5+4,y
	dey
	bpl loopb4
	dec opcodb1		; "bne xyz"
	dex
skipb1	dex
	bpl loopb3

	bmi loopb1		; infinite loop


misc	sty opcodx1+1
	sta hposp0
	sta hposp1
	clc
	adc #$08
	sta hposp2
	sta hposp3
	ldx #$03
loopx1	*
opcodx1	lda colors1,x
	sta colpm0,x
	dex
	bpl loopx1
	rts
	
	run start
