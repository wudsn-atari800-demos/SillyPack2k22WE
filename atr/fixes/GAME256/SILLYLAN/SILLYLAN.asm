; "Silly Lander", Atari XL/XE Game 256B

; code by SuN / Tight, tr1x / Agenda
; requirements: original Atari XL/XE 64KB PAL
; version: 1.0
; release date: 2022-12-10

velymax	equ $2e			; max landing y velocity
lndrsiz	equ $06			; lander's height

velx	equ $0083		; velocity x
vely	equ $0084		; velocity y
posxlo	equ $0085		; position x, lower byte
posylo	equ $0086		; position y, lower byte

lander	equ $e301		; lander's shape
lander2	equ $e1bf		; lander's shape

pmgadr	equ $4000

rtclok	equ $0012
rowcrs	equ $0054
colcrs	equ $0055
oldrow	equ $005a
oldcol	equ $005b
sdmctl	equ $022f
gprior	equ $026f
stick0	equ $0278
pcolr0	equ $02c0
atachr	equ $02fb
hposp0	equ $d000
hposp3	equ $d003
p0pf	equ $d004
p0pl	equ $d00c
colbk	equ $d01a
gractl	equ $d01d
hitclr	equ $d01e
consol	equ $d01f
audc1	equ $d201
random	equ $d20a
pmbase	equ $d407
wsync	equ $d40a
vcount	equ $d40b
osgraph	equ $ef9c
osdraw	equ $f9c2


;	org $87

;	bvc start
	org $89

posxhi	.byte $30		; position x, higher byte
posyhi	.byte $14		; position y, higher byte

veltab	.byte -1, -1		; velocities (-1, -1, 1)
velofs	.byte 1,0,0		; offsets (1, 0, 0)

stck	.byte %00000010, %00000100, %00001000

start	lda #$0f
	jsr osgraph

	lda #>pmgadr
	sta pmbase

	lsr gractl

	lda #%00111110
	sta sdmctl

	; set up gradient colors
	ldx #$c3
loopa1	txa
	lsr
	lsr
	lsr
	lsr
	ora #$20
	cpx random
	bcc skipa1
	adc #$01
skipa1	sta $501a,x
	dex
	bne loopa1

	inx
	stx gprior

	; draw mountains
	ldx #$03
	stx atachr
	lda #162
	sta rowcrs
loopb2	*
opcodb1	bit $c400
	dec rowcrs
	bvs skipb1
	inc rowcrs
	inc rowcrs
skipb1	inc opcodb1+1
	lda #$c0
	sta oldrow
	jsr osdraw
	inc colcrs
	inc oldcol
	bne loopb2

	; create landing site
	lda #$81
	sta hposp3
;	ldy #$b7
	lda #$ff
;	sta pmgadr+$700,y
;	sta pmgadr+$701,y
	sta pmgadr+$700+$b7
	sta pmgadr+$701+$b7
	sta pcolr0

respawn	stx posyhi

loopc1	ldy vcount
	sty hitclr
loopc2	bne loopc1
	sty audc1		; silence

	; show gradient    
loopc3	*
opcodc1	lda $5000,x
	sta wsync
	sta colbk
	tya
	sta pmgadr+$400,x
	inx
	cpx #$e0
	bcc loopc3

	lda posxhi
	sta hposp0

	; draw the lander
	ldy posyhi
	ldx #lndrsiz
loopc4	lda lander,x
	ora lander2,x
	sta pmgadr+$41e,y
	dey
	dex
	bpl loopc4

	; playfield collision (collision with mountains)
	lda p0pf
	beq skipc1
	bne respawn 

	; player collision (collision with landing site)
skipc1	lda p0pl
	beq skipc2

	lda #velymax
	cmp vely
	bcc respawn

	; gradient goes away
	inc opcodc1+1
	beq *

loopc5	bne loopc2
skipc2	*

	; joystick
	ldx #$02
loopc6	lda stck,x
	bit stick0
	bne skipc3
	sta audc1		; engines' sound
	lda veltab,x
	clc
	ldy velofs,x
	adc velx,y
	bvs skipc3
	sta velx,y
skipc3	dex
	bpl loopc6

	; gravity, every two frames
	lda rtclok+2
	lsr			; affects carry flag
	lda #$00
	adc vely
	bvs skipc4
	sta vely
skipc4	*

	; add velocity to position
	ldx #$01
loopc7	lda velx,x
	bpl skipc5
	dec posxhi,x
skipc5	clc
	adc posxlo,x
	sta posxlo,x
	bcc skipc6
	inc posxhi,x
skipc6	dex
	bpl loopc7

	bne loopc5		; infinite loop
	
	run start
