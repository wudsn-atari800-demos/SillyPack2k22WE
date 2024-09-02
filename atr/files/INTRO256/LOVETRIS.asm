; "Lovetris", Atari XL/XE Intro 256B
;
; code by tr1x / Agenda
; requirements: original Atari XL/XE 64KB PAL
; version: 1.0
; release date: 2022-12-10

cntr	equ $00
x	equ $01
y	equ $02
roto	equ $03

colcrs	equ $0055
rowcrs	equ $0054
color0	equ $02c4
color2	equ $02c6
atachr	equ $02fb
osplot	equ $f1d8
osgraph	equ $ef9c


	org $0080

	bvc start

brick0	.word %1111\
	       0000\
	       0000\
	       0000

	.word %0100\
	       0100\
	       0100\
	       0100

brick1	.word %0011\
	       0000\
	       0000\
	       0110

	.word %0010\
	       0000\
	       0001\
	       0011

brick2	.word %0110\
	       0000\ 
	       0000\
	       0011

	.word %0001\
	       0000\
	       0010\
	       0011

brick3	.word %0010\
	       0000\
	       0000\
	       1110

	.word %1100\
	       0000\
	       0100\
	       0100

	.word %1110\
	       0000\
	       0000\
	       1000

	.word %0100\
	       0000\
	       0110\
	       0100

brick4	.word %1000\
	       0000\ 
	       0000\
	       1111

	.word %0100\
	       0000\
	       1100\
	       0100

	.word %1110\
	       0000\
	       0000\
	       0010

; not used
;	.word %0110\
;	       0000\
;	       0100\
;	       0100

brick5	.word %0100\
	       0000\
	       0000\
	       1110

	.word %0100\
	       0000\
	       0100\
	       1100

	.word %0000\
	       0000\
	       0100\
	       1110

	.word %0100\
	       0000\
	       0100\
	       0110

brick6	.word %0110\
	       0000\ 
	       0000\
	       0110

howmany	.byte xy-brckrot-1

brckadr	.byte <brick0, <brick1, <brick2, <brick3
	.byte <brick4, <brick5, <brick6

	; high nibble: a brick (brick0 = 0, brick1 = 1, ...)
	;  low nibble: number of brick turns (0 = no turn, 1 = one turn, ...)
brckrot	.byte $21, $70, $21, $20, $42, $52, $63, $70
	.byte $61, $43, $62, $11, $30, $63, $70, $52
	.byte $21, $41, $63, $51, $70, $10, $60, $42
	.byte $31, $52, $20, $60

	; high nibble: X position
	;  low nibble: final Y position
xy	.byte $76, $45, $47, $a5, $b6, $25, $d7, $27
	.byte $78, $08, $98, $48, $a8, $19, $89, $d8
	.byte $1a, $5a, $6a, $8c, $6c, $b9, $bb, $4b
	.byte $8c, $8d, $4d, $7f


start	lda #$03
	jsr osgraph

loopa1	lda #$00
	sta y
	sta roto

loopa2	ldx howmany
	lda xy,x
	pha
	lsr
	lsr
	lsr
	lsr
	clc
	adc #$0a
	sta x

	pla
	and #%00001111
	cmp y
	bcs skipa1

	lda #$01
	jsr put
	dec howmany
	bpl loopa1
	bvc *			; "jmp *"

skipa1	ldx howmany
	lda brckrot,x
	and #%00001111
	beq skipa2
	dec brckrot,x
	inc roto
	inc roto

skipa2	lda brckrot,x
	ora #%00000111
	sta color2
	lsr
	lsr
	lsr
	lsr
	tax
	lda brckadr-1,x
	sta opcodz1+1

	; put a brick
	lda #$03
	jsr put

	; delay
	ldx #$fd
	jsr $ecb1

	; erase the brick
	jsr put

	inc y

	bvc loopa2		; "jmp loopa2"


	; put a brick
put	sta atachr
	lda #$0f
	sta color0

	sta cntr
loopz1	lda cntr
	lsr
	lsr
	lsr
	clc
	adc roto
	tax
	lda cntr
	and #%00000111
	tay
opcodz1	lda brick0,x
loopz2	asl
	dey
	bpl loopz2
	bcc skipz1
	lda cntr
	and #%00000011
	adc x
	sta colcrs
	lda cntr
	lsr
	lsr
	clc
	adc y
	sta rowcrs
	jsr osplot
skipz1	dec cntr
	bpl loopz1
	rts