; "Village", Atari XL/XE Intro 256B
;
; code by tr1x / Agenda
; requirements: original Atari XL/XE 64KB PAL
; version: 1.0
; release date: 2022-12-10
;
; It uses the "chaos game" method.

cntr	equ $00

rowcrs	equ $0054
colcrs	equ $0055
color0	equ $02c4
color1	equ $02c5
color2	equ $02c6
color4	equ $02c8
atachr	equ $02fb
audc1	equ $d201
random	equ $d20a
osgraph	equ $ef9c
ospoint	equ $f1d8

	org $100-chaosdta.areas+start


start	lda #$0f
	jsr osgraph		; A=$01, X=$0f, Y=$01 after "jsr osgraph"

.if chaosdta.colnum <= 2
  .if .def chaosdt.color0 .and chaosdta.color0 != $00
	lda #chaosdta.color0
	sta color4
  .endif
  .if .def chaosdt.color1
	lda #chaosdta.color1
	sta color0
  .endif
  .if .def chaosdt.color2 
	lda #chaosdta.color2
	sta color1
  .endif
  .if .def chaosdt.color3
	lda #chaosdta.color3
	sta color2
  .endif
.else
	ldx #chaosdta.colnum-1
loopz0	lda chaosdta.colors,x
        sta color0,x
        dex
        bpl loopz0
.endif

loopx1
opcodx1	lda chaosdta.denscol
	sta atachr
loopx2	lda #chaosdta.densmul
	sta cntr
loopx3	lda random
opcodx2	cmp chaosdta.anchnum
	bcs loopx3
	asl
	tay
	ldx #$01
loopx4	*
opcodx3	lda chaosdta.areas,y
	adc rowcrs,x
	ror
	sta rowcrs,x
	iny
	dex
	bpl loopx4
	sta audc1
	jsr ospoint		; X=$02, Y=$00 after "jsr ospoint"
	dec cntr
	bne loopx3
opcodx4	dec chaosdta.denscol
	bne loopx2
opcodx5	lda chaosdta.anchnum
	asl
	clc
	adc opcodx3+1
	sta opcodx3+1
	inc opcodx1+1
	inc opcodx2+1
	inc opcodx4+1
	inc opcodx5+1

	bne loopx1

	icl 'VILLAGEDTA.ASM'
	
	run start
