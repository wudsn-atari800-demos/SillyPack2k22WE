; Original VU Player 1.0 Export
;
; 1900-1EFF
; 2000-2FFF
; 02E0-02E1
; 1F40-...
;
; Reexported VU Player 2.0 Expoert
; 0CEB-1F40
; 02E0-02E1
; 1F40-...

p1	= $80
p2	= $82
p3	= $84

	opt h-
	.byte $ff,$ff
	ins "RUN-Export-2.0.xex", +$1332
	
	opt h+
	org $8400
start	mva #$ff 580
	mwa #block_0c1b p1
	mwa #$c1b p2
	mwa #[.len block_0c1b] p3
	ldy #0
loop	mva (p1),y (p2),y
	inw p1
	inw p2
	sbw p3 #1
	lda p3
	ora p3+1
	bne loop
	mva #$4c $1d14
	mwa #$e477 $1d15
	jmp $1810

	org $8c1b
	.local block_0c1b
	ins "RUN-Export-2.0.xex", +$6, +$1320
	.endl
	
	run start


