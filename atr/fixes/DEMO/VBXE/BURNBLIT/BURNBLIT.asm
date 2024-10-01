
	icl "../../../asm/Fixes.asm"
	


	m_disable_basic
	
	zp = $ca	;Defined free ZP address space, 8 bytes required



	.macro m_load_high2 zp filename
	.echo "Fixed: Now runs from DOS 2.5"
;
;	>>> In-Memory XEX-Loader <<<
;
;	Loads complex XEX files to high address and then emulates DOS loading.
;	Use as follows in your own source file:
;
;	zp = $ca	;Defined free ZP address space, 8 bytes required
;	org $4000	
;	m_load_high zp 'FOXCHASE-Original.xex'


p1	= :zp
l1	= :zp+2
p2	= :zp+4
p3	= :zp+6

dosvec  = $0a
coldst  = $244
runad	= $2e0
iniad	= $2e2

	.proc main
	mva #1 coldst
	mwa #$e477 dosvec

;	mwa #$0100 p1
;	ldy #0
;	sty nmien
;	sty irqen
;	sei
;	mva #$fe portb
;
;fill1	lda random
;	sta (p1),y
;	iny
;	bne fill1
;	inc p1+1
;	lda p1+1
;	beq fill_done
;	cmp #$6b
;	sne
;	lda #$bc
;	cmp #$bc
;	sne
;	lda #$c0
;	cmp #$d0
;	sne
;	lda #$d8
;	sta p1+1
;	jmp fill1
;fill_done
;;	.byte 2
;
;;	mva #$ff portb

	mwa #file p1
	mwa #[.len file] l1
	mwa #$e477 runad
	mwa #$0000 iniad

segment_loop
	ldy #0
	lda (p1),y
	iny
	and (p1),y
	cmp #$ff
	bne no_header
	jsr get_byte
	jsr get_byte
	jmp segment_loop

no_header
	jsr get_byte
	sta p2
	jsr get_byte
	sta p2+1
	jsr get_byte
	sta p3
	jsr get_byte
	sta p3+1
byte_loop
	jsr get_byte
	sta (p2),y
	cpw p2 p3
	beq segment_end
	inw p2
	jmp byte_loop
segment_end
	lda iniad
	ora iniad+1
	beq no_iniad
	jsr jsr_iniad
	lda #0
	sta iniad
	sta iniad+1
no_iniad
	lda l1
	ora l1+1
	bne no_runad
	jsr jsr_runad
	jmp $e474
no_runad
	jmp segment_loop

jsr_iniad
	jmp (iniad)

jsr_runad
	jmp (runad)

	.proc get_byte
	dew l1	;Changes <A>

	ldy #0
	lda (p1),y
	inw p1
	rts
	.endp
	.endp

clear_start
	.local file			;Include actual XEX file in first segment
	ins :filename
	.endl

	run main

	.endm

	org $6bc0
	m_load_high2 zp 'burning_blitter/nordlicht2020_1.xex'