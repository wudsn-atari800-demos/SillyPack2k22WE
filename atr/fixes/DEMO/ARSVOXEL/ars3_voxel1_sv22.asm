* Voxel Flight Vx
* Silly Venture 2022 WE Editon

relocate = 0
;	opt f+
;	opt h-
;bp -n main_loop "r @t2 @frame-@t0; r @t3 @clk-@t1; r @t0 @frame; r @t1 @clk"
;wx @t2
;wx @t3

;deflater depacker 0400-0619, buffer fc80, load 8800

; @com.wudsn.ide.lng.mainsourcefile=ARSVOXEL.asm

;flags
debug = 0
camyflag =1
stereomode=0
small_window=0 ;=1 renders only 48 scanlines
music=0
city_flag=0
shaking_flag=0
xres=40 ;in bytes
yres=80
maxy=100
maxray=180+38

megatexture equ $3400   ;$4000 so from $3400-74ff ;80FF with added 256 extension
;$3200+8100 = b200
offsettablo	equ $7500
offsettabhi	equ offsettablo+40*maxray+256 ;+$1800
persptab	equ $1000 ;+$1800 -->$c000
vram 		equ $e010
vram2		equ $e010+96*$28 ;ef00-
eor_filler1	equ $d800
eor_filler2	equ $0800
p0		equ $dc00
m0		equ p0-256 ;db00

;test music
;msx		equ $3200 ;-$3a00
;rmtplayer	equ $0c00 ;$3c00
;zp
rega		equ $00
regx		equ $01
regy		equ $02
miny		equ $03
si		equ $04 ;$05
di		equ $06 ;$07
camy		equ $08 ;$09
angle		equ $0a
nibble		equ $0b
eor_nibble	equ $0c
vram_offset	equ $0d
uvoffset	equ $10 ;$11
angle_counter	equ $12
inner_loop	equ $13
cloc		equ $14
fps		equ $15
COLBK           equ $D01A
WSYNC           equ $D40A
VCOUNT          equ $D40B
;colum buffer
collumbuffer	equ $16 ;128 bytes!

px		equ $98 ;$99
ex		equ $9a ;$9b
vex		equ $9c ;$9d
py		equ $9e ;$9f
ey		equ $a0 ;$a1
vey		equ $a2 ;$a3
kernel		equ $a4
demostate	equ $a5

;rmt player ZP $cc ff.!!!


;General extended RAM Mode
;Bit 5   Bit 4   Bit 3   Bit 2   CPU accesses:   ANTIC accesses:
;VBE     CPE     Bank selection
;0       0       0       0       E $0000-$3FFF   E $0000-$3FFF #0
;0       0       0       1       E $4000-$7FFF   E $4000-$7FFF #1
;0       0       1       0       E $8000-$BFFF   E $8000-$BFFF #2
;0       0       1       1       E $C000-$FFFF   E $C000-$FFFF #3
;1       1 = normal $fe base ram 
;$20 $10 = $30

;		org $2000
;loading_dump	
;		sei
;		lda #0
;		sta $d40e
;		ldy #$40
;copy_to_bank	
;		ldx #0
;@
;		lda #$fe
;		sta $d301		
;copy_to_bank0	lda dump,x
;		pha
;		lda #%11000010 ;basic off, rom off, bank #0
;		sta $d301
;		pla
;copy_to_bank1	sta dump,x
;		inx
;		bne @-
;		inc copy_to_bank0+2
;		inc copy_to_bank1+2
;		sty $bc40
;		dey
;		bne copy_to_bank
;		lda #$40
;		sta $d40e
;		lda #$ff
;		sta $d301
;		cli
;		rts
;		
;		org $4000
;		ins "pokey_dump.dat"
;		
;		ini loading_dump
		

persptab_load	= $4000
		org persptab_load	;@$1000, L=$1000
;		ins "persptab.bin"
;		ins "persptab128.bin"
;		ins "persptabx3.bin" ;****
;		ins "persptabx4.bin" ;****
		ins "persptabx40b.bin"	;$1000 bytes

		org $2000
move_persptab_to_c000
      		lda:cmp:req 20      ;wait 1 frame
		sei			;stop IRQ interrupts
		mva #$00 $d40e		;stop NMI interrupts
		sta $d400
		sta $22f
		mva #$fe $d301		;switch off ROM to get 16k more ram
.proc memclear
		ldx #0
		txa
mem		sta $d800,x
;		sta 0,x ;take care with RMT.
		inx
		bne mem
		inc mem+2
		lda mem+2
		cmp #$ff
		bne memclear
.endp




		ldy #$10
loop 		ldx #0
loop1		lda persptab_load,x
		sta $c000,x
		inx
		bne loop1
		inc loop1+2
		inc loop1+5
		dey
		bne loop	
;move pm_data
pm_init_panels
		ldx #0
		txa
@
		sta p0,x
		sta p0+256,x
		sta p0+512,x
		sta p0+768,x
		sta p0-256,x
		inx
		bne @-

;		ldy #31
		ldx #31
@
		lda left_panel_gfx,x
		sta p0+24,x
		lda left_panel_gfx+256,x
		sta p0+256+24,x
		lda left_panel_gfx+512,x
		sta p0+512+24,x
		lda left_panel_gfx+768,x
		sta p0+768+24,x
;		iny
		inx
		cpx #186
		bne @-

	lda #$ff ;rom/os on
	sta $d301

	mva #$40 $d40e		;switch on NMI+DLI again
	cli
	rts
	

	.align $100
left_panel_gfx	
; player 0
	dta $00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$C0,$E0,$FF,$FF,$7F,$3F
	dta $1F,$0F,$07,$03,$39,$C4,$20,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dta $00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$82
	dta $42,$22,$12,$0A,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	dta $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$C2,$3A,$02
	dta $2A,$2A,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	dta $02,$06,$06,$06,$06,$06,$06,$06,$04,$04,$04,$04,$04,$F4,$04,$04
	dta $04,$04,$04,$04,$06,$06,$06,$06,$0E,$06,$02,$02,$02,$02,$02,$02
	dta $02,$02,$02,$02,$02,$02,$2A,$2A,$02,$02,$02,$1A,$E2,$02,$02,$02
	dta $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	dta $02,$02,$02,$02,$02,$02,$0A,$12,$22,$42,$82,$02,$02,$02,$02,$02
	dta $02,$02,$02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dta $00,$00,$10,$62,$1C,$01,$03,$07,$0F,$1F,$3F,$7F,$FF,$F0,$E0,$C0
	dta $C0,$E0,$C0,$C0,$C0,$C0,$C0,$C0,$C1,$C3,$C2,$C2,$C2,$C2,$C2,$C2
	dta $C2,$C2,$C2,$C2,$C2,$42,$82,$82,$82,$BE,$BC,$80,$80,$80,$80,$00
	dta $2E,$80,$00,$40,$60,$7F,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00

; player 1
	dta $00,$00,$00,$00,$00,$00,$00,$00,$7F,$7F,$7F,$7F,$3F,$1F,$0F,$07
	dta $83,$C1,$E0,$F0,$FC,$F8,$C0,$00,$00,$00,$00,$00,$00,$00,$01,$03
	dta $06,$0E,$1E,$16,$06,$16,$16,$16,$16,$16,$16,$16,$06,$96,$C6,$E6
	dta $76,$3E,$1E,$0E,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
	dta $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$E6,$FE,$FE,$3E,$06
	dta $56,$D6,$86,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
	dta $06,$02,$02,$06,$0E,$0E,$0E,$0E,$0C,$0C,$0C,$0C,$FC,$FC,$0C,$0C
	dta $0C,$0C,$0C,$0C,$0E,$0E,$0E,$0E,$06,$02,$06,$06,$06,$06,$06,$06
	dta $06,$06,$06,$06,$06,$86,$D6,$56,$06,$1E,$FE,$FE,$E6,$06,$06,$06
	dta $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
	dta $06,$06,$06,$06,$0E,$1E,$3E,$76,$E6,$C6,$96,$06,$16,$16,$16,$16
	dta $16,$16,$16,$06,$16,$1E,$0E,$06,$03,$01,$00,$00,$00,$00,$00,$00
	dta $00,$00,$E0,$FC,$FE,$F8,$F0,$E0,$C1,$83,$07,$0F,$9F,$3F,$3F,$3F
	dta $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3E,$3D,$3D,$3D,$3D,$3D,$3D,$3D
	dta $3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$03,$3F,$3F,$3F,$3F,$3F,$3F
	dta $3F,$1F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


; player 0
	dta $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$03,$07,$FF,$FF,$FE,$FC
	dta $F8,$F0,$E0,$C0,$9C,$23,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dta $00,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$41,$42,$44
	dta $48,$50,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
	dta $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$47,$58,$40,$40,$40
	dta $54,$54,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$60,$70
	dta $60,$60,$60,$60,$20,$20,$20,$20,$20,$20,$2F,$20,$20,$20,$20,$20
	dta $60,$60,$60,$60,$60,$60,$60,$40,$40,$40,$40,$40,$40,$40,$40,$40
	dta $40,$40,$40,$40,$40,$40,$54,$54,$40,$5C,$43,$40,$40,$40,$40,$40
	dta $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
	dta $40,$40,$40,$40,$50,$48,$44,$42,$41,$40,$40,$40,$40,$40,$40,$40
	dta $40,$40,$40,$40,$40,$40,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dta $00,$00,$08,$46,$38,$80,$C0,$E0,$F0,$F8,$FC,$FE,$FF,$0F,$07,$03
	dta $03,$07,$03,$03,$03,$03,$03,$03,$83,$C3,$43,$43,$43,$43,$43,$43
	dta $43,$43,$43,$43,$43,$42,$41,$41,$41,$7D,$3D,$01,$01,$01,$01,$00
	dta $74,$01,$00,$02,$06,$FE,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00

; player 1
	dta $00,$00,$00,$00,$00,$00,$00,$00,$FE,$FE,$FE,$FE,$FC,$F8,$F0,$E0
	dta $C1,$83,$07,$0F,$3F,$1F,$03,$00,$00,$00,$00,$00,$00,$00,$80,$C0
	dta $60,$70,$78,$68,$60,$68,$68,$68,$68,$68,$68,$68,$60,$69,$63,$67
	dta $6E,$7C,$78,$70,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60
	dta $60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$67,$7F,$7F,$78,$60
	dta $6A,$6B,$61,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$40,$60
	dta $70,$70,$70,$70,$30,$30,$30,$30,$30,$30,$3F,$3F,$30,$30,$30,$30
	dta $70,$70,$70,$70,$60,$40,$40,$60,$60,$60,$60,$60,$60,$60,$60,$60
	dta $60,$60,$60,$60,$60,$61,$6B,$6A,$60,$7C,$7F,$7F,$67,$60,$60,$60
	dta $60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60,$60
	dta $60,$60,$60,$60,$70,$78,$7C,$6E,$67,$63,$69,$60,$68,$68,$68,$68
	dta $68,$68,$68,$60,$68,$78,$70,$60,$C0,$80,$00,$00,$00,$00,$00,$00
	dta $00,$00,$07,$3F,$7F,$1F,$0F,$07,$83,$C1,$E0,$F0,$F9,$FC,$FC,$FC
	dta $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$7C,$BC,$BC,$BC,$BC,$BC,$BC,$BC
	dta $BC,$BC,$BC,$BC,$BC,$BC,$BC,$BC,$BC,$C0,$FC,$FC,$FC,$FC,$FC,$FC
	dta $FC,$F8,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

	ini move_persptab_to_c000

main_length = $1500
main_load = $4c00
main_os   = $d800

	.if relocate=1
	org $c00,main_load
	.else
	org $c00
	.endif

		.proc main
start		jmp init

codestart
code3    eor collumbuffer+99-0
code4    sta vram+[99-0]*$28,y
code5    stx collumbuffer+99-0
code6	stx eor_nibble
codeend
	
codestart2
code33    eor collumbuffer+99-0
code44    sta vram2+[99-0]*$28,y
code55    stx collumbuffer+99-0
code66	stx eor_nibble
codeend2	

codegenerator_eorfiller
	ldy #yres
	lda #$a9
	jsr putcode
	lda #0
	jsr putcode
	lda #{tax}
	jsr putcode
loop111		
	ldx #0
@
	lda codestart,x
	jsr putcode
	inx
	cpx #(codeend-codestart)
	bne @-

	dec code3+1
	dec code5+1
	lda code4+1
	sec
	sbc #40
	sta code4+1
	bcs @+
	dec code4+2
@
	dey
	bne loop111
	lda #$60
putcode sta eor_filler1
	inc putcode+1
	bne @+
	inc putcode+2
@
	rts

	.proc codegenerator_eorfiller2
	ldy #yres
	lda #$a9
	jsr putcode2
	lda #0
	jsr putcode2
	lda #{tax}
	jsr putcode2
loop222		
	ldx #0
@
	lda codestart2,x
	jsr putcode2
	inx
	cpx #(codeend2-codestart2)
	bne @-

	dec code33+1
	dec code55+1
	lda code44+1
	sec
	sbc #40
	sta code44+1
	bcs @+
	dec code44+2
@
	dey
	bne loop222
	lda #$60
putcode2 sta eor_filler2
	inc putcode2+1
	bne @+
	inc putcode2+2
@
	rts
	.endp

;demo starts here
	.proc init
		lda $d40b
		bne init
		sta demostate
        lda #0
        sta 710
        sta 712
        sta $14
        lda #<dlist0
        sta 560
        lda #>dlist0
        sta 561
        lda #0
        sta 708
        lda #34
        sta 559
;fade in
        lda #0
        sta $14
@       lda $14
        sta 708
        cmp #15
        bne @-

@       lda $14
        cmp #200
        bcc @-

;fade out
        lda #0
        sta $14
@       lda $14
@       cmp $14
        beq @-
        dec 708
        bne @-1


		sei			;stop IRQ interrupts
		mva #$00 $d40e		;stop NMI interrupts	
		mva #$fe $d301		;switch off ROM to get 16k more ram
		jsr expand_data
.if music=1
		lda #0
		ldx #<msx
		ldy #>msx
		jsr rmtplayer
.endif
.if city_flag
		jsr prepare_citymap
.endif


		mwa #NMI $fffa		;new NMI handler
		lda #<dlist
		sta $d402
		lda #>dlist
		sta $d403
		mva #$c0 $d40e		;switch on NMI+DLI again
;		cli
;		lda #$50
;		sta $d01b
		lda #34
		sta $d400
		lda #$c0
		sta $d01a
		lda #$00 ;$0e
		sta $d404
		lda #62
		sta $d400

		jsr pm_init_surface		

    	jsr codegenerator_eorfiller
	    jsr codegenerator_eorfiller2
					
		lda #0
		sta uvoffset
		lda #$00 ;$b0-$c0 = scrolling left uvoffset+8, -8 =scrolling right
		sta angle_counter
		sta camy
		ldx angle_counter
        lda sintab,x
        sta main_loop+1
        lda #0
        sta cloc
        
;wait for start
@
	lda cloc
	tax
	lsr
	lsr
	lsr
	lsr
	eor #$0f
	ora #$f0
	sta $d01a	
	cpx #$ff
	bne @-
@
	lda cloc
	tax
	cpx #200
	bne @-
	.endp


main_loop		
		lda #0                  ;counter for angle is in main_loop+1
		sta angle

      	  lda:cmp:req 20      ;wait 1 frame
	      lda #<(vram2+16*$28)
	      sta vramad+1
	      lda #>(vram2+16*$28)
	      sta vramad+2
	      lda #0
	      sta vram_offset

			ldx #$40+$31
			lda 53770
			bpl @+
			ldx #$c0+$31
@
;			stx $d01b

	      ;ldy #39
	      	lda #0
	      	sta fps

		.local calc_voxel0
		lda #xres-1
		sta inner_loop
		
		lda uvoffset            ;set low byte of uvoffset in lax
        	sta di
		
.if camyflag=1
		lda camy
.rept 40,#
		sta camx0:1+1
		sta cam0x:1+1
.endr
.endif

xloop
		lda #maxy
		sta miny
		lda #0
		sta eor_nibble
		
;check 40x z for X0-nibble
		ldx angle
		txs
.rept 40,#
;		.local
		tsx
		lda offsettabhi+#*maxray,x   ; +2
        sta di+1                ;set hi byte of height
		                        ;low byte(di) is set to uvoffset_lo
		ldy offsettablo+#*maxray,x   ;y = offset lo from offsets table 
		lax (di),y                ;add offset lo and take height from heights
.if camyflag=1
camx0:1		adc #camy
		tax
.endif
		lda persptab+#*100+$b000,x    
		cmp miny                
		bcs @+                    
		sta miny                 
        	dey                     ;get texture byte 
		lax (di),y            ;add offset lo and take color from texture  
		
		ldy miny
		eor eor_nibble        
		sta collumbuffer,y    
		stx eor_nibble       
@
;		.endl                
.endr

pos01		ldy miny
		lda eor_nibble
		sta collumbuffer-1,y

		lda #maxy
		sta miny

		inc angle
		ldx angle
		txs
		lda #0
		sta eor_nibble
		
loop0x
.rept 40,#
;		.local
		tsx
	        lda offsettabhi+#*maxray,x   
	        sta di+1                ;set hi byte of height 
                                ;low byte(di) is set to uvoffset_lo
	        ldy offsettablo+#*maxray,x   ;y = offset lo from offsets table 
	        lax (di),y                ;add offset lo and take height from heights 
                                ;heights has to be expanded with copy of first 256 bytes
                                ;because +y_reg overlap in last line of texture
.if camyflag=1
cam0x:1	adc #camy
	tax
.endif
        lda persptab+#*100+$b000,x     
	cmp miny                  
	bcs @+                    
	sta miny                  
        iny                    
        lax (di),y            
        ldy miny              
        eor eor_nibble        
        ora collumbuffer,y    
        sta collumbuffer,y    
        stx eor_nibble       

@
;		.endl                
.endr
pos10		
	        ldy miny
		lda eor_nibble
		ora collumbuffer-1,y
		sta collumbuffer-1,y

		ldy inner_loop
;eor_filler1
;        lda #0
;        tax
;      .rept yres
;            eor collumbuffer+[99-#]
 ;           sta vram+[99-#]*$28,y
;            stx collumbuffer+[99-#]
;        .endr   
;		stx eor_nibble
		jsr eor_filler1
;		jmp *
		
		inc angle
		
		
		dec inner_loop
		bmi @+
		jmp xloop
@
		.endl
		
		m_info calc_voxel0
;		rts


                
		lda uvoffset
		clc
		adc #-8
		sta uvoffset

		inc angle_counter
		ldx angle_counter
;		lda sintabadd,x
;		asl
;		asl
;		clc
;		adc uvoffset
;		sta uvoffset

		lda sintab,x
		sta main_loop2+1


main_loop2
		lda #0                ;;counter for angle is in main_loop2+1
		sta angle
      		lda:cmp:req 20      ;wait 1 frame
	      lda #<(vram+16*$28)
	      sta vramad+1
	      lda #>(vram+16*$28)
	      sta vramad+2
	      lda #40
	      sta vram_offset

        lda #0
        sta fps

		.local calc_voxel1
		
		.proc block1
	
		lda #xres-1
		sta inner_loop
		
		lda uvoffset            ;set low byte of uvoffset in lax
        	sta di
		
.if camyflag=1
		lda camy
.rept 40,#
		sta block1.camx0:1+1
		sta block2.cam0x:1+1
.endr
.endif

xloop
		lda #maxy
		sta miny
		lda #0
		sta eor_nibble
		
;check 40x z for X0-nibble
		ldx angle
		txs
.rept 40,#
;		.local
		tsx
		lda offsettabhi+#*maxray,x   ; +2
        sta di+1                ;set hi byte of height                                  +8
		                        ;low byte(di) is set to uvoffset_lo
		ldy offsettablo+#*maxray,x   ;y = offset lo from offsets table                     +10
		lax (di),y                ;add offset lo and take height from heights             +13
.if camyflag=1
camx0:1		adc #camy
		tax
.endif
		lda persptab+#*100+$b000,x    ;                                                        +15
		cmp miny                  ;                                                       +18
		bcs @+                    ;                                                       +20
		sta miny                  ;                                                       +22
        dey                     ;get texture byte                                         +24
		lax (di),y            ;add offset lo and take color from texture                  +25
		ldy miny
		eor eor_nibble        ;                                                           +30
		sta collumbuffer,y    ;sta NNNN,y                                                 +32
		stx eor_nibble        ;                                                           +35
@
;		.endl                 ;                                                           +37
.endr

	.endp

	.if * <> $20bd
	.print "ERROR: Wrong PC."
	.endif

	m_info block1
	.endp 	; End of main
	m_info main

	.if relocate=1
	org $8000
	.proc main_to_os
	sei
	mva #0 nmien
	dec portb
	ldy #>main_length
	ldx #0
loop
src	lda main_load,x
trg	sta main_os,x
	inx
	bne loop
	inc src+2
	inc trg+2
	dey
	bne loop
	inc portb
	mva #$40 nmien
	cli
	rts
	.endp

	ini main_to_os
	.endif

	org $20bd
	.proc block2

pos01		ldy miny
		lda eor_nibble
		sta collumbuffer-1,y

		lda #maxy
		sta miny

		inc angle
		ldx angle
		txs
		lda #0
		sta eor_nibble
loop0x
.rept 40,#
;		.local
;		ldx angle                 ;+0
 		tsx
        lda offsettabhi+#*maxray,x   ; +2
        sta di+1                ;set hi byte of height                                  +8
                                ;low byte(di) is set to uvoffset_lo
        ldy offsettablo+#*maxray,x   ;y = offset lo from offsets table                     +10
        lax (di),y                ;add offset lo and take height from heights             +13
.if camyflag=1
;	clc
cam0x:1	adc #camy
	tax
.endif
        lda persptab+#*100+$b000,x      ;                                                   +15
	cmp miny                  ;                                                   +18
	bcs @+                    ;                                                   +20
	sta miny                  ;                                                   +22
        iny                     ;                                                     +24
        lax (di),y            ;add offset lo and take color from texture                  +25
        ldy miny              ;                                                           +27
        eor eor_nibble        ;                                                           +29
        ora collumbuffer,y    ;                                                           +32
        sta collumbuffer,y    ;sta NNNN,y                                                 +35
        stx eor_nibble        ;                                                           +37

@
;		.endl                 ;                                                           +39
.endr
pos10		;ldy #0
        ldy miny
;		dey
		lda eor_nibble
		ora collumbuffer-1,y
		sta collumbuffer-1,y

		ldy inner_loop
;		clc
;		adc vram_offset
;		tay
;eor_filler2
	jsr eor_filler2
;        lda #0
;        tax
;
;
;        .rept yres
;            eor collumbuffer+[99-#]
;            sta vram2+[99-#]*$28,y
;            stx collumbuffer+[99-#]
;        .endr   
;	stx eor_nibble

		inc angle

		dec inner_loop
		bmi @+
		jmp main.block1.xloop
@
;		rts
		.endl

		
		lda uvoffset
		clc
		adc #-8
		sta uvoffset
	
		inc angle_counter
		ldx angle_counter
;		lda sintabadd,x
;		asl
;		asl
;		clc
;		adc uvoffset
;		sta uvoffset
		
		lda sintab,x
		sta main.main_loop+1
.if camyflag=1
		lda sintab2,x
		sta camy
.endif
;		cpx #$b3
;		bcs @+
		jmp main.main_loop
;		jmp *
@

	.endp	; End of block2
	m_info block2


;	.align $100
        icl 'mpt_player.asm'

	.align $100 ;$player+$500
msx	;equ $4000

	mpt_relocator 'arsa2.mpt' , msx

	.align $100	
;	org msx
	.sav [6] ?length

	opt l-
	icl 'mpt_relocator.mac'



;backup is offsetablo4 and offsetabhi

		org megatexture
;		ins "megaheight64x64c.dat"

;		ins"megaheight64x64_noheight.dat"
;---
;		ins "megaheight64x64f.dat" ;artic **** 6
		ins "megaheight64x64i.dat" ;desert *** 7

				
;		ins "megaheight64x64xx.dat" ;test
;		ins "megaheight64x64yy.dat" ;test
		
;		ins "megaheight64x64g.dat" ;moon **** 6
;		ins "megaheight64x64v.dat" ;moon *** 6 more contrast
;		ins "megaheight64x64w.dat" ;artic *** 6 darker
		

;		ins "megaheight64x64t.dat" ;mountains *** 6
		
						
        	org offsettablo
;		ins "offsettablo4.bin"
;		ins "offsettablox3.bin" ; ****
;		ins "offsettablox4.bin"
;--
;		ins "offsettablox40.bin" ;*****
		ins "offsettablox40yy.bin" ;*****
						
		org offsettabhi
;		ins "offsettabhi4.bin"
;		ins "offsettabhix3.bin" ;****
;		ins "offsettabhix4.bin"

;		ins "offsettabhix40.bin" ;*****
		ins "offsettabhix40yy.bin" ;*****
		.ds $100
;		org topplanetgfx
;		ins "panel2v2.dat"
.if music=1		
		org rmtplayer
		icl "rmtplayr.asm"
;;		
		org msx
		ins "space_panda_disco.rmt",6
.endif
		
;		org $bc00
	.align $100
sintab dta sin(67,67,256)
;sintabadd 
;:2	.byte sin(1,4,128)
.if camyflag=1
sintab2 
:4 	dta sin(23,23,64)
.endif

.proc	NMI
	bit $d40f
	bpl VBL
	jmp dummy_dli
dliv	equ *-2

VBL
	sta regA
	stx regX
	sty regY
	sta $d40f		;reset NMI flag
	inc cloc
	inc fps
	lda #<top_dli
	sta dliv
	lda #>top_dli
	sta dliv+1
.if shaking_flag=1
	lda 53770
	and #1
	asl
	asl
	asl
	asl
	ora #$80
	sta dlist
.endif
;illuminate ship parts

;		lda #40
;		sec
;		sbc camy
;		lsr
;		lsr
;		and #$0f

		lda vram+36*40+2
		and #$0f		
		sta $d015
		lda vram+36*40+38
		and #$0f
		sta $d013


	lda #$10
	sta $d01a
	lda #$71
	sta $d01b	
;this area is for yours routines
.if music=1
	jsr rmtplayer+3
.endif
     jsr mpt_player.play


;sound
;sound1		lda #$fe ;$6a
;		sta $d200
;		lda #$a1
;		sta $d201
;		lda #$6a
;		sta $d202
;		lda #$84
;		sta $d203
;		lda #$66
;		sta $d204
;		lda #$a1
;		sta $d205
;		lda #$0e
;		sta $d206
;		lda #$81
;		sta $d207
;		lda #$60
;		sta $d208
;	lda #$fe
;	sta $d301

;    jsr set_levelmeter

	lda #0
	sta kernel
	sta $d011
quit
	lda regA
	ldx regX
	ldy regY
	rti
	

.proc top_dli
;DLI routine which is needed for the Mode9++ trick 
	sta top_dli0+1
	sta $d40a
	lda #14
	sta $d405
	lda #2
    	sta $d405
top_dli0 lda #0
    	rti
.endp

	lda #<bottom_dli
	sta dliv
	lda #>bottom_dli
	sta dliv+1
	pla
dummy_dli	rti
bottom_dli	
	pha
	lda #$00
	sta $d40a
	sta $d01b
	lda #$00
	sta $d01a
	sta $d011
;	sta $d404
;	lda #4
;	sta $d016
;	lda #8
;	sta $d017
;	lda #14
;	sta $d018
	pla
	rti
 .endp
	
	
.proc expand_data
        ldx #0
@       lda megatexture,x
        sta megatexture+$4000,x
        lda offsettablo,x
        sta offsettablo+40*maxray,x ; $1680,x
        lda offsettabhi,x
        sta offsettabhi+40*maxray,x ;
        dex
        bne @-
        rts
.endp

		
.proc pm_init_surface
;
		ldx #6
		ldy #0
@
		lda crosshair,x
		sta p0-256+128,y
		iny
		iny
		dex
		bpl @-

		lda #$71
		sta $d01b
		lda #3
		sta 53277
		lda #>(p0&$f800)
		sta 54279
		lda #62
		sta $d400
		lda #0
		sta 53256
		sta 53257
		sta 53258
		sta 53259
		lda #%01010101
		sta 53260
;sidepanels left/right
		lda #$30
		sta 53248
		sta 53249
		lda #$c8
		sta 53250
		sta 53251
		lda #4
		sta $d012
		sta $d014
		lda #8
		sta $d013
		sta $d015
		lda #$b0
		sta $d019
		lda #120-2
		sta 53255
		lda #124-2
		sta 53254
		lda #128
		sta 53253
		lda #132
		sta 53252
		rts
.endp
crosshair
	.byte %11111111
	.byte %10000001
	.byte %10000001
	.byte %11100111
	.byte %10000001
	.byte %10000001
	.byte %11111111


line0   dta d'SILLY VENTURE 2K22WE'
line1   dta d'ARSANTICA VOXEL 2016'
line2   dta d'CODE BY HEAVEN      '
line3   dta d'MUSIC BY MIKER      '
line4   dta d'GFX BY ALIEN        '
        dta d'EARLY PROTOTYPE 2016'

dlist
.if small_window=0
		.byte $80
vramad		.byte $6f,<(vram+16*$28),>(vram+16*$28)		
.rept  (yres/2-2) ;37
;		.byte $00
		.byte $8f
		.byte $2f
.endr		
		.byte $41
		.word dlist
.else
:8		.byte $70
		.byte $80
vramad		.byte $6f,<(vram+16*$28),>(vram+16*$28)		
.rept  (yres/4-2) ;37
;		.byte $00
		.byte $8f
		.byte $2f
.endr		
		.byte $41
		.word dlist
.endif	

dlist0
        .byte $70,$70,$70
        .byte $47
        .word line0
        .byte $70,$06,$70,$70,$70,$06,$70,$06,$70,$06,$70,$70,$06
        .byte $41
        .word dlist0


	.if relocate=1
	org $9710
	.proc main_to_ram
	sei
	mva #0 nmien
	dec portb
	ldy #>main_length
	ldx #0
loop
src	lda main_os,x
trg	sta main,x
	inx
	bne loop
	inc src+2
	inc trg+2
	dey
	bne loop
	inc portb
	mva #$40 nmien
	cli
	jmp main.start
	.endp
	
	run main_to_ram
	
	.else
	
	run main.start

	.endif