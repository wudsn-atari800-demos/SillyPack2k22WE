;Arsantica 1 Prototype
;for Silly Venture 2022 Winter Edition

;Revision Party Entry 2014
;Code by heaven

;panel = 77*40 = 3080

;---------------------------------------
; 1bir - 1 block interactive raycaster
;
; coded by huseyin kilic (wisdom)
; Atari conversion by Heaven
; copyright (c) 2009-2013 crescent
;---------------------------------------
; mandatory locations due to basic/kernal
; these locations have been selected to make use of
; some basic/kernal routines to help with decreasing the size of the program

;3d tunnel
;texture_lo = $1000 = 64x64 texture
;code = $0000 - $35ff 

;unlimited 

load_2000 = $2900
; @com.wudsn.ide.lng.mainsourcefile=ARSANTIC.asm

;	icl "DSR-LOGO-2-by-akira.h"

xboot_load_file equ $5f1


rega	equ $00
regx	equ $01
regy	equ $02
counter equ $03 ;$04
;stick0	equ $05
linecounter equ $06
cloc	equ $07
si equ $08
di equ $0a
si2 equ $0c
di2 equ $0e

rayposx     = $61
rayposxh    = $62
rayposy     = $63
rayposyh    = $64

stepx       = $66 ; leave 1 byte between x and y
stepy       = $68

playerx     = $69
playerxh    = $6a
playery     = $6b
playeryh    = $6c
distance    = $6d ; reset in $bc00 and $bc0f calls

rowptr      = $6e
rowptrh     = $6f
heights     = $70 ; 3 bytes consecutively
; temp
currentrow  = $73

map_t       = map8x8 ;$cc00 ; this area has been choosen because of lots of zeroes

; external value dependencies
heading     = $74 ; default = $20
sinadd      = $75 ; default = $03
colors      = $76 ; 3 bytes consecutively

temp equ $79
scrollflag equ $7a
ypos equ $7b
charpos equ $7c
softscroll equ $7d
pmscroll_counter equ $82
pmscroll_direction equ $83
v0 equ $84


; constants
sin_t       = $c800
blocksize   = $29 ;$29
vram equ $2000 ;$b000 $b400;$b7ff
vramoffset equ $84
scr		equ $b800 ;akira logo screen data
font	equ $3000
pmfont equ $3400
pmsintab equ $6800	; $100 bytes
pmcolors equ $6c00

chartabl equ $c000
chartabh equ $c080
charbuffer equ $c200 
;sintab	equ $a180
p0 equ $c400

;akira logo
font_part2 equ $7000

;desire mode10 logo
title_screen equ $8010	; $1e00 bytes
coltab equ $800	;$8000, $200 bytes

rmt_player equ $7000 ;xe-bank #0, xl $8400
;module equ $4000
module	equ $4000 ;xe-bank #0

;unlimited bob part
screen1 equ $4010	; $b40 bytes
screen_tunnel equ $3010

;draw_code equ $6800 ;$9e02 ;-$7602
;tunneltexturex0 equ $d800 ;$-b040
;tunneltexture0x equ $e900 ;$e900 ;-$9040
;draw_code_ad equ $8000;-$1800



;part 2
WIDTH	= 40
HEIGHT	= 30

p1	= $80
x1	= $e0

;rmt-player needs -$400 prior replay code


;General extended RAM Mode
;Bit 5   Bit 4   Bit 3   Bit 2   CPU accesses:   ANTIC accesses:
;VBE     CPE     Bank selection
;0       0       0       0       E $0000-$3FFF   E $0000-$3FFF #0
;0       0       0       1       E $4000-$7FFF   E $4000-$7FFF #1
;0       0       1       0       E $8000-$BFFF   E $8000-$BFFF #2
;0       0       1       1       E $C000-$FFFF   E $C000-$FFFF #3
;1       1 = normal $fe base ram 
;$20 $10 = $30

;bank #0 = rmt player & music
;bank #1 = texture_x0
;bank #2 = draw_code texture tunnel $4000-$7602
;bank #3 = 

draw_code equ $4000 ;-$7602 --> bank #2
tunneltexturex0 equ $a000 ;$-b040 --> bank #1 aber Konflikt mit main code $a000 im XL RAM
tunneltexture0x equ $8000 ;-$9040 --> bank #1 will be moved into main ram at runtime
;draw_code_ad equ $1100

	org $2000

	.proc loader1
	m_disable_basic

	org $12
	.byte $00,$00,$00
	
	org 580
	.byte 1

		org $bc40
		.byte d'                                        '
		.byte d'                                        '
		.byte d'                                        '
		.byte d'                                        '
		.byte d'     ARSANTICA 0 - THE ROUGH CUT        '
		.byte d'                                        '
		.byte d'   SILLY VENTURE 2022  WINTER EDITION   '
		.byte d'  MACHINE: ATARI XL/XE WITH MIN 128KB   '
		.byte d'                                        '
		.byte d'              CREDITS:                  '
		.byte d'                                        '
		.byte d'            CODE: HEAVEN                '
		.byte d'     GFX: ALIEN,BOKANOID,LOWLIFE        '
		.byte d'                                        '
		.byte d'              MSX: BEWU                 '
		.byte d'                                        '
		.byte d'        PRESENTED IN STEREO!            '
		.byte d'                                        '
		.byte d'      (C) 2014-2022 BY DESIRE           '
		.byte d'                                        '
		.byte d'  THIS VERSION HAS ALTERNATIVE MUSIC    '
		.byte d' WHICH BEWU REWORKED LATER IN THE FINAL '
		.byte d'                                        '

	.endp

;	org $0c00
;	
;clearclear	
;
;	lda #0
;	sta 65
;
;	.proc test
;	jsr fill_memory
;;	rts
;
;	.proc fill_memory
;	.macro m_fill
;	lda #:1
;	ldx #:2
;	ldy #[:3+1-:2]
;	jsr fill
;	.endm
;	
;	sei
;	mva #0 $d40e
;	m_fill $ff $0d $bb
;	m_fill $fe $c0 $cf
;	m_fill $fe $d8 $ff
;;	m_fill $23 $40 $7f
;;	m_fill $27 $40 $7f
;;	m_fill $2b $40 $7f
;;	m_fill $2f $40 $7f
;;	m_fill $63 $40 $7f
;;	m_fill $67 $40 $7f
;;	m_fill $6b $40 $7f
;;	m_fill $6f $40 $7f
;;	m_fill $a3 $40 $7f
;;	m_fill $a7 $40 $7f
;;	m_fill $ab $40 $7f
;;	m_fill $af $40 $7f
;;	m_fill $e3 $40 $7f
;;	m_fill $e7 $40 $7f
;;	m_fill $eb $40 $7f
;;	m_fill $ef $40 $7f
;	mva #$ff $d301
;	mva #$40 $d40e
;	cli
;	rts
;	
;	.proc fill
;	sta $d301
;	stx p1+1
;	sty x1
;	ldy #0
;	sty p1
;mloop	lda #0
;	sta (p1),y
;	iny
;	bne mloop
;	inc p1+1
;	dec x1
;	bne mloop
;	rts
;	.endp
;
;	.endp
;	
;	.endp
;
;	ini clearclear

;move rmt player and mod file into bank #0
		org load_2000 ;now switch to XE-bank #0
		.proc activate_bank0
		lda #%11000011 ;basic off, rom on, bank #0
		sta $d301
		ldy #15
@		lda loadtext0,y
		sta (88),y
		dey
		bpl @-		
		rts
loadtext0	dta d'LOADING MUSIC.#0'
		.endp
	
		ini activate_bank0

		org rmt_player
		icl "rmtplayr_remix.asm"

		org module
;		ins "desire_5.rmt",6
		;ins "desire_5_0800.rmt",6
		ins "desire_5_4000_for_xe.rmt",6
	
		org load_2000 ;now switch back
		.proc activate_base_ram
		lda #%01111111 ;basic off, rom on, XL-RAM
		sta $d301
		;jmp *
		rts
		.endp
		ini activate_base_ram

;texture tunnel bank #1+bank #2

		org load_2000
		.proc activate_bank1
		
		lda #%11000111 ;basic off, rom on, bank #1
		sta $d301
		ldy #15
@		lda loadtext1,y
		sta (88),y
		dey
		bpl @-		
		rts
loadtext1	dta d'LOADING DATA..#2'
		.endp
		ini activate_bank1

		org $4000 ;tunneltexturex0
;		ins "tunneltexturex0.dat" ;original
		;ins "tunneltex_x0.dat" ;robin
;		ins "tunnel8c_x0.dat" ;8col
		ins "tunneltex2_x0.dat" ;robin $100 bytes

		org load_2000
* convert x0 texture into 0x 
		.proc convert_texture
		ldy #$10
@		ldx #0
ttinit0		lda $4000,x ;tunneltexturex0,x
		lsr
		lsr
		lsr
		lsr
ttinit1		sta $6000,x ;tunneltexture0x,x
		inx
		bne ttinit0
		inc ttinit0+2
		inc ttinit1+2
		dey
		bne @-
* convert first texture line to avoid overflow check
		ldx #63
@		lda $4000,x ;tunneltexturex0,x
		sta $5000,x ;tunneltexturex0+$1000,x
		lda $6000,x ;tunneltexture0x,x
		sta $7000,x ;tunneltexture0x+$1000,x
		dex
		bpl @-
		rts
		.endp
		ini convert_texture

		org load_2000
		.proc activate_bank2
		lda #%11001011 ;basic off, rom on, bank #2
		sta $d301	
		ldy #15
@		lda loadtext3,y
		sta (88),y
		dey
		bpl @-		
		rts
loadtext3	dta d'LOADING DATA..#1'
		.endp
		ini activate_bank2

;		org draw_code_ad ;$1100 => $8000??

		org $2000
;code generator
	.proc code_generator
		lda #<draw_code_ad
		sta si
		lda #>draw_code_ad
		sta si+1
		lda #<draw_code_ad
		sta di
		lda #>(draw_code_ad+$0c00)
		sta di+1
		lda #<screen_tunnel ;vram
		sta v0
		lda #>screen_tunnel ;vram
		sta v0+1
		lda #{ldx #}
		jsr put_code
		lda #0
		jsr put_code
code_gen_loop
		lda #$bd ;{lda $ffff,x}
		jsr put_code
		ldy #0
		lda (si),y
		jsr put_code
		iny
		lda (si),y ;hibyte
		clc
		adc #$38 ;--> $a000-->$d800
		jsr put_code
		lda #$1d ;ora $ffff,x
		jsr put_code
		ldy #0
		lda (di),y
		jsr put_code
		iny
		lda (di),y
		clc
		adc #$69 ;$8000-$e900
		jsr put_code
		lda #{sta}
		jsr put_code
		lda v0
		jsr put_code
		lda v0+1
		jsr put_code
		inc v0
		bne @+
		inc v0+1
@		lda si
		clc
		adc #2
		sta si
		bcc @+
		inc si+1
@		lda di
		clc
		adc #2
		sta di
		bcc @+
		inc di+1
@		lda v0+1
		cmp #>(screen_tunnel+48*32)
		bcs @+
		jmp code_gen_loop
@
		lda v0
		cmp #<(screen_tunnel+48*32)
		bcs @+
		jmp code_gen_loop
		
@		lda #{rts}
		jsr put_code
		;brk				
		rts		
put_code	sta draw_code
		inc put_code+1
		bne putcode0
		inc put_code+2
putcode0 	rts

	.local draw_code_ad
	ins "tunnelcodead.dat"	;$1800 bytes
	.endl

	.endp
	m_info code_generator
	ini code_generator

		org load_2000
		.proc activate_bank3
		lda #%11001111 ;basic off, rom on, bank #3
		sta $d301	
		ldy #15
@		lda loadtext5,y
		sta (88),y
		dey
		bpl @-		
		rts
loadtext5	dta d'LOADING DATA..#3'
		.endp
		ini activate_bank3
;panda pic
		org $4010
panda_pic	ins"loadscreen0.mic" ;9600 = $2580
		.align $100
dlist_panda	.byte $70
		.byte $4e,<panda_pic,>panda_pic
:101	.byte $0e
		.byte $4e
		.word panda_pic+102*40
:91		.byte $0e
		.byte $41
		.word dlist_panda

		.proc show_panda
wait5secs	lda $12			;Text screen shall be visible for 5s
		ora $13
		beq wait5secs

		lda:cmp:req $14		;wait 1 frame
		lda #<dlist_panda
		sta 560
		lda #>dlist_panda
		sta 561

@
		lda:cmp:req $14		;wait 1 frame

		lda #0
		sta 712
		lda #15
		sta 708
		lda #10
		sta 709
		lda #4
		sta 710
		lda #0
		sta $14

@		lda $14
		cmp #240
		bne @-

		rts
		.endp
	
		ini show_panda
		

		
		org load_2000 ;now switch back
		.proc activate_base_ram2
		lda #%01111111 ;basic off, rom on, XL-RAM
		sta $d301
		rts
		.endp
		ini activate_base_ram2

		org load_2000
		.proc loading_demo
		ldy #15
@		lda loadtext4,y
		sta (88),y
		dey
		bpl @-
		rts
loadtext4	dta d'LOADING DEMO... '
		.endp
		ini loading_demo

		org title_screen	;$1010-2e0f
		ins "dsr_blue.mic"


;		org $0800
;fast Mode9++ display list code
;this method works just in combination with the DLI!!!
;it uses the internal antic line ram to speed up the mode9 generation
; 2 blank lines, 1 line of mode
		

	org $600
	.local dlist_part3
 	dta $90,$6f
ad    	dta a(screen1)

; 29 times $8f,$2f => 58 lines

    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f,$8f,$2f,$8f,$2f,$8f,$2f
    dta $8f,$2f
    dta $41
    dta a(dlist_part3)
    .endl
    m_info dlist_part3

         org font		; $3000-$3087
;		ins "dither.fnt"
		.he 00 00 00 00 00 00 00 00
:16  	.byte #*16+#,#*16+#,#*16+#,#*16+#,#*16+#,#*16+#,#*16+#,#*16+#            
		.he 00 00 00 00 00 00 00 00

;	org $2d00
	
scrtabl dta 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
scrtabh dta 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
linetabl dta 0,0,0,0,0,0,0,0,0,0
:5 dta 0,0,0,0,0,0,0,0,0,0					;xasm specific duplicate line 5 times
linetabh dta 0,0,0,0,0,0,0,0,0,0
:5 dta 0,0,0,0,0,0,0,0,0,0					;s.a.


;the sprite data
;shape_l = non shifted
	.align $100
shape_l .byte $00,$33,$33,$40
	.byte $05,$88,$88,$35
	.byte $64,$78,$87,$74
	.byte $64,$77,$77,$44
	.byte $66,$66,$66,$66
	.byte $66,$77,$77,$76
	.byte $76,$37,$77,$66
	.byte $06,$66,$66,$60

shape_l_mask
	.byte $ff,$00,$00,$0f
	.byte $f0,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $f0,$00,$00,$0f

shape_h	.byte $00,$03,$33,$34,$00
	.byte $00,$58,$88,$83,$50
	.byte $06,$47,$88,$77,$40
	.byte $06,$47,$77,$74,$40
	.byte $06,$66,$66,$66,$60
	.byte $06,$67,$77,$77,$60
	.byte $07,$63,$77,$76,$60
	.byte $00,$66,$66,$66,$00

shape_h_mask
	.byte $ff,$f0,$00,$00,$ff
	.byte $ff,$00,$00,$00,$0f
	.byte $f0,$00,$00,$00,$0f
	.byte $f0,$00,$00,$00,$0f
	.byte $f0,$00,$00,$00,$0f
	.byte $f0,$00,$00,$00,$0f
	.byte $f0,$00,$00,$00,$0f
	.byte $ff,$00,$00,$00,$ff
	
;shape_l dta $14,$41
;        dta $48,$84
;       dta $14,$41
       

       
;shape_h = shifted
;shape_h dta $01,$44,$10
;	dta $04,$88,$40
;	dta $01,$44,$10

		
;vars for the sprite movement
addx	dta 0,0
addy 	dta 0,0
posx	dta 0,0
posy	dta 0,0
next	dta 255,1
count	dta 0
count2	dta 0
count3 	dta 64 ;amount of bobs per fx
count4  dta 7 ;how many run throughs
addxtabl .byte 128,64,32,96,48,128,0,0
addxtabh .byte 2,-1,2,1,1,1,-2,1
addytabl .byte 200,64,32,96,48,16,0,0
addytabh .byte 2,-2,1,-1,1,2,-1,2
	m_align_page
:256	.byte $00

		org pmfont		 ;$3400-$37ff
;		ins "agasoft.fnt"
;		ins "chset.fnt"
;		ins "hugo.fnt"
;		ins "polfont.fnt"
;		ins "hohl.fnt"
		;ins "robbo1.fnt"
		ins "erwin_8x8font3.fnt"

            org $3800		; $3800-$3906

dlist	.byte $70,$70,$60,$80,$42
dlvramad .word vram
:21		.byte $02
		.byte $41
		.word dlist

;DESIRE logo gr.0
dlist_part2	dta $70,$70,$70,$42,a(vram+40)
	dta $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	dta $02,$02,$02,$02,$02;$02;,$02;,$02,$02,$02,$02,$02;,$22
	dta $41,a(dlist_part2)
;	.align $0100

;title screen
dlist_mode10
	.byte $70,$70,$70,$4f
	.word title_screen
:101	.byte $0f
	.byte $4f
	.word title_screen+102*40
:89	.byte $0f
	.byte $41
	.word dlist_mode10

;xasm specific
;generates the 2 sinus lookup tables
        .align $100
sintabx  dta sin(0,40,256)
sintaby  dta sin(0,24,256)

		org screen1		;$4010-4b4f
		ins "m10_back.raw"


		org pmsintab		;$6800-$68ff
		ins "pmsintab.dat"

		org pmcolors		;$6c00-$6cff
:16		.byte #+$60
:16		.byte (15-#+$60)
:16		.byte #+$70
:16		.byte (15-#+$70)
:16		.byte #+$70
:16		.byte (15-#+$70)
:16		.byte #+$60
:16		.byte (15-#+$60)
:16		.byte #+$80
:16		.byte (15-#+$80)
:16		.byte #+$60
:16		.byte (15-#+$60)
:16		.byte #+$70
:16		.byte (15-#+$70)
:16		.byte #+$60
:16		.byte (15-#+$60)

		org font_part2
		ins "DSR-LOGO-2-by-akira.fnt"	;$7000-73ff


;	.ALIGN $0400

     		org $a000 
main
	;	lda #%01110011 ;basic off, rom on, bank #0
	;	lda #%11000111 ;basic off, rom on, bank #1
	;	sta $d301	
	;	brk
	
		lda #$90	
		sta $d01a
		
		sei			;stop IRQ interrupts
		mva #$00 nmien		;stop NMI interrupts
		sta dmactl

;texture tunnel preparations
	lda #%11000110 ;basic off, rom off, bank #1
	sta $d301	
;move texture_x0 from $4000 to $d800
part4_tunnel
	lda #0
	sta si
	sta di
	sta si2
	sta di2
	lda #$40
	sta si+1
	lda #$60
	sta si2+1
	lda #$d8 ;$d800-$e8ff
	sta di+1
	lda #$e9 ;$e900-$fa00
	sta di2+1
	ldx #$10
part4_tunnel0 
	ldy #0
@	
	lda (si),y
	sta (di),y
	lda (si2),y
	sta (di2),y
	iny
	bne @-
	inc si+1
	inc di+1
	inc si2+1
	inc di2+1
	dex
	bpl part4_tunnel0
;	brk

;---------


		lda #%11000010 ;basic off, rom off, bank #0
		sta portb
		ldx #<module					;low byte of RMT module to X reg
		ldy #>module					;hi byte of RMT module to Y reg
		jsr RASTERMUSICTRACKER		;Init
;		jmp *
		mva #$fe portb		;switch off ROM to get 16k more ram
		mwa #NMI $fffa		;new NMI handler
		mva #$c0 nmien		;switch on NMI+DLI again

;show DESIRE logo
		lda:cmp:req cloc		;wait 1 frame
		jsr show_desire_logo
        lda #0
        sta $d400
;debug
;		jmp part4
				
		mwa #dlist dlptr		;ANTIC address program
			
;big PM Scroller

			lda #$ff
			sta $d00d

			jsr pm_scroll_init

			lda #62
			sta $d400
		
			lda #0
			sta vramoffset ;for double buffering

            ;sei
			lda #>font
			sta $d409
			lda #$20
			sta heading
			lda #7
			sta colors
			lda #127
			sta colors+2
;init ZP vars of raycaster $61-$6d
			ldx #$0e
@			lda raycast_vartab_init,x
			sta rayposx,x
			dex
			bpl @-


 			jsr autopilot_init
;---------------------------------------
; sin/cos table generator
;---------------------------------------
            lda #3
            sta sinadd
            
            lda #$00
			sta distance
            ;anc #$00 ; clears carry for sinadd below
            tay
gensin_loop
            sta sin_t,y
            iny
            clc
            adc sinadd
            ldx sinadd
            dec sincount_t,x
            bne gensin_loop
            dec sinadd
            bpl gensin_loop

            ; x = $00
            ; y = $40
gensin_loop2
            lda sin_t,x
            sta sin_t+$0100,x ; needed for cos extension
            sta sin_t-1+$40,y
            eor #$ff
            sta sin_t+$80,x
            sta sin_t-1+$c0,y
            ;lda #$ff
            ;sta $2900,y
            inx
            dey
            bpl gensin_loop2

;---------------------------------------
; raycaster            
;---------------------------------------
raycaster_loop_main
;			lda:cmp:req $14		;wait 1 frame

			lda #>vram
			eor vramoffset
			sta dlvramad+1
			
			lda vramoffset
			eor #4
			sta vramoffset



;			lda cloc
;			sta $d01a

            ; cast 40 rays for each screen column
            ; starting with rightmost one
            ; yr is used as global column index
            ; throughout the rest of the program
            ldy #39
loop_ray
            ;sty 712
            ; determine current ray's direction
            ; by taking player's current direction
            ; and fov into account
            ; fov is 40 brads out of 256 brads
            tya
            clc
            adc heading
            ;sec
            sbc #19+1 ; half of the fov (+1 because of sec)
            tax
            ; get sin/cos values accordingly
            ; and copy player position to current ray position
            ; distance is reset on return from this call
            jsr getsincos_copyplr2ray
            
            ; reset line row before each column gets drawn
            ; (needed in vertical line section)
            stx currentrow

loop_dist
            ; step along current ray's path and find distance
            inc distance
            
            ; limit distance when it is needed in larger maps
            ; or open (wrapped) maps
            
            ; max distance = $29
            ;lda distance
            ;cmp #$29
            ;bcs skip_dist
            
            ; max distance = $40 (make sure ar is always 0 here)
            bit distance
            bvs skip_dist

            ;max distance = $80
            ;lda distance
            ;bmi skip_dist
            
            jsr addsteptopos
            ; on return from last call, ar is cell (block) value
            ; ar = 0 means empty cell, so continue tracing
            beq loop_dist
skip_dist
            ; now ar contains the value in reached map position
            ; (or last cell value fetched if max distance is reached)
            
            ; use ar or xr to colorize the block
            ;and #$7f
            ;ora #$03
            ;sta colors+1
            lda distance
 			
 			lsr
;			lsr
;            asl
            eor #15
            sta colors+1

            ; find out visible block height
            ; according to distance
            ;ldx #$ff
            ;txa
            ; as lax #imm is unstable, the following is needed to
            ; load ar and xr both with $ff value from kernal stkey var.
            ;lax $91
            ldx #$ff
    
            ; fill the single char that appears on screen
            ; (as in char $a0 in default charset)
            ; dirty but needed because of size restriction
            ;sta $2900,y 
            
            ; calculate visible block height through simple division
            lda #<blocksize
loop_div
            inx
            ;sec
            sbc distance
            bcs loop_div

            ; xr = half of visible block height
            txa
;---------------------------------------
; vertical line
;---------------------------------------
            ; yr = x position (screen column)
            ; ar = half height (zero height is ok)
            cmp #13          ; height > 12?
            bcc vline_validheight
            lda #12          ; make sure max height = 12
vline_validheight
            asl              ; calculate full height
            sta heights+1    ; store for looping below
            eor #$ff         ; subtract full height from screen height
            ;sec             ; (24 rows)
            adc #24+1        ; +1 because of sec
            lsr              ; sky/floor heights are equal to each other
            sta heights
            sta heights+2
            ;jsr setheights   ; dirty again, but works
            
            ; loop through 3 sections of one screen column
            ; i.e. sky - wall - floor
			;jsr draw_sky_floor
            
            ldx #$02 ;$02
vline_loop
            dec heights,x
            bmi vline_sectioncomplete
            stx xxreg+1
            ;txs              ; dirty way of saving xr temporarily
            ldx currentrow   ; this was reset before the distance loop
            ; there are two ways used in this program to set up
            ; current row address, either through kernal call ($e549)
            ; or by directly modifiying zp pointer
            ;jsr setrowptr ; call $e549 in main if you comment out this line
            lda linel_t,x
            sta rowptr
            lda lineh_t,x
;double buffering
			ora vramoffset
            sta rowptrh
xxreg	    ldx #0
            ;tsx
            lda colors,x ; each section can be assigned to a different color
;			lda #1
            sta (rowptr),y
            ; advance to next screen row
            inc currentrow
            bne vline_loop ; absolute jump, as currentrow never reaches zero
vline_sectioncomplete
            ; advance to next column section
           dex
            bpl vline_loop
;---------------------------------------
            ; advance to next ray/column
            dey
            bpl loop_ray
;---------------------------------------
; user input
;---------------------------------------
            ; common preparation code to set up sin/cos and
            ; to copy player position to ray position to trace movement
            ; direction to determine if player hits a block
            ; in case player actually tries to move forward or backwards
            ldx heading
            jsr getsincos_copyplr2ray
autopilot_loop:	
		dec steps
		bpl automove

;get new command		
		ldx movescript_counter
		lda movescript+1,x
		sta steps
		lda movescript,x
		sta direction
		bmi part2
		inc movescript_counter
		inc movescript_counter
automove
		jsr autopilot
		jmp raycaster_loop_main

raycast_vartab_init: 
		.he 28 C1 42 00 02 63 00 4D 62 C0 A8 FF 02 80 22 00


;show DESIRE GR.0
part2:	
		lda:cmp:req cloc		;wait 1 frame
		lda #<nmi.vbl_fontcopy
		ldx #>nmi.vbl_fontcopy
		sta nmi.fx_subroutines+1
		stx nmi.fx_subroutines+2
	
		lda #0
		sta 53248
		sta 53249
		sta 53250
		sta 53251
		sta 53252
		sta 53253
		sta 53254
		sta 53255
		sta 53277
		lda #34
		sta dmactl		

;		ldx #0
;		txa
;@		sta vram,x
;		sta vram+256,x
;		sta vram+512,x
;		sta vram+768,x
;		inx
;		bne @-
;set starting points
		lda #$f8
;		sta vram+23*40
;		sta vram+23*40+1
;		sta vram+22*40
;		sta vram+1
;		sta vram+2
;		sta vram+40
		sta vram+39+16*40
		sta vram+38+16*40				

;save char #16 and set $ff
		ldx #7
@		lda font_part2+15*8,x
		sta backup_char16,x
		lda #255
		sta font_part2+15*8,x
		dex
		bpl @-				
		
		lda #>font_part2
		sta $d409
		lda #1
		sta $d01b
		lda #$90
		sta $d018
		lda #$9f
		sta $d017
		mwa #dlist_part2 dlptr		;ANTIC address program
			
		jsr set_chars

;now erase all $0f remaining on screen	
		ldy #4 ;4 pages
		ldx #0
erase_chars_0f
erase_chars_0f1	lda vram+40,x
		cmp #15
		bne @+
		lda #0
erase_chars_0f2
		sta vram+40,x
erase_chars_0f0
		sta vram+40,x
@		inx
		bne erase_chars_0f
		inc erase_chars_0f0+2
		inc erase_chars_0f1+2
		inc erase_chars_0f2+2
		dey
		bne erase_chars_0f
	
;copy back char #16
		ldx #7
@		lda backup_char16,x
		sta font_part2+15*8,x
		dex
		bpl @-				

		jsr set_text
;now erase all $f8 remaining on screen
	
		ldy #4 ;4 pages
		ldx #0
erase_chars
erase_chars1	lda scr+40,x
erase_chars0
		sta vram+40,x
		inx
		bne erase_chars
		inc erase_chars0+2
		inc erase_chars1+2
		dey
		bne erase_chars

;wait
		lda #255
		sta cloc
part2_exit
		lda cloc
		bne part2_exit
		inc cloc

@		lda cloc
		bne @-

		inc cloc

@		lda cloc
		bne @-

;fade out
@		lda:cmp:req cloc		;wait 1 frame
		eor #15
		sta $d017
		bne @-

		inc cloc

@		lda cloc
		bne @-

;debug
;		jmp part4



;here screen is black		
part3
;unlimited bobs part	
		lda:cmp:req cloc		;wait 1 frame
		lda #{nop}
		sta nmi.fx_subroutines
		sta nmi.fx_subroutines+1
		sta nmi.fx_subroutines+2
		lda #<bob_dli
		sta nmi.dliv
		lda #>bob_dli
		sta nmi.dliv+1

		lda:cmp:req cloc		;wait 1 frame
		lda #<dlist_part3
		sta dlptr
		lda #>dlist_part3
		sta dlptr+1
		lda #35
		sta dmactl
		lda #$80
		sta $d01b
		lda #$8e
		sta $d012
		lda #$0e
		sta $d013
		lda #$8a
		sta $d014
		lda #$38 ;88
		sta $d015
		lda #$36 ;86
		sta $d016
		lda #$82
		sta $d017
		lda #$96
		sta $d018
		lda #$3c ;8c
		sta $d019
		lda #$9f
		sta $d01a
		;jmp *
;TODO		
;screen rams setzen 48*60=2880 $b40
bob_clr_vram mwa #screen1 si					;clears all 5 vrams 
	mwa #screen1+$1000 di
	ldx #5
bobclr0	ldy #0
bobclr1	lda (si),y
	sta (di),y+
	bne bobclr1
	inc si+1
	inc di+1
	lda si+1
	cmp #>(screen1+$0c00)
	bcc bobclr0
	lda #>screen1
	sta si+1
	inc di+1
	inc di+1
	inc di+1
	inc di+1
	dex
	bne bobclr0
;generates lookup tables
bob_tabinit 
	mwa #screen1 si
        ldx #0
tabinit0 lda si							;lookup table for 16 virtual screen adresses
        sta scrtabl,x
        lda si+1
        sta scrtabh,x
        lda si
        add #0
        sta si
        lda si+1
        adc #$10						;on each $1000 boundary 1 screen
        sta si+1
	inx
        cpx #16							;calculated for 16 virtual screens
        bcc tabinit0
	
;	jmp *
	
	ldx #59							;screen has 96x60 resolution
	ldy #0
	sty si
	sty si+1
tabinit1 lda si							;lookup tables for sprite setting routine
	sta linetabl,y					;calcs start adress of each line
	lda si+1
	sta linetabh,y+
	lda #48							;xasm specific code:adds 48 bytes to si
	add:sta si						;as each line has 48 bytes in vram
	scc:inc si+1
	dex
	bpl tabinit1

	;jmp *
        lda 53370						;now set default random values to start
        sta posx
        lda 53770
        sta posy
	lda 53770
	sta posx+1
	lda 53770
	sta posy+1
new_val  ;lda 53770						;now set random movement variables
        ;and #3
	ldx count4
	lda addxtabh,x
       ; lda #1
        sta addx+1
        ;lda 53770
        ;and #3
	;lda #2
        lda addytabh,x
        sta addy+1
        ;lda 53770
        lda addxtabl,x
        sta addx
        ;lda 53770
        lda addytabl,x
        sta addy
;	lda 53770
;	bmi new_val0
;	lda addx+1
;	eor #$ff
;	adc #1
;	sta addx+1
;new_val0 lda 53770							;sometimes negative move
 ;       lda addy+1
  ;      eor #$ff
  ;      adc #1
   ;     sta addy+1
		

;bob_loop
	lda #240
	sta count3
bob_loop inc count						;count = vram # number
    lda count
    cmp #5
    bcc loop00
    lda #0
    sta count			
loop00    tax								;now set the VRAM adress
    lda scrtabl,x
    sta dlist_part3.ad
    lda scrtabh,x
    sta dlist_part3.ad+1
	
	lda:cmp:req cloc		;wait 1 frame

	lda posx						;now calculate the movement of the sprite
	add addx
	sta posx
	lda posx+1
	adc addx+1
	sta posx+1
	tax
	lda posy
	add addy
	sta posy
	lda posy+1
	adc addy+1
	sta posy+1
	tay
	lda sintabx,x					;now get the sin values and center them on screen
	add #76							;mx
	tax
	lda sintaby,y
	add #26							;my
	tay
	jsr setshape					;x,y = position of sprite on screen, now set
	dec count3
	bne bob_loop
	dec count4
	bmi part4
    jmp new_val

part4 
;test exit to fire


		lda #200
		sta cloc
@		lda cloc
		bne @-

tunnel_loop


		lda:cmp:req cloc		;wait 1 frame
		lda #33
		sta $d400
		lda #$b0
		sta $d01a
		lda #$40
		sta $d01b
		lda #<(screen_tunnel)
		sta dlist_part3.ad
		lda #>(screen_tunnel)
		sta dlist_part3.ad+1
tunnel_loop0		

		lda:cmp:req cloc		;wait 1 frame

		lda #%11001010 ;basic off, rom off, bank #2
		sta $d301
		jsr draw_code
;		brk
		inc draw_code+1
		inc draw_code+1
;		dec draw_code+1
		lda draw_code+1
		and #$3f
		sta draw_code+1
		bne tunnel_loop0
		dec tunnel_loop1+1
tunnel_loop1	lda #160
		bne tunnel_loop0
		;jmp tunnel_loop0
;exit tunnel
exit_tunnel:
	jmp exit_tunnel

	lda:cmp:req cloc		;wait 1 frame

	lda #0
	sta $d40e
	lda #%01111111 ;basic off, rom on, XL-RAM
	sta $d301
	lda #0
	sta 559
	lda #$40
	sta $d40e
	cli
exit
	ldx #>filename_part2
	ldy #<filename_part2
	jmp xboot_load_file
filename_part2 .byte c'ARSPART2   '
;setshape[x,y]
;this routine sets the sprite in the non-active vram to generate the illusion
;of unlimited sprites on screen
setshape lda linetabl,y						;get line adress
	sta di
	lda linetabh,y
	add dlist_part3.ad+1				;in which screen should sprite be copied?
	sta di+1
	iny
	iny
	iny
	iny
	lda linetabl,y						;get line adress
	sta di2
	lda linetabh,y
	add dlist_part3.ad+1				;in which screen should sprite be copied?
	sta di2+1	
	txa					;xpos will be analysed
	clc
	lsr 					;x div 2 = byte no in scanline = y reg
	tay					;2 pixel per byte in mode9
	clc
	adc #48
	sta setshape2+1
	sta setshape3+1
	clc
	adc #48
	sta setshape4+1
	sta setshape5+1
	clc
	adc #48
	sta setshape6+1
	sta setshape7+1
	txa									;check if bit0 is set in xpos 
	and #1								;to detect in which nibble sprite has to be
	beq	setshape0
	jmp setshape1						;set and which shifted sprite data has to be
;copy data
setshape0
	lda (di),y							;used. (di),y points to vram
	and shape_l_mask
	ora shape_l							;get vram & OR sprite data
	sta (di),y
	lda(di2),y
	and shape_l_mask+16
	ora shape_l+16
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+1
	ora shape_l+1
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+1
	ora shape_l+16+1
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+2
	ora shape_l+2
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+2
	ora shape_l+16+2
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+3
	ora shape_l+3
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+3
	ora shape_l+16+3
	sta (di2),y							;next byte of sprite data...
	
setshape3 ldy #40							;next line
	lda (di),y
	and shape_l_mask+4
	ora shape_l+4
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+4
	ora shape_l+16+4
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+5
	ora shape_l+5
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+5
	ora shape_l+16+5
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+6
	ora shape_l+6
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+6
	ora shape_l+16+6
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+7
	ora shape_l+7
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+7
	ora shape_l+16+7
	sta (di2),y						;next byte of sprite data...	
setshape4 ldy #80							;next line (4x3 pixel sprite)
	lda (di),y
	and shape_l_mask+8
	ora shape_l+8
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+8
	ora shape_l+16+8
	sta (di2),y+						;next byte of sprite data...
	lda (di),y
	and shape_l_mask+9
	ora shape_l+9
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+9
	ora shape_l+16+9
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+10
	ora shape_l+10
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+10
	ora shape_l+16+10
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+11
	ora shape_l+11
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+11
	ora shape_l+16+11
	sta (di2),y							;next byte of sprite data...
setshape7 ldy #80							;next line (4x3 pixel sprite)
	lda (di),y
	and shape_l_mask+12
	ora shape_l+12
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+12
	ora shape_l+16+12
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+13
	ora shape_l+13
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+13
	ora shape_l+16+13
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+14
	ora shape_l+14
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+14
	ora shape_l+16+14
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_l_mask+15
	ora shape_l+15
	sta (di),y
	lda(di2),y
	and shape_l_mask+16+15
	ora shape_l+16+15
	sta (di2),y						;next byte of sprite data...

	rts
;0x
;here the same code but it uses the shifted sprite
setshape1 
	lda (di),y							;used. (di),y points to vram
	and shape_h_mask
	ora shape_h							;get vram & OR sprite data
	sta (di),y
	lda(di2),y
	and shape_h_mask+20
	ora shape_h+20
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+1
	ora shape_h+1
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+1
	ora shape_h+20+1
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+2
	ora shape_h+2
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+2
	ora shape_h+20+2
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+3
	ora shape_h+3
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+3
	ora shape_h+20+3
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+4
	ora shape_h+4
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+4
	ora shape_h+20+4
	sta (di2),y							;next byte of sprite data...
	
setshape2 ldy #40							;next line
	lda (di),y
	and shape_h_mask+5
	ora shape_h+5
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+5
	ora shape_h+20+5
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+6
	ora shape_h+6
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+6
	ora shape_h+20+6
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+7
	ora shape_h+7
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+7
	ora shape_h+20+7
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+8
	ora shape_h+8
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+8
	ora shape_h+20+8
	sta (di2),y+						;next byte of sprite data...	
	lda (di),y
	and shape_h_mask+9
	ora shape_h+9
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+9
	ora shape_h+20+9
	sta (di2),y							;next byte of sprite data...

setshape5 ldy #80							;next line (4x3 pixel sprite)
	lda (di),y
	and shape_h_mask+10
	ora shape_h+10
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+10
	ora shape_h+20+10
	sta (di2),y+						;next byte of sprite data...
	lda (di),y
	and shape_h_mask+11
	ora shape_h+11
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+11
	ora shape_h+20+11
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+12
	ora shape_h+12
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+12
	ora shape_h+20+12
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+13
	ora shape_h+13
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+13
	ora shape_h+20+13
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+14
	ora shape_h+14
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+14
	ora shape_h+20+14
	sta (di2),y							;next byte of sprite data...
setshape6 ldy #80							;next line (4x3 pixel sprite)
	lda (di),y
	and shape_h_mask+15
	ora shape_h+15
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+15
	ora shape_h+20+15
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+16
	ora shape_h+16
	sta (di),y
	lda(di2),y
	and shape_h_mask+20+16
	ora shape_h+20+16
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+17
	ora shape_h+17
	sta (di),y
	lda (di2),y
	and shape_h_mask+20+17
	ora shape_h+20+17
	sta (di2),y+							;next byte of sprite data...
	lda (di),y
	and shape_h_mask+18
	ora shape_h+18
	sta (di),y
	lda (di2),y
	and shape_h_mask+20+18
	ora shape_h+20+18
	sta (di2),y+						;next byte of sprite data...
	lda (di),y
	and shape_h_mask+19
	ora shape_h+19
	sta (di),y
	lda (di2),y
	and shape_h_mask+20+19
	ora shape_h+20+19
;	sta (di2),y					;next byte of sprite data...
	rts


bob_dli
;DLI routine which is needed for the Mode9++ trick 
 		pha
    		sta $d40a
    		lda #13
    		sta $d405
    		lda #3
    		sta $d405
   		pla
    		rti


		
backup_char16
:8		.byte 0


;		dec steps
;		bpl automove
;		jsr autopilot_init
;automove jsr autopilot
;		jmp loop_main

;---------------------------------------
; ray tracing subroutines
;---------------------------------------
            ; heart of tracing, very slow, because of looping
            ; for x and y components and also because of
            ; brute force approach
addsteptopos
;            ldx #$02
loop_stepadd
            lda stepx+2    ; & y
            ora #$7f       ; sign extend 8 bit step value to 16 bit
            bmi *+4
            lda #$00
            pha
            clc
            lda stepx+2    ; & y
            adc rayposx+2  ; & y
            sta rayposx+2  ; & y
            pla
            adc rayposxh+2 ; & y
            sta rayposxh+2 ; & y

            lda stepx+1    ; & y
            ora #$7f       ; sign extend 8 bit step value to 16 bit
            bmi *+4
            lda #$00
            pha
            clc
            lda stepx+1    ; & y
            adc rayposx+1  ; & y
            sta rayposx+1  ; & y
            pla
            adc rayposxh+1 ; & y
            sta rayposxh+1 ; & y

            lda stepx    ; & y
            ora #$7f       ; sign extend 8 bit step value to 16 bit
            bmi *+4
            lda #$00
            pha
            clc
            lda stepx    ; & y
            adc rayposx  ; & y
            sta rayposx  ; & y
            pla
            adc rayposxh ; & y
            sta rayposxh ; & y


            ;dex
            ;dex
            ;bpl loop_stepadd
            
            ; ar = rayposxh

            ; calculate index to look up the map cell
            ; the map area is 8x8 bytes
            ; instead of the usual y * 8 + x
            ; x * 8 + y done here, to save some bytes
            ; (just causing a flip of the map as a side effect)
            asl
            asl
            asl
            ; by doing ora instead of adc, it is possible to have
            ; a closed area map in $ecb9
            adc rayposyh
            ;ora rayposyh
            tax
            lda map_t,x
step_exit
            rts
;---------------------------------------
getsincos_copyplr2ray
            lda sin_t,x      ; sin(x)
            sta stepx
            lda sin_t+$40,x  ; cos(x)
            sta stepy
            ; copy player position to ray position for a start
            ; through the basic rom
            jmp copyplr2ray
;---------------------------------------
invertstepandcopy
            ; invert step variables for backward motion
            ;jsr invertstepx ; eor stepx through basic rom
            lda stepx
            eor #$ff
            sta stepx
            lda stepy
            eor #$ff
            sta stepy
;---------------------------------------
stepandcopy
            ; see if player can move to the direction desired
            jsr addsteptopos
            bne step_exit ; no, return without doing anything
            ; yes, move player by
            ; copying ray position to player position
            ; through the basic rom
;            jmp copyray2plr
copyray2plr
		ldx #6
cp1		lda rayposx-1,x
		sta stepy,x
		dex 
		bne cp1
		stx distance
		rts
		           
copyplr2ray
		ldx #5
cp0		lda stepy,x
		sta rayposx-1,x
		dex
		bne cp0
		stx distance
		rts

autopilot_init:
		lda #0
		sta steps
		sta movescript_counter
		rts
autopilot: 
		lda direction
		cmp #2
		bne auto0
		dec heading
		dec heading
dummy		rts
auto0	cmp #1
		bne auto1
		lda dlist
		eor #$40
		sta dlist
		jmp stepandcopy
auto1	cmp #4
		bne auto2
		lda dlist
		eor #$40
		sta dlist
		jmp invertstepandcopy
auto2	cmp #3
		bne auto3
		inc heading
		inc heading
auto3	rts		

steps .byte 0
direction .byte 0
movescript_counter .byte 0



set_chars     LDA #$00
         STA $b0
         STA $b1

;main loop
L4EB3:   
		ldx #32
		
xxx		dex
		bne xxx
		
		LDA RANDOM ;random position start
         STA $b2
         LDA RANDOM
         AND #$03
         ORA #>vram ;$20 ;$2xyy
         STA $b3
;range check inside VRAM
         CMP #>(vram+$300) ;$23
         BCC L4ECB
         LDA $b2
         CMP #$97
         BCS L4EB3
         
L4ECB:   LDA #$F7 ;tresh hold ?
         LDY #$29 ; pos(x+1,y+1)
         CMP ($b2),Y
         bcc L4EB3
         LDX #$00
         LDY #$00 ;pos(x,y)
         CMP ($b2),Y 
         BCS L4EDC
         INX
L4EDC:   INY ;pos(x+1,y)
         CMP ($b2),Y
         BCS L4EE2
         INX
L4EE2:   INY ;pos(x+2,y)
         CMP ($b2),Y
         BCS L4EE8
         INX
L4EE8:   LDY #$28 ;pos(x,y+1)
         CMP ($43),Y
         BCS L4EEF
         INX
L4EEF:   LDY #$2A ;pos(x+2,y+1)
         CMP ($b2),Y
         BCS L4EF6
         INX
L4EF6:   LDY #$50 ;pos(x,y+2)
         CMP ($b2),Y
         BCS L4EFD
         INX
L4EFD:   INY ;pos(x+1,y+2)
         CMP ($b2),Y
         BCS L4F03
         INX
L4F03:   INY ;pos(x+2,y+2)
         CMP ($b2),Y
         BCS L4F09
         INX
L4F09:   CPX #$02 ;trashold 2
         BCC L4EB3
         LDA RANDOM
         AND #$07
         ORA #$F8
         LDY #$29
         STA ($b2),Y
         INC $b0
         BNE L4F1E
         INC $b1
L4F1E:   LDA $b0
         CMP #$8e ;low byte of counter
         BEQ L4F27
L4F24:   JMP L4EB3
L4F27:   LDA $b1
         CMP #$03 ;hi byte of counter
         BNE L4F24
         LDY #$27
         LDA #$F8 
L4F31:   STA vram,Y
         STA vram+$3c0,Y
         DEY
         BPL L4F31
         STA vram+40;$2028
         RTS

.proc set_text
set_text     LDA #$00
         STA $b0
         STA $b1
;main loop
L4EB3:   
		ldx #64
xxx		dex
		bne xxx

		LDA RANDOM ;random position start
         STA $b2
         LDA RANDOM
         AND #$03
         ORA #$20 ;$2xyy
         STA $b3
;range check inside VRAM
         CMP #$23
         BCC L4ECB
         LDA $b2
         CMP #$97
         BCS L4EB3
         
L4ECB:   LDA #$f7 ;tresh hold ?
         LDY #$29 ; pos(x+1,y+1)
         CMP ($b2),Y
         bcs L4EB3
         LDX #$00
         LDY #$00 ;pos(x,y)
         CMP ($b2),Y 
         BCS L4EDC
         INX
L4EDC:   INY ;pos(x+1,y)
         CMP ($b2),Y
         BCS L4EE2
         INX
L4EE2:   INY ;pos(x+2,y)
         CMP ($b2),Y
         BCS L4EE8
         INX
L4EE8:   LDY #$28 ;pos(x,y+1)
         CMP ($43),Y
         BCS L4EEF
         INX
L4EEF:   LDY #$2A ;pos(x+2,y+1)
         CMP ($b2),Y
         BCS L4EF6
         INX
L4EF6:   LDY #$50 ;pos(x,y+2)
         CMP ($b2),Y
         BCS L4EFD
         INX
L4EFD:   INY ;pos(x+1,y+2)
         CMP ($b2),Y
         BCS L4F03
         INX
L4F03:   INY ;pos(x+2,y+2)
         CMP ($b2),Y
         BCS L4F09
         INX
L4F09:   CPX #$07 ;trashold 2
         bcs L4EB3
		 lda $b2
		 clc
		 adc #<scr
		 sta $b4
		 lda $b3
		 and #$03
		 clc
		 adc #>scr
		 sta $b5
;         LDA RANDOM
;        AND #$07
;         ORA #$F8
         LDY #$29
 		 lda ($b4),y
         STA ($b2),Y
         INC $b0
         BNE L4F1E
         INC $b1
L4F1E:   LDA $b0
         CMP #$8e ;low byte of counter
         BEQ L4F27
L4F24:   JMP L4EB3
L4F27:   LDA $b1
         CMP #$03 ;hi byte of counter
         BNE L4F24
;         LDY #0
;L4F31:   lda text,y
		 ;STA vram,Y
         ;STA vram+256,Y
;        sta vram+512,y
;         sta vram+768,y
 ;        iny
;        bne L4F31
         RTS
.endp


.proc pm_scroll_init
		lda #0		;clear pm
		tax
		sta ypos
		sta charpos
		sta pmscroll_counter
		sta pmscroll_direction
		;lda #$00
l00		sta p0,x
		sta p0+256,x
		sta p0+512,x
		sta p0+768,x
		sta p0-256,x
		inx
		bne l00
		;brk
		lda #3
		sta sizep0
		sta sizep0+1
		sta sizep0+2
		sta sizep0+3
		lda #$ff
		sta sizep0+4 
		lda #15
		sta $d012
		sta $d013
		sta $d014
		sta $d015
		
		lda #$88
		sta $d012
		sta $d013
		sta $d014
		sta $d015
		
		lda #48
		sta hposp0
		lda #48+32
		sta hposp0+1
		lda #48+64
		sta hposp0+2
		lda #48+64+32
		sta hposp0+3
		lda #48+64+64
		sta hposp0+7
		lda #48+64+64+8
		sta hposp0+6
		lda #48+64+64+16
		sta hposp0+5
		lda #48+64+64+24
		sta hposp0+4
		lda #3
		sta 53277
		lda #$41
		sta $d01b
		lda #>(p0&$f000)
		sta pmbase
		txa
		sta temp
		lda #>pmfont
		sta temp+1
l01		lda temp
		sta chartabl,x
		lda temp+1
		sta chartabh,x
		clc
		lda temp
		adc #8
		sta temp
		bcc l02
		inc temp+1
l02		inx
		cpx #128
		bne l01
		lda #8
		sta scrollflag
		lda #0
		sta softscroll
		tax
@		lda #0
		sta charbuffer,x
		inx
		bne @-
		rts
	
.endp

.proc do_pm_scroll
scroll:	lda scrollflag
		bne scroll0

		lda #8
		sta scrollflag
		ldx charpos
		lda scrolltext,x
;in a = char		
copychar: sec
		sbc #32 
		bpl @+
		lda #0
@
		tax
		lda chartabl,x
		sta co1+1
		lda chartabh,x
		sta co1+2
		ldy #7
co1		lda $ffff,y
		sta charbuffer+40,y
		dey
		bpl co1
		inc charpos
		
;		jmp copychars
		
scroll0	dec scrollflag
		rol charbuffer+40
		rol charbuffer+32
		rol charbuffer+24
		rol charbuffer+16
		rol charbuffer+8		
		rol charbuffer
		rol charbuffer+40+1
		rol charbuffer+32+1
		rol charbuffer+24+1
		rol charbuffer+16+1
		rol charbuffer+8+1
		rol charbuffer+1
		rol charbuffer+40+2
		rol charbuffer+32+2
		rol charbuffer+24+2
		rol charbuffer+16+2
		rol charbuffer+8+2
		rol charbuffer+2
		rol charbuffer+40+3
		rol charbuffer+32+3
		rol charbuffer+24+3
		rol charbuffer+16+3
		rol charbuffer+8+3
		rol charbuffer+3
		rol charbuffer+40+4
		rol charbuffer+32+4
		rol charbuffer+24+4
		rol charbuffer+16+4
		rol charbuffer+8+4
		rol charbuffer+4
		rol charbuffer+40+5
		rol charbuffer+32+5
		rol charbuffer+24+5
		rol charbuffer+16+5
		rol charbuffer+8+5
		rol charbuffer+5
		rol charbuffer+40+6
		rol charbuffer+32+6
		rol charbuffer+24+6
		rol charbuffer+16+6
		rol charbuffer+8+6		
		rol charbuffer+6
		rol charbuffer+40+7
		rol charbuffer+32+7
		rol charbuffer+24+7
		rol charbuffer+16+7
		rol charbuffer+8+7
		rol charbuffer+7
;now copy buffer to screen
copychars: 
		ldx pmscroll_counter
		lda pmsintab,x
		clc
		adc #32
		tay
		ldx #0
@		lda charbuffer,x
		sta p0,y
		sta p0+1,y
		sta p0+2,y
		sta p0+3,y
		sta p0+4,y
		lda charbuffer+8,x
		sta p0+256,y
		sta p0+256+1,y
		sta p0+256+2,y
		sta p0+256+3,y
		sta p0+256+4,y
		lda charbuffer+16,x
		sta p0+512,y
		sta p0+512+1,y
		sta p0+512+2,y
		sta p0+512+3,y
		sta p0+512+4,y
		lda charbuffer+24,x
		sta p0+768,y
		sta p0+768+1,y
		sta p0+768+2,y
		sta p0+768+3,y
		sta p0+768+4,y
		lda charbuffer+32,x
		sta p0-256,y
		sta p0-256+1,y
		sta p0-256+2,y
		sta p0-256+3,y
		sta p0-256+4,y
		inx
		iny
		iny
		iny
		iny
		iny
		cpx #8
		bne @-

		lda pmscroll_direction
		bne do_scroll2
		inc pmscroll_counter
		lda pmscroll_counter
		cmp #$75
		beq do_scroll3
		rts
do_scroll3
		lda #1
		sta pmscroll_direction
		rts
do_scroll2
		dec pmscroll_counter
		beq do_scroll4
		rts
do_scroll4
		lda #0
		sta pmscroll_direction
		rts
.endp

.proc dli
		sta dli.rega+1
		stx dli.regx+1
		ldx #140
dli0	
		sta $d40a
dlicol		lda pmcolors-1,x
		sta $d012
		sta $d013
		sta $d014
		sta $d015
		dex
		bne dli0
dli1
rega		lda #0 ;rega
regx		ldx #0 ;regx
		rti
.endp


dummy_dli	rti


.proc	NMI

	bit nmist
	bpl VBL

	jmp dli ;dummy_dli
dliv	equ *-2

VBL
	sta regA
	stx regX
	sty regY

	sta nmist		;reset NMI flag

	lda #0
	sta linecounter
    lda $d300            ;prepare joystick
    and #$0f
    sta stick0
	
;	mwa #dlist dlptr		;ANTIC address program

	inc cloc		;little timer
	inc counter
	bne vbl0
	inc counter+1
vbl0
	lda #%11000010 ;basic off, rom off, bank #0
	sta $d301
	jsr rastermusictracker+3
bank_select lda #$fe
	sta $d301
	;inc dli.dlicol+1
	;mwa #DLI.dli_start dliv	;set the first address of DLI interrupt

;this area is for yours routines
	;jsr vbl_fontcopy
fx_subroutines
	jsr dummy
	jsr do_pm_scroll
;for inserting addition FX routines
;	nop
;	nop
;	nop
	
quit
	lda regA
	ldx regX
	ldy regY
	rti

vbl_fontcopy
	ldx #63
fontcopy	lda 53770
		sta font_part2+$78*8,x
		dex
		bpl fontcopy
		rts

.endp
;show title screen

.proc show_desire_logo
		lda:cmp:req cloc	;wait 1 frame
		mwa #dlist_mode10 dlptr		;ANTIC address program
		lda #$80
		sta $d01b
		lda #34
		sta dmactl
		lda #$2
		sta $d012
		lda #$80
		sta $d013
		lda #$84
		sta $d014
		lda #$96
		sta $d015
		lda #$88
		sta $d016
		lda #$02
		sta $d017
		lda #$9a
		sta $d018
		lda #$ac
		sta $d019
		lda #$0f
		sta $d01a

;tabinit
	lda #2
	ldx #0
@	sta coltab,x
	inx
	bne @-
@
	lda #2
	sta coltab+256,x
	sta coltab+96+256,x
	lda #4
	sta coltab+8+256,x
	sta coltab+88+256,x
	lda #6
	sta coltab+16+256,x
	sta coltab+80+256,x
	lda #8
	sta coltab+24+256,x
	sta coltab+72+256,x
	lda #10
	sta coltab+32+256,x
	sta coltab+64+256,x
	lda #12
	sta coltab+40+256,x
	sta coltab+56+256,x
	lda #14
	sta coltab+48+256,x
	inx
	cpx #8
	bcc @-
	lda #2
	ldy #$08
dsrfill ldx #0
dsrfill0 sta coltab+104+256,x
	inx
	bne dsrfill0
	inc dsrfill0+2
	dey
	bne dsrfill

kernel
	ldy $d40b
	bne kernel
	lda color
	sta colad+1
	lda color+1
	and #$07 ;3
	clc
	adc #>coltab
	sta colad+2

kernel0
colad	lda $ffff
	sta $d40a
	sta $d017
	inc colad+1
	bne kernel1
	inc colad+2
kernel1
	iny
	cpy #192+24
	bcc kernel0
	lda color
	clc
	adc #4
	sta color
	bcc @+
	inc color+1
@
	lda color+1
	cmp #7
	bne kernel
	lda #0
	sta color+1
	sta color
	dec cycles
	beq kernel_exit
	lda:cmp:req cloc	;wait 1 frame
	
	jmp kernel
kernel_exit 
	lda #$40
	sta $d01b
	lda #$90
	sta $d01a
	rts
		
color	.word 0
cycles .byte 2
.endp
	
scrolltext 	dta d'          SCREAM FOR ME  PARTY PEOPLE       SCREAM FOR ME    HERE AT SILLY VENTURE   '
		dta d'          LET S ROCK N ROLL          AND LET END     '
		.byte $ff

;---------------------------------------
; data
;---------------------------------------
            ; number of sin additions (backwards)
sincount_t
            .byte 6,14,19,25
;---------------------------------------
linel_t
:24		.byte <(vram+#*40)	
lineh_t
:24		.byte >(vram+#*40)
		.align $100
map8x8		
		.he 11 11 00 00 11 00 11 11
		.he 11 00 00 00 00 00 00 11
		.he 11 00 00 00 00 00 00 11
		.he 00 00 00 00 11 00 00 00
		.he 11 00 00 11 11 11 00 11
		.he 11 00 00 00 00 11 00 11
		.he 11 00 00 00 00 00 00 11
		.he 11 00 00 00 00 11 11 11

movescript:
;00AA wait AA frames
;01 forward
;02 left
;03 right
;04 back
;ff end

		.byte $04,$09
		.byte $02,$02
		.byte $03,$09
		.byte $01,$02
		.byte $03,$03
		.byte $01,$0c
		.byte $03,$08
		.byte $02,$02
		.byte $01,$03
		.byte $03,$06
		.byte $02,$02
		.byte $03,$0a
		;.byte $01,$0a
		;.byte $03,$06
		.byte $ff,$ff
;		.he 00 00 00 00 00 00 00 00
;		.he 00 00 00 00 00 00 00 00
;		.he 00 00 00 00 00 00 00 00
;		.he 00 00 00 11 11 11 11 11
;		.he 00 00 00 00 00 00 00 00
;		.he 00 00 00 11 00 00 00 00
;		.he 00 00 00 11 00 00 00 00
;		.he 00 00 00 11 00 00 00 00
;		.he 11 00 00 11 00 00 00 11


		org scr
		ins "DSR-LOGO-2-by-akira.scr"	;$b800-$bcaf

		run main
		