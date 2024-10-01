maxlines = $60

;	opt f+

;                       VBXE summary
;
; $D640   video control bit0 - xdl enable
;                       bit1 - enable 256 colors for GTIA instead of 128
;                       bit2 - disable transparency for color 0
;                       bit3 - enable color $xF being transparent
; $D641-3 XDL address low/mid/high
; $D644   MSEL write $Cx to update palette x (after MB0), write $80 to
; $D645   MB0 write colr index (after RGB) \               update 
; $D646   MB1 write R component             \             priority
; $D647   MB2 write G component              \             definitions
; $D648   MB3 write B component               \
; $D649   colmask - mask applied to overlay color for collision detection
; $D64A   colclr - resets coldetect
;       read = coldetect - bits correspond to GTIA colors that have collided
;                          with an overlay color
; $D64C   ma-cpu   \ bit 7 enables VRAM window at $2000-$3FFF
; $D64D   ma-antic / bits 0-5 select bank
; $D64E   mb-cpu   \ bit 7 enables VRAM window at $4000-$7FFF
; $D64F   mb-antic / bits 0-4 select bank
; $D650-2 blitter address
;       read $D650 = collision code >0 pixel value that was overwritten
; $D653   blitter start - write 1 to start, write 0 to stop
;       read = blitter busy (busy when non-zero)
; $D654   IRQ control - bit 0 enables blitter done IRQ
;
; XDL 
; byte 0
;   bit 0 - set to enable text mode
;   bit 1 - set to enable gfx mode
;   bit 2 - set to disable overlay
;   bit 3 - set to enable color attributes
;   bit 4 - set to disable color attributes
;   bit 5 - RPTL
;   bit 6 - OVADR
;   bit 7 - OVSCRL
; byte 1
;   bit 0 - CHBASE
;   bit 1 - MAPADR (color attributes)
;   bit 2 - MAPPAR (color attributes)
;   bit 3 - OVATT
;   bit 4 - enable hi-res mode (640)
;   bit 5 - enable low-res mode (160)
;   bit 6 - reserved
;   bit 7 - end of XDL, wait for VBLANK

; RPTL - number of scanlines-1 for this modeline
; OVADR - 3 bytes overlay address, 2 bytes line step size
; OVSCRL - 1 byte hscroll, 1 byte vscroll
; CHBASE - high byte of charset address
; MAPADR - 3 bytes map address, 2 bytes line step size
; MAPPAR - 1b hscroll, 1b vscroll, 1b cell width-1, 1b cell height-1
; OVATT - byte 1 - overlay width and color shift
;         byte 2 - priority bits - bit0 - overlay above PM0
;                                  bit1 - ... PM1
;                                  bit2 - ... PM2
;                                  bit3 - ... PM3
;                                  bit4 - ... PF0
;                                  bit5 - ... PF1
;                                  bit6 - ... PF2
;                                  bit7 - ... PF3
; attributes map format
; byte 0 - colpf0 substitute color
; byte 1 - colpf1
; byte 2 - colpf2
; byte 3 - bits 0-1 - priority setting select
;          bit 2 - resolution override
;          bit 3 - reserved
;          bits 4-5 - overlay palette select
;          bits 6-7 - GTIA palette select
;
; Blitter Command Block
; bytes 0-2 source addr
; bytes 3-4 source line step
; bytes 5-7 dest. addr
; bytes 8-9 dest. line step
; bytes 10-11 obj width-1 in bytes
; byte 12 obj height
; byte 13 source AND mask
; byte 14 source XOR mask
; byte 15 collision AND mask
; byte 16 zoom bits 0-2 - x zoom
;              bit 3 - increment by 2 if set, else increment by 1
;              bits 4-6 - y zoom
; byte 17 pattern - bit 7 enables repeating source data horizontally
;                   bits 0-5 pattern width       
; byte 18 control - bits 0-2 - MODE
;                   bit3 - NEXT - 0 for last BCB
;                   bit4 - source x decrement if set, else increment
;                   bit5 - source y
;                   bit6 - dest. x
;                   bit7 - dest. y
; mode 0 - copy opaque
; mode 1 - copy with transparency
; mode 2 - source added to dest.
; mode 3 - OR
; mode 4 - AND
; mode 5 - XOR
; mode 6 - same as 1, with transparency and collision detection by nibbles
; mode 7 - reserved
VBXE_BASE			equ	0xd600		;d600, d700 or 0 (autodetection)
VBXE_VIDEO_CONTROL	equ	VBXE_BASE+0x40
VBXE_XDL_ADR0		equ	VBXE_BASE+0x41
VBXE_XDL_ADR1		equ	VBXE_BASE+0x42
VBXE_XDL_ADR2		equ	VBXE_BASE+0x43
VBXE_CSEL			equ	VBXE_BASE+0x44
VBXE_PSEL			equ	VBXE_BASE+0x45
VBXE_CR				equ	VBXE_BASE+0x46
VBXE_CG				equ	VBXE_BASE+0x47
VBXE_CB				equ	VBXE_BASE+0x48
VBXE_COLMASK		equ	VBXE_BASE+0x49
VBXE_COLCLR			equ	VBXE_BASE+0x4a
;VBXE_MA_CPU			equ	VBXE_BASE+0x4c
;VBXE_MA_ANTIC		equ	VBXE_BASE+0x4d
;VBXE_MB_CPU			equ	VBXE_BASE+0x4e
;VBXE_MB_ANTIC		equ	VBXE_BASE+0x4f
VBXE_BL_ADR0		equ	VBXE_BASE+0x50
VBXE_BL_ADR1		equ	VBXE_BASE+0x51
VBXE_BL_ADR2		equ	VBXE_BASE+0x52
VBXE_BLITTER_START	equ	VBXE_BASE+0x53
VBXE_BLITTER_BUSY	equ	VBXE_BASE+0x53
VBXE_IRQ_CONTROL	equ	VBXE_BASE+0x54


music_flag=0
stereomode=0
divtab	equ $7f00

;vram	equ $7000 ;-$aa00
;title_screen equ vram+$10
;backdrop equ $3000

screen equ $20
pointer equ $01 ;$02
pointer2 equ $03 ;$04
lines equ $05
points equ $06
charoffset equ $07
char_flag equ $08

cloc equ $14 ;$15
random	equ 53770

gradientmap	equ $4000 ;xl ram, banked via VBXE
redge equ $7d00
ledge equ $7e00

M:	EQU $25
S:	EQU $26
I:	EQU $27
J:	EQU $28
u	equ $29
v	equ $2a
dx	equ u
dy	equ v


sx1	equ $2b
sx2	equ $2c
sy1	equ $2d
sy2	equ $2e
X1:	EQU sx1
X2:	EQU sx2
Y1:	EQU sy1
Y2:	EQU sy2

T1 	equ $2f ;2
T2 	equ $31 ;2
PRODUCT equ $33 ;4
xreg_save equ $37
yreg_save equ $38
linetype equ $39
linetype2 equ $3a

square1_lo equ $c000
square1_hi equ square1_lo+512
square2_lo equ square1_hi+512
square2_hi equ square2_lo+512

;linebcbstartlo equ $d800
;linebcbstarthi equ $d900
;linebcbendlo equ $da00
;linebcbendhi equ $db00



fps	equ $0600

;	org $2000
;	lda #$84 ;$10000
;	sta $d65d
;	rts
;	ini $2000
;	
;	org $4000
;witchpic ins "ring2.bmp",54+1024
;
;	org $2000
;	lda #$00 ;window off
;	sta $d65d
;	rts
;	ini $2000

		org $2000
	.proc VBXE_SetPalette
.local
		ldx	#1
		stx	VBXE_PSEL
		ldx	#0
		stx	VBXE_CSEL ;which colour # to change
;		ldy #0
;		ldx #0
;@	
;		lda 	coltab_r,x
;		sta	VBXE_CR ;R
;		lda	coltab_g,x
;		sta	VBXE_CG ;G
;		lda 	coltab_b,x
;		sta	VBXE_CB  ;B
;		inx
;		iny
;		bpl	@-

		ldx	#0 ;108
		stx	VBXE_CSEL ;which colour # to change
		ldy #0
setcolloop	
setcol		lda pal1+2
		sta VBXE_CR
setcol1		lda pal1+1
		sta VBXE_CG
setcol2		lda pal1
		sta VBXE_CB
		lda setcol+1
		clc
		adc #4
		sta setcol+1
		lda setcol+2
		adc #0
		sta setcol+2
		lda setcol1+1
		clc
		adc #4
		sta setcol1+1
		lda setcol1+2
		adc #0
		sta setcol1+2
		lda setcol2+1
		clc
		adc #4
		sta setcol2+1
		lda setcol2+2
		adc #0
		sta setcol2+2
		
		dey
		bne setcolloop
.endl
.local
		ldx	#2
		stx	VBXE_PSEL
		ldx	#0
		stx	VBXE_CSEL ;which colour # to change
;		ldy #0
;		ldx #0
;@	
;		lda 	coltab_r,x
;		sta	VBXE_CR ;R
;		lda	coltab_g,x
;		sta	VBXE_CG ;G
;		lda 	coltab_b,x
;		sta	VBXE_CB  ;B
;		inx
;		iny
;		bpl	@-

		ldx	#0 ;108
		stx	VBXE_CSEL ;which colour # to change
		ldy #0
setcolloop	
setcol		lda pal2+2
		sta VBXE_CR
setcol1		lda pal2+1
		sta VBXE_CG
setcol2		lda pal2
		sta VBXE_CB
		lda setcol+1
		clc
		adc #4
		sta setcol+1
		lda setcol+2
		adc #0
		sta setcol+2
		lda setcol1+1
		clc
		adc #4
		sta setcol1+1
		lda setcol1+2
		adc #0
		sta setcol1+2
		lda setcol2+1
		clc
		adc #4
		sta setcol2+1
		lda setcol2+2
		adc #0
		sta setcol2+2
		
		dey
		bne setcolloop
.endl
		rts
		
		
 ;ins "palette_orange.bmp",54+48*4,4*256
		;ins "palette_blue.bmp",54+4*4,4*256
		;ins "palette_smoke.bmp",54,1024
		;ins "infrared2_palette.bmp",54,1024

pal1		ins "firepal5b.bmp",54,1024
pal2		ins "firepalir1.bmp",54,1024
	.endp
	ini VBXE_SetPalette
	

	org $800
linebcbstartlo
:128	.byte <$2000
:128	.byte  <($2000+#*21)
linebcbstarthi
:128	.byte >$2000
:128	.byte  >($2000+#*21)
linebcbendlo
:128	.byte <($6000+20)
:128	.byte  <($6000+#*21+20)
linebcbendhi
:128	.byte >($6000+20)
:128	.byte  >($6000+#*21+20)
;linebcbstartlo2
;:128	.byte  <($2000+#*21)
;linebcbstarthi2
;:128	.byte  >($2000+#*21)
;linebcbendlo2
;:128	.byte  <($6000+#*21+20)
;linebcbendhi2
;:128	.byte  >($6000+#*21+20)

.proc	NMI
	bit $d40f
	bpl VBL
	jmp dli
dliv	equ *-2

VBL
	sta vbl_a+1
	stx vbl_x+1
	sty vbl_y+1
	dec cloc
	bne @+
	dec cloc+1
@	
;	inc do_vect+1
	lda #0
	sta $d65d
	sta $d65e
	jsr mpt_player.play
	lda memwindow
	sta $d65d
	lda memwindow2
	sta $d65e
backgrnd	lda #$c0
	sta $d01a
vbl_a	lda #0
vbl_x	ldx #0
vbl_y	ldy #0
dli	rti

.endp
memwindow .byte 0
memwindow2 .byte 0
; init
init
		sei			;stop IRQ interrupts
		mva #$00 $d40e		;stop NMI interrupts
		sta $d20e
	        sta $D640       ; enable XDL
		sta $d400
		mva #$fe $d301		;switch off ROM to get 16k more ram

		lda #<nmi
		sta $fffa
		sta $fffe
		lda #>nmi
		sta $fffb
		sta $ffff

;		ldx #0
;		txa
;@		sta 0,x
;		inx
;		bne @-		
	
        lda #$80		;bank 0
        sta $d65d       ;cpu-vram access window at $4000
	sta memwindow

        ldx #23
a3:     lda xdl,x
        sta $4000,x     ; copy XDL to vram
        dex
        bpl a3
        
	ldx #0
@	lda slope_bcb,x
	sta $4080,x
	inx
	cpx #21
	bne @-

	ldx #0
@	lda slope2_bcb,x
	sta $40c0,x
	inx
	cpx #21*2
	bne @-

	
        ldx #0
a8:     lda cls_bcb,x
        sta $4200,x ;$000200 = $4200 bank #0 
        inx
        cpx #21*5
        bne a8
        
	
	ldx #0
a9:	lda add_bcb,x
	sta $4300,x
	inx
	cpx #8*21 
	bne a9
	
	ldx #0
@	lda moveup_bcb,x
	sta $43c0,x
	inx
	cpx #21
	bne @-

	ldx #0
@	lda reset_bcb,x
	sta $43e0,x
	inx
	cpx #21
	bne @-
	
	
	ldy #0
@	ldx #0
a10	lda div4_bcb,x
a11	sta $4440,x ;v:$000440
	inx
	cpx #21
	bne a10
	
	inc div4_bcb+6
	
	lda a11+1
	clc
	adc #21
	sta a11+1
	lda a11+2
	adc #0
	sta a11+2
	iny
;	cpy #128
	bne @-
	lda #0
	sta $4440+255*21+20 ;$1500

;random
	ldx #0
@
	lda 53770
	and #$4f
	sta $5e00,x
	lda 53770
	and #$4f
	sta $5f00,x
	inx
	bne @-
	

;line 256x
	ldy #0
@	ldx #0
b10	lda line_bcb,x
b11	sta $6000,x ;v:$002000
	inx
	cpx #21
	bne b10
	
	inc line_bcb+7 ;ypos
	
	lda b11+1
	clc
	adc #21
	sta b11+1
	lda b11+2
	adc #0
	sta b11+2
	iny
;	cpy #128
	bne @-
	
	lda #0
	sta $6000+255*21+20
	
	lda #$82
	sta $d65d
	sta memwindow

;line 256x
	ldy #0
@	ldx #0
bb10	lda line2_bcb,x
bb11	sta $4000,x ;v:$008000
	inx
	cpx #21
	bne bb10
	
	inc line2_bcb+6 ;xpos
	
	lda bb11+1
	clc
	adc #21
	sta bb11+1
	lda bb11+2
	adc #0
	sta bb11+2
	iny
	bne @-
	lda #0
	sta $4000+255*21+20
	
	jsr init_gouraud_map
	
		;jsr prepare_pal7
		;jsr VBXE_SetPalette

	
;  		lda:cmp:req 20      ;wait 1 frame

		jsr generate_square_tables

		lda #16
		sta char_flag
		lda #0
		sta $d65d
		
;@		lda $d40b
;		bne @-
;		jsr $3900
;		jmp @-
;	
		
		lda #0
		sta memwindow
		cli
		lda #$40
		sta $d40e
;@
;		lda 53770
;		sta $d01a
;		jmp @-
		
		lda #0
		sta $d400
		lda #1
		sta cloc+1
		lda #200
		sta cloc
	
@		lda cloc+1
		bne @-
		
@		lda cloc
		bne @-
;setcolors for 3d
		
		jsr mocap_main
		
		ldy #0
	
	lda #$07
	sta screen
	jsr clear_vram
	lda #2
	sta $4200+20
	lda #$ff
	sta $4200+15
	lda #$40
	sta $4200
	lda #$a8
	sta $4201
	jsr init_div4tab

	lda cloc
	cmp cloc
	beq *-2
	
	lda #0
        sta $D641       ;
        sta $D642       ;
        sta $D643       ;
        sta $D647       ; disable collision detection
        sta $D654       ; disable IRQ
        lda #1
        sta $D640       ; enable XDL

	lda #$80
	sta $d65d
	sta memwindow

	lda #0
	sta $4003
	sta $4004
	lda #$07
	sta $4005 ;show that screen

	
main_demo_loop
loop
	jsr render_scene
@	lda cloc
	cmp cloc
	beq *-2	
;now copy result back to screen
	lda #$00+7*21
        sta $D650
        lda #$03          ;$000300 = $4300 bank #0
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
    	lda #1
        sta $D653       ; start blitter
	jsr waitblt

;	lda #$80
;	sta $d65d
;	
;	lda 53770
;	and #3
;	clc
;	adc #$20
;	sta $4200+4*21+6
;	lda 53770
;	and #3
;	clc
;	adc #$b8
;	sta $4200+4*21+7
;
;	lda #$00+4*21
;        sta $D650
;        lda #$02          ;$000300 = $4300 bank #0
;        sta $D651       ; blitter addr
;        lda #$00          ;
;        sta $D652
;    	lda #1
;        sta $D653       ; start blitter (draw stars)
;	jsr waitblt


continue2
	jmp loop
oldx1	.byte 0
oldy1	.byte 0
oldx2	.byte 0
oldy2	.byte 0
	
render_scene
	jmp cont
	lda #$12
	sta $4008
	lda #0
	sta $d65d
	sta nmi.backgrnd+1
	sta memwindow
do_vect ldx #0
:2	inc do_vect+1
	
.rept 3
	ldx do_vect+1
	lda verticesx,x
	sta line_x1
	lda verticesy,x
	sta line_y1
	lda verticesx+#*256+256,x
	sta line_x2
	lda verticesy+#*256+256,x
	sta line_y2
	lda #$80
	sta $d65d
	sta memwindow
	jsr draw_line
	lda #0
	sta $d65d
	sta memwindow
.endr	
	ldx do_vect+1
	lda verticesx+256,x
	sta line_x1
	lda verticesy+256,x
	sta line_y1
	lda verticesx+256+256,x
	sta line_x2
	lda verticesy+256+256,x
	sta line_y2
	lda #$80
	sta $d65d
	sta memwindow
	jsr draw_line
	lda #0
	sta $d65d
	sta memwindow

	ldx do_vect+1
	lda verticesx+256,x
	sta line_x1
	lda verticesy+256,x
	sta line_y1
	lda verticesx+256+512,x
	sta line_x2
	lda verticesy+256+512,x
	sta line_y2
	lda #$80
	sta $d65d
	sta memwindow
	jsr draw_line
	lda #0
	sta $d65d
	sta memwindow

	ldx do_vect+1
	lda verticesx+256+256,x
	sta line_x1
	lda verticesy+256+256,x
	sta line_y1
	lda verticesx+256+512,x
	sta line_x2
	lda verticesy+256+512,x
	sta line_y2
	lda #$80
	sta $d65d
	sta memwindow
	jsr draw_line
	lda #0
	sta $d65d
	sta memwindow
cont	
	lda char_flag
	beq @+
	lda #$80
	sta $d65d
	sta memwindow
	jsr draw_char
	dec char_flag
	bne @+
	lda #{jsr $ffff}
	sta cc
	lda #<mocap_endless
	sta cc+1
	lda #>mocap_endless
	sta cc+2
	lda #{nop}
	sta nofire
	sta nofire+1
	sta nofire+2
@	
cc	nop ;
	nop ;jsr mocap_endless
	nop
nofire	jmp random_fire
	
	lda #$80+$1b ;$06c000 --> $4000 window
	sta $d65d
	sta memwindow

	lda #$70
	sta fire1+1
	sta fire3+1
	lda #$7f
	sta fire2+1
	sta fire4+1

	lda frame_num+1
	cmp #>273
	bne @+
	lda Frame_num
	cmp #<273
	bne @+
	lda #$7f
	sta fire1+1
	sta fire3+1
	lda #$30
	sta fire2+1
	sta fire4+1
	lda #{nop}
	sta cc
	sta cc+1
	sta cc+2

	lda #{nop}
	sta render_scene
	sta render_scene+1
	sta render_scene+2	
@
;random at bottom		
;	ldy #0
;@
;	lda 53770
;fire1	and #$00 ;3f
;	sta $6e10,y ;$06ed00
;	lda 53770
;fire2	and #$00 ;40
;	sta $6f10,y ;$6ef00
;fire3	and #$00 ;3f
;	sta $6eb0,y ;$06ed00
;	lda 53770
;fire4	and #$00 ;40
;	sta $6fb0,y ;$6ef00
;	iny
;	cpy #32
;	bne @-
;no_fire_flag	jmp no_fire
random_fire
	lda #$80+$1b ;$06c000 --> $4000 window
	sta $d65d
	sta memwindow
		
	ldy #16
@
	ldx 53770

@	and #0
	ldx 53770
	sta $6f00,x
	sta $6f01,x
	lda 53770
fire1	and #$00
	sta $6f02,x
	sta $6f03,x
	lda 53770
fire2	and #$00
	sta $6e00,x
	sta $6e01,x
	lda 53770
fire3	and #$00
	sta $6e02,x
	sta $6e03,x
	sta $6d00,x
	sta $6d01,x
	lda 53770
fire4	and #$00
	sta $6d02,x
	sta $6d03,x
	jmp @+1
@	lda #$01
	bne @-1
@	dey
	bpl @-3
no_fire
;	
;test line

;	lda #$80
;	sta $d65d
;	lda #16
;	sta ffff+1
;	lda #$60
;	sta ffff+2
;	
;	ldx sy2
;@
;	lda 53770
;ffff	sta $ffff
;	lda ffff+1
;	clc
;	adc #21
;	sta ffff+1
;	bcc @+
;	inc ffff+1
;@
;	inx
;	cpx sy1
;	bne @-1



;	lda #$cf
;	sta x1
;	lda #$82
;	sta y1
;	lda #$ef
;	sta x2
;	lda #$df
;	sta y2
;	jsr draw_line_entry
;xxxxxxxxxx
;          xxxxxxxxxxxx
	lda #$80
	sta $d65d
	sta memwindow
;4 adds
	lda #$ee-maxlines 
	sta $4300+1+21*1 ;A+B+C
	sta $4300+1+21*2
	sta $4300+1+21*3 		
	lda #$ef-maxlines
	sta $4300+1+21*4 ;+D	
;line buffer
;	lda #$bd	;pos
;	sta $4300+7+21*1
;	sta $4300+7+21*2
;	sta $4300+7+21*3
;	sta $4300+7+21*4
;	sta $4300+1+21*5

;	lda #$bd-2 ;which lines to copy on screen
;	sta $4300+1+21*7
;	sta $4300+7+21*7	

;reset divtab dest ad
	lda #$e0
        sta $D650
        lda #$03          ;$000300 = $4300 bank #0
        sta $D651       ; blitter addr
        lda #$00          
        sta $D652
    	lda #1
        sta $D653       ; start blitter 
	jsr waitblt

rrrloop
;do the adds into $060000
;adds 4 bytes, copies result into 256x blitlist
	lda #$00+21
        sta $D650
        lda #$03          ;$000300 = $4300 bank #0
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
    	lda #1
        sta $D653       ; start blitter
	jsr waitblt	
					
;now do the div4-1 via blitter
;and put result on screen
	lda #$40
        sta $D650
        lda #$04         
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
    	lda #1
        sta $D653       ; start blitter
	jsr waitblt
	
;move 1 line up of the dest of the 256 blits
	lda #$c0
        sta $D650
        lda #$03         
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
    	lda #1
        sta $D653       ; start blitter
	jsr waitblt

	
;now go one line up
	inc $4300+1+21*1 ;a
;	dec $4300+7+21*1	
	inc $4300+1+21*2 ;b
;	dec $4300+7+21*2	
	inc $4300+1+21*3 ;c
;	dec $4300+7+21*3	
	inc $4300+1+21*4 ;d
;	dec $4300+7+21*4	

;	dec $4300+1+21*5 ;copy src

;	dec $4300+1+21*7 ;copy src to scr
;	dec $4300+7+21*7	

	lda $4300+1+1*21
	cmp #$f0
	bcs @+
	jmp rrrloop		
@

;	ldx #0
;blur_loop
;	.rept 3
;	lda $7e00+#*256,x
;	clc
;	adc $7e01+#*256,x
;	adc $7e02+#*256,x
;	adc $7f01+#*256,x
;	lsr
;	lsr
;	sec 
;	sbc #1
;	bcs @+
;	lda #0
;@
;	sta $7d00+#*256,x
;	.endr
;	inx
;	bne blur_loop
;
;qqq	lda #0
;	bne @+1
;	lda #1
;	sta qqq+1

	rts

.proc init_div4tab
	lda #$81
	sta $d65d
	sta memwindow
	ldx #0
@
	txa
	lsr
	lsr
	sec
	sbc #1
	bcs @+
	lda #0
@
	sta $7f00,x ;$07f00
	inx
	bne @-1
	rts
.endp
	


;coltab_r .byte 0,0,0,0,0,0,0,32,64
;coltab_g .byte 0,32,144,64,176,96,208,255,255,255
;coltab_b .byte 0,0,0,0,0,0,32,64

waitblt:
        lda $D653       ; wait until not-busy
        bne waitblt
        rts


clear_vram:
cccc	
	lda #$80
	sta $d65d
	sta memwindow      
        lda #0
        sta $D653       ; stop blitter
	lda #$00
        sta $D650
        lda #$02          ;$000200 = $4200 bank #0
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
	lda #$06
    	sta $4208 ;set vram in XDL
	sta $4208+21*1
	sta $4208+21*2
	sta $4208+21*3
    	lda #1
        sta $D653       ; start blitter (draw stars)
	jsr waitblt
	lda #$07
    	sta $4208 ;set vram in XDL
	sta $4208+21*1
	sta $4208+21*2
	sta $4208+21*3
    	lda #1
        sta $D653       ; start blitter (draw stars)
	jsr waitblt

	rts

	
xdl:
        .byte $72 ;repeat+adr+gfx
        .byte $a8 ;stop
        .byte 239     ;RPTL
;screenram
vramadr	.long $070000 ;OVADR 
	.byte <256,>256  ;ystep of display          
        .byte $22,$ff   ;OVATT

cls_bcb: 
		.long $010000 ;source adress ;$0400 texture $005000
		.word 64 ;source step y
		.byte 1 ;source step x
		.long $020000 ;destination adress
		.word 256 ;dest. step y
		.byte 1
		.word 256 ;size x
		.byte 239	;size y
		.byte $00 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte $00       ; pattern
	        .byte 0; control

		.long $001000 ;source adress ;$0400 texture $5000
		.word 64 ;source step y
		.byte 1 ;source step x
		.long $024000 ;destination adress
		.word 256 ;dest. step y
		.byte 1
		.word 256 ;size x
		.byte 63	;size y
		.byte $00 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte $bf       ; pattern
	        .byte 8; control
		.long $001000 ;source adress ;$0400 texture $5000
		.word 64 ;source step y
		.byte 1 ;source step x
		.long $028000 ;destination adress
		.word 256 ;dest. step y
		.byte 1
		.word 256 ;size x
		.byte 63	;size y
		.byte $00 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte $bf       ; pattern
	        .byte 8; control
		.long $001000 ;source adress ;$0400 texture $5000
		.word 64 ;source step y
		.byte 1 ;source step x
		.long $02c000 ;destination adress
		.word 256 ;dest. step y
		.byte 1
		.word 256 ;size x
		.byte 47	;size y
		.byte $00 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte $bf       ; pattern
	        .byte 0; control

		.long $010000 ;source adress ;$0400 texture $5000
		.word 168 ;source step y
		.byte 1 ;source step x
		.long $068060 ;destination adress
		.word 256 ;dest. step y
		.byte 1
		.word 167 ;size x
		.byte 47	;size y
		.byte $03	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte $00       ; pattern
	        .byte 2; control

add_bcb:
;clear buffer
		.long $078000 ;source adress
		.word 256 ;source step y
		.byte 1 ;source step x
		.long $068000 ;destination adress
		.word 256 ;0 ;dest. step y
		.byte 1 ;-1
		.word 0 ;size x
		.byte 0	;size y
		.byte $00 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 0       ; control
;from top to down, random at bottom, written blurred lines-1
;a 
		.long $06ee00-$4000 ;source adress
		.word 0
		.byte 1 ;source step x
		.long $000440 ;$060001 ;destination adress should be low byte of div4_bcb
		.word 0 ;dest. step y
		.byte 21 ;-1
		.word 183 ;size x
		.byte 0	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8       ; control
;b 
		.long $06ee01-$4000 ;+238*256 ;source adress
		.word 0 ;source step y
		.byte 1 ;source step x
		.long $000440 ;$060001 ;destination adress
		.word 0 ;dest. step y
		.byte 21 ;-1
		.word 183 ;size x
		.byte 0	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8+2       ; control
;c 
		.long $06ee02-$4000 ;+238*256 ;source adress
		.word 0 ;source step y
		.byte 1 ;source step x
		.long $000440 ;$060001 ;destination adress
		.word 0 ;dest. step y
		.byte 21 ;-1
		.word 183 ;size x
		.byte 0	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8+2       ; control
;d 
		.long $06ef02-$4000 ;+239*256 ;source adress
		.word 0 ;source step y
		.byte 1 ;source step x
		.long $000440 ;$060001 ;destination adress
		.word 0 ;dest. step y
		.byte 21 ;-1
		.word 255 ;size x
		.byte 0	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 2       ; control

		.long $060001 ;source adress (a+b+c+d)
		.word 0 ;source step y
		.byte 1 ;source step x
		.long $000440 ;destination adress into low-byte of source
		.word 0 ;0 ;dest. step y
		.byte 21
		.word 183 ;size x
		.byte 0	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 0       ; control

		.long $000447 ;source adress (a+b+c+d) one line up
		.word 0 ;source step y
		.byte 21 ;source step x
		.long $000447 ;destination adress into low-byte of source
		.word 0 ;0 ;dest. step y
		.byte 21
		.word 255 ;size x
		.byte 0	;size y
		.byte $00 	;and
	        .byte $ff     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 2       ; control
		
;stuff back to screen
		.long $06ea01 ;source adress (a+b+c+d)
		.word -256 ;source step y
		.byte 1 ;source step x
		.long $07ef00 ;source adress		.word -256 ;source step y
		.word -256 ;0 ;dest. step y
		.byte 1 ;-1
		.word 183 ;size x
		.byte maxlines-1	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $10       ; zoom
		.byte 0       ; pattern
	        .byte 0       ; control

;$03f00-$03fff LUT of div4-1
;for each pixel for 1 line (256x)
div4_bcb
		.long $007f00 ;source adress
		.word 0 ;source step y
		.byte 0 ;source step x
		.long $06ed01;+237*256 ;destination adress
		.word 0 ;0 ;dest. step y
		.byte 0 ;-1
		.word 0 ;size x
		.byte 0	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8       ; control

;move line up
moveup_bcb
		.long $00000 ;source adress
		.word 0 ;source step y
		.byte 0 ;source step x
		.long $00447 ;destination adress
		.word 0 ;dest. step y
		.byte 21 
		.word 184 ;size x
		.byte 0	;size y
		.byte $00 	;and ;$00
	        .byte $01     ; XOR  ;$ff+dest
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 2       ; control

reset_bcb		
		.long $00000 ;source adress
		.word 0 ;source step y
		.byte 0 ;source step x
		.long $00447 ;destination adress
		.word 0 ;dest. step y
		.byte 21 
		.word 184 ;size x
		.byte 0	;size y
		.byte $00 	;and
	        .byte $ed-maxlines     ; XOR 
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 0       ; control

;calc slope
slope_bcb	.long $050000 ;source length edge
		.word $0000 ;source step y = m
 		.byte 0 ;source step x
		.long $002006 ;right edge or left
		.word 21 ;dest. step y
		.byte 0 ;dest step x
		.word 0 ;size x ;only 1 byte copy
		.byte 255	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 0	       ; control

slope2_bcb	.long $050000 ;source length edge
		.word $0000 ;source step y = m
 		.byte 0 ;source step x
		.long $008007 ;right edge or left
		.word 21 ;dest. step y
		.byte 0 ;dest step x
		.word 0 ;size x ;only 1 byte copy
		.byte 255	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8	       ; control

		.long $001e00 ;source length edge
		.word $0001 ;source step y = m
 		.byte 0 ;source step x
		.long $002010 ;right edge or left
		.word 21 ;dest. step y
		.byte 0 ;dest step x
		.word 0 ;size x ;only 1 byte copy
		.byte 255	;size y
		.byte $ff 	;and
	        .byte $00     ; XOR
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 0	       ; control

;set 1 pixel
line_bcb	.long $000000 ;source length edge
		.word $0000 ;source step y = m
 		.byte 0 ;source step x
		.long $068040 ;vram
		.word 0 ;dest. step y
		.byte 1 ;dest step x
		.word 0 ;size x ;only 1 byte copy
		.byte 0	;size y
		.byte $00 	;and
	        .byte $1f     ; XOR linecolor
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8	       ; control
;set 1 pixel
line2_bcb	.long $000000 ;source length edge
		.word $0000 ;source step y = m
 		.byte 0 ;source step x
		.long $060000 ;vram
		.word 0 ;dest. step y
		.byte 0 ;dest step x
		.word 0 ;size x ;only 1 byte copy
		.byte 0	;size y
		.byte $00 	;and
	        .byte $1f     ; XOR linecolor
	        .byte 0       ;  collision AND
 	        .byte $00       ; zoom
		.byte 0       ; pattern
	        .byte 8	       ; control

		
init_gouraud_map
.local
	lda #$dc-8		;bank 1
        sta $d65d       ;cpu-vram access window at $4000, $70000
	sta memwindow
	ldy #0
@	ldx #0
@	tya
	clc
	adc #0
ad	sta gradientmap,x
	inx 
	bne @-
	inc ad+2
	iny
	cpy #$40
	bne @-1
	;jmp init_end
		
	lda #$dd-8		;bank 2
        sta $d65d       ;$008000
	sta memwindow
	ldy #$40
@	ldx #0
@	tya
	clc
	adc #0
ad2	sta gradientmap,x
	inx 
	bne @-
	inc ad2+2
	iny
	cpy #$80
	bne @-1

	lda #$de-8		;bank 3
        sta $d65d       ;cpu-vram access window at $4000
	sta memwindow
	ldy #128
@	ldx #0
@	tya
	clc
	adc #0
ad3	sta gradientmap,x
	inx 
	bne @-
	inc ad3+2
	iny
	cpy #$c0
	bne @-1

	lda #$df-8		;bank 1
        sta $d65d       ;cpu-vram access window at $4000
	sta memwindow
	ldy #$c0
@	ldx #0
@	tya
	clc
	adc #0
ad4	sta gradientmap,x
	inx 
	bne @-
	inc ad4+2
	iny
	bne @-1
.endl
	rts

;-----
;     -----
;          -----
	
Draw_line:
	lda Line_x1
	clc
	adc #24
	sta x1
	lda line_y1
	clc
	adc #16+64 ;+56
	sta y1
	lda line_x2
	clc
	adc #24
	sta x2
	lda line_y2
	clc
	adc #16+64 ;+56
	sta y2
;	jmp software_rendering
	
draw_line_entry
	jsr draw_line_blitter
	lda linetype
	beq @+
	cmp #3
	bcs @+1 ;>=
;this blits dy>dx lines
	lda #$80
	sta $d65d
	sta memwindow
	lda 53770
	sta $40c0+21
;random values
	lda #$c0+21
	sta $d650
	lda #0
	sta $d651
	sta $d652
	lda #1
	sta $d653
	jsr waitblt

	ldx sy1
	lda linebcbstartlo,x
        sta $D650
	lda linebcbstarthi,x
        sta $D651       ; blitter addr
        lda #$00          
        sta $D652
        ldx sy2
        lda linebcbendlo,x
        sta eeee+1
        sta eeeee+1
        lda linebcbendhi,x
        sta eeee+2
        sta eeeee+2
        lda #0
eeee	sta $ffff
    	lda #1
        sta $D653       ; start blitter 
	jsr waitblt
	lda #8
eeeee	sta $ffff	
@	rts		
@
	jmp software_rendering
;	rts

;	lda #$82
;	sta $d65d ;$08000
;	ldx sx1
;	lda linebcbstartlo2,x
;        sta $D650
;	lda linebcbstarthi2,x
;	clc
;	adc #$60
;        sta $D651       ; blitter addr
;        lda #$00          
;        sta $D652
;        ldx sx2
;        lda linebcbendlo2,x
;        sta feeee+1
;        sta feeeee+1
;        lda linebcbendhi2,x
;        sec
;        sbc #$20
;        sta feeee+2
;        sta feeeee+2
;        lda #0
;feeee	sta $ffff
;	lda #$00
;	sta $d65d
;    	lda #1
;        sta $D653       ; start blitter 
;	jsr waitblt
;	lda #8
;feeeee	sta $ffff
;	rts		
;@
;	cmp #4
;	bne @+
;	lda #$82
;	sta $d65d ;$08000
;	ldx sx1
;	lda linebcbstartlo2,x
;        sta $D650
;	lda linebcbstarthi2,x
;	clc
;	adc #$60
;        sta $D651       ; blitter addr
;        lda #$00          
;        sta $D652
;        ldx sx2
;        lda linebcbendlo2,x
;        sta feeeeee+1
;        sta feeeeeee+1
;        lda linebcbendhi2,x
;        sec
;        sbc #$20
;        sta feeeeee+2
;        sta feeeeeee+2
;        lda #0 ;stop byte
;feeeeee	sta $ffff
;	lda #$00
;	sta $d65d
;    	lda #1
;        sta $D653       ; start blitter 
;	jsr waitblt
;	lda #8
;feeeeeee sta $ffff
;@	rts		

software_rendering
	lda Line_x1
	clc
	adc #24
	sta x1
	lda line_y1
	clc
	adc #16 ;+56
	sta y1
	lda line_x2
	clc
	adc #24
	sta x2
	lda line_y2
	clc
	adc #16 ;+56
	sta y2

	lda #$4f ;32k window at $4000-$bfff
	sta $d65e
	sta memwindow2
	lda #$e8 ;window base --> $068000
	sta $d65f
	
 ; u=x2-x1
	SEC 
	LDA X2
	SBC X1
	STA U
 ; v=y2-y1
	SEC 
	LDA Y2
	SBC Y1
	STA V
	BPL Line1
	LDA #-1 ; 
	STA D1y+1
	CLC 
	LDA V
	EOR #$ff
	ADC #1
	STA N+1
	BNE Line2
Line1:
	STA N+1
	LDA #1
	STA D1y+1
Line2:
	LDA U
	BPL Line3
	LDA #-1
	STA D1x+1
	STA D2x+1
	CLC 
	LDA U
	EOR #$ff
	ADC #1
	STA M
	BNE Line4
Line3:	STA M
	LDA #1
	STA D1x+1
	STA D2x+1
Line4:	LDA #0
	STA D2y+1
 ; if not (m>n)
	LDA N+1
	CMP M
	BCC Line5
	LDA #0
	STA D2x+1
	LDA D1y+1
	STA D2y+1
	LDX N+1
	LDA M
	STA N+1
	STX M
Line5:	LDA M
	LSR 
	STA S
	LDA #0
	STA I
Lineloop:
	LDY X1
	LDX Y1
	cpx #$40
	bcs @+
	ldx #$40
@
	cpx #$f0
	bcc @+
	ldx #$f0
@
	STX Putpixel+2
;	stx putpixel2+2
;	STY Putpixel+1
;	sty putpixel2+1
	LDA 53770
	and #$4f
Putpixel: STA $fe00,y
;	LDA 53770
;	and #$3f
;	iny
;Putpixel2: STA $fe00,y

	CLC 
	LDA S
N:	ADC #0
	STA S
 ; if not (s<m)
	CMP M
	BCC Line6
	SEC 
	LDA S
	SBC M
	STA S
	CLC 
	LDA X1
D1x:	ADC #<D1x
	STA X1
	CLC 
	LDA Y1
D1y:	ADC #<D1y
	STA Y1
	BNE Line7
Line6:	CLC 
	LDA X1
D2x:	ADC #<D2x
	STA X1
	CLC 
	LDA Y1
D2y:	ADC #<D2y
	STA Y1
Line7:	INC I
	LDA I
	CMP M
	BCC Lineloop
	lda #0
	sta $d65f
	RTS 



Line_x1	equ	$F0
Line_y1	equ	$F1
Line_x2	equ	$F2
Line_y2	equ	$F3
Frame_num equ	$Fc
Frame_ptr equ	$50
Frame_val equ	$500
Frame_lohi equ	$500+Joint_NUMBER*2
Frame_rpt equ	$500+Joint_NUMBER*4
Joint_x	equ	$600

	.align $100
;	org	$4000




Screen_vblRet
	mva	#{rts}	Screen_vblRet

;	lda	Frame_ptr
;	cmp	#5
;	bcc	Screen_setBr
;	sbc	#Frame_NUMBER	+
;	eor	#$ff
;	cmp	#5
;	bcc	Screen_setBr
;	lda	#3	+
;Screen_setBr
;	adc	#1
;	sta	Screen_br

Screen_clear1
do_frame
 	ldy	#0
Frame_read
	lda Frame_lohi,y
	beq process_lo
	tya
 	tax
	lda Frame_val,y
	dec Frame_rpt,x
	bne add_delta
next_delta
	tya
	asl 
 	tax
	inc Frame_ptr,x
	bne get_delta
	inc Frame_ptr+1,x
get_delta
	lda	(Frame_ptr,x)
	sta Frame_val,y
	bpl process_hi
	pha
	and #127
:4	lsr 
	sta Frame_rpt,y
	lda #1
	sta Frame_lohi,y
	pla
	and #7
	sta Frame_val,y
	bpl add_delta
process_lo
	tya
 	tax
	inc Frame_lohi,x
	lda #1
	sta Frame_rpt,y
	lda Frame_val,y
	and #7
	beq next_delta
	bpl add_delta
process_hi
	beq full_delta
	tya
 	tax
	dec Frame_lohi,x
	lda #1
	sta Frame_rpt,y
	lda Frame_val,y
	and #127
:4	lsr 
	bpl add_delta
full_delta
	lda #1
	sta Frame_lohi,y
	sta Frame_rpt,y
	tya
	asl 
 	tax
	inc Frame_ptr,x
	bne *+4
	inc Frame_ptr+1,x
	lda	(Frame_ptr,x)
	bne no_sub
add_delta
	sec
	sbc #4
no_sub
	add	Joint_x,y
	sta	Joint_x,y
	iny
	cpy	#Joint_NUMBER*2
	beq	do_draw
	jmp Frame_read
do_draw
	jsr	Joint_draw

	lda Frame_num
	add #1
	sta Frame_num
	lda Frame_num+1
	adc #0
	sta Frame_num+1
	
	lda	Frame_num
	cmp	#<Frame_NUMBER
	bne	Frame_next
	lda	Frame_num+1
	cmp	#>Frame_NUMBER
	beq	Frame_ret

Frame_next
	mva	#{lda #}	Screen_vblRet

Frame_ret
	rts

mocap_main
;	ldx	#0
;	ldy	#0
;	lda	#$80
;Screen_column_init1
;	sta	Screen_columnMaskH,x
;	lsr	
;	sta	Screen_columnMaskL,x
;	tya:sta	Screen_columnByte,x
;	lda	Screen_columnMaskL,x
;	lsr	
;	bcc	Screen_column_init2
;	ror		;+
;	iny
;Screen_column_init2
;	inx
;	bpl	Screen_column_init1
;
;	lda	#<Screen_1
;	ldx	#>Screen_1
;	ldy	#0
;	clc
;Screen_line_init
;	sta	Screen_line_lo,y
;	txa
;	sta	Screen_line_hi1,y
;	eor	#(>Screen_1^>Screen_2)
;	sta	Screen_line_hi2,y
;	lda	#Screen_BYTES_PER_LINE
;	adc	Screen_line_lo,y	;-
;	bcc @+
;	inx
;@	iny
;	cpy	#Screen_LINES
;	bcc	Screen_line_init

do_mocap
	mwa	#0 Frame_num
	
	ert	<Frame_data!=0

	ldx	#0
Frame_ptr_init
	lda Frame_data,x
	sta Frame_ptr,x
	inx
	cpx #Joint_NUMBER*4
	bne Frame_ptr_init

	ldx	#0
	ldy	#0
Frame_rpt_init
	lda #1
	sta Frame_lohi,y
	sta Frame_rpt,y
	lda (Frame_ptr,x)
	sta Joint_x,y
	inx
	inx
	iny
	cpy #Joint_NUMBER*2
	bne Frame_rpt_init

;	jsr	Screen_vblRet

;	lda:rne	$d40b
;	sta	$d01b
;	mva	#$21	$d400
;	sta $22F
;	mwa	#Screen_dl	$d402
;	mwa	#Screen_dl	$230
;	mva	#$00	$d017
;;	mva	#$04	^1a
;	sta	$d01a
;	mva #$9b $2c8 ; top background (blue)
;	mva	#$c5 Screen_br ; bottom background (green)
;	mva #$06 $2c4 ; shadow colour (dark grey)
;	mva #$0e $2c5 ; line colour (white)
;	sta $2c6 ; protects from ORing of shadow and line
;	mwa	#Screen_dli	$200
;	mva #$C0	$d40e
	rts
mocap_endless
;	lda $14
;wvb 	cmp $14
;	beq wvb
	jsr do_frame ;Screen_vblRet
	rts ;jmp mocap_endless



; a/|\d
; /b|c\
Line
	jmp draw_line

Line_secondaryColor
	jmp draw_line
Line_primaryColor
	jmp draw_line
Line_setColor
	rts


fastexit rts
.proc draw_line_blitter	
;2 cases dx<dy
; and dy<dx
	lda #$80
	sta $d65d
	sta memwindow
	lda #21
	sta $4089
	lda #0
	sta $408a
	lda #1
	sta linetype

cont
	lda sy2
	cmp sy1 
;	beq fastexit ;can happen!!! straight horizontal line!
	bcs @+ ;swap needed?
	ldx sy1
	lda sy2
	sta sy1
	stx sy2
	ldx sx1
	lda sx2
	sta sx1
	stx sx2
@	
	lda sx2
	sec
	sbc sx1
	sta dx
	bpl @+
	eor #$ff
	clc
	adc #1
	sta dx
@
	lda sy2
	sec
	sbc sy1
	sta dy
	cmp dx ;dy>dx?
	bcs @+ ;>=
;dx>dy
	ldx sy1
	lda sx1
	sta sy1
	stx sx1

	ldx sy2
	lda sx2
	sta sy2
	stx sx2
	
	lda #3
	sta linetype
	rts
;	bne cont
	;jmp draw_line_blitter2
	
@
;from here on DY>DX = blitter line is y++ while x offsets injected by blitter
	lda sx1
	sta $4081 ;start x value of edge = startpos in array (*256 as 8.8 format)
;table entry point is at sy1
	ldx sy1
	lda linebcbstartlo,x
	clc
	adc #6
	sta $4086
	lda linebcbstarthi,x
	adc #0
	sta $4087

	lda sx2
	sec
	sbc sx1
	sta dx
	bpl @+
	jmp signed_version ;>=
@	
	sta T1
	lda #0
	sbc #0
	sta T1+1

	lda sy2
	sec
	sbc sy1 ;delta y in 16bit signed but always >0
	tax
	stx $408e ;edge length = blit size y
	stx dy
;	cpx dx ;dx<=dx?
;	bcc @+ ;need to change stepy
;	rts    ;dy>dx
@
	dex
	lda recitab32768,x
	sta T2
	lda recitab32768+256,x
	sta T2+1
	sec
	jsr multiply_16bit_unsigned ;dx/dy = dx * 1/dy
	;jsr multiply_s16u8
	asl product+1 ;shift up *2
	rol product+2
	
blitprep 
;check if slope>$1000 then slope=$1000
;check if slope>$-1000 then slope=-$1000 = $1fff	
		
;	sta $4284
;$f000 = -4096 ;in blitter 12bit step (13bits!!!)
;$f001 = -4095
;$ffff = -1
;$0001 = +1
;$0fff = +4095
;	lda #$00
;	sta product+1
;	lda #$ea
;	sta product+2
;	lda #$00
;	sta product+3

	lda product+1
	sta $4083 ;stepy
	lda product+2
	sta $4084
	lda product+3
	bpl @+
;minus step $2xxx
	lda product+2
	cmp #$f0
	bcs do_blit
	lda #$f0
	sta $4084
	lda #0
	sta $4083
	bne do_blit
	
@	lda product+2
	cmp #$0f
	bcc do_blit
	lda #$0f
	sta $4084
	lda #$ff
	sta $4083
	
			
;start blit
do_blit
	lda #$80
        sta $D650
        lda #$00          ;$000280 = $4280 bank #0
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
    	lda #1
        sta $D653       ; start blitter (draw stars)
	jsr waitblt

exit	
	rts

signed_version
;	rts
.local
	lda #2
	sta linetype
	lda #-21
	sta $4089
	lda #$ff
	sta $408a

	ldx sy2
	lda linebcbstartlo,x
	clc
	adc #6 ;ypos
	sta $4086
	lda linebcbstarthi,x
	adc #0
	sta $4087

	lda sx2
	sta $4081 ;start x value of edge = startpos in array (*256 as 8.8 format)

	lda sx1 ;delta x in 16bit signed
	sec
	sbc sx2
	sta T1	
	lda #0
	sbc #0
	sta T1+1

	lda sy2
	sec
	sbc sy1 ;delta y in 16bit signed but always >0
	tax
	stx $408e ;edge length = blit size y
	dex
	lda recitab32768,x
	sta T2
	lda recitab32768+256,x
	sta T2+1
	sec
	jsr multiply_16bit_unsigned ;dx/dy = dx * 1/dy
	;jsr multiply_s16u8
	asl product+1 ;shift up *2
	rol product+2
	
blitprep 
;check if slope>$1000 then slope=$1000
;check if slope>$-1000 then slope=-$1000 = $1fff	
		
;	sta $4284
;$f000 = -4096 ;in blitter 12bit step (13bits!!!)
;$f001 = -4095
;$ffff = -1
;$0001 = +1
;$0fff = +4095
;	lda #$00
;	sta product+1
;	lda #$ea
;	sta product+2
;	lda #$00
;	sta product+3
	
	lda product+1
	sta $4083
	lda product+2
	sta $4084
	lda product+3
	bpl @+
;minus step $2xxx
	lda product+2
	cmp #$f0
	bcs do_blit
	lda #$f0
	sta $4084
	lda #0
	sta $4083
	bne do_blit
	
@	lda product+2
	cmp #$0f
	bcc do_blit
	lda #$0f
	sta $4084
	lda #$ff
	sta $4083		
;start blit
do_blit
;	lda #%00011010
;	sta $4284
;	lda #$00
;	sta $4283
	lda #$80
        sta $D650
        lda #$00          ;$000280 = $4280 bank #0
        sta $D651       ; blitter addr
        lda #$00          ;
        sta $D652
    	lda #1
        sta $D653       ; start blitter (draw stars)
	jsr waitblt
exit	;brk
	rts
.endl	
.endp

charexit rts
.proc draw_char
;format is points, lines
;points x x-coord 0-7 bytes
;points x y-coord 0-7 bytes
;lines x id1 0-7 bytes
;lines x id2 0-7 bytes
	lda #$00
	sta $d65d
	sta memwindow
	lda #0
	sta charloop+1
	lda 53770
	and #1
	ora #-12
	sta charoffset
charloop
	ldy #0
	ldx text,y ;"A"
	beq charexit
	inc charloop+1
	lda littabl,x
	sta pointer
	lda littabh,x
	sta pointer+1
	ldy #0
	lda (pointer),y
	sta points
	iny
	lda (pointer),y
	sta lines

	lda pointer
	clc
	adc #2
	sta pointsx+1
	sta pointsx2+1
	lda pointer+1
	adc #0
	sta pointsx+2
	sta pointsx2+2

	lda pointer
	clc
	adc #10
	sta pointsy+1
	sta pointsy2+1
	lda pointer+1
	adc #0
	sta pointsy+2
	sta pointsy2+2

	lda pointer
	clc
	adc #17
	sta verticeid1+1
	lda pointer+1
	adc #0
	sta verticeid1+2

	lda pointer
	clc
	adc #25
	sta verticeid2+1
	lda pointer+1
	adc #0
	sta verticeid2+2
	
@
	ldy lines
verticeid1 ldx lit_a+2+16-1,y
pointsx	lda lit_a+2,x
;	asl
	clc
	adc charoffset
	sta line_x1
	lda #0
	sec
pointsy sbc lit_a+2+8,x
;	asl
	clc
	adc #32+84
	sta line_y1
verticeid2 ldx lit_a+2+24-1,y
pointsx2 lda lit_a+2,x
;	asl
	clc
	adc charoffset
	sta line_x2
	lda #0
	sec
pointsy2 sbc lit_a+2+8,x
;	asl
	clc
	adc #32+84
	sta line_y2
	lda #$80
	sta $d65d
	sta memwindow
	jsr draw_line
	lda #0
	sta $d65d
	sta memwindow
	dec lines
	bne @-
	lda charoffset
	clc
	adc #32
	sta charoffset
	lda charloop+1
	cmp #6
	beq @+
	jmp charloop
@
	rts
text	.byte 4,5,18,9,17,5 ;desire
	;.byte 18,9,12,12,22,0 ;silly
.endp

.proc generate_square_tables
      ; generate f(x)=int(x*x/4)
      ldx #$00
      txa
      .byte $c9
lb1   tya
      adc #$00
ml1   sta square1_hi,x
      tay
      cmp #$40
      txa
      ror 
ml9   adc #$00
      sta ml9+1
      inx
ml0   sta square1_lo,x
      bne lb1
      inc ml0+2
      inc ml1+2
      clc
      iny
      bne lb1
      ; generate f(x)=int((x-255)*(x-255)/4)
      ldx #$00
      ldy #$ff
ml2   lda square1_hi+1,x
      sta square2_hi+$100,x
      lda square1_hi,x
      sta square2_hi,y
      lda square1_lo+1,x
      sta square2_lo+$100,x
      lda square1_lo,x
      sta square2_lo,y
      dey
      inx
      bne ml2
      rts
.endp      

multiply_16x8bit_unsigned
                ; <T1 * <T2 = AAaa
                ; >T1 * <T2 = CCcc
                ;
                ;       AAaa
                ; +   CCcc
                ; ----------
                ;   PRODUCT!

                ; Setup T1 if changed
                bcc m16x8u_setup_done
                    lda T1+0
                    sta sm1a16x8+1
                    sta sm3a16x8+1
                    eor #$ff
                    sta sm2a16x8+1
                    sta sm4a16x8+1
                    lda T1+1
                    sta sm1b16x8+1
                    sta sm3b16x8+1
                    eor #$ff
                    sta sm2b16x8+1
                    sta sm4b16x8+1
m16x8u_setup_done

                ; Perform <T1 * <T2 = AAaa
                ldx T2+0
                sec
sm1a16x8        lda square1_lo,x
sm2a16x8        sbc square2_lo,x
                sta PRODUCT+0
sm3a16x8        lda square1_hi,x
sm4a16x8        sbc square2_hi,x
                sta _AA16x8+1

                ; Perform >T1_hi * <T2 = CCcc
                sec
sm1b16x8        lda square1_lo,x
sm2b16x8        sbc square2_lo,x
                sta _cc16x8+1
sm3b16x8        lda square1_hi,x
sm4b16x8        sbc square2_hi,x
                sta PRODUCT+2

                ; Add the separate multiplications together
                clc
_AA16x8         lda #0
_cc16x8         adc #0
                sta PRODUCT+1
                scc:inc PRODUCT+2

                rts
multiply_s16u8

                jsr multiply_16x8bit_unsigned

                ; Apply sign (See C=Hacking16 for details).
                lda T1+1
                bpl ms16u8_signfix1_done
                    sec
                    lda PRODUCT+2
                    sbc T2+0
                    sta PRODUCT+2
ms16u8_signfix1_done

                rts

multiply_u16s8
		lda t2
		bmi mulneg
		sec
		jsr multiply_16x8bit_unsigned
		rts			
mulneg          
		lda #0 ;neg t2
		sec
		sbc t2
		sta t2
		sec
		jsr multiply_16x8bit_unsigned
                ; Apply sign (See C=Hacking16 for details).
		lda #0
		sec
		sbc product+1
		sta product+1
		lda #0
		sbc product+2
		sta product+2            	    
mu16s8_signfix1_done

                rts


; Description: Unsigned 16-bit multiplication with unsigned 32-bit result.
;
; Input: 16-bit unsigned value in T1
;        16-bit unsigned value in T2
;        Carry=0: Re-use T1 from previous multiplication (faster)
;        Carry=1: Set T1 (slower)
;
; Output: 32-bit unsigned value in PRODUCT
;
; Clobbered: PRODUCT, X, A, C
;
; Allocation setup: T1,T2 and PRODUCT preferably on Zero-page.
;                   square1_lo, square1_hi, square2_lo, square2_hi must be
;                   page aligned. Each table are 512 bytes. Total 2kb.
;
; Table generation: I:0..511
;                   square1_lo = <((I*I)/4)
;                   square1_hi = >((I*I)/4)
;                   square2_lo = <(((I-255)*(I-255))/4)
;                   square2_hi = >(((I-255)*(I-255))/4)

.proc multiply_16bit_unsigned
                ; <T1 * <T2 = AAaa
                ; <T1 * >T2 = BBbb
                ; >T1 * <T2 = CCcc
                ; >T1 * >T2 = DDdd
                ;
                ;       AAaa
                ;     BBbb
                ;     CCcc
                ; + DDdd
                ; ----------
                ;   PRODUCT!

                ; Setup T1 if changed
                bcc m16u_setup_done
                    lda T1+0
                    sta sm1a+1
                    sta sm3a+1
                    sta sm5a+1
                    sta sm7a+1
                    eor #$ff
                    sta sm2a+1
                    sta sm4a+1
                    sta sm6a+1
                    sta sm8a+1
                    lda T1+1
                    sta sm1b+1
                    sta sm3b+1
                    sta sm5b+1
                    sta sm7b+1
                    eor #$ff
                    sta sm2b+1
                    sta sm4b+1
                    sta sm6b+1
                    sta sm8b+1
m16u_setup_done

                ; Perform <T1 * <T2 = AAaa
                ldx T2+0
                sec
sm1a            lda square1_lo,x
sm2a            sbc square2_lo,x
                sta PRODUCT+0
sm3a            lda square1_hi,x
sm4a            sbc square2_hi,x
                sta _AA+1

                ; Perform >T1_hi * <T2 = CCcc
                sec
sm1b            lda square1_lo,x
sm2b            sbc square2_lo,x
                sta _cc+1
sm3b            lda square1_hi,x
sm4b            sbc square2_hi,x
                sta _CCC+1

                ; Perform <T1 * >T2 = BBbb
                ldx T2+1
                sec
sm5a            lda square1_lo,x
sm6a            sbc square2_lo,x
                sta _bb+1
sm7a            lda square1_hi,x
sm8a            sbc square2_hi,x
                sta _BBB+1

                ; Perform >T1 * >T2 = DDdd
                sec
sm5b            lda square1_lo,x
sm6b            sbc square2_lo,x
                sta _dd+1
sm7b            lda square1_hi,x
sm8b            sbc square2_hi,x
                sta PRODUCT+3

                ; Add the separate multiplications together
                clc
_AA             lda #0
_bb             adc #0
                sta PRODUCT+1
_BBB            lda #0
_CCC            adc #0
                sta PRODUCT+2
                bcc m16u_carry1_done
                    inc PRODUCT+3
                    clc
m16u_carry1_done
_cc             lda #0
                adc PRODUCT+1
                sta PRODUCT+1
_dd             lda #0
                adc PRODUCT+2
                sta PRODUCT+2
                bcc m16u_carry2_done
                    inc PRODUCT+3
m16u_carry2_done
                rts

size_u16 equ *-multiply_16bit_unsigned
.endp
      
      
; Description: Signed 16-bit multiplication with signed 32-bit result.
;
; Input: 16-bit signed value in T1
;        16-bit signed value in T2
;        Carry=0: Re-use T1 from previous multiplication (faster)
;        Carry=1: Set T1 (slower)
;
; Output: 32-bit signed value in PRODUCT
;
; Clobbered: PRODUCT, X, A, C
.proc multiply_16bit_signed
		stx xreg_save
		sty yreg_save

                jsr multiply_16bit_unsigned

                ; Apply sign (See C=Hacking16 for details).
                lda T1+1
                bpl m16s_signfix1_done
                    sec
                    lda PRODUCT+2
                    sbc T2+0
                    sta PRODUCT+2
                    lda PRODUCT+3
                    sbc T2+1
                    sta PRODUCT+3
m16s_signfix1_done
                lda T2+1
                bpl m16s_signfix2_done
                    sec
                    lda PRODUCT+2
                    sbc T1+0
                    sta PRODUCT+2
                    lda PRODUCT+3
                    sbc T1+1
                    sta PRODUCT+3
m16s_signfix2_done
		ldx xreg_save
		ldy yreg_save

                rts
size_s16 equ *-multiply_16bit_unsigned
.endp


	;icl	"dance.asm"
	;icl "liege.asx"
	;icl "anim2.asm"
	icl "affe.asm"

	.align $100
recitab32768 ins "recitab32768.dat"

	.align $100
;	org $e800
        icl 'mpt_player.asm'

	.align $100 ;$player+$500
msx	;equ $4000

;	mpt_relocator 'shortie3.mpt' , msx
	mpt_relocator 'subm3.mpt' , msx
	;mpt_relocator 'lastninja.mpt', msx
	org msx
	.sav [6] ?length

	.align $100
verticesx
	ins "firevecx.dat"
verticesy	
	ins "firevecy.dat"



; litery
littabl    dta l(0,lit_a,lit_b,lit_c,lit_d,lit_e,lit_f,lit_g,lit_h,lit_i,lit_j,lit_k)
           dta l(lit_l,lit_m,lit_n,lit_o,lit_p,lit_r,lit_s,lit_t,lit_u,lit_w,lit_y,lit_z)

littabh    dta h(0,lit_a,lit_b,lit_c,lit_d,lit_e,lit_f,lit_g,lit_h,lit_i,lit_j,lit_k)
           dta h(lit_l,lit_m,lit_n,lit_o,lit_p,lit_r,lit_s,lit_t,lit_u,lit_w,lit_y,lit_z)

;vertices-1, lines
lit_a     
           dta 5,3
           dta -12,0,12,-6,6,0,0,0 ;x
           dta -12,12,-12,0,0,0,0,0 ;y
           dta 0,1,3,0,0,0,0,0 ;id1
           dta 1,2,4,0,0,0,0,0 ;id2

lit_b     
           dta 8,8
           dta -12,0,12,0,12,0,-12,-12
           dta 12,12,6,0,-6,-12,-12,0
           dta 0,1,2,3,4,5,6,3
           dta 1,2,3,4,5,6,0,7

lit_c       
           dta 6,5
           dta 12,0,-12,-12,0,12,0,0
           dta 6,12,6,-6,-12,-6,0,0
           dta 0,1,2,3,4,0,0,0
           dta 1,2,3,4,5,0,0,0

lit_d       
           dta 6,6
           dta -12,0,12,12,0,-12,0,0
           dta 12,12,6,-6,-12,-12,0,0
           dta 0,1,2,3,4,5,0,0
           dta 1,2,3,4,5,0,0,0

lit_e       
           dta 6,4
           dta 12,-12,-12,12,-12,0,0,0
           dta 12,12,-12,-12,0,0,0,0
           dta 0,1,2,4,0,0,0,0
           dta 1,2,3,5,0,0,0,0

lit_f       
           dta 5,3
           dta 12,-12,-12,-12,0,0,0,0
           dta 12,12,-12,0,0,0,0,0
           dta 0,1,3,0,0,0,0,0
           dta 1,2,4,0,0,0,0,0

lit_g       
           dta 8,7
           dta 12,0,-12,-12,0,12,12,0
           dta 6,12,6,-6,-12,-6,0,0
           dta 0,1,2,3,4,5,6,0
           dta 1,2,3,4,5,6,7,0

lit_h       
           dta 6,3
           dta -12,-12,-12,12,12,12,0,0
           dta 12,-12,0,0,12,-12,0,0
           dta 0,2,4,0,0,0,0,0
           dta 1,3,5,0,0,0,0,0

lit_i       
           dta 6,3
           dta -6,6,0,0,-6,6,0,0
           dta 12,12,12,-12,-12,-12,0,0
           dta 0,2,4,0,0,0,0,0
           dta 1,3,5,0,0,0,0,0

lit_j       
           dta 5,4
           dta -12,12,12,0,-12,0,0,0
           dta 12,12,-6,-12,-6,0,0,0
           dta 0,1,2,3,0,0,0,0
           dta 1,2,3,4,0,0,0,0

lit_k       
           dta 5,3
           dta -12,-12,12,-12,12,0,0,0
           dta 12,-12,12,0,-12,0,0,0
           dta 0,2,3,0,0,0,0,0
           dta 1,3,4,0,0,0,0,0

lit_l       
           dta 3,2
           dta -12,-12,12,0,0,0,0,0
           dta 12,-12,-12,0,0,0,0,0
           dta 0,1,0,0,0,0,0,0
           dta 1,2,0,0,0,0,0,0

lit_m       
           dta 5,4
           dta -12,-12,0,12,12,0,0,0
           dta -12,12,0,12,-12,0,0,0
           dta 0,1,2,3,0,0,0,0
           dta 1,2,3,4,0,0,0,0

lit_n       
           dta 4,3
           dta -12,-12,12,12,0,0,0,0
           dta -12,12,-12,12,0,0,0,0
           dta 0,1,2,0,0,0,0,0
           dta 1,2,3,0,0,0,0,0

lit_o       
           dta 6,6
           dta 0,12,12,0,-12,-12,0,0
           dta 12,6,-6,-12,-6,6,0,0
           dta 0,1,2,3,4,5,0,0
           dta 1,2,3,4,5,0,0,0

lit_p       
           dta 6,5
           dta -12,-12,0,12,0,-12,0,0
           dta -12,12,12,6,0,0,0,0
           dta 0,1,2,3,4,0,0,0
           dta 1,2,3,4,5,0,0,0

lit_r       
           dta 7,6
           dta -12,-12,0,12,0,-12,12,0
           dta -12,12,12,6,0,0,-12,0
           dta 0,1,2,3,4,4,0,0
           dta 1,2,3,4,5,6,0,0

lit_s       
           dta 6,5
           dta 12,0,-12,12,0,-12,0,0
           dta 6,12,6,-6,-12,-6,0,0
           dta 0,1,2,3,4,0,0,0
           dta 1,2,3,4,5,0,0,0

lit_t       
           dta 4,2
           dta -12,12,0,0,0,0,0,0
           dta 12,12,12,-12,0,0,0,0
           dta 0,2,0,0,0,0,0,0
           dta 1,3,0,0,0,0,0,0

lit_u       
           dta 5,4
           dta -12,-12,0,12,12,0,0,0
           dta 12,-6,-12,-6,12,0,0,0
           dta 0,1,2,3,0,0,0,0
           dta 1,2,3,4,0,0,0,0

lit_w       
           dta 5,4
           dta -12,-6,0,6,12,0,0,0
           dta 12,-12,0,-12,12,0,0,0
           dta 0,1,2,3,0,0,0,0
           dta 1,2,3,4,0,0,0,0

lit_y       
           dta 4,3
           dta -12,0,12,0,0,0,0,0
           dta 12,0,12,-12,0,0,0,0
           dta 0,1,1,0,0,0,0,0
           dta 1,2,3,0,0,0,0,0

lit_z       
           dta 4,3
           dta -12,12,-12,12,0,0,0,0
           dta 12,12,-12,-12,0,0,0,0
           dta 0,1,2,0,0,0,0,0
           dta 1,2,3,0,0,0,0,0

	run init
	
	opt l-
	icl 'mpt_relocator.mac'