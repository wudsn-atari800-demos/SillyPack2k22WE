//------------------------------------------------------------------------
//	Just S P R I T E S - by SuN 2022 grzegorzsun@gmail.com
//------------------------------------------------------------------------

Timer equ 552
VTimr equ 538
sdmctl equ 559; dma controll

cursor_y	equ $54		; Row of cursor, 1 byte
cursor_x	equ $55		; Column of cursor, 2 bytes

prev_y  equ $5A
prev_x  equ $5B      ; 2 bytes

VVBLKD  equ $0224
XITVBV  equ $e462
WSYNC   equ $D40A
VCOUNT  equ $D40B
RANDOM  equ $D20A

SPLAYER0 equ $D008 ; size player0
GPLAYER0 equ $D00D ; rejestr grafiki gracza 0    
PPLAYER0 equ $D000 ; pozycja gracza 0
CPLAYER0 equ $D012 ; kolor gracza 0

colpf0	=	$D016	; rejestr koloru pola gry 0 (Z)
colpf1	=	$D017	; rejestr koloru pola gry 1 (Z)
colpf2	=	$D018	; rejestr koloru pola gry 2 (Z)
colpf3	=	$D019	; rejestr koloru pola gry 3 (Z)
colpf4	=	$D01A
colbak	=	$D01A	; rejestr koloru t?a (Z)

COLOR2   equ $02c6;
RTCLOCK  equ $0012;

* ---------------------------------------------------------------------------------------------
* ---	POKEY
* ---------------------------------------------------------------------------------------------

irqens	=	$0010	; rejestr-cie? IRQEN
irqstat	=	$0011	; rejestr-cie? IRQST

audf1	=	$d200	; cz?stotliwo?? pracy generatora 1 (Z)
audc1	=	$d201	; rejestr kontroli d?wi?ku generatora 1 (Z)
audf2	=	$d202	; cz?stotliwo?? pracy generatora 2 (Z)
audc2	=	$d203	; rejestr kontroli d?wi?ku generatora 2 (Z)
audf3	=	$d204	; cz?stotliwo?? pracy generatora 3 (Z)
audc3	=	$d205	; rejestr kontroli d?wi?ku generatora 3 (Z)
audf4	=	$d206	; cz?stotliwo?? pracy generatora 4 (Z)
audc4	=	$d207	; rejestr kontroli d?wi?ku generatora 4 (Z)

audctl	=	$D208	; rejestr kontroli generator?w d?wi?ku (Z)
stimer	=	$D209	; rejestr zerowania licznik?w (Z)
kbcode	=	$D209	; kod ostatnio naci?ni?tego klawisza (O)
skstres	=	$D20A	; rejestr statusu z??cza szeregowego (Z)
serout	=	$D20D	; szeregowy rejestr wyj?ciowy (Z)
serin	=	$D20D	; szeregowy rejestr wej?ciowy (O)
irqen	=	$D20E	; zezwolenie przerwa? IRQ (Z)
irqst	=	$D20E	; status przerwa? IRQ (O)
skctl	=	$D20F	; rejestr kontroli z??cza szeregowego (Z)
skstat	=	$D20F	; rejestr statusu z??cza szeregowego (O)

; OS functions
openmode  equ $ef9c
drawpoint equ $f1d8
drawto    equ $f9c2
putline   equ $c642
print     equ $f1a4

xx equ $80

   org	$81
start
;	dma off
    lsr sdmctl;
;    mva vbrout VVBLKD
restart
    ldy #0;
    sty pplayer0;
    ldx #1;
    stx xx;
    stx rtclock+2
; shape of players
    dey
    sty GPLAYER0;
    sty GPLAYER0+1;	

    stx GPLAYER0+2;
    stx GPLAYER0+3;
    
loopb1 	
    lda VCOUNT;
	sta WSYNC;
	adc rtclock+2;
	sta CPLAYER0;
    nop;
    sta SPLAYER0;
    asl
	sta splayer0+1;
	sta CPLAYER0+1;
;	lda random
pyk 
    and #$e0;
;    and random
    nop
	sta pplayer0+1;

    lda rtclock+2;
    bne loopb1	
    dex
    bne ef2
    
    inc rtclock+2;
    ;and random
    lda #$2D; and
    sta pyk    
    lda #$0A
    sta pyk+1
    lda #$D2
    sta pyk+2
    bne loopb1    

//ef2
ef2
    inc rtclock+2;
    
loopbe1	
    ldy #$01
loopbe2
    lda vcount
; cos dziwnego
	adc rtclock+2
; tez fajny z eor #$ff
    eor #$1f
    sta opcodb1+1
    lda vcount;
opcodb1	
    and #$00
    adc #$26
	eor tab,y
	sta pplayer0+2,y
	lda vcount
;	ora #$07
	sta cplayer0+2,y
	dey
	bpl loopbe2
	lda rtclock+2
	bne loopbe1

//ef3 kasza
    inc rtclock+2;

loopbf1	ldy #$01
loopbf2	equ *
    lda rtclock+2;
 	sta opcodbf1+1

    lda random
    sta GPLAYER0,y;
    tax
opcodbf1	
    eor #$00
    adc #$26
    eor tab,y
    sta pplayer0,y
    AND #$02
	STA SPLAYER0,Y
    txa
	sta cplayer0,y
	dey
	bpl loopbf2
	lda rtclock+2
	bne loopbf1

//ef4
    inc rtclock+2

loopbg1	ldy #$01
loopbg2	equ *
    lda vcount
	asl @
    adc rtclock+2

; tez fajny z eor #$ff
eors equ * +1
;	eor #$1f
;	sta opcodbg1+1

; takie coœ
    lda vcount;
opcodbg1	
;    and #$00
    and random
	adc #$26
	eor tab,y
	sta pplayer0,y
;	lda vcount
	ora #$07
	sta cplayer0,y
	dey
	bpl loopbg2
	lda rtclock+2
	bne loopbg1

;    rol eors
;    lda eors
;    adc #$26
;    sta eors
    
    inc rtclock+2
    
// restart first eff
    lda #$29; and
    sta pyk    
    lda #$E0
    sta pyk+1
    lda #$EA
    sta pyk+2
    
    jmp restart
    
; VBLANK interrupt routine
vbrout	
    lda rtclock+2
	bne skipa
    rol opc
    ror opaud
skipa
opc equ *+1
    eor #%00001111
	sta audc1
opaud equ *+1
    eor #%10101010
;    eor #$af
	sta audf1
	jmp xitvbv

tab equ *
    dta $00,$ff
	
    org vvblkd
	.word .adr(vbrout)
    
    run start