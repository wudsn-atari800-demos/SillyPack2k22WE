;
;	>>> SillyMenu by JAC! <<<
;
;	@com.wudsn.ide.lng.mainsourcefile=..\asm\SillyMenu.asm
;
;	Lengths specifies the number of line with same color.
;	Length 0 terminates the sequence.
;	Chromas specified the PAL color values with luma zero.

;              LO, LO, BO, GF, AT, PO, G2, GA, 25, 16, DE, VB, WI, TI
lengths	.byte  10,  7 , 7, 15, 14, 16, 12, 12, 19, 13, 18, 10, 23,  3,0
chromas	.byte $30,$10,$e0,$d0,$b0,$90,$70,$50,$40,$20,$f0,$c0,$a0,$00
;chroma .byte $30,$10,$e0,$d0,$b0,$90,$70,$50,$40,$20,$f0,$c0,$a0,$00

