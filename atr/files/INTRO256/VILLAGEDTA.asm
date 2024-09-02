; Created by Chaos Game Editor v0.1 by Agenda, 2022-11-18 15:37:47

.local	chaosdta

; number of colors
colnum	equ $03

; colors
color1	equ $16
color2	equ $14
color3	equ $1a

colors	.byte color1, color2, color3

; density multiplier
densmul	equ $64

; density and color
denscol	.byte $0d, $21, $05, $22, $0b, $09, $09, $09
	.byte $13, $0f, $0b, $21, $12, $11, $1b, $14
	.byte $18

; number of anchors
anchnum	.byte $04, $04, $04, $05, $04, $04, $04, $04
	.byte $04, $02, $02, $04, $04, $05, $04, $04
	.byte $04
	.byte $00

areas

area20	.byte $9f, $8a, $9f, $be, $00, $be, $00, $87

area0	.byte $0a, $9a, $0a, $36, $1b, $2d, $1b, $9d

area24	.byte $2b, $0d, $2c, $09, $38, $32, $35, $34

area1	.byte $1c, $9e, $1c, $2d, $2a, $0e, $34, $30, $34, $98

area2	.byte $08, $34, $1c, $2a, $2c, $09, $17, $16

area8	.byte $18, $00, $23, $00, $2a, $18, $25, $1f

area9	.byte $38, $20, $34, $16, $3d, $00, $4a, $00

area10	.byte $47, $67, $41, $77, $30, $42, $35, $39

area11	.byte $21, $3c, $26, $48, $0e, $82, $04, $6d

area3	.byte $44, $00, $0c, $73

area4	.byte $42, $69, $1e, $00

area12	.byte $4c, $74, $4c, $a6, $7b, $b0, $7b, $73

area13	.byte $7c, $72, $9a, $74, $9a, $9e, $7c, $af

area15	.byte $71, $33, $7d, $70, $9e, $73, $8d, $51, $7c, $3a

area14	.byte $49, $73, $7c, $70, $70, $33, $5e, $49

area18	.byte $56, $82, $56, $93, $5e, $94, $5e, $82

area19	.byte $68, $aa, $68, $82, $71, $83, $71, $ab

.endl