

	icl "..\..\asm\Fixes.asm"
	
	opt h+
	org $2000
	.byte 'Fixed version by JAC! - 2024-09-02',13,0

	opt h-
	ins "HAPPYFOX-Original.xex",+$2,$1D80

	opt h+
	org $2000
	m_fade_screen_out
	
	opt h-
	ins "HAPPYFOX-Original.xex",+$1d82
