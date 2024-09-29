
	.macro m_info
	.print ":1: " , :1, " - ", :1 + .len :1 -1, " (", .len :1,")"
	.endm

	nmien = $d40e
	portb = $d301
	
	icl "ars3_voxel1_sv22.asm"
