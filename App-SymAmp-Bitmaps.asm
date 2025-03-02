;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                  BITMAPS                                   @
;@             (c) 2022-2022 by Prodatron / SymbiosiS (J�rn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


xid_frame_l     equ 1
xid_frame_r     equ 2
xid_kbpskhz     equ 3
xid_sldpos      equ 4
xid_sldvol      equ 5
xid_open        equ 6
xid_mixer       equ 7
xid_control     equ 8
xid_stereo      equ 9
xid_titmix      equ 10
xid_sldpan      equ 11
xid_sldmix      equ 12



;general stuff
gfx_edge_ul  db 2,4,3:dw gfx_edge_ul0, gfx_edge,2*15
gfx_edge_ur  db 2,4,3:dw gfx_edge_ur0, gfx_edge,2*15
gfx_edge_dl  db 2,4,3:dw gfx_edge_dl0, gfx_edge,2*15
gfx_edge_dr  db 2,4,3:dw gfx_edge_dr0, gfx_edge,2*15
gfx_edge_drw db 2,4,3:dw gfx_edge_drw0,gfx_edge,2*15
gfx_edge db 5
gfx_edge_ul0  db #11,#16, #16,#66, #16,#66
gfx_edge_ur0  db #61,#11, #66,#61, #66,#61
gfx_edge_dl0  db #16,#66, #16,#66, #11,#16
gfx_edge_dr0  db #66,#61, #66,#61, #61,#11
gfx_edge_drw0 db #88,#81, #88,#81, #81,#11

;sliders (horizontal blue, white, vertical)
gfx_sld_horb db 6,12,6:dw $+7:dw $+4,6*6:db 5:db #61,#11,#11,#11,#11,#16, #11,#88,#88,#88,#88,#11, #18,#88,#88,#88,#88,#81, #18,#86,#86,#86,#86,#81, #11,#88,#88,#88,#88,#11, #61,#11,#11,#11,#11,#16
gfx_sld_horw db 6,12,6:dw $+7:dw $+4,6*6:db 5:db #81,#11,#11,#11,#11,#18, #11,#88,#88,#88,#88,#11, #18,#88,#88,#88,#88,#81, #18,#86,#86,#86,#86,#81, #11,#88,#88,#88,#88,#11, #81,#11,#11,#11,#11,#18
gfx_sld_vert db 4,8,12:dw $+7:dw $+4,4*12:db 5:db #66,#11,#11,#66, #61,#18,#81,#16, #61,#88,#88,#16, #61,#88,#68,#16, #61,#88,#88,#16, #61,#88,#68,#16, #61,#88,#88,#16, #61,#88,#68,#16, #61,#88,#88,#16, #61,#88,#68,#16, #61,#18,#81,#16, #66,#11,#11,#66


;==============================================================================
;### MAIN WINDOW ##############################################################
;==============================================================================

;control buttons inactive
gfx_ctr_ply0 db 4,8,6:dw $+7:dw $+4,4*6:db 5:db #11,#88,#88,#88, #11,#11,#88,#88, #11,#11,#11,#88, #11,#11,#11,#88, #11,#11,#88,#88, #11,#88,#88,#88
gfx_ctr_pau0 db 4,8,6:dw $+7:dw $+4,4*6:db 5:db #81,#18,#11,#88, #81,#18,#11,#88, #81,#18,#11,#88, #81,#18,#11,#88, #81,#18,#11,#88, #81,#18,#11,#88
gfx_ctr_stp0 db 4,8,6:dw $+7:dw $+4,4*6:db 5:db #81,#11,#11,#88, #81,#11,#11,#88, #81,#11,#11,#88, #81,#11,#11,#88, #81,#11,#11,#88, #81,#11,#11,#88

;control buttons active
gfx_ctr_ply1 db 4,8,6:dw $+7:dw $+4,4*6:db 5:db #FF,#88,#88,#88, #FF,#FF,#88,#88, #FF,#FF,#FF,#88, #FF,#FF,#FF,#88, #FF,#FF,#88,#88, #FF,#88,#88,#88
gfx_ctr_pau1 db 4,8,6:dw $+7:dw $+4,4*6:db 5:db #8F,#F8,#FF,#88, #8F,#F8,#FF,#88, #8F,#F8,#FF,#88, #8F,#F8,#FF,#88, #8F,#F8,#FF,#88, #8F,#F8,#FF,#88
gfx_ctr_stp1 db 4,8,6:dw $+7:dw $+4,4*6:db 5:db #8F,#FF,#FF,#88, #8F,#FF,#FF,#88, #8F,#FF,#FF,#88, #8F,#FF,#FF,#88, #8F,#FF,#FF,#88, #8F,#FF,#FF,#88

;status button
gfx_sta_play db 4,8,6:dw $+7:dw $+4,8*6:db 5:db #18,#81,#11,#11, #18,#88,#81,#11, #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#81,#11, #18,#81,#11,#11
gfx_sta_paus db 4,8,6:dw $+7:dw $+4,8*6:db 5:db #18,#81,#88,#11, #18,#81,#88,#11, #18,#81,#88,#11, #18,#81,#88,#11, #18,#81,#88,#11, #18,#81,#88,#11
gfx_sta_stop db 4,8,6:dw $+7:dw $+4,8*6:db 5:db #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81

;spectrum analyzer with "SymbiosiS" logo
gfx_analyz0 db 10,40,12
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0
db #F1,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FC
db #F2,#F0,#F0,#F0,#F2,#F0,#F0,#F4,#F0,#F0
db #F1,#FC,#FA,#F5,#F3,#F2,#F4,#F9,#F3,#FC
db #F0,#F2,#F4,#FA,#FA,#FA,#FA,#F5,#F4,#F0
db #F3,#FC,#F4,#F8,#FB,#F2,#F4,#F9,#F3,#F8
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F4
db #F3,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F8
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0
db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0

fnt_time
db 8,47
db 3, %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000, 0,0,0,0,0,0,0    ;" "
db 5, %01100000,%10010000,%10010000,%00000000,%10010000,%10010000,%10010000,%01100000, 0,0,0,0,0,0,0    ;0
db 5, %00010000,%00010000,%00010000,%00000000,%00010000,%00010000,%00010000,%00010000, 0,0,0,0,0,0,0    ;1
db 5, %11100000,%00010000,%00010000,%01100000,%10000000,%10000000,%10000000,%01110000, 0,0,0,0,0,0,0    ;2
db 5, %11100000,%00010000,%00010000,%11100000,%00010000,%00010000,%00010000,%11100000, 0,0,0,0,0,0,0    ;3
db 5, %10010000,%10010000,%10010000,%01100000,%00010000,%00010000,%00010000,%00010000, 0,0,0,0,0,0,0    ;4
db 5, %01110000,%10000000,%10000000,%01100000,%00010000,%00010000,%00010000,%11100000, 0,0,0,0,0,0,0    ;5
db 5, %01110000,%10000000,%10000000,%01100000,%10010000,%10010000,%10010000,%01100000, 0,0,0,0,0,0,0    ;6
db 5, %11100000,%00010000,%00010000,%00000000,%00010000,%00010000,%00010000,%00010000, 0,0,0,0,0,0,0    ;7
db 5, %01100000,%10010000,%10010000,%01100000,%10010000,%10010000,%10010000,%01100000, 0,0,0,0,0,0,0    ;8
db 5, %01100000,%10010000,%10010000,%01100000,%00010000,%00010000,%00010000,%01100000, 0,0,0,0,0,0,0    ;9
db 3, %00000000,%00000000,%10000000,%00000000,%00000000,%10000000,%00000000,%00000000, 0,0,0,0,0,0,0    ;:

fnt_micro
db 4,47
db 4, %00000000,%00000000,%00000000,%00000000,0,0,0,0,0,0,0,0,0,0,0 ;" "
db 4, %01000000,%10100000,%10100000,%01000000,0,0,0,0,0,0,0,0,0,0,0 ;0
db 4, %00100000,%00100000,%00100000,%00100000,0,0,0,0,0,0,0,0,0,0,0 ;1
db 4, %11000000,%00100000,%01000000,%11100000,0,0,0,0,0,0,0,0,0,0,0 ;2
db 4, %11000000,%01100000,%01100000,%11000000,0,0,0,0,0,0,0,0,0,0,0 ;3
db 4, %10000000,%10100000,%11100000,%00100000,0,0,0,0,0,0,0,0,0,0,0 ;4
db 4, %11100000,%11000000,%00100000,%11000000,0,0,0,0,0,0,0,0,0,0,0 ;5
db 4, %01100000,%11000000,%10100000,%01000000,0,0,0,0,0,0,0,0,0,0,0 ;6
db 4, %11100000,%00100000,%01000000,%01000000,0,0,0,0,0,0,0,0,0,0,0 ;7
db 4, %11100000,%11100000,%10100000,%01000000,0,0,0,0,0,0,0,0,0,0,0 ;8
db 4, %01000000,%10100000,%01100000,%11000000,0,0,0,0,0,0,0,0,0,0,0 ;9
db 4, %00000000,%00000000,%11100000,%00000000,0,0,0,0,0,0,0,0,0,0,0 ;-
