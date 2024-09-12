;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@                                 MOD PLUGIN                                 @
;@         by Maarten Loor/NOP (Z80), Peter Hanning (original 68000),         @
;@                   Prodatron/SymbiosiS (SymAmp adaption)                    @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;- bpm convert to ticks? (works sometimes, sometimes not)
;- optimization (replace maal, sla/rl -> add hl,hl, srl/rr -> other direction)


OPL4_REG    equ #ff7e
OPL4_DATA   equ #ff7f

OPL4_FM_C6  equ #ffc6
OPL4_FM_C7  equ #ffc7

MACRO   opl4_wt
        ld a,#ff
        in a,(#c4)
        and %11
        jr nz,$-6
MEND

op4_64kbnk  db 0    ;number of 64k banks


;==============================================================================
;### API-ROUTINES #############################################################
;==============================================================================

;### MODLOD ->  Loads MOD-Sound
;### Input      (sndfil)=found path and file
;### Output     CF=0 -> ok, CF=1 -> Error, A=error type (1=disc, 2=format, 3=memory), C=disc error code
;### Destroyed  AF,BC,DE,HL,IX,IY
modlod  call mt_load
        ret c
        ld hl,modheader
        ld a,(hl)
        call clcucs
        ld (hl),a
        ld de,sndtxt_titl
        ld bc,20
        ldir
        xor a
        ld (de),a
        ret

;### MODINI ->  Initializes MOD-Sound
;### Input      HL=address
;### Output     CF=0 -> ok, HL=number of song positions, A=number of channels
;###	        CF=1 -> not supported format
;### Destroyed  AF,BC,DE,HL,IX,IY
modini	di
    	call mt_start
    	ei
        ld hl,(song_length)
        xor a
        ld h,a
        ld a,4
    	ret

;### MODVOL -> Set global volume
;### Input      A=Volume (0-255)
;### Destroyed  ?
modvol  rlca:rlca:rlca
        and 7
        cpl
        add 7+1
        ld (vol),a
        jp SetVol

;### MODVAL -> Get volume and frequency for a specific channel
;### Input      A=Channel (0-3)
;### Output     A=Volume (0-15), L=Frequency (0-15)
;### Destroyed  F,DE,H
modval  add a:add a:add a:add a
        ld e,a
        ld d,0
        ld hl,pos_temp_ch0+n_period
        add hl,de
        ld e,(hl)
        inc hl
        ld a,(hl)
        inc hl
        ex de,hl
        and 1
        ld h,a          ;0-511
        add hl,hl
        add hl,hl
        add hl,hl       ;0-4095
        ld l,h
        ld a,(de)       ;0-64
        srl a
        srl a
        cp 16
        ret c
        ld a,15
	ret

;### MODPLY -> Plays MOD-Sound (called every 50Hz interrupt)
;### Output     HL=Position, CF=1 -> Song ended
;### Destroyed  AF,BC,DE,HL,IX,IY
modply  di
    	call mt_music
        ei
        ld hl,(song_pos)
        xor a
        ld h,a
        ld a,(mt_stopflag)
        rra
        ret

;### MODSTP -> Turn MOD-Sound off
;### Destroyed  AF,BC,DE,HL,IX,IY
modstp	di
    	call mt_stop0
    	ei
    	ret

;### MODPOS -> Sets MOD-SOund on a specific location
;### Input      HL=Position, Sound must be initialized
;### Destroyed  AF,BC,DE,HL,IX,IY
modpos  ld a,l
        ld (song_pos),a
        ld a,128
        ld (song_step),a
        ret


;==============================================================================
;### MOD-FUNCTIONS (INTERN) ###################################################
;==============================================================================

;N.O.P. Mod player adapted by Maarten Loor 2020 from the Amiga 68000 version by Peter Hanning

;todo
;- use less memory
;- the notenumber is always searched (iy+n_notenum), als when the sample does not have a ft
;- cannot start other samples with period (when it is set to loop)

;### data

temp_size 	equ	16

n_sampnum 	equ	0	;B    SampleNumber   
n_notenum 	equ	1	;B    NoteNumber   
n_period 	equ	2	; W   Period   
n_volume 	equ	4	;B    Volume   
n_oldsamp 	equ	5	;B    OldSampleNumber   
n_cmdtemp 	equ	6	; W   CommandTemp   
n_oldpbendsp 	equ	8	;B    OldPitchBendSpeed
n_oldvibcmd 	equ	9	;B    OldVibratoCommand
n_vibtabpos 	equ	10	;B    VibratoTablePosition
n_oldtremcmd 	equ	11	;B    OldTremoloCommand
n_tremtabpos 	equ	12	;B    TremoloTablePosition
n_wavectrl 	equ	13	;B    WaveControl %0000TTVV
n_pattloop 	equ	14	; W   PatternLoop(E6)temp

pos_temp_ch0    ds	temp_size,0
pos_temp_ch1    ds	temp_size,0
pos_temp_ch2    ds	temp_size,0
pos_temp_ch3    ds	temp_size,0

numpatterns	db	0	        ;number of patterns loaded      /loading/
sampnum	        db	0	        ;temp -> sampnum  
address	        db	#20,#01,#47	;temp -> address in chipmem  
samplen	        dw	0	        ;temp -> length of sample  
samprpp	        dw	0	        ;temp -> sample repeat point  
samprpl	        dw	0	        ;temp -> sample repeat length  

finetunes	ds	31,0	        ;finetunes for samples [0-30]   /playing/
volumes	        ds	31,0	        ;volumes of samples [0-30]  
patterns	ds	128	        ;pattern data [0-127]  
pansetting	db	5,11,11,5
song_length	db	0	        ;number of patterns (songlength)  
song_pos	db	0	        ;current position [cur,copy]  
song_pospat	db	0
song_step	db	0	        ;current step [cur,copy]  
channel	        db	255	        ;current channel (255 if not playing)
vblanks	        db	6,6	        ;vblank timing (cur, org)  
tempo	        dw	0,249,125	;cia timing    (cur, org) (*80uS)  
arpeggio	db	0	        ;for playing c64 sounds  
command_E5temp	db	0	        ;temp. for finetune  
command_EEtemp	db	0	        ;temp. voor patt.delay  
mt_stopflag	db	0

filenr	        db	0

modheader	ds      1084

repeat_song	db	0
paused	        db	0
pan	        db	5
vol	        db	0

mod_init
	db	2,%00010000
	db	#50,254,#51,254,#52,254,#53,254
	db	0

mod_load	db	#68,0,#69,0,#6a,0,#6b,0,0

wave_hdr
	db	0,0,0	;start         (3b) 
	db	0,0	;loop          (2b) 
	db	0,0	;end           (2b) 
	db	0	;lfo,vib       (1b) 
	db	%11111111	;ar, d1r       (1b) 
	db	0	;dl, d2r       (1b) 
	db	0	;rate comp, rr (1b) 
	db	0	;am            (1b) 

periods
	dw	0	;for identification if no note specified 
; Tuning 0, Normal
	dw	856,808,762,720,678,640,604,570,538,508,480,453
	dw	428,404,381,360,339,320,302,285,269,254,240,226
	dw	214,202,190,180,170,160,151,143,135,127,120,113
ft_tab
; Tuning 1
	db	-6 ,-6 ,-5 ,-5 ,-4 ,-3 ,-3 ,-3 ,-3 ,-3 ,-3 ,-3
	db	-3 ,-3 ,-2 ,-3 ,-2 ,-2 ,-2 ,-1 ,-1 ,-1 ,-1 ,-1
	db	-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,0
; Tuning 2
	db	-12,-12,-10,-11,-8 ,-8 ,-7 ,-7 ,-6 ,-6 ,-6 ,-6
	db	-6 ,-6 ,-5 ,-5 ,-4 ,-4 ,-4 ,-3 ,-3 ,-3 ,-3 ,-2
	db	-3 ,-3 ,-2 ,-3 ,-3 ,-2 ,-2 ,-2 ,-2 ,-2 ,-2 ,-1
; Tuning 3
	db	-18,-17,-16,-16,-13,-12,-12,-11,-10,-10,-10,-9
	db	-9 ,-9 ,-8 ,-8 ,-7 ,-6 ,-6 ,-5 ,-5 ,-5 ,-5 ,-4
	db	-5 ,-4 ,-3 ,-4 ,-4 ,-3 ,-3 ,-3 ,-3 ,-2 ,-2 ,-2
; Tuning 4
	db	-24,-23,-21,-21,-18,-17,-16,-15,-14,-13,-13,-12
	db	-12,-12,-11,-10,-9 ,-8 ,-8 ,-7 ,-7 ,-7 ,-7 ,-6
	db	-6 ,-6 ,-5 ,-5 ,-5 ,-4 ,-4 ,-4 ,-4 ,-3 ,-3 ,-3
; Tuning 5
	db	-30,-29,-26,-26,-23,-21,-20,-19,-18,-17,-17,-16
	db	-15,-14,-13,-13,-11,-11,-10,-9 ,-9 ,-9 ,-8 ,-7
	db	-8 ,-7 ,-6 ,-6 ,-6 ,-5 ,-5 ,-5 ,-5 ,-4 ,-4 ,-4
; Tuning 6
	db	-36,-34,-32,-31,-27,-26,-24,-23,-22,-21,-20,-19
	db	-18,-17,-16,-15,-14,-13,-12,-11,-11,-10,-10,-9
	db	-9 ,-9 ,-7 ,-8 ,-7 ,-6 ,-6 ,-6 ,-6 ,-5 ,-5 ,-4
; Tuning 7
	db	-42,-40,-37,-36,-32,-30,-29,-27,-25,-24,-23,-22
	db	-21,-20,-18,-18,-16,-15,-14,-13,-13,-12,-12,-10
	db	-10,-10,-9 ,-9 ,-9 ,-8 ,-7 ,-7 ,-7 ,-6 ,-6 ,-5
; Tuning -8
	db	51,48,46,42,42,38,36,34,32,30,28,27
	db	25,24,23,21,21,19,18,17,16,15,14,14
	db	12,12,12,10,10,10,9 ,8 ,8 ,8 ,7 ,7
; Tuning -7
	db	44,42,40,37,37,35,32,31,29,27,25,24
	db	22,21,20,19,18,17,16,15,15,14,13,12
	db	11,10,10,9 ,9 ,9 ,8 ,7 ,7 ,7 ,6 ,6
; Tuning -6
	db	38,36,34,32,31,30,28,27,25,24,22,21
	db	19,18,17,16,16,15,14,13,13,12,11,11
	db	9 ,9 ,9 ,8 ,7 ,7 ,7 ,6 ,6 ,6 ,5 ,5
; Tuning -5
	db	31,30,29,26,26,25,24,22,21,20,18,17
	db	16,15,14,13,13,12,12,11,11,10,9 ,9
	db	8 ,7 ,8 ,7 ,6 ,6 ,6 ,5 ,5 ,5 ,5 ,5
; Tuning -4
	db	25,24,23,21,21,20,19,18,17,16,14,14
	db	13,12,11,10,11,10,10,9 ,9 ,8 ,7 ,7
	db	6 ,6 ,6 ,5 ,5 ,5 ,5 ,4 ,4 ,4 ,3 ,4
; Tuning -3
	db	19,18,17,16,16,15,15,14,13,12,11,10
	db	9 ,9 ,9 ,9 ,8 ,8 ,7 ,7 ,7 ,6 ,5 ,6
	db	5 ,4 ,5 ,4 ,4 ,4 ,4 ,3 ,3 ,3 ,3 ,3
; Tuning -2
	db	12,12,12,10,11,11,10,10,9 ,8 ,7 ,7
	db	6 ,6 ,6 ,5 ,6 ,5 ,5 ,5 ,5 ,4 ,4 ,4
	db	3 ,3 ,3 ,3 ,2 ,3 ,3 ,2 ,2 ,2 ,2 ,2
; Tuning -1
	db	6 ,6 ,6 ,5 ,6 ,6 ,6 ,5 ,5 ,5 ,4 ,4
	db	3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,3 ,2 ,2 ,2
	db	2 ,1 ,2 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1

pitch_tab
	;per 2 bytes, eerste 2 bytes voor period 907   
	;laatste voor period 108 (dus ook voor alle fine-tunes!)   

	DB	#06,#04,#EA,#03,#CE,#03,#B2,#03,#98,#03
	DB	#7E,#03,#64,#03,#4A,#03,#32,#03,#18,#03
	DB	#00,#03,#E8,#02,#D2,#02,#BA,#02,#A4,#02
	DB	#8E,#02,#78,#02,#62,#02,#4E,#02,#38,#02
	DB	#24,#02,#10,#02,#FC,#01,#E8,#01,#D6,#01
	DB	#C2,#01,#B0,#01,#9E,#01,#8C,#01,#7A,#01
	DB	#68,#01,#56,#01,#46,#01,#34,#01,#24,#01
	DB	#14,#01,#04,#01,#F4,#00,#E4,#00,#D4,#00
	DB	#C6,#00,#B6,#00,#A8,#00,#98,#00,#8A,#00
	DB	#7C,#00,#6E,#00,#60,#00,#52,#00,#44,#00
	DB	#38,#00,#2A,#00,#1C,#00,#10,#00,#04,#00
	DB	#EE,#F7,#D6,#F7,#BC,#F7,#A4,#F7,#8C,#F7
	DB	#74,#F7,#5E,#F7,#46,#F7,#30,#F7,#18,#F7
	DB	#02,#F7,#EC,#F6,#D6,#F6,#C2,#F6,#AC,#F6
	DB	#96,#F6,#82,#F6,#6E,#F6,#58,#F6,#44,#F6
	DB	#30,#F6,#1C,#F6,#0A,#F6,#F6,#F5,#E2,#F5
	DB	#D0,#F5,#BE,#F5,#AA,#F5,#98,#F5,#86,#F5
	DB	#74,#F5,#62,#F5,#50,#F5,#40,#F5,#2E,#F5
	DB	#1E,#F5,#0C,#F5,#FC,#F4,#EC,#F4,#DA,#F4
	DB	#CA,#F4,#BA,#F4,#AA,#F4,#9A,#F4,#8C,#F4
	DB	#7C,#F4,#6C,#F4,#5E,#F4,#4E,#F4,#40,#F4
	DB	#30,#F4,#22,#F4,#14,#F4,#06,#F4,#F8,#F3
	DB	#EA,#F3,#DC,#F3,#CE,#F3,#C0,#F3,#B2,#F3
	DB	#A4,#F3,#98,#F3,#8A,#F3,#7E,#F3,#70,#F3
	DB	#64,#F3,#56,#F3,#4A,#F3,#3E,#F3,#32,#F3
	DB	#24,#F3,#18,#F3,#0C,#F3,#00,#F3,#F4,#F2
	DB	#E8,#F2,#DE,#F2,#D2,#F2,#C6,#F2,#BA,#F2
	DB	#B0,#F2,#A4,#F2,#9A,#F2,#8E,#F2,#84,#F2
	DB	#78,#F2,#6E,#F2,#62,#F2,#58,#F2,#4E,#F2
	DB	#44,#F2,#38,#F2,#2E,#F2,#24,#F2,#1A,#F2
	DB	#10,#F2,#06,#F2,#FC,#F1,#F2,#F1,#E8,#F1
	DB	#E0,#F1,#D6,#F1,#CC,#F1,#C2,#F1,#BA,#F1
	DB	#B0,#F1,#A6,#F1,#9E,#F1,#94,#F1,#8C,#F1
	DB	#82,#F1,#7A,#F1,#70,#F1,#68,#F1,#60,#F1
	DB	#56,#F1,#4E,#F1,#46,#F1,#3E,#F1,#34,#F1
	DB	#2C,#F1,#24,#F1,#1C,#F1,#14,#F1,#0C,#F1
	DB	#04,#F1,#FC,#F0,#F4,#F0,#EC,#F0,#E4,#F0
	DB	#DC,#F0,#D4,#F0,#CE,#F0,#C6,#F0,#BE,#F0
	DB	#B6,#F0,#AE,#F0,#A8,#F0,#A0,#F0,#98,#F0
	DB	#92,#F0,#8A,#F0,#84,#F0,#7C,#F0,#74,#F0
	DB	#6E,#F0,#66,#F0,#60,#F0,#5A,#F0,#52,#F0
	DB	#4C,#F0,#44,#F0,#3E,#F0,#38,#F0,#30,#F0
	DB	#2A,#F0,#24,#F0,#1C,#F0,#16,#F0,#10,#F0
	DB	#0A,#F0,#04,#F0,#FA,#E7,#EE,#E7,#E2,#E7
	DB	#D6,#E7,#CA,#E7,#BC,#E7,#B0,#E7,#A4,#E7
	DB	#98,#E7,#8C,#E7,#80,#E7,#74,#E7,#6A,#E7
	DB	#5E,#E7,#52,#E7,#46,#E7,#3A,#E7,#30,#E7
	DB	#24,#E7,#18,#E7,#0E,#E7,#02,#E7,#F8,#E6
	DB	#EC,#E6,#E2,#E6,#D6,#E6,#CC,#E6,#C2,#E6
	DB	#B6,#E6,#AC,#E6,#A2,#E6,#96,#E6,#8c,#e6
	DB	#82,#e6,#78,#e6,#6e,#e6,#62,#e6,#58,#e6
	DB	#4e,#e6,#44,#E6,#3A,#E6,#30,#E6,#26,#E6
	DB	#1C,#E6,#12,#E6,#0A,#E6,#00,#E6,#F6,#E5
	DB	#EC,#E5,#E2,#E5,#DA,#E5,#D0,#E5,#C6,#E5
	DB	#BE,#E5,#B4,#E5,#AA,#E5,#A2,#E5,#98,#E5
	DB	#90,#E5,#86,#E5,#7E,#E5,#74,#E5,#6C,#E5
	DB	#62,#E5,#5A,#E5,#50,#E5,#48,#E5,#40,#E5
	DB	#36,#E5,#2E,#E5,#26,#E5,#1E,#E5,#14,#E5
	DB	#0C,#E5,#04,#E5,#FC,#E4,#F4,#E4,#EC,#E4
	DB	#E2,#E4,#DA,#E4,#D2,#E4,#CA,#E4,#C2,#E4
	DB	#BA,#E4,#B2,#E4,#AA,#E4,#A2,#E4,#9A,#E4
	DB	#94,#E4,#8C,#E4,#84,#E4,#7C,#E4,#74,#E4
	DB	#6C,#E4,#64,#E4,#5E,#E4,#56,#E4,#4E,#E4
	DB	#46,#E4,#40,#E4,#38,#E4,#30,#E4,#2A,#E4
	DB	#22,#E4,#1A,#E4,#14,#E4,#0C,#E4,#06,#E4
	DB	#FE,#E3,#F8,#E3,#F0,#E3,#EA,#E3,#E2,#E3
	DB	#DC,#E3,#D4,#E3,#CE,#E3,#C6,#E3,#C0,#E3
	DB	#B8,#E3,#B2,#E3,#AC,#E3,#A4,#E3,#9E,#E3
	DB	#98,#E3,#90,#E3,#8A,#E3,#84,#E3,#7E,#E3
	DB	#76,#E3,#70,#E3,#6A,#E3,#64,#E3,#5E,#E3
	DB	#56,#E3,#50,#E3,#4A,#E3,#44,#E3,#3E,#E3
	DB	#38,#E3,#32,#E3,#2A,#E3,#24,#E3,#1E,#E3
	DB	#18,#E3,#12,#E3,#0C,#E3,#06,#E3,#00,#E3
	DB	#FA,#E2,#F4,#E2,#EE,#E2,#E8,#E2,#E2,#E2
	DB	#DE,#E2,#D8,#E2,#D2,#E2,#CC,#E2,#C6,#E2
	DB	#C0,#E2,#BA,#E2,#B6,#E2,#B0,#E2,#AA,#E2
	DB	#A4,#E2,#9E,#E2,#9A,#E2,#94,#E2,#8E,#E2
	DB	#88,#E2,#84,#E2,#7E,#E2,#78,#E2,#72,#E2
	DB	#6E,#E2,#68,#E2,#62,#E2,#5E,#E2,#58,#E2
	DB	#52,#E2,#4E,#E2,#48,#E2,#44,#E2,#3E,#E2
	DB	#38,#E2,#34,#E2,#2E,#E2,#2A,#E2,#24,#E2
	DB	#20,#E2,#1A,#E2,#16,#E2,#10,#E2,#0C,#E2
	DB	#06,#E2,#02,#E2,#FC,#E1,#F8,#E1,#F2,#E1
	DB	#EE,#E1,#E8,#E1,#E4,#E1,#E0,#E1,#DA,#E1
	DB	#D6,#E1,#D0,#E1,#CC,#E1,#C8,#E1,#C2,#E1
	DB	#BE,#E1,#BA,#E1,#B4,#E1,#B0,#E1,#AC,#E1
	DB	#A6,#E1,#A2,#E1,#9E,#E1,#9A,#E1,#94,#E1
	DB	#90,#E1,#8C,#E1,#88,#E1,#82,#E1,#7E,#E1
	DB	#7A,#E1,#76,#E1,#70,#E1,#6C,#E1,#68,#E1
	DB	#64,#E1,#60,#E1,#5C,#E1,#56,#E1,#52,#E1
	DB	#4E,#E1,#4A,#E1,#46,#E1,#42,#E1,#3E,#E1
	DB	#3A,#E1,#34,#E1,#30,#E1,#2C,#E1,#28,#E1
	DB	#24,#E1,#20,#E1,#1C,#E1,#18,#E1,#14,#E1
	DB	#10,#E1,#0C,#E1,#08,#E1,#04,#E1,#00,#E1
	DB	#FC,#E0,#F8,#E0,#F4,#E0,#F0,#E0,#EC,#E0
	DB	#E8,#E0,#E4,#E0,#E0,#E0,#DC,#E0,#D8,#E0
	DB	#D4,#E0,#D0,#E0,#CE,#E0,#CA,#E0,#C6,#E0
	DB	#C2,#E0,#BE,#E0,#BA,#E0,#B6,#E0,#B2,#E0
	DB	#AE,#E0,#AC,#E0,#A8,#E0,#A4,#E0,#A0,#E0
	DB	#9C,#E0,#98,#E0,#96,#E0,#92,#E0,#8E,#E0
	DB	#8A,#E0,#86,#E0,#84,#E0,#80,#E0,#7C,#E0
	DB	#78,#E0,#74,#E0,#72,#E0,#6E,#E0,#6A,#E0
	DB	#66,#E0,#64,#E0,#60,#E0,#5C,#E0,#5A,#E0
	DB	#56,#E0,#52,#E0,#4E,#E0,#4C,#E0,#48,#E0
	DB	#44,#E0,#42,#E0,#3E,#E0,#3A,#E0,#38,#E0
	DB	#34,#E0,#30,#E0,#2E,#E0,#2A,#E0,#26,#E0
	DB	#24,#E0,#20,#E0,#1C,#E0,#1A,#E0,#16,#E0
	DB	#14,#E0,#10,#E0,#0C,#E0,#0A,#E0,#06,#E0
	DB	#04,#E0,#00,#E0,#FA,#D7,#F4,#D7,#EE,#D7
	DB	#E8,#D7,#E2,#D7,#DC,#D7,#D6,#D7,#D0,#D7
	DB	#CA,#D7,#C4,#D7,#BC,#D7,#B6,#D7,#B0,#D7
	DB	#AA,#D7,#A4,#D7,#9E,#D7,#98,#D7,#92,#D7
	DB	#8C,#D7,#86,#D7,#80,#D7,#7A,#D7,#74,#D7
	DB	#70,#D7,#6A,#D7,#64,#D7,#5E,#D7,#58,#D7
	DB	#52,#D7,#4C,#D7,#46,#D7,#40,#D7,#3A,#D7
	DB	#36,#D7,#30,#D7,#2A,#D7,#24,#D7,#1E,#D7
	DB	#18,#D7,#14,#D7,#0E,#D7,#08,#D7,#02,#D7
	DB	#FE,#D6,#F8,#D6,#F2,#D6,#EC,#D6,#E6,#D6
	DB	#E2,#D6,#DC,#D6,#D6,#D6,#D2,#D6,#CC,#D6
	DB	#C6,#D6,#C2,#D6,#BC,#D6,#B6,#D6,#B2,#D6
	DB	#AC,#D6,#A6,#D6,#A2,#D6,#9C,#D6,#96,#D6
	DB	#92,#D6,#8C,#D6,#86,#D6,#82,#D6,#7C,#D6
	DB	#78,#D6,#72,#D6,#6E,#D6,#68,#D6,#62,#D6
	DB	#5E,#D6,#58,#D6,#54,#D6,#4E,#D6,#4A,#D6
	DB	#44,#D6,#40,#D6,#3A,#D6,#36,#D6,#30,#D6
	DB	#2C,#D6,#26,#D6,#22,#D6,#1C,#D6,#18,#D6
	DB	#12,#D6,#0E,#D6,#0A,#D6,#04,#D6,#00,#D6
	DB	#FA,#D5,#F6,#D5,#F2,#D5,#EC,#D5,#E8,#D5
	DB	#E2,#D5,#DE,#D5,#DA,#D5,#D4,#D5,#D0,#D5
	DB	#CC,#D5,#C6,#D5,#C2,#D5,#BE,#D5,#B8,#D5
	DB	#B4,#D5,#B0,#D5,#AA,#D5,#A6,#D5,#A2,#D5
	DB	#9C,#D5,#98,#D5,#94,#D5,#90,#D5,#8A,#D5
	DB	#86,#D5,#82,#D5,#7E,#D5,#78,#D5,#74,#D5
	DB	#70,#D5,#6C,#D5,#66,#D5,#62,#D5,#5E,#D5
	DB	#5A,#D5,#56,#D5,#50,#D5,#4C,#D5,#48,#D5
	DB	#44,#D5,#40,#D5,#3C,#D5,#36,#D5,#32,#D5
	DB	#2E,#D5,#2A,#D5,#26,#D5,#22,#D5,#1E,#D5
	DB	#1A,#D5,#14,#D5,#10,#D5,#0C,#D5,#08,#D5
	DB	#04,#D5,#00,#D5,#FC,#D4,#F8,#D4,#F4,#D4
	DB	#F0,#D4,#EC,#D4,#E8,#D4,#E2,#D4,#DE,#D4
	DB	#DA,#D4,#D6,#D4,#D2,#D4,#CE,#D4,#CA,#D4
	DB	#C6,#D4,#C2,#D4,#BE,#D4,#BA,#D4,#B6,#D4
	DB	#B2,#D4,#AE,#D4,#AA,#D4,#A6,#D4,#A2,#D4
	DB	#9E,#D4,#9A,#D4,#96,#D4,#94,#D4,#90,#D4
	DB	#8C,#D4,#88,#D4,#84,#D4,#80,#D4,#7C,#D4
	DB	#78,#D4,#74,#D4,#70,#D4,#6C,#D4,#68,#D4
	DB	#64,#D4,#62,#D4,#5E,#D4,#5A,#D4,#56,#D4
	DB	#52,#D4,#4E,#D4,#4A,#D4,#46,#D4,#44,#D4
	DB	#40,#D4,#3C,#D4,#38,#D4,#34,#D4,#30,#D4
	DB	#2E,#D4,#2A,#D4,#26,#D4,#22,#D4,#1E,#D4
	DB	#1A,#D4,#18,#D4,#14,#D4,#10,#D4,#0C,#D4
	DB	#08,#D4,#06,#D4,#02,#D4,#FE,#D3,#FA,#D3
	DB	#F8,#D3,#F4,#D3,#F0,#D3,#EC,#D3,#EA,#D3
	DB	#E6,#D3,#E2,#D3,#DE,#D3,#DC,#D3,#D8,#D3
	DB	#D4,#D3,#D0,#D3,#CE,#D3,#CA,#D3,#C6,#D3
	DB	#C4,#D3,#C0,#D3,#BC,#D3,#B8,#D3,#B6,#D3
	DB	#B2,#D3,#AE,#D3,#AC,#D3,#A8,#D3,#A4,#D3
	DB	#A2,#D3,#9E,#D3,#9A,#D3,#98,#D3,#94,#D3
	DB	#90,#D3,#8E,#D3,#8A,#D3,#86,#D3,#84,#D3
	DB	#80,#D3,#7E,#D3,#7A,#D3,#76,#D3,#74,#D3

vol_tab
	DB	#7F,#5C,#52,#4A,#44,#3F,#3B,#37,#34,#31
	DB	#2F,#2D,#2A,#28,#27,#25,#23,#22,#20,#1F
	DB	#1E,#1C,#1B,#1A,#19,#18,#17,#16,#15,#14
	DB	#13,#12,#12,#11,#10,#0F,#0F,#0E,#0D,#0C
	DB	#0C,#0B,#0B,#0A,#09,#09,#08,#08,#07,#06
	DB	#06,#05,#05,#04,#04,#03,#03,#03,#02,#02
	DB	#01,#01,#00,#00,#00

vib_tab
	db	000,024,049,074,097,120,141,161
	db	180,197,212,224,235,244,250,253
	db	255,253,250,244,235,224,212,197
	db	180,161,141,120,097,074,049,024

;### subroutines

MAAL	LD	A,16    ;hl=de*bc
MAAL1	SLA	L
	RL	H
	SLA	E
	RL	D
	JP	NC,MAAL2
	ADD	HL,BC
MAAL2	DEC	A
	JR	NZ,MAAL1
	RET

deel
	ld	bc,-1
deel1
	inc	bc
	scf
	ccf
	sbc	hl,de
	jp	nc,deel1
	add	hl,de
	ret

rst20           ;cp hl,de (hl<de -> cf=1, hl=de -> zf=1)
	ld	a,h
	sub	d
	ret	nz
	ld	a,l
	sub	e
	ret

ini_wav
;set expansion register to OPL4 (enables WAV)
        ld bc,OPL4_FM_C6
	;opl4_wt
	ld	a,5
	out	(c),a
	inc c
	;opl4_wt
	ld	a,3
	out	(c),a
	ret

set_opl4        ;(hl)=reg, (hl+1)=data, hl=hl+2, until (hl)=0
        ld bc,OPL4_REG
set_opl41
	opl4_wt
	ld	a,(hl)
	or	a
	ret	z
	out	(c),a
	inc	hl
	inc c
	opl4_wt
	ld	a,(hl)
	out	(c),a
	dec c
	inc	hl
	jp	set_opl41

set_adr         ;a[21-16],h[15-8],l[7-0]=adr -> send 24bit adr to opl4
        ld bc,OPL4_REG
	push	af
	opl4_wt
	ld	a,3
	out	(c),a
	inc c
	opl4_wt
	pop	af
	out	(c),a
	dec c
	opl4_wt
	ld	a,4
	out	(c),a
	inc c
	opl4_wt
	out	(c),h
	dec c
	opl4_wt
	ld	a,5
	out	(c),a
	inc c
	opl4_wt
	out	(c),l
	dec c
	ret

;error handler -> close file, if open, restore stack, return from mt_load
put_error
    ld c,a
    ld b,1
put_error0      ;b=main error (1=disc, 2=format, 3=memory)
    push bc
    ld a,(filenr)
    cp -1
    call nz,SyFile_FILCLO
    pop bc
    ld a,b
put_error1
    ld sp,0
    scf
exit
	ret

SetVol
	ld bc,OPL4_REG
	opl4_wt
	ld	a,#f9
	out	(c),a
	ld	a,(vol)
	rlca
	rlca
	rlca
	ld	e,a
	ld	a,(vol)
	or	e
	inc c
	ld e,a
	opl4_wt
	out	(c),e
	ret

telop
	inc	de
	inc	de
	ld	c,0	;nextbyteextra =0 
	ld	a,(de)	;get destin d7-d0 
	add	a,l	;add source d7-d0 
	ld	(de),a	;put destin d7-d0 
	call	c,telop1	;if>255 then inc nextbyteextra 
	dec	de

	ld	a,(de)	;get destin d15-d8 
	add	c	;add lastbyteextra 
	ld	c,0	;nextbyteextra=0 
	call	c,telop1	;if>255 then inc nextbyteextra 
	add	h	;add source d15-d8 
	call	c,telop1
	ld	(de),a
	dec	de

	ld	a,(de)
	add	c
	ld	c,0
	call	c,telop1
	add	b
	call	c,telop1
	ld	(de),a
	ret

telop1
	inc	c
	ret

telaf
	ld	a,b
	cpl
	ld	b,a
	ld	a,h
	cpl
	ld	h,a
	ld	a,l
	cpl
	ld	l,a
	jp	telop


	;loadsamp 
	;------- 
	;hl=length of sample
	;set destination chipmem address before 
q_samploadbuffersize	equ sndmax-2048
loadsamp
	ld	a,h
	or	l
	ret	z	;return when sample size=0

	push	hl	;sample < #2001?
	ld	de,q_samploadbuffersize+1
	call	rst20
	jp	c,loadsamp1
	ld	hl,q_samploadbuffersize	;load a block of #2000
loadsamp1
	push	hl

	ld	a,(filenr)
	ld	b,h	;number of bytes in BC
	ld	c,l
	ld	hl,sndmem
	ld  de,2048
	add hl,de
	ld	de,(App_BnkNum)   ;File laden
	call SyFile_FILINP
	ret c               ;##!!## clean up stack first!!

	;tranfer the sample data to chipmem
	ld	d,b
	ld	e,c
	ld	hl,sndmem
	ld  bc,2048
	add hl,bc
	ld bc,OPL4_REG

	di
	opl4_wt
	ld	a,6
	out	(c),a
	inc c
move_samp
    opl4_wt
	ld	a,(hl)
	out	(c),a
	inc	hl
	dec	de
	ld	a,d
	or	e
	jp	nz,move_samp
	ei

	pop	de
	pop	hl
	or	a
	sbc	hl,de
	jp	loadsamp

;### main prg

;id strings for 31 instruments at 1080
modids31i
db "M.K."
db "M!K!"
db "FLT4"

mt_load_id
        ld hl,modheader+1080        
        ld b,4
        push de
        call strcmp
        pop de
        ld hl,4
        add hl,de
        ex de,hl
        ret

mt_load
        ld a,-1
        ld (filenr),a
        ld (put_error1+1),sp
	ld	a,(channel)
	cp	255
	call	nz,mt_stop

	call	ini_wav
	ld	hl,mod_load		;damp the 4 first OPL4 channels
	call	set_opl4
	call	SetVol

        ld bc,OPL4_REG
	opl4_wt
        ld	a,2	;set opl4 for memory access
	out	(c),a
	inc c
	opl4_wt
	ld	a,%00000001
	out	(c),a

	ld	hl,#4701	;initalize address in opl4 mem. #200174 (31 samples with 12 bytes of header)
	ld	(address+1),hl
	ld	a,#20
	ld	(address),a

	ld	a,(App_BnkNum)
	db	#dd:ld h,a
	ld	hl,sndfil
	call SyFile_FILOPN
	jp	c,put_error
	ld	(filenr),a

	ld	de,(App_BnkNum)   ;File laden
	ld	hl,modheader
	ld	bc,1084	;1084 bytes
	call SyFile_FILINP
	jp	c,put_error

        ld hl,"82"
        ld (prgwintxt4),hl
        xor a
        ld (prgwintxt4+2),a
        ld hl,"13"
        ld (propertxt26+1),hl
        ld de,modids31i         ;check, if it's 15 or 31 samples ("M.K.", "M!K!", "FLT4")
        call mt_load_id:jr z,mt_load1
        call mt_load_id:jr z,mt_load1
        call mt_load_id:jr z,mt_load1
    
        ld hl,"51"
        ld (propertxt26+1),hl
        ld a,(filenr)               ;only 15 -> move file pointer back
        ld c,1
        ld iy,-1
        ld ix,-484
	call SyFile_FILPOI
	jp	c,put_error             ;go 16*30+4 bytes backwards (only 15 instead of 31 samples, no "M.K." 4chars)

        ld hl,modheader+#1d6        ;move songlen+songlist behind 31 sample
        ld de,modheader+#3b6
        ld bc,128+2
        ldir
        ld hl,modheader+#1d6        ;set samples 16-31 to 0
        ld de,modheader+#1d6+1
        ld (hl),0
        ld bc,30*16-1
        ldir

mt_load1
	;get song length (number of patterns to play)
	ld	ix,modheader+950
	ld	a,(ix)
	ld	(song_length),a

	;copy the pattern index table (max 128 bytes)
	ld	hl,modheader+952
	ld	de,patterns
	ld	bc,128
	ldir

	;search the pattern table to find the highest pattern
	;defines how much pattern data to load
	ld	ix,modheader+952	;find highest pattern number 
	ld	c,0
	ld	b,128
find_pat
	ld	a,(ix)
	inc	ix
	cp	c
	jp	c,find_pat1
	ld	c,a
find_pat1
	djnz	find_pat

	inc	c
	ld	a,c
	ld	(numpatterns),a

	;load patterns  
	;-------------  
	;[hl]=number of patterns to load
	;[de]=destination address (mapping blocks as defined in bits 15 & 14)  

        ld a,(numpatterns)
        cp 62+1
        ld b,3
        jp nc,put_error0    ;too many patterns -> memory full
        add a:add a
        ld b,a
        xor a               ;a = any bank
        ld c,a
        ld (sndxtrlen),bc   ;bc = patnum*4*256 = patnum*1024 = required pattern memory
        ld e,a              ;e = code area
        push bc
        rst #20:dw jmp_memget
        pop bc
        ld d,b
        ld b,3
        jp c,put_error0     ;not enough memory available -> memory full
        ld b,d
        ld (sndxtrbnk),a
        ld (sndxtradr),hl

        ld e,a
	ld	a,(filenr)
	call SyFile_FILINP  ;load all pattern
	jp	c,put_error

        ld hl,-1
        ld (mt_address_l+1),hl
	xor	a
	ld	(song_pos),a
	ld	(song_step),a

	;EXAMINE sampleinfo
	;------------------

        ld ix,modheader+20          ;*** check total sample length
        ld b,31
        xor a
        ld l,a
        ld h,a
mt_smplen1
        ld d,(ix+22)
        ld e,(ix+23)
        add hl,de
        adc 0
        ld de,30
        add ix,de
        djnz mt_smplen1     ;a,hl=smplen_total/2
        add hl,hl
        adc a               ;a,hl=smplen_total, a=number of 64K bank
        ld c,a
        ld a,l
        or h
        jr z,mt_smplen2
        inc c               ;c=required 64K banks
mt_smplen2
        ld a,(op4_64kbnk)
        cp c
        ld a,3
        ret c               ;samples too big -> error

	ld	ix,modheader+20 ;mod sample attribute data
	xor	a
	ld	(sampnum),a
	ld	b,31	;31 samples!
make_hdrs                       ;*** sample loading main loop
	push	bc	;sampnum counter

        push ix:pop hl      ;show name of loaded sample
        push hl
        ld de,lstnam
        ld (prgwindsc2),de
        ld b,22
mt_smpnam1
        ld a,(hl)
        inc hl
        cp 32
        jr c,mt_smpnam2
        cp 127
        jr nc,mt_smpnam2
        ld (de),a
        inc de
mt_smpnam2
        djnz mt_smpnam1
        xor a
        ld (de),a
        ld e,prgwin_tit
        call winupd
        pop ix

	ld	d,(ix+42-20)
	ld	e,(ix+43-20)
	ld	bc,2
	call	maal

	ld	(samplen),hl	;set sample length

	ld	hl,finetunes
	ld	a,(sampnum)
	ld	e,a
	ld	d,0
	add	hl,de

	ld	a,(ix+44-20)	;sample finetune [-7...+7]
	ld	(hl),a

	ld	b,(ix+45-20)	;sample volume [0...64]

	ld	hl,volumes
	ld	a,(sampnum)
	ld	e,a
	ld	d,0
	add	hl,de
	ld	(hl),b

	ld	d,(ix+46-20)
	ld	e,(ix+47-20)
	ld	bc,2
	call	maal

	ld	(samprpp),hl	;sample repeat point

	ld	d,(ix+48-20)	;sample repeat length
	ld	e,(ix+49-20)
	ld	bc,2
	call	maal

	ld	(samprpl),hl

	ld	hl,address	;set wave header start address
	ld	de,wave_hdr+0
	ld	bc,3
	ldir

	push	ix	;load sample in chipmem
        ld hl,(address+1)   ;5
        ld a,l              ;1
        ld l,h              ;1
        ld h,a              ;1
        ld a,(address+0)    ;4 12

	ld	a,(address+1)   ;4
	ld	h,a             ;1
	ld	a,(address+2)   ;4
	ld	l,a             ;1
	ld	a,(address+0)   ;4 14
	call	set_adr	;set the opl4 sram destination address
	ld	hl,(samplen)
	call	loadsamp	;load the sample data and transfer it
	pop	ix
        jp c,put_error

	ld	hl,(samplen)
	ld	a,h
	or	l
	jp	z,lengthready
	dec	hl
	ld	(samplen),hl

	ld	hl,(samprpp)
	ld	a,h
	or	l
	jp	nz,repeat_nozer
	ld	hl,(samprpl)
	ld	de,3
	call	rst20
	jp	c,repeat_zero
repeat_nozer
	ld	hl,(samprpp)	;adres
	ld	de,(samprpl)	;number of bytes
	add	hl,de
	dec	hl
	ld	(samplen),hl	;address

	ld	hl,(samprpp)	;address
	jp	lengthready
repeat_zero
	ld	hl,(samplen)
lengthready
	ld	a,h	            ;set wave header loop address
	ld	(wave_hdr+3),a
	ld	a,l
	ld	(wave_hdr+4),a

	ld	a,(samplen+0)	;set wave header end address
	cpl
	ld	h,a
	ld	a,(samplen+1)
	cpl
	ld	l,a
	ld	(wave_hdr+5),hl

	ld	a,(sampnum)	;calculate the destination chipmem addres for the note header
	ld	c,a
	ld	b,0
	ld	de,12
	call	maal
	ld	a,#20       ;RAM starts at #200000 = 2MB
	call	set_adr

	ld bc,OPL4_REG
	opl4_wt
	ld	a,6	;set OPL4 memory data register
	out	(c),a

	ld	hl,wave_hdr	;put header in OPL4 memory
	ld	e,12
	inc c
wave_hdr_send
	opl4_wt
	ld	a,(hl)
	out	(c),a
	inc	hl
	dec e
	jr nz,wave_hdr_send

	ld	b,0	;add length
	ld	hl,(samplen)
	ld	de,address
	call	telop
	ld	hl,(samplen)
	ld	a,h
	or	l
	jp	z,nosample

	ld	hl,1
	ld	b,0
	ld	de,address
	call	telaf

	ld	a,(address+1)
	ld	h,a
	ld	a,(address+2)
	ld	l,a
	ld	a,(address+0)
	call	set_adr
        ld bc,OPL4_REG
	opl4_wt
	ld	a,6
	out	(c),a
	inc c
	opl4_wt
	in	e,(c)
	opl4_wt
	out	(c),e
	opl4_wt
	out	(c),e
	opl4_wt
	out	(c),e
	
	ld	b,0
	ld	hl,2+3
	ld	de,address
	call	telop

nosample
	ld	de,30	;add 30h bytes to samplepointer
	add	ix,de
	ld	a,(sampnum)
	inc	a
	ld	(sampnum),a

	pop	bc	;pop sampnum teller
	dec	b	;next sample
	ld	a,b
	or	a
	jp	nz,make_hdrs
	ld	a,1
	ld	(sampnum),a

	;close file
	ld	a,(filenr)
	call SyFile_FILCLO
        or a
	ret

;### playing routine

mt_start
	di
	ld	a,(channel)
	cp	255
	jp	nz,mt_start1
	call	mt_init
mt_start1
	ld	hl,pos_temp_ch0
	ld	de,pos_temp_ch0+1
	ld	bc,temp_size*4-1
	ld	(hl),0
	ldir
	xor	a
	ld	(mt_stopflag),a

	ld	a,(paused)
	or	a
	jp	nz,mt_start2

	ld	a,0
	ld	(song_pos),a
	ld	a,128
	ld	(song_step),a
	ld	hl,0
	ld	(tempo+0),hl
	ld	hl,249
	ld	(tempo+2),hl
	ld	hl,125
	ld	(tempo+4),hl
	ld	a,6
	ld	(vblanks+0),a
	ld	(vblanks+1),a
mt_start2
	ret

mt_stop
        call mt_stop0
	ld a,1
	ld (mt_stopflag),a
	ret
mt_stop0    ;mute only
	di
	ld	a,(channel)
	cp	255
	ret	z
	ld	hl,mod_init
	call	set_opl4
	ld	a,255
	ld	(channel),a
	ret

mt_music
	di

	ld	hl,(tempo)
	ld	a,h
	or	l
	jp	z,tempozero
	call	settempous
	jp	mt_exit
tempozero
	ld	hl,(tempo+2)
	ld	(tempo),hl
	call	settempous


	ld	a,(vblanks)
	dec	a
	ld	(vblanks),a
	or	a
	jp	z,mt_music_l0

off_note
	ld	a,(arpeggio)
	inc	a
	cp	3
	jp	nz,arpeggio_max
	xor	a
arpeggio_max
	ld	(arpeggio),a

	;maybe store old page settings
	call	mt_address	;will change the page

	xor	a
	ld	(channel),a

	ld	iy,pos_temp_ch0

	ld	b,4
offnote1
	push	bc
	ld	a,(ix+2)
	and	%1111
	jp	z,offncom_0
	cp	1
	jp	z,offncom_1
	cp	2
	jp	z,offncom_2
	cp	3
	jp	z,offncom_3
	cp	4
	jp	z,offncom_4
	cp	5
	jp	z,offncom_5
	cp	6
	jp	z,offncom_6
	cp	7
	jp	z,offncom_7
	cp	#A
	jp	z,offncom_A
	cp	#B
	jp	z,offncom_B
	cp	#D
	jp	z,offncom_D
	cp	#E
	jp	nz,of_return
	ld	a,(ix+3)
	and	%11110000
	cp	#60
	jp	z,offncom_E6
	cp	#90
	jp	z,offncom_E9
	cp	#C0
	jp	z,offncom_EC
	cp	#D0
	jp	z,offncom_ED
of_return
	pop	bc
	call	mt_addadr
	ld	a,(channel)
	cp	255
	jp	z,of_return1
	inc	a
	ld	(channel),a
of_return1
	djnz	offnote1
	jp	mt_exit

offncom_0
    	ld	a,(ix+3)
    	or	a
    	jp	z,of_return
    
    	ld	a,(arpeggio)
    	ld	l,(iy+n_period)
    	ld	h,(iy+n_period+1)
    	or	a
    	jp	z,offncom_09        ;arp0 -> hl=n_period
    	ld	l,(iy+n_notenum)
    	ld	h,0
    	cp	1
    	ld	a,(ix+3)
    	jp	nz,offncom_02       ;arp1/2 -> hl=n_notenum, a=offset 0/1
    	rrca
    	rrca
    	rrca
    	rrca
offncom_02
    	and	%1111
offncom_08
    	ld	e,a
    	ld	d,0
    	add	hl,de               ;hl=notenum + offset
    	sla	l
    	rl	h
    	ld	de,periods
    	add	hl,de
    	ld	e,(hl)
    	inc	hl
    	ld	d,(hl)
    	ex	de,hl               ;hl=period(notenum+offset)
    	ld	a,(iy+n_notenum)
    	push	af
    	call	mt_setnotep	;add finetunes
    	pop	af
    	ld	(iy+n_notenum),a
offncom_09
    	call	op_fnum
    	jp	of_return

offncom_1
	ld	e,(ix+3)
	ld	d,0
	ld	l,(iy+n_period)
	ld	h,(iy+n_period+1)
	scf
	ccf
	sbc	hl,de
	ld	de,108
	call	rst20
	jp	nc,offncom_11
	ld	hl,108
offncom_11	ld	(iy+n_period),l
	ld	(iy+n_period+1),h
	call	op_fnum
	jp	of_return

offncom_2
	ld	e,(ix+3)
	ld	d,0
	ld	l,(iy+n_period)
	ld	h,(iy+n_period+1)
	add	hl,de
	ld	de,907+1
	call	rst20
	jp	c,offncom_21
	ld	hl,907
offncom_21	ld	(iy+n_period),l
	ld	(iy+n_period+1),h
	call	op_fnum
	jp	of_return

offncom_3
	call	offncom_30
	jp	of_return
offncom_30
	ld	l,(iy+n_period)	;get current note
	ld	h,(iy+n_period+1)
	ld	e,(iy+n_cmdtemp)	;get new note
	ld	d,(iy+n_cmdtemp+1)
	call	rst20
	jp	z,op_fnum	;curnote = newnote

	jp	c,offncom_31

	push	de	;curnote > newnote (pitch down)
	ld	e,(iy+n_oldpbendsp)
	ld	d,0
	scf
	ccf
	sbc	hl,de
	pop	de
	jp	c,offncom_34
	call	rst20
	jp	nc,offncom_32
offncom_34
	ex	de,hl
	jp	offncom_32
offncom_31
	push	de	;curnote < newnote  (pitch up)
	ld	e,(iy+n_oldpbendsp)
	ld	d,0
	add	hl,de
	pop	de
	inc	de
	call	rst20
	jp	c,offncom_32
	dec	de
	ex	de,hl
offncom_32
	ld	(iy+n_period),l
	ld	(iy+n_period+1),h
	jp	op_fnum

offncom_4
	call	offncom_40
	jp	of_return
offncom_40
	ld	a,(iy+n_vibtabpos)
	rrca		;lsr.w #2,{a}
	rrca
	and	#1f	;and.w #$001F,{a}
	ld	c,a
	ld	b,0
	ld	a,(iy+n_wavectrl)
	and	%11
	jp	z,offncom_42

	sla	c	;lsl.b #3,{bc}
	sla	c
	sla	c
	cp	1
	jp	z,offncom_41	;vib_rampdown
	ld	c,255
	jp	offncom_43
offncom_41	;mt_vib_rampdown
	ld	a,(iy+n_vibtabpos)
	bit	7,a
	jp	z,offncom_43
	ld	a,255
	sub	c
	ld	c,a
	jp	offncom_43
offncom_42	;mt_vib_sine
	ld	hl,vib_tab
	add	hl,bc
	ld	c,(hl)
offncom_43	;mt_vib_set
	ld	a,(iy+n_oldvibcmd)
	and	%1111	;and.w #15,{a}
	ld	b,0
	ld	e,a
	ld	d,0
	call	maal	;mulu {bc},{de} -> {hl}
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l	;lsr.w #7,{hl}
	ex	de,hl
	ld	l,(iy+n_period)	;move.w n_period,{hl}
	ld	h,(iy+n_period+1)

	ld	a,(iy+n_vibtabpos)
	bit	7,a
	jp	nz,offncom_44
	add	hl,de
	jp	offncom_45
offncom_44	;mt_VibratoNeg
	scf
	ccf
	sbc	hl,de
offncom_45	;mt_vibrato3
	ld	(iy+n_cmdtemp),l
	ld	(iy+n_cmdtemp+1),h
	call	op_fnum
	ld	a,(iy+n_oldvibcmd)
	rrca
	rrca
	and	%111100
	add	(iy+n_vibtabpos)
	ld	(iy+n_vibtabpos),a
	ret

offncom_5
	call	offncom_30
	jp	offncom_A

offncom_6
	call	offncom_40
	jp	offncom_A

offncom_7
	ld	a,(iy+n_tremtabpos)
	rrca		;lsr.w #2,{a} 
	rrca
	and	#1f	;and.w #$001F,{a} 
	ld	c,a
	ld	b,0
	ld	a,(iy+n_wavectrl)
	rrca
	rrca
	and	%11
	jp	z,offncom_72

	sla	c	;lsl.b #3,{bc} 
	sla	c
	sla	c
	cp	1
	jp	z,offncom_71	;trem_rampdown
	ld	c,255
	jp	offncom_73
offncom_71	;mt_trem_rampdown
	ld	a,(iy+n_tremtabpos)
	bit	7,a
	jp	z,offncom_73
	ld	a,255
	sub	c
	ld	c,a
	jp	offncom_73
offncom_72	;mt_trem_sine
	ld	hl,vib_tab
	add	hl,bc
	ld	c,(hl)
offncom_73	;mt_trem_set
	ld	a,(iy+n_oldtremcmd)
	and	%1111	;and.w #15,{a} 
	ld	b,0
	ld	e,a
	ld	d,0
	call	maal	;mulu {bc},{de} -> {hl} 
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l	;lsr.w #6,{hl}
	ex	de,hl
	ld	l,(iy+n_volume)	;move.w n_volume,{hl}
	ld	h,0

	ld	a,(iy+n_tremtabpos)
	bit	7,a
	jp	nz,offncom_74
	add	hl,de
	jp	offncom_75
offncom_74	;mt_tremoloNeg
	scf
	ccf
	sbc	hl,de
	jp	nc,offncom_75
	ld	hl,0
offncom_75	;mt_tremolo3
	ld	a,l
	cp	#41
	jp	c,offncom_76
	ld	l,#40
offncom_76
	ld	e,l
	call	op_totallev0

	ld	a,(iy+n_oldtremcmd)
	rrca
	rrca
	and	%111100
	add	(iy+n_tremtabpos)
	ld	(iy+n_tremtabpos),a
	jp	of_return

offncom_A
	ld	a,(ix+3)
	cp	#10
	jp	nc,offncom_A1
	and	%1111
	ld	b,a
	ld	a,(iy+n_volume)
	sub	b
	jp	nc,offncom_A2
	xor	a
	jp	offncom_a2
offncom_A1
	rrca
	rrca
	rrca
	rrca
	and	%1111
	ld	b,a
	ld	a,(iy+n_volume)
	add	b
	cp	#41
	jp	c,offncom_A2
	ld	a,#40
offncom_A2
	ld	(iy+n_volume),a
	call	op_totallev
	jp	of_return

offncom_B
	ld	a,(song_pos)
	ld	b,a
	ld	a,(song_length)
	dec	a
	cp	b
	jp	nz,offncom_B1
	ld	a,(repeat_song)
	or	a
	;jp	nz,offncom_B1
	call	mt_stop
	jp	of_return
offncom_B1
	ld	a,(vblanks)
	cp	1
	jp	nz,of_return
	ld	a,(ix+3)
	ld	(song_pos),a
	ld	a,128
	ld	(song_step),a
	jp	of_return

offncom_D
	ld	a,(song_pos)
	ld	b,a
	ld	a,(song_length)
	dec	a
	cp	b
	jp	nz,offncom_D2
	ld	a,(repeat_song)
	or	a
	;jp	nz,offncom_D2
	call	mt_stop
	jp	of_return
offncom_D2
	ld	a,(vblanks)
	cp	1
	jp	nz,of_return
	ld	a,(song_step)
	bit	7,a
	jp	nz,of_return
	ld	a,(ix+3)
	set	7,a
	ld	(song_step),a
	ld	a,(song_length)
	ld	b,a
	ld	a,(song_pos)
	inc	a
	cp	b
	jp	c,offncom_D1
	xor	a
offncom_D1	ld	(song_pos),a
	jp	of_return

offncom_E6
	ld	a,(vblanks)
	cp	1
	jp	nz,of_return
	ld	a,(ix+3)
	and	%1111
	ld	b,a
	jp	nz,offncom_E61
	ld	a,(song_step)
	ld	(iy+n_pattloop),a
	jp	of_return
offncom_E61
	ld	a,(iy+n_pattloop+1)
	inc	a
	inc	b
	cp	b
	jp	nz,offncom_E62
	xor	a
	ld	(iy+n_pattloop+1),a
	jp	of_return
offncom_E62
	ld	(iy+n_pattloop+1),a
	ld	a,(iy+n_pattloop)
	set	7,a
	ld	(song_step),a
	jp	of_return


offncom_E9
	ld	a,(iy+n_cmdtemp)
	or	a
	jp	z,offncom_E91
	dec	a
	ld	(iy+n_cmdtemp),a
	or	a
	jp	nz,of_return
	ld	a,(ix+3)
	and	%1111
	ld	(iy+n_cmdtemp),a
offncom_E91
	call	mt_newnote
	jp	of_return

offncom_EC
	ld	a,(iy+n_cmdtemp)
	or	a
	jp	z,of_return
	dec	a
	ld	(iy+n_cmdtemp),a
	or	a
	jp	nz,of_return
	xor	a
	jp	offncom_A2

offncom_ED
	ld	a,(iy+n_cmdtemp)
	or	a
	jp	z,of_return
	dec	a
	ld	(iy+n_cmdtemp),a
	or	a
	jp	nz,of_return
	jp	offncom_E91

mt_music_l0
	ld	a,(vblanks+1)
	ld	(vblanks),a

	ld	a,(command_EEtemp)
	or	a
	jp	z,mt_music_l1
	dec	a
	ld	(command_EEtemp),a
	jp	mt_exit

mt_music_l1

	call	mt_addstep
	ld	a,(Channel)
	cp	255
	jp	z,mt_exit
	call	mt_address
	xor	a
	ld	(channel),a

	ld	iy,pos_temp_ch0

	ld	b,4
mt_music_l2
	push	bc
	ld	l,(iy+n_period)
	ld	h,(iy+n_period+1)
	call	op_fnum
	call	op_totallev
	xor	a
	ld	(command_E5temp),a
	call	mt_setsampl
	ld	a,(ix+2)
	and	%1111
	jp	z,command_0
	cp	4
	jp	z,command_4
	cp	7
	jp	z,command_7
	cp	#B
	jp	z,command_B
	cp	#C
	jp	z,command_C
	cp	#D
	jp	z,command_D
	cp	#f
	jp	z,command_F
	cp	#E
	jp	nz,mt_return
	ld	a,(ix+3)
	and	%11110000
	cp	#40
	jp	z,command_E4
	cp	#50
	jp	z,command_E5
	cp	#70
	jp	z,command_E7
	cp	#90
	jp	z,command_E9
	cp	#A0
	jp	z,command_EA
	cp	#B0
	jp	z,command_EB
	cp	#C0
	jp	z,command_EC
	cp	#D0
	jp	z,command_ED
	cp	#E0
	jp	z,command_EE
mt_return
	call	mt_newnote
	ld	a,(ix+2)
	and	%1111
	cp	#e
	jp	nz,mt_return_ED
	ld	a,(ix+3)
	and	%11110000
	cp	#10
	call	z,command_E1
	cp	#20
	call	z,command_E2
mt_return_ED
	call	op_key
	set	7,a
	ld e,a
	opl4_wt
	out	(c),e
	call	op_totallev
	call	mt_addadr

	ld	a,(channel)
	inc	a
	ld	(channel),a

	ld	a,(mt_stopflag)
	or	a
	call	nz,mt_stop
	pop	bc
	dec	b
	ld	a,b
	or	a
	jp	nz,mt_music_l2

mt_exit
	ret

mt_addstep
	ld	a,(song_step)
	bit	7,a
	jp	z,mt_addstep0
	res	7,a
	ld	(song_step),a
	ret
mt_addstep0
	inc	a
	cp	64
	jp	nz,mt_addstep2

	ld	a,(song_length)
	ld	b,a
	ld	a,(song_pos)
	inc	a
	cp	b
	jp	c,mt_addstep1
	ld	a,(repeat_song)
	or	a
	;jp	nz,mt_addstep3
	call	mt_stop
	ret
mt_addstep3
	xor	a
mt_addstep1	ld	(song_pos),a
	xor	a
mt_addstep2	ld	(song_step),a
	ret

mt_init
	ld	hl,mod_init
	call	set_opl4
	ret

mt_address
	ld	a,(song_pos)
	ld	e,a
	ld	d,0
	ld	hl,patterns
	add	hl,de
	ld	a,(hl)
	ld	(song_pospat),a

        add a:add a
        ld d,a
        ld e,0
        ld a,(song_step)
        add a
        ld l,a
        ld h,e
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,de
    
        ld de,(sndxtradr)
        add hl,de
    
        ex de,hl
mt_address_l        ;adr of last loaded patternline (-1=no)
        ld hl,0
        or a
        sbc hl,de
        jr z,mt_address1
    
        ld a,(App_BnkNum)
        add a:add a:add a:add a
        ld hl,sndxtrbnk
        or (hl)
        ex de,hl
    
        ld de,patlinbuf
        ld bc,16
    
        rst #20:dw jmp_bnkcop
mt_address1
        ld ix,patlinbuf
        ret

patlinbuf   ds 16


mt_addadr
	ld	de,4
	add	ix,de
	ld	de,temp_size
	add	iy,de
	ret

mt_setsampl
	ld	a,(ix+2)
	rrca
	rrca
	rrca
	rrca
	and	%1111
	ld	b,a
	ld	a,(ix+n_sampnum)
	and	%11110000
	or	b
	or	a
	ret	z
	ld	(iy+n_sampnum),a
	dec	a
	ld	e,a
	ld	d,0
	ld	hl,volumes
	add	hl,de
	ld	a,(hl)
	ld	(iy+n_volume),a	;set standard volume
	ret

mt_newnote
	ld	a,(iy+n_sampnum)	;get previous set samplenumber
	or	a
	ret	z	;exit if no samplenumber is set 

	ld	a,(ix+n_sampnum)
	and	%00001111
	ld	h,a
	ld	l,(ix+1)
	or	l
	ret	z	;exit if no period specified 

	ld	a,(ix+2)
	and	%1111
	cp	3
	jp	z,command_3
	cp	5
	jp	nz,no_comm_3
command_3
	ld	a,(ix+3)
	or	a
	jp	z,command_5
	ld	(iy+n_oldpbendsp),a
command_5
	call	mt_setnotep	;add finetunes  
	ld	a,h
	or	l
	ret	z
	ld	(iy+n_cmdtemp),l	;in command temp.
	ld	(iy+n_cmdtemp+1),h
	ret

no_comm_3
	push	hl
	ld	e,0
	call	op_totallev0
	call	op_key
	res	7,a
	ld e,a
	opl4_wt
	out	(c),e

	ld	hl,channel

	ld bc,OPL4_REG
	opl4_wt
	ld	a,#8	;set sample number   
	add	(hl)
	out	(c),a
	ld	a,(iy+n_oldsamp)
	ld	l,a
	ld	a,(iy+n_sampnum)
	cp	l
	jp	z,mt_newnote1
	ld	(iy+n_oldsamp),a
	add	127
	inc c
	ld e,a
	opl4_wt
	out	(c),e
mt_newnote1
	pop	hl
	call	mt_setnotep	;convert [hl] (nontuned) into tuned 
	ld	(iy+n_period),l
	ld	(iy+n_period+1),h
	call	op_fnum
	ret

mt_setnotep
	ld	a,h
	or	l
	ret	z	;exit if no period in pos_temp_ch
	push	hl	;push period nr. in tabel 
	ld	b,h
	ld	c,l

	call	mt_findnote	;zoek period in tabel 
	ld	(iy+n_notenum),l
	ld	b,h
	ld	c,l

	ld	e,(iy+n_sampnum)	;get samplenumber
	dec	e
	ld	d,0
	ld	hl,finetunes	;get finetune of sample 
	add	hl,de
	ld	de,0
	ld	a,(command_E5temp)
	or	a
	jp	nz,mt_setnotep0
	ld	a,(hl)
mt_setnotep0
	or	a
	jp	z,mt_setnotep1	;fintune=0 -> do not alter 

	dec	a
	push	bc	;push noteoffset

;    add a
;    ld l,a      ;*2
;    add a
;    add a
;    add a       ;*16
;    add l       ;*18
;    ld l,a
;    adc 0
;    ld h,a
;    add hl,hl   ;*36

	ld	h,0	; HL = A * 36
	sla	a
	sla	a
	ld	l,a
	sla	a
	sla	a
	sla	a
	rl	h
	add	a,l
	ld	l,a
	ld	a,h
	adc	a,0
	ld	h,a

	ld	de,ft_tab
	add	hl,de
	pop	de	;pop noteoffset
	add	hl,de	;add offset
	ld	a,(hl)
	ld	e,a
	ld	d,0

	bit	7,a
	jp	z,mt_setnotep1
	ld	d,255
mt_setnotep1
	pop	hl
	add	hl,de
	ret

mt_findnote
	ld	hl,periods	;find period in tabel; [hl]=notenr.

	ld	e,c
	ld	d,b

	ld	bc,36*256+255

mt_findnote1	ld	a,e
	cpi
	jp	nz,mt_findnote2
	ld	a,d
	cpi
	jp	nz,mt_findnote3
	ld	de,periods+2
	or a
	sbc	hl,de
	srl	h
	rr	l
	ret

mt_findnote2
        inc	hl
mt_findnote3
        djnz	mt_findnote1
	ld	hl,0
	ret

op_fnum
	ld	de,-108	;[HL] = amiga period
	add	hl,de

	sla	l
	rl	h
	ld	de,pitch_tab
	add	hl,de
	ld	d,(hl)
	inc	hl
	ld	e,(hl)

	ld	hl,channel

	ld bc,OPL4_REG
	opl4_wt
	ld	a,#38	;set octave   
	add	(hl)
	out	(c),a
	inc c
	opl4_wt
	ld	a,e
	set	3,a	;reverb?  
	out	(c),a
	dec c
	opl4_wt
	ld	a,#20	;set f-num  
	add	(hl)
	out	(c),a
	inc c
	opl4_wt
	ld	a,d
	set	0,a
	out	(c),a
	ret

op_totallev
	ld	e,(iy+n_volume)
op_totallev0
	ld	hl,channel
	ld bc,OPL4_REG
	opl4_wt
	ld	a,#50	;set total level   
	add	(hl)
	out	(c),a
	ld	d,0
	ld	hl,vol_tab
	add	hl,de
	ld	a,(hl)
	rlca
	set	0,a	;level direct 
	inc c
	ld e,a
	opl4_wt
	out	(c),e
	ld	hl,channel
	ret

op_key
	ld	hl,channel
	ld bc,OPL4_REG
	opl4_wt
	ld	a,#68	;keyon 
	add	(hl)
	out	(c),a
	inc c
	ld	e,(hl)
	ld	d,0
	ld	hl,pansetting
	add	hl,de
	ld	a,(hl)
	ret

settempous
	ld	hl,(tempo)
	ld	a,h	;tempo > 255  
	or	a
	jp	z,settempous1
	ld	l,255	;dan r2=255  
settempous1
	ld	a,l
	neg
	ld	b,a
;	ld	a,2	;disable OPL4 timer
;	out	(#c4),a
;	ld	a,b
;	out	(#c5),a
;	ld	a,4
;	out	(#c4),a
;	ld	a,128
;	out	(#c5),a
	ld	e,l
	ld	d,0
	or	a
	ld	hl,(tempo)
	sbc	hl,de
	ld	(tempo),hl
	ret

command_0
	xor	a
	ld	(arpeggio),a
	jp	mt_return

command_4
	ld	l,(iy+n_period)
	ld	h,(iy+n_period+1)
	ld	(iy+n_cmdtemp),h
	ld	(iy+n_cmdtemp+1),l

	ld	a,(ix+3)
	and	%11110000
	jp	z,command_41
	ld	b,a
	ld	a,(iy+n_oldvibcmd)
	and	%00001111
	or	b
	ld	(iy+n_oldvibcmd),a
command_41
	ld	a,(ix+3)
	and	%00001111
	jp	z,mt_return
	ld	b,a
	ld	a,(iy+n_oldvibcmd)
	and	%11110000
	or	b
	ld	(iy+n_oldvibcmd),a
	jp	mt_return

command_7
	ld	a,(ix+3)
	and	%11110000
	jp	z,command_71
	ld	b,a
	ld	a,(iy+n_oldtremcmd)
	and	%00001111
	or	b
	ld	(iy+n_oldtremcmd),a
command_71
	ld	a,(ix+3)
	and	%00001111
	jp	z,mt_return
	ld	b,a
	ld	a,(iy+n_oldtremcmd)
	and	%11110000
	or	b
	ld	(iy+n_oldtremcmd),a
	jp	mt_return

command_B
	ld	a,(song_pos)
	ld	b,a
	ld	a,(song_length)
	dec	a
	cp	b
	jp	nz,command_B1
	ld	a,(repeat_song)
	or	a
	;jp	nz,command_B1
	ld	a,1
	ld	(mt_stopflag),a
	jp	mt_return
command_B1
	ld	a,(vblanks+1)
	cp	1
	jp	nz,mt_return
	ld	a,(ix+3)
	ld	(song_pos),a
	ld	a,128
	ld	(song_step),a
	jp	mt_return

command_C
	ld	a,(ix+3)
	cp	#41
	jp	c,command_C1
	ld	a,#40
command_C1	ld	(iy+n_volume),a
	call	op_totallev
	jp	mt_return

command_D
	ld	a,(song_pos)
	ld	b,a
	ld	a,(song_length)
	dec	a
	cp	b
	jp	nz,command_D2
	ld	a,(repeat_song)
	or	a
	;jp	nz,command_D2
	ld	a,1
	ld	(mt_stopflag),a
	jp	mt_return
command_D2
	ld	a,(vblanks+1)
	cp	1
	jp	nz,mt_return
	ld	a,(song_step)
	bit	7,a
	jp	nz,mt_return
	ld	a,(ix+3)
	set	7,a
	ld	(song_step),a
	ld	a,(song_length)
	ld	b,a
	ld	a,(song_pos)
	inc	a
	cp	b
	jp	c,command_D1
	xor	a
command_D1	ld	(song_pos),a
	jp	mt_return

command_E1
	push	af
	ld	a,(ix+3)
	and	%1111
	ld	e,a
	ld	d,0
	ld	l,(iy+n_period)
	ld	h,(iy+n_period+1)
	scf
	ccf
	sbc	hl,de
	ld	de,108
	call	rst20
	jp	nc,command_E11
	ld	hl,108
command_E11
        ld	(iy+n_period),l
	ld	(iy+n_period+1),h
	call	op_fnum
	pop	af
	ret

command_E2
	push	af
	ld	a,(ix+3)
	and	%1111
	ld	e,a
	ld	d,0
	ld	l,(iy+n_period)
	ld	h,(iy+n_period+1)
	add	hl,de
	ld	de,907+1
	call	rst20
	jp	c,command_E21
	ld	hl,907
command_E21	ld	(iy+n_period),l
	ld	(iy+n_period+1),h
	call	op_fnum
	pop	af
	ret

command_E4
	ld	a,(ix+3)
	and	%11
	ld	b,a
	ld	a,(iy+n_wavectrl)
	and	%11111100
	or	b
	ld	(iy+n_wavectrl),a
	xor	a
	ld	(iy+n_vibtabpos),a
	jp	mt_return

command_E5
	ld	a,(ix+3)
	and	%1111
	ld	(command_E5temp),a
	jp	mt_return

command_E7
	ld	a,(ix+3)
	rlca
	rlca
	and	%1100
	ld	b,a
	ld	a,(iy+n_wavectrl)
	and	%11110011
	or	b
	ld	(iy+n_wavectrl),a
	xor	a
	ld	(iy+n_tremtabpos),a
	jp	mt_return

command_E9
	ld	a,(ix+3)
	and	%1111
	ld	(iy+n_cmdtemp),a
	jp	mt_return

command_EA
	ld	a,(ix+3)
	rrca
	rrca
	rrca
	rrca
	and	%1111
	ld	b,a
	ld	a,(iy+n_volume)
	add	b
	cp	#41
	jp	c,command_EA1
	ld	a,#40
command_EA1
	ld	(iy+n_volume),a
	call	op_totallev
	jp	mt_return

command_EB
	ld	a,(ix+3)
	and	%1111
	ld	b,a
	ld	a,(iy+n_volume)
	sub	b
	jp	nc,command_EA1
	xor	a
	jp	command_EA1

command_EC
	ld	a,(ix+3)
	and	%1111
	jp	z,command_EC1
	inc	a
	ld	(iy+n_cmdtemp),a
	jp	mt_return
command_EC1
	xor	a
	ld	(iy+n_volume),a
	jp	mt_return

command_ED
	ld	a,(ix+3)
	and	%1111
	jp	z,mt_return
	inc	a
	ld	(iy+n_cmdtemp),a
	jp	mt_return_ED

command_EE
	ld	a,(ix+3)
	and	%1111
	ld	(command_EEtemp),a
	jp	mt_return

command_F
	ld	a,(ix+3)
	cp	#20
	jp	nc,command_F1
	or	a
	push	af
	call	z,mt_stop
	pop	af
command_F0
	ld	(vblanks+1),a
	ld	(vblanks),a
	jp	mt_return
command_F1


if 1
	ld	e,(ix+3)
	ld	d,0
	ld	(tempo+4),de
	ld	hl,31250
	call	deel
	dec	bc
	ld	(tempo+2),bc
	ld	(tempo+0),bc
	call	settempous
	jp	mt_return
else
        ld e,a
        ld hl,750
        call div168
        jr command_F0
endif
