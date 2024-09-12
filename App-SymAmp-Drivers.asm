;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                            SOUND DEVICE DRIVERS                            @
;@                                                                            @
;@             (c) 2005-2021 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;todo


;==============================================================================
;### GENERAL DRIVER ROUTINES ##################################################
;==============================================================================

;### HRDDET -> searches for additional sound hardware
;### Input      (hrdbas)=platform (1=CPC, 2=MSX, 3=PCW, 4=EP, 5=NXT)
;### Output     HL=hardware (b[0]=PSG, b[1]=MP3MSX, b[2]=Playcity, b[3]=Darky, b[4]=OPL4, b[5]=OPL3, b[6]=TurboSound)
hrddet  ld a,(hrdbas)
        ld hl,0
        cp 1
        ret c
        jr z,hrddeta        ;cpc
        cp 3
        jr c,hrddetb        ;msx
        jr z,hrddetc        ;pcw
        cp 5
        jr c,hrddetd        ;ep
        jr z,hrddete        ;nxt
        ret
hrddeta call m3cres                 ;*** CPC
        ld hl,1
        jr c,hrddet1
        set 1,l             ;+ mp3
hrddet1 push hl
        call pcydet
        pop hl
        jr c,hrddet2
        set 2,l             ;+ playcity
hrddet2 push hl
        call dkydet
        pop hl
        jr c,hrddet5
        set 3,l             ;+ darky
hrddet5 push hl
        call op4det
        pop hl
        ret c
        set 4,l             ;+ opl4
        ret
hrddetb call m3mres                 ;*** MSX
        ld hl,1
        jr c,hrddet3
        set 1,l             ;+ mp3
hrddet3 push hl
        call dkydet
        pop hl
        jr c,hrddet4
        set 3,l             ;+ darky
hrddet4 push hl
        call op4det
        pop hl
        ret c
        set 4,l             ;+ opl4
        ret
hrddetc call pcwdet                 ;*** PCW
        ld hl,0
        ret c
        set 0,l             ;+ ay
        ret
hrddetd call m3mres                 ;*** EP
        ld hl,1
        jr c,hrddet6
        set 1,l             ;+ mp3
hrddet6 push hl
        call op4det
        pop hl
        ret c
        set 4,l             ;+ opl4
        ret
hrddete ld hl,1+64                  ;*** NXT
        ret

;### HRDINI -> selects correct hardware and inits it
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3, 4=MOD, 5=SA2), (sndmem)=module
;### Output     CF=1 -> can't be played on available hardware
hrdinichn   dw skschn,st2chn,0,pt3chn

hrdini  ld hl,(hrdext)
        cp 2
        jr z,hrdini6
        cp 4 
        jr nc,hrdini7
        push af             ;*** PSG
        add a
        ld l,a
        ld h,0
        ld bc,hrdinichn
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (hrdini1+1),de
        ld hl,sndmem
hrdini1 call 0              ;a=3/6 psg channels
        pop bc              ;b=module type
        ld hl,(hrdext)
        cp 6
        jr z,hrdini5
        bit 0,l             ;*** 3CHN psg
        scf
        ret z               ;no psg hardware available -> exit
        ld e,16
        bit 2,l
        jr nz,hrdini4       ;check, if playcity is wanted for 3chn module
        inc e
        bit 3,l
        jr nz,hrdini4       ;check, if darky    is wanted for 3chn module
hrdini2 ld a,(hrdbas)       ;use internal PSG
hrdini3 call psgset
        or a
        ret
hrdini4 ld a,(prfobjdatv)
        or a
        jr z,hrdini2        ;use internal 3CHN hardware for 3chn module
        ld a,e
        jr hrdini3          ;use external 6CHN hardware for 3chn module
hrdini5 ld a,16             ;*** 6CHN psg
        bit 2,l
        jr nz,hrdini3       ;cpc playcity   -> use it
        inc a
        bit 3,l
        jr nz,hrdini3       ;msx darky      -> use it
        inc a
        bit 6,l
        jr nz,hrdini3       ;zxs turbosound -> use it
        scf
        ret                 ;no 6chn psg hardware available -> exit
hrdini6 bit 1,l             ;*** MP3
        jr hrdini8
hrdini7 jr nz,hrdini9
        ld hl,(hrdext)      ;*** MOD
        bit 4,l
hrdini8 scf
        ret z               ;no mp3/wavetable hardware available -> exit
        or a
        ret
hrdini9 ld a,(prfobjdatw)   ;*** SA2
        or a
        jr nz,hrdinia
        ld hl,(hrdext)
        bit 4,l
        jr z,hrdini8
hrdinia call op3set
        or a
        ret

;### OP3SET -> set opl3 port addresses
;### Input      A=type (0=moonsound/opl4, 1=willy/opl3lpt)
op3set  or a
        ld hl,#ffc4         ;HL=#FFC4=sel1
        ld de,#c5c6         ;HD=#FFC5=dat1, HE=#FFC6=sel2
        ld bc,#3ec7         ;HC=#FFC7=dat2
        jr z,op3set1
        ld a,(prfwprobj+12)
        add a:add a
        ld c,a
        ld b,0
        ld hl,prfwprlst
        add hl,bc
        ld l,(hl)           ;L=base adr
        ld h,#fe            ;HL=#FEBC=sel1
        ld d,l
        inc d               ;HD=#FEBD=dat1
        ld e,d
        inc e               ;HE=#FEBE=sel2
        ld c,d              ;HC=#FEBD=dat2
        ld b,#c9
op3set1 ld a,b
        ld (opl3wt),a
        ld (opl3s1),hl
        ld l,d
        ld (opl3d1),hl
        ld l,e
        ld (opl3s2),hl
        ld l,c
        ld (opl3d2),hl
        ret

;### PSGSET -> patches sound routines for selected PSG sound hardware
;### Input      A=hardware (1=CPC PSG, 2=MSX PSG, 3=PCW PSG, 4=EP Dave, 5=ZXS PSG,
;###                        16=CPC PlayCity, 17=MSX Darky, 18=ZXS TurboSound)
;###            B=module type (0=SKM, 1=ST2, 3=PT3)

psgsetrou0
;skm prepare, skm one reg output, reg read, pt3 all reg output, hardware init
dw sendregCPC,cpcskm,cpcreg,cpcpt3,cpcini  ;cpc psg
dw sendregMSX,msxskm,msxreg,msxpt3,msxini  ;msx psg
dw sendreg0  ,pcwskm,pcwreg,pcwpt3,pcwini  ;pcw psg
dw sendreg0  ,eprskm,eprreg,eprpt3,eprini  ;ep  dave
dw sendreg0  ,zxsskm,zxsreg,zxspt3,zxsini  ;zxs psg

psgsetrou1
dw sendreg0  ,pcyskm,pcyreg,pcypt3,pcyini  ;cpc playcity
dw sendregMSX,dkyskm,dkyreg,dkypt3,dkyini  ;msx darky
dw sendreg0  ,zxsskm,zxsreg,turpt3,turini  ;zxs turbosound

psgsetadr
db  1:dw SENDREG+1
db 14:dw skmreg0+1,skmreg1+1,skmreg2+1,skmreg3+1,skmreg4+1,skmreg5+1,skmreg6+1,skmreg7+1,skmreg8+1,skmreg9+1,skmrega+1,skmregb+1,skmregc+1,skmregd+1
db  1:dw st2reg+1
db  1:dw TS_Play_ROUT+1
db  0

psgsetst3   dw cpcst2,msxst2,pcwst2,eprst2,zxsst2,0,0,0,0,0,0,0,0,0,0,pcyst2,dkyst2,zxsst2

psgset  push bc
        dec a
        add a
        ld (psgset5+1),a
        ld hl,psgsetrou0
        cp 16*2-2
        jr c,psgset7
        ld hl,psgsetrou1
        sub 16*2-2
psgset7 ld c,a
        add a
        add a
        add c               ;a=a*10
        ld c,a
        ld b,0
        add hl,bc
        ld ix,psgsetadr
psgset1 ld a,(ix+0)         ;a=number of patch addresses
        or a
        jr z,psgset4
        inc ix
        ld c,(hl)
        inc hl
        ld b,(hl)           ;bc=driver routine
        inc hl
        ex de,hl
psgset2 ld l,(ix+0)         ;loop
        ld h,(ix+1)
        ld (hl),c
        inc hl
        ld (hl),b           ;patch code
psgset3 inc ix:inc ix
        dec a
        jr nz,psgset2
        ex de,hl
        jr psgset1
psgset4 ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        pop af
        cp 1
        jr nz,psgset6
        push hl             ;ST2 -> overwrite old code with regout-routine
psgset5 ld hl,0
        ld bc,psgsetst3
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld bc,sndmem
        ld hl,#4a5+128
        add hl,bc
        ex de,hl
        ld bc,30
        ldir
        pop hl
psgset6 jp (hl)             ;call init routine

;### PT3FRQ -> sets PT3 frequency
;### Input      HL=frequency table (NT_CPC=CPC, NT_ZX=ZX Spectrum)
pt3frq  ld de,NT_
        ld bc,12*2*8
        ldir
        ret


;==============================================================================
;### CPC PSG ##################################################################
;==============================================================================

;### CPCINI -> inits CPC-PSG sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
cpcini  ld hl,NT_CPC    ;set CPC frequency for PT3 modules
        jr pt3frq

;### CPCREG -> reads PSG register from the CPC-PSG
;### Input      C=register
;### Output     A=value
;### Destroyed  F,BC,DE
cpcreg  ld de,#82f4
        ld b,e
        out (c),c
        ld bc,#f6c0
        out (c),c
        ld c,0
        out (c),c
        inc b
        ld a,#92
        out (c),a
        set 6,c
        ld b,#f6
        out (c),c
        ld b,e
        in a,(c)
        ld bc,#f700
        out (c),d
        dec b
        out (c),c
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### CPCSKM -> sends SKM register to the CPC-PSG
;### Input      D=register, A=data
;###            BC=#F4A0, BC'=#FC60, E'=#80
cpcskm  out (c),d
    	exx
    	out (c),c
    	defb #ed,#71
    	exx
    	out (c),a
    	exx
    	out (c),e
    	defb #ed,#71
    	exx
        ret

;### CPCST2 -> sends ST2 register to the CPC-PSG (**NOT YET IN USE**)
;### Input      A=register, C=data
cpcst2  ld b,#f4        ;2
        out (c),a       ;4
        ld a,c          ;1
        ld bc,#f6c0     ;3
        out (c),c       ;4
        db #ed,#71      ;4 out (c),0
        ld b,#f4        ;2
        out (c),a       ;4
        ld bc,#f680     ;3
        out (c),c       ;4
        db #ed,#71      ;4 out (c),0
        ret

;### CPCPT3 -> sends PT3 registers to the CPC-PSG
;### Input      (VARS1+VRS_AYREGS)=register (0-13)
cpcpt3  ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        xor a
cpcpt31 call cpcpt32
        inc a
        cp 13
        jr nz,cpcpt31
        bit 7,(hl)
        ret nz
cpcpt32 ld b,#f4        ;2
        out (c),a       ;4
        ld bc,#f6c0     ;3
        out (c),c       ;4
        db #ed,#71      ;4 out (c),0
        ld b,#f4+1      ;2
        outi            ;5
        ld bc,#f680     ;3
        out (c),c       ;4
        db #ed,#71      ;4 out (c),0
        ret


;==============================================================================
;### MSX PSG ##################################################################
;==============================================================================

;### MSXINI -> inits MSX-PSG sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
msxini  ld hl,NT_ZX     ;set ZXS frequency for PT3 modules
        jp pt3frq

;### MSXREG -> reads PSG register from the MSX-PSG
;### Input      C=register
;### Output     A=value
;### Destroyed  -
msxreg  ld a,c
        out (#a0),a
        in a,(#a2)
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### MSXSKM -> sends SKM register to the MSX-PSG
;### Input      D=register, A=data
;###            C=#A0
msxskm  out (c),d
        out (#a1),a
        ret

;### MSXST2 -> sends ST2 register to the MSX-PSG
;### Input      A=register, C=data
msxst2  out (#a0),a
        ld a,c
        out (#a1),a
        ret

;### MSXPT3 -> sends PT3 registers to the MSX-PSG
;### Input      (VARS1+VRS_AYREGS)=register (0-13)
msxpt3  ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        ld c,#a0
        xor a
msxpt31 out (c),a
        inc c
        outi
        dec c
        inc a
        cp 13
        jr nz,msxpt31
        out (c),a
        ld a,(hl)
        and a
        ret m
        inc c
        out (c),a
        ret


;==============================================================================
;### PCW PSG ##################################################################
;==============================================================================

PCW_AY_INDEX    equ #aa
PCW_AY_WRITE    equ #ab
PCW_AY_READ     equ #a9


;### PCWDET -> tries to detect a dk'tronics AY soundexpansion
;### Output     CF=0 -> AY found
;###            CF=1 -> no hardware detected
pcwdet  ld a,11
        ld c,#aa
        call pcwst2
        call pcwreg1
        cp c
        scf
        ret nz
        ld a,11
        ld c,#55
        call pcwst2
        call pcwreg1
        cp c
        scf
        ret nz
        or a
        ret

;### PCWINI -> inits PCW-PSG sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
pcwini  ld hl,NT_CPC    ;set CPC frequency for PT3 modules
        jp pt3frq

;### PCWREG -> reads PSG register from the PCW-PSG
;### Input      C=register
;### Output     A=value
;### Destroyed  -
pcwreg  ld a,c
        out (PCW_AY_INDEX),a
pcwreg1 in a,(PCW_AY_READ)
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### PCWSKM -> sends SKM register to the PCW-PSG
;### Input      D=register, A=data
pcwskm  ld c,a
        ld a,d
;### PCWST2 -> sends ST2 register to the PCW-PSG
;### Input      A=register, C=data
pcwst2  out (PCW_AY_INDEX),a
        ld a,c
        out (PCW_AY_WRITE),a
        ret

;### PCWPT3 -> sends PT3 registers to the PCW-PSG
;### Input      (VARS1+VRS_AYREGS)=register (0-13)
pcwpt3  ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        ld c,PCW_AY_INDEX
        xor a
pcwpt31 out (c),a
        inc c   ;c=write
        outi
        dec c   ;c=index
        inc a
        cp 13
        jr nz,pcwpt31
        out (c),a
        ld a,(hl)
        and a
        ret m
        inc c
        out (c),a
        ret


;==============================================================================
;### EP DAVE ##################################################################
;==============================================================================

;### EPRINI -> inits EP-Dave sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
eprini  ld hl,NT_ZX                 ;set ZXS frequency for PT3 modules
        call pt3frq
        cp 3
        ld a,0
        ld (envelopeInterrupt),a    ;enable EP driver routines
        ld (ayReset),a
        jp c,ayReset1               ;SKM,ST2 -> set CPC frequency
        jp ayReset2                 ;PT3     -> set ZXS frequency

;### EPRREG -> reads PSG register from the EP-Dave
;### Input      C=register
;### Output     A=value
;### Destroyed  F
eprreg  ld a,c
        jp ayRegisterRead


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### EPRSKM -> sends SKM register to the EP-Dave
;### Input      D=register, A=data
eprskm  ld      c,a
        ld      a,d
        jp      ayRegisterWrite

;### EPRST2 -> sends ST2 register to the EP-Dave
;### Input      A=register, C=data
eprst2  jp ayRegisterWrite

;### EPRPT3 -> sends PT3 registers to the EP-Dave
;### Input      (VARS1+VRS_AYREGS)=register (0-13)
eprpt3  ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        ld b,0
eprpt31 ld c,(hl)
        inc hl
        ld a,b
        call ayregisterwrite
        inc b
        ld a,b
        cp 13
        jr nz,eprpt31
        bit 7,(hl)
        ret nz
        ld c,(hl)
        ld a,b
        jp ayregisterwrite


;==============================================================================
;### ZXS PSG ##################################################################
;==============================================================================

ZXS_REGSEL  equ #FFFD   ;[W] 0000xxxx -> select PSG register,
                        ;    1lr111cc -> select chip (cc=3/2/1 for AY1/2/3) and enable l=left/r=right
                        ;[R] read from selected register
ZXS_REGDAT  equ #BFFD   ;[W] write to register


;### ZXSINI -> inits ZXS-PSG sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
zxsini  ld hl,NT_ZX     ;set CPC frequency for PT3 modules
        jp pt3frq

;### ZXSREG -> reads PSG register from the ZXS-PSG
;### Input      C=register
;### Output     A=value
;### Destroyed  F,BC,DE
zxsreg  ld a,c
        ld bc,ZXS_REGSEL
        out (c),a
        in a,(c)
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### ZXSST2 -> sends ST2 register to the ZXS-PSG
;### Input      A=register, C=data
zxsst2  ld d,a
        ld a,c
;### ZXSSKM -> sends SKM register to the ZXS-PSG
;### Input      D=register, A=data
zxsskm  ld bc,ZXS_REGSEL
        out (c),d
list
        ld b,ZXS_REGDAT/256
nolist
        out (c),a
        ret

;### ZXSPT3 -> sends PT3 registers to the ZXS-PSG
;### Input      (VARS1+VRS_AYREGS)=register (0-13)
zxspt3  ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        ld c,#fd
zxspt30 ld de,#c0ff ;e=#ff (select), d=#bf+1 (data)
        xor a
zxspt31 call zxspt32
        inc a
        cp 13
        jr nz,zxspt31
        bit 7,(hl)
        ret nz
zxspt32 ld b,e
        out (c),a
        ld b,d
        outi
        ret


;==============================================================================
;### ZXS TURBOSOUND ###########################################################
;==============================================================================

;### TURINI -> inits TurboSound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
turini  ld hl,NT_ZX         ;set ZXS frequency for PT3 modules
        call pt3frq
        ;...
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### TURPT3 -> sends PT3 registers to the TurboSound
;### Input      (VARS1+VRS_AYREGS)=register left  (0-13)
;###            (VARS2+VRS_AYREGS)=register right (0-13)
turpt3  ld hl,VARS2+VRS_AYREGS      ;channel 3-5
        ld a,%11111110
        call turpt31
        ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        ld a,%11111111
turpt31 ld bc,ZXS_REGSEL
        out (c),a
        jr zxspt30


;==============================================================================
;### CPC PLAYCITY #############################################################
;==============================================================================

PCY_YMZ_SEL_LEFT    equ #F988
PCY_YMZ_SEL_RIGHT   equ #F984
PCY_CHANNEL_0       equ #F880
PCY_CHANNEL_1       equ #F881
PCY_START_TIMER256  equ %00110111
PCY_STOP_CHANNEL    equ %00000011


;### PCYDET -> tries to detect a PlayCity
;### Output     CF=0 -> PlayCity found
;###            CF=1 -> no hardware detected
pcydetb ds 3
pcydetj jp pcydetn
pcydetf db 0

pcydet  di
        ld hl,#66
        push hl
        ld de,pcydetb       ;save nmi bytes
        ldi:ldi:ldi
        pop de
        ld hl,pcydetj       ;set nmi handler
        ldi:ldi:ldi
        ld b,#f5            ;wait for framefly
pcydet1 in a,(c)
        rra
        jr nc,pcydet1
        ld hl,32*256+PCY_START_TIMER256
        ld bc,PCY_CHANNEL_1
        out (c),l           ;enable timer
        out (c),h           ;set 32 scanlines constant
        ld ix,33*4-1
        call pcydet2        ;wait 33 scanlines
        ld hl,pcydetb
        ld de,#66
        ldi:ldi:ldi
        ei
        ld a,(pcydetf)
        cp 1
        ret
pcydet2 ds 5                ;** wait IX/4 scanlines
pcydet3 ds 6
        dec ix
        ld a,ixl
        or ixh
        jr nz,pcydet3
        ret
pcydetn push bc             ;** nmi handler
        push af
        ld a,1
        ld (pcydetf),a
        ld bc,PCY_CHANNEL_1
        ld a,PCY_STOP_CHANNEL
        out (c),a           ;disable timer 
        pop af
        pop bc
        retn

;### PCYINI -> inits PlayCity sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
pcyini  ld hl,NT_ZX         ;set ZXS frequency for PT3 modules
        call pt3frq
        ld de,#7f01
        ld bc,PCY_CHANNEL_0
        out (c),d
        cp 3
        jr c,pcyini1
        ld e,4
pcyini1 out (c),e
        ret

;### PCYREG -> reads PSG register from the PlayCity (left)
;### Input      C=register
;### Output     A=value
;### Destroyed  F,B,DE
pcyregs ds 16

pcyreg  ex de,hl
        ld b,0
        ld hl,pcyregs
        add hl,bc
        ld a,(hl)
        ex de,hl
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### PCYSKM -> sends SKM register to the PlayCity (left)
;### Input      D=register, A=data
pcyskm  ld bc,PCY_YMZ_SEL_LEFT
        out (c),d
        dec b
        out (c),a
        ret

;### PCYST2 -> sends ST2 register to the PlayCity (left)
;### Input      A=register, C=data
pcyst2  push hl
        ld l,c
        ld bc,PCY_YMZ_SEL_LEFT
        out (c),a
        dec b
        out (c),l
        and 15
        ld c,a
        ld b,0
        ld a,l
        ld hl,pcyregs
        add hl,bc
        ld (hl),a
        pop hl
        ret

;### PCYPT3 -> sends PT3 registers to the playcity
;### Input      (VARS1+VRS_AYREGS)=register left  (0-13)
;###            (VARS2+VRS_AYREGS)=register right (0-13)
pcypt3  ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        ld bc,PCY_YMZ_SEL_LEFT
        call pcypt31
        ld a,(snddatchn)
        cp 3
        ret z
        ld hl,VARS2+VRS_AYREGS      ;channel 3-5
        ld bc,PCY_YMZ_SEL_RIGHT
pcypt31 xor a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a:outi:inc b:inc a
        out (c),a
        ld a,(hl)
        and a
        ret m
        outi
        ret


;==============================================================================
;### MSX/CPC DARKY ############################################################
;==============================================================================

DKY_PSWITCH_ID  equ 170
DKY_PSWITCH     equ #ff40
DKY_CPLD_CTRL   equ #ff41
DKY_PSG1_INDEX  equ #ff44
DKY_PSG1_WRITE  equ #ff45
DKY_PSG1_READ   equ #ff46
DKY_PSG2_INDEX  equ #ff4c
DKY_PSG2_WRITE  equ #ff4d
DKY_PSG2_READ   equ #ff4e


;### DKYDET -> tries to detect a Darky
;### Output     CF=0 -> Darky found
;###            CF=1 -> no hardware detected
dkydet  ld a,8
        ld c,15
        call dkyst2
        ld c,8
        call dkyreg
        cp 15
        scf
        ret nz
        ld a,8
        ld c,0
        call dkyst2
        ld c,8
        call dkyreg
        cp 0
        scf
        ret nz
        or a
        ret

;### DKYINI -> inits Darky sound output
;### Input      A=module type (0=SKM, 1=ST2, 2=MP3, 3=PT3)
dkyini  ld a,DKY_PSWITCH_ID
        di
        ld bc,DKY_PSWITCH
        out (c),a
        xor a
        ld c,DKY_CPLD_CTRL
        out (c),a
        ei
        ld hl,NT_ZX         ;set ZXS frequency for PT3 modules
        jp pt3frq

;### DKYREG -> reads PSG register from the Darky (left)
;### Input      C=register
;### Output     A=value
;### Destroyed  BC,E
dkyreg  ld e,c
        ld a,DKY_PSWITCH_ID
        ld bc,DKY_PSWITCH
        di
        out (c),a
        ld c,DKY_PSG1_INDEX
        out (c),e
        ld c,DKY_PSG1_READ
        in a,(c)
        ei
        ret


;------------------------------------------------------------------------------
;@@@ MODULE DRIVERS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------------------------------------------------

;### DKYSKM -> sends SKM register to the Darky (left)
;### Input      D=register, A=data
dkyskm  ld c,a
        ld a,d
;### DKYST2 -> sends ST2 register to the Darky (left)
;### Input      A=register, C=data
dkyst2  push hl
        ld l,a
        ld h,c
        ld a,DKY_PSWITCH_ID
        ld bc,DKY_PSWITCH
        di
        out (c),a
        ld c,DKY_PSG1_INDEX
        out (c),l
        inc c
        out (c),h
        ei
        pop hl
        ret

;### DKYPT3 -> sends PT3 register to the Darky (left)
;### Input      (VARS1+VRS_AYREGS)=register left  (0-13)
;###            (VARS2+VRS_AYREGS)=register right (0-13)
dkypt3  ld a,DKY_PSWITCH_ID
        di
        ld bc,DKY_PSWITCH
        out (c),a
        ld c,DKY_PSG1_INDEX
        ld hl,VARS1+VRS_AYREGS      ;channel 0-2
        call dkypt31
        ld c,DKY_PSG2_INDEX
        ld hl,VARS2+VRS_AYREGS      ;channel 3-5
        ld a,(snddatchn)
        cp 6
        call z,dkypt31
        ei
        ret
dkypt31 xor a
        out (c),a:inc c:inc b:outi:dec c:inc a    ;12
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a:inc c:inc b:outi:dec c:inc a
        out (c),a
        ld a,(hl)
        rla:ret c:inc c:inc b:outi
        ret


;==============================================================================
;### MSX/CPC/EP OPL4 ##########################################################
;==============================================================================

;### OP4DET -> tries to detect an OPL4 chip
;### Output     CF=0 -> OPL4 found
;###            CF=1 -> no hardware detected
op4det  call ini_wav
        ld bc,OPL4_REG
;	    opl4_wt
        ld a,2
        out (c),a
        inc c
;	    opl4_wt
        in a,(c)
        and #e0
        cp #20
        jr z,op4det1
        scf
        ret
op4det1 di
        call op4mem
        ei
        ld (op4_64kbnk),a
        ld de,64
        call clcm16
        push hl:pop ix
        ld iy,prfobjtxt1f1
        ld e,4
        call clcnum
        ld (iy+1)," "
        ld (iy+2),"K"
        ld (iy+3),"B"
        ld (iy+4),")"
        ld (iy+5),0
        or a
        ret

;### OP4MEM -> detects OPL4 wavetable memory
;### Output     A=number of 64K banks
op4mem  ld bc,OPL4_REG
        ld h,#02
        ld a,#01
        call op4mem5
        ld e,32
op4mem1 dec e
        ld a,e
        call op4mema        ;set address
        ld a,e
        call op4memw        ;write values
        ld a,e:xor #55
        call op4memw
        ld a,e:xor #aa
        call op4memw
        inc e:dec e
        jr nz,op4mem1
op4mem2 ld a,e
        call op4mema
        call op4memr
        cp e
        jr nz,op4mem3
        call op4memr
        xor #55:cp e
        jr nz,op4mem3
        call op4memr
        xor #aa:cp e
        jr nz,op4mem3
        inc e
        bit 6,e
        jr z,op4mem2
op4mem3 ld h,#02
        ld a,#10
        call op4mem5
        ld a,e
        ret
;set 64k bank in A
op4mema add #20
        ld h,#03   :call op4mem5    ;hig
        inc h:xor a:call op4mem5    ;mid
        inc h:xor a:jr   op4mem5    ;low
;write A to memory
op4memw ld h,6
op4mem5 ld l,a
	    opl4_wt
        out (c),h
        inc c
	    opl4_wt
        out (c),l
        dec c
        ret
;read A from memory
op4memr ld h,6
	    opl4_wt
        out (c),h
        inc c
	    opl4_wt
        in a,(c)
        dec c
        ret
