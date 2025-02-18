;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@                                 PT3 PLUGIN                                 @
;@                   (c) by S.V.Bulba, SyX, Grim, Prodatron                   @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo



;==============================================================================
;### API-ROUTINES #############################################################
;==============================================================================

;### PT3CHN -> detects number of channels
;### Input      HL=address
;### Output     A=number of channels (3, 6)
pt3chn  ld bc,(sndlodlen)
        call pt3tss
        ld a,3
        ret c
        ld a,6
        ret

;### PT3INI -> Initialisiert PT3-Sound
;### Eingabe    HL=Adresse
;### Ausgabe    CF=0 -> ok, HL=Anzahl Positionen, A=Anzahl Kanäle
;###            CF=1 -> nicht unterstütztes Format
;### Veraendert AF,BC,DE,HL,IX,IY
pt3inia dw 0
pt3ini  ld (pt3inia),hl
        push hl
        ld (hl),0
        ld de,sndtxt_titl   ;copy titel
        ld bc,256*30+30
        call pt3ini2
        ld de,sndtxt_auth   ;copy author
        ld bc,256*30+6
        call pt3ini2
        pop hl
        push hl
        ld bc,(sndlodlen)
        call pt3tss
        pop hl
        ld bc,256*3+%00000000
        jr c,pt3ini1
        ;...check if 6chn hardware vorhanden -> scf,ret falls nicht
        ld bc,256*6+%00010000
        ex de,hl
        add hl,de
        ex de,hl
pt3ini1 ld a,b
        add "0"
        ld (propertxt27+1),a
        push bc
        ld a,c
        ld (TS_SETUP),a
        push hl
        ld bc,101
        add hl,bc
        ld l,(hl)
        ex (sp),hl
        di
        ex af,af'
        push af
        ex af,af'
        call TS_Init
        ex af,af'
        pop af
        ex af,af'
        ei
        pop hl
        xor a
        ld h,a
        pop bc
        ld a,b
        ret
pt3ini2 ld a,b              ;copy songinfo
        ld b,0
        add hl,bc
        ld c,a
        ldir
        ex de,hl
        ld a,32
pt3ini3 ld (hl),0
        dec hl
        cp (hl)
        jr z,pt3ini3
        ex de,hl
        ret

;### PT3VOL -> Setzt die globale Lautstärke
;### Eingabe    A=Lautstärke (0-255)
;### Verändert  AF
pt3vol  srl a:srl a:srl a:srl a
        neg
        add 15
        ;ld (ROUT+1),a  ##!!##
        ret

;### PT3VAL -> Holt Lautstärke und Frequenz eines Kanales
;### Eingabe    A=Kanal (0-x)
;### Ausgabe    A=Lautstärke (0-15), L=Frequenz (0-15)
;### Verändert  ?
pt3val  cp 3
        ld de,VARS1+VRS_AYREGS
        jr c,pt3val1
        ld de,VARS2+VRS_AYREGS
        sub 3
pt3val1 ld hl,8
        ld c,a
        ld b,h
        add hl,de
        add hl,bc
        ld a,(hl)
        ex de,hl
        ld e,a
        add hl,bc
        add hl,bc
        ld a,(hl)
        inc hl
        and 7
        add a
        ld l,a
        ld a,(hl)
        rlca
        and 1
        or l
        ld l,a
        ld a,e
        and 15
        ret

;### PT3PLY -> Spielt initialisierten PT3-Sound (muß mit 50Hz aufgerufen werden)
;### Ausgabe    HL=Position, CF=1 -> Ende wurde erreicht
;### Veraendert AF,BC,DE,HL,IX,IY
pt3ply  di
        ex af,af'
        push af
        call envelopeInterrupt      ;!!!
        ex af,af'
        call TS_Play
        ex af,af'
        pop af
        ex af,af'
        ei
        ld a,(VARS1+VRS_CurPos)
        ld hl,snddatlen
        cp (hl)
        jr c,pt3ply1
        ld a,(hl)
        dec a
pt3ply1 ld hl,TS_SETUP
        or a
        bit 7,(hl)
        res 7,(hl)
        ld l,a
        ld h,0
        ret z
        scf
        ret

;### PT3STP -> Hält PT3-Sound an
;### Veraendert AF,BC,DE,HL,IX,IY
pt3stp  di
        ex af,af'
        push af
        ex af,af'
        call TS_Stop
        ex af,af'
        pop af
        ex af,af'
        ei
        ret

;### PT3POS -> Setzt PT3-Sound auf eine bestimmte Position
;### Eingabe    HL=Position, Sound muß initialisiert sein
;### Veraendert AF,BC,DE,HL,IX,IY
pt3pos  ld a,(VARS1+VRS_PosSub)
        add l
        ld (VARS1+VRS_CrPsPtr),a
        ld a,(VARS2+VRS_PosSub)
        add l
        ld (VARS2+VRS_CrPsPtr),a
        ret


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

;### PT3TSS -> Searches for TurboSound-ID (2xPT3 module for 6 channel play)
;### Input      HL=address, BC=maximum length
;### Output     CF=0 -> ok, DE=offset of 2nd module
;###            CF=1 -> no TurboSound-ID found
;### Destroyed  ??
pt3tssa db "PT3!"
pt3tssb db "02TS"

pt3tss  ld a,c                  ;test, if end reached
        or b
        scf
        ret z
        ld a,"P"                ;search for "P"
        cpir
        scf
        ret nz
        ld de,pt3tssa           ;test, if "PT3!" at this address
        call pt3tss1
        jr nz,pt3tss
        push hl
        ld de,6
        add hl,de
        ld de,pt3tssa           ;test, if "PT3!" 6 bytes ahead
        call pt3tss1
        jr nz,pt3tss4
        ld de,6
        add hl,de
        ld de,pt3tssb           ;test, if "02TS" 12 bytes ahead
        call pt3tss1
pt3tss4 pop hl
        jr nz,pt3tss
        inc hl:inc hl:inc hl    ;found -> grab offset (=length of the 1st module)
        ld e,(hl)
        inc hl
        ld d,(hl)
        or a
        ret
pt3tss1 push hl
        dec hl
        db #dd:ld l,4
pt3tss2 ld a,(de)
        cp (hl)
        jr nz,pt3tss3
        inc de
        inc hl
        db #dd:dec l
        jr nz,pt3tss2
pt3tss3 pop hl
        ret


;==============================================================================
;### PT3-ROUTINEN (INTERN) ####################################################
;==============================================================================

; ---------------------------------------------------------------------------
; PT3 Turbo Sound player for Amstrad CPC
; (c) 2013 SyX
; Based in the Universal PT2'n'PT3 Turbo Sound player for ZX Spectrum
; (c)2004-2007 S.V.Bulba <vorobey@mail.khstu.ru>
; http//bulba.untergrund.net/ (http//bulba.at.kz/)
; and the Amstrad CPC/Plus Port by Grim/Arkos^Semilanceata 
; 2014 modified by Prodatron for Maxam assembler (WinApe) and SymAmp/SymbOS
; ---------------------------------------------------------------------------

; Variables for the conditional assembly

PSG_Test        equ 0   ;psg testmode
; 4) Use CPC frequencies or ZX
CPCFreq         equ 0

; Features
; --------
; - Can be compiled at any address (i.e. no need rounding ORG
;    address).
; - Variables (VARS) can be located at any address (not only after
;    code block).
; - TS_Init subprogram checks PT3-module version and rightly
;    generates both note and volume tables outside of code block
;    (in VARS).
; - Two portamento (spc. command 3xxx) algorithms (depending of
;    PT3 module version).
; - New 1.XX and 2.XX special command behaviour (only for PT v3.7
;    and higher).
; - Any Tempo value are accepted (including Tempo=1 and Tempo=2).
; - TS modes 2xPT3, 2xPT2 and PT v3.7 TS standard.
; - Fully compatible with Ay_Emul PT3 and PT2 players codes.

; Limitations
; -----------
; - Can run in RAM only (self-modified code is used).
; - PT2 position list must be end by #FF marker only.

; Warning!!! TS_Play subprogram can crash if no module are loaded
; into RAM or TS_Init subprogram was not called before.

; Call TS_Stop or TS_Init one more time to mute sound after stopping
; playing 

; Constants with the AY/YM registers 
TonA        EQU 0            ; Channel A Frequency
TonB        EQU 2            ; Channel B Frequency
TonC        EQU 4            ; Channel C Frequency
Noise       EQU 6            ; Noise Frequency 
Mixer       EQU 7            ; PSG/YM Control 
AmplA       EQU 8            ; Channel A Amplitude
AmplB       EQU 9            ; Channel B Amplitude
AmplC       EQU 10           ; Channel C Amplitude
Env         EQU 11           ; Envelope Frequency
EnvTp       EQU 13           ; Envelope Shape/Cycle

TS_SETUP    DEFB 0  ; set bit0, if you want to play without looping (optional)
                    ; set bit1 for PT2 and reset for PT3 before calling TS_Init
                    ; bits2-3 %00-ABC, %01-ACB, %10-BAC (optional)
                    ; bits4-5 %00-no TS, %01-2 modules TS, %10-
                    ;           autodetect PT3 TS-format by AlCo (PT 3.7+);
                    ;           Remark old PT3 TS-format by AlCo (PT 3.6) is not
                    ;           documented and must be converted to new standard.
                    ; bit6 is set each time, when loop point of 2nd TS
                    ;          module is passed (optional).
                    ; bit7 is set each time, when loop point of 1st TS
                    ;          or of single module is passed (optional).

CHECKLP
    LD   HL,TS_SETUP
    BIT  0,(IY-100+VRS_ModNum)
    JR   Z,CHL1
    SET  6,(HL)
    JR   CHL2
CHL1
    SET  7,(HL)
CHL2
    BIT  0,(HL)
    RET  Z
    POP  HL
    INC  (IY-100+VRS_DelyCnt)
    INC  (IY-100+VRS_ChanA+CHP_NtSkCn)
    XOR  A
    LD   (IY-100+VRS_AYREGS+AmplA),A
    LD   (IY-100+VRS_AYREGS+AmplB),A
    LD   (IY-100+VRS_AYREGS+AmplC),A
    RET

; ---------------------------------------------------------------------------
; Stop the music and silent the channels.
; ---------------------------------------------------------------------------
TS_Stop
    XOR  A
    LD   H,A
    LD   L,A
    LD   (VARS1+VRS_AYREGS+AmplA),A
    LD   (VARS1+VRS_AYREGS+AmplB),HL
    LD   (VARS2+VRS_AYREGS+AmplA),A
    LD   (VARS2+VRS_AYREGS+AmplB),HL
    JP   TS_Play_ROUT

; ---------------------------------------------------------------------------
; Initialize the song
; ENTRIES
;     HL = Address of module or first module in case of split songs
;     DE = Address of second module in case of split songs (optional)
; ---------------------------------------------------------------------------
TS_Init
    PUSH DE
    PUSH HL
    
    ; Clean VARS1 + VARS2
    LD   HL,VARS
    LD   (HL),0
    LD   DE,VARS+1
    LD   BC,VAR0END-VARS-1
    LDIR
 
    INC  HL
    LD   (VARS1+VRS_AdInPtA),HL ; ptr to zero
    LD   (VARS2+VRS_AdInPtA),HL

    POP  HL
    LD   IY,VARS1+100
    CALL INITPT3

    POP  HL
    ; Use version and tone table of 1st module *** REVIEW for reduce size in files ***
    LD   A,(IX+13-100)  ; Extract version number
    SUB  #30            ; A - '0'
    JR   C,TS_Init_L20
    CP   10             ; < 10?
    JR   C,TS_Init_L21
TS_Init_L20
    LD   A,6            ; Version 6 for unknown type
TS_Init_L21
    LD   (Version),A    ; Set version for the song

    ; Test for Turbo-Sound
    LD   IY,VARS2+100
    LD   A,(TS_SETUP)
    AND  %00110000      ; Mask bits 5-4

jr z,nots
    ;RET  Z              ; (JR Z,NOTS) %00 = No Turbo Sound

    CP   16
    JR   Z,TwoPT3s      ; %01 = 2 x PT3 songs
    LD   A,(Version)
    CP   7

jr c,nots
    ;RET  C              ; (JR C,NOTS) Song version <> 7, then no Turbo Sound

    LD   A,(IX+98-100)  ; Get Alco Turbo Sound marker
    CP   32             ; is space?

jr z,nots
    ;RET  Z              ; (JR Z,NOTS) Yes, then NO Turbo Sound

    ; It's a Turbo Sound song, copy VARS1 to VARS2
    LD   HL,VARS1
    LD   DE,VARS2
    LD   BC,VRS_datlen
    LDIR

    SET  1,(IY-100+VRS_ModNum)
    LD   C,A
    ADD  A,A
    ADD  A,C
    SUB  2
    LD   (TSSub),A
    JR   AlCoTS_
TwoPT3s
    CALL INITPT3
AlCoTS_
    LD   A,1
    LD   (is_ts),A
    SET  0,(IY-100+VRS_ModNum)

NOTS
    LD   A,1
    LD   (VARS1+VRS_DelyCnt),A
    LD   (VARS2+VRS_DelyCnt),A

    LD   HL,#F001       ; H - CHP_Volume, L - CHP_NtSkCn
    LD   (VARS1+VRS_ChanA+CHP_NtSkCn),HL
    LD   (VARS1+VRS_ChanB+CHP_NtSkCn),HL
    LD   (VARS1+VRS_ChanC+CHP_NtSkCn),HL
    LD   (VARS2+VRS_ChanA+CHP_NtSkCn),HL
    LD   (VARS2+VRS_ChanB+CHP_NtSkCn),HL
    LD   (VARS2+VRS_ChanC+CHP_NtSkCn),HL

    LD   HL,PT3EMPTYORN
    LD   (VARS1+VRS_ChanA+CHP_OrnPtr),HL
    LD   (VARS1+VRS_ChanB+CHP_OrnPtr),HL
    LD   (VARS1+VRS_ChanC+CHP_OrnPtr),HL
    LD   (VARS2+VRS_ChanA+CHP_OrnPtr),HL
    LD   (VARS2+VRS_ChanB+CHP_OrnPtr),HL
    LD   (VARS2+VRS_ChanC+CHP_OrnPtr),HL

    JP   TS_Play_ROUT

    ; Initialize variables for PT3 song
INITPT3
    ; Set song pointer
    LD   (IY-100+VRS_MODADDR),L
    LD   (IY-100+VRS_MODADDR+1),H

    PUSH HL
    LD   DE,100
    ADD  HL,DE
    LD   A,(HL)             ; Get song Tempo
    LD   (IY-100+VRS_Delay),A
    PUSH HL
    POP  IX
    ADD  HL,DE

    ; SETCPPT
    LD   (IY-100+VRS_CrPsPtr),L
    LD   (IY-100+VRS_CrPsPtr+1),H

    LD   E,(IX+102-100)
    INC  HL

    LD   (IY-100+VRS_PosSub),L

    ADD  HL,DE
    ; SETLPPT
    LD   (IY-100+VRS_LPosPtr),L
    LD   (IY-100+VRS_LPosPtr+1),H
    POP  DE
    LD   L,(IX+103-100)
    LD   H,(IX+104-100)
    ADD  HL,DE
    ; SETPTPT
    LD   (IY-100+VRS_PatsPtr),L
    LD   (IY-100+VRS_PatsPtr+1),H
    LD   HL,169
    ADD  HL,DE
    ; SETORPT
    LD   (IY-100+VRS_OrnPtrs),L
    LD   (IY-100+VRS_OrnPtrs+1),H
    LD   HL,105
    ADD  HL,DE
    ; SETSMPT
    LD   (IY-100+VRS_SamPtrs),L
    LD   (IY-100+VRS_SamPtrs+1),H
    RET

PTDECOD
    PUSH IY
    POP  IX
    ADD  IX,DE
    PUSH IY
    POP  IX
    ADD  IX,DE
    JP   PT3PD

; PT3 pattern decoder
PD_OrSm
    LD   (IX-12+CHP_Env_En),0
    CALL SETORN
PD_SAM_
    LD   A,(BC)
    INC  BC
    RRCA

PD_SAM
    CALL SETSAM
    JR   PD_LOOP

PD_VOL
    RRCA
    RRCA
    RRCA
    RRCA
    LD   (IX-12+CHP_Volume),A
    JR   PD_LP2
    
PD_EOff
    LD   (IX-12+CHP_Env_En),A
    LD   (IX-12+CHP_PsInOr),A
    JR   PD_LP2

PD_SorE
    DEC  A
    JR   NZ,PD_ENV
    LD   A,(BC)
    INC  BC
    LD   (IX-12+CHP_NNtSkp),A
    JR   PD_LP2

PD_ENV
    CALL SETENV
    JR   PD_LP2

PD_ORN
    CALL SETORN
    JR   PD_LOOP

PD_ESAM
    LD   (IX-12+CHP_Env_En),A
    LD   (IX-12+CHP_PsInOr),A
    CALL NZ,SETENV
    JR   PD_SAM_

PT3PD
    LD   A,(IX-12+CHP_Note)
    LD   (PrNote+1),A
    LD   L,(IX-12+CHP_CrTnSl)
    LD   H,(IX-12+CHP_CrTnSl+1)
    LD   (PrSlide+1),HL

PD_LOOP
    LD   DE,#2010
PD_LP2
    LD   A,(BC)
    INC  BC
    ADD  A,E
    JR   C,PD_OrSm
    ADD  A,D
    JR   Z,PD_FIN
    JR   C,PD_SAM
    ADD  A,E
    JR   Z,PD_REL
    JR   C,PD_VOL
    ADD  A,E
    JR   Z,PD_EOff
    JR   C,PD_SorE
    ADD  A,96
    JR   C,PD_NOTE
    ADD  A,E
    JR   C,PD_ORN
    ADD  A,D
    JR   C,PD_NOIS
    ADD  A,E
    JR   C,PD_ESAM
    ADD  A,A
    LD   E,A
    LD   HL,SPCCOMS ;+#FF20-#2000
    ADD  HL,DE
ld de,#FF20-#2000
add hl,de
    LD   E,(HL)
    INC  HL
    LD   D,(HL)
    PUSH DE
    JR   PD_LOOP

PD_NOIS
    LD   (IY-100+VRS_Ns_Base),A
    JR   PD_LP2

PD_REL
    RES  0,(IX-12+CHP_Flags)
    JR   PD_RES

PD_NOTE
    LD   (IX-12+CHP_Note),A
    SET  0,(IX-12+CHP_Flags)
    XOR  A

PD_RES
    ;DI                  ; Disable Ints for SP
    LD   (PDSP_ + 1),SP
    LD   SP,IX
    LD   H,A
    LD   L,A
    PUSH HL
    PUSH HL
    PUSH HL
    PUSH HL
    PUSH HL
    PUSH HL
PDSP_
    LD   SP,#0000
    ;EI                  ; Enable Ints

PD_FIN
    LD   A,(IX-12+CHP_NNtSkp)
    LD   (IX-12+CHP_NtSkCn),A
    RET

C_PORTM
    LD   A,(BC)
    INC  BC
    ; Skip precalculated tone delta (because cannot be right after pt3 compilation)
    INC  BC
    INC  BC
    EX   AF,AF'
    LD   A,(BC)         ; Signed tone step
    INC  BC
    LD   (LoStep),A
    LD   A,(BC)
    INC  BC
    AND  A
    EX   AF,AF'
    LD   L,(IX-12+CHP_CrTnSl)
    LD   H,(IX-12+CHP_CrTnSl+1)

; Set portamento variables
; A - Delay; A' - Hi(Step); ZF' - (A'=0); HL - CrTnSl
SETPORT
    RES  2,(IX-12+CHP_Flags)
    LD   (IX-12+CHP_TnSlDl),A
    LD   (IX-12+CHP_TSlCnt),A
    PUSH HL
    LD   DE,NT_
    LD   A,(IX-12+CHP_Note)
    LD   (IX-12+CHP_SlToNt),A
    ADD  A,A
    LD   L,A
    LD   H,0
    ADD  HL,DE
    LD   A,(HL)
    INC  HL
    LD   H,(HL)
    LD   L,A
    PUSH HL
PrNote
    LD   A,#3E
    LD   (IX-12+CHP_Note),A
    ADD  A,A
    LD   L,A
    LD   H,0
    ADD  HL,DE
    LD   E,(HL)
    INC  HL
    LD   D,(HL)
    POP  HL
    SBC  HL,DE
    LD   (IX-12+CHP_TnDelt),L
    LD   (IX-12+CHP_TnDelt+1),H
    POP  DE
Version  EQU $+1
    LD   A,#3E
    CP   6
    JR   C,OLDPRTM  ; Old 3xxx for PT v3.5- *** Review ***
PrSlide
    LD   DE,#1111
    LD   (IX-12+CHP_CrTnSl),E
    LD   (IX-12+CHP_CrTnSl+1),D
LoStep   EQU $+1
OLDPRTM
    LD   A,#3E
    EX   AF,AF'
    JR   Z,NOSIG
    EX   DE,HL
NOSIG
    SBC  HL,DE
    JP   P,SET_STP
    CPL
    EX   AF,AF'
    NEG
    EX   AF,AF'
SET_STP
    LD   (IX-12+CHP_TSlStp+1),A
    EX   AF,AF'
    LD   (IX-12+CHP_TSlStp),A
    LD   (IX-12+CHP_COnOff),0
    RET

C_GLISS
    SET  2,(IX-12+CHP_Flags)
    LD   A,(BC)
    INC  BC
    LD   (IX-12+CHP_TnSlDl),A
    AND  A
    JR   NZ,GL36
    LD   A,(Version)    ; AlCo PT3.7+
    CP   7
    SBC  A,A
    INC  A
GL36
    LD   (IX-12+CHP_TSlCnt),A
    LD   A,(BC)
    INC  BC
    EX   AF,AF'
    LD   A,(BC)
    INC  BC
    JR   SET_STP

C_SMPOS
    LD   A,(BC)
    INC  BC
    LD   (IX-12+CHP_PsInSm),A
    RET

C_ORPOS
    LD   A,(BC)
    INC  BC
    LD   (IX-12+CHP_PsInOr),A
    RET

C_VIBRT
    LD   A,(BC)
    INC  BC
    LD   (IX-12+CHP_OnOffD),A
    LD   (IX-12+CHP_COnOff),A
    LD   A,(BC)
    INC  BC
    LD   (IX-12+CHP_OffOnD),A
    XOR  A
    LD   (IX-12+CHP_TSlCnt),A
    LD   (IX-12+CHP_CrTnSl),A
    LD   (IX-12+CHP_CrTnSl+1),A
    RET

C_ENGLS
    LD   A,(BC)
    INC  BC
    LD   (IY-100+VRS_Env_Del),A
    LD   (IY-100+VRS_CurEDel),A
    LD   A,(BC)
    INC  BC
    LD   L,A
    LD   A,(BC)
    INC  BC
    LD   H,A
    LD   (IY-100+VRS_ESldAdd),L
    LD   (IY-100+VRS_ESldAdd+1),H
    RET

C_DELAY
    LD   A,(BC)
    INC  BC
    LD   (IY-100+VRS_Delay),A
    LD   HL,VARS2+VRS_ModNum    ; if AlCo_TS
    BIT  1,(HL)
    RET  Z
    LD   (VARS1+VRS_Delay),A
    LD   (VARS1+VRS_DelyCnt),A
    LD   (VARS2+VRS_Delay),A
    RET
    
SETENV
    LD   (IX-12+CHP_Env_En),E
    LD   (IY-100+VRS_AYREGS+EnvTp),A
    LD   A,(BC)
    INC  BC
    LD   H,A
    LD   A,(BC)
    INC  BC
    LD   L,A
    ; SETENBS
    LD   (IY-100+VRS_EnvBase),L
    LD   (IY-100+VRS_EnvBase+1),H
    XOR  A
    LD   (IX-12+CHP_PsInOr),A
    LD   (IY-100+VRS_CurEDel),A
    LD   H,A
    LD   L,A
    ; SETESLD
    LD   (IY-100+VRS_CurESld),L
    LD   (IY-100+VRS_CurESld+1),H
    RET

SETORN
    ADD  A,A
    LD   E,A
    LD   D,0
    LD   (IX-12+CHP_PsInOr),D
    LD   L,(IY-100+VRS_OrnPtrs)
    LD   H,(IY-100+VRS_OrnPtrs+1)
    ADD  HL,DE
    LD   E,(HL)
    INC  HL
    LD   D,(HL)
    LD   L,(IY-100+VRS_MODADDR)
    LD   H,(IY-100+VRS_MODADDR+1)
    ADD  HL,DE
    LD   (IX-12+CHP_OrnPtr),L
    LD   (IX-12+CHP_OrnPtr+1),H
C_NOP
    RET

SETSAM
    ADD  A,A
    LD   E,A
    LD   D,0
    LD   L,(IY-100+VRS_SamPtrs)
    LD   H,(IY-100+VRS_SamPtrs+1)
    ADD  HL,DE
    LD   E,(HL)
    INC  HL
    LD   D,(HL)
    LD   L,(IY-100+VRS_MODADDR)
    LD   H,(IY-100+VRS_MODADDR+1)
    ADD  HL,DE
    LD   (IX-12+CHP_SamPtr),L
    LD   (IX-12+CHP_SamPtr+1),H
    RET

; All 16 addresses to protect from broken pt3 modules
SPCCOMS
    DEFW C_NOP
    DEFW C_GLISS
    DEFW C_PORTM
    DEFW C_SMPOS
    DEFW C_ORPOS
    DEFW C_VIBRT
    DEFW C_NOP
    DEFW C_NOP
    DEFW C_ENGLS
    DEFW C_DELAY
    DEFW C_NOP
    DEFW C_NOP
    DEFW C_NOP
    DEFW C_NOP
    DEFW C_NOP
    DEFW C_NOP

CHREGS
    PUSH IY
    POP  IX
    ADD  IX,DE
    XOR  A
    LD   (Ampl),A
    BIT  0,(IX+CHP_Flags)
    PUSH HL
    JP   Z,CH_EXIT
    ;DI                      ; Disable Ints for SP
    LD   (CSP_+1),SP
    LD   L,(IX+CHP_OrnPtr)
    LD   H,(IX+CHP_OrnPtr+1)
    LD   SP,HL
    POP  DE
    LD   H,A
    LD   A,(IX+CHP_PsInOr)
    LD   L,A
    ADD  HL,SP
    INC  A
OrnCP
    CP   D
    JR   C,CH_ORPS
OrnLD
    LD   A,E
CH_ORPS
    LD   (IX+CHP_PsInOr),A
    LD   A,(IX+CHP_Note)
    ADD  A,(HL)
    JP   P,CH_NTP
    XOR  A
CH_NTP
    CP   96
    JR   C,CH_NOK
    LD   A,95
CH_NOK
    ADD  A,A
    EX   AF,AF'
    LD   L,(IX+CHP_SamPtr)
    LD   H,(IX+CHP_SamPtr+1)
    LD   SP,HL
    POP  DE
    LD   H,0
    LD   A,(IX+CHP_PsInSm)
    LD   B,A
    ADD  A,A
SamClc2
    ADD  A,A
    LD   L,A
    ADD  HL,SP
    LD   SP,HL
    LD   A,B
    INC  A
SamCP
    CP   D
    JR   C,CH_SMPS
SamLD
    LD   A,E
CH_SMPS
    LD   (IX+CHP_PsInSm),A
    POP  BC
    POP  HL
    LD   E,(IX+CHP_TnAcc)
    LD   D,(IX+CHP_TnAcc+1)
    ADD  HL,DE
    BIT  6,B
    JR   Z,CH_NOAC
    LD   (IX+CHP_TnAcc),L
    LD   (IX+CHP_TnAcc+1),H
CH_NOAC
    EX   DE,HL
    EX   AF,AF'

    ld hl,NT_
    add l
    ld l,a
    adc h
    sub l
    ld h,a

    LD   SP,HL
    POP  HL
    ADD  HL,DE
    LD   E,(IX+CHP_CrTnSl)
    LD   D,(IX+CHP_CrTnSl+1)
    ADD  HL,DE
CSP_
    LD   SP,#0000
    ;EI                      ; Enable Ints
    EX   (SP),HL
    XOR  A
    OR   (IX+CHP_TSlCnt)
    JR   Z,CH_AMP
    DEC  (IX+CHP_TSlCnt)
    JR   NZ,CH_AMP
    LD   A,(IX+CHP_TnSlDl)
    LD   (IX+CHP_TSlCnt),A
    LD   L,(IX+CHP_TSlStp)
    LD   H,(IX+CHP_TSlStp+1)
    LD   A,H
    ADD  HL,DE
    LD   (IX+CHP_CrTnSl),L
    LD   (IX+CHP_CrTnSl+1),H
    BIT  2,(IX+CHP_Flags)
    JR   NZ,CH_AMP
    LD   E,(IX+CHP_TnDelt)
    LD   D,(IX+CHP_TnDelt+1)
    AND  A
    JR   Z,CH_STPP
    EX   DE,HL
CH_STPP
    SBC  HL,DE
    JP   M,CH_AMP
    LD   A,(IX+CHP_SlToNt)
    LD   (IX+CHP_Note),A
    XOR  A
    LD   (IX+CHP_TSlCnt),A
    LD   (IX+CHP_CrTnSl),A
    LD   (IX+CHP_CrTnSl+1),A
CH_AMP
    LD   A,(IX+CHP_CrAmSl)
    BIT  7,C
    JR   Z,CH_NOAM
    BIT  6,C
    JR   Z,CH_AMIN
    CP   15
    JR   Z,CH_NOAM
    INC  A
    JR   CH_SVAM
CH_AMIN
    CP   -15
    JR   Z,CH_NOAM
    DEC  A
CH_SVAM
    LD   (IX+CHP_CrAmSl),A
CH_NOAM
    LD   L,A
    LD   A,B
    AND  15
    ADD  A,L
    JP   P,CH_APOS
    XOR  A
CH_APOS
    CP   16
    JR   C,CH_VOL
    LD   A,15
CH_VOL
    OR   (IX+CHP_Volume)

    push de
    ld de,VT_
    ADD  A,e
    LD   L,A
    ADC  A,d
    SUB  L
    LD   H,A
    pop de

    LD   A,(HL)
CH_ENV
    BIT  0,C
    JR   NZ,CH_NOEN
    OR   (IX+CHP_Env_En)
CH_NOEN
    LD   (Ampl),A
    BIT  7,B
    LD   A,C
    JR   Z,NO_ENSL
    RLA
    RLA
    SRA  A
    SRA  A
    SRA  A
    ADD  A,(IX+CHP_CrEnSl)      ; See comment below
    BIT  5,B
    JR   Z,NO_ENAC
    LD   (IX+CHP_CrEnSl),A
NO_ENAC
    ADD  A,(IY-100+VRS_AddToEn) ; Bug in PT3 - Need word here
    LD   (IY-100+VRS_AddToEn),A
    JR   CH_MIX
NO_ENSL
    RRA
    ADD  A,(IX+CHP_CrNsSl)
    LD   (IY-100+VRS_AddToNs),A
    BIT  5,B
    JR   Z,CH_MIX
    LD   (IX+CHP_CrNsSl),A
CH_MIX
    LD   A,B
    RRA
    AND  #48
CH_EXIT
    OR   (IY-100+VRS_AYREGS+Mixer)
    RRCA
    LD   (IY-100+VRS_AYREGS+Mixer),A
    POP  HL
    XOR  A
    OR   (IX+CHP_COnOff)
    RET  Z
    DEC  (IX+CHP_COnOff)
    RET  NZ
    XOR  (IX+CHP_Flags)
    LD   (IX+CHP_Flags),A
    RRA
    LD   A,(IX+CHP_OnOffD)
    JR   C,CH_ONDL
    LD   A,(IX+CHP_OffOnD)
CH_ONDL
    LD   (IX+CHP_COnOff),A
    RET

PLAY_
    XOR  A
    LD   (IY-100+VRS_AddToEn),A
    LD   (IY-100+VRS_AYREGS+Mixer),A
    DEC  A
    LD   (IY-100+VRS_AYREGS+EnvTp),A
    DEC  (IY-100+VRS_DelyCnt)
    JP   NZ,PL2
    DEC  (IY-100+VRS_ChanA+CHP_NtSkCn)
    JR   NZ,PL1B
    LD   C,(IY-100+VRS_AdInPtA)
    LD   B,(IY-100+VRS_AdInPtA+1)
    LD   A,(BC)
    AND  A
    JR   NZ,PL1A
    LD   D,A
    LD   (IY-100+VRS_Ns_Base),A
    LD   L,(IY-100+VRS_CrPsPtr)
    LD   H,(IY-100+VRS_CrPsPtr+1)
    INC  HL
    LD   A,(HL)
    INC  A
    JR   NZ,PLNLP

    CALL CHECKLP

    LD   L,(IY-100+VRS_LPosPtr)
    LD   H,(IY-100+VRS_LPosPtr+1)
    LD   A,(HL)
    INC  A
PLNLP
    ; SETCPPT
    LD   (IY-100+VRS_CrPsPtr),L
    LD   (IY-100+VRS_CrPsPtr+1),H
    DEC  A
    BIT  1,(IY-100+VRS_ModNum)
    JR   Z,NoAlCo
TSSub    EQU $+1
    SUB  #D6
    CPL
NoAlCo
    ADD  A,A
    LD   E,A
    RL   D

    LD   A,L
    SUB  (IY-100+VRS_PosSub)
    LD   (IY-100+VRS_CurPos),A

    LD   L,(IY-100+VRS_PatsPtr)
    LD   H,(IY-100+VRS_PatsPtr+1)
    ADD  HL,DE
    LD   E,(IY-100+VRS_MODADDR)
    LD   D,(IY-100+VRS_MODADDR+1)
    ;DI                      ; Disable Ints for SP
    LD   (PSP_+1),SP
    LD   SP,HL
    POP  HL
    ADD  HL,DE
    LD   B,H
    LD   C,L
    POP  HL
    ADD  HL,DE
    LD   (IY-100+VRS_AdInPtB),L
    LD   (IY-100+VRS_AdInPtB+1),H
    POP  HL
    ADD  HL,DE
    LD   (IY-100+VRS_AdInPtC),L
    LD   (IY-100+VRS_AdInPtC+1),H
PSP_
    LD   SP,#0000
    ;EI                      ; Enable Ints
PL1A
    LD   DE,VRS_ChanA+12-100
    CALL PTDECOD
    LD   (IY-100+VRS_AdInPtA),C
    LD   (IY-100+VRS_AdInPtA+1),B

PL1B
    DEC  (IY-100+VRS_ChanB+CHP_NtSkCn)
    JR   NZ,PL1C
    LD   DE,VRS_ChanB+12-100
    LD   C,(IY-100+VRS_AdInPtB)
    LD   B,(IY-100+VRS_AdInPtB+1)
    CALL PTDECOD
    LD   (IY-100+VRS_AdInPtB),C
    LD   (IY-100+VRS_AdInPtB+1),B

PL1C
    DEC  (IY-100+VRS_ChanC+CHP_NtSkCn)
    JR   NZ,PL1D
    LD   DE,VRS_ChanC+12-100
    LD   C,(IY-100+VRS_AdInPtC)
    LD   B,(IY-100+VRS_AdInPtC+1)
    CALL PTDECOD
    LD   (IY-100+VRS_AdInPtC),C
    LD   (IY-100+VRS_AdInPtC+1),B

PL1D
    LD   A,(IY-100+VRS_Delay)
    LD   (IY-100+VRS_DelyCnt),A

PL2
    LD   DE,VRS_ChanA-100
    LD   L,(IY-100+VRS_AYREGS+TonA)
    LD   H,(IY-100+VRS_AYREGS+TonA+1)
    CALL CHREGS
    LD   (IY-100+VRS_AYREGS+TonA),L
    LD   (IY-100+VRS_AYREGS+TonA+1),H
Ampl     EQU $+1
    LD   A,#3E
    LD   (IY-100+VRS_AYREGS+AmplA),A
    LD   DE,VRS_ChanB-100
    LD   L,(IY-100+VRS_AYREGS+TonB)
    LD   H,(IY-100+VRS_AYREGS+TonB+1)
    CALL CHREGS
    LD   (IY-100+VRS_AYREGS+TonB),L
    LD   (IY-100+VRS_AYREGS+TonB+1),H
    LD   A,(Ampl)
    LD   (IY-100+VRS_AYREGS+AmplB),A
    LD   DE,VRS_ChanC-100
    LD   L,(IY-100+VRS_AYREGS+TonC)
    LD   H,(IY-100+VRS_AYREGS+TonC+1)
    CALL CHREGS
    LD   (IY-100+VRS_AYREGS+TonC),L
    LD   (IY-100+VRS_AYREGS+TonC+1),H
    LD   A,(Ampl)
    LD   (IY-100+VRS_AYREGS+AmplC),A

    LD   A,(IY-100+VRS_Ns_Base)
    ADD  (IY-100+VRS_AddToNs)
    LD   (IY-100+VRS_AYREGS+Noise),A

    LD   A,(IY-100+VRS_AddToEn)
    LD   E,A
    ADD  A,A
    SBC  A,A
    LD   D,A
    LD   L,(IY-100+VRS_EnvBase)
    LD   H,(IY-100+VRS_EnvBase+1)
    ADD  HL,DE
    LD   E,(IY-100+VRS_CurESld)
    LD   D,(IY-100+VRS_CurESld+1)
    ADD  HL,DE
    LD   (IY-100+VRS_AYREGS+Env),L
    LD   (IY-100+VRS_AYREGS+Env+1),H

    XOR  A
    OR   (IY-100+VRS_CurEDel)
    RET  Z
    DEC  (IY-100+VRS_CurEDel)
    RET  NZ
    LD   A,(IY-100+VRS_Env_Del)
    LD   (IY-100+VRS_CurEDel),A
    LD   L,(IY-100+VRS_ESldAdd)
    LD   H,(IY-100+VRS_ESldAdd+1)
    ADD  HL,DE
    ; SETESLD
    LD   (IY-100+VRS_CurESld),L
    LD   (IY-100+VRS_CurESld+1),H
    RET

; ---------------------------------------------------------------------------
; Play sound
; ---------------------------------------------------------------------------
TS_Play
    ; Proccess Music
    ; 1.- YM LEFT
    LD   IY,VARS1+100
    CALL PLAY_

    ld a,(snddatchn)
    cp 3
    jr z,TS_Play_ROUT

    ; 2.- PSG
    LD   IY,VARS2+100
    CALL PLAY_

TS_Play_ROUT
    jp 0
; ---------------------------------------------------------------------------

PT3EMPTYORN EQU $-1
    DEFB 1,0

; vars from here can be stripped (you can move VARS to any other address)
VARS
is_ts   DEFB 0          ; 0 = No Turbo Sound | 1 = Turbo Sound

; ChannelsVars

; reset group
CHP_PsInOr  equ  0       ;DEFB 0
CHP_PsInSm  equ  1       ;DEFB 0
CHP_CrAmSl  equ  2       ;DEFB 0
CHP_CrNsSl  equ  3       ;DEFB 0
CHP_CrEnSl  equ  4       ;DEFB 0
CHP_TSlCnt  equ  5       ;DEFB 0
CHP_CrTnSl  equ  6       ;DEFW 0
CHP_TnAcc   equ  8       ;DEFW 0
CHP_COnOff  equ 10       ;DEFB 0
; reset group

CHP_OnOffD  equ 11       ;DEFB 0

; IX for PTDECOD here (+12)
CHP_OffOnD  equ 12       ;DEFB 0
CHP_OrnPtr  equ 13       ;DEFW 0
CHP_SamPtr  equ 15       ;DEFW 0
CHP_NNtSkp  equ 17       ;DEFB 0
CHP_Note    equ 18       ;DEFB 0
CHP_SlToNt  equ 19       ;DEFB 0
CHP_Env_En  equ 20       ;DEFB 0
CHP_Flags   equ 21       ;DEFB 0
; Enabled - 0, SimpleGliss - 2
CHP_TnSlDl  equ 22       ;DEFB 0
CHP_TSlStp  equ 23       ;DEFW 0
CHP_TnDelt  equ 25       ;DEFW 0
CHP_NtSkCn  equ 27       ;DEFB 0
CHP_Volume  equ 28       ;DEFB 0

CHP_datlen  equ 29


VRS_ModNum  equ  0       ;DEFB 0          ; bit0 ChipNum
                         ;                  bit1 1-reversed patterns order (AlCo TS)

VRS_ChanA   equ 0*29+1   ;DEFS CHP_datlen
VRS_ChanB   equ 1*29+1   ;DEFS CHP_datlen
VRS_ChanC   equ 2*29+1   ;DEFS CHP_datlen

; GlobalVars
VRS_MODADDR equ  88      ;DEFW 0          ; Pointer to the song
VRS_OrnPtrs equ  90      ;DEFW 0
VRS_SamPtrs equ  92      ;DEFW 0
VRS_PatsPtr equ  94      ;DEFW 0
VRS_AdInPtA equ  96      ;DEFW 0
VRS_AdInPtB equ  98      ;DEFW 0
VRS_AdInPtC equ 100      ;DEFW 0
VRS_CrPsPtr equ 102      ;DEFW 0
VRS_LPosPtr equ 104      ;DEFW 0

VRS_Delay   equ 106      ;DEFB 0          ; Song Tempo
VRS_DelyCnt equ 107      ;DEFB 0
VRS_ESldAdd equ 108      ;DEFW 0
VRS_CurESld equ 110      ;DEFW 0
VRS_Env_Del equ 112      ;DEFB 0
VRS_CurEDel equ 113      ;DEFB 0
VRS_Ns_Base equ 114      ;DEFB 0
VRS_AddToNs equ 115      ;DEFB 0
VRS_AddToEn equ 116      ;DEFB 0
VRS_EnvBase equ 117      ;DEFW 0
VRS_AYREGS  equ 119      ;DEFS 14

VRS_CurPos  equ 133      ;DEFB 0
VRS_PosSub  equ 134      ;DEFB 0

VRS_datlen  equ 135


VARS1   DEFS VRS_datlen
VARS2   DEFS VRS_datlen

VT_     EQU $-16
    ; Volume table
    DEFB #00,#00,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01
    DEFB #00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#02,#02,#02,#02
    DEFB #00,#00,#00,#01,#01,#01,#01,#01,#02,#02,#02,#02,#02,#03,#03,#03
    DEFB #00,#00,#01,#01,#01,#01,#02,#02,#02,#02,#03,#03,#03,#03,#04,#04
    DEFB #00,#00,#01,#01,#01,#02,#02,#02,#03,#03,#03,#04,#04,#04,#05,#05
    DEFB #00,#00,#01,#01,#02,#02,#02,#03,#03,#04,#04,#04,#05,#05,#06,#06
    DEFB #00,#00,#01,#01,#02,#02,#03,#03,#04,#04,#05,#05,#06,#06,#07,#07
    DEFB #00,#01,#01,#02,#02,#03,#03,#04,#04,#05,#05,#06,#06,#07,#07,#08
    DEFB #00,#01,#01,#02,#02,#03,#04,#04,#05,#05,#06,#07,#07,#08,#08,#09
    DEFB #00,#01,#01,#02,#03,#03,#04,#05,#05,#06,#07,#07,#08,#09,#09,#0A
    DEFB #00,#01,#01,#02,#03,#04,#04,#05,#06,#07,#07,#08,#09,#0A,#0A,#0B
    DEFB #00,#01,#02,#02,#03,#04,#05,#06,#06,#07,#08,#09,#0A,#0A,#0B,#0C
    DEFB #00,#01,#02,#03,#03,#04,#05,#06,#07,#08,#09,#0A,#0A,#0B,#0C,#0D
    DEFB #00,#01,#02,#03,#04,#05,#06,#07,#07,#08,#09,#0A,#0B,#0C,#0D,#0E
    DEFB #00,#01,#02,#03,#04,#05,#06,#07,#08,#09,#0A,#0B,#0C,#0D,#0E,#0F

NT_Pointer  dw NT_

NT_
    ; ZX Note table
;    DEFW 4095,4095,4095,4095,4095,4095,4095,4095,4095,3977,3754,3543
    DEFW 3344,3157,2980,2812,2655,2506,2365,2232,2107,1989,1877,1772
    DEFW 1672,1578,1490,1406,1327,1253,1182,1116,1053,994,939,886
    DEFW 836,789,745,703,664,626,591,558,527,497,469,443
    DEFW 418,395,372,352,332,313,296,279,263,249,235,221
    DEFW 209,197,186,176,166,157,148,140,132,124,117,111
    DEFW 105,99,93,88,83,78,74,70,66,62,59,55
    DEFW 52,49,47,44,41,39,37,35,33,31,29,28
    DEFW 26,25,23,22,21,20,18,17,16,15,14,13 ; the last 3 should be 16, 15 and 14
;    DEFW 13,12,12,11,10,10,9,9,8,8,7,7
;    DEFW 7,6,6,5,5,5,5,4,4,4,4,3
;    DEFW 3,3,3,3,3,2,2,2,2,2,2,2

NT_ZX
    ; ZX Note table
;    DEFW 4095,4095,4095,4095,4095,4095,4095,4095,4095,3977,3754,3543
    DEFW 3344,3157,2980,2812,2655,2506,2365,2232,2107,1989,1877,1772
    DEFW 1672,1578,1490,1406,1327,1253,1182,1116,1053,994,939,886
    DEFW 836,789,745,703,664,626,591,558,527,497,469,443
    DEFW 418,395,372,352,332,313,296,279,263,249,235,221
    DEFW 209,197,186,176,166,157,148,140,132,124,117,111
    DEFW 105,99,93,88,83,78,74,70,66,62,59,55
    DEFW 52,49,47,44,41,39,37,35,33,31,29,28
    DEFW 26,25,23,22,21,20,18,17,16,15,14,13 ; the last 3 should be 16, 15 and 14
;    DEFW 13,12,12,11,10,10,9,9,8,8,7,7
;    DEFW 7,6,6,5,5,5,5,4,4,4,4,3
;    DEFW 3,3,3,3,3,2,2,2,2,2,2,2

NT_CPC
    ; CPC Note table (it's valid for ST, too)
    DEFW 3822/2,3608/2,3405/2,3214/2,3034/2,2863/2,2703/2,2551/2,2408/2,2273/2,2145/2,2025/2
    DEFW 1911/2,1804/2,1703/2,1607/2,1517/2,1432/2,1351/2,1276/2,1204/2,1136/2,1073/2,1012/2
    DEFW 956/2,902/2,851/2,804/2,758/2,716/2,676/2,638/2,602/2,568/2,536/2,506/2
    DEFW 478/2,451/2,426/2,402/2,379/2,358/2,338/2,319/2,301/2,284/2,268/2,253/2
    DEFW 239/2,225/2,213/2,201/2,190/2,179/2,169/2,159/2,150/2,142/2,134/2,127/2
    DEFW 119/2,113/2,106/2,100/2,95/2,89/2,84/2,80/2,75/2,71/2,67/2,63/2
    DEFW 60/2,56/2,53/2,50/2,47/2,45/2,42/2,40/2,38/2,36/2,34/2,32/2
    DEFW 30/2,28/2,27/2,25/2,24/2,22/2,21/2,20/2,19/2,18/2,17/2,16/2
;    DEFW 15,14,13,13,12,11,11,10,9,9,8,8
;    DEFW 7,7,7,6,6,6,5,5,5,4,4,4
;    DEFW 4,4,3,3,3,3,3,2,2,2,2,2
;    DEFW 2,2,2,2,1,1,1,1,1,1,1,1

VAR0END EQU VT_+16              ; TS_Init zeroes from VARS to VAR0END-1
; ---------------------------------------------------------------------------
pt3end
