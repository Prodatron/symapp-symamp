;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@                        SOUNDTRAKKER 128 ST2 PLUGIN                         @
;@                    (c) 2006-2021 by Prodatron/SymbiosiS                    @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;==============================================================================
;### API ROUTINEN #############################################################
;==============================================================================

;### ST2CHN -> detects number of channels
;### Input      HL=address
;### Output     A=number of channels (always 3)
st2chn  ld a,3
        ret

;### ST2INI -> Initialisiert ST2-Sound
;### Eingabe    HL=Adresse
;### Ausgabe    CF=0 -> ok, HL=Anzahl Positionen
;###            CF=1 -> nicht unterstütztes Format
;### Veraendert AF,BC,DE,HL,IX,IY
st2valbeg   dw 0    ;Beginn Playlist Track 0
st2vallen   dw 0    ;Songlänge*3

st2ini  ld hl,(sndmem)      ;*** Relocieren
        inc hl
        ld a,l
        or h
        jr z,st2ini1
        ld hl,-1        ;nur einmal relocieren und patchen
        ld (sndmem),hl
        ld hl,sndmem-128
        inc h
        call st2rel
        ld bc,sndmem        ;*** Code patchen (ld i,a/a,i -> push/pop af)
        ld ix,#18e+128
        add ix,bc
        ld (ix+#00+0),#f5
        ld (ix+#00+1),0
        ld (ix+#31+0),#f1
        ld (ix+#31+1),0
st2ini2 ld ix,#104+128
        add ix,bc
        ld bc,st2ply2
        ld (ix+0),#cd
        ld (ix+1),c
        ld (ix+2),b
st2ini1 ld bc,128+0         ;*** Sound resetten
        call st2ply1
        ld hl,(st2vallen)
        dec hl:dec hl:dec hl
        or a
        ld a,3
        ret

;### ST2VOL -> Setzt die globale Lautstärke
;### Eingabe    A=Lautstärke (0-255)
;### Verändert  AF
st2vol  ;...
        ret

;### ST2PLY -> Spielt initialisierten ST2-Sound (muß mit 50Hz aufgerufen werden)
;### Ausgabe    HL=Position, CF=1 -> Ende wurde erreicht
;### Veraendert AF,BC,DE,HL,IX,IY
st2plyr db 0
st2ply  ld bc,128+3
        call st2ply1
        ld hl,sndmem
        ld bc,128+#5cf+3
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld hl,(st2valbeg)
        ex de,hl
        or a
        sbc hl,de
        call envelopeInterrupt      ;!!!
        ld ix,st2plyr
        ld a,(ix+0)
        ld (ix+0),0
        sub 1
        ccf
        ret

st2ply1 ld hl,sndmem ;##!!##
        add hl,bc

        ld bc,st2ply3
        push bc
        di
        jp (hl)
st2ply3 ei
        ret

st2ply2 ex de,hl
        inc hl
        ld e,(hl)
        ld a,1
        ld (st2plyr),a
        ret

;### ST2STP -> Hält ST2-Sound an
;### Veraendert AF,BC,DE,HL,IX,IY
st2stp  ld bc,128+6
        jr st2ply1

;### ST2POS -> Setzt ST2-Sound auf eine bestimmte Position
;### Eingabe    HL=Position, Sound muß initialisiert sein
;### Veraendert AF,BC,DE,HL,IX,IY
st2pos  ld a,l
        or h
        ret z
        ld bc,-3
        ld de,-2
st2pos0 inc de
        add hl,bc
        jr c,st2pos0
        ld bc,sndmem
        ld hl,128+#4c4+6
        add hl,bc
        ld (hl),1
        inc hl
        ld (hl),1
        ld l,e
        ld h,d
        add hl,hl
        add hl,de
        ex de,hl        ;DE=(Position-1) * 3
        ld ix,#5cf+3+128
        call st2pos1
        ld ix,#5fd+3+128
        call st2pos1
        ld ix,#62b+3+128
st2pos1 add ix,bc
        ld l,(ix+0)
        ld h,(ix+1)
        add hl,de
        ld (ix+0),l
        ld (ix+1),h
        ret

;### ST2VAL -> Holt Lautstärke und Frequenz eines Kanales
;### Eingabe    A=Kanal (0-2)
;### Ausgabe    A=Lautstärke (0-15), L=Frequenz (0-15)
;### Verändert  F,BC,DE,HL
st2val  ld hl,#0800
        cp 1
        jr c,st2val1
        ld hl,#0902
        jr z,st2val1
        ld hl,#0a04
st2val1 ld c,l
        di
        call st2reg
        ld c,l
        inc c
        ld l,a
        call st2reg
        ld c,h
        ld h,a          ;HL=Frq
        call st2reg
        ei
        and 15          ;A=Volume
        push af
        ld a,l
        and 7
        add a
        ld l,a
        ld a,h
        rlca
        and 1
        or l
        ld l,a
        pop af
        ret


;==============================================================================
;### SOUNDTRAKKER 128 SUB-ROUTINEN ############################################
;==============================================================================

;### ST2REG -> Liest PSG-Register aus
;### Eingabe    C=Register
;### Ausgabe    A=Wert
;### Veraendert F,BC,DE
st2reg  jp cpcreg

;### ST2REL -> Relociert geladenes Soundtrakker 128 Modul
;### Eingabe    HL=Adresse
st2relnew   dw 0    ;neue Adresse
st2reldif   dw 0    ;Differenz
st2reltab
db #81,#40,#84,#40,#87,#40,#8B,#40,#8E,#40,#91,#40,#94,#40,#97,#40,#9A,#40
db #9E,#40,#A2,#40,#D6,#40,#E9,#40,#EC,#40,#F2,#40,#F8,#40,#06,#41,#0D,#41,#14,#41
db #1B,#41,#1E,#41,#23,#41,#26,#41,#29,#41,#2D,#41,#30,#41,#33,#41,#37,#41,#3A,#41
db #3D,#41,#40,#41,#43,#41,#48,#41,#4D,#41,#52,#41,#57,#41,#5A,#41,#67,#41,#6A,#41
db #6D,#41,#70,#41,#73,#41,#A5,#41,#BD,#41,#C0,#41,#C4,#41,#D0,#41,#E1,#41,#FB,#41
db #23,#42,#4F,#42,#B6,#42,#D4,#42,#30,#43,#50,#43,#57,#43,#7F,#43,#9B,#43,#AC,#43
db #BC,#43,#C0,#43,#DD,#43,#E3,#43,#05,#44,#0C,#44,#18,#44,#29,#44,#51,#44,#58,#44
db #5E,#44,#6B,#44,#72,#44,#7C,#44,#82,#44,#85,#44,#88,#44,#8E,#44,#99,#44,#A5,#44
db #B0,#44,#B5,#44,#B8,#44,#D7,#44,#DA,#44,#E2,#44,#F6,#44,#11,#45,#50,#45,#52,#45
db #54,#45,#56,#45,#58,#45,#5A,#45,#5C,#45,#5E,#45,#60,#45,#62,#45,#64,#45,#66,#45
db #68,#45,#6A,#45,#6C,#45,#BA,#46,#BC,#46,#BE,#46
dw #46c6+#80,#46ce+#80,#46d6+#80,#46de+#80
dw 0

st2rel  ld (st2relnew),hl   ;HL=Neue Adresse
        inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)           ;DE=Alte Adresse + 9
        ld bc,7
        add hl,bc           ;HL=Neue Adresse + 9
        sbc hl,de           ;HL=Differenz (neu-alt)
        ld (st2reldif),hl
        ld ix,st2reltab         ;*** Code relozieren
st2rel1 ld l,(ix+0)
        ld h,(ix+1)
        ld a,l
        or h
        jr z,st2rel2
        ld bc,-#4080
        add hl,bc
        ld bc,(st2relnew)
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld bc,(st2reldif)
        ex de,hl
        add hl,bc
        ex de,hl
        ld (hl),d
        dec hl
        ld (hl),e
        inc ix
        inc ix
        jr st2rel1
st2rel2 ld ix,(st2relnew)       ;*** Daten 2 relocieren
        ld bc,#063a
        add ix,bc
        ld l,(ix+2)
        ld h,(ix+3)
        ld e,(ix+0)
        ld d,(ix+1)
        ld (st2valbeg),de
        sbc hl,de
        ld c,l
        ld b,h              ;BC=Anzahl -> len*3
        ld (st2vallen),bc
        push de
        pop ix              ;IX=Anfang
        ld de,(st2reldif)
        jr st2rel4
st2rel3 ld l,(ix+1)
        ld h,(ix+2)
        add hl,de
        ld (ix+1),l
        ld (ix+2),h
        inc ix
        inc ix
        inc ix
        dec bc
st2rel4 ld a,c
        or b
        jr nz,st2rel3
        ld ix,(st2relnew)       ;*** Daten 1 relocieren
        ld bc,#0640
        add ix,bc
        ld b,80/4
st2rel5 call st2rel6
        call st2rel6
        call st2rel6
        inc ix
        inc ix
        djnz st2rel5
        ret
st2rel6 ld l,(ix+0)
        ld h,(ix+1)
        add hl,de
        ld (ix+0),l
        ld (ix+1),h
        inc ix
        inc ix
        ret
