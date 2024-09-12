;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@                    SURPRISE! ADLIB TRACKER 2 SA2 PLUGIN                    @
;@            (c) 200x by Madram (qdplay port for CPC-ISA project)            @
;@               (c) 2020 by PulkoMandy (Willy board adaption),               @
;@  (c) 2022 Prodatron (partially rewritten, effects added, SymAmp adaption)  @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;- position jump
;- pattern break with line>0

;missing
;- vibrato, release, special arpeggio instruments


OPL3LPT_SEL1    equ #FEBC   ;willy/opl3lpt (cpc only)
OPL3LPT_DATA1   equ #FEBD
OPL3LPT_SEL2    equ #FEBE
OPL3LPT_DATA2   equ #FEBD

OPL3MNS_SEL1    EQU #FFC4   ;moonsound (msx, cpc, ep)
OPL3MNS_DATA1   EQU #FFC5
OPL3MNS_SEL2    EQU #FFC6
OPL3MNS_DATA2   EQU #FFC7

opl3s1  dw OPL3LPT_SEL1     ;will be set by drivers.asm/op3set
opl3d1  dw OPL3LPT_DATA1
opl3s2  dw OPL3LPT_SEL2
opl3d2  dw OPL3LPT_DATA2

opl3wt  ld a,#ff
        in a,(#c4)
        and %11
        jr nz,$-6
        ret


;==============================================================================
;### API-ROUTINES #############################################################
;==============================================================================

;### SA2INI ->  Initializes SA2-Sound
;### Input      HL=address
;### Output     CF=0 -> ok, HL=number of song positions, A=number of channels
;###	        CF=1 -> not supported format
;### Destroyed  AF,BC,DE,HL,IX,IY
sa2init dw p_sa2_songlength+1,      sa2_songlength
        dw p_sa2_songloop+1,        sa2_songloop
        dw p_sa2_bpm+1,             sa2_bpm
        dw p_sa2_instruments_m15+1, sa2_instruments-15
        dw p_sa2_patternorder+1,    sa2_patternorder
        dw p_sa2_trackorder+1,      sa2_trackorder
        dw p_sa2_trackdata_m192+1,  sa2_trackdata-192
        dw 0

sa2inii db "SADT"

sa2ini	push hl
        ld de,sa2inii       ;check ID
        ld b,4
        call strcmp
        pop hl
        scf
        ret nz
        push hl
        inc h
        ld de,sndtxt_titl   ;copy titel
        ld bc,256*16+#d7
        call pt3ini2
        ld de,sndtxt_auth   ;copy author
        ld bc,256*16+1
        call pt3ini2
        pop de
        ld ix,sa2init       ;patch song access addresses
sa2ini1 ld c,(ix+0)
        ld b,(ix+1)
        ld a,c
        or b
        jr z,sa2ini2
        ld l,(ix+2)
        ld h,(ix+3)
        add hl,de
        ld a,l
        ld (bc),a
        inc bc
        ld a,h
        ld (bc),a
        ld bc,4
        add ix,bc
        jr sa2ini1
sa2ini2 ld bc,(opl3s2)      ;set to opl2 mode
        ld a,5
        out (c),a
        ld bc,(opl3d2)
        xor a
        out (c),a
        xor a
        ld (sa2_song_pos),a
        ld (sa2_song_end),a
        call sa2_init

        ld hl,(sa2_song_length)
        xor a
        ld h,a
        ld a,9
        ret

;### SA2VOL -> Set global volume
;### Input      A=Volume (0-255)
;### Destroyed  ?
sa2vol  ;...
        ret

;### SA2VAL -> Get volume and frequency for a specific channel
;### Input      A=Channel (0-8)
;### Output     A=Volume (0-15), L=Frequency (0-15)
;### Destroyed  F,BC,H
sa2val  add a:add a:add a:add a
        ld c,a
        ld b,0
        ld hl,chndat+chn_note
        add hl,bc
        ld a,(hl)
        inc hl
        ld c,(hl)
        and #1e
        rrca
        ld l,a
        ld a,c
        and 63
        cpl
        add 64
        srl a
        srl a
	    ret

;### MODPLY -> Plays SA2-Sound (called every 50Hz interrupt)
;### Output     HL=Position, CF=1 -> Song ended
;### Destroyed  AF,BC,DE,HL,IX,IY
sa2_song_end db 0           ;flag, if song end reached

sa2ply  call sa2_play
        ld hl,(sa2_song_pos)
        ld h,0
        ld a,(sa2_song_end)
        cp 1
        ccf
        ret

;### SA2STP -> Turn SA2-Sound off
;### Destroyed  AF,BC,DE,HL,IX,IY
sa2stp	jp sa2_stop

;### SA2POS -> Sets SA2-Sound on a specific location
;### Input      HL=Position, Sound must be initialized
;### Destroyed  AF,BC,DE,HL,IX,IY
sa2pos  ld a,(delay_bak)
        ld (delay),a
        dec a
        ld (d_count),a
        ld a,63
        ld (current_line0),a
        ld a,l
        dec a
        ld (sa2_song_pos),a
        ret


;==============================================================================
;### SA2-FUNCTIONS (INTERN) ###################################################
;==============================================================================


;
; Offsets du header sadt 
;

sa2_instruments     equ 5
sa2_patternorder    equ 966
sa2_songlength      equ 1096
sa2_songloop        equ 1097
sa2_restart         equ 1097
sa2_bpm             equ 1098
sa2_arpeggio        equ 1100
sa2_acommands       equ 1356
sa2_trackorder      equ 1612
sa2_channels        equ 2188
sa2_trackdata       equ 2190


sa2_init    
; Init variables

        ld hl,defaut
        ld de,variable
        ld bc,defaut-variable
        ldir
        ld a,(delay)
        ld (delay_bak),a
            
p_sa2_songlength
        ld a,(0)    ;(file+sa2_songlength)
        ld (sa2_song_length),a
p_sa2_songloop
        ld a,(0)    ;(file+sa2_songloop)
        ld (sa2_song_loop),a
p_sa2_bpm
        ld a,(0)    ;(file+sa2_bpm)
        call ef_effect_set_speed

        xor a
        call init_segments

; Registres generaux

        ld a,#01 : ld l,#20 : call Setreg
        ld a,#08 : ld l,#40 : call Setreg
        ld a,#BD : ld l,#C0 : call Setreg

        or a            ; Carry a 0 = init ok
        ret

sa2_play

; Nouvelle ligne ?

        ld a,(d_count) : or a : jp nz,effects_tick

        ld ix,chndat
        ld bc,#900
Pl_lp
        push bc
;
        ld a,(IX+chn_active)
        or a
        jr z,do_nothing

; On lit donnees fraiches
        ld l,(IX+chn_tradr+0)           ;adr pattern
        ld h,(IX+chn_tradr+1)
        ld a,(hl)                       ;Note
        and #fe                         ;On ecarte bit instrument
        jr z,effects
        ld (IX+chn_note),a              ;On stocke pour effets

; Conversion note frequence
        ex de,hl
        ld l,(ix+chn_freq+0)
        ld h,(ix+chn_freq+1)
        ld (ix+chn_freq_old+0),l
        ld (ix+chn_freq_old+1),h
        call frqset
        ex de,hl

; On isole instrument
        ld e,(hl) : inc hl : ld a,(hl)
        rr e : rra : srl a : srl a : srl a
        ld d,1              ;d=1 -> use ins volume
        jr nz,ins_found
        ld d,0              ;d=0 -> use cur volume
        ld a,(IX+chn_instr) ;on recupere no ins precedent
ins_found       
        ld (IX+chn_instr),a
        ld e,a
        ld a,(hl)
        and #f
        cp 3
        jr z,effects        ;skip ins set, if tone portamento
        cp 5
        jr z,effects
        ld a,e
        push de
        ld de,15:call Multi       ;Sacre Gilles
p_sa2_instruments_m15
        ld de,0     ;file+sa2_instruments-15
        add hl,de
        pop de
        pop bc                  ;Recupere canal
        push bc                 
        call SetINS             ;On envoie direct
        scf
        ld a,4

effects
        push af
        ld l,(IX+chn_tradr+0)           ;adr pattern
        ld h,(IX+chn_tradr+1)
        inc hl: ld a,(hl) : inc hl
        and #f
        add a
        ld b,0 : ld c,a
        ld a,(hl)                       ;A=parametre
        ld hl,effect_calls : add hl,bc
        ld e,(hl) : inc hl : ld d,(hl)
        ld bc,effect_ret : push bc
        ex de,hl : jp (hl)
effect_ret
        pop af

        pop bc
        push bc
        call c,Send_SND   

do_nothing
        ld bc,chn_len
        add ix,bc
        pop bc

        inc c
        dec b
        jp nz,Pl_lp
        jr ticknext

effects_tick

        ld ix,chndat
        ld bc,#900
Ef_lp
        push bc
;
        ld a,(IX+chn_active)
        or a
        jr z,ef_do_nothing

;
; Meme demarche que precedemment
; 

        ld l,(IX+chn_tradr+0)           ;adr pattern
        ld h,(IX+chn_tradr+1)
        inc hl                          ;on saute note
        ld a,(hl) : inc hl
        and #f : rlca

        ld d,0 : ld e,a
        ld a,(hl)                       ;parametre                      
        ld hl,effect_tick_calls : add hl,de
        ld e,(hl) : inc hl : ld d,(hl)
        ex de,hl
        ld de,ef_do_nothing : push de
        jp (hl)

ef_do_nothing
        ld bc,chn_len
        add ix,bc
        pop bc
        inc c
        djnz Ef_lp

;
; Avancee de la pattern
;
ticknext
        ld a,(arppos)
        inc a
        cp 3
        jr c,arp_inc
        xor a
arp_inc ld (arppos),a

        ld a,(d_count)
        inc a
        ld hl,delay
        cp (hl)
        jr nc,avancee
        ld (d_count),a
        ret

avancee
        xor a:ld (d_count),a
        
        ld a,(current_line0)
        inc a : and 63
        ld (current_line0),a
        jr z,Next_Ptn

        ld hl,chndat+chn_tradr
        ld bc,chn_len
        ld a,9
avanc_lp
        ld e,(hl):inc hl
        ld d,(hl)
        inc de:inc de:inc de
        ld (hl),d:dec hl
        ld (hl),e
        add hl,bc
        dec a
        jr nz,avanc_lp
        ret

Next_Ptn
        ld a,(sa2_song_pos)
        inc a
        ld hl,sa2_song_length
        cp (hl)
        jr nz,NP_ok
        ld a,1
        ld (sa2_song_end),a
        ld a,(sa2_song_loop)
NP_ok
        ld (sa2_song_pos),a

init_segments
        
        ld b,0
        ld c,a
p_sa2_patternorder
        ld hl,0     ;file+sa2_patternorder
        add hl,bc
        ld a,(hl)

        ld h,b    ; on multiplie par neuf
        ld l,a
        ld c,a
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,bc
        
p_sa2_trackorder
        ld bc,0     ;file+sa2_trackorder
        add hl,bc

        ld ix,chndat
        ld b,9
        
set_track
        ld a,(hl) 
        ld (ix+chn_active),a    ;set active track (0=inactive, 1-x=track)
        or a
        jr z,st_noactive        ;not active
        
        push bc
        push hl
        ld de,192
        call Multi
p_sa2_trackdata_m192
        ld de,0     ;file+sa2_trackdata-192
        add hl,de
        ld (ix+chn_tradr+0),l
        ld (ix+chn_tradr+1),h
        pop hl
        pop bc

st_noactive
        ld de,chn_len
        add ix,de
        inc hl
        djnz set_track

        ret

effect_calls
        dw ef_arpeggio                  ;0
        dw ef_slide_up                  ;1
        dw ef_slide_down                ;2
        dw ef_portamento                ;3
        dw ef_vibrato                   ;4 N/A
        dw ef_protamento_volume_slide   ;5
        dw ef_volume_slide              ;6  ef_vibrato_volume_slide (vibrato missing)
        dw ef_empty                     ;7
        dw ef_release_note              ;8 N/A
        dw ef_empty                     ;9
        dw ef_volume_slide              ;A
        dw ef_position_jump             ;B N/A
        dw ef_set_volume                ;C
        dw ef_pattern_break             ;D (line missing)
        dw ef_empty                     ;E
        dw ef_effect_set_speed          ;F

effect_tick_calls
        dw et_arpeggio                  ;0
        dw et_slide_up                  ;1
        dw et_slide_down                ;2
        dw et_portamento                ;3
        dw et_vibrato                   ;4
        dw et_protamento_volume_slide   ;5
        dw et_volume_slide              ;6  et_vibrato_volume_slide
        dw ef_empty                     ;7
        dw et_release_note              ;8
        dw ef_empty                     ;9
        dw et_volume_slide              ;A
        dw ef_empty                     ;B
        dw ef_empty                     ;C
        dw ef_empty                     ;D
        dw ef_empty                     ;E
        dw ef_empty                     ;F

et_arpeggio                  ;0
        or a
        ret z
        push bc
        call et_arpeggio0
        pop bc

et_arpeggio3
        ld b,(IX+chn_freq+0)
        ld a,#a0                ;Frequence (low 8 bits)
        call setreg_type0
        ld b,(IX+chn_freq+1)
        ld a,#b0                ;Key On / Octave / Frequence (high 2 bits)
        set 5,b
        jp setreg_type0       

et_arpeggio0
        ld e,a
        ld a,(arppos)
        cp 1
        jr c,et_arpeggio2
        ld a,e
        jr nz,et_arpeggio1
        rrca:rrca:rrca:rrca
et_arpeggio1
        and #f
et_arpeggio2
        add a
        add (ix+chn_note)
        jp frqset

et_slide_up                  ;1
        push bc
        ld c,a
        ld b,0
et_slide_up0
        ld l,(ix+chn_freq+0)
        ld h,(ix+chn_freq+1)
        call frqadd
        ld (ix+chn_freq+0),l
        ld (ix+chn_freq+1),h
        pop bc
        jr et_arpeggio3

et_slide_down                ;2
        push bc
        neg
        ld c,a
        ld b,-1
        jr et_slide_up0

et_portamento                ;3
        push bc
        call ef_portamento1
        pop bc
        jr et_arpeggio3

et_vibrato                   ;4
        ret

et_protamento_volume_slide   ;5
        call et_volume_slide
        jr et_portamento

et_vibrato_volume_slide      ;6
        ret

et_release_note              ;8
        ret

et_volume_slide              ;A
        ld a,(ix+chn_volsld)
        push bc
        ld e,a
        call et_volume_slide4
        inc ix
        call et_volume_slide4
        dec ix
        pop bc
        jp Send_SND_vol
et_volume_slide4
        ld a,(ix+chn_vol1)
        ld d,a
        and 63
        add e
        cp 64
        bit 7,e
        jr c,et_volume_slide5
        jr nz,et_volume_slide6
        ld a,63
et_volume_slide5
        ld e,a
        ld a,d
        and #c0
        or e
        ld (ix+chn_vol1),a
        ret
et_volume_slide6
        xor a
        jr et_volume_slide5

ef_empty                    ;7/9/E
        ret

ef_arpeggio                 ;0  ARPEGGIO
        or a
        ret z
        call et_arpeggio0
        ld c,2
        jp ef_send_snd

ef_slide_up                 ;1
        ret

ef_slide_down               ;2
        ret

ef_portamento               ;3
        or a
        jr z,ef_portamento6
        ld (ix+chn_plast),a
ef_portamento6
        call ef_portamento0
        ld c,2
        jp ef_send_snd

ef_portamento0
        ld l,(ix+chn_freq_old+0)
        ld h,(ix+chn_freq_old+1)
        ld a,l
        or h
        jr z,ef_portamento1     ;keep current frequency and destination
        ld (ix+chn_freq_old+0),0
        ld (ix+chn_freq_old+1),0
        ld e,(ix+chn_freq+0)
        ld d,(ix+chn_freq+1)    ;de=new destination
        ld (ix+chn_freq+0),l
        ld (ix+chn_freq+1),h    ;restore current frequency
        ld (ix+chn_freq_new+0),e
        ld (ix+chn_freq_new+1),d
        jr ef_portamento2
ef_portamento1
        ld e,(ix+chn_freq_new+0)
        ld d,(ix+chn_freq_new+1)
        ld l,(ix+chn_freq+0)
        ld h,(ix+chn_freq+1)
ef_portamento2                  ;hl=cur, de=new
        push hl
        or a
        sbc hl,de
        pop hl
        ret z                   ;cur=new -> finish
        jr nc,ef_portamento4
        ld c,(ix+chn_plast)     ;cur<new -> slide up
        ld b,0
        push de
        call frqadd
        pop de
        push hl
        or a
        sbc hl,de
        pop hl
        jr c,ef_portamento3
ef_portamento5
        ld l,e
        ld h,d
ef_portamento3
        ld (ix+chn_freq+0),l
        ld (ix+chn_freq+1),h    ;set updated cur frq
        ret
ef_portamento4                  ;cur>new slide down
        ld a,(ix+chn_plast)
        neg
        ld c,a
        ld b,-1
        push de
        call frqadd
        pop de
        push hl
        or a
        sbc hl,de
        pop hl
        jr nc,ef_portamento3
        jr ef_portamento5

ef_vibrato                  ;4
        ret

ef_protamento_volume_slide  ;5
        call ef_portamento0
        call ef_volume_slide
        ld c,2
        jr ef_send_snd

ef_vibrato_volume_slide     ;6
        ret

ef_release_note             ;8
        ret

ef_volume_slide             ;A
        ld e,a
        and #f0
        jr z,ef_volume_slide2
        rrca:rrca:rrca:rrca     ;slide up (=neg value)
        neg
        jr ef_volume_slide3
ef_volume_slide2
        ld a,e                  ;slide down (=pos value)
        and #f
ef_volume_slide3
        ld (ix+chn_volsld),a
        ret

ef_position_jump            ;B
        ret

ef_set_volume               ;C  SET VOLUME
        cp 64
        jr c,ef_set_volume1
        ld a,63
ef_set_volume1
        cpl
        add 64      ;0-63 -> 63-0
        ld e,a
        ld a,(ix+chn_vol1)
        and #c0
        or e
        ld (ix+chn_vol1),a
        ld a,(ix+chn_vol2)
        and #c0
        or e
        ld (ix+chn_vol2),a
        ld c,1
ef_send_snd         ;send sound
        pop hl
        pop af
        or c
        scf
        push af
        jp (hl)

ef_pattern_break            ;D
        ld a,63 ; ne g}re pas le param}tre pour l'instant !!!
        ld (current_line0),a
        ret

ef_effect_set_speed         ;F
        cp #20
        jr c,ef_effect_set_speed1
        ld e,a
        ld hl,750
        call div168
ef_effect_set_speed1
        ld (delay),a

        ret

;A=type (+4=full, +1=vol, +2=frq)
Send_SND
        cp 4
        jr nc,Send_SND_full
        bit 0,a
        push af
        call nz,Send_SND_vol
        pop af
        bit 1,a
        ret z
        jr Send_SND_frq0

Send_SND_full
; Envoie des notes   Entree c = canal
        call Send_SND_frq

Send_SND_vol
        ld e,(ix+chn_vol1)      ;volume
        ld a,#40
        call setreg_type21

        ld e,(ix+chn_vol2)
        ld a,#43
        jp setreg_type21

Send_SND_frq
        ld b,(IX+chn_freq+0)
        ld a,#a0                ;Frequence (low 8 bits)
        call setreg_type0

        ld b,(IX+chn_freq+1)
        ld a,#b0                ;Key On / Octave / Frequence (high 2 bits)
        res 5,b
        call setreg_type0
        jr Send_SND_frq1

Send_SND_frq0
        ld b,(IX+chn_freq+0)
        ld a,#a0                ;Frequence (low 8 bits)
        call setreg_type0

        ld b,(IX+chn_freq+1)
Send_SND_frq1
        ld a,#b0                ;Key On / Octave / Frequence (high 2 bits)
        set 5,b
        jp setreg_type0


SetINS
;
; Entree = hl, D=1 use ins vol
;
        ld a,#c0                ; Feedback / Connection
        call setreg_type1
        
        ld a,#20                ; Amp mod / Vibrato / EG type / Key Scaling / Multiple
        call setreg_type2
        ld a,#23                ; Idem pour operateur 2
        call setreg_type2

        ld a,#60                ; Decay / Attack
        call setreg_type2
        ld a,#63                ; Op 2
        call setreg_type2

        ld a,#80                ; Sustain / Release
        call setreg_type2
        ld a,#83
        call setreg_type2

        ld a,#E0                ; Wave select
        call setreg_type2
        ld a,#E3
        call setreg_type2

        dec d
        ret nz

        ld a,(hl)
        ld (ix+chn_vol1),a
        inc hl
        ld a,(hl)
        ld (ix+chn_vol2),a

        ret     

Setreg  ;Place L dans registre opl designe par A 

        LD BC,(opl3s1)
        OUT (C),A
        INC C
        call opl3wt
        OUT (C),L
        ;ld b,5                 ; A regler finement si player devient trop lent
        ;call tempo
        ret

setreg_type0    ;Place B dans REG + no canal 

        ; REG = A+C
        ; VAL = B
        PUSH HL
        PUSH BC
        LD L,B
        add a,c

        ; REG = A
        ; VAL = L

        LD BC,(opl3s1)
        OUT (C),A
        INC C
        call opl3wt
        OUT (C),L

        POP BC
        POP HL
        ret


;C=subreg, A=register offset (#x0), (HL)=value -> HL=HL+1
setreg_type1    ;Place donnee dans REG + no canal (reg C)

        add a,c
        PUSH BC
        LD BC,(opl3s1)
        OUT (C),A
        inc c
        call opl3wt
        ld a,(hl)
        OUT (C),a
        inc hl
        POP BC
        ret

;C=channel (0-8), A=register offset (#x0/3), (HL)=value -> HL=HL+1
setreg_type2    ;Place donnee dans REG (d'apres offsets operateurs)

        ld e,(hl)
        inc hl
setreg_type21       ;E=value
        push hl
        ld hl,op_offset
        ld b,0
        add hl,bc
        ld b,(hl)
        pop hl

        add a,b

        PUSH BC
        LD BC,(opl3s1)
        OUT (C),A

        INC C
        call opl3wt
        OUT (C),e
        POP BC
        ret




sa2_stop
        ld b,256-#20
        ld e,#20
SS_lp
        PUSH BC
        LD BC,(opl3s1)
        OUT (C),e
        inc C
        inc e
        call opl3wt
        db #ed,#71      ;out (c),0
        POP BC
        djnz SS_lp
        ret

; HL=DE*A
Multi   ld b,8
        ld hl,0
Mulbouc rra
        jr nc,Mulsaut
        add hl,de
Mulsaut sla e
        rl d
        djnz Mulbouc
        ret


;### FRQSET -> sets frequency
;### Input      A=note*2
;### Destroyed  AF,BC,HL
frqset  ld c,a
        ld b,0
        ld hl,NoteTab          
        add hl,bc
        ld a,(hl)               
        ld (IX+chn_freq+0),a
        inc hl
        ld a,(hl)
        ld (IX+chn_freq+1),a
        ret

;### FRQADD -> adds/subs frequency value
;### Input      HL=current frq, BC=dif (-#100<BC<+#100)
;### Output     HL=new frq
;### Destroyed  AF,BC,DE
frqadd  ld a,h
        and %00111100
        ld e,a              ;e=key/octave
        ld a,h
        and 3
        ld h,a              ;hl=frq
        bit 7,b
        jr nz,frqadd4
        add hl,bc       ;*POS*
        ld c,e
        ex de,hl            ;de=new frq
frqadd1 ld hl,647
        or a
        sbc hl,de
        jr nc,frqadd3
        ld a,c
        cp %00111100
        jr nz,frqadd2
        ld hl,#3c00+647     ;too high -> use max oct/frq
        ret
frqadd2 add 4               ;next octave
        ld c,a
        srl d:rr e
        jr frqadd1
frqadd3 ld l,e
        ld a,c
        or d
        ld h,a
        ret
frqadd4 add hl,bc       ;*NEG*
        ld c,e
        ex de,hl            ;de=new frq
frqadd5 ld hl,343
        or a
        sbc hl,de
        jr c,frqadd3
        ld a,c
        cp %00100000
        jr nz,frqadd6
        ld hl,#2000+343     ;too low -> use min oct/frq
        ret
frqadd6 sub 4               ;prev octave
        ld c,a
        sla e:rl d
        jr frqadd5


op_offset
        db 0,1,2,      8,9,10,         16,17,18
        ;        3,4,5,       11,12,13,         19,20,21

Okt1 equ %0010000000000000
Okt2 equ %0010010000000000
Okt3 equ %0010100000000000
Okt4 equ %0010110000000000
Okt5 equ %0011000000000000
Okt6 equ %0011010000000000
Okt7 equ %0011100000000000
Okt8 equ %0011110000000000


NoteTab dw 0 ;dummy note
        dw Okt1+343,Okt1+363,Okt1+385,Okt1+408,Okt1+432,Okt1+458
        dw Okt1+485,Okt1+514,Okt1+544,Okt1+577,Okt1+611,Okt1+647
        dw Okt2+343,Okt2+363,Okt2+385,Okt2+408,Okt2+432,Okt2+458
        dw Okt2+485,Okt2+514,Okt2+544,Okt2+577,Okt2+611,Okt2+647
        dw Okt3+343,Okt3+363,Okt3+385,Okt3+408,Okt3+432,Okt3+458
        dw Okt3+485,Okt3+514,Okt3+544,Okt3+577,Okt3+611,Okt3+647
        dw Okt4+343,Okt4+363,Okt4+385,Okt4+408,Okt4+432,Okt4+458
        dw Okt4+485,Okt4+514,Okt4+544,Okt4+577,Okt4+611,Okt4+647
        dw Okt5+343,Okt5+363,Okt5+385,Okt5+408,Okt5+432,Okt5+458
        dw Okt5+485,Okt5+514,Okt5+544,Okt5+577,Okt5+611,Okt5+647
        dw Okt6+343,Okt6+363,Okt6+385,Okt6+408,Okt6+432,Okt6+458
        dw Okt6+485,Okt6+514,Okt6+544,Okt6+577,Okt6+611,Okt6+647
        dw Okt7+343,Okt7+363,Okt7+385,Okt7+408,Okt7+432,Okt7+458
        dw Okt7+485,Okt7+514,Okt7+544,Okt7+577,Okt7+611,Okt7+647
        dw Okt8+343,Okt8+363,Okt8+385,Okt8+408,Okt8+432,Okt8+458
        dw Okt8+485,Okt8+514,Okt8+544,Okt8+577,Okt8+611,Okt8+647
        dw Okt8+647,Okt8+647,Okt8+647,Okt8+647  ; Pourquoi cette saturation ?
        dw Okt8+647,Okt8+647,Okt8+647,Okt8+647
        dw Okt8+647,Okt8+647,Okt8+647,Okt8+647
        dw Okt8+647,Okt8+647,Okt8+647,Okt8+647

chn_active      equ 0   ;1B active track (0=not active)
chn_instr       equ 1   ;1B last used instument
chn_note        equ 2   ;1B note (normal) \
chn_vol1        equ 3   ;1B volume 1       |-keep together
chn_vol2        equ 4   ;1B volume 2      /
chn_volsld      equ 5   ;1B volume slide (-15 -> +15)
chn_plast       equ 6   ;1B last portamento

chn_freq        equ 8   ;1W frequency
chn_freq_old    equ 10  ;1W frequency old (restore old for portamento)
chn_freq_new    equ 12  ;1W frequency new (destination for portamento)
chn_tradr       equ 14  ;1W track address

chn_len     equ 16
chndat      ds 9*chn_len    ;channel data


; Variables a nous

arppos      db 0    ;arpeggio position (0-2)
delay       db 0    ; Meme role que current_speed, mais en global
delay_bak   db 0
sa2_song_length db 0
sa2_song_loop db 0  


variable

; Attention les valeurs chargees seront celles suivant 'defaut', ou fixees
; d'apres le header sadt.

d_count db 0            ; equivalent du s_count 
current_line0 db 0
sa2_song_pos db 0

defaut
        db 0, 0,0
