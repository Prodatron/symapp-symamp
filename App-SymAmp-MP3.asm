;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@                                 MP3 PLUGIN                                 @
;@                    (c) 2007-2021 by Prodatron/SymbiosiS                    @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;- id3v2 tags
;- variable bitrate -> zeit erkennen, aktuelle anzeigen


;==============================================================================
;### API ROUTINES #############################################################
;==============================================================================

;### MP3DRV -> Sets MP3 hardware driver and tests and initialises the hardware
;### Input      A=type (0=dummy, 1=CPC, 2=MSX, 3=PCW, 4=EP)
;### Output     CF=0 -> ok, CF=1 -> hardware not detected
;### Destroyed  AF,BC,DE,HL,IX,IY
mp3drvadr   dw mp3plya+1
            dw mp3plyb+1
            dw mp3vol+1
            dw mp3vala+1
            dw mp3posa+1
mp3drvtab   dw m3tchk,m3tsnd,m3tvol,m3tspe,m3tset,m3tres    ;dummy
            dw m3cchk,m3csnd,m3mvol,m3cspe,m3cset,m3cres    ;cpc
            dw m3mchk,m3msnd,m3mvol,m3mspe,m3mset,m3mres    ;msx
            dw m3tchk,m3tsnd,m3tvol,m3tspe,m3tset,m3tres    ;pcw (=dummy)
            dw m3mchk,m3msnd,m3mvol,m3mspe,m3mset,m3mres    ;ep  (=msx)
mp3drvnum   equ 5
mp3drvmax   equ 5


mp3drv  cp mp3drvmax
        ccf
        ret c
        add a
        add a
        ld l,a
        add a
        add l
        ld l,a
        ld h,0
        ld bc,mp3drvtab
        add hl,bc
        ld ix,mp3drvadr
        ld a,mp3drvnum
mp3drv1 ld e,(ix+0)
        ld d,(ix+1)
        ldi
        ldi
        inc ix
        inc ix
        dec a
        jr nz,mp3drv1
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        jp (hl)

;### MP3INI -> Init MP3 sound
;### Input      HL=address
;### Output     CF=0 -> ok, HL=number of positions
;###            CF=1 -> format is not supported
;### Destroyed  AF,BC,DE,HL,IX,IY
mp3ini  ld a,(mp3filhnd)
        inc a
        jp nz,mp3ini9       ;** MP3 already open -> just set pos to begin
        push hl
        call mp3del
        pop hl
        ld a,(App_BnkNum)    ;** open MP3 file
        db #dd:ld h,a
        ld hl,sndmem
        call SyFile_FILOPN
        ret c
        ld (mp3filhnd),a
        ld ix,-128          ;** get length of file
        ld iy,-1
        ld c,2
        call SyFile_FILPOI
        jp c,mp3del
        db #fd:ld a,h
        db #fd:ld d,l
        db #dd:ld e,h
        ex de,hl            ;A,HL=length/256
        ld b,4
mp3ini1 rrca
        rr h
        rr l
        djnz mp3ini1        ;A[0-3],HL=length/4096
        ld c,1
mp3ini2 and 15
        jr z,mp3ini3
        rrca
        rr h
        rr l
        sla c
        jr mp3ini2
mp3ini3 ld a,c
        ld (mp3filgrd),a
        ld (mp3fillen),hl
        ld a,(App_BnkNum)    ;** load ID3V1 tag (last 128bytes of a MP3)
        ld e,a
        ld bc,128
        ld hl,mp3id3v1
        ld (hl),b
        ld a,(mp3filhnd)
        call SyFile_FILINP
        call mp3id1
        ld ix,0                 ;back to file start
        ld iy,0
        ld c,0
        ld a,(mp3filhnd)
        call SyFile_FILPOI
        jp c,mp3del
        ld hl,sndmem        ;** get MP3 properties
        inc h
        push hl
        ld a,(App_BnkNum)
        ld e,a
        ld bc,sndmax-256
        ld a,(mp3filhnd)
        call SyFile_FILINP      ;load first part of the MP3
        pop hl
        jp c,mp3del
mp3ini4 ld a,c                  ;search for frame begin
        or b
        jp z,mp3del
        ld a,-1
        cpir
        jp nz,mp3del
        ld a,(hl)
        cp #e0
        jr c,mp3ini4
        push hl
        ld b,a                  ;extract MP3 information
        rrca:rrca:rrca
        and 3
        ld (mp3prpver),a        ;0=mpeg2.5, 1=reserved, 2=mpeg2, 3=mpeg1
        ld a,b
        rrca
        and 3                   ;0=reserved, 1=layer3, 2=layer2, 3=layer1
        ld (mp3prplay),a
        neg
        add 4+"0"
        ld (propertxt22+15),a
        inc hl
        ld a,(hl)
        rrca:rrca
        ld c,a
        rrca:rrca
        and 15
        ld (mp3prpbrt),a        ;bit rate
        ld a,c
        and 3
        ld (mp3prpsrt),a        ;sample rate
        inc hl
        ld a,(hl)
        rlca:rlca
        and 3                   ;0=stereo, 1=joint stereo, 2=dual channel (two mono channels), 3=single channel (mono)
        add a
        ld l,a
        ld h,0
        ld bc,mp3modtxt
        add hl,bc
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,propertxt23+15
        ld bc,12
        ldir
        pop hl
        dec hl
        ld bc,sndmem
        sbc hl,bc
        dec h
        ld (mp3fildat),hl   ;** store MP3 data startposition (not needed yet)
        ld a,(mp3prpver)    ;** write Mpeg version
        add a
        ld l,a
        ld h,0
        ld bc,mp3vertxt
        add hl,bc
        ld a,(hl)
        ld (propertxt22+5),a
        inc hl
        ld a,(hl)
        ld (propertxt22+7),a
        ld a,(mp3prpver)    ;** get bitrate (0=mpeg2.5, 1=reserved, 2=mpeg2, 3=mpeg1)
        sub 3
        jr nc,mp3ini5
        ld a,3
mp3ini5 ld c,a
        ld a,(mp3prplay)        ;0=reserved, 1=layer3, 2=layer2, 3=layer1
        ld b,a
        ld a,3
        sub b
        add c
        cp 5
        jr c,mp3ini6
        ld a,4                  ;A=tabidex
mp3ini6 ld c,a
        ld a,(mp3prpbrt)
        ld b,a
        add a
        add a
        add b
        add c
        ld c,a
        ld b,0
        ld hl,mp3brttab
        add hl,bc
        ld a,(hl)               ;A=bitrate/2
        ld (mp3prpbre),a
        ld l,a
        ld h,0
        add hl,hl
        call clcdz3
        ld (prgwintxt3+1),hl
        ld (propertxt22+19),hl
        ld (prgwintxt3),a
        ld (propertxt22+18),a
        xor a
        ld (prgwintxt3+3),a
        ld a,(mp3prpver)    ;** get samplerate (0=mpeg2.5, 1=reserved, 2=mpeg2, 3=mpeg1)
        cp 2
        ld c,2
        jr c,mp3ini7
        ld c,1
        jr z,mp3ini7
        dec c
mp3ini7 ld a,(mp3prpsrt)
        ld b,a
        add a
        add b
        add c
        ld c,a
        ld b,0
        ld hl,mp3srttab
        add hl,bc
        ld a,(hl)               ;A=sample rate
        call clcdez
        ld (prgwintxt4),hl
        ld (propertxt23+8),hl
        xor a
        ld (prgwintxt4+2),a
        ld a,(mp3prpbre)    ;** init time-calculation
        ld de,125
        call clcm16
        ld (mp3timb),hl
        ld a,(mp3filgrd)
        add a:add a:add a
        ld (mp3timg),a
        ld hl,(mp3fillen)
        call mp3tim
        ex de,hl
        call clcdez
        ld (propertxt23+4),hl
        ex de,hl
        call clcdz3
        ld (propertxt23+0),a
        ld (propertxt23+1),hl
        ld hl,0             ;** find out 5sec position-skipvalue
mp3ini8 inc hl
        push hl
        call mp3tim
        pop hl
        cp 5
        jr c,mp3ini8
        ld (ctlskp),hl
        ;...check for ID3v2 tag
mp3ini9 ld ix,0
        ld iy,0
        ld c,0
        ld a,(mp3filhnd)
        call SyFile_FILPOI  ;** set filepointer to file start (reading aligned 4kb blocks makes loading faster)
        jr c,mp3del
        call mp3stp
        ld a,1
        ld (mp3filcnt),a
        ld hl,0
        ld (mp3filpos),hl
        ld hl,(mp3fillen)
        or a
        ld a,1
        ret

;### MP3DEL -> Closes MP3 sound
;### Output     CF=1
;### Destroyed  AF,BC,DE,HL,IX,IY
mp3del  ld a,(mp3filhnd)
        inc a
        scf
        ret z
        dec a
        call SyFile_FILCLO
        ld a,-1
        ld (mp3filhnd),a
        scf
        ret

;### MP3ID1 -> Tests, if ID3V1 tag available
mp3id1  ld hl,(mp3id3v1)
        ld a,l
        cp "T"
        jp nz,sndlod0
        ld a,h
        cp "A"
        jp nz,sndlod0
        ld a,(mp3id3v1+2)
        cp "G"
        jp nz,sndlod0
        ld hl,mp3id3v1
        ld bc,128*256+95
        ld a,127
mp3id12 cp (hl)
        jr nc,mp3id13
        ld (hl),c
mp3id13 inc hl
        djnz mp3id12
        ld de,mp3id3v1dat-2
        ld hl,mp3id3v1dat-6
        xor a
        ld bc,28:call mp3id11
        ld bc, 4:call mp3id11
        ld bc,30:call mp3id11
        ld bc,30
mp3id11 lddr
        ld (de),a
        dec de
        ret

mp3bufdbg   ;begin of buffer data
mp3bufply   db 0    ;buffer play-position (high byte)
mp3bufsta   ds 2    ;status for each page (0=empty/loading, 1=ok)
mp3buflod   db 0    ;page, which is currently loaded (0=nothing, 1/2=page)
mp3bufoky   db 0    ;flag, if songplay is possible because of filled buffer

mp3bufstp   db 0    ;flag, if songplay should be stopped
mp3bufdln   equ $-mp3bufply ;length of buffer data

mp3filhnd   db -1   ;handler ID
mp3fildat   dw 0    ;start of MP3 data inside the file
mp3fillen   dw 0    ;length of file/(4096*mp3filgrd)
mp3filpos   dw 0    ;current position in file
mp3filgrd   db 1    ;number of 4KB blocks for each position
mp3filcnt   db 1    ;grid-counter

mp3prpver   db 0    ;0=mpeg2.5, 1=reserved, 2=mpeg2, 3=mpeg1
mp3prplay   db 0    ;0=reserved, 1=layer3, 2=layer2, 3=layer1
mp3prpbrt   db 0    ;   bit rate (table index)
mp3prpsrt   db 0    ;sample rate (table index)

mp3prpbre   db 0    ;final bit rate/2

mp3brttab
;  m1-l1, m1-l2, m1-l3, m2-l1, m2-l2/3
db     0,     0,     0,     0,     0
db  32/2,  32/2,  32/2,  32/2,   8/2
db  64/2,  48/2,  40/2,  48/2,  16/2
db  96/2,  56/2,  48/2,  56/2,  24/2
db 128/2,  64/2,  56/2,  64/2,  32/2
db 160/2,  80/2,  64/2,  80/2,  40/2
db 192/2,  96/2,  80/2,  96/2,  48/2
db 224/2, 112/2,  96/2, 112/2,  56/2
db 256/2, 128/2, 112/2, 128/2,  64/2
db 288/2, 160/2, 128/2, 144/2,  80/2
db 320/2, 192/2, 160/2, 160/2,  96/2
db 352/2, 224/2, 192/2, 176/2, 112/2
db 384/2, 256/2, 224/2, 192/2, 128/2
db 416/2, 320/2, 256/2, 224/2, 144/2
db 448/2, 384/2, 320/2, 256/2, 160/2
db     0,     0,     0,     0,     0

mp3srttab
;  1, 2, 2.5 (mpeg)
db 44,22,11
db 48,24,12
db 32,16,08
db 00,00,00

mp3modtxt   dw mp3modtxt0,mp3modtxt1,mp3modtxt2,mp3modtxt3
mp3modtxt0  db "stereo",0
mp3modtxt1  db "joint stereo",0
mp3modtxt2  db "dual channel",0
mp3modtxt3  db "mono",0

mp3vertxt   db "25??2010"

;### MP3TIM -> Calculates position in seconds and minutes of the current MP3
;### Input      HL=position
;### Output     A=second, HL=minute
;### Destroyed  F,BC,DE,IX,IY
mp3timb dw 0
mp3timg db 0,0
mp3tim  ex de,hl
        ld bc,(mp3timg)
        call clcmul             ;A,HL=4kgrid*pos*8
        ex de,hl
        db #fd:ld h,a
        db #fd:ld l,d
        ld b,e
        ld c,0                  ;IY,BC=4kgrid*pos*2048
        ld ix,(mp3timb)         ;IX=bitrate/2*125
        call clcd32             ;IY,BC=mp3filgrd*mp3filpos*4096*8/(bitrate*1000) = seconds
        db #fd:ld a,l
        ld de,60
        call clcdiv             ;HL=mins, DE=secs
        ld a,e
        ret

;### MP3PLY -> Plays initialised MP3 sound (has to be called 50/60times per second)
;### Output     HL=position, CF=1 -> end has been reached
;### Destroyed  AF,BC,DE,HL,IX,IY
mp3ply  ld a,(mp3bufstp)
        or a
        jp nz,mp3ply6
        ld a,(mp3bufoky)
        or a
        jr z,mp3ply2
mp3plya call m3mchk
        jr nc,mp3ply2
        ld a,(mp3bufply)    ;A=buffer pointer (only high)
        push af
        inc a
        ld l,0
        ld h,a
        ld de,sndmem
        add hl,de
mp3plyb call m3msnd
        pop af
        add 8               ;set to next 2048 bytes (=8*256)
        res 5,a             ;keep pointer inside the 8kb buffer
        ld (mp3bufply),a
        bit 3,a             ;test, if 4kb-page changed
        jr nz,mp3ply2
        cp 16               ;yes -> test, which page has to be available, and which has to be loaded
        ld hl,mp3bufsta     ;HL=page, which is empty again
        ld de,mp3bufsta+1   ;DE=page, which has to be available
        jr z,mp3ply1
        inc hl
        dec de
mp3ply1 ld (hl),0
        ld a,(de)
        or a
        jr nz,mp3ply2
        ld (mp3bufoky),a
mp3ply2 ld a,(mp3buflod)    ;do we currently load a page?
        or a
        jr nz,mp3ply4
        ld hl,(mp3bufsta)   ;no -> check if, empty page has to be loaded
        ld de,256
        inc a
        dec l
        jr nz,mp3ply3
        ld d,4096/256+1
        inc a
        dec h
        jp z,mp3ply6        ;no -> finished (cf=0 here)
mp3ply3 ld (mp3buflod),a    ;load next 4096 bytes

        ld hl,m3mmode+1
        bit 5,(hl)
        jr z,mp3ply7
        ld a,(mp3posm)
        dec a
        ld (mp3posm),a
        jr nz,mp3ply7
        res 5,(hl)
        push de
        call mp3posa
        pop de

mp3ply7 ld hl,sndmem
        add hl,de
        ld (timmsgb+08),hl
        ld hl,4096
        ld (timmsgb+04),hl
        ld hl,256*FNC_FIL_FILINP+MSC_SYS_SYSFIL
        ld (timmsgb+00),hl
        ld a,(App_BnkNum)
        ld (timmsgb+06),a
        ld a,(mp3filhnd)
        ld (timmsgb+03),a
        ld iy,timmsgb
        ld a,(prgtimn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #10             ;send loading command
        jr mp3ply5
mp3ply4 ld iy,timmsgb       ;check for loading-response
        ld a,(prgtimn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #18
        db #dd:dec l
        jr nz,mp3ply5
        ld hl,(timmsgb+02)
        push hl
        pop af
        jr c,mp3ply6
        scf
        jr nz,mp3ply6
        ld hl,mp3buflod
        ld c,(hl)
        ld b,0
        ld (hl),b
        ld hl,mp3bufsta-1
        add hl,bc
        ld a,1
        ld (hl),a
        ld (mp3bufoky),a
        ld hl,mp3filcnt
        dec (hl)
        jr nz,mp3ply5
        ld a,(mp3filgrd)
        ld (hl),a
        ld hl,(mp3filpos)
        inc hl
        ld (mp3filpos),hl
        call mp3tim
        ex de,hl            ;a,de=sec,min
        ld hl,ctltim
        ld c,(hl)
        ld (hl),a
        sub c
        jr nz,mp3ply8       ;sec changed -> let's update it
        ld hl,(ctltim+1)
        sbc hl,de
        jr z,mp3ply5
mp3ply8 ld (ctltim+1),de    ;min changed -> let's update it
        ld a,1
        ld (prgtimc),a
mp3ply5 or a
mp3ply6 ld hl,(mp3filpos)
        ret

;### MP3POS -> Sets MP3 sound position
;### Input      HL=position, sound has already to be initialised
;### Destroyed  AF,BC,DE,HL,IX,IY
mp3posm db 0
mp3pos  ld a,1
        ld (mp3bufstp),a        ;stop current songplay
        push hl
        ld hl,m3mmode+1         ;mute song for a while to prevent clicks and ugly noises, caused by the interrupted MP3 data stream
        set 5,(hl)
        call mp3posa
        ld a,4
        ld (mp3posm),a
        pop de
        ld hl,(mp3fillen)
        dec hl
        or a
        sbc hl,de
        ex de,hl
        jr nc,mp3pos1
        add hl,de
mp3pos1 ld (mp3filpos),hl
        ld a,1
        ld (mp3filcnt),a
        ld a,l
        db #dd:ld h,a
        ld l,h
        xor a
        db #dd:ld l,a
        ld h,a
        ld a,(mp3filgrd)
        ld b,4-1
mp3pos2 inc b
        rrca
        jr nc,mp3pos2
mp3pos3 add ix,ix
        adc hl,hl
        djnz mp3pos3
        push hl
        pop iy
        ld c,0
        ld a,(mp3filhnd)
        call SyFile_FILPOI
mp3pos4 ld hl,mp3bufdbg         ;reset buffer data
        ld de,mp3bufdbg+1
        ld (hl),0
        ld bc,mp3bufdln-1
        ldir
        ret
mp3posa jp m3mset

;### MP3STP -> Stops MP3 sound
;### Destroyed  AF,BC,DE,HL,IX,IY
mp3stp  ld iy,timmsgb       ;check for loading-response
        ld a,(prgtimn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #18
        jr mp3pos4

;### MP3VOL -> Sets global volume
;### Input      A=volume (0-255)
;### Destroyed  AF
mp3vol  jp m3mvol

;### MP3EXT -> Sets extended audio settings (balance, bass, treble)
;### Input      A=balance (0-255), B=bass (0-255), C=treble (0-255), E=Stereomode (0=mono, 1=linear stereo, 2=pseudo stereo, 3=spartial stereo)
;### Destroyed  AF,BC,DE,HL
mp3ext  jp m3mext

;### MP3VAL -> Gets current volume and frequency of one channel
;### Input      A=channel (0-2)
;### Output     A=volume (0-15), L=frequency (0-15)
;### Destroyed  F,BC,DE,HL
mp3val  pop hl          ;** [dirty] skip ANAUPD7 standard routine to save CPU time **
        or a
        ret nz
mp3vala jp m3mspe


;==============================================================================
;### MP3 ROUTINEN (TEST) ######################################################
;==============================================================================

m3tres  xor a
m3tset  ret

m3tchkc db 1
m3tchk  ld hl,m3tchkc
        or a
        dec (hl)
        ret nz
        ld (hl),10
        scf
        ret

m3tsnd  ld a,2048/32
m3tsnd1 ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix
        ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix:ex (sp),ix
        dec a
        jr nz,m3tsnd1
        ret

m3tvol  ret

m3tspe  ret

;==============================================================================
;### MP3 ROUTINEN (GENERAL) ###################################################
;==============================================================================

;MP3MSX reset code
m3mresr db 12,0,   6,0,    0,2,    #98,0,  0,0,    #AC,#45,0,0,    #13,#84
        db 0,0,    0,0,    0,80,   0,0,    0,0,    0,0

;Spectrum analyzer table
m3mspet ds 31

;Audio parameters
m3mvols db 255      ;volume
m3mbals db 128      ;balance (0-127=lower right volume, 128=both 100%, 129-255=lower left volume)

m3mtab  db #64,#ff,#08      ;Initiate start condition
        db #44,#82,#18      ;Load audio chips slave address in buffer
        db #44,#00,#28      ;Load audio register number in buffer (number 0)
m3mvoll db #44,#3c,#28      ;set register 0 (volume left;    #00-#3f -> 16-step range should be #00,#2e-#3c)
m3mvolr db #44,#3c,#28      ;set register 1 (volume right;   #00-#3f)
m3mbass db #44,#07,#28      ;set register 2 (bass control;   #00-#0f)
m3mtreb db #44,#07,#28      ;set register 3 (treble control; #00-#0f)
        db #64,#ff,#10      ;Initiate start condition
        db #44,#82,#18      ;Load audio chips slave address in buffer
        db #44,#08,#28      ;Load audio register number in buffer (number 8)
m3mmode db #44,#0e,#28      ;set register 8 (switch function; 0-0-mute[1bit]-stereomode[2bit]-sourcemode[3bit])
m3mtabl equ 11      ;number of entries
m3mtabx db #f8

;### M3MEXT -> Sets extended audio settings (balance, bass, treble)
;### Input      A=balance (0-255), B=bass (0-255), C=treble (0-255), E=Stereomode (0=mono, 1=linear stereo, 2=pseudo stereo, 3=spartial stereo)
;### Destroyed  AF,BC,DE,HL
m3mext  ld (m3mbals),a      ;store balance
        ld a,e
        add a
        add a
        add a
        or #6   ;#26=mute
        ld (m3mmode+1),a    ;store mode
        ld a,b              ;shrink bass+treble down to 4 bit
        rrca:rrca:rrca:rrca
        and 15
        ld (m3mbass+1),a
        ld a,c
        rrca:rrca:rrca:rrca
        and 15
        ld (m3mtreb+1),a
        ld a,(m3mvols)      ;calculate left/write volume again and set all values
        jr m3mvol0

;### M3MVOL -> Sets global volume
;### Input      A=volume (0-255)
;### Destroyed  AF
m3mvol  push bc
        push de
        push hl
        ld (m3mvols),a
        call m3mvol0
        pop hl
        pop de
        pop bc
        ret
m3mvol0 ld c,a              ;C=left volume
        ld b,a              ;R=right volume
        ld a,(m3mbals)
        cp 128
        jr z,m3mvol2
        ld d,0
        jr nc,m3mvol1
        add a               ;balance<128 -> decrease left volume
        ld e,c
        call clcm16         ;H=right volume*balance/256
        ld c,h
        jr m3mvol2
m3mvol1 cpl                 ;balance>128 -> decrease right volume
        add a
        ld e,b
        call clcm16         ;H=left volume*(255-balance)/256
        ld b,h
m3mvol2 ld a,c
        call m3mvol3
        ld (m3mvoll+1),a
        ld a,b
        call m3mvol3
        ld (m3mvolr+1),a
        jp mp3posa
m3mvol3 rrca:rrca:rrca:rrca
        and 15
        ret z
        add #2d
        ret


;==============================================================================
;### MP3 ROUTINEN (MSX) #######################################################
;==============================================================================

;### M3MINI -> Initialise the MP3 hardware
;### Output     CF=0 ok, CF=1 error, hardware not detected
m3mres  xor a
        out (#22),a     ;reset mp3 decoder
        ld a,#44
        out (#27),a     ;reset audio-mixer hardware
        rst #30
        rst #30
        ld a,#40        ;write MP3 registers
        out (#23),a
        in a,(#23)      ;detect ATMega presence
        and 16
        jr z,m3mres1
        rst #30
        rst #30
        in a,(#23)
        and 16
        scf
        ret nz
m3mres1 ld hl,m3mresr
        ld bc,#1c22
        otir            ;write to ATM
        ld a,#c0
        out (#23),a     ;write to MP3 encoder
        call m3mwai
        ld a,#00        ;go to audio mode
        out (#23),a
        call m3mwai
        call m3mset     ;init audio-mixer
        or a            ;**currently no detection implemented**
        ret

m3mwai  in a,(#23)
        and 16
        jr nz,m3mwai
        ret

;### M3MCHK -> Test, if MP3 card requires new data
;### Output     CF=1 new data is required
m3mchkd equ 1
m3mchk  ld a,0
        sub 1
        jr nc,m3mchk1
        in a,(#23)
        rlca
        ret nc
        ld a,m3mchkd
m3mchk1 ld (m3mchk+1),a
        ret

;### M3MSND -> Sends 2048 bytes to the MP3 card
;### Input      HL=points to data
;### Output     HL=points behind data
m3msnd  ld a,2048/32
        ld c,#22
m3msnd1 outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi
        outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi
        dec a
        jr nz,m3msnd1
        ret

;### M3MSPE -> Reads spectrum analyzer information
m3mspe  ld a,31
        out (#23),a
        call m3mwai
        ld c,#22
        ld hl,m3mspet
        ini:ini:ini
        ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini
        ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini
m3mspe1 ld de,anatab
        ld hl,m3mspet+11
        ld b,10
m3mspe2 ld a,(hl)
        and 31
        srl a
        sub 3
        jr nc,m3mspe3
        xor a
m3mspe3 inc a
        ld (de),a
        inc de
        inc hl:inc hl
        djnz m3mspe2
        ret

;### M3MSET -> Sends the current volume/bass/treble settings to the TDA8425
;### Destroyed  AF,BC,HL
m3mset  ld c,m3mtabl
        ld hl,m3mtab
m3mset1 ld a,(hl)           ;send one or two bytes
        inc hl
        cp #44
        jr nz,m3mset2
        ld b,a
        ld a,(hl)
        out (#25),a
        ld a,b
m3mset2 inc hl
        out (#27),a
        call m3msta
        jr c,m3mset4
        inc hl
        dec c
        jr nz,m3mset1
m3mset3 ld a,#54            ;Generate stop condition
        out (#27),a
        ret
m3mset4 call m3mset3
        ld hl,m3mtabx
;...don't insert any code here

;### M3MSTA -> Wait for I2C status
;### Input      (HL)=expected status
;### Output     CF=1 error
m3msta  ld b,0
m3msta1 dec b
        scf
        ret z
        in a,(#27)
        and 8
        jr z,m3msta1
m3msta2 in a,(#24)
        cp (hl)
        ret z
        scf
        ret


;==============================================================================
;### MP3 ROUTINEN (CPC) #######################################################
;==============================================================================

m3cprt_base equ #ff

m3cprt_rset equ #21
m3cprt_data equ #22
m3cprt_mode equ #23
m3cprt_ista equ #24
m3cprt_idat equ #25
m3cprt_ictr equ #27


;### M3CINI -> Initialise the MP3 hardware
;### Output     CF=0 ok, CF=1 error, hardware not detected
m3cres  ld b,m3cprt_base
        xor a
        ld c,m3cprt_data:out (c),a  ;reset mp3 decoder
        ld a,#44
        ld c,m3cprt_ictr:out (c),a  ;reset audio-mixer hardware
        rst #30
        rst #30
        ld a,#40                    ;write MP3 registers
        ld c,m3cprt_mode:out (c),a
        in a,(c)                    ;detect ATMega presence
        and 16
        jr z,m3cres1
        rst #30
        rst #30
        in a,(c)
        and 16
        scf
        ret nz
m3cres1 ld hl,m3mresr
        ld e,#1c
        ld c,m3cprt_data
m3cres2 inc b:outi
        dec e
        jr nz,m3cres2
        inc c
        ld a,#c0
        out (c),a       ;write to MP3 encoder
        call m3cwai
        ld a,#00        ;go to audio mode
        out (c),a
        call m3cwai
        call m3cset     ;init audio-mixer
        or a            ;**currently no detection implemented**
        ret

;### Input      BC=Mode port
m3cwai  in a,(c)
        and 16
        jr nz,m3cwai
        ret

;### M3CCHK -> Test, if MP3 card requires new data
;### Output     CF=1 new data is required
;### Destroyed  AF,BC
m3cchk  ld a,0
        sub 1
        jr nc,m3cchk1
        ld bc,m3cprt_base*256+m3cprt_mode
        in a,(c)
        rlca
        ret nc
        ld a,m3mchkd
m3cchk1 ld (m3cchk+1),a
        ret

;### M3CSND -> Sends 2048 bytes to the MP3 card
;### Input      HL=points to data
;### Output     HL=points behind data
;### Destroyed  AF,BC
m3csnd  ld a,2048/32
        ld bc,m3cprt_base*256+m3cprt_data
m3csnd1 inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi
        inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi
        inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi
        inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi
        dec a
        jr nz,m3csnd1
        ret

;### M3CSPE -> Reads spectrum analyzer information
;### Destroyed  AF,BC,HL
m3cspe  ld a,31
        ld bc,m3cprt_base*256+m3cprt_mode
        out (c),a
        call m3cwai
        dec c
        ld hl,m3mspet
        ini:inc b:ini:inc b:ini:inc b
        ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b
        ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b
        jp m3mspe1

;### M3CSET -> Sends the current volume/bass/treble settings to the TDA8425
;### Destroyed  AF,BC,DE,HL
m3cset  ld e,m3mtabl
        ld b,m3cprt_base
        ld hl,m3mtab
m3cset1 ld a,(hl)           ;send one or two bytes
        inc hl
        cp #44
        jr nz,m3cset2
        ld d,a
        ld a,(hl)
        ld c,m3cprt_idat:out (c),a
        ld a,d
m3cset2 inc hl
        ld c,m3cprt_ictr:out (c),a
        call m3csta
        jr c,m3cset4
        inc hl
        dec e
        jr nz,m3cset1
m3cset3 ld a,#54            ;Generate stop condition
        ld c,m3cprt_ictr:out (c),a
        ret
m3cset4 call m3cset3
        ld hl,m3mtabx
;...don't insert any code here

;### M3CSTA -> Wait for I2C status
;### Input      (HL)=expected status, B=base port address
;### Output     CF=1 error
;### Destroyed  AF,C,D
m3csta  ld d,0
        ld c,m3cprt_ictr
m3csta1 dec d
        scf
        ret z
        in a,(c)
        and 8
        jr z,m3csta1
m3csta2 ld c,m3cprt_ista:in a,(c)
        cp (hl)
        ret z
        scf
        ret
