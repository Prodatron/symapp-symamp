;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@                             STARKOS SKM PLUGIN                             @
;@                 (c) 2005-2021 by Targhan/Arkos, Prodatron                  @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;==============================================================================
;### API-ROUTINEN #############################################################
;==============================================================================

skmreglow   equ #00
skmreghig   equ #01
skmregvol   equ #02


;### SKSCHN -> detects number of channels
;### Input      HL=address
;### Output     A=number of channels (always 3)
skschn  ld a,3
        ret

;### SKSINI -> Initialisiert SKS-Sound
;### Eingabe    HL=Adresse
;### Ausgabe    CF=0 -> ok, HL=Anzahl Positionen
;###            CF=1 -> nicht unterstütztes Format
;### Veraendert AF,BC,DE,HL,IX,IY
sksfrq  db 1,1
sksinie dw 0
sksini  ld de,128       ;skip amdos header
        add hl,de
        push hl         ;Frequenz ermitteln
        pop ix
        ld a,(ix+8)
        or a
        scf
        ret nz
        ld a,(ix+7)
        cp 50
        ld b,1
        jr z,sksini1
        ccf
        ret c
        cp 25
        ld b,2
        jr z,sksini1
        ld b,4
sksini1 ld c,1
        ld (sksfrq),bc
        push hl
        call sksrel     ;Song relocieren
        pop de
        call INITZIC    ;Song initialisieren
        ld hl,(PTTRACKSTAB+1)
        ld (sksplyb),hl
        ex de,hl
        ld hl,(ADENDPOS+1)
        call sksply3
        ld hl,0
        ld (sksplyp),hl
        ld l,c
        ld h,b
        or a
        ld a,3
        ret

;### SKSVOL -> Setzt die globale Lautstärke
;### Eingabe    A=Lautstärke (0-255)
;### Verändert  AF
sksvol  srl a:srl a:srl a:srl a
        neg
        add 15
        ld (r8v+1),a
        ld (r9v+1),a
        ld (rav+1),a
        ret

;### SKSVAL -> Holt Lautstärke und Frequenz eines Kanales
;### Eingabe    A=Kanal (0-2)
;### Ausgabe    A=Lautstärke (0-15), L=Frequenz (0-15)
;### Verändert  F,BC,DE,HL
sksval  cp 1
        jr z,sksval1
        jr nc,sksval2
        ld de,reg8 :ld hl,reg0:ld bc,reg1
        jr sksval3
sksval1 ld de,reg9 :ld hl,reg2:ld bc,reg3
        jr sksval3
sksval2 ld de,reg10:ld hl,reg4:ld bc,reg5
sksval3 ld a,(hl)
        and 7
        add a
        ld l,a
        ld a,(bc)
        rlca
        and 1
        or l
        ld l,a
        ld a,(de)
        ret

;### SKSPLY -> Spielt initialisierten SKS-Sound (muß mit 50Hz aufgerufen werden)
;### Ausgabe    HL=Position, CF=1 -> Ende wurde erreicht
;### Veraendert AF,BC,DE,HL,IX,IY
sksplyb dw 0
sksplyp dw 0
sksply  ld hl,sksfrq
        dec (hl)
        ret nz
        inc hl
        ld a,(hl)
        dec hl
        ld (hl),a
        di
        exx
        ex af,af'
        push af
        push bc
        push de
        push hl
        ld hl,(PTTRACKSTAB+1)
        push hl
        call envelopeInterrupt      ;!!!
        call PLAY
        pop de
        ld hl,(PTTRACKSTAB+1)
        ex de,hl
        or a
        sbc hl,de
        jr z,sksply1
        ld hl,(sksplyb)
        ex de,hl
        call sksply3
        ld (sksplyp),bc
sksply1 pop hl
        pop de
        pop bc
        pop af
        ex af,af'
        exx
        ei
        ld hl,(PTTRACKSTAB+1)
        ld de,(ADENDPOS+1)
        or a
        sbc hl,de
        jr nz,sksply4
        ld a,(PATTHEIGHT+1)
        or a
        jr nz,sksply4
        ld a,(PLAYWAIT+1)
        ld b,a
        ld a,(PLAYSPEED+1)
        cp b
        jr nz,sksply4
        ld a,(PLAYWAIT+1)
        ld b,a
        ld a,(PLAYSPEED+1)
        cp b
        jr nz,sksply4
        scf
        jr sksply5
sksply4 or a
sksply5 ld hl,(sksplyp)
        ret
;BC=(HL-DE)/6
sksply3 or a
        sbc hl,de
        ld bc,-1
        ld de,6
sksply2 inc bc
        or a
        sbc hl,de
        jr nc,sksply2
        ret

;### SKSSTP -> Hält SKS-Sound an
;### Veraendert AF,BC,DE,HL,IX,IY
sksstp  di
        exx
        ex af,af'
        push af
        push bc
        push de
        push hl
        call STOPSNDS
        pop hl
        pop de
        pop bc
        pop af
        ex af,af'
        exx
        ei
        ret

;### SKSPOS -> Setzt SKS-Sound auf eine bestimmte Position
;### Eingabe    HL=Position, Sound muß initialisiert sein
;### Veraendert AF,BC,DE,HL,IX,IY
skspos  ex de,hl
        ld hl,PTSTR2
        ld a,(hl)
        push af
        ld (hl),#c9
        jr skspos2
skspos1 push de
        xor a
        ld (TRWAIT+1),a
        ld (HTWAIT+1),a
        ld (STEWAIT+1),a
        di
        exx
        ex af,af'
        push af
        push bc
        push de
        push hl
        call TRWAIT
        pop hl
        pop de
        pop bc
        pop af
        ex af,af'
        exx
        ei
        pop de
        dec de
skspos2 ld a,e
        or d
        jr nz,skspos1
        pop af
        ld (PTSTR2),a
        ret


;==============================================================================
;### STARKOS-ROUTINEN (INTERN) ################################################
;==============================================================================

;	Player STARKOS V1.1 (compatible V1.0)

;	By Targhan/Arkos
;	27/02/03
;

;	This player is for Prodatron.
;	Modifies IX, IY, HL, DE, BC, AF, HL', DE', BC', AF'
;	Uses Stack, but in a normal way (call, push, pop....), doesn't relocate it (ld sp,...)

;	How to use the thing =

;	First of all, initialise the song =
;	ld de,AdrMusic
;	call INITZIC

;	then play the music
;	call PLAY

;	to stop all the sound
;	call STOPSNDS

;	That's it.

;	Needless to say, one player is use for any song.

;	All the code that seems useless is assembled under condition, so unless you set one of the 
;	three flags below to 1, they're not assembled, so don't worry about them.

;	Here's the header of a Starkos song. Useful to know the Frequency for example.
;	To relocate the song (usefull to create a generic player), please refer to the Starkos kit
;	which contains a relocator.

;OFFSETS
;0-3        DB "SK10"
;           Just a little ASCII tag for rippers to detect STK songs easier ).
;	    Even if the song is made with Stk1.1, the format is marked as SK10
;4-5        DW Base Address of the Song. #00,#04 if the song has to be loaded in
;           #4000, for example.
;6          DB No of the Channel used by Special Tracks Digidrums. 1,2 or 3.
;           Shouldn't be really useful, but it's here.
;7-8        DW Song Frequency in Hertz (13 (=#D), 25 (=#19), 50 (=#32), 100 (=#
;	    64), 150 (=#96) or 300 (=#12C)). Only used by the INTERRUPTION player when 
;	    initialising.




;	Les volumes de tracks sont inverses (effets 1 dans sountrakker) en mem
;	ainsi que PITCH.


;	Optimisations et divers de player final =
;	-----------------------------------------
;	Le signal envoyé disant 'finpatt' pourrait etre un 'or a'=#b7  /'scf'=#37.
;	On peut jarter le jp sendreg et coller cette routine au reste


;	ATTENTION
;	Pour le player BASIC, ne pas oublier de sauver BC' et AF' !
;	Comme le player editeur, RST modifie OLDINSTR en 0.

;	Le player sous inter BASIC doit etre place en #4000+ !!
;

;	*** THESE THREE CONSTANTES SHOULD BE SET TO 0 FOR INTEGRATION.
DEBUGPLAY equ 0

SAVEBCAF equ 0			;Sauve af' et bc'. (pour BASIC RAW et BASIC INTER)
BASICINT equ 0			;BASIC INTERRUPTIONS. IF set to 1, Set SAVEBCAF to 1 too.



SIZEINSTRNEWHEADER equ 7	;Taille nouveau header son. Don't fucking modify it.





;** just a try that test a song
	if DEBUGPLAY

ADZIC	equ #4000

	ld de,ADZIC
	call INITZIC

	di
	ld hl,#c9fb
	ld (#38),hl
	ld sp,#100



LOOP
	call VSYNC
	ei
	nop
	halt
	halt
	di
	defs 41,0
	ld b,13
DESC	defs 60,0
	djnz DESC
	ld bc,#7f10
	out (c),c
	ld a,#4b
	out (c),a

	call PLAY

	ld bc,#7f10
	out (c),c
	ld a,#54
	out (c),a
	ei
	nop
	halt
	di
	ld bc,#7f10
	out (c),c
	ld a,#44
	out (c),a
PEK
	jp LOOP

CHALP	ld a,0+64
	call ROUTOUCH
CHAPY	cp %11111110
	jr nz,CHALP
	ld a,(CHAPY+1)
	xor %00000101
	ld (CHAPY+1),a
	jp LOOP

ROUTOUCH
	LD BC,#F782
	OUT (C),C
	LD BC,#F40E
	OUT (C),C
	LD BC,#F6C0
	OUT (C),C
	defb #ed,#71
	LD BC,#F792
	OUT (C),C
	LD B,#F6
	OUT (C),A
	LD B,#F4
	IN A,(C)
	LD BC,#F782
	OUT (C),C
	dec b
	defb #ed,#71
	RET


	endif



;******* Enclenchement interruptions
	if BASICINT

	jp INTERON
	jp INTEROFF
	defw $-6			;Pour connaitre l'adr d'origine du player

DIGI	defb 0				;Digidrum a jouer
SPLCHAN	defb 0				;Chanel du digidrum (1,2,3)

INTERON	call INITZIC
	ld hl,FREQCONV
REPFREQ ld a,0		;a=low byte de frequence (13,25,50,100,150,44)
REPFRLP	cp (hl)
	jr z,REPOK
	inc hl
	inc hl
	jr REPFRLP
REPOK
	inc hl
	ld a,(hl)	;Chope nbinter wait
	ld (IPWAIT+1),a
	xor a
	ld (IPWCPT+1),a

	ld hl,BLOCCTRL
	ld bc,%10000001*256+0
	ld de,INTERPLAY
	jp #bce0
INTEROFF ld hl,BLOCCTRL
	call #bce6
	jp STOPSNDS

BLOCCTRL defs 10,0

;Routine qui va lancer PLAY en fct de frequence replay.
INTERPLAY di

IPWCPT	ld a,0
	sub 1
	jr c,IPPLAY
	ld (IPWCPT+1),a
	ret

IPPLAY
IPWAIT	ld a,0
	ld (IPWCPT+1),a
	jp PLAY


;Sert a convertir freq --> no freq
;Format=lowbyte freq et nb inter wait
FREQCONV defb 13,17,  25,11,  50,5,  100,2,  150,1,  44,0


	else
;******* Player Basic RAW ou ASM.
	jp INITZIC
	jp PLAY
	jp STOPSNDS

	defw $-9			;Pour connaitre l'adr d'origine du player

DIGI	defb 0				;Digidrum a jouer
SPLCHAN	defb 0				;Chanel du digidrum (1,2,3)
	endif





;Joue la musique
PLAY	xor a			;0=pas de digidrum
	ld (DIGI),a
	ld (RETRIG+1),a


	if SAVEBCAF
	 di
	 ex af,af'
	 exx
	 push af
	 push bc
	 push ix
	 push iy
	endif


PLAYWAIT ld a,0			;Nb VBL avant prochaine ligne (speed). inc.
PLAYSPEED cp 0			;Si pas a =speed, on continue les sons
	jr z,PLAYNEWLINE
	inc a
	ld (PLAYWAIT+1),a
	jp CONTSNDS

;On va entamer une nouvelle ligne dans pattern
PLAYNEWLINE xor a
	ld (PLAYWAIT+1),a
ISPATTEND or a			;Signal pour tester fin patt, envoyé par lecteur de patt. ('or a'=#b7  /'scf'=#37)
	jp nc,READPATT		;Pas de carry=continue pattern



;Nouvelle pattern.
	ld (TR1WAIT+1),a
	ld (TR2WAIT+1),a
	ld (TR3WAIT+1),a
	ld a,#b7		;Remet un OR A
	ld (ISPATTEND),a




;Gere transp.

;DB oct1 = bit 0=1=pas de transp pdt (bit 7-1) lignes
;          bit 0=0=TR1=bit (7-3) + bit(2-1)TR2 (=bits de poids fort)
;	          DB oct2. TR2=bit (7-5) (=faible) TR3=bit (4-0)

TRWAIT	ld a,0			;Nb lignes a attendre
	sub 1
	jr nc,TRFIN

PTTRANSPSTAB ld hl,0		;Lis nouvelle donnee TRANSP
	ld a,(hl)
	inc hl

	srl a
	jr c,TRFI0		;Carry=wait. Bit 7-1=wait.
				;Transp trouvee
	ld b,a			;S'occupe du transp1 (sauve a vers b)
	and %11111
	bit 4,a
	jr z,TRT1
	or %11100000
TRT1	ld (TRANSP1+1),a
;
	rl b
	rl b			;tr2 presente ?
	jr nc,TRF2
	ld a,(hl)
	ld (TRANSP2+1),a
	inc hl

TRF2	rl b			;tr3 presente ?
	jr nc,TRF3
	ld a,(hl)
	ld (TRANSP3+1),a
	inc hl

TRF3
	ld (PTTRANSPSTAB+1),hl
	jr TRFI2

TRFI0	ld (PTTRANSPSTAB+1),hl
TRFIN	ld (TRWAIT+1),a
TRFI2




;Lis HEIGHTSTAB
HTWAIT	ld a,0			;Nb pattern avant de lire encore HEIGHTSTAB
	sub 1
	jr c,PTHEIGHTSTAB
	ld (HTWAIT+1),a
HTOLDHEIGHT ld a,0		;Stocke l'ancienne hauteur stockee si several hauteur
	jr HTFI2

PTHEIGHTSTAB ld hl,0		;Lis nouvelle donnee HEIGHTSTAB
	ld a,(hl)
	inc hl
	srl a
	jr c,HTSEVERAL
;
	ld (PTHEIGHTSTAB+1),hl	;One height trouvee. Set nouvelle hauteur
	jr HTFI2
HTSEVERAL 
	ld (HTOLDHEIGHT+1),a	;Plusieurs heights trouvee. Set nouvelle hauteur pour now et plus tard aussi.
	ld b,a
	ld a,(hl)		;Chope la qqt de cette hauteur
	inc hl
	ld (PTHEIGHTSTAB+1),hl
;
HTFIN	ld (HTWAIT+1),a
 	 ld a,b
HTFI2	 ld (PATTHEIGHT+1),a



;Lis TRACKSTAB
PTTRACKSTAB ld hl,0
	ld de,PTTRACK1+1
	ldi
	ldi
	ld de,PTTRACK2+1
	ldi
	ldi
	ld de,PTTRACK3+1
	ldi
	ldi
	ld (PTTRACKSTAB+1),hl




;Lis STRACKSTAB
STSTATE	ld a,0		;0=egal 1=diff
	or a
	jr nz,STDIFF

STEWAIT	ld a,0
	sub 1
	jr c,STNEW

STECONT	ld (STEWAIT+1),a
STESTR	ld hl,0		;STRACK sauvee a repeter
	;ld (PTSTRACK+1),hl
	jr PTSTR2



STDIFF
STDWAIT ld a,0
	sub 1
	jr c,STNEW
	ld (STDWAIT+1),a
	ld hl,(PTSTRACKSTAB+1)
	jr STNDGET


STNEW
PTSTRACKSTAB ld hl,0
	ld a,(hl)		;Chope new state
	inc hl
	srl a
	jr c,STNEWDIFF
	ld (STEWAIT+1),a	;New identique
	xor a
	ld (STSTATE+1),a
	ld e,(hl)		;Chope strack adr identique et inc compteur
	inc hl
	ld d,(hl)
	inc hl
	ld (PTSTRACKSTAB+1),hl
	ex de,hl
	ld (STESTR+1),hl
	jr PTSTR2		;On se permet de skipper le test wait car on sait qu'il est a 0 !
STNEWDIFF ld (STDWAIT+1),a	;New diff
	ld a,1
	ld (STSTATE+1),a
STNDGET	ld e,(hl)		;Chope strack adr diff et inc compteur
	inc hl
	ld d,(hl)
	inc hl
	ld (PTSTRACKSTAB+1),hl
	ex de,hl
	;ld (PTSTRACK+1),hl
	jr PTSTR2		;On se permet de skipper le test wait car on sait qu'il est a 0 !








;Lis pattern (=strack+tracks)
READPATT


;Lis STRACK
STRWAIT	ld a,0			;Nb lignes vides a attendre avant lecture nouvelle ligne
	sub 1
	jr nc,STRFIN

PTSTRACK ld hl,0
PTSTR2	ld a,(hl)
	inc hl
	srl a
	jr c,STRSETW		;Carry=new wait
;
	srl a
	jr c,STRDIGI
	ld (PLAYSPEED+1),a	;Set speed
	jr STRSETX
STRDIGI	ld (DIGI),a		;Set digidrum
NOSPLCHAN ld a,1		;Set Spl Chan (cette val est settee lors de l'init)
	ld (SPLCHAN),a
STRSETX	xor a
STRSETW	ld (PTSTRACK+1),hl
STRFIN	ld (STRWAIT+1),a






;Lis les 3 TRACKS
TR1WAIT	ld a,0			;Nb ligne vides pour TRACK 1
	sub 1
	jr nc,TR1FIN

;d=transp
;e=instr   c=vol
;b=channel (1,2,3)
;hl'=now pitch
PTTRACK1 ld hl,0
;TRANSP1	ld d,0		;ld d,0
DECVOL1 ld bc,#0100
INSTR1	ld de,0		;ld e,0
TRANSP1 equ INSTR1+1
NOTE1	defb #dd : ld l,0

;
	call READTRACK
;
	defb #dd : ld a,l	;Chope note modifiee ou non.
	ld (NOTE1+2),a
;
	ld (PITADD1+1),hl
	exx
	ld (PTTRACK1+1),hl
	ld a,c
	ld (DECVOL1+1),a
	ld (DECVOL12+1),a
;
	xor a
	;ld (INOWSTP1+1),a
	defb #fd : or h		;Recalcule instr ? 0=oui. Si 1, l'instr continue.
	jr nz,TR1FI0
;
TR1NEW	ld (INOWSTP1+1),a
	ld d,a

	ld a,e
	ld (INSTR1+1),a

	ld l,d			;Si nouvo son, pitch=0 (pitchadd deja updaté)
	ld h,l
	ld (PITCH1+1),hl

PTINSTRS1 ld hl,0
	ex de,hl
	add hl,hl
	add hl,de
;
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld de,ADILOOP1+1
	ldi
	ldi
	ld de,ADIEND1+1
	ldi
	ldi
	ld de,ISTEP1+1
	ldi

	ld de,ADIISLOOP1
	ldi

	ld a,(hl)
	inc hl
	ld (PTSND1+1),hl

	ld hl,RETRIG+1
	or (hl)
	ld (hl),a

TR1FI0	defb #fd : ld a,l		;Recupere TRWAIT depuis lix
TR1FIN
	ld (TR1WAIT+1),a




TR2WAIT	ld a,0			;Nb ligne vides pour TRACK 2
	sub 1
	jr nc,TR2FIN

PTTRACK2 ld hl,0
;TRANSP2	ld d,0
DECVOL2 ld bc,#0200
INSTR2	ld de,0
TRANSP2	equ INSTR2+1
NOTE2	defb #dd : ld l,0

	call READTRACK
;
	defb #dd : ld a,l	;Chope note modifiee ou non.
	ld (NOTE2+2),a
;
	ld (PITADD2+1),hl
	exx
	ld (PTTRACK2+1),hl
	ld a,c
	ld (DECVOL2+1),a
	ld (DECVOL22+1),a
;
	xor a
	defb #fd : or h		;Recalcule instr ? 0=oui. Si 1, l'instr continue.
	jr nz,TR2FI0
;
TR2NEW	ld (INOWSTP2+1),a
	ld d,a
;
	ld a,e
	ld (INSTR2+1),a

	ld l,d			;Si nouvo son, pitch=0 (pitchadd deja updaté)
	ld h,l
	ld (PITCH2+1),hl

PTINSTRS2 ld hl,0
	ex de,hl
	add hl,hl
	add hl,de
;
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld de,ADILOOP2+1
	ldi
	ldi
	ld de,ADIEND2+1
	ldi
	ldi
	ld de,ISTEP2+1
	ldi

	ld de,ADIISLOOP2
	ldi

	ld a,(hl)
	inc hl
	ld (PTSND2+1),hl

	ld hl,RETRIG+1
	or (hl)
	ld (hl),a

TR2FI0	defb #fd : ld a,l		;Recupere TRWAIT depuis lix
TR2FIN
	ld (TR2WAIT+1),a





TR3WAIT	ld a,0			;Nb ligne vides pour TRACK 3
	sub 1
	jr nc,TR3FIN

PTTRACK3 ld hl,0
;TRANSP3	ld d,0
DECVOL3 ld bc,#0300
INSTR3	ld de,0
TRANSP3	equ INSTR3+1
NOTE3	defb #dd : ld l,0

	call READTRACK
;
	defb #dd : ld a,l	;Chope note modifiee ou non.
	ld (NOTE3+2),a
;
	ld (PITADD3+1),hl
	exx
	ld (PTTRACK3+1),hl
	ld a,c
	ld (DECVOL3+1),a
	ld (DECVOL32+1),a
;
	xor a
	defb #fd : or h		;Recalcule instr ? 0=oui. Si 1, l'instr continue.
	jr nz,TR3FI0
;
TR3NEW	ld (INOWSTP3+1),a
	ld d,a
;
	ld a,e
	ld (INSTR3+1),a

	ld l,d			;Si nouvo son, pitch=0 (pitchadd deja updaté)
	ld h,l
	ld (PITCH3+1),hl

PTINSTRS3 ld hl,0
	ex de,hl
	add hl,hl
	add hl,de
;
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld de,ADILOOP3+1
	ldi
	ldi
	ld de,ADIEND3+1
	ldi
	ldi
	ld de,ISTEP3+1
	ldi

	ld de,ADIISLOOP3
	ldi

	ld a,(hl)
	inc hl
	ld (PTSND3+1),hl

	ld hl,RETRIG+1
	or (hl)
	ld (hl),a

TR3FI0	defb #fd : ld a,l		;Recupere TRWAIT depuis lix
TR3FIN
	ld (TR3WAIT+1),a











;Gere la fin de pattern
PATTHEIGHT ld a,0
	sub 1
	jr c,PATTHEIGH2		;Carry=fin de zic
	ld (PATTHEIGHT+1),a
	jr CONTSNDS
PATTHEIGH2 ld a,#37		;Place un SCF quand pattern finie.
	ld (ISPATTEND),a





;Teste fin musique
	ld hl,(PTTRACKSTAB+1)
ADENDPOS ld de,0		;Adresse se trouvant APRES TRACKSTAB, utile pour tester si fin zic atteinte
	xor a
	sbc hl,de
	jr nz,ADENDPO2
;Fin musique !
	ld (TRWAIT+1),a
	ld (HTWAIT+1),a
	ld (STRWAIT+1),a
ADTRANSPSLOOP ld hl,0
	ld (PTTRANSPSTAB+1),hl
ADHEIGHTSLOOP ld hl,0
	ld (PTHEIGHTSTAB+1),hl
ADTRACKSLOOP ld hl,0
	ld (PTTRACKSTAB+1),hl
ADSTRACKSLOOP ld hl,0
	ld (PTSTRACKSTAB+1),hl
STSTATLOOP ld a,0		;Place etat de bouclage pour strack (egal/diff)
	ld (STSTATE+1),a
;
ADENDPO2



;On continue de jouer les sons
CONTSNDS
;Entree PLAYSND=
;HL=adresse des DONNEES du son (pas le header!)
;IY=REG0+1 ou 2/3
;D'=decvol
;LIX=note DEJA TRANSPOSEE
;PITCH+1=pitch actuel pour track
 ;A=NOWSTEP (utilisé si snd hard)
 ;HL'=RETRIG+1

;retour=
;HL=nouvelle adresse des donnees
;HIX=Masque REG7 (0000x00x) a ORer
	ld hl,RETRIG+1			;HL'=RETRIG+1 une fois pour toute
DECVOL12 ld d,0 ;ld a,(DECVOL1+1)	;d' chargé.
	exx
PITCH1	ld hl,0
PITADD1	ld de,0
	add hl,de
	ld (PITCH1+1),hl
	ld (CPITCH+1),hl
;
	ld a,(NOTE1+2)
	defb #dd : ld l,a
;
PTSND1	ld hl,0
	ld iy,REG0
	ld a,(INOWSTP1+1)
	call PLAYSND
	ex de,hl
INOWSTP1 ld a,0
ISTEP1	cp 0
	jr z,ADIEND1
	inc a			;On n'avance pas dans le son, car step pas encore atteint
	jr SND1FI2		
ADIEND1 ld hl,0			;Adresse ENDINSTR
	xor a			;Si son fini, nowstp=0
	sbc hl,de
	jr nz,SND1FIN		;Si END pas atteinte, on sauve la pos instr rendue par fct PLAYSND
ADILOOP1 ld de,0		;Sinon, on pos le pt instr sur ADR LOOP ! (qui peut etre = DATA INSTR 0 !)
ADIISLOOP1 or a			;Loop ? or a=non  scf=oui
	jr c,SND1FIN		;si oui (jump), alors PTSND1 et STEP ne changent pas.
				;si non, alors step=255
ADILOOPVIDE1 ld hl,0		;Et on resette le END sur fin data instr 0 (a calculer par init)
	ld (ADIEND1+1),hl	;Le adptson pointe automatiquement sur data instr0
	dec a
	ld (ISTEP1+1),a
	inc a
SND1FIN	ld (PTSND1+1),de
SND1FI2	ld (INOWSTP1+1),a
	defb #dd : ld a,h
	ld (R7S1+1),a










DECVOL22 ld d,0 ;ld a,(DECVOL1+1)	;d' chargé.
	exx
PITCH2	ld hl,0
PITADD2	ld de,0
	add hl,de
	ld (PITCH2+1),hl
	ld (CPITCH+1),hl
;
	ld a,(NOTE2+2)
	defb #dd : ld l,a
;
PTSND2	ld hl,0
	ld iy,REG2
	ld a,(INOWSTP2+1)
	call PLAYSND
	ex de,hl
INOWSTP2 ld a,0
ISTEP2	cp 0
	jr z,ADIEND2
	inc a
	jr SND2FI2		;On n'avance pas dans le son, car step pas encore atteint
ADIEND2 ld hl,0			;Adresse ENDINSTR
	xor a			;Si son fini, nowstp=0
	sbc hl,de
	jr nz,SND2FIN		;Si END pas atteinte, on sauve la pos instr rendue par fct PLAYSND
ADILOOP2 ld de,0		;Sinon, on pos le pt instr sur ADR LOOP ! (qui peut etre = DATA INSTR 0 !)
ADIISLOOP2 or a			;Loop ? or a=non  scf=oui
	jr c,SND2FIN		;si oui (jump), alors PTSND1 et STEP ne changent pas.
				;si non, alors step=255
ADILOOPVIDE2 ld hl,0		;Et on resette le END sur fin data instr 0 (a calculer par init)
	ld (ADIEND2+1),hl	;Le adptson pointe automatiquement sur data instr0
	dec a
	ld (ISTEP2+1),a
	inc a
SND2FIN	ld (PTSND2+1),de
SND2FI2	ld (INOWSTP2+1),a
	defb #dd : ld a,h
	ld (R7S2+1),a










DECVOL32 ld d,0 ;ld a,(DECVOL1+1)	;d' chargé.
	exx
PITCH3	ld hl,0
PITADD3	ld de,0
	add hl,de
	ld (PITCH3+1),hl
	ld (CPITCH+1),hl
;
	ld a,(NOTE3+2)
	defb #dd : ld l,a
;
PTSND3	ld hl,0
	ld iy,REG4
	ld a,(INOWSTP3+1)
	call PLAYSND
	ex de,hl
INOWSTP3 ld a,0
ISTEP3	cp 0
	jr z,ADIEND3
	inc a
	jr SND3FI2		;On n'avance pas dans le son, car step pas encore atteint
ADIEND3 ld hl,0			;Adresse ENDINSTR
	xor a			;Si son fini, nowstp=0
	sbc hl,de
	jr nz,SND3FIN		;Si END pas atteinte, on sauve la pos instr rendue par fct PLAYSND
ADILOOP3 ld de,0		;Sinon, on pos le pt instr sur ADR LOOP ! (qui peut etre = DATA INSTR 0 !)
ADIISLOOP3 or a			;Loop ? or a=non  scf=oui
	jr c,SND3FIN		;si oui (jump), alors PTSND1 et STEP ne changent pas.
				;si non, alors step=255
ADILOOPVIDE3 ld hl,0		;Et on resette le END sur fin data instr 0 (a calculer par init)
	ld (ADIEND3+1),hl	;Le adptson pointe automatiquement sur data instr0
	dec a
	ld (ISTEP3+1),a
	inc a
SND3FIN	ld (PTSND3+1),de
SND3FI2	ld (INOWSTP3+1),a
	defb #dd : ld a,h
	;ld (R7S3+1),a



;Gere le REG7
	sla a
R7S2	or 0
	rla
R7S1	or 0
	;ld h,a
	;ld (REG7+1),a
	jp SENDREG






;Routine generale pour lire les tracks dune voix selectionnee
;hl=pt track
;d=transp
;e=instr   c=decvol
;b=now channel (1,2,3)
;lix=ancienne note
;
;RET=
;hl=nouv pt track
;hl'=pitchADD donne par track
;hiy=0=recalculer instr (pitch=0)  1=continue instr
;liy=nouv nbwait
;e=instr nouv or old       lix=note, changee ou non.
;c=decvol
;ATTENTION ! RETOUR SUR REGISTRES AUXILIAIRES ! (Comme ca, on sauve le pitchadd direct)
.READTRACK
	ld a,(hl)
	inc hl
	srl a
	jr c,RTFULLOPT		;Full Optimisation
	cp 96
	jr nc,RTSPECIAL		;Cas special
;
	defb #fd		;hiy=0 car nouv instr
	ld h,0

	add a,d			;Note a jouer, avec ou sans effets derriere
	defb #dd : ld l,a	;lix=note
;
	ld b,(hl)		;chope vol s'il existe
	inc hl
	ld a,b
	rra
	jr nc,RTNOVOL		;Jump si pas de volume. C n'est pas modif
	and %1111
	ld c,a
RTNOVOL

	rl b
	jr nc,RTANCINS
	ld e,(hl)		;Nouv instr
	inc hl
RTANCINS rl b
	jr nc,RTNOPITCH
RTPITCH	ld a,(hl)		;Le SPECIAL PITCH/VOLPITCH jump ici aussi.
	inc hl
	exx			;Set pitchadd.
	ld l,a
	ld h,0
	rla			;Test signe pitchadd. Si neg, alors mets poids fort a #ff
	jr nc,RTPOSPIT
	dec h
RTPOSPIT
	;add hl,de

	defb #fd : ld l,0
	ret

RTNOPITCH exx			;Nouv instr mais pas de pitchadd
	ld hl,0

	defb #fd : ld l,0
	ret

;Full optimisation = note + pitch a 0 + instr et vol pareil qu'avant
RTFULLOPT
	defb #fd		;hiy=0 car meme instr, mais il faut le resetter
	ld h,0

	add a,d
	defb #dd : ld l,a	;Note a jouer
;
	jr RTNOPITCH


;Special. Pas de note, mais un ou deux effets (vol/pitch)
RTSPECIAL
	defb #fd		;hiy=1 car instr cont
	ld h,1

	sub 96
	jr z,RTSWAIT
	dec a
	jr z,RTSFINTRACK
	dec a
	jr z,RTSVOL
	dec a
	jr z,RTPITCH
	dec a
	jr z,RTSVOLPITCH
	dec a
	jr z,RTSRESET
	dec a
	jr z,RTSDIGI
	dec a			;Wait 0-24 lignes
	exx
	defb #fd : ld l,a
	ld hl,0
	ret
RTSIMPAIR			;Fonction speciale, impair


RTSWAIT	ld a,(hl)		;Wait X lignes
	inc hl
	exx
	defb #fd : ld l,a
	ld hl,0
	ret

RTSVOL	ld c,(hl)		;Vol only
	inc hl
	exx
	defb #fd : ld l,0
	ld hl,0
	ret

RTSVOLPITCH 
	ld c,(hl)		;Vol+pitch
	inc hl
	jr RTPITCH

RTSFINTRACK defb #fd : ld l,255
	exx
	ld hl,0
	ret

RTSDIGI ld a,(hl)
	inc hl
	ld (DIGI),a
	ld a,b
	ld (SPLCHAN),a
RTSRESET ld iy,#0000		;nouvo son et nbbwait a 0.
	ld e,0			;instr 0 car digidrum coupe la voie !
	exx
	ld hl,0
	ret









;PLAYSND. Joue un son dans un canal
;Entree=
;HL=adresse des DONNEES du son (pas le header!)
;IY=REG0+1 ou 2/4
;D'=decvol
;LIX=note DEJA TRANSPOSEE
 ;HL'=RETRIG+1
 ;A=nowstep

;retour=
;HL=nouvelle adresse des donnees
;HIX=Masque REG7 (0000x00x) a ORer


;pd la gestion = e=valeur 1er octet  d=mask reg 7 pour ce canal



.PLAYSND
	ld e,(hl)		;Son hard ou non
	inc hl
	bit 7,e
	jp nz,CGSHARD

;Son non hard
CNOHARD
	bit 4,e			;Noise ?
	jr z,CNHNONOIS
	ld a,(hl)
	bit 6,a			;Freq donnee? Cas particulier!
	jr z,CNHNOISE

;********* cas particulier (freq donnee pour son normal).
	ld d,%1000 ;%111110		;freq donnee. son forcement on.
	inc hl
	and %11111		;Teste si le noise=0
	jr z,CNHFGV
	ld (REG6+1),a		;Modif reg noise, autoriz noise
	res 3,d
;
CNHFGV				;noise geré.
	ld a,e			;On balance le volume
	and %1111
	exx
	sub d			;sub (ix+0)		;Volumedec de track
	exx
	jr nc,CDECVOLF4
	xor a
CDECVOLF4 ld (iy+skmregvol),a		;Code volume
	defb #dd : ld h,d ;ld (ix+2),d

CNHFGFREQ			;lis freq
	ld a,(hl)
	ld (iy+skmreglow),a
	inc hl
	ld a,(hl)
	ld (iy+skmreghig),a
	inc hl
	ret



;******** Cas normal
CNHNOISE 			;Modif reg noise, freq calculee (normal, quoi)
	ld (REG6+1),a
	inc hl
	ld d,%0001 ;%110111		;autorise noise sur ce canal
	bit 5,a			;Noise=1 donc on a un bit designe etat son
	jr z,CNHNOISNOS
	res 0,d			;Noise et son. Enclenche son
	ld a,e			;On balance le volume
	and %1111
	exx
	sub d			;(ix+0)		;Volumedec de track
	exx
	jr nc,CDECVOLF1
	xor a
CDECVOLF1 ld (iy+skmregvol),a		;Code volume
	defb #dd : ld h,d ;ld (ix+2),d
	jr CNHGARP
CNHNOISNOS 			;Noise mais pas de sons
	ld a,e			;On balance le vol qd meme (pour bruit)
	and %1111
	exx
	sub d			;(ix+0)		;Volumedec de track
	exx
	jr nc,CDECVOLF2
	xor a
CDECVOLF2 ld (iy+skmregvol),a		;Code volume
CNHNNNS2 ;xor a
	;ld (iy+skmreglow),a		;freq=0 (ca peut optimiser prochaines vbls)
	;ld (iy+skmreghig),a
	defb #dd : ld h,d ;ld (ix+2),d
	ret			;Si on coupe le son, rien ne sert de gerer freq!

CNHNONOISNOSND			;pas de son pas de bruit (appellee plus bas)
	;xor a			;(C1NHNONOIS)
	;ld (REG6+1),a	*** inutile de modifier REG6, puisque canal noise off
	ld (iy+skmregvol),0		;Code volume, utile pour couper hard
	ld d,%1001 ;%111111
	jr CNHNNNS2
CNHNONOIS ld d,%1000 ;%111110		;Pas de noise mais peut etre sound
	ld a,e
	and %1111		;Si vol alors son=1 sinon son=0
	jr z,CNHNONOISNOSND
	exx
	sub d			;(ix+0)		;Volumedec de track
	exx
	jr nc,CDECVOLF3
	xor a
CDECVOLF3 ld (iy+skmregvol),a		;Code volume. pas de noise mais son
	defb #dd : ld h,%1000 ;ld (ix+2),%111110	;Enclenche son et coupe noise


CNHGARP
	bit 5,e			;arp?
	jr z,CNHNOARP
	ld a,(hl)		;Get arp
	inc hl
	jr CNHARPF
CNHNOARP xor a
CNHARPF ;ld (CNHARP+1),a

	bit 6,e			;Pitch?
	jr z,CNHNOPITCH
	ld e,(hl)		;Get pitch
	inc hl
	ld d,(hl)
	inc hl
	jr CNHPITCHF
CNHNOPITCH ld de,0
CNHPITCHF

;On va maintenant gerer la frequence.
;Freq = note clavier/track + transp patt + arp + pitch instr + pitch patt

	
	defb #dd : add a,l	;ld a,(ix+1)		;note canal1/clavie
;CNHARP add a,0			;a=note a jouer
	cp 96			;Note >95?
	jr c,CNHOK
CNHNOK	ld a,96			;Si oui, rétabli
CNHOK
	push hl

	add a,a			;On trouve la periode
	ld l,a
	ld h,0
	ld bc,TABPERIODS
	add hl,bc
	ld c,(hl)
	inc hl
	ld b,(hl)		;bc=periode

CPITCH	ld hl,0			;add pattern pitch
	 sra h
	 rr l
	add hl,bc
	add hl,de		;add instr pitch
	;ld a,l
	ld (iy+skmreglow),l
	;ld a,h
	ld (iy+skmreghig),h

	pop hl
	ret








;************* Son hard
CGSHARD
	 or a			;Chope retrig
	 jr nz,CHNORETR		;Seulement si NOWSTEP=0 !
	 ld a,e
	 exx
	 and %01000000
	 or (hl)
	 ld (hl),a
	 exx
 CHNORETR


	ld a,e			;Code le SND = bit 0. Coupe NOISE pour l instant
	or  %1000		;si bit 0 de e = 1(pas de snd)->000001 ;111111
	and %1001
	ld d,a


CHSNDF	;ld a,%10000		;On mets le bit 5 du vol a 5 (activ hard).
	ld (iy+skmregvol),%10000 ;a	;Code volume


	;ld a,e
	;exx
	;ld hl,RETRIG+1
	;and %01000000		;Chope retrig
	;ld c,a
	;or (hl)
	;ld (hl),a
	;exx


	ld b,(hl)		;Prends 2e octet
	inc hl

	ld a,b			;Trouve courbe hard 8/A/C/E
	and %11
	add a,a
	add a,8
	ld (REG13+1),a


	bit 3,e			;noise ?
	jr z,CHNONOIS
	ld a,(hl)
	inc hl
	ld (REG6+1),a		;Noise.
	res 3,d			;autorise noise sur ce canal
	jr CHNOISF
CHNONOIS 			;No noise. Le canal noise est deja coupe
CHNOISF defb #dd : ld h,d ;ld (ix+2),d		;set reg7

;On chope le Finetune eventuel, le SHIFT (*2 car on l'utilise pour saut)
	xor a
	bit 7,b			;Hardsync
	jr z,CHGETSHIFT
	bit 6,b			;Finetune?
	jr z,CHNOFTUNE
	ld a,(hl)		;Get Finetune
	inc hl
CHNOFTUNE ld (CHHSYFTUNE+1),a
	ld a,1
CHGETSHIFT ld (CHISHARDSYNC+1),a
	ld a,b
	rra			;Chope le shift
	and %00001110
	ld (CHSHIFT+1),a


	bit 4,e			;freq auto/donnee?
	jp nz,CHFREQDONNEE	;freq donnee=arp et pitch inexistants
;Freq auto. On doit dabord choper l arp et pitch sils existent
	bit 1,e			;arp?
	jr z,CHNOARP

	ld a,(hl)
	;ld (CHARP+1),a
	inc hl
	jr CHARPF
CHNOARP xor a
	;ld (CHARP+1),a
CHARPF
	ex af,af'
	 ld a,e			;sauve e
	ex af,af'
	 ;ld (CHFREQF+1),a

	bit 2,e			;pitch?
	jr z,CHNOPITCH
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	jr CHPITCHF
CHNOPITCH ld de,0
CHPITCHF

;On peut maintenant calculer freq auto
;Freq = note clavier/track + transp patt + arp + pitch instr + pitch patt

;	ld a,(ix+1)
;CHARP	add a,0
	defb #dd : add a,l	;note canal1/clavier

	cp 96			;Note >95?
	jr c,CHOK
	ld a,96			;Si oui, rétabli
CHOK
	push hl
	;push de

	add a,a			;On trouve la periode
	ld l,a
	ld h,0
	ld bc,TABPERIODS
	add hl,bc
	ld c,(hl)
	inc hl
	ld b,(hl)		;de=periode

	ld hl,(CPITCH+1)	;add pattern pitch
	 sra h
	 rr l
	add hl,bc
	add hl,de		;add instr pitch

	ld c,l
	ld b,h

CHISHARDSYNC ld a,0		;Utilise t on HARDSYNC ?
	or a			;Si oui, pas la peine de sauver sfreq !
	jr nz,CHSHIFT		;Mais elle va servir pour calcul hfreq.

;
CHFREQF	ex af,af'		;a=e sauve plus haut
	bit 5,a			;FREQHARD donnée?
	jr nz,CHFREQHARDDONNEE



;FREQHARD Auto. On la calcule en fct de freq.
CHFREQ	ld a,c
	ld (iy+skmreglow),c
	ld (iy+skmreghig),b

CHSHIFT	ld e,0				;Shift deja *2
	ld a,e				;Multiple Shift par 3
	srl a				;Pour trouver ou sauter
	add a,e
	ld (CHSHJP+1),a
	ld a,c

CHSHJP	jr CHS7
CHS7	srl b
	rra
CHS6	srl b
	rra
CHS5	srl b
	rra
CHS4	srl b
	rra
CHS3	srl b
	rra
CHS2	srl b
	rra
CHS1	srl b
	rra
CHS0	ld c,a
	jr nc,CHSHIFTF2
	inc bc

CHSHIFTF2
	ld a,c
	ld (REG11),a
	ld a,b
	ld (REG12),a
;
	ld a,(CHISHARDSYNC+1)		;Utilise t on HARDSYNC ? (bis)
	or a
	jr z,CHSHIFTF

	ld a,(CHSHIFT+1)		;Fin calcul HARDSYNC.
	ld e,a				;Sfreq=hfreq*2 puissance SHIFT
	srl a
	add a,e
	ld (CHSHJP2+1),a
	ld a,b
CHSHJP2	jr CHHS7
CHHS7	sla c
	rla
	sla c
	rla
	sla c
	rla
	sla c
	rla
	sla c
	rla
	sla c
	rla
	sla c
	rla
	ld b,a
CHHSYFTUNE ld hl,0		;FineTune
	add hl,bc
	ld (iy+skmreglow),l		;On poke la sfreq calcule
	ld (iy+skmreghig),h
CHSHIFTF pop hl
	ret

;




CHFREQDONNEE ;ld a,b
	ld c,(hl)		;Freq donnee. On la chope
	inc hl
	ld b,(hl)
	inc hl
	push hl
;	rla			;Hardsync? bit 7=1?
;	jr c,CHAVHSYNC
	bit 5,e
	jr z,CHFREQ


CHFREQHARDDONNEE
	pop hl
	;ld a,c			;On poke la freq precedement calcule
	ld (iy+skmreglow),c
	;ld a,b
	ld (iy+skmreghig),b
	ld a,(hl)		;Periode donnee. On la chope
	ld (REG11),a
	inc hl
	ld a,(hl)
	ld (REG12),a
	inc hl
	ret




;Table des periodes
.TABPERIODS
	defw 3822,3608,3405,3214,3034,2863,2703,2551,2408,2273,2145,2025
	defw 1911,1804,1703,1607,1517,1432,1351,1276,1204,1136,1073,1012
	defw 956,902,851,804,758,716,676,638,602,568,536,506
	defw 478,451,426,402,379,358,338,319,301,284,268,253
	defw 239,225,213,201,190,179,169,159,150,142,134,127
	defw 119,113,106,100,95,89,84,80,75,71,67,63
	defw 60,56,53,50,47,45,42,40,38,36,34,32
	defw 30,28,27,25,24,22,21,20,19,18,17,16


reg0  db 0
reg1  db 0
reg8  db 0
reg2  db 0
reg3  db 0
reg9  db 0
reg4  db 0
reg5  db 0
reg10 db 0
reg11 db 0
reg12 db 0

;Balance les registres aux PSG
;a=val REG7
.SENDREG jp sendregCPC
sendregMSX
    ld hl,(reg0)
    ld c,l:ld b,h
    add hl,hl:add hl,hl:add hl,hl:sbc hl,bc
    srl h:rr l:srl h:rr l
    ld (reg0),hl
    ld hl,(reg2)
    ld c,l:ld b,h
    add hl,hl:add hl,hl:add hl,hl:sbc hl,bc
    srl h:rr l:srl h:rr l
    ld (reg2),hl
    ld hl,(reg4)
    ld c,l:ld b,h
    add hl,hl:add hl,hl:add hl,hl:sbc hl,bc
    srl h:rr l:srl h:rr l
    ld (reg4),hl
    ld hl,(reg11)
    ld c,l:ld b,h
    add hl,hl:add hl,hl:add hl,hl:sbc hl,bc
    srl h:rr l:srl h:rr l
    ld (reg11),hl

sendregCPC

	ld bc,#f4a0
	exx
	ld bc,#f6c0
	ld e,#80
	exx
sendreg0
    ld h,a 
;
.REG0x ld a,(reg0)		;NE PAS RAJOUTER DINSTRUCT NULLE PART
REG0OLD	cp 0			;ENTRE REG0/1, 2/3, 4/5 car on se sert
	jr z,REG1x		;de iy pour les adresser !!!
	ld d,0		;reg 0 select

skmreg0 call cpcskm
	ld (REG0OLD+1),a
.REG1x	ld a,(reg1)
REG1OLD	cp 0
	jr z,REG8x
	ld d,1		;reg 1 select

skmreg1 call cpcskm
	ld (REG1OLD+1),a
.REG8x	ld a,(reg8)
REG8OLD	cp 0
	jr z,REG2x
    bit 4,a
    jr nz,r8j
r8v sub 0    
    jr nc,r8j
    xor a
r8j
	ld d,8		;reg 8 select

skmreg8 call cpcskm
	ld (REG8OLD+1),a
.REG2x	ld a,(reg2)
REG2OLD	cp 0
	jr z,REG3x
	ld d,2		;reg 2 select

skmreg2 call cpcskm
	ld (REG2OLD+1),a
.REG3x	ld a,(reg3)
REG3OLD	cp 0
	jr z,REG9x
	ld d,3		;reg 3 select

skmreg3 call cpcskm
	ld (REG3OLD+1),a
.REG9x	ld a,(reg9)
REG9OLD	cp 0
	jr z,REG4x
    bit 4,a
    jr nz,r9j
r9v sub 0    
    jr nc,r9j
    xor a
r9j
	ld d,9		;reg 9 select

skmreg9 call cpcskm
	ld (REG9OLD+1),a
.REG4x	ld a,(reg4)
REG4OLD	cp 0
	jr z,REG5x
	ld d,4		;reg 4 select

skmreg4 call cpcskm
	ld (REG4OLD+1),a
.REG5x	ld a,(reg5)
REG5OLD	cp 0
	jr z,REG10x
	ld d,5		;reg 5 select

skmreg5 call cpcskm
	ld (REG5OLD+1),a
.REG10x	ld a,(reg10)
REG10OLD cp 0
	jr z,REG6
    bit 4,a
    jr nz,raj
rav sub 0    
    jr nc,raj
    xor a
raj
	ld d,10		;reg 10 select

skmrega call cpcskm
	ld (REG10OLD+1),a
.REG6	ld a,0
REG6OLD	cp 0
	jr z,REG7
REG62	ld d,6		;reg 6 select

skmreg6 call cpcskm
	ld (REG6OLD+1),a
.REG7	ld a,h
REG7OLD	cp %11000000
	jr z,REG11x
	ld d,7		;reg 7 select

skmreg7 call cpcskm
	ld (REG7OLD+1),a
.REG11x	ld a,(reg11)
REG11OLD cp 0
	jr z,REG12x
	ld d,11		;reg 11 select

skmregb call cpcskm
	ld (REG11OLD+1),a
.REG12x	ld a,(reg12)
REG12OLD cp 0
	jr z,REG13
	ld d,12		;reg 12 select

skmregc call cpcskm
	ld (REG12OLD+1),a
.REG13	ld a,0
REG13OLD cp 255
	jr nz,REG13G
	ld h,a
RETRIG	ld a,0		;retrig donne par colonne dans instr ou header instr
	or a
	ret z
	ld a,h
REG13G	ld d,13		;reg 13 select
skmregd call cpcskm
    exx
	ld (REG13OLD+1),a
	ret	






;Resette les VALEURS du psg.
.STOPSNDS

	if SAVEBCAF
	 di
	 ex af,af'
	 exx
	 push af
	 push bc
	 push ix
	 push iy
	endif

	xor a
	ld (REG8),a
	ld (REG9),a
	ld (REG10),a
	dec a
	ld (REG8OLD+1),a	;Ainsi, on est sur que les vals seront changees
	ld (REG9OLD+1),a
	ld (REG10OLD+1),a
	ld (REG7OLD+1),a

	ld a,%00111111
	;ld (REG7+1),a
	jp SENDREG



;Init la musique.
;DE=zic
.INITZIC

	ld hl,6
	add hl,de
	ld a,(hl)
	ld (NOSPLCHAN+1),a
	if BASICINT
	 inc hl			;Si INTERRUPTIONS, chope freqreplay (le low byte suffit)
	 ld a,(hl)
	 ld (REPFREQ+1),a
	 inc hl
	 inc hl
	else
	 ld de,3
	 add hl,de
	endif
;
	ld de,PTHEIGHTSTAB+1	;Si on veut initialiser sur une song vide, modifier ca. Attention
	ldi			;a ENDPOS et LOOPTO ! Dans cas les faire reinit aussi dans INITSNGTABS.
	ldi
	ld de,PTTRACKSTAB+1
	ldi
	ldi
	ld de,PTSTRACKSTAB+1
	ldi
	ldi
	ld de,PTINSTRS1+1
	ldi
	ldi

	ld de,ADTRANSPSLOOP+1
	ldi
	ldi
	ld de,ADHEIGHTSLOOP+1
	ldi
	ldi
	ld de,ADTRACKSLOOP+1
	ldi
	ldi
	ld de,ADSTRACKSLOOP+1
	ldi
	ldi

	ld a,(hl)
	inc hl
	ld (PLAYSPEED+1),a	;Chope begspd
	ld (PLAYWAIT+1),a

	ld (PTTRANSPSTAB+1),hl

	ld hl,(PTSTRACKSTAB+1)
	ld (ADENDPOS+1),hl	;ADENDPOS correspond a l'adr qui se situe APRES la TRACKSTAB
	ld a,(hl)		;Cherche l'etat de debut de strack (diff/egal)
	and %1
	ld (STSTATE+1),a

	ld hl,(ADSTRACKSLOOP+1)
	ld a,(hl)		;De meme avec lors du bouclage.
	and %1
	ld (STSTATLOOP+1),a


	ld hl,(PTINSTRS1+1)
	ld (PTINSTRS2+1),hl
	ld (PTINSTRS3+1),hl

	ld e,(hl)		;Chope adresse de DATA INSTR0 (pour que canaux debutent sur son vide)
	inc hl
	ld d,(hl)
	inc hl
	ex de,hl
	ld bc,SIZEINSTRNEWHEADER
	add hl,bc
	ld (PTSND1+1),hl
	ld (PTSND2+1),hl
	ld (PTSND3+1),hl
	ld (ADILOOP1+1),hl
	ld (ADILOOP2+1),hl
	ld (ADILOOP3+1),hl
	ex de,hl


	;ld hl,(PTINSTRS1+1)	;Chope adresse de fin de instr0, cad debut instr 1 !
	;inc hl			;Elle sert a setter le END d'un instr qui ne boucle pas, puisque pointeur
	;inc hl			;d'un tel instr pointera sur instr0 qd son fini.
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (ADILOOPVIDE1+1),de
	ld (ADILOOPVIDE2+1),de
	ld (ADILOOPVIDE3+1),de

	ld (ADIEND1+1),de
	ld (ADIEND2+1),de
	ld (ADIEND3+1),de


	;xor a
	;ld (TRWAIT+1),a
	;ld (HTWAIT+1),a
	;ld (STEWAIT+1),a
	;ld (STDWAIT+1),a
	;ld (STRWAIT+1),a


	;ld (INOWSTP1+1),a
	;ld (INOWSTP2+1),a
	;ld (INOWSTP3+1),a

	;ld ix,REG0+1
	;call SETCHANTO0
	;ld ix,REG2+1
	;call SETCHANTO0
	;ld ix,REG4+1
	;call SETCHANTO0

	;ld (REG11OLD+1),a
	;ld (REG12OLD+1),a
	;ld (REG13OLD+1),a
	 ;ld (DECVOL1+1),a
	 ;ld (DECVOL2+1),a
	 ;ld (DECVOL3+1),a

	;dec a
	;ld (REG6OLD+1),a
	;ld (REG7OLD+1),a
	;ld (ISTEP1+1),a
	;ld (ISTEP2+1),a
	;ld (ISTEP3+1),a

	ld a,#37
	ld (ISPATTEND),a
	;set 0,a
	;ld (ADIISLOOP1),a
	;ld (ADIISLOOP2),a
	;ld (ADIISLOOP3),a
;
	ld hl,SETTO0LIST
SETLISTINI ld a,(hl)
	or a
	ret z
	ld b,a
	inc hl
	ld a,(hl)
	inc hl
;
SETLISTLP ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (de),a
	djnz SETLISTLP
	jr SETLISTINI
;
	;ret

;Mets les reg de OLDVALS vol/freq a 0.
;SETCHANTO0
	;ld (ix+0),a
	;ld (ix+skmreghig),a
	;ld (ix+#36),a

;	ld (ix+2),a
;	ld (ix+#1d),a
;	ld (ix+#38),a
;	ret


SETTO0LIST defb 24,0		;NbIt (0=fini), fillbyte
	defw REG0,REG1,REG2,REG3,REG4,REG5
	defw REG6+1,REG8,REG9,REG10
	defw REG11,REG12,REG13+1

	defw TRWAIT+1,HTWAIT+1,STEWAIT+1,STDWAIT+1,STRWAIT+1
	defw INOWSTP1+1,INOWSTP2+1,INOWSTP3+1
	defw DECVOL1+1,DECVOL2+1,DECVOL3+1

SETTOFFLIST defb 17,#ff
	defw REG0OLD+1,REG1OLD+1,REG2OLD+1,REG3OLD+1,REG4OLD+1,REG5OLD+1
	defw REG6OLD+1,REG7OLD+1,REG8OLD+1,REG9OLD+1,REG10OLD+1
	defw REG11OLD+1,REG12OLD+1,REG13OLD+1

	defw ISTEP1+1,ISTEP2+1,ISTEP3+1


	defb 3,#b7
	defw ADIISLOOP1,ADIISLOOP2,ADIISLOOP3

	defb 0


	if DEBUGPLAY

VSYNC	ld b,#f5
VSYN2	in a,(c)
	rra
	jr nc,VSYN2
	ret

	endif




;	Starkos 1.0 Song Relocator
;	By Targhan/Arkos
;	On 22/03/03
;
;	This code is meant to be launched with a BASIC command line =
;	CALL #a000,param1,param2

;	To use it in an asm code, remove the 6 first lines below
;	and set HL and DE by yourself.
;
;	Parameter 1 HL = Address where is loaded the song
;	Parameter 2 HL = Address from which you want the internal data
;		of the song to be related.
;	Generally, param1=param2.

;	Data AREN'T moved ! You have to do it by yourself.
;
sksrel  ld e,l
        ld d,h
	ld (SONGADR),hl
	ld (NEWOFFSET),hl
;
	ld bc,4			;Get Base Adress of the song data
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
;
	ex de,hl
	or a
	sbc hl,de		;hl=Old data adr - New data adr
	ld a,l
	or h
	ret z			;if the diff=0 then nothing to do !
	ld (DIFFDATA),hl
;
;
	ld ix,(SONGADR)		;Code new Base Adress
	ld hl,(NEWOFFSET)
	ld (ix+4),l
	ld (ix+5),h
;
	ld b,8			;Change the offset of some 
	ld hl,9			;internal pointers
CHINTLP	call CHOFFREL
	inc hl
	inc hl
	djnz CHINTLP
;
	ld hl,11
	call GETWORDREL
	ld (TRACKSTAB),de
	ld hl,13
	call GETWORDREL
	ld (STRACKSTAB),de
	ld hl,15
	call GETWORDREL
	ld (INSTRS),de
;
;Modify the TRACKS table
	ld hl,(STRACKSTAB)
	ld de,(TRACKSTAB)
	or a
	sbc hl,de
	srl h
	rr l
	ld c,l
	ld b,h
;
	ex de,hl
	call MODLINTAB
;
;
;
;
;
;Modify the STRACKS table
	ld hl,(INSTRS)
	ld (STRADEND+1),hl
	ld de,(STRACKSTAB)
;
STRLOOP	ex de,hl
	ld a,(hl)
	inc hl
	srl a
	jr c,STROLP
	xor a
STROLP	call CHOFF
	inc hl
	inc hl
	sub 1
	jr nc,STROLP
;
	ex de,hl
STRADEND ld hl,0
	or a
	sbc hl,de
	jr nz,STRLOOP
;
;
;
;
;Modify the INSTRS table
	ld hl,(INSTRS)
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec hl
	ex de,hl
	ld bc,(DIFFDATA)
	add hl,bc
	ld (INSTEND+1),hl
	or a
	sbc hl,de
	srl h
	rr l
	ld c,l
	ld b,h
;
	ex de,hl
	call MODLINTAB
;
;
;
;Modify the INSTRUMENTS data
	ld hl,(INSTRS)
	ld (PTINSTAB+1),hl
;
PTINSTAB ld hl,0
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (PTINSTAB+1),hl
;
	ex de,hl
	call CHOFF	;Modify instr internal pointers
	inc hl
	inc hl
	call CHOFF
;
INSTEND ld hl,0
	ld de,(PTINSTAB+1)
	or a
	sbc hl,de
	jr nz,PTINSTAB
	ret
;
;
;
;
;
;
;***************
;LOW-LEVEL ROUTINES
;***************
;
;Add the offset to the word pointed by HL.
CHOFF
	push hl
	push bc
;
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec hl
	ld bc,(DIFFDATA)
	ex de,hl
	add hl,bc
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
;
;
	pop bc
	pop hl
	ret
;
;Add the offset to the relative adress in HL.
CHOFFREL
	push hl
;
	call GETADR
	ld l,(ix+0)
	ld h,(ix+1)
	ld de,(DIFFDATA)
	add hl,de
	ld (ix+0),l
	ld (ix+1),h
;
	pop hl
	ret
;
;
;Return in IX the adress in the song from the relative
;adress given in HL.
GETADR
	push de
	ld de,(SONGADR)
	add hl,de
	push hl
	pop ix
	pop de
	ret
;
;Return in DE the word pointed by HL
GETWORD
	ld e,(hl)
	inc hl
	ld d,(hl)
	ret
;
;Return in DE the word pointed by the relative
;adress in HL.
GETWORDREL
	call GETADR
	push ix
	pop hl
	call GETWORD
	ret
;
;
;Modify a linear table, composed of BC elem, pt by HL.
MODLINTAB 
;
MODLINLP call CHOFF
	inc hl
	inc hl
;
	dec bc
	ld a,c
	or b
	jr nz,MODLINLP
	ret
;
;
;******
;VARIABLES
;******
;
;
SONGADR	defw 0		;Adress of the song in memory
NEWOFFSET defw 0	;New adress of internal data
DIFFDATA defw 0		;Diff between old and new data adr

TRACKSTAB defw 0
STRACKSTAB defw 0
INSTRS defw 0
