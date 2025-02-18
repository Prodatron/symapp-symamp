;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                S y m  A m p                                @
;@                                                                            @
;@             (c) 2005-2025 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;- move bitmaps to other bank
;- dynamic bitrate display fix for mp3
;- options
;  - skip bad songs
;  - autosort list
;- songlist load -> reset marked song/position

;- hauptprozess hängt sich manchmal bei song-add auf -> keine message von system-manager
;- clean up (kernel syslib, msgsnd, loader pro modul)
;- songlist -> filediag open while playing -> next song starts at this time -> freeze

;modules/hardware
;- direct SF3 playback
;- darky bass/treble/panning/master volume
;- opl4 panning
;- ayc support
;- ST2 auf turboR + korrektur

;new


;--- CODE-TEIL ----------------------------------------------------------------
;### PRGPRZ -> Programm-Prozess
;### PRGHLP -> shows help
;### PRGFOC -> Focus-Änderung von Hauptfenster verarbeiten
;### PRGKEY -> Taste auswerten
;### PRGEND -> Programm beenden
;### PRGINF -> open info window
;### PRGERR -> Disc-Error-Fenster anzeigen
;### PRGDBL -> Test, ob Programm bereits läuft

;--- SONG-INFO ----------------------------------------------------------------
;### PRPLST -> Infos über Listeneintrag anzeigen
;### PRPSNG -> Infos über geladenen Song anzeigen
;### PRPOPN -> Songinfo-Fenster öffnen
;### PRPLST -> Song-Infos generieren

;--- SPECTRUM ANALYZER --------------------------------------------------------
;### ANASWT -> Analyzer an-/ausschalten
;### ANAOFF -> Analyzer löschen
;### ANAPLT -> Plottet Analyzer
;### ANAUPD -> Analyzer-Grafik updaten

;--- PLAY-LIST ----------------------------------------------------------------
;### LSTINI -> Playlist init (sets filename pointers)
;### LSTNUM -> Playlist renumbering (generates numbers in front of filenames)
;### LSTOPN -> Öffnet Playlist
;### LSTSWT -> Schaltet Playliste an/aus
;### LSTUPD -> Baut Liste neu auf inklusive Größe
;### LSTREF -> Baut Liste neu auf
;### LSTRSL -> Entfernt selektierte Einträge
;### LSTLAS -> finds entry with last string
;### LSTREM -> removes one entry
;### LSTRAL -> Entfernt alle Einträge
;### LSTSAL -> Selektiert alle Einträge
;### LSTSNO -> Deselektiert alle Einträge
;### LSTSIV -> Invertiert Selektion
;### LSTEUP -> Selektierte Einträge einen Platz nach oben
;### LSTEDW -> Selektierte Einträge einen Platz nach unten
;### LSTADD -> adds filename to list
;### LSTAFI -> Fügt Datei hinzu
;### LSTADI -> Fügt Verzeichnis hinzu
;### LSTCLK -> Doppelklick auf die Songliste
;### LSTCHK -> Prüft, ob Liste Einträge hat
;### LSTNXT -> Holt Pfad des nächsten Eintrages in der Playliste
;### LSTPRV -> Holt Pfad des vorherigen Eintrages in der Playliste
;### LSTRND -> Holt Pfad eines zufälligen Eintrages in der Playliste
;### LSTAKT -> Holt Pfad des aktuellen Eintrages in der Playliste
;### LSTSRT -> sort songlist
;### LSTLOD -> Lädt Songliste
;### LSTSAV -> Speichert Songliste

;--- PATH ROUTINES ------------------------------------------------------------
;### PTHFND -> searches for path, tries to add a new, if not found
;### PTHGET -> gets path

;--- SUB WINDOWS --------------------------------------------------------------
;### WINUPD -> updates controls in main window
;### WINOPN -> (HL)=winID, DE=win record -> opens windows, if not already opened
;### WINCLO -> (HL)=winID -> closes windows, if opened
;### PRFOPN -> opens preferences window
;### PRFCLO -> closes preferences window
;### MIXOPN -> opens equalizer
;### MIXSWT -> switches equalizer on/off
;### MIXVOL -> volume clicked
;### MIXBAS -> bass clicked
;### MIXTRE -> treble clicked
;### MIXPAN -> panning clicked
;### MIXSM0-3
;### MIXCLK -> vertical mixer clicked
;### MIXSHW -> shows vertical mixer setting
;### MIXSET -> Setzt erweiterte Mixer-Einstellungen
;### DIAOPN -> DE=data record -> opens dialoge window
;### DIACNC -> closes dialoge window

;--- CONTROLS -----------------------------------------------------------------
;### CTLINC -> Spielzeit erhöhen
;### CTLPUP -> Position anzeigen
;### WINSLH -> updates h-slider position
;### WINSLV -> updates v-slider position
;### WINSLP -> calculates slider position
;### CTLVUP -> Volume anzeigen
;### CTLTUP -> Spielzeit anzeigen
;### CTLSTA -> Spielstatus updaten
;### CTLNRS -> Löscht Songnamen
;### CTLNAM -> Setzt Songnamen
;### CTLPLY -> Sound abspielen
;### CTLPAU -> Sound anhalten
;### CTLSTP -> Sound stoppen
;### CTLSKF -> Skip forward
;### CTLSKB -> Skip backward
;### CTLPOS -> Position setzen
;### CTLVLU -> Increase Volume
;### CTLVLD -> Decrease Volume
;### CTLVOL -> Volume setzen
;### CTLREW -> Ein Lied zurück
;### CTLFFW -> Ein Lied weiter
;### CTLOPN -> neuen Sound laden
;### CTLREP -> Repeat an/aus
;### CTLSHU -> Shuffle an/aus
;### CTLRMT -> remote control

;--- SOUND-PLAY ---------------------------------------------------------------
;### SNDPAR -> Angehängten Sound suchen
;### SNDUNL -> unloads extra data of current song
;### SNDLOD -> Lädt und initialisiert Modul
;### SNDCHN -> get number of channels of loaded module
;### SNDINI -> Initialisiert geladenes Modul eines bestimmten Types
;### SNDRES -> Deaktiviert die Soundausgabe
;### SNDPLY -> Startet Abspielen
;### SNDPAU -> Hält Abspielen an
;### SNDSTP -> Stoppt Song und setzt Position an den Anfang
;### SNDPOS -> Setzt Song-Position
;### SNDVOL -> Setzt Song-Volume
;### SNDVAL -> Holt Frequenz und Volume
;### PRGTIM -> Programm-Timer, spielt Sound ab

;--- SUB-ROUTINEN -------------------------------------------------------------
;### SYSINI -> Computer-Typ abhängige Initialisierung
;### CFGINI -> Config übernehmen
;### SPRCNV -> Konvertiert Sprite vom CPC ins MSX Format
;### GFXINI -> Bitmap init
;### MSGGET -> Message für Programm abholen
;### MSGSND -> Message an Desktop-Prozess senden
;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### CLCDIV -> Dividiert zwei Werte (24bit)
;### CLCD32 -> Dividiert zwei Werte (32bit)
;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### CLCD16 -> Dividiert zwei Werte (16bit)
;### STRLEN -> Ermittelt Länge eines Strings
;### STRCMP -> Vergleicht zwei Strings
;### DIV168 -> A=HL/E, H=HL mod E
;### CLCNUM -> Wandelt 16Bit-Zahl in ASCII-String um (mit 0 abgeschlossen)
;### CLCDZ3 -> Rechnet Word in drei Dezimalziffern um
;### CLCDEZ -> Rechnet Byte in zwei Dezimalziffern um
;### CLCUCS -> Wandelt Klein- in Großbuchstaben um
;### CLCLCS -> Wandelt Groß- in Kleinbuchstaben um

;---


;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

lstmax  equ 250     ;number of entries

;### Config
anaflg  db 1
ctlrepf db 1
ctlshuf db 0
lstwin  db -1   ;songlist    window ID (>=0 -> active, =-1 -> not active, 1=open at start)
mixwin  db -1   ;mixer       window ID (>=0 -> active, =-1 -> not active)
prfwin  db -1   ;preferences window ID (-1 = not opened)

sndset_vol db 255   ;volume  (0-255)
sndset_pan db 128   ;panning (0-255)
sndset_bas db 128   ;bass    (0-255)
sndset_tre db 128   ;treble  (0-255)
sndset_ste db 1     ;stereo mode (0=mono, 1=stereo, 2=pseudo stereo, 3=partial stereo)

prgid   db "SymAmp":ds 6

;### PRGPRZ -> Programm-Prozess
dskprzn     db 2
sysprzn     db 3
windatprz   equ 3   ;Prozeßnummer
windatsup   equ 51  ;Nummer des Superfensters+1 oder 0
prgwin      db 0    ;Nummer des Haupt-Fensters
diawin      db -1   ;Nummer des Dialog-Fensters

prgprz  call SySystem_HLPINI
        call prgdbl
        call bmplod             ;load extended bitmaps
        call bmpset             ;set/patch extended bitmaps

        ld a,(App_PrcID)
        ld (prgwindat+windatprz),a
        ld (prglstdat+windatprz),a
        ld (properwin+windatprz),a
        ld (prgwinmix+windatprz),a
        ld (prgwinprf+windatprz),a

        call SySound_SNDINI     ;check for sound daemon
        jr c,prgprz3
        ld (hrddem),a           ;-> yes, use volume settings and opl4 parameters
        ld a,h
        ld (op4_64kbnk),a
        ld a,b
        ld (sndset_vol),a
        call ctlvup0            ;set volslider-position main
        call winslh0
        call volupd2            ;set volslider-position mixer

prgprz3 call sysini             ;Computer-Typ abhängige Initialisierung, sound device detection
        call gfxini             ;Grafiken aus "Skin" kopieren
        call lstini
        call lstupd0
        ld a,(lstwin)           ;Songliste optional anzeigen
        dec a
        ld a,-1
        ld (lstwin),a
        call nz,lstopn
        call cfgini

        ld de,prgwindat
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN   ;open window
        jp c,prgend
        ld (prgwin),a           ;Fenster wurde geöffnet -> Nummer merken

        ld hl,prgtims           ;Timer hinzufügen
        ld a,(App_BnkNum)
        call SyKernel_MTADDT
        jp c,prgend
        ld (prgprztab+0),a
        jp sndpar

prgprz0 call msgget
        jr nc,prgprz0
        ld b,a
        ld a,(prgtimn)
        db #dd:cp h
        jp z,ctlinc             ;*** Message vom eigenen Timer bekommen
        ld a,b
        cp "R"                  ;remote control (from sound daemon)
        jp z,ctlrmt
        cp MSC_GEN_FOCUS        ;*** Application soll sich Focus nehmen
        jp z,prgfoc0
        cp MSR_DSK_WFOCUS
        jr z,prgfoc
        cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz0
        ld e,(iy+1)
        ld a,(prgwin)
        cp e
        jr z,prgprz4
        ld a,(lstwin)
        cp e
        jr nz,prgprz9
        ld a,(iy+2)             ;*** LIST-WINDOW
        cp DSK_ACT_CLOSE
        jp z,lstswt
        jr prgprz7
prgprz9 ld a,(prfwin)           ;*** PREFERENCES-WINDOW
        cp e
        jr nz,prgprz6
        ld a,(iy+2)
        cp DSK_ACT_CLOSE
        jr nz,prgprz5
        jp prfclo
prgprz6 ld a,(mixwin)           ;*** MIXER-WINDOW
        cp e
        jr nz,prgprz8
        ld a,(iy+2)
        cp DSK_ACT_CLOSE
        jr nz,prgprz5
        jp mixswt
prgprz8 ld a,(diawin)
        cp e
        jr nz,prgprz0
        ld a,(iy+2)             ;*** DIALOG-WINDOW
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,diacnc
        jr prgprz5
prgprz4 ld a,(iy+2)             ;*** HAUPT-FENSTER ODER SONGLIST
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
prgprz7 cp DSK_ACT_KEY          ;*** Taste wurde gedrückt
        jp z,prgkey
prgprz5 cp DSK_ACT_MENU         ;*** Menü wurde geklickt
        jr z,prgprz2
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        jr nz,prgprz0
prgprz2 ld l,(iy+8)
        ld h,(iy+9)
        ld a,l
        or h
        jp z,prgprz0
        ld a,(iy+3)             ;A=Klick-Typ (0/1/2=Maus links/rechts/doppelt, 7=Tastatur)
        jp (hl)

;### PRGHLP -> shows help
prghlp  call SySystem_HLPOPN
        jp prgprz0

;### PRGFOC -> Focus-Änderung von Hauptfenster verarbeiten
prgfoc  ld e,(iy+1)
        ld a,(prgwin)
        cp e
        jp nz,prgprz0
        ld a,(prgwindat)
        cp 3
        jr nz,prgfoc1
        ld a,(lstwin)           ;** main window now minimized -> minimize list+equalizer, too
        cp -1
        jr z,prgfoc3
        ld c,MSC_DSK_WINMIN         ;minimize list window if existing
        call ctlsta2
prgfoc3 ld a,(mixwin)
        cp -1
        jp z,prgprz0
        ld c,MSC_DSK_WINMIN         ;minimize equalizer window if existing
        call ctlsta2
        jp prgprz0
prgfoc1 xor a
        ld (prgfoc5+1),a
        ld a,(prglstdat)        ;** main window now restored -> restore list+equalizer, too
        cp 3
        jr nz,prgfoc4
        ld a,(lstwin)
        cp -1
        jr z,prgfoc4
        ld (prgfoc5+1),a
        ld c,MSC_DSK_WINMID
        call ctlsta2
prgfoc4 ld a,(prgwinmix)
        cp 3
        jr nz,prgfoc5
        ld a,(mixwin)
        cp -1
        jr z,prgfoc5
        ld (prgfoc5+1),a
        ld c,MSC_DSK_WINMID
        call ctlsta2
prgfoc5 ld a,0
        or a
        jp z,prgprz0
prgfoc2 ld a,(prgwin)
        ld c,MSC_DSK_WINTOP
        call ctlsta2
        jp prgprz0

prgfoc0 ld a,(prgwin)
        ld b,a
        ld c,MSC_DSK_WINMID
        call msgsnd
        jr prgfoc1

;### PRGKEY -> Taste auswerten
prgkeya equ 24
prgkeyt db "Z":dw ctlrew    ;Z=Rückwärts
        db "X":dw ctlply    ;X=Play
        db "C":dw ctlpau    ;C=Pause
        db "V":dw ctlstp    ;V=Stop
        db "B":dw ctlffw    ;B=Vorwärts
        db "L":dw ctlopn    ;L=Laden
        db "R":dw ctlrep    ;R=Repeat
        db "S":dw ctlshu    ;S=Shuffle
        db 139:dw ctlskf    ;->=skip forward
        db 138:dw ctlskb    ;<-=skip backward
        db 136:dw ctlvlu    ;up=increase volume
        db 137:dw ctlvld    ;dw=decrease volume
        db 156:dw lstswt    ;Alt+E=Playlist
        db "A":dw lstafi    ;A=Add file
        db 152:dw lstadi    ;Alt+A=Add folder
        db 008:dw lstrsl    ;Del=Remove Selected
        db 127:dw lstral    ;Clr=Remove All
        db 001:dw lstsal    ;Ctrl+A=Select All
        db 014:dw lstsno    ;Ctrl+N=Select None
        db 160:dw lstsiv    ;Alt+I=Invert Selection
        db "U":dw lsteup    ;U=Up
        db "D":dw lstedw    ;D=Down
        db 015:dw lstlod    ;Ctrl+O=Load Playlist
        db 019:dw lstsav    ;Ctrl+S=Save Playlist

prgkey  ld hl,prgkeyt
        ld b,prgkeya
        ld de,3
        ld a,(iy+4)
        call clcucs
prgkey1 cp (hl)
        jr z,prgkey2
        add hl,de
        djnz prgkey1
        jp prgprz0
prgkey2 inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld a,7
        jp (hl)

;### PRGEND -> Programm beenden
prgend  call sndstp
        call mp3del
        call dmnfre
        ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGINF -> open info window
prginf  ld b,1+64+128
        ld hl,prgmsginf
        call prginf0
        jp prgprz0
prginf0 ld a,(App_BnkNum)
        ld de,prgwindat
        jp SySystem_SYSWRN

;### PRGERR -> Disc-Error-Fenster anzeigen
prgerr  ret nc
        call clcdez
        ld (txterrlod2a),hl
        ld b,1+64
        ld hl,daterrlod
        jr prginf0

;### PRGDBL -> Test, ob Programm bereits läuft
prgdbl  xor a
        ld (App_BegCode+prgdatnam),a
        ld hl,prgid
        ld b,l
        ld e,h
        ld l,a
        ld c,MSC_SYS_PRGSRV
        ld a,(App_BnkNum)
        ld d,a
        ld a,PRC_ID_SYSTEM
        call msgsnd1
prgdbl1 db #dd:ld h,PRC_ID_SYSTEM
        call msgget1
        jr nc,prgdbl1
        cp MSR_SYS_PRGSRV
        jr nz,prgdbl1
        ld a,"S"
        ld (App_BegCode+prgdatnam),a
        ld a,(App_MsgBuf+1)
        or a
        ret nz
        ld a,(App_MsgBuf+9)
        ld c,MSC_GEN_FOCUS
        call msgsnd1
        jp prgend


;==============================================================================
;### SONG-INFO ################################################################
;==============================================================================

;### PRPLST -> Infos über Listeneintrag anzeigen
prplst  ld ix,lstentlst
        ld hl,lstmem+4
        ld a,(prgobjlst1)
        or a
        jp z,prgprz0
        ld bc,4
        ld de,17
prplst1 bit 7,(ix+1)
        jr nz,prplst2
        add hl,de
        add ix,bc
        dec a
        jr nz,prplst1
        jp prgprz0
prplst2 push hl
        ld a,(ix+0)
        call pthget
        ld de,propertxt10
        call prpinf3
        pop hl
        call prpinf3
        ld a,7
        call prpinf
        jr prpopn

;### PRPSNG -> Infos über geladenen Song anzeigen
prpsng  ld a,(sndinif)
        or a
        jp z,prgprz0
        ld hl,sndfil
        ld de,propertxt10
        call prpinf3
        ld a,19
        call prpinf
;### PRPOPN -> Songinfo-Fenster öffnen
prpopn  ld de,properwin
        jp diaopn

;### PRPLST -> Song-Infos generieren
;### Eingabe    A=Anzahl Objekte von Property-Fenster, DE=letztes Filenamen-Zeichen
prpinft db "skmst2mp3pt3modsa2",0
prpinfa dw propertxt2x,propertxt20,propertxt21,propertxt22,propertxt24,propertxt25,propertxt28

prpinf  ld (propergrp),a
        ld ix,-4
        add ix,de
        ld iy,prpinft
        ld c,0
        ld a,(ix+0)
        cp "."
        jr nz,prpinf6
        ld de,3
        ld b,1
prpinf7 ld a,(iy+0)
        or a
        jr z,prpinf6
                    cp (ix+1):jr nz,prpinf4
        ld a,(iy+1):cp (ix+2):jr nz,prpinf4
        ld a,(iy+2):cp (ix+3):jr z,prpinf5
prpinf4 add iy,de
        inc b
        jr prpinf7
prpinf5 ld c,b
prpinf6 ld b,0
        ld hl,prpinfa
        add hl,bc
        add hl,bc
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (properdsc2b),hl
        ld a,c
        cp 3
        ld hl,propertxt23
        jr z,prpinf8
        cp 4
        ld hl,propertxt27
        jr z,prpinf8
        cp 5
        ld hl,propertxt26
        jr z,prpinf8
        ld hl,propertxt2y
prpinf8 ld (properdsc2c),hl
        ret
prpinf3 ld a,(hl)
        ldi
        or a
        jr nz,prpinf3
        dec de
        ret


;==============================================================================
;### SPECTRUM ANALYZER ########################################################
;==============================================================================

anatab  ds 10,1     ;Volume-Tabelle (1-13) für alle 16/10 Frequenzen
anatab0 ds 10,1

;### ANASWT -> Analyzer an-/ausschalten
anaswt  ld a,(anaflg)
        xor 1
        ld (anaflg),a
        call z,anaoff
        jp prgprz0

;### ANAOFF -> Analyzer löschen
anaoff  ld hl,anatab
        ld de,anatab+1
        ld (hl),1
        ld bc,10+10-1
        ldir
        ld hl,gfx_analyz0+3
        ld de,gfx_analyz1+3
        ld bc,12*10
        ldir
        jr anaplt1

;### ANAPLT -> Plottet Analyzer
anaplt  call anaupd
        ret z
anaplt1 ld e,prgwin_ana
        jp winupd

;### ANAUPD -> Analyzer-Grafik updaten
;### Ausgabe    ZF=1 keine Änderung stattgefunden
anaupdf db 0                    ;Flag, ob Änderung

anaupd  xor a
        ld (anaupdf),a
        ld hl,anatab            ;alte Lautstärken verringern
        ld b,10
anaupd1 dec (hl)
        jr nz,anaupd2
        inc (hl)
anaupd2 inc hl
        djnz anaupd1
        ld a,(snddatchn)
anaupd8 dec a                   ;neue Lautstärken eintragen
        push af
        call anaupd7
        pop af
        jr nz,anaupd8
        ld hl,anatab            ;Grafik mit Differenzen updaten
        ld de,anatab0
        ld b,10
anaupd3 ld a,(de)
        ld c,(hl)
        ldi
        inc c
        sub c
        jr z,anaupd4
        push hl
        push de
        push bc
        ld hl,gfx_analyz0+3
        jr nc,anaupd5
        ld hl,gfx_analyz2+3
        ld d,a
        add c
        ld c,a
        ld a,d
        neg             ;C=Start von unten, A=Länge
anaupd5 ld d,a          ;D=Länge
        ld a,13
        sub c           ;A=Start-Position (0-12)
        add a
        ld c,a
        add a
        add a
        add c
        ld c,a          ;C=Y*10
        ld a,10
        sub b
        add c
        ld c,a          ;C=Y*10+X=Offset
        ld b,0          ;BC=Offset
        ld a,d          ;A=Länge
        ex de,hl
        ld hl,gfx_analyz1+3
        add hl,bc
        ex de,hl        ;DE=Zielgrafik
        add hl,bc       ;HL=Quellgrafik
        ld (anaupdf),a
anaupd6 ldi
        ld bc,-11
        add hl,bc
        ex de,hl
        add hl,bc
        ex de,hl
        dec a
        jr nz,anaupd6
        pop bc
        pop de
        pop hl
anaupd4 djnz anaupd3
        ld a,(anaupdf)
        or a
        ret
anaupd7 call sndval
        ld c,a
        add a
        add c
        add a:add a             ;A=A*12
        srl a:srl a:srl a:srl a ;A=A*12/16
        inc a
        ld e,a
        ld a,l
        add a:add a:add l:add a ;A=A*10
        srl a:srl a:srl a:srl a ;A=A*10/16
        ld l,a
        ld h,0
        ld bc,anatab
        add hl,bc
        ld (hl),e
        ret


;==============================================================================
;### PLAY-LIST ################################################################
;==============================================================================

;### LSTINI -> Playlist init (sets filename pointers)
lstini  ld hl,lstmem
        ld b,lstmax
        ld ix,lstentlst
        ld d,0
lstini1 ld (ix+0),d
        ld (ix+1),d
        ld (ix+2),l
        ld (ix+3),h
        ld e,17
        add hl,de
        ld e,4
        add ix,de
        djnz lstini1
;### LSTNUM -> Playlist renumbering (generates numbers in front of filenames)
lstnum  xor a
        ld ix,lstentlst
lstnum1 ld hl,prgobjlst1
        cp (hl)
        ret nc
        inc a
        ld c,a
        ld l,(ix+2)
        ld h,(ix+3)
        ld de,3
        add hl,de
        ld (hl)," "
        sbc hl,de
        inc e
        add ix,de
        ld e,"0"
lstnum2 cp 100
        jr c,lstnum3
        sub 100
        inc e
        jr lstnum2
lstnum3 ld d,a
        ld a,e
        cp "0"
        jr z,lstnum4
        ld (hl),a
        inc hl
lstnum4 ld a,d
        ex de,hl
        call clcdez
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d
        inc hl
        ld (hl),"."
        ld a,c
        jr lstnum1

;### LSTOPN -> Öffnet Playlist
lstopn  ld hl,lstwin
        ld de,prglstdat
        call winopn0
        ret c
        ld hl,prgwinmen4+2
        set 1,(hl)
        ret

;### LSTSWT -> Schaltet Playliste an/aus
lstswt  ld a,(lstwin)
        inc a
        jr nz,lstswt1
        call lstopn
        jp c,prgprz0
        jp prgfoc2
lstswt1 ld hl,prgwinmen4+2
        res 1,(hl)
        ld hl,lstwin
        jp winclo0

;### LSTUPD -> Baut Liste neu auf inklusive Größe
lstupd  call lstupd0
        ld a,(lstwin)
        ld c,MSC_DSK_WINSLD
        call ctlsta2
        ld a,(lstwin)
        ld c,MSC_DSK_WINDIN
        ld e,0
        jp ctlsta2
lstupd0 ld a,(prgobjlst1)
        or a
        jr nz,lstupd1
        ld a,2
lstupd1 ld l,a
        ld h,0
        add hl,hl
        add hl,hl
        add hl,hl
        inc hl
        inc hl
        ld (prglstobj+16+12),hl
        ld (prglstclc+16+12),hl
        ld (prglstdat+18),hl
        ret

;### LSTREF -> Baut Liste neu auf
lstref  ld e,1
        ld c,MSC_DSK_WINDIN
        ld a,(lstwin)
        jp ctlsta2

;### LSTRSL -> Entfernt selektierte Einträge
lstrsl  ld a,(prgobjlst1)
        sub 1
        jp c,prgprz0
        ld l,a              ;a=last entry
        ld h,0
        add hl,hl:add hl,hl
        ld de,lstentlst+1
        add hl,de           ;hl=row data
lstrsl1 bit 7,(hl)
        jr z,lstrsl2
        push af
        push hl
        ld c,a
        call lstrem
        pop hl
        pop af
lstrsl2 ld de,-4
        add hl,de
        sub 1
        jr nc,lstrsl1
        ld a,(prgobjlst1)
        or a
        jr nz,lstrsl3
        ld (lstpthnum),a
lstrsl3 call lstnum
        call lstupd
        jp lstsno

;### LSTLAS -> finds entry with last string
;### Output     IY=entry, DE=string
lstlas  ld ix,lstentlst     ;ix=entry pointer
        ld a,(prgobjlst1)   ;a=counter
        inc a               ;(was already decreased by lstrem)
        ld de,lstmem        ;de=latest string
        push ix:pop iy      ;iy=entry with latest string
        ld bc,4
lstlas1 ld l,(ix+2)
        ld h,(ix+3)
        or a
        sbc hl,de
        jr c,lstlas2
        add hl,de           ;new stringadr > old
        ex de,hl            ;store as new latest
        push ix:pop iy      ;update entry
lstlas2 add ix,bc
        dec a
        jr nz,lstlas1
        ret

;### LSTREM -> removes one entry
;### Input      C=entry (0-x)
lstrem  ld hl,prgobjlst1
        ld a,(hl)           ;a=total
        sub 1
        ret c               ;no entry -> finish
        ld (hl),a
        sub c
        push af             ;a=total - 1 - entry = entries behind removed
        push bc
        call lstlas         ;iy=entry with latest string, de=latest string
        pop hl
        ld h,0
        add hl,hl
        add hl,hl
        ld bc,lstentlst+2
        add hl,bc           ;hl=dest pointer
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a              ;hl=dest string
        ex de,hl            ;hl=latest string, de=dest string
        ld (iy+2),e
        ld (iy+3),d         ;entry with latest string now points to dest string
        ld bc,17
        ldir                ;copy latest to dest
        pop de
        dec de:dec de
        ld hl,4
        add hl,de           ;hl=area to move
        pop af              ;a=entries behind to move forward
        ret z
        ld c,a
        ld b,0
        sla c:rl b
        sla c:rl b
        ldir                ;move entries forward
        ret

;### LSTRAL -> Entfernt alle Einträge
lstral  xor a
        ld (prgobjlst1),a
        ld (lstpthnum),a
        call lstral1
        jp prgprz0
lstral1 call lstupd
        jp lstref

;### LSTSAL -> Selektiert alle Einträge
lstsal  ld hl,lstentlst+1
        ld b,lstmax
        ld de,4
lstsal1 set 7,(hl)
        add hl,de
        djnz lstsal1
        call lstref
        jp prgprz0

;### LSTSNO -> Deselektiert alle Einträge
lstsno  ld hl,lstentlst+1
        ld b,lstmax
        ld de,4
lstsno1 res 7,(hl)
        add hl,de
        djnz lstsno1
        call lstref
        jp prgprz0

;### LSTSIV -> Invertiert Selektion
lstsiv  ld hl,lstentlst+1
        ld b,lstmax
        ld de,4
lstsiv1 ld a,(hl)
        xor 128
        ld (hl),a
        add hl,de
        djnz lstsiv1
        call lstref
        jp prgprz0

;### LSTEUP -> Selektierte Einträge einen Platz nach oben
lsteup  ld a,(prgobjlst1)
        sub 1
        jp c,prgprz0
        jp z,prgprz0
        ld ix,lstentlst+4
        ld b,a
        ld de,4
        ld a,1
lsteup0 ld (lsteup1+2),a
lsteup1 bit 7,(ix+1)
        jr z,lsteup2
        ld a,(ix+0):ld c,(ix-4):ld (ix-4),a:ld (ix+0),c
        ld a,(ix+1):ld c,(ix-3):ld (ix-3),a:ld (ix+1),c
        ld a,(ix+2):ld c,(ix-2):ld (ix-2),a:ld (ix+2),c
        ld a,(ix+3):ld c,(ix-1):ld (ix-1),a:ld (ix+3),c
lsteup2 add ix,de
        djnz lsteup1
        call lstnum
        call lstref
        jp prgprz0

;### LSTEDW -> Selektierte Einträge einen Platz nach unten
lstedw  ld a,(prgobjlst1)
        sub 1
        jp c,prgprz0
        jp z,prgprz0
        ld b,a
        ld l,a
        ld h,0
        add hl,hl:add hl,hl
        ex de,hl
        ld ix,lstentlst
        add ix,de
        ld de,-4
        ld a,-3
        jr lsteup0

;### LSTADD -> adds filename to list
;### Input      BC=filename (0-terminated), A=pathID
;### Output     CF=1 memory full
lstadd  push af
        ld hl,prgobjlst1
        ld a,(hl)
        cp lstmax
        jr z,lstadd0
        inc (hl)
        ld l,a
        ld h,0
        ld e,l
        ld d,h
        add hl,hl
        add hl,hl           ;*4
        push hl
        add hl,hl
        add hl,hl
        add hl,de           ;*17
        ld de,lstmem
        add hl,de
        push hl
        ld de,4
        add hl,de
        ex de,hl            ;de=dest string
        ld l,c
        ld h,b
        ldi
        ld b,12
lstadd1 ld a,(hl)
        call clclcs
        ld (de),a
        inc hl
        inc de
        djnz lstadd1
        pop de
        pop hl
        pop af
        ld bc,lstentlst
        add hl,bc
        ld (hl),a           ;store path ID
        inc hl:inc hl
        ld (hl),e           ;store string address
        inc hl
        ld (hl),d
        ret
lstadd0 pop af
        scf
        ret

;### LSTAFI -> Fügt Datei hinzu
lstafim db 0                    ;0=Datei hinzufügen, 1=Verzeichnis hinzufügen
lstafi  ld c,0
        ld a,(prgobjlst1)
        cp lstmax
        jr c,lstafi4
        ld hl,daterrlfe
        call lstafi1
        jp prgprz0
lstafi4 ld a,c
        ld (lstafim),a
        ld hl,lstmsk
        ld a,(App_BnkNum)
        rrc c
        or c
        ld c,8
        ld ix,100
        ld iy,10000
        ld de,prgwindat
        call SySystem_SELOPN
        or a
        jp nz,prgprz0
        call lstafi0
        jp prgprz0
lstafi0 ld hl,lstfil
        push hl
        call ctlnam0            ;HL=Pfad -> HL=Name (directly behind "/")
        pop de                  ;DE=Pfad
        push hl
        or a
        sbc hl,de
        ld c,l                  ;c=pathlen including "/"
        pop hl
        ret z                   ;kein Pfad gefunden -> Fehler, nicht hinzufügen
        push hl
        ex de,hl
        call pthfnd             ;A=path number (0-x), DE=path, CF=1 memory full
        pop bc                  ;bc=name
        ld hl,daterrlfp
        jr c,lstafi1            ;too many pathes
        ld hl,lstafim
        dec (hl)
        jr z,lstadi1
        call lstadd             ;add single file
        ld hl,daterrlfe
        jr c,lstafi1            ;too many entries
        call lstnum
        jp lstral1
lstafi1 ld b,1+64
        jp prginf0

;### LSTADI -> Fügt Verzeichnis hinzu
dirmsk  db "skmst2binmp3pt3modsa2",0
lstadib ds 32
lstadi  ld c,1
        jp lstafi4
lstadi1 push de
        ld (lstadi7+1),a        ;store path ID
        xor a                   ;*** Speicher für Dir reservieren
        ld e,a
        ld bc,5500
        push bc
        rst #20:dw jmp_memget
        pop bc
        pop de
        ret c
        ld (prgmemtab+0),a
        ld (prgmemtab+1),hl
        ld (prgmemtab+3),bc
        ex de,hl                ;*** Directory laden
        ld ix,(App_BnkNum-1)
        db #dd:ld l,16+8
        ld iy,0
        call SyFile_DIRINP
        jp c,lstadi0
        ld c,l
        ld b,h              ;BC=Anzahl Einträge
        ld hl,(prgmemtab+1) ;HL=Zeiger auf Daten
lstadi2 push bc
        push hl
        ld de,lstadib           ;*** Eintrag aus Directory holen
        ld a,(prgmemtab+0)
        ld c,a
        ld a,(App_BnkNum)
        add a:add a:add a:add a
        add c
        ld bc,32
        rst #20:dw jmp_bnkcop

        ld hl,lstadib+9         ;*** Test, ob unterstützte Erweiterung
lstadi8 ld a,(hl)
        inc hl
        or a
        jr z,lstadi6    ;kein Punkt gefunden -> kein Extension, also kein gültiges File
        cp "."
        jr nz,lstadi8
        push hl
        pop ix
        ld hl,dirmsk-1
lstadi9 inc hl
        ld a,(hl)       ;Test, ob gültige Filemaske
        or a
        jr z,lstadi6    ;keine gültige Filemaske gefunden -> Ende
        ld c,a
        inc hl
        ld b,(hl)
        inc hl
        ld a,(ix+2)
        call clclcs
        cp (hl)
        jr nz,lstadi9
        ld a,(ix+0)
        call clclcs
        cp c
        jr nz,lstadi9
        ld a,(ix+1)
        call clclcs
        cp b
        jr nz,lstadi9

        ld bc,lstadib+9         ;*** gültigen Eintrag Liste hinzufügen
lstadi7 ld a,0
        call lstadd
lstadi6 ld hl,lstadib+9
        call strlen
        ld hl,9+1
        add hl,bc
        pop bc
        add hl,bc
        pop bc
        ld a,(prgobjlst1)
        cp lstmax
        jr z,lstadi5
        dec bc
        ld a,c
        or b
        jr nz,lstadi2
lstadi5 call lstnum
        xor a
        ld (prgobjlst1+12),a
        ld l,a
        ld h,a
        ld (prglstdat+14),hl
        call lstadi0
        jp lstral1
lstadi4 call lstadi0
        jp prgprz0
lstadi0 ld a,(prgmemtab+0)
        ld hl,(prgmemtab+1)
        ld bc,(prgmemtab+3)
        rst #20:dw jmp_memfre
        xor a
        ld l,a
        ld h,a
        ld (prgmemtab+0),a
        ld (prgmemtab+1),hl
        ld (prgmemtab+3),hl
        ret

;### LSTCLK -> Doppelklick auf die Songliste
lstclk  cp 2
        jp nz,prgprz0
lstclk1 call lstakt
        jp c,prgprz0
        jp ctlopn1

;### LSTCHK -> Prüft, ob Liste Einträge hat
;### Ausgabe    ZF=0 Liste hat Einträge, A=Anzahl, CF=1
lstchk  ld a,(prgobjlst1)
        or a
        scf
        ret

;### LSTNXT -> Holt Pfad des nächsten Eintrages in der Playliste
;### Ausgabe    CF=0 -> (sndfil)=File, ZF=1 wieder am Anfang
;###            CF=1 -> kein Eintrag vorhanden
lstnxt  call lstchk
        ret z
        ld e,a
        ld a,(prgobjlst1+12)
        inc a
        cp e
        jr c,lstnxt1
        xor a
        scf
lstnxt1 push af
        call lstakt1
        pop af
        ccf
        ret

;### LSTPRV -> Holt Pfad des vorherigen Eintrages in der Playliste
;### Ausgabe    CF=0 -> (sndfil)=File, CF=1 -> kein Eintrag vorhanden
lstprv  call lstchk
        ret z
        ld e,a
        ld a,(prgobjlst1+12)
        sub 1
        jr nc,lstakt1
        ld a,e
        dec a
        jr lstakt1

;### LSTRND -> Holt Pfad eines zufälligen Eintrages in der Playliste
;### Ausgabe    CF=0 -> (sndfil)=File, CF=1 -> kein Eintrag vorhanden
lstrnd  call lstchk
        ret z
        dec a
        jr z,lstakt1
        inc a
        ld e,a
        ld d,0
lstrnd1 push de
        ld a,r
        and 127
        call clcm16
        pop de
        sla l
        rl h
        ld a,(prgobjlst1+12)
        cp h
        jr z,lstrnd1
        ld a,h
        jr lstakt1

;### LSTAKT -> Holt Pfad des aktuellen Eintrages in der Playliste
;### Ausgabe    CF=0 -> (sndfil)=File, CF=1 -> kein Eintrag vorhanden
lstakt  call lstchk
        ret z
        ld a,(prgobjlst1+12)
lstakt1 ld (prgobjlst1+12),a
        push af
        ld l,a
        ld h,0
        add hl,hl:add hl,hl:add hl,hl       ;HL=Position in Fenster
        inc hl
        ld e,l
        ld d,h
        ld bc,(prglstdat+14)
        or a
        sbc hl,bc
        ld bc,(prglstdat+48)
        jr c,lstakt3
        push bc
        dec bc:dec bc:dec bc:dec bc:dec bc:dec bc:dec bc
        sbc hl,bc
        pop bc
        jr c,lstakt4
lstakt3 ld hl,(prglstdat+18)
        sbc hl,bc
        jr c,lstakt5
        sbc hl,de
        jr nc,lstakt6
        add hl,de
        ex de,hl
        jr lstakt6
lstakt5 ld de,0
lstakt6 ld (prglstdat+14),de
        ld c,MSC_DSK_WINSLD
        ld a,(lstwin)
        call ctlsta2
lstakt4 ld e,1
        ld c,MSC_DSK_WINDIN
        ld a,(lstwin)
        call ctlsta2
        pop af
        push af
        ld bc,lstentlst
        call lstakt7    ;a=pathID
        call pthget     ;hl=path
        ld de,sndfil
lstakt2 ld a,(hl)
        ldi
        or a
        jr nz,lstakt2
        dec de
        pop af
        call lstakt0
        ld bc,4
        add hl,bc
        ld c,13
        ldir
        or a
        ret
lstakt0 ld bc,lstentlst+2   ;a=entry -> hl=entry text (num+filename) pointer
lstakt7 ld l,a          
        ld h,0
        add hl,hl
        add hl,hl
        add hl,bc
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ret

;### LSTSRT -> sort songlist
lstsrtx db "xxxx"

lstsrt  ld de,lstmem
        ld a,lstmax
lstsrt1 ld hl,lstsrtx
        ld bc,4
        ldir
        ld hl,17-4
        add hl,de
        ex de,hl
        dec a
        jr nz,lstsrt1
        ld hl,-5000
        ld a,64
        call lstsrt2
        rst #30
        call lstnum
        xor a
        ld l,a
        ld h,a
        call lstsrt2
        jp prgprz0
lstsrt2 ld (prglstobj+16+6),hl
        ld (prgobjlst1+9),a
        ld e,1
        ld a,(lstwin)
        jp SyDesktop_WINDIN

;### LSTLOD -> Lädt Songliste
lstlod  ld hl,splmsk
        ld a,(App_BnkNum)
        ld c,8
        ld ix,200
        ld iy,4000
        ld de,prgwindat
        call SySystem_SELOPN
        or a
        jp nz,prgprz0
        ld a,(App_BnkNum)    ;File öffnen
        db #dd:ld h,a
        ld hl,splfil
        call SyFile_FILOPN   ;IXH,HL=Dateiangabe
        jp c,lstsav1
        push af
        ld de,(App_BnkNum)   ;File laden
        ld hl,lstfilid
        ld bc,lstmax*17+lstmem-lstfilid
        call SyFile_FILINP
        pop bc
        push af
        ld a,b              ;File schließen
        call SyFile_FILCLO
        pop af
        ld a,(lstanz)
        jr nc,lstlod1
        call prgerr
        xor a
lstlod1 ld (prgobjlst1),a
        jr z,lstlod3

        ld hl,lstmem        ;filename string address "relocation"
        ld de,(lstmemofs)
        ld (lstmemofs),hl
        or a
        sbc hl,de
        ex de,hl
        ld ix,lstentlst
        ld bc,4
lstlod2 ld l,(ix+2)
        ld h,(ix+3)
        add hl,de
        ld (ix+2),l
        ld (ix+3),h
        add ix,bc
        dec a
        jr nz,lstlod2

lstlod3 call lstupd
        jp lstsno

;### LSTSAV -> Speichert Songliste
lstsav  ld hl,splmsk
        ld a,(App_BnkNum)
        set 6,a
        ld c,8
        ld ix,200
        ld iy,4000
        ld de,prgwindat
        call SySystem_SELOPN
        or a
        jp nz,prgprz0
        ld a,(prgobjlst1)
        ld (lstanz),a
        ld a,(App_BnkNum)    ;File erstellen
        db #dd:ld h,a
        xor a
        ld hl,splfil
        call SyFile_FILNEW   ;IXH,HL=Dateiangabe, A=Attribut
        jr c,lstsav1
        push af
        ld de,(App_BnkNum)   ;File speichern
        ld hl,lstfilid
        ld bc,lstmax*17+lstmem-lstfilid
        call SyFile_FILOUT
        pop bc
        push af
        ld a,b              ;File schließen
        call SyFile_FILCLO
        pop af
lstsav1 call c,prgerr
        jp prgprz0


;==============================================================================
;### PATH ROUTINES ############################################################
;==============================================================================

;- todo
;if memful, user is informed to clear list
;clear list/all removed -> clear pthbuf

;### PTHFND -> searches for path, tries to add a new, if not found
;### Input      HL=path string (end with "/"), C=path len (including final "/")
;### Output     CF=0 -> A=path number (0-x), DE=path string
;###            CF=1 -> not found and memory full
pthfnd  ld de,lstpthbuf
        ld b,c
        ld a,(lstpthnum)
        or a
        jr z,pthfnd5
        ld c,a
pthfnd1 push bc
        push hl
        push de
pthfnd2 ld a,(de)           ;check, if path matches with existing
        cp (hl)
        jr nz,pthfnd3
        inc de
        inc hl
        djnz pthfnd2
        ld a,(de)           ;check, if existing ends as well
        or a
        jr nz,pthfnd3
        pop de              ;de=path
        pop hl
        pop bc
        ld a,(lstpthnum)    ;found -> return index
        sub c
        ret
pthfnd3 pop hl
        xor a
        ld c,a
        ld b,a
        cpir                ;not found -> move to next path
        ex de,hl
        pop hl
        pop bc
        dec c
        jr nz,pthfnd1
pthfnd5 push hl
        ld hl,lstpthend
        or a
        sbc hl,de           ;hl=remaining bytes in path buffer (without 0-terminator)
        inc h:dec h
        jr nz,pthfnd4
        ld a,b
        cp l
        pop hl
        ccf
        ret c               ;required >= remaining-1 -> memory full
        push hl
pthfnd4 pop hl
        xor a
        push de
        ld c,b
        ld b,a
        ldir
        ld (de),a
        ld hl,lstpthnum
        ld a,(hl)
        inc (hl)
        pop de
        ret

;### PTHGET -> gets path
;### Input      A=path number (0-x)
;### Output     HL=path (ends with "/", 0-terminated)
pthget  ld hl,lstpthbuf
        ld e,a
        xor a
        cp e
pthget1 ret z
        ld c,a
        ld b,a
        cpir
        dec e
        jr pthget1


;==============================================================================
;### SUB WINDOWS ##############################################################
;==============================================================================

;### WINUPD -> updates controls in main window
;### Input      (D)E=controls
winupd  ld a,(prgwin)
        jp SyDesktop_WINDIN

;### WINOPN -> (HL)=winID, DE=win record -> opens windows, if not already opened
winopn  call winopn0
        jp prgprz0
winopn0 ld a,(hl)
        inc a
        ret nz
        ld a,(App_BnkNum)
        push hl
        call SyDesktop_WINOPN
        pop hl
        ret c
        ld (hl),a
        ret
;### WINCLO -> (HL)=winID -> closes windows, if opened
winclo  ld a,(hl)
        inc a
        jp z,prgprz0
winclo0 dec a
        ld (hl),-1
        call SyDesktop_WINCLS
        jp prgprz0

;### PRFOPN -> opens preferences window
prfopn  ld hl,prfwin
        ld de,prgwinprf
        jr winopn
;### PRFCLO -> closes preferences window
prfclo  ld hl,prfwin
        jr winclo

;### MIXOPN -> opens equalizer
mixopn  ld hl,mixwin
        ld de,prgwinmix
        call winopn0
        ret c
        ld hl,prgwinmen4+2+8
        set 1,(hl)
        ret

;### MIXSWT -> switches equalizer on/off
mixswt  ld a,(mixwin)
        inc a
        jr nz,mixswt1
        call mixopn
        jp c,prgprz0
        jp prgfoc2
mixswt1 ld hl,prgwinmen4+2+8
        res 1,(hl)
        ld hl,mixwin
        jr winclo0

;### MIXVOL -> volume clicked
mixvol  ld hl,sndset_vol
        call mixclk
        call mixvold
        call ctlvup             ;show in main
        jp volupd0              ;set and send to daemon
mixvold call mixvole
        jp mixshw
mixvole ld a,(sndset_vol)       ;display volume
        ld ix,prgmix_vol*16+prgdatmix
        ld bc,256*prgmix_volb+prgmix_vol
        ret

;### MIXBAS -> bass clicked
mixbas  ld hl,sndset_bas
        call mixclk
        call mixbasd
        jr mixtre0
mixbasd ld a,(sndset_bas)       ;display bass
        ld ix,prgmix_bas*16+prgdatmix
        ld bc,256*prgmix_basb+prgmix_bas
        jp mixshw

;### MIXTRE -> treble clicked
mixtre  ld hl,sndset_tre
        call mixclk
        call mixtred
mixtre0 call mixset
        jp prgprz0
mixtred ld a,(sndset_tre)       ;display treble
        ld ix,prgmix_tre*16+prgdatmix
        ld bc,256*prgmix_treb+prgmix_tre
        jp mixshw

;### MIXPAN -> panning clicked
mixpan  ld e,(iy+4)
        ld d,(iy+5)
        ld bc,-6-6
        call mixclk0
        ld (sndset_pan),a
        call mixpand
        jr mixtre0
mixpand ld a,(sndset_pan)
        ld ix,prgmix_pan*16+prgdatmix
        ld bc,256*prgmix_panb+prgmix_pan
        call winslp
        ld de,6
        add hl,de
        push bc
        call winslh0
        ld a,(mixwin)
        call SyDesktop_WINPIN
        pop de
        ld a,(mixwin)
        jp SyDesktop_WINDIN

;### MIXSM0-3
mixsm0  xor a
        jr mixsms
mixsm1  ld a,1
        jr mixsms
mixsm2  ld a,2
        jr mixsms
mixsm3  ld a,3
mixsms  ld (sndset_ste),a
        ld hl,(prgmix_smd*16+24+prgdatmix)
        ld (prgmix_smd*16+08+prgdatmix),hl
        add a
        ld l,a
        add a:add a:add a
        sub l
        add 8
        ld l,a
        ld (prgmix_smd*16+24+prgdatmix),hl
        ld de,256*prgmix_smd+256-2
        ld a,(mixwin)
        call SyDesktop_WINDIN
        jr mixtre0

;### MIXCLK -> vertical mixer clicked
;### Input      HL=variable
mixclk  ld e,(iy+6)
        ld d,(iy+7)
        ld bc,-11-6
        call mixclk0
        cpl
        ld (hl),a
        ret
mixclk0 push hl
        ex de,hl
        add hl,bc           ;HL=Ypos auf Slider
        jr c,mixclk1
        ld l,0
mixclk1 ld b,l
        ld c,0
        ld de,48-12
        call clcd16         ;HL=sldpos*256/sldlen
        dec h
        jr nz,mixclk2
        ld l,255
mixclk2 ld a,l
        pop hl
        ret

;### MIXSHW -> shows vertical mixer setting
;### Input      A=value, IX=record, B=background control, C=slider control
mixshw  call mixshw0
        jp winslv
mixshw0 cpl
        call winslp
        ld de,11
        add hl,de
        ret

;### MIXSET -> Setzt erweiterte Mixer-Einstellungen
mixset  ld a,(sndset_bas)
        ld b,a
        ld a,(sndset_tre)
        ld c,a
        ld a,(sndset_ste)
        ld e,a
        ld a,(sndset_pan)
        jp mp3ext

;### DIAOPN -> DE=data record -> opens dialoge window
diaopn  ld a,(App_BnkNum)
        call SyDesktop_WINOPN
        jp c,prgprz0
        ld (diawin),a
        inc a
        ld (prgwindat+windatsup),a
        jp prgprz0

;### DIACNC -> closes dialoge window
diacnc  ld hl,diawin
        jp winclo


;==============================================================================
;### CONTROLS #################################################################
;==============================================================================

snddattyp   db 0    ;module type (0=Starkos, 1=SoundTrakker, 2=MP3, 3=PT3)
snddatlen   dw 0    ;number of songlist positions
snddatchn   db 0    ;number of channels (1, 3, 4, 6)

ctltim  db 0,0,0    ;1B seconds (0-59), 2B minutes (0-999)
ctlskp  dw 0

;### CTLINC -> Spielzeit erhöhen
ctlincs db 0            ;Flag, ob weiterspielen
ctlinc  ld a,(iy+1)
        inc a
        jr z,ctlinc4
        dec a
        jr z,ctlinc3
        dec a
        jp nz,prgprz0
        ld a,(snddattyp)    ;*** Timer
        cp 2
        jr z,ctlinc9
        ld a,(ctltim+0)
        inc a
        cp 60
        jr c,ctlinc2
        ld de,(ctltim+1)
        inc de
        ld hl,999
        sbc hl,de
        jr nc,ctlinc1
        ld de,0
ctlinc1 ld (ctltim+1),de
        xor a
ctlinc2 ld (ctltim+0),a
ctlinc9 call ctltup
        jp prgprz0
ctlinc3 ld e,(iy+2)         ;*** Position
        ld d,(iy+3)
        call ctlpup
        jp prgprz0
ctlinc4 ld a,1              ;*** Ende
        ld (ctlincs),a
        ld a,(ctlshuf)
        or a
        jr z,ctlinc5
        call lstrnd
        jr c,ctlinc7        ;kein eintrag, selben sound evtl.repeaten
        jr ctlinc6
ctlinc5 call lstnxt
        jr c,ctlinc7        ;kein eintrag, selben sound evtl.repeaten
        jr nz,ctlinc6
        ld a,(ctlrepf)
        or a
        jr nz,ctlinc6
        ld (ctlincs),a      ;Ende erreicht + kein Repeat -> Stop
ctlinc6 call sndlod
        push af
        call ctlnam
        pop af
        jp c,ctlstp         ;Laden gescheitert -> Stop
        ld a,(ctlincs)
ctlinc8 or a                ;Play oder Stop, je nach Stop-Flag
        jp nz,ctlply
        jp ctlstp
ctlinc7 ld a,(ctlrepf)
        jr ctlinc8

;### CTLPUP -> Position anzeigen
;### Eingabe    DE=Position
ctlpup  ld bc,124-12
        call clcmul     ;A,HL=sngpos*sldlen
        ld c,l
        ld b,h
        ld de,(snddatlen)
        call clcdiv     ;HL=sngpos*sldlen/snglen=sldpos
        ex de,hl
ctlpup1 ld hl,-1
        or a
        sbc hl,de
        ret z
        ld (ctlpup1+1),de
        ld hl,4
        add hl,de
        ld ix,prgwin_pos*16+prgwinobj
        ld bc,256*prgwin_barp+prgwin_pos
        jp winslh

;### WINSLH -> updates h-slider position
;### Input      IX=slider record, HL=new x-position, B=background control, C=slider control
winslh  push bc
        call winslh0
        ld a,(prgwin)
        call SyDesktop_WINPIN
        pop de
        jp winupd
winslh0 ld e,(ix+6)
        ld d,(ix+7)
        ld (ix+6),l
        ld (ix+7),h
        ex de,hl        ;hl=x
        ld e,b
        ld c,(ix+8)
        ld b,(ix+9)     ;bc=y
        ld ix,12
        ld iy,6
        ret

;### WINSLV -> updates v-slider position
;### Input      IX=slider record, HL=new y-position, B=background control, C=slider control
winslv  push bc
        ld e,b
        ld c,(ix+8)
        ld b,(ix+9)     ;bc=old y
        call winslv0
        ld l,(ix+6)
        ld h,(ix+7)     ;hl=x
        ld ix,8
        ld iy,12
        ld a,(mixwin)
        push af
        call SyDesktop_WINPIN
        pop af
        pop de
        jp SyDesktop_WINDIN
winslv0 ld (ix+8),l
        ld (ix+9),h     ;set new y
        ret

;### WINSLP -> calculates slider position
;### Input      A=value
;### Output     HL=value*sldlen/256=sldpos
winslp  inc a
        ld de,48-12
        ld h,e
        jr z,winslp1
        dec a
        call clcm16
winslp1 ld l,h
        ld h,0
        ret

;### CTLVUP -> Volume anzeigen
ctlvup  call ctlvup0
        jp winslh
ctlvup0 ld a,(sndset_vol)
        call winslp
        ld de,96
        add hl,de
        ld ix,prgwin_vol*16+prgwinobj
        ld bc,256*prgwin_barv+prgwin_vol
        ret

;### CTLTUP -> Spielzeit anzeigen
ctltup  ld a,(ctltim+0)
        call clcdez
        ld (prgwintxt1+4),hl
        ld hl,(ctltim+1)
        ld de,100
        xor a
ctltup1 inc a
        sbc hl,de
        jr nc,ctltup1
        add "0"-1
        cp "0"
        jr nz,ctltup2
        ld a,"/"
ctltup2 ld (prgwintxt1+0),a
        ld a,l
        add 100
        call clcdez
        ld (prgwintxt1+1),hl
        ld e,prgwin_tim
        jp winupd

;### CTLSTA -> Spielstatus updaten
;### Eingabe    HL=Grafik
ctlsta  ld (prgwin_sta*16+prgwinobj+4),hl
        ld e,prgwin_sta
        jp winupd

ctlsta2 or a        ;##!!##replace
        ret z
        ld b,a
        jp msgsnd

;### CTLNRS -> Löscht Songnamen
ctlnrs  ld hl,prgwintxt0
        ld (prgwindsc2),hl
        ret

;### CTLNAM -> Setzt Songnamen
ctlnam  ld a,(prgobjlst1+12)
        call lstakt0
        ld de,lstnam
        ld (prgwindsc2),de
        ld bc,17
        ldir
        ld hl,4-17
        add hl,de
        ld a,(sndtxt_titl+0):cp "-":jr nz,ctlnam3   ;do we have a real title?
        ld a,(sndtxt_titl+1):or a  :jr z,ctlnam2
ctlnam3 ex de,hl
        ld hl,sndtxt_titl   ;yes, use this
        ld bc,30
        ldir
        xor a
        ld (de),a
        ld e,prgwin_tit
        jp winupd
ctlnam2 ld a,(hl)
        call clcucs
        ld (hl),a
        ld e,prgwin_tit
ctlnam1 inc hl
        ld a,(hl)
        or a
        jp z,winupd
        call clclcs
        ld (hl),a
        jr ctlnam1
;HL=Pfad -> HL=Name=Pfadende+2
ctlnam0 ld e,l
        ld d,h
ctlnama ld a,(hl)
        inc hl
        cp "/"
        jr z,ctlname
        cp "\"
        jr nz,ctlnamb
ctlname ld e,l
        ld d,h
ctlnamb or a
        jr nz,ctlnama
        ex de,hl        ;HL=Name
        ret

;### CTLPLY -> Sound abspielen
ctlply  ld a,(sndinif)
        or a
        jp z,lstclk1
        ld hl,gfx_sta_play
        call ctlsta
        call sndply
        jp prgprz0

;### CTLPAU -> Sound anhalten
ctlpau  ld a,(sndinif)
        or a
        jp z,prgprz0
        ld hl,(prgwin_sta*16+prgwinobj+4)
        ld de,gfx_sta_paus
        sbc hl,de
        jr z,ctlply
        ex de,hl
        call ctlsta
        call sndpau
        jp prgprz0

;### CTLSTP -> Sound stoppen
ctlstp  call sndstp
        jp prgprz0

;### CTLSKF -> Skip forward
ctlskf  ld hl,(prgtimp)
        ld de,(ctlskp)
        add hl,de
        ex de,hl
        ld hl,(snddatlen)
        dec hl
        sbc hl,de
        ex de,hl
        jr nc,ctlpos2
        add hl,de
        jr ctlpos2

;### CTLSKB -> Skip backward
ctlskb  ld hl,(prgtimp)
        ld de,(ctlskp)
        inc de
        or a
        sbc hl,de
        jr nc,ctlpos2
        ld hl,0
        jr ctlpos2

;### CTLPOS -> Position setzen
ctlpos  ld l,(iy+4)
        ld h,(iy+5)
        ld de,-4-6
        add hl,de           ;HL=Xpos auf Slider
        ld b,0
        jr c,ctlpos1
        ld l,b
ctlpos1 ld c,l
        ld de,(snddatlen)
        call clcmul         ;A,HL=sldpos*snglen
        ld c,l
        ld b,h
        ld de,124-12
        call clcdiv         ;HL=sldpos*snglen/sldlen
ctlpos2 push hl
        call sndpos
        pop de
        call ctlpup
        jp prgprz0

;### CTLVLU -> Increase Volume
ctlvlu  ld a,(sndset_vol)
        add 8
        jr nc,ctlvol3
        ld a,-1
        jr ctlvol3

;### CTLVLD -> Decrease Volume
ctlvld  ld a,(sndset_vol)
        sub 8
        jr nc,ctlvol3
        xor a
        jr ctlvol3

;### CTLVOL -> Volume setzen
ctlvol  ld l,(iy+4)
        ld h,(iy+5)
        ld de,-96-6
        add hl,de           ;HL=Xpos auf Slider
        jr c,ctlvol1
        ld l,0
ctlvol1 ld b,l
        ld c,0
        ld de,48-12
        call clcd16         ;HL=sldpos*256/sldlen
        dec h
        jr nz,ctlvol2
        ld l,255
ctlvol2 ld a,l
ctlvol3 ld (sndset_vol),a   ;** set new volume
        call volupd         ;show in displays
        jr volupd0          ;set and send to daemon

;### VOLUPD -> update volume
volupd  ld a,(mixwin)
        inc a
        push af
        call nz,mixvold     ;vis -> update and show in mixer
        pop af
        call z,volupd2      ;unvis -> only update in mixer
        jp ctlvup           ;show in mainwindow
volupd0 ld a,(SySound_PrcID)
        or a
        jr z,volupd1
        ld a,2
        ld hl,(sndset_vol)  ;send to daemon
        call SySound_RMTCTR
volupd1 ld a,(sndset_vol)
        call sndvol         ;set for songs
        jp prgprz0
volupd2 call mixvole        ;update in mixer
        call mixshw0
        jp winslv0

;### CTLRMT -> Remote control
;### Input      (App_MsgBuf+1)=type (1=volume), (App_MsgBuf+2)=new volume
ctlrmt  ld a,(App_MsgBuf+1)
        cp 1
        jp nz,prgprz0
        ld a,(App_MsgBuf+2)     ;new volume
        ld (sndset_vol),a
        call volupd             ;show in displays
        jr volupd1              ;set volume

;### CTLREW -> Ein Lied zurück
ctlrew  ld a,(ctlshuf)
        or a
        jr z,ctlrew1
ctlrew0 call lstrnd
        jr ctlrew2
ctlrew1 call lstprv         ;CF=0 -> (sndfil)=File, CF=1 -> kein Eintrag vorhanden
ctlrew2 jp c,prgprz0
        ld hl,(prgwin_sta*16+prgwinobj+4)
        ld de,gfx_sta_play
        sbc hl,de
        jr z,ctlopn1
        call sndlod
        push af
        call ctlnam
        pop af
        jp ctlstp

;### CTLFFW -> Ein Lied weiter
ctlffw  ld a,(ctlshuf)
        or a
        jr nz,ctlrew0
        call lstnxt         ;CF=0 -> (sndfil)=File, ZF=1 am Anfang, CF=1 -> kein Eintrag vorhanden
        jr ctlrew2

;### CTLOPN -> neuen Sound laden
ctlopn  ld hl,sndmsk
        ld a,(App_BnkNum)
        ld c,8
        ld ix,200
        ld iy,4000
        ld de,prgwindat
        call SySystem_SELOPN
        push af
        call ctlnrs
        pop af
        or a
        jp nz,prgprz0
ctlopn0 xor a
        ld (prgobjlst1),a
        ld (prgobjlst1+12),a
        ld hl,sndfil
        ld de,lstfil
        ld bc,256
        ldir
        xor a
        ld (lstafim),a
        ld l,a
        ld h,a
        ld (prglstdat+14),hl
        call lstafi0            ;Songliste löschen und Sound hinzufügen
ctlopn1 call sndlod
        push af
        call ctlnam
        pop af
        jp c,ctlstp
        jp ctlply

;### CTLREP -> Repeat an/aus
ctlrep  ld hl,ctlrepf
        ld bc,prgwin_rep*16+4+prgwinobj
        ld ix,prgwinmen2a
        ld e,prgwin_rep
ctlrep1 ld a,(hl)
        xor 1
        ld (hl),a
        call ctlrep0
        call winupd
        jp prgprz0
ctlrep0 ld a,(hl)       ;off = 8, on = 15
        add a
        ld d,a
        ld a,(ix+0)
        and 255-2
        or d
        ld (ix+0),a
        ld a,(hl)
        neg
        and 7
        add 8+128
        ld (bc),a
        ret

;### CTLSHU -> Shuffle an/aus
ctlshu  ld hl,ctlshuf
        ld bc,prgwin_shu*16+4+prgwinobj
        ld ix,prgwinmen2b
        ld e,prgwin_shu
        jr ctlrep1

;==============================================================================
;### SOUND-PLAY ###############################################################
;==============================================================================

plugins_beg
READ "App-SymAmp-Drivers.asm"
READ "App-SymAmp-SKM.asm"
READ "App-SymAmp-ST2.asm"
READ "App-SymAmp-PT3.asm"
READ "App-SymAmp-MP3.asm"
READ "App-SymAmp-MOD.asm"
READ "App-SymAmp-SA2.asm"
list
plugins_len equ $-plugins_beg
nolist

;### SNDPAR -> Angehängten Sound suchen
sndpar  ld hl,(App_BegCode)       ;nach angehängtem Sound suchen
        ld de,App_BegCode
        dec h
        add hl,de               ;HL=CodeEnde=Pfad
        ld b,255
sndpar1 ld a,(hl)
        or a
        jp z,prgprz0
        cp 32
        jr z,sndpar2
        inc hl
        djnz sndpar1
        jp prgprz0
sndpar2 inc hl
        ld de,sndfil
        ld bc,256
        ldir
        jp ctlopn0

;### SNDUNL -> unloads extra data of current song
sndxtrbnk   equ prgmemtab+5+0     ;bank 1-15, 0=no extra data loaded
sndxtradr   equ prgmemtab+5+1     ;adr of extra song data
sndxtrlen   equ prgmemtab+5+3     ;len of extra song data

sndunl  ld a,(sndxtrbnk)
        or a
        ret z
        ld hl,(sndxtradr)
        ld bc,(sndxtrlen)
        rst #20:dw jmp_memfre
        xor a
        ld (sndxtrbnk),a
        ret

;### SNDLOD -> Lädt und initialisiert Modul
;### Eingabe    (sndfil)=File
;### Ausgabe    CF=0 ok, CF=1 Fehler
sndlodskm   db "SK10"
sndlodst2   db "ST-128"
sndlodmp3   db "MP3"
sndlodpt3   db "PT3"
sndlodmod   db "MOD"
sndlodsa2   db "SA2"
sndlodcpr   db "SYMZX0"
sndlodbuf   ds 8

sndlodlen   dw 0    ;länge der geladenen datei
sndlodt     db 0    ;typ bereits durch endung bekannt

sndlod  xor a
        ld (sndlodt),a
        call sndpau         ;stop current song
        call sndres
        call mp3del
        call dmnfre
        call sndlod0
        ld hl,256*":"+"/"
        ld (prgwintxt3),hl
        ld (prgwintxt4),hl
        ld hl,256*0+"/"
        ld (prgwintxt3+2),hl
        ld (prgwintxt4+2),hl
        call sndunl
        ld de,0             ;search for extension
        ld hl,sndfil
        ld b,255
sndlod6 ld a,(hl)
        inc hl
        or a
        jr z,sndlod8
        cp "."
        jr nz,sndlod7
        ld e,l
        ld d,h
sndlod7 djnz sndlod6
sndlod8 ld a,e              ;de="."position
        or d
        jr z,sndloda
        ex de,hl
        push hl
        ld de,sndlodmp3
        ld b,3
        call strcmp
        pop hl
        jr nz,sndlodb
        ld hl,sndfil
        ld de,sndmem
        ld bc,256
        ldir
        ld a,2
        jp sndlod3
sndlodb push hl
        ld de,sndlodpt3
        ld b,3
        call strcmp
        pop hl
        jr nz,sndlodc
        ld a,3	            ;3=PT3
        ld (sndlodt),a
        jp sndloda
sndlodc push hl
        ld de,sndlodmod
        ld b,3
        call strcmp
        pop hl
        jr nz,sndlodd
        call dwtbeg             ;* stop daemon, if possible
        jp c,sndlode
        call modlod
        jp c,sndlode
        ld a,4	            ;4=MOD
        ld (sndlodt),a
        jp sndlod3
sndlodd push hl
        ld de,sndlodsa2
        ld b,3
        call strcmp
        pop hl
        jr nz,sndloda
        call dwtbeg             ;* stop daemon, if possible
        jp c,sndlode
        ld a,5	            ;5=SA2
        ld (sndlodt),a

sndloda ld a,(App_BnkNum)   ;File öffnen
        db #dd:ld h,a
        ld hl,sndfil
        call SyFile_FILOPN
        jp c,sndlod1

        push af
        ld de,(App_BnkNum)  ;test, if compressed
        ld hl,sndlodbuf
        ld bc,8
        call SyFile_FILINP
        pop bc
        jp c,sndlodg
        ld c,b
        ld hl,sndlodbuf
        ld de,sndlodcpr
        ld b,6
        call strcmp
        ld a,c
        jr nz,sndlodh
        ld hl,(sndlodbuf+6)     ;** compressed
        ld (sndlodlen),hl
        ld de,-sndmax
        add hl,de
        bit 0,b
        ccf
        jr nc,sndlodi
        push af
        ld hl,sndmem
        ld bc,sndmax
        ld de,(App_BnkNum)
        call SyFile_FILCPR
        pop bc
        ld c,1
        bit 0,c
        jr sndlodi
sndlodh ld ix,0                 ;** uncompressed
        ld iy,0
        ld c,0
        push af
        call SyFile_FILPOI
        pop bc
        jr c,sndlodg
        ld a,b

        push af
        ld de,(App_BnkNum)  ;File laden
        ld hl,sndmem
        ld bc,sndmax
        call SyFile_FILINP
        ld (sndlodlen),bc
        pop bc
sndlodi push af
        ld a,b              ;File schließen
        call SyFile_FILCLO
        pop af
        jr c,sndlod1
        ld a,3              ;Länge überprüfen
        scf
        jr z,sndlode        ;error -> song is too long (exactly = buffer size)
        ld a,(sndlodt)
        or a
        jr nz,sndlod3
        ld hl,sndmem        ;Format erkennen
        ld bc,128
        add hl,bc
        ld de,sndlodskm     ;skm?
        ld b,4
        call strcmp
        jr z,sndlod3
        ld hl,sndmem
        ld bc,#498+128
        add hl,bc
        ld de,sndlodst2     ;sound trakker 128?
        ld b,6
        call strcmp
        ld a,1
        jr z,sndlod3
        ld a,4              ;unbekanntes Format
        jr sndlode

sndlod3 call sndini         ;Sound initialisieren
        ld a,0
        ret nc
sndlod4 ld a,2              ;error -> unsupported format
        jr sndlode
sndlodg ld a,b
        call SyFile_FILCLO
sndlod1 ld c,a
        ld a,1              ;error -> disc error
sndlode dec a
        jr nz,sndlodf
        ld a,c
        call prgerr
        jp sndres
sndlodf add a
        ld l,a
        ld h,0
        ld de,daterrtab
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld b,1+64
        call prginf0
        jp sndres
sndlod0 ld hl,45
        ld (sndtxt_titl),hl
        ld (sndtxt_auth),hl
        ld (sndtxt_albm),hl
        ld (sndtxt_year),hl
        ld (sndtxt_comt),hl
        ret

;### SNDCHN -> get number of channels of loaded module
;### Input      A=Typ (0=SKM, 1=ST2, 2=MP3, 3=PT3), (sndmem)=loaded module
;### Output     A=channels (1,3,6)
sndchn  ld e,1
        cp 2
        jr z,sndchn0        ;mp3 always 1 channel (no psg)
        ld e,3
        cp 3
        jr nz,sndchn0       ;st2,skm always 3 channel (standard psg)
        ld hl,sndmem
        ld bc,(sndlodlen)
        call pt3tss
        ld e,3
        jr c,sndchn0        ;normal pt3  -> 3 channel pt3
        ld e,6              ;turbo sound -> 6 channel pt3
sndchn0 ld a,e
        ret

;### SNDINI -> Initialisiert geladenes Modul eines bestimmten Types
;### Eingabe    A=Typ (0=Starkos, 1=SoundTrakker, 2=MP3, 3=PT3, 4=MOD, 5=SA2)
;### Ausgabe    CF=1 nicht unterstütztes Format
sndjmp  dw sksini,sksply,sksstp,skspos,sksvol,sksval
        dw st2ini,st2ply,st2stp,st2pos,st2vol,st2val
        dw mp3ini,mp3ply,mp3stp,mp3pos,mp3vol,mp3val
        dw pt3ini,pt3ply,pt3stp,pt3pos,pt3vol,pt3val
        dw modini,modply,modstp,modpos,modvol,modval
        dw sa2ini,sa2ply,sa2stp,sa2pos,sa2vol,sa2val
sndinif dw 0        ;Flag, ob gültiger Sound geladen wurde

sndini  push af
        ;call dmnini
        pop bc
        ret c
        ld a,b
        ld (snddattyp),a
        cp 2
        jr z,sndini1        ;mp3 -> no hardware init necessary
        cp 5+1
        ccf
        ret c               ;unknown format -> exit
        push af
        call hrdini         ;init required sound hardware and patch drivers
        pop bc
        ret c
        ld a,b
sndini1 add a
        add a
        ld c,a
        add a
        add c
        ld c,a
        ld b,0
        ld ix,sndjmp
        add ix,bc
        ld l,(ix+0)
        ld h,(ix+1)
        ld (sndstp1+1),hl   ;Init Routine setzen
        ld l,(ix+2)
        ld h,(ix+3)
        ld (prgtim1+1),hl   ;Play Routine setzen
        ld l,(ix+4)
        ld h,(ix+5)
        ld (sndpau1+1),hl   ;Mute Routine setzen
        ld l,(ix+6)
        ld h,(ix+7)
        ld (sndpos1+1),hl   ;Pos  Routine setzen
        ld l,(ix+8)
        ld h,(ix+9)
        ld (sndvol +1),hl   ;Vol  Routine setzen
        ld l,(ix+10)
        ld h,(ix+11)
        ld (sndval +1),hl   ;Chn  Routine setzen
        ld hl,1
        ld (ctlskp),hl
        ld a,l
        ld (sndinif),a
        call sndstp0        ;Song initialisieren
        ld (snddatlen),hl   ;Länge merken
        jr c,sndres
        ld de,256*prgwin_bhz+256-2
        call winupd        ;KHz und Kbps updaten
        or a
        ret

;### SNDRES -> Deaktiviert die Soundausgabe
sndres  ld hl,sndres1       ;Routinen deaktivieren
        ld (sndstp1+1),hl
        ld hl,sndres0
        ld (prgtim1+1),hl
        ld (sndpau1+1),hl
        ld (sndpos1+1),hl
        ld (sndvol +1),hl
        ld hl,-1
        ld (prgtimp),hl
        inc hl
        ld (sndinif),hl
sndres1 scf
sndres0 ret

;### SNDPLY -> Startet Abspielen
sndply  call dmnply
        db #3e:scf
        ld (prgtim),a
        ret

;### SNDPAU -> Hält Abspielen an
sndpau  db #3e:or a
        ld (prgtim),a
        call dmnpau
sndpau1 jp sndres0          ;### MUTE

;### SNDSTP -> Stoppt Song und setzt Position an den Anfang
;### Ausgabe    CF=1 nicht unterstütztes Format
sndstp  ld hl,gfx_sta_stop
        call ctlsta
        call sndpau
sndstp0 ld a,50
        ld (prgtimc),a
        ld hl,-1
        ld (prgtimp),hl
        inc hl
        ld (ctltim),hl
        xor a
        ld (ctltim+2),a
        ex de,hl
        call ctlpup
        call ctltup
        call anaoff
sndstp2 ld a,(sndset_vol)
        call sndvol
        ld hl,sndmem
sndstp1 call sndres1        ;### INIT
        ret c
        ld (snddatchn),a
        ld a,(diawin)
        inc a
        ret z
        dec a
        push hl
        push af
        ld hl,sndfil
        ld de,propertxt10
        call prpinf3
        ld a,19
        call prpinf
        pop bc
        ld c,MSC_DSK_WINDIN
        ld e,-1
        call msgsnd
        pop hl
        or a
        ret

;### SNDPOS -> Setzt Song-Position
;### Eingabe    HL=neue Position
sndpos  push hl
        call sndpau
        call sndstp2
        pop hl
sndpos1 call sndres0        ;### POS
        jr sndply

;### SNDVOL -> Setzt Song-Volume
;### Eingabe    A=neue Volume (0-255)
sndvol  jp sndres0          ;### VOLUME

;### SNDVAL -> Holt Frequenz und Volume
;### Ausgabe    A=Volume (0-15), L=Frequenz (0-15)
sndval  jp sndres0          ;### DAT

;### PRGTIM -> Programm-Timer, spielt Sound ab
prgtima db 4
prgtimc db 50
prgtimp dw 0
prgtim  or a
        jr nc,prgtim0
        or a
        ld hl,0
prgtim1 call sndres0        ;### PLAY
        jr nc,prgtim4
        push hl
        ld c,-1             ;Sound hat Ende erreicht
        call prgtim2
        pop hl
prgtim4 ex de,hl
        ld hl,(prgtimp)
        ld (prgtimp),de
        or a
        sbc hl,de
        jr z,prgtim3
        ld c,0              ;Position geändert
        call prgtim2
prgtim3 ld a,(anaflg)
        or a
        jr z,prgtim5
        ld hl,prgtima
        dec (hl)
        jr nz,prgtim5
        ld (hl),4
        call anaplt         ;Analizer updaten
prgtim5 ld hl,prgtimc
        dec (hl)
        jr nz,prgtim0
        ld a,(snddattyp)
        cp 2
        ld a,-1
        jr z,prgtim6
        ld a,50
prgtim6 ld (hl),a
        ld c,1              ;eine Sekunde bzw. Verzögerung abgelaufen
        call prgtim2
prgtim0 rst #30
        jr prgtim
prgtim2 ld a,(prgtimn)      ;Message an Hauptprozess senden
        db #dd:ld l,a
        ld a,(App_PrcID)
        db #dd:ld h,a
        ld iy,timmsgb
        ld (iy+0),1
        ld (iy+1),c         ;0=Pos, 1=Sekunde, -1=Ende
        ld (iy+2),e         ;aktuelle Pos eintragen
        ld (iy+3),d
        rst #10
        ret


;==============================================================================
;### SOUND DAEMON COOPERATION #################################################
;==============================================================================

dmnuse  db 0    ;0=no daemon psg/wavetable usage, 1=psg usage, 2=wavetable usage

;### DMNPLY -> sound daemon usage begin with song-play
;### Input      (snddattyp)=Typ (0=Starkos, 1=SoundTrakker, 2=MP3, 3=PT3, 4=MOD, 5=SA2)
;### Destroyed  AF,BC,DE,HL,IX,IY
dmnply  ld a,(snddattyp)
        cp 2
        ret z
        cp 4
        jr c,dpsbeg
        ret

;### DMNPAU -> sound daemon usage stop with song-pause
;### Destroyed  AF,BC,DE,HL,IX,IY
dmnpau  ld a,(dmnuse)
        dec a
        ret nz
        ld a,16
        jr dpsbeg0

;### DMNFRE -> sound daemon usage stop with song-unload
;### Destroyed  AF,BC,DE,HL,IX,IY
dmnfre  ld a,(dmnuse)
        cp 2
        ret c
        ld a,17
        jr dpsbeg0

;### DPSBEG -> sound daemon psg usage begin
;### Output     CF=0
;### Destroyed  AF,BC,DE,HL,IX,IY
dpsbeg  ld a,(hrddem)
        or a
        bit 0,a
        ret z                   ;no psg-daemon -> ignore daemon
        ld a,1
        ld (dmnuse),a
        xor a
dpsbeg0 jp SySound_SNDCOO

;### DWTBEG -> sound daemon wavetable usage begin
;### Output     CF=0 -> ok (mt_load2/mt_load3 updated), CF=1 -> error, A=4, wavetable hardware in use
;### Destroyed  AF,BC,DE,HL,IX,IY
dwtbeg  ld a,(hrddem)
        or a
        bit 1,a
        ret z                   ;no wavetable-daemon -> ignore daemon
        ld a,1+00
        call SySound_SNDCOO     ;cf=0 -> de,hl=startofs
        ld a,4
        ret c
        ld a,#20
        add e
        ld (mt_load3+1),a
        ld a,l:ld l,h:ld h,a
        ld (mt_load2+1),hl
        ld a,2
        ld (dmnuse),a
        or a
        ret


;==============================================================================
;### EXTERNAL BITMAPS #########################################################
;==============================================================================

;### PRGFIL -> Generates datafile path
datnam  db "symamp.dat":datnam0

datpth  dw 0
datfil  dw 0
datend  dw 0

prgfil  ld hl,(App_BegCode)
        ld de,App_BegCode
        dec h
        add hl,de           ;HL = CodeEnd = path
        ld (datpth),hl
        ld e,l
        ld d,h              ;DE=HL
        ld b,255
prgfil1 ld a,(hl)           ;search end of path
        or a
        jr z,prgfil2
        cp " "
        jr z,prgfil2
        inc hl
        djnz prgfil1
        jr prgfil4
        ld a,255
        sub b
        jr z,prgfil4
        ld b,a
prgfil2 dec hl              ;search start of filename
        ld a,(hl)
        cp "/"
        jr z,prgfil3
        cp "\"
        jr z,prgfil3
        cp ":"
        jr z,prgfil3
        djnz prgfil2
        jr prgfil4
prgfil3 inc hl
        ex de,hl
prgfil4 ld (datfil),de
        ld hl,datnam        ;replace application filename with config filename
        ld bc,datnam0-datnam
        ldir
        ex de,hl
        ld a,(hl)
        ld (datend+2),a
        ld (hl),0
        ld (datend),hl
        ret


bmpdato ds 64   ;offset table (max 64bytes/32 entries)

bmpdatb equ 0*5+prgmemtab+0     ;bitmap ram bank
bmpdata equ 0*5+prgmemtab+1     ;bitmap start address
bmpdatl equ 0*5+prgmemtab+3     ;bitmap length



;### BMPSET -> links external bitmaps into control records
bmpsett
db xid_frame_l  :dw 3+gfx_frame_l
db xid_frame_r  :dw 3+gfx_frame_r
db xid_kbpskhz  :dw 3+gfx_kbpskhz
db xid_sldpos   :dw 3+gfx_sldpos
db xid_sldvol   :dw 3+gfx_sldvol
db xid_open     :dw 3+gfx_open
db xid_mixer    :dw 3+gfx_mixer
db xid_control  :dw 3+gfx_control
db xid_stereo   :dw 3+gfx_stereo
db xid_titmix   :dw 3+gfx_titmix
db xid_sldpan   :dw 3+gfx_sldpan
db xid_sldmix   :dw 3+gfx_sldmix1
db xid_sldmix   :dw 3+gfx_sldmix2
db xid_sldmix   :dw 3+gfx_sldmix3
db 0

bmpset  ld hl,bmpsett
bmpset1 ld a,(hl)
        or a
        ret z
        add a
        ld c,a
        ld b,0
        inc hl
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        push hl
        ld a,(bmpdatb)
        ld (de),a
        inc de
        ld hl,bmpdato-2
        add hl,bc
        ldi:ldi
        pop hl
        jr bmpset1


;### BMPLOD -> loads external bitmaps
bmplodl dw 0    ;entry table size (bit15=crunch flag)
        dw 0    ;bitmap size

bmplod  call prgfil             ;generate path
        ld hl,(datpth)
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN      ;open data file
        jp c,bmplod0
        ld hl,bmplodl
        ld de,(App_BnkNum)
        ld bc,4
        push af
        call SyFile_FILINP      ;load length information
        jp c,bmplode
        pop af
        ld hl,bmpdato
        ld de,(App_BnkNum)
        ld bc,(bmplodl+0)
        res 7,b
        push af
        call SyFile_FILINP      ;load bitmap table
        jr c,bmplode
        xor a
        ld e,1
        ld bc,(bmplodl+2)
        ld (bmpdatl),bc
        push bc
        rst #20:dw jmp_memget   ;reserve bitmap memory
        pop bc
        jr c,bmplode
        ld (bmpdatb),a          ;register additional memory
        ld (bmpdata),hl
        ld de,(bmplodl+0)       ;d7=crunch flag
        ld e,a
        pop af
        push af
        rl d
        call SyFile_FILCPR      ;load bitmap data
        jr c,bmplode
        jr nz,bmplode
        pop af
        call SyFile_FILCLO      ;close data file
        ld a,(datend+2)
        ld hl,(datend)
        ld (hl),a

        ld a,(bmplodl+0)        ;relocate bitmap headers
        srl a
        ld b,a
        ld a,(bmpdatb)
        ld hl,bmpdato
bmplod1 push bc
        ld e,(hl):inc hl
        ld d,(hl):dec hl
        ex de,hl
        ld bc,(bmpdata)
        add hl,bc
        ex de,hl
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        push hl
        ex de,hl
        inc hl:inc hl:inc hl
        call bmplod2
        call bmplod2
        pop hl
        pop bc
        djnz bmplod1
        ret

bmplod2 rst #20:dw jmp_bnkrwd
        ex de,hl
        ld hl,(bmpdata)
        add hl,bc
        ld c,l
        ld b,h
        ex de,hl
        dec hl:dec hl
        rst #20:dw jmp_bnkwwd
        ret

bmplode pop af
        call SyFile_FILCLO
bmplod0 ;...show error
        jp prgend


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

hrdbas  db 0    ;0=N/A, 1=CPC, 2=MSX, 3=PCW, 4=EP, 5=NXT
hrdext  dw 0    ;b0=PSG, b1=MP3, b2=Playcity, b3=Darky, b4=OPL4
hrddem  db 0    ;+1/+2=psg/opl4 daemon existing

;### SYSINI -> Computer-Typ abhängige Initialisierung
sysini  ld hl,jmp_sysinf        ;*** Computer-Typ holen
        ld de,256*1+5
        ld ix,cfgcpctyp
        ld iy,66+2+6+8
        rst #28                 ;cfgcpctyp=computer typ
        ld a,(cfgcpctyp)
        and 31
        cp 6
        ld e,1
        jr c,sysini1    ; 0- 5 -> 1=cpc psg
        ld ix,prgdatprf2
        ld (ix+2+00),64     ;no cpc -> hide willy settings
        ld (ix+2+16),64
        ld (ix+2+32),64
        ld e,4
        jr z,sysini1    ; 6    -> 4=ep dave
        cp 10+1
        ld e,2
        jr c,sysini1    ; 7-10 -> 2=msx psg
        cp 13+1
        ld e,3
        jr c,sysini1    ;12-13 -> 3=pcw psg
        cp 20
        ld e,5
        jr z,sysini1    ;20    -> 5=nxt psg
        ;...
        ld e,0          ;?     -> 0=unsupported base hardware
sysini1 ld a,e
        ld (hrdbas),a   ;store base hardware

        call hrddet
        ld (hrdext),hl  ;store extended sound hardware
        ld ix,prfobjdat1a
        bit 0,l
        jr z,sysini3
        ld de,prfobjtxt1a
        ld a,(hrdbas)
        cp 4
        jr nz,sysini2
        ld de,prfobjtxt1b
sysini2 call sysini0
sysini3 bit 1,l
        ld de,prfobjtxt1c
        call nz,sysini0
        bit 2,l
        ld de,prfobjtxt1d
        call nz,sysini0
        bit 3,l
        ld de,prfobjtxt1e
        call nz,sysini0
        bit 4,l
        ld de,prfobjtxt1f
        jr nz,sysini4
        ld a,(hrdbas)   ;no opl4 -> maybe opl3lpt/willy manually?
        cp 1
        jr nz,sysini5   ;but only on cpc
        ld (prfobjdatw),a
sysini4 call nz,sysini0

sysini5 ld hl,jmp_scrget
        rst #28
        ld bc,-149
        add ix,bc
        ld (prgwindat+4),ix
        ld (prglstdat+4),ix
        ld bc,-74
        add ix,bc
        ld (prgwinmix+4),ix
        ld bc,-96
        add iy,bc
        ld (prgwindat+6),iy
        ld (prgwinmix+6),iy
        ld bc,96-124
        add iy,bc
        ld (prglstdat+10),iy
        ld a,(hrdbas)
        call mp3drv
        ret nc
        xor a
        jp mp3drv

sysini0 ld (ix+0),e
        ld (ix+1),d
        ld de,4
        add ix,de
        ret

;### CFGINI -> Config übernehmen
cfgini  ld hl,ctlrepf
        ld bc,prgwin_rep*16+4+prgwinobj
        ld ix,prgwinmen2a
        call ctlrep0
        ld hl,ctlshuf
        ld bc,prgwin_shu*16+4+prgwinobj
        ld ix,prgwinmen2b
        jp ctlrep0

;### SPRCNV -> Konvertiert Sprite vom CPC ins MSX Format
;### Eingabe    HL=Sprite (inklusive Header)
;### Veraendert BC
sprcnv  set 7,(hl)
        ld de,10*12
        inc hl:inc hl
sprcnv1 inc hl
        ld c,(hl)
        xor a:rl c:rla
        add a:rl c:rla
        add a:rl c:rla
        add a:rl c:rla
        ld b,a
        rl c:rla:add a
        rl c:rla:add a
        rl c:rla:add a
        rl c:rla:add a
        or b
        ld (hl),a
        dec de
        ld a,e
        or d
        jr nz,sprcnv1
        ret

;### GFXINI -> Bitmap init
gfxini  ld a,(cfgcpctyp)
        rla
        jr nc,gfxini2
        ld a,16*8+5
;        ld (prgwindsc2+2),a
        ld hl,gfx_analyz0
        call sprcnv
        ld hl,gfx_analyz2
        call sprcnv
gfxini2 ld hl,gfx_analyz0
        ld de,gfx_analyz1
        ld bc,10*12+3
        ldir
        ret

;### MSGGET -> Message für Programm abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(App_MsgBuf+0), IY=App_MsgBuf
;### Veraendert 
msgget  db #dd:ld h,-1
msgget1 ld a,(App_PrcID)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        ld iy,App_MsgBuf           ;IY=Messagebuffer
        rst #08                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        or a
        db #dd:dec l
        ret nz
        ld iy,App_MsgBuf
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGSND -> Message an Desktop-Prozess senden
;### Eingabe    C=Kommando, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(App_PrcID)
        db #dd:ld l,a
        ld iy,App_MsgBuf
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    A,HL=Wert1*Wert2 (24bit)
;### Veraendert F,BC,DE,IX
clcmul  ld ix,0
        ld hl,0
clcmul1 ld a,c
        or b
        jr z,clcmul3
        srl b
        rr c
        jr nc,clcmul2
        add ix,de
        ld a,h
        adc l
        ld h,a
clcmul2 sla e
        rl d
        rl l
        jr clcmul1
clcmul3 ld a,h
        db #dd:ld e,l
        db #dd:ld d,h
        ex de,hl
        ret

;### CLCDIV -> Dividiert zwei Werte (24bit)
;### Eingabe    A,BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE,IX,IYL
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        or d
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        adc hl,hl
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,d
        sbc 0
        ld d,a          ;D,HL=D,HL-IX
        jr nc,clcdiv2
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        ld a,d
        adc 0
        ld d,a
        scf
clcdiv2 db #fd:dec l
        jr nz,clcdiv1
        ex de,hl        ;DE=Wert1 MOD Wert2
        ld a,c
        rla
        cpl
        ld l,a
        ld a,b
        rla
        cpl
        ld h,a          ;HL=Wert1 DIV Wert2
        ret

;### CLCD32 -> Dividiert zwei Werte (32bit)
;### Eingabe    IY,BC=Wert1, IX=Wert2
;### Ausgabe    IY,BC=Wert1/Wert2, HL=Wert1 MOD Wert2
;### Veraendert AF,BC,DE,IY
clcd32c db 0
clcd32  ld hl,0
        db #dd:ld a,l
        db #dd:or h
        ret z           ;IY,BC=Wert1(Zaehler)
        ld de,0         ;DE,HL=RechenVar
        ld a,32         ;Counter auf 32 setzen
clcd321 ld (clcd32c),a
        rl c
        rl b
        db #fd:ld a,l:rla:db #fd:ld l,a
        db #fd:ld a,h:rla:db #fd:ld h,a
        adc hl,hl
        rl e
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,e
        sbc 0
        ld e,a
        ld a,d
        sbc 0
        ld d,a
        jr nc,clcd322
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        ld a,e
        adc 0
        ld e,a
        ld a,d
        adc 0
        ld d,a
        scf
clcd322 ccf
        ld a,(clcd32c)
        dec a
        jr nz,clcd321   ;HL=Wert1 MOD Wert2
        rl c
        rl b
        db #fd:ld a,l:rla:db #fd:ld l,a
        db #fd:ld a,h:rla:db #fd:ld h,a
        ret             ;IY,BC=Wert1 DIV Wert2

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
        or a
clcm161 rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        or a
        jr nz,clcm161
        ret

;### CLCD16 -> Dividiert zwei Werte (16bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE
clcd16  ld a,e
        or d
        ld hl,0
        ret z
        ld a,b
        ld b,16
clcd161 rl c
        rla
        rl l
        rl h
        sbc hl,de
        jr nc,clcd162
        add hl,de
clcd162 ccf
        djnz clcd161
        ex de,hl
        rl c
        rla
        ld h,a
        ld l,c
        ret

;### STRLEN -> Ermittelt Länge eines Strings
;### Eingabe    HL=String
;### Ausgabe    HL=Stringende (0), BC=Länge (maximal 255)
;### Verändert  -
strlen  push af
        xor a
        ld bc,255
        cpir
        ld a,254
        sub c
        ld c,a
        dec hl
        pop af
        ret

;### STRCMP -> Vergleicht zwei Strings
;### Eingabe    HL=Quell-String, DE=Vergleichstring, B=Länge
;### Ausgabe    UCASE(HL)=(DE) -> ZF=1, A=0
;### Verändert  B,DE,HL
strcmp  ex de,hl
strcmp1 ld a,(de)
        call clcucs
        cp (hl)
        ret nz
        inc hl
        inc de
        djnz strcmp1
        xor a
        ret

;### DIV168 -> A=HL/E, H=HL mod E
div168  ld b,8
div1681 adc hl,hl
        ld a,h
        jr c,div1682
        cp e
        jr c,div1683
div1682 sub e
        ld h,a
        or a
div1683 djnz div1681
        ld a,l
        rla
        cpl
        ret

;### CLCNUM -> Wandelt 16Bit-Zahl in ASCII-String um (mit 0 abgeschlossen)
;### Eingabe    IX=Wert, IY=Adresse, E=MaximalStellen
;### Ausgabe    (IY)=letzte Ziffer
;### Veraendert AF,BC,DE,HL,IX,IY
clcnumt dw -1,-10,-100,-1000,-10000
clcnum  ld d,0          ;2
        ld b,e          ;1
        push ix         ;5
        pop hl          ;3
        sla e           ;2
        ld ix,clcnumt-2 ;4
        add ix,de       ;4
        dec b           ;1
        jr z,clcnum4    ;3/2
        xor a           ;1
clcnum1 ld e,(ix+0)     ;5
        ld d,(ix+1)     ;5
        dec ix          ;3
        dec ix          ;3
        ld c,"0"-1      ;2
clcnum2 add hl,de       ;3
        inc c           ;1
        inc a           ;1
        jr c,clcnum2    ;3
        sbc hl,de       ;4
        dec a           ;1
        jr z,clcnum3    ;3/2
        ld (iy+0),c     ;5
        inc iy          ;3
clcnum3 djnz clcnum1    ;4/3
clcnum4 ld a,"0"        ;2
        add l           ;1
        ld (iy+0),a     ;5
        ld (iy+1),0     ;5
        ret

;### CLCDZ3 -> Rechnet Word in drei Dezimalziffern um
;### Eingabe    HL=Wert
;### Ausgabe    A=100er-Ascii-Ziffer, L=10er-Ascii-Ziffer, H=1er-Ascii-Ziffer
;### Veraendert F,C,DE
clcdz3  ld de,100
        ld c,"0"-1
        or a
clcdz31 inc c
        sbc hl,de
        jr nc,clcdz31
        ld a,l
        add e
        call clcdez
        ld a,c
        ret

;### CLCDEZ -> Rechnet Byte in zwei Dezimalziffern um
;### Eingabe    A=Wert
;### Ausgabe    L=10er-Ascii-Ziffer, H=1er-Ascii-Ziffer
;### Veraendert AF
clcdez  ld l,0
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ld a,"0"
        add l
        ld l,a
        ret

;### CLCUCS -> Wandelt Klein- in Großbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=ucase(Zeichen)
;### Verändert  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret

;### CLCLCS -> Wandelt Groß- in Kleinbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=lcase(Zeichen)
;### Verändert  F
clclcs  cp "A"
        ret c
        cp "Z"+1
        ret nc
        add "a"-"A"
        ret

;a=number -> (DE)=hexdigits, DE=DE+2
;c destroyed
hexplt6 ld c,a
        rlca:rlca:rlca:rlca
        call hexplt7
        ld a,c
hexplt7 and 15
        add "0"
        cp "9"+1
        jr c,hexplt8
        add "A"-"9"-1
hexplt8 ld (de),a
        inc de
        ret


;==============================================================================
;### LIBRARY-ROUTINEN #########################################################
;==============================================================================

SyKernel_MTADDT
        ld c,MSC_KRL_MTADDT
        call SyKernel_Message
        xor a
        cp l
        ld a,h
        ret

SyKernel_Message
        ld iy,App_MsgBuf
        ld (iy+0),c
        ld (iy+1),l
        ld (iy+2),h
        ld (iy+3),e
        ld (iy+4),a
        ld (iy+5),b
        ld a,c
        add 128
        ld (SyKMsgN),a
        db #dd:ld h,1       ;1 is the number of the kernel process
        ld a,(App_PrcID)
        db #dd:ld l,a
        rst #10
SyKMsg1 db #dd:ld h,1       ;1 is the number of the kernel process
        ld a,(App_PrcID)
        db #dd:ld l,a
        rst #08             ;wait for a kernel message
        db #dd:dec l
        jr nz,SyKMsg1
        ld a,(SyKMsgN)
        cp (iy+0)
        jr nz,SyKMsg1
        ld l,(iy+1)
        ld h,(iy+2)
        ret
SyKMsgN db 0

READ "App-SymAmp-EP.asm"

;### Song-Speicher
sndmxb  equ 18
sndmax  equ sndmxb*1024
sndmem  db 0                ;##!! LETZTER LABEL !!##

;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #88,#88,#88,#88,#88,#81,#18,#88,#88,#88,#81,#18,#88,#88,#88,#88,#88,#15,#51,#88,#88,#88,#12,#18,#88,#88,#88,#88,#81,#56,#65,#18,#88,#11,#2F,#18,#88,#88,#88,#88,#15,#66,#66,#51,#81,#22,#FF,#18
db #88,#88,#88,#81,#56,#66,#66,#61,#12,#0F,#21,#88,#88,#88,#88,#15,#66,#66,#66,#1F,#20,#FF,#18,#88,#88,#88,#81,#56,#66,#66,#11,#F2,#0F,#21,#88,#88,#88,#88,#15,#66,#66,#61,#FF,#20,#FF,#11,#88,#88
db #88,#81,#56,#66,#61,#1F,#F2,#2F,#21,#65,#18,#88,#88,#15,#66,#66,#1F,#FF,#22,#FF,#11,#66,#51,#88,#81,#56,#66,#11,#F2,#22,#22,#FF,#11,#16,#65,#18,#15,#66,#61,#22,#00,#00,#2F,#F1,#FF,#16,#66,#51
db #15,#66,#61,#F2,#20,#02,#F1,#1F,#2F,#16,#66,#51,#81,#56,#61,#FF,#20,#2F,#1F,#FF,#11,#66,#65,#18,#88,#15,#66,#11,#02,#11,#FF,#21,#66,#66,#51,#88,#88,#81,#56,#10,#21,#FF,#21,#16,#66,#65,#18,#88
db #88,#88,#15,#01,#1F,#2F,#16,#66,#66,#51,#88,#88,#88,#88,#F2,#1F,#FF,#11,#66,#66,#65,#18,#88,#88,#88,#81,#FF,#FF,#21,#66,#66,#66,#51,#88,#88,#88,#88,#1F,#FF,#21,#16,#66,#66,#65,#18,#88,#88,#88
db #81,#FF,#2F,#18,#15,#66,#66,#51,#88,#88,#88,#88,#81,#FF,#11,#88,#81,#56,#65,#18,#88,#88,#88,#88,#81,#21,#88,#88,#88,#15,#51,#88,#88,#88,#88,#88,#81,#18,#88,#88,#88,#81,#18,#88,#88,#88,#88,#88

;### SOUND PFAD UND NAME ######################################################

sndmsk  db "*  ",0
sndfil  ds 256+1

lstmsk  db "*  ",0
lstfil  ds 256+1

splmsk  db "spl",0
splfil  ds 256+1


;### Verschiedenes
prgmsginf1  db "SymAmp 4.1 (build "
read "..\..\..\SRC-Main\build.asm"
            db ")",0

prgmsginf2  db " by Prodatron/SymbiosiS et al",0
prgmsginf3  db " -see help for full credits-",0

prgwintit   db "SymAmp",0
prglsttit   db "Playlist Editor",0
propertit   db "File info box",0
prgtitmix   db "Equalizer",0
prgtitprf   db "Hardware settings",0

prgtxtok    db "Ok",0
prgtxtcnc   db "Cancel",0
prgtxtyes   db "Yes",0
prgtxtno    db "No",0

;### Menues
prgwinmentx1 db "File",0
prgwinmen1tx1 db 6,128,-1:dw menicn_fileplay    +1:db " Play file...",0
prgwinmen1tx2 db 6,128,-1:dw menicn_fileinfo    +1:db " View file info",0
prgwinmen1tx3 db 6,128,-1:dw menicn_fileopen    +1:db " Open playlist",0
prgwinmen1tx4 db 6,128,-1:dw menicn_filesave    +1:db " Save playlist",0
prgwinmen1tx5 db 6,128,-1:dw menicn_quit        +1:db " Exit",0
menicn_fileplay     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#61,#16,#66, #66,#61,#11,#66, #66,#61,#61,#66, #61,#11,#66,#66, #18,#81,#69,#96, #1e,#e1,#69,#99, #61,#16,#69,#96
menicn_fileinfo     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #66,#66,#66,#66, #16,#ff,#1f,#ff, #66,#66,#66,#66, #16,#ff,#ff,#1f, #66,#66,#66,#66, #16,#ff,#f1,#ff
menicn_fileopen     db 4,8,7:dw $+7,$+4,28:db 5: db #61,#16,#66,#66, #18,#81,#16,#66, #18,#88,#77,#77, #18,#87,#22,#27, #18,#72,#22,#76, #17,#22,#27,#66, #77,#77,#76,#66
menicn_filesave     db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#11, #1f,#ee,#ee,#f1, #1f,#ee,#ee,#f1, #1f,#ff,#ff,#f1, #1f,#11,#c1,#f1, #1f,#11,#c1,#f1, #61,#11,#11,#11
menicn_quit         db 4,8,7:dw $+7,$+4,28:db 5: db #11,#16,#16,#66, #14,#46,#11,#66, #14,#11,#1e,#16, #14,#1e,#ee,#e1, #14,#11,#1e,#16, #14,#46,#11,#66, #11,#16,#16,#66

prgwinmentx2 db "Play",0
prgwinmen2tx1 db 6,128,-1:dw menicn_plyprev     +1:db " Previous",0
prgwinmen2tx2 db 6,128,-1:dw menicn_plyplay     +1:db " Play",0
prgwinmen2tx3 db 6,128,-1:dw menicn_plypaus     +1:db " Pause",0
prgwinmen2tx4 db 6,128,-1:dw menicn_plystop     +1:db " Stop",0
prgwinmen2tx5 db 6,128,-1:dw menicn_plynext     +1:db " Next",0
prgwinmen2tx6 db 6,128,-1:dw menicn_plyrept     +1:db " Repeat",0
prgwinmen2tx7 db 6,128,-1:dw menicn_plyshuf     +1:db " Shuffle",0
menicn_plyprev      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #77,#66,#67,#76, #77,#66,#77,#76, #77,#67,#77,#76, #77,#66,#77,#76, #77,#66,#67,#76, #66,#66,#66,#66
menicn_plyplay      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #69,#99,#66,#66, #69,#99,#96,#66, #69,#99,#99,#66, #69,#99,#96,#66, #69,#99,#66,#66, #66,#66,#66,#66
menicn_plypaus      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #55,#56,#55,#56, #55,#56,#55,#56, #55,#56,#55,#56, #55,#56,#55,#56, #55,#56,#55,#56, #66,#66,#66,#66
menicn_plystop      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #6f,#ff,#ff,#66, #6f,#ff,#ff,#66, #6f,#ff,#ff,#66, #6f,#ff,#ff,#66, #6f,#ff,#ff,#66, #66,#66,#66,#66
menicn_plynext      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #77,#66,#67,#76, #77,#76,#67,#76, #77,#77,#67,#76, #77,#76,#67,#76, #77,#66,#67,#76, #66,#66,#66,#66
menicn_plyrept      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#16,#66,#66, #66,#11,#66,#66, #11,#11,#16,#11, #16,#11,#66,#61, #16,#16,#66,#61, #16,#66,#66,#61, #11,#11,#11,#11
menicn_plyshuf      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#16, #77,#66,#61,#11, #66,#76,#16,#16, #66,#61,#66,#66, #66,#16,#76,#76, #11,#66,#67,#77, #66,#66,#66,#76

prgwinmentx3 db "Options",0
prgwinmen3tx1 db 6,128,-1:dw menicn_settings    +1:db " Hardware settings",0
menicn_settings     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#6c,#66,#66, #6c,#6c,#6c,#66, #6f,#cd,#cf,#66, #cc,#c1,#cc,#c6, #ff,#cc,#cf,#f6, #6c,#fc,#fc,#66, #6f,#6c,#6f,#66

prgwinmentx4 db "View",0
prgwinmen4tx1 db 6,128,-1:dw menicn_vwplaylist  +1:db " Playlist",0
prgwinmen4tx2 db 6,128,-1:dw menicn_vwequaliz   +1:db " Equalizer",0
menicn_vwplaylist   db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#ff,#ff,#ff, #88,#88,#88,#86, #ff,#ff,#ff,#f6, #88,#88,#86,#66, #ff,#ff,#f6,#66, #88,#88,#88,#66, #ff,#ff,#ff,#66
menicn_vwequaliz    db 4,8,7:dw $+7,$+4,28:db 5: db #00,#10,#01,#00, #00,#10,#01,#00, #07,#77,#01,#00, #00,#10,#01,#00, #00,#10,#77,#70, #00,#10,#01,#00, #00,#10,#01,#00

prgwinmentx5 db "?",0
prgwinmen5tx1 db 6,128,-1:dw menicn_help        +1:db " Help topics",0
prgwinmen5tx2 db 6,128,-1:dw menicn_about       +1:db " About SymAmp...",0
menicn_help         db 4,8,7:dw $+7,$+4,28:db 5: db #66,#1f,#f1,#66, #61,#fc,#cf,#16, #1f,#ff,#fc,#f1, #ff,#fc,#cc,#f1, #ff,#ff,#ff,#18, #1f,#cf,#f1,#81, #61,#ff,#18,#16
menicn_about        db 4,8,7:dw $+7,$+4,28:db 5: db #66,#10,#07,#66, #66,#10,#07,#66, #66,#66,#66,#66, #61,#00,#07,#66, #66,#10,#07,#66, #66,#10,#07,#66, #61,#00,#00,#76

prglstmentx1    db "+Add",0
prglstmentx2    db "-Rem",0
prglstmentx3    db "=Sel",0
prglstmentx4    db "*Misc",0
prglstmentx5    db "Up",0
prglstmentx6    db "Down",0

prglstmen1tx1   db 6,128,-1:dw menicn_lstaddfile  +1:db " Add file...",0
prglstmen1tx2   db 6,128,-1:dw menicn_lstaddfold  +1:db " Add folder...",0
menicn_lstaddfile   db 4,8,7:dw $+7,$+4,28:db 5: db #66,#61,#16,#66, #66,#61,#11,#66, #66,#61,#61,#66, #61,#11,#66,#66, #18,#81,#66,#76, #1e,#e1,#67,#77, #61,#16,#66,#76
menicn_lstaddfold   db 4,8,7:dw $+7,$+4,28:db 5: db #6d,#dd,#66,#66, #d0,#00,#dd,#d6, #d0,#07,#00,#01, #d0,#77,#70,#01, #d0,#07,#00,#01, #d0,#00,#00,#01, #61,#11,#11,#16

prglstmen2tx1   db 6,128,-1:dw menicn_lstremsel   +1:db " Remove selected",0
prglstmen2tx2   db 6,128,-1:dw menicn_delete      +1:db " Remove all",0
menicn_lstremsel    db 4,8,7:dw $+7,$+4,28:db 5: db #66,#61,#16,#66, #66,#61,#11,#66, #66,#61,#61,#66, #61,#11,#66,#66, #18,#81,#6f,#6f, #1e,#e1,#66,#f6, #61,#16,#6f,#6f
menicn_delete       db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#66,#66,#ff, #6f,#f6,#6f,#f6, #66,#ff,#ff,#66, #66,#6f,#f6,#66, #66,#ff,#ff,#66, #6f,#f6,#6f,#f6, #ff,#66,#66,#ff

prglstmen3tx1   db 6,128,-1:dw menicn_lstselall   +1:db " Select all",0
prglstmen3tx2   db 6,128,-1:dw menicn_lstselnon   +1:db " Select none",0
prglstmen3tx3   db 6,128,-1:dw menicn_lstselinv   +1:db " Invert selection",0
menicn_lstselall    db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#11, #77,#77,#77,#76, #11,#11,#11,#16, #77,#77,#76,#66, #11,#11,#16,#66, #77,#77,#77,#66, #11,#11,#11,#66
menicn_lstselnon    db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#ff,#ff,#ff, #88,#88,#88,#86, #ff,#ff,#ff,#f6, #88,#88,#86,#66, #ff,#ff,#f6,#66, #88,#88,#88,#66, #ff,#ff,#ff,#66
menicn_lstselinv    db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#11, #77,#77,#77,786, #ff,#ff,#ff,#f6, #88,#88,#86,#66, #11,#11,#16,#66, #77,#77,#77,#66, #ff,#ff,#ff,#66

prglstmen4tx1   db 6,128,-1:dw menicn_fileinfo    +1:db " File info",0
prglstmen4tx2   db 6,128,-1:dw menicn_lstsort     +1:db " Sort list",0
menicn_lstsort      db 4,8,7:dw $+7,$+4,28:db 5: db #67,#76,#66,#66, #76,#67,#66,#66, #77,#77,#61,#11, #76,#67,#66,#61, #76,#67,#66,#16, #66,#66,#61,#66, #66,#66,#61,#11

prgwintxt1  db "/00:00/"
prgwintxt0  db 0
prgwintxt3  db "/:/",0    ;kbps
prgwintxt4  db "/:/",0    ;khz

propertxt1a db "File",0
propertxt2a db "Type",0
propertxt3a db "Title",0
propertxt4a db "Artist",0
propertxt5a db "Album",0
propertxt6a db "Year",0
propertxt7a db "Comment",0

mp3id3v1    ds 3        ;               \
sndtxt_titl ds 31       ;Title
sndtxt_auth ds 31       ;Artist
sndtxt_albm ds 31       ;Album
sndtxt_year ds 5        ;Year
sndtxt_comt ds 29       ;Comment
mp3id3v1dat db 0,0,0    ;?,track,genre  /

propertxt10 ds 256+1
propertxt2x db "Unknown"
propertxt2y db 0
propertxt20 db "Starkos 1.0/1.1 Song",0
propertxt21 db "SoundTrakker 128 Song",0
propertxt22 db "MPEG-#.# layer #, ###kbps",0
propertxt23 db "###:##, ##khz, ############",0
propertxt24 db "Pro/VortexTracker Song",0
propertxt27 db "(x channels)",0
propertxt25 db "Amiga MOD Song",0
propertxt26 db "(xx instruments, 04 channels)",0
propertxt28 db "Adlib Tracker 2 Song",0

prfobjtxt1  db "Autodetected sound devices",0
prfobjtxt1a db "AY-3-8910 compatible PSG",0
prfobjtxt1b db "Dave chip with AY emulation",0
prfobjtxt1c db "MP3MSX compatible",0
prfobjtxt1d db "PlayCity 6chn PSG",0
prfobjtxt1e db "Darky 6chn PSG",0
prfobjtxt1f db "Moonsound OPL4 ("
prfobjtxt1f1 db "xxxx KB)",0
prfobjtxt1z db "no sound hardware detected",0
prfobjtxt10 db "-",0
prfobjtxt2  db "6chn PSG device usage",0
prfobjtxt2a db "for 6chn modules only",0
prfobjtxt2b db "for all PSG-based modules",0

prfobjtxt12 db "Manually configured",0
prftxtdat11 db "OPL3LPT/Willy at port",0
prfwprtbc   db "#FEBC",0
prfwprtac   db "#FEAC",0
prfwprta4   db "#FEA4",0
prfwprt9c   db "#FE9C",0
prfwprt94   db "#FE94",0
prfwprt8c   db "#FE8C",0
prfwprt84   db "#FE84",0


;### Error messages
txterrlod1  db "A disc error occured.",0
txterrlod2  db "(Error code ":txterrlod2a db "##)",0
txterrlod3  db "",0

txterrfrm1  db "A required sound device for",0
txterrfrm2  db "playing this module is not",0
txterrfrm3  db "available in your system.",0

txterrmem1  db "Not enough memory.",0
txterrmem2  db "This module requires too much",0
txterrmem3  db "RAM or wavetable memory.",0

txterrocc1  db "Device is occupied.",0
txterrocc2  db "The required sound device",0
txterrocc3  db "is currently in use.",0

txterrunk1  db "Unknown or unsupported file.",0
txterrunk2  db "SymAmp has no idea what to do",0
txterrunk3  db "with this bunch of bytes.",0

txterrlfp1  db "Too many directory pathes.",0
txterrlfp2  db "Please clear the songlist to",0
txterrlfp3  db "add songs from a new folder.",0

txterrlfe1  db "Too many entries in list.",0
txterrlfe2  db "The maximum of 250 songs has",0
txterrlfe3  db "been reached.",0


;### Grafiken
gfx_analyz1 db 10,40,12:ds 10*12    ;analyzer complete  (current vu meters)
gfx_analyz2 db 10,40,12             ;analyzer full      (all meters to the max)
        ds 10,#00:ds 10,#00:ds 10,#00:ds 10,#00:ds 10,#00:ds 10,#00:ds 10,#0F:ds 10,#00:ds 10,#0F:ds 10,#FF:ds 10,#0F:ds 10,#FF

read "App-SymAmp-Bitmaps.asm"


;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

App_BegTrns

;### PRGPRZS -> Stack für Programm-Prozess
        ds 128
prgstk  ds 6*2
        dw prgprz

App_PrcID   db 0
App_MsgBuf  ds 14

;### PRGTIMS -> Stack für Programm-Timer
        ds 128
prgtims ds 6*2
        dw prgtim
prgtimn db 0
timmsgb ds 14

cfgcpctyp   db 0

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,0,prgicnbig,prgicn16c

;### HAUPT-FENSTER ############################################################

prgwindat dw #3501,0,171,104,148,62,0,0,148,62,148,62,148,62,prgicnsml,prgwintit,0,prgwinmen,prgwingrp,0,0:ds 136+14

prgwinmen  dw 5, 1+4,prgwinmentx1,prgwinmen1,0, 1+4,prgwinmentx2,prgwinmen2,0, 1+4,prgwinmentx3,prgwinmen3,0, 1+4,prgwinmentx4,prgwinmen4,0, 1+4,prgwinmentx5,prgwinmen5,0
prgwinmen1 dw 7, 17,prgwinmen1tx1,ctlopn,0, 17,prgwinmen1tx2,prpsng,0, 1+8,0,0,0, 17,prgwinmen1tx3,lstlod,0, 17,prgwinmen1tx4,lstsav,0, 1+8,0,0,0, 17,prgwinmen1tx5,prgend,0
prgwinmen2 dw 8, 17,prgwinmen2tx1,ctlrew,0, 17,prgwinmen2tx2,ctlply,0, 17,prgwinmen2tx3,ctlpau,0, 17,prgwinmen2tx4,ctlstp,0, 17,prgwinmen2tx5,ctlffw,0, 1+8,0,0,0
prgwinmen2a   dw 17,prgwinmen2tx6,ctlrep,0
prgwinmen2b   dw 17,prgwinmen2tx7,ctlshu,0
prgwinmen3 dw 1, 17,prgwinmen3tx1,prfopn,0
prgwinmen4 dw 2, 17,prgwinmen4tx1,lstswt,0, 17,prgwinmen4tx2,mixswt,0
prgwinmen5 dw 3, 17,prgwinmen5tx1,prghlp,0, 1+8,0,0,0, 17,prgwinmen5tx2,prginf,0

prgwingrp db 41,0:dw prgwinobj,0,0,0,0,0,0
prgwinobj
dw     00,255*256+00,128+6    ,0,0,10000,10000,0    ;00=background
dw     00,255*256+10,gfx_edge_ul,  0, 0,  4, 3,0    ;01=edge ul
dw     00,255*256+10,gfx_edge_ur,144, 0,  4, 3,0    ;02=edge ur
dw     00,255*256+10,gfx_edge_dl,  0,59,  4, 3,0    ;03=edge dl
gfx_frame_l
dw     00,255*256+10,gfx_frame_l,  4, 4,  4,31,0    ;04=frame left
dw     00,255*256+00,128+7      ,  8, 4,108, 1,0    ;05=frame up
dw     00,255*256+00,128+1      ,  8, 5,108,29,0    ;06=frame middle
dw     00,255*256+00,128+8      ,  8,34,108, 1,0    ;07=frame down
gfx_frame_r
dw     00,255*256+10,gfx_frame_r,116, 4, 28,31,0    ;08=frame right
gfx_kbpskhz
dw     00,255*256+10,gfx_kbpskhz, 44, 8, 36,14,0    ;09=kbps/khz
dw     00,255*256+00,128+3      ,  7,25,112, 8,0    ;10=title background
prgwin_barp equ 11
gfx_sldpos
dw     00,255*256+10,gfx_sldpos ,  4,38,124, 6,0    ;11=bar pos
gfx_open
dw     00,255*256+10,gfx_open   ,128,36, 16,10,0    ;12=open
gfx_mixer
dw     00,255*256+10,gfx_mixer  , 68,47, 28,15,0    ;13=mix left
dw     00,255*256+00,128+7      , 96,47, 52, 1,0    ;14=mix up
dw     00,255*256+00,128+8      , 96,48, 52,14,0    ;15=mix middle
dw     00,255*256+10,gfx_edge_drw,144,59, 4, 3,0    ;16=edge dr
prgwin_barv equ 17
gfx_sldvol
dw     00,255*256+10,gfx_sldvol , 96,52, 48, 6,0    ;17=bar volume
gfx_control
dw     00,255*256+10,gfx_control,  4,47, 72,12,0    ;18=controls
prgwin_basofs equ 19

prgwin_tim equ prgwin_basofs+0
dw     00,255*256+05,prgwindsc1, 15,11, 22, 08,0    ;xx=PlayZeit Display
prgwin_sta equ prgwin_basofs+1
dw     00,255*256+10,gfx_sta_stop,7,12, 08, 08,0    ;xx=PlayModus
prgwin_tit equ prgwin_basofs+2
dw     00,255*256+01,prgwindsc2, 14,25, 98, 08,0    ;xx=SoundName Display
prgwin_bhz equ prgwin_basofs+3
dw     00,255*256+05,prgwindsc3, 64, 9, 12, 04,0    ;xx=KbpS Display \_
dw     00,255*256+05,prgwindsc4, 64,17, 12, 04,0    ;xx=KHz Display  /
dw ctlvol,255*256+19,0         , 96,52, 48, 06,0    ;xx=Click   Volume
prgwin_vol equ prgwin_basofs+6
dw ctlvol,255*256+10,gfx_sld_horw,132,52,12,06,0    ;xx=Slider  Volume
dw ctlpos,255*256+19,0         ,  4,38,124, 06,0    ;xx=Click   Position
prgwin_pos equ prgwin_basofs+8
dw     00,255*256+10,gfx_sld_horb,4,38, 12, 06,0    ;xx=Slider  Position
prgwin_shu equ prgwin_basofs+9
dw     00,255*256+00,8+128     ,141,28,  2,  3,0    ;xx=Display Shuffle
prgwin_rep equ prgwin_basofs+10
dw     00,255*256+00,8+128     ,141,18,  2,  3,0    ;xx=Display Repeat
prgwin_ana equ prgwin_basofs+11
dw anaswt,255*256+08,gfx_analyz1,80,11, 40, 12,0    ;xx=Frequenz-Analyzer

dw ctlrew,255*256+19,0         , 04,47, 13, 12,0    ;xx=Button Zurück
dw ctlply,255*256+19,0         , 18,47, 13, 12,0    ;xx=Button Play
dw ctlpau,255*256+19,0         , 32,47, 13, 12,0    ;xx=Button Pause
dw ctlstp,255*256+19,0         , 46,47, 13, 12,0    ;xx=Button Stop
dw ctlffw,255*256+19,0         , 60,47, 13, 12,0    ;xx=Button Vorwärts

dw ctlopn,255*256+19,0         ,131,36, 13, 10,0    ;xx=Button Auswerfen
dw ctlshu,255*256+19,0         ,124,25, 20,  9,0    ;xx=Button Shuffle
dw ctlrep,255*256+19,0         ,124,15, 20,  9,0    ;xx=Button Repeat

dw lstswt,255*256+19,0         ,124, 5, 20,  9,0    ;xx=Button Playlist
dw mixswt,255*256+19,0         , 82,50, 11, 10,0    ;xx=Button Equalizer

prgwindsc1  dw prgwintxt1:db 16*8+1,0+128:dw fnt_time
prgwindsc3  dw prgwintxt3:db 16*1+4,2+128:dw fnt_micro
prgwindsc4  dw prgwintxt4:db 16*1+4,2+128:dw fnt_micro
prgwindsc2  dw lstnam,16*8+3+16384+32768

;### PLAYLIST #################################################################

prglstdat   dw #3701,5,171,0,140,54,0,0,140,162,96,24,10000,10000,prgicnsml,prglsttit,0,prglstmen,prglstgrp,0,0:ds 136+14

prglstmen  dw 6, 1+4,prglstmentx1,prglstmen1,0, 1+4,prglstmentx2,prglstmen2,0, 1+4,prglstmentx3,prglstmen3,0, 1+4,prglstmentx4,prglstmen4,0, 1,prglstmentx5,lsteup,0, 1,prglstmentx6,lstedw,0
prglstmen1 dw 2, 17,prglstmen1tx1,lstafi,0, 17,prglstmen1tx2,lstadi,0
prglstmen2 dw 2, 17,prglstmen2tx1,lstrsl,0, 17,prglstmen2tx2,lstral,0
prglstmen3 dw 3, 17,prglstmen3tx1,lstsal,0, 17,prglstmen3tx2,lstsno,0, 17,prglstmen3tx3,lstsiv,0
prglstmen4 dw 5, 17,prglstmen4tx1,prplst,0, 1+8,0,0,0, 17,prglstmen4tx2,lstsrt,0, 17,prgwinmen1tx3,lstlod,0, 17,prgwinmen1tx4,lstsav,0

prglstgrp   db 2,0:dw prglstobj,prglstclc,0,00,0,0,0
prglstobj
dw 00,     255*256+0, 0,           0,0,0,1000,0     ;00=Hintergrund
dw lstclk, 255*256+41,prgobjlst1,  0,0,0, 162,0     ;01=Liste Einträge
prglstclc
dw 0,0,0,0,10000,0,10000,0
dw 0 , 0,0,0, 0,256+1,162,0

prgobjlst1  dw 0,0,lstentlst,0,256*0+1,lstentrow,0,2
lstentrow   dw 0,1000,00,0

;### PROPERTY FENSTER #########################################################

properwin   dw #1401,4+16,059,011,200,114,0,0,200,114,200,114,200,114,0,propertit,0,0,propergrp,0,0:ds 136+14
propergrp   db 19,0:dw properdat,0,0,256*8+8,0,0,0
properdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 1,properdsc1a,05, 07, 55, 8,0       ;01=Beschreibung "File"
dw      00,255*256+ 1,properdsc1b,51, 07,104,12,0       ;02=Angabe       "File"
dw      00,255*256+ 1,properdsc2a,05, 17, 55, 8,0       ;03=Beschreibung "Type"
dw      00,255*256+ 1,properdsc2b,51, 17,104,12,0       ;04=Angabe       "Type1"
dw      00,         0,1,          05, 38,190, 1,0       ;05=Trennlinie
dw  diacnc,255*256+16,prgtxtok,  147, 97, 48,12,0       ;06="Cancel"-Button
dw      00,255*256+ 1,properdsc2c,51, 27,104,12,0       ;07=Angabe       "Type2"
dw      00,255*256+ 1,properdsc3a,05, 42, 55, 8,0       ;08=Beschreibung "Title"
dw      00,255*256+ 1,properdsc3b,51, 42,104,12,0       ;09=Angabe       "Title"
dw      00,255*256+ 1,properdsc4a,05, 52, 55, 8,0       ;10=Beschreibung "Artist"
dw      00,255*256+ 1,properdsc4b,51, 52,104,12,0       ;11=Angabe       "Artist"
dw      00,255*256+ 1,properdsc5a,05, 62, 55, 8,0       ;12=Beschreibung "Album"
dw      00,255*256+ 1,properdsc5b,51, 62,104,12,0       ;13=Angabe       "Album"
dw      00,255*256+ 1,properdsc6a,05, 72, 55, 8,0       ;14=Beschreibung "Year"
dw      00,255*256+ 1,properdsc6b,51, 72,104,12,0       ;15=Angabe       "Year"
dw      00,255*256+ 1,properdsc7a,05, 82, 55, 8,0       ;16=Beschreibung "Comment"
dw      00,255*256+ 1,properdsc7b,51, 82,104,12,0       ;17=Angabe       "Comment"
dw      00,         0,1,          05, 93,190, 1,0       ;18=Trennlinie

properdsc1a dw propertxt1a,2+4
properdsc1b dw propertxt10,2+4
properdsc2a dw propertxt2a,2+4
properdsc2b dw 00         ,2+4
properdsc2c dw 00         ,2+4
properdsc3a dw propertxt3a,2+4
properdsc3b dw sndtxt_titl,2+4
properdsc4a dw propertxt4a,2+4
properdsc4b dw sndtxt_auth,2+4
properdsc5a dw propertxt5a,2+4
properdsc5b dw sndtxt_albm,2+4
properdsc6a dw propertxt6a,2+4
properdsc6b dw sndtxt_year,2+4
properdsc7a dw propertxt7a,2+4
properdsc7b dw sndtxt_comt,2+4

;### PREFERENCES ##############################################################

prgwinprf dw #1501,0,80,10,160,140,0,0,160,140,160,140,160,140, prgicnsml,prgtitprf,0,0,prggrpprf,0,0:ds 136+14
prggrpprf db 13,0:dw prgdatprf,0,0,2*256+2,0,0,0
prgdatprf
dw 00,     255*256+0,2, 0,0,1000,1000,0                 ;00=Hintergrund
dw prfclo, 255*256+16,prgtxtok,   60,125,40,12,0        ;01="Ok"-Button
dw 00,     255*256+3, prfobjdat1,  0, 1,160,56,0        ;02=Rahmen Detected
prgdatprf1
dw 00,     255*256+1, prfobjdat1a, 8,12,144, 8,0        ;03=Text hardware 1
dw 00,     255*256+1, prfobjdat1b, 8,22,144, 8,0        ;04=Text hardware 2
dw 00,     255*256+1, prfobjdat1c, 8,32,144, 8,0        ;05=Text hardware 3
dw 00,     255*256+1, prfobjdat1d, 8,42,144, 8,0        ;06=Text hardware 4
prgdatprf2
dw 00,     255*256+3, prfobjdat10, 0,58,160,28,0        ;07=Rahmen "manually configured"
dw 00,     255*256+17,prfobjdat11, 8,70,100, 8,0        ;08=check    opl3lpt
dw 00,     255*256+42,prfwprobj, 110,69, 40,12,0        ;09=dropdown opl3lpt port

dw 00,     255*256+3, prfobjdat2,  0, 87,160,38,0       ;10=Rahmen 6chn usage
dw 00,     255*256+18,prfobjdat2a, 8, 98,112, 8,0       ;11=only for 6chn psg modules
dw 00,     255*256+18,prfobjdat2b, 8,108,112, 8,0       ;12=for all psg modules

prfobjdat10 dw prfobjtxt12, 2+4
prfobjdat11 dw prfobjdatw,prftxtdat11,2+4
prfobjdatw  db 0

prfwprobj   dw 7,0,prfwprlst,0,1,prfwprrow,0,1
prfwprrow   dw 0,56,0,0
prfwprlst   dw #BC,prfwprtbc
            dw #AC,prfwprtac
            dw #A4,prfwprta4
            dw #9C,prfwprt9c
            dw #94,prfwprt94
            dw #8C,prfwprt8c
            dw #84,prfwprt84

prfobjdat1  dw prfobjtxt1, 2+4
prfobjdat1a dw prfobjtxt1z,2+4
prfobjdat1b dw prfobjtxt10,2+4
prfobjdat1c dw prfobjtxt10,2+4
prfobjdat1d dw prfobjtxt10,2+4
prfobjdat1f dw prfobjtxt10,2+4

prfobjdat2  dw prfobjtxt2, 2+4
prfobjdat2a dw prfobjdatv,prfobjtxt2a,256*0+2+4,prfobjdatk
prfobjdat2b dw prfobjdatv,prfobjtxt2b,256*1+2+4,prfobjdatk
prfobjdatv  db 0
prfobjdatk  ds 4

;### MIXER-SETTINGS ###########################################################
prgwinmix dw #1501,4,80,10,72,71,0,0,72,71,72,71,72,71, prgicnsml,prgtitmix,0,0,prggrpmix,0,0:ds 136+14
prggrpmix db 21,0:dw prgdatmix,0,0,2*256+2,0,0,0
prgdatmix
dw     00,255*256+00,128+6    ,0,0,10000,10000,0    ;00=background
dw     00,255*256+10,gfx_edge_ul,  0, 0,  4, 3,0    ;01=edge ul
dw     00,255*256+10,gfx_edge_ur, 68, 0,  4, 3,0    ;02=edge ur
dw     00,255*256+10,gfx_edge_dl,  0,68,  4, 3,0    ;03=edge dl
dw     00,255*256+10,gfx_edge_dr, 68,68,  4, 3,0    ;04=edge dr
gfx_titmix
dw     00,255*256+10,gfx_titmix,   4, 3, 36, 7,0    ;05=mix title
prgmix_volb equ 6
gfx_sldmix1
dw mixvol,255*256+10,gfx_sldmix1,  6,11,  8,48,0    ;06=mix volume
prgmix_basb equ 7
gfx_sldmix2
dw mixbas,255*256+10,gfx_sldmix2, 18,11,  8,48,0    ;07=mix bass
prgmix_treb equ 8
gfx_sldmix3
dw mixtre,255*256+10,gfx_sldmix3, 30,11,  8,48,0    ;08=mix treble
gfx_stereo
dw     00,255*256+10,gfx_stereo,  44, 3, 24,55,0    ;09=stereo mode
prgmix_panb equ 10
gfx_sldpan
dw mixpan,255*256+10,gfx_sldpan,   4,61, 64, 7,0    ;10=bar panning
prgmix_vol equ 11
dw mixvol,255*256+10,gfx_sld_vert, 6,11,  8,12,0    ;11=slider volume
prgmix_bas equ 12
dw mixbas,255*256+10,gfx_sld_vert,18,11+18,8,12,0   ;12=slider bass
prgmix_tre equ 13
dw mixtre,255*256+10,gfx_sld_vert,30,11+18,8,12,0   ;13=slider treble
prgmix_pan equ 14
dw mixpan,255*256+10,gfx_sld_horb,6+18,62,12,6,0    ;14=slider panning
prgmix_smd equ 15
dw     00,255*256+00,128+8       ,65, 8,  2, 3,0    ;15=sound mode old
dw     00,255*256+00,128+15      ,65,22,  2, 3,0    ;16=sound mode new
dw mixsm0,255*256+19,0           ,44, 3, 24,13,0    ;17=click mono
dw mixsm1,255*256+19,0           ,44,17, 24,13,0    ;18=click stereo
dw mixsm2,255*256+19,0           ,44,31, 24,13,0    ;19=click pseudo stereo
dw mixsm3,255*256+19,0           ,44,45, 24,13,0    ;20=click spatial stereo

;### ERROR-FENSTER ############################################################

daterrtab  dw daterrlod,daterrfrm,daterrmem,daterrocc

daterrlod  dw txterrlod1,4*1+2,txterrlod2,4*1+2,txterrlod3,4*1+2
daterrfrm  dw txterrfrm1,4*1+2,txterrfrm2,4*1+2,txterrfrm3,4*1+2
daterrmem  dw txterrmem1,4*1+2,txterrmem2,4*1+2,txterrmem3,4*1+2
daterrocc  dw txterrocc1,4*1+2,txterrocc2,4*1+2,txterrocc3,4*1+2

daterrunk  dw txterrunk1,4*1+2,txterrunk2,4*1+2,txterrunk3,4*1+2

daterrlfp  dw txterrlfp1,4*1+2,txterrlfp2,4*1+2,txterrlfp3,4*1+2
daterrlfe  dw txterrlfe1,4*1+2,txterrlfe2,4*1+2,txterrlfe3,4*1+2


;### Playlist
lstnam  db "SYMAMP 4.1",0
        ds 4+31-11

;--- BEG songlist filedata
lstfilid    db "SL"
lstanz      db 0        ;number of list entries
lstpthnum   db 0        ;number of different pathes
lstpthbuf   ds 512      ;all pathes with "/" ending, seperated/terminated by 0
lstpthend
lstentlst   ds lstmax*4
lstmemofs   dw lstmem
lstmem      ds 5    ;because of +4 to skip number
        ;##!! LAST LABEL !!##
;--- END songlist filedata
