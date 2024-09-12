;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                             C P U   M e t e r                              @
;@                          (SymbOS Desktop Widget)                           @
;@             (c) 2016-2016 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;- widget management in symbosxt buggy


prgprz  ld hl,wdggrpwin0
        call wdgini
        call wdgclk0

        ld b,10                 ;wait for first message (max 10 idles)
prgprz1 push bc
        rst #30
        ld a,(App_PrcID)
        db #dd:ld l,a
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #18
        db #dd:dec l
        pop bc
        jr z,prgprz2
        djnz prgprz1
        jr prgend

prgprz0 ld a,(App_PrcID)
        db #dd:ld l,a
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #18
        db #dd:dec l
        jr z,prgprz2
        rst #30
        ld hl,moncnt
        dec (hl)
        call z,monupd
        jr prgprz0
prgprz2 ld a,(App_MsgBuf)
        or a
        jr z,prgend
        cp MSR_DSK_WCLICK
        jr z,prgprz4
        cp MSC_WDG_SIZE
        jp z,wdgsiz
        cp MSC_WDG_PROP
        jp z,wdgprp
        cp MSC_WDG_CLICK
        jp z,wdgclk
        jr prgprz0
prgprz4 ld a,(App_MsgBuf+2)
        cp DSK_ACT_CLOSE
        jp z,wdgprp0
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
        ld hl,(App_MsgBuf+8)
        ld a,l
        or h
        jr z,prgprz0
        jp (hl)

;### PRGEND -> End program
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0


;==============================================================================
;### MONITOR ROUTINES #########################################################
;==============================================================================

cfgdatbeg
cfgflg  db 1    ;0=slow, 1=normal, 2=fast
cfgdatend

moncnt  db 1

;### MONUPD -> Updated Monitor-Widget
monupdler   dw 0        ;idle counter
monupdsys   dw 0        ;system counter
monlascpu   db -1       ;last cpu usage
monlasmem   ds 4        ;last memory free

monupd  ld (hl),100
monupd6 ld hl,jmp_mtgcnt        ;*** CPU-Last Infos holen
        rst #28                 ;IY,IX=Systemzähler, DE=Leerlaufprozess-Counter
        ld hl,(monupdler)
        ex de,hl
        ld (monupdler),hl
        or a
        sbc hl,de               ;HL=Differenz Leerlauf-Counter
        push hl  
        push ix
        pop hl
        ld de,(monupdsys)
        ld (monupdsys),hl
        or a
        sbc hl,de               ;HL=Differenz System-Counter
        ex de,hl
        ld a,156
        call clcm16             ;HL=Sys*156
        pop de
        push hl
        ld bc,100
        call clcmul             ;A,HL=Leer*100
        pop de
        ld c,l
        ld b,h
        call clcdiv             ;HL = (Leer * 100) / (Sys * 156) = freie CPU in %
        ex de,hl
        ld hl,100
        or a
        sbc hl,de
        jr nc,monupd1
        ld hl,0                 ;HL = 100-freie CPU = Auslastung
monupd1 ld a,l
        cp 101
        jr c,monupd2
        ld a,100
monupd2 ld hl,monlascpu
        cp (hl)
        jr z,monupd4
        ld (hl),a
        ld l,a
        ld h,0
        push hl
        ld iy,wdgdatwin0a
        call monadj             ;A=A*XX/100
        pop ix                  ;IX=Wert
        ld iy,txtcpuval         ;(IY)=Ascii-String
        ld e,3
        ld hl,jmp_clcnum
        rst #28                 ;Wert umwandeln
        ld (iy+1),"%"
        ld (iy+2),0
        ld bc,3*256+3
        call monupd0

monupd4 ld hl,jmp_memsum        ;*** Speicher-Infos holen
        rst #28                 ;E,IX=freier Speicher insgesamt, D=Anzahl verfügbarer Bänke
        push ix:pop bc
        ld hl,(monlasmem+0)
        or a
        sbc hl,bc
        ld (monlasmem+0),bc
        ld a,(monlasmem+2)
        ld (monlasmem+2),de
        jr nz,monupd5
        cp e
        ret z
monupd5 ld l,d                  ;Gesamter Speicher
        ld h,0
        add hl,hl:add hl,hl:add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ld bc,64
        add hl,bc               ;HL=Gesamter Speicher
        push hl
        push hl
        push ix
        push de
        ld iy,txttotval
        call mondsp
        pop de                  ;Freier Speicher
        pop ix
        ld h,e
        db #dd:ld a,h
        ld l,a
        srl h:rr l
        srl h:rr l
        push hl
        ld iy,txtfreval
        call mondsp
        pop de                  ;Belegter Speicher (=Gesamt-Frei)
        pop hl
        or a
        sbc hl,de
        push hl
        ld iy,txtusdval
        call mondsp
        pop de                  ;DE=Belegt
        ld a,50
        call clcm16             ;HL=Belegt*50
        ld c,l
        ld b,h
        pop de
        srl d:rr e
        call clcd16             ;HL=Belegt*50/(Gesamt/2)=MemNutzung in %
        ld a,l
        cp 101
        jr c,monupd3
        ld a,100
monupd3 ld l,a
        ld h,0
        push hl
        ld iy,wdgdatwin0b
        call monadj             ;A=A*XX/100
        pop ix                  ;IX=Wert
        ld iy,txtramval         ;(IY)=Ascii-String
        ld e,3
        ld hl,jmp_clcnum
        rst #28                 ;Wert umwandeln
        ld (iy+1),"%"
        ld (iy+2),0
        ld bc,256*3+07
        call monupd0
        ld a,(wdggrpwin0)
        cp 16
        ret c
        ld bc,3*256+13
monupd0 nop
        push bc
        ld de,(wdgctrid)
        ld d,c
        ld a,(wdgwinid)
        call SyDesktop_WINSIN
        pop bc
        inc c
        djnz monupd0
        ret

;### MONADJ -> calculates bar-value
;### Input      A=percent (0-100), IY=bar control
;### Output     A=value (0-X)
monadjm dw 96
monadj  ld de,(monadjm)
        call clcm16
        ld c,l
        ld b,h
        ld de,100
        call clcd16
        ld a,l
        ld (iy+10),a
        add 27
        ld (iy+06+16),a
        sub 27
        ld (iy+2),0
        jr nz,monadj1
        ld (iy+2),64
monadj1 neg
        ld hl,monadjm
        add (hl)
        ld (iy+10+16),a
        ld (iy+02+16),0
        ret nz
        ld (iy+02+16),64
        ret

;### MONDSP -> updates memory value
;### Input      HL=new value, IY=textpointer
;### Destroyed  AF,BC,DE,HL,IX
mondsp  push hl:pop ix
        ld e,4
        ld hl,jmp_clcnum
        rst #28                 ;Wert umwandeln
        ld (iy+1),"K"
        ld (iy+2),0
        ret


;==============================================================================
;### WIDGET ROUTINES ##########################################################
;==============================================================================

wdgwinid    db 0    ;window ID
wdgctrid    db 0    ;control collection ID

;### WDGINI -> init controls
;### Input      HL=control group
wdgini  ld b,(hl)
        inc hl:inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        inc hl
        ld a,(App_PrcID)
        ld de,16
wdgini1 ld (hl),a
        add hl,de
        djnz wdgini1
        ret

;### WDGSIZ -> size event
wdgsizt db 10,68, 97,22,  2, 33, 65, 0
        db 16,68, 97,39,  2, 33, 65, 0
        db 10,96,149,22, 13, 59,105, 0
        db 16,96,149,39, 13, 59,105, 0

wdgsiz  push ix
        ld hl,(App_MsgBuf+1)
        ld (wdgwinid),hl
        ld a,(App_MsgBuf+3)
        add a
        add a
        add a
        ld l,a
        ld h,0
        ld bc,wdgsizt
        add hl,bc
        ld a,(hl)
        ld (wdggrpwin0+0),a
        inc hl
        ld a,(hl)
        ld (monadjm),a
        inc hl
        ld a,(hl)
        ld (wdgdatwin0+16+10),a
        inc hl
        ld a,(hl)
        ld (wdgdatwin0+16+12),a
        ld ix,wdgdatwin0c
        inc hl:ld a,(hl):ld (ix+00+6),a:ld (ix+00+6+48),a
        inc hl:ld a,(hl):ld (ix+16+6),a:ld (ix+16+6+48),a
        inc hl:ld a,(hl):ld (ix+32+6),a:ld (ix+32+6+48),a

        ld hl,-1
        ld (monlascpu),hl
        ld (monlascpu+2),hl
        ld a,#c9
        ld (monupd0),a
        call monupd6
        xor a
        ld (monupd0),a

        pop ix
        ld hl,256*FNC_DXT_WDGOKY+MSR_DSK_EXTDSK
        ld (App_MsgBuf+0),hl
        ld hl,wdgobjsup
        ld (App_MsgBuf+2),hl
        ld a,(App_BnkNum)
        ld (App_MsgBuf+4),a
        ld a,(App_PrcID)
        db #dd:ld l,a
        ld iy,App_MsgBuf
        rst #10
        jp prgprz0

;### WDGPRP -> properties event
wdgprpw db 0

wdgprp  ld a,(wdgprpw)
        or a
        jp nz,prgprz0
        ld a,(cfgflg)
        ld (cfgspd),a
        ld de,configwin
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN
        ld (wdgprpw),a
        jp prgprz0
wdgprp1 ld a,(cfgspd)
        ld (cfgflg),a
        call wdgclk0
wdgprp0 ld hl,wdgprpw           ;close
        ld a,(hl)
        ld (hl),0
        call SyDesktop_WINCLS
        jp prgprz0

;### WDGCLK -> click event
wdgclk  jp prgprz0
wdgclk0 ld a,(cfgflg)
        cp 1
        ld a,100
        jr z,wdgclk1
        ld a,200
        jr c,wdgclk1
        ld a,50
wdgclk1 ld (monupd+1),a
        ret


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

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
;### Ausgabe    HL=Wert1/Wert2 (16bit)
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
        rl c
        rla
        ld h,a
        ld l,c
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
;### Ausgabe    HL=Wert1/Wert2
;### Veraendert AF,BC,DE,IX,IY
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        db #dd:or h
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        rl l
        rl h
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
clcdiv2 ccf
        db #fd:dec l
        jr nz,clcdiv1
        rl c
        rl b
        ld l,c
        ld h,b
        ret


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

App_BegData

txtcpudsc   db "CPU",0
txtramdsc   db "RAM",0
txtusddsc   db "Used",0
txtfredsc   db "Free",0
txttotdsc   db "Total",0

configtit   db "CPU Meter Setup",0
configtxt0a db "Settings",0
configtxt0b db "About",0
configtxt0c db "CPU Meter Desktop Widget for SymbOS",0
configtxt0d db "(c)2016 by Prodatron/SymbiosiS",0
configtxt1  db "Update speed",0
configtxt1a db "Slow",0
configtxt1b db "Normal",0
configtxt1c db "Fast",0

prgtxtok    db "OK",0
prgtxtcnc   db "Cancel",0


;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns
;### PRGPRZS -> stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID db 0

;### App_MsgBuf -> message buffer
App_MsgBuf ds 14

txtcpuval   db "xxx%",0
txtramval   db "xxx%",0
txtusdval   db "0000K",0
txtfreval   db "0000K",0
txttotval   db "0000K",0

;### WIDGET CONTROL COLLECTION ################################################

wdgobjsup   dw wdggrpwin0,1000,1000,0,0,0

objcpudsc   dw txtcpudsc,16*6+1+32768
objcpuval   dw txtcpuval,16*8+1+32768+256+16384
objramdsc   dw txtramdsc,16*6+1+32768
objramval   dw txtramval,16*8+1+32768+256+16384
objusddsc   dw txtusddsc,16*6+1+32768+512
objusdval   dw txtusdval,16*8+1+32768+512+16384
objfredsc   dw txtfredsc,16*6+1+32768+512
objfreval   dw txtfreval,16*8+1+32768+512+16384
objtotdsc   dw txttotdsc,16*6+1+32768+512
objtotval   dw txttotval,16*8+1+32768+512+16384

wdggrpwin0  db 16,0:dw wdgdatwin0,0,0,00*256+00,0,0,00
wdgdatwin0
dw 00,255*256+ 0,128+1,       0, 0,1000,1000,0
dw 00,255*256+ 2,256*255+128, 1, 1, 149,  39,0      ;01 frame
dw 00,255*256+ 1,objcpudsc,  06,04,  24,   8,0      ;02 cpu
wdgdatwin0a
dw 00,255*256+ 0,128+8,      27,05,   1,   6,0      ;03 cpu-balken-used
dw 00,255*256+ 0,128+7,      27,05,   1,   6,0      ;04 cpu-balken-free
dw 00,255*256+ 1,objcpuval, 124,04,  22,   8,0      ;05 cpu value

dw 00,255*256+ 1,objramdsc,  06,12,  24,   8,0      ;06 ram
wdgdatwin0b
dw 00,255*256+ 0,128+8,      27,13,   1,   6,0      ;07 ram-balken-used
dw 00,255*256+ 0,128+7,      27,13,   1,   6,0      ;08 ram-balken-free
dw 00,255*256+ 1,objramval, 124,12,  22,   8,0      ;09 ram value
wdgdatwin0c
dw 00,255*256+ 1,objusddsc,  13,21,  31,   8,0      ;10 used
dw 00,255*256+ 1,objfredsc,  59,21,  32,   8,0      ;11 free
dw 00,255*256+ 1,objtotdsc, 105,21,  32,   8,0      ;12 total
dw 00,255*256+ 1,objusdval,  13,29,  31,   8,0      ;13 used value
dw 00,255*256+ 1,objfreval,  59,29,  32,   8,0      ;14 free value
dw 00,255*256+ 1,objtotval, 105,29,  32,   8,0      ;15 total value


;### PROPERTIES ###############################################################

configwin   dw #1501,0,059,035,192,75,0,0,192,75,192,75,192,75,prgicnsml,configtit,0,0,configgrp,0,0:ds 136+14
configgrp   db 11,0:dw configdat,0,0,256*11+10,0,0,00
configdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 3,configdsc0a,00, 01,192,25,0       ;01=Rahmen "Settings"
dw      00,255*256+ 1,configdsc1, 08, 11, 54, 8,0       ;02=Beschreibung "Update Speed"
dw      00,255*256+18,configrad1a,74, 11, 20, 8,0       ;03=Radio Slow
dw      00,255*256+18,configrad1b,106,11, 20, 8,0       ;04=Radio Normal
dw      00,255*256+18,configrad1c,148,11, 20, 8,0       ;05=Radio Fast
dw      00,255*256+ 3,configdsc0b,00, 25,192,35,0       ;06=Rahmen "Misc"
dw      00,255*256+ 1,configdsc0c,08, 35,144, 8,0       ;07=Beschreibung "About 1"
dw      00,255*256+ 1,configdsc0d,08, 45,144, 8,0       ;08=Beschreibung "About 2"
dw wdgprp1,255*256+16,prgtxtok,   91, 60, 48,12,0       ;09="Ok"    -Button
dw wdgprp0,255*256+16,prgtxtcnc, 141, 60, 48,12,0       ;10="Cancel"-Button

configdsc0a dw configtxt0a,2+4
configdsc0b dw configtxt0b,2+4
configdsc0c dw configtxt0c,2+4
configdsc0d dw configtxt0d,2+4

configdsc1  dw configtxt1,2+4
configrad1k ds 4
configrad1a dw cfgspd,configtxt1a,256*0+2+4,configrad1k
configrad1b dw cfgspd,configtxt1b,256*1+2+4,configrad1k
configrad1c dw cfgspd,configtxt1c,256*2+2+4,configrad1k

cfgspd db 1
