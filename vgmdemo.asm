;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-packer
;******************************************************************


SPEED_TEST = FALSE   ;  *run Main in MODE2 to see blue line where vgmplayer finishes work
TEST_DATA = FALSE

; Allocate vars in ZP
.zp_start
ORG &70
GUARD &8f


;----------------------------------------------------------------------------------------------------------
; Common global defines
;----------------------------------------------------------------------------------------------------------
INCLUDE "lib/bbc.h.asm"
INCLUDE "lib/bbc_utils.h.asm"



;----------------------------------------------------------------------------------------------------------
; Common code headers
;----------------------------------------------------------------------------------------------------------
; Include common code headers here - these can declare ZP vars from the pool using SKIP...

INCLUDE "lib/vgmplayer.h.asm"

.zp_end


\ ******************************************************************
\ *	Utility code - always memory resident
\ ******************************************************************

ORG &1100
GUARD &7c00

.start

;----------------------------


;-------------------------------------------
; main
;-------------------------------------------



.vgm_buffer_start

; reserve space for the vgm decode buffers (8x256 = 2Kb)
ALIGN 256
.vgm_stream_buffers
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256


.vgm_buffer_end


.main
{
    ; initialize the vgm player with a vgc data stream
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgm_data)
    ldy #hi(vgm_data)
    jsr vgm_init

;    lda #22
;    jsr &ffee
;    lda #2
;    jsr &ffee


IF TEST_DATA
    jmp mytest
ENDIF

    ; loop & update
    sei
.loop
    ;lda #19:jsr &fff4

; set to false to playback at full speed for performance testing
IF TRUE 
    lda #2
    .vsync1
    bit &FE4D
    beq vsync1
    sta &FE4D
ENDIF

    lda #&03:sta&fe21
    jsr vgm_update
    pha
    lda #&07:sta&fe21
    pla
    beq loop
    cli
    rts
}



IF TEST_DATA
.testdata
INCBIN "data/nd-ui.vgm.3.part"

.hex equs "0123456789ABCDEF"
.drawnum
{
    pha
    and #&f0
    lsr a:lsr a:lsr a:lsr a
    tax
    lda hex, X
    jsr &ffee
    pla
    and #&0f
    tax
    lda hex, X
    jsr &ffee

    rts
}



ALIGN 256
.mytest
{
START = testdata
;DLEN0 = &3f2
;DLEN1 = &3d5
;DLEN2 = &457
;DLEN3 = &48
;DLEN4 = &1ea
;DLEN5 = &18
;DLEN6 = &5d
;DLEN7 = &b0

DLEN0=&162
DLEN1=&192
DLEN2=&21b
DLEN3=&2f
DLEN4=&18a
DLEN5=&10
DLEN6=&4f
DLEN7=&6e

OFFSET0 = START ; + 4 + 7
OFFSET1 = OFFSET0 + DLEN0 + 4
OFFSET2 = OFFSET1 + DLEN1 + 4
OFFSET3 = OFFSET2 + DLEN2 + 4
OFFSET4 = OFFSET3 + DLEN3 + 4
OFFSET5 = OFFSET4 + DLEN4 + 4
OFFSET6 = OFFSET5 + DLEN5 + 4
OFFSET7 = OFFSET6 + DLEN6 + 4

STREAM = 3
OFFSET = OFFSET3
DLEN = DLEN3


    lda #STREAM
   ; set the stream buffer (is fixed)
    tax
    clc
    adc vgm_buffers ; hi byte of where the 2kb vgm stream buffer is located
    sta zp_buffer+1
    lda #0
    sta zp_buffer+0
    txa
    
    ; calculate the stream buffer context
    asl a
    asl a
    asl a
IF USE_HUFFMAN
    asl a
ENDIF
    sta temp
    tax

    ; since we have 8 separately compressed register streams
    ; we have to load the required decoder context to ZP
    jsr vgm_load_register_context   ; TODO:inline


    lda #lo(testdata)
    sta &90
    lda #hi(testdata)
    sta &91

    lda #lo(DLEN)
    sta &92
    lda #hi(DLEN)
    sta &93


    .loop
    ;jsr lz_fetch_byte:sta &94
    jsr vgm_update_register1:sta &94


    ldy #0
    lda (&90),Y
    cmp &94
    beq ok

    lda &91:jsr drawnum
    lda &90:jsr drawnum
    lda #32:jsr &ffee

    lda &94:jsr drawnum
    lda #32:jsr &ffee
    lda #32:jsr &ffee
.ok

    inc &90
    bne skip
    inc &91
.skip

    dec &92
    bne skip2
    lda &93
    beq end
    dec &93
.skip2

    ;jsr &ffe0
    jmp loop

.end 
lda#65:jsr &ffee
rts
jsr &ffe0
.temp equb 0
}
ENDIF


INCLUDE "lib/vgmplayer.asm"



.vgm_data
INCBIN "testvgm/nd-ui.vgc"
;INCBIN "testvgm/androids.vgc"
;INCBIN "testvgm/syner5.vgc"
;INCBIN "testvgm/darkside1.vgc"
;INCBIN "testvgm/bbcapple.vgc"
;INCBIN "testvgm/mongolia.vgc"
;INCBIN "testvgm/things.vgc"
;INCBIN "testvgm/lethal7.vgc"
;INCBIN "testvgm/CPCTL10A.vgc"
;INCBIN "testvgm/bestpart.vgc"
;INCBIN "testvgm/tale7.vgc"


PRINT ~vgm_data


.end

SAVE "Main", start, end, main

