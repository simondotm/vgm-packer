;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-packer
;******************************************************************


; Allocate vars in ZP
.zp_start
ORG &70
GUARD &8f

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
    sec
    jsr vgm_init

    ; loop & update
    sei
.loop

; set to false to playback at full speed for performance testing
IF TRUE 
    ; vsync
    lda #2
    .vsync1
    bit &FE4D
    beq vsync1
    sta &FE4D
ENDIF


    ;ldy#10:.loop0 ldx#0:.loop1 nop:nop:dex:bne loop1:dey:bne loop0

    lda #&03:sta&fe21
    jsr vgm_update
    pha
    lda #&07:sta&fe21
    pla
    beq loop
    cli
    rts
}



INCLUDE "lib/vgmplayer.asm"

; include your tune of choice here, some samples provided....
.vgm_data
INCBIN "testvgm/ym_009.vgc"
;INCBIN "testvgm/song_091.vgc"
;INCBIN "testvgm/axelf.vgc"
;INCBIN "testvgm/bbcapple.vgc"
;INCBIN "testvgm/nd-ui.vgc"
;INCBIN "testvgm/outruneu.vgc"



PRINT ~vgm_data


.end

SAVE "Main", start, end, main

