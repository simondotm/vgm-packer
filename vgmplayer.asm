\ ******************************************************************
\ *	Headers
\ ******************************************************************


; Allocate vars in ZP
.zp_start
ORG 0
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

INCLUDE "lib/exomiser.h.asm"


.zp_end


\ ******************************************************************
\ *	Utility code - always memory resident
\ ******************************************************************

ORG &1900
.start

;INCLUDE "lib/swr.asm"
;INCLUDE "lib/print.asm"     ; feels unnecessary, hardly used, and only for debugging mainly
;INCLUDE "lib/disksys.asm"

.vgm_stream_0
INCBIN "data/nd-ui.bin.0.part"
.vgm_stream_1
INCBIN "data/nd-ui.bin.1.part"
.vgm_stream_2
INCBIN "data/nd-ui.bin.2.part"
.vgm_stream_3
INCBIN "data/nd-ui.bin.3.part"
.vgm_stream_4
INCBIN "data/nd-ui.bin.4.part"
.vgm_stream_5
INCBIN "data/nd-ui.bin.5.part"
.vgm_stream_6
INCBIN "data/nd-ui.bin.6.part"
.vgm_stream_7
INCBIN "data/nd-ui.bin.7.part"



;-------------------------------
; vgm player


; A contains data to be written to sound chip
; clobbers X
.sn_write
{
    ldx #255
    stx &fe43
    sta &fe41
    inx
    stx &fe40
    lda &fe40
    ora #8
    sta &fe40
    rts ; 21 bytes
}

; A contains data to be written to sound chip
; clobbers Y
.sn_write2
{
	sei					; **SELF-MODIFIED CODE**

	ldy #255
	sty &fe43
	
	sta &fe41
	lda #0
	sta &fe40
	nop
	nop
	nop
	nop
	nop
	nop
	lda #8
	sta &fe40

	cli					; **SELF-MODIFIED CODE**
	rts
}


.sn_reset
{
	\\ Zero volume on all channels
	lda #&9f : jsr sn_write
	lda #&bf : jsr sn_write
	lda #&df : jsr sn_write
	lda #&ff : jsr sn_write
	rts
}

.vgm_streams
    equb LO(vgm_stream_0)
    equb LO(vgm_stream_1)
    equb LO(vgm_stream_2)
    equb LO(vgm_stream_3)
    equb LO(vgm_stream_4)
    equb LO(vgm_stream_5)
    equb LO(vgm_stream_6)
    equb LO(vgm_stream_7)
    equb HI(vgm_stream_0)
    equb HI(vgm_stream_1)
    equb HI(vgm_stream_2)
    equb HI(vgm_stream_3)
    equb HI(vgm_stream_4)
    equb HI(vgm_stream_5)
    equb HI(vgm_stream_6)
    equb HI(vgm_stream_7)


; A is stream id (0-7)
; clobbers X,Y
.vgm_get_stream_byte
{
    tax
    lda vgm_streams + 0, X
    sta &80
    lda vgm_streams + 8, X
    sta &81
    inc vgm_streams + 0, X
    bne no_page
    inc vgm_streams + 8, X
.no_page
    ldy #0
    lda (&80), y
    rts
}


.vgm_finished EQUB 0

.vgm_get_data
{
    ; SN76489 data register format is %1cctdddd where cc=channel, t=0=tone, t=1=volume, dddd=data

    ; Get Channel 3 tone
    lda #3:jsr vgm_get_stream_byte
    cmp #&ff:bne no_eof
    jsr sn_reset
    sta vgm_finished
    rts
.no_eof
    ; skip tone3 updates if 15
    cmp #&0f:beq no_change3

    ora #&80 + (3<<5):jsr sn_write

.no_change3

    ; Channel 0 tone
    lda #0:jsr vgm_get_stream_byte:ora #&80 + (0<<5):jsr sn_write
    lda #0:jsr vgm_get_stream_byte:ora #&00:jsr sn_write

    ; Channel 1 tone
    lda #1:jsr vgm_get_stream_byte:ora #&80 + (1<<5):jsr sn_write
    lda #1:jsr vgm_get_stream_byte:ora #&00:jsr sn_write

    ; Channel 2 tone
    lda #2:jsr vgm_get_stream_byte:ora #&80 + (2<<5):jsr sn_write
    lda #2:jsr vgm_get_stream_byte:ora #&00:jsr sn_write


    ; Channel 0-3 volumes
    lda #4:jsr vgm_get_stream_byte:ora #&90 + (0<<5):jsr sn_write
    lda #5:jsr vgm_get_stream_byte:ora #&90 + (1<<5):jsr sn_write
    lda #6:jsr vgm_get_stream_byte:ora #&90 + (2<<5):jsr sn_write
    lda #7:jsr vgm_get_stream_byte:ora #&90 + (3<<5):jsr sn_write

    rts
}

; returns non-zero when VGM is finished.
.vgm_update
{
    lda vgm_finished
    pha
    bne done
    jsr vgm_get_data
.done
    pla
    rts
}

.vgm_init
{
    lda #0:sta vgm_finished
    jsr sn_reset
    rts
}





;----------------------------

.main
{
    jsr vgm_init

.loop
    lda #19:jsr &fff4
    jsr vgm_update
    beq loop
    rts
}

.end

SAVE "Main", start, end, main

