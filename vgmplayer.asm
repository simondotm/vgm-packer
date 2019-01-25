\ ******************************************************************
\ *	Headers
\ ******************************************************************

; VGM player uses N zero page vars
VGM_ZP = &80


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

.vgm_data
INCBIN "data/nd-ui.bin.lz4"

IF FALSE
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
ENDIF


;-------------------------------
; vgm player
.vgm_start

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
    equw 0,0,0,0,0,0,0,0
IF FALSE
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
ENDIF

; A is stream id (0-7)
; clobbers X,Y
.vgm_get_stream_byte
{
    tax
    lda vgm_streams + 0, X
    sta VGM_ZP+0
    lda vgm_streams + 8, X
    sta VGM_ZP+1
    inc vgm_streams + 0, X
    bne no_page
    inc vgm_streams + 8, X
.no_page
    ldy #0
    lda (VGM_ZP+0), y
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
    lda #0:jsr vgm_get_stream_byte:jsr sn_write

    ; Channel 1 tone
    lda #1:jsr vgm_get_stream_byte:ora #&80 + (1<<5):jsr sn_write
    lda #1:jsr vgm_get_stream_byte:jsr sn_write

    ; Channel 2 tone
    lda #2:jsr vgm_get_stream_byte:ora #&80 + (2<<5):jsr sn_write
    lda #2:jsr vgm_get_stream_byte:jsr sn_write


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



; X/Y point to VGM data stream
.vgm_init
{
    ; parse data stream
    ; we use LZ4 frame & block format for convenience
    ; however there are assumptions for format:
    ;  Magic number[4], Flags[1], MaxBlockSize[1], Header checksum[1]
    ;  Contains 8 blocks
    ; Obviously since this is an 8-bit CPU no files or blocks can be > 64Kb in size
zp_block_data = VGM_ZP+0
zp_block_size = VGM_ZP+2

    ; Skip frame header, and move to first block
    txa
    clc
    adc #7
    sta zp_block_data+0
    tya
    adc #0
    sta zp_block_data+1

    ; read the block headers (size)
    ldx #0
.block_loop

    ; read 16-bit block size to zp_block_size
    ldy #0
    lda (zp_block_data),Y
    sta zp_block_size+0
    iny
    lda (zp_block_data),Y
    sta zp_block_size+1

    ; get address of block, store in vgm_streams[x]
    lda zp_block_data+0
    clc
    adc #4
    sta zp_block_data+0
    sta vgm_streams + 0, x
    lda zp_block_data+1
    adc #0
    sta zp_block_data+1
    sta vgm_streams + 8, x

    ; move to next block
    lda zp_block_data+0
    clc
    adc zp_block_size+0
    sta zp_block_data+0
    lda zp_block_data+1
    adc zp_block_size+1
    sta zp_block_data+1

    ; for all 8 blocks
    inx
    cpx #8
    bne block_loop

    ; clear vgm finished flag
    lda #0:sta vgm_finished

    ; reset soundchip
    jsr sn_reset
    rts
}


.vgm_end


PRINT " vgm code size is", (vgm_end-vgm_start), "bytes"

;----------------------------

.main
{
    ldx #lo(vgm_data)
    ldy #hi(vgm_data)
    jsr vgm_init

.loop
    lda #19:jsr &fff4
    jsr vgm_update
    beq loop
    rts
}

.end

SAVE "Main", start, end, main

