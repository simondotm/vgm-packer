\ ******************************************************************
\ *	Headers
\ ******************************************************************

; VGM player uses 11 zero page vars from this address
VGM_ZP = &80
LZ4_FORMAT = FALSE

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

;INCLUDE "lib/exomiser.h.asm"


.zp_end


\ ******************************************************************
\ *	Utility code - always memory resident
\ ******************************************************************

ORG &1100
GUARD &7c00

.start

;----------------------------

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

    ; loop & update
.loop
    lda #19:jsr &fff4
    jsr vgm_update
    beq loop
    rts
}


;INCLUDE "lib/swr.asm"
;INCLUDE "lib/print.asm"     ; feels unnecessary, hardly used, and only for debugging mainly
;INCLUDE "lib/disksys.asm"



;---------------------------------------------------------------
; VGM library code
;---------------------------------------------------------------

;-------------------------------
; lz4 decoder
;-------------------------------
.lz_start

; 8 zero page registers used
lz_zp = VGM_ZP + 0
zp_stream_src   = lz_zp + 0    ; stream data ptr LO/HI
zp_literal_cnt  = lz_zp + 2    ; literal count LO/HI
zp_match_cnt    = lz_zp + 4    ; match count LO/HI
zp_window_src   = lz_zp + 6    ; window read ptr - index
zp_window_dst   = lz_zp + 7    ; window write ptr - index
; 8 bytes total workspace

; these variables are not preserved across context switches
zp_buffer = lz_zp + 8          ; Lo byte of current decode window buffer
zp_temp = lz_zp + 10           ; 2 bytes
zp_stash = lz_zp + 12          ; 1 byte
; 13 bytes total

; fetch a byte from the currently selected compressed register data stream
; returns byte in A, clobbers Y
.lz_fetch_byte
{
    ldy #0
    lda (zp_stream_src),y
    inc zp_stream_src+0
    bne ok
    inc zp_stream_src+1
.ok
    rts
}

; fetch a byte from the current decode buffer at the current read ptr offset
; returns byte in A, clobbers Y
.lz_fetch_buffer
{
    ldy zp_window_src
    lda (zp_buffer),Y
    iny
    sty zp_window_src
    rts
}

; push byte into decode buffer
; clobbers Y, preserves A
.lz_store_buffer
{
    ldy zp_window_dst
    sta (zp_buffer),Y
    iny
    sty zp_window_dst
    rts
}

; Calculate a multi-byte lz4 style length into zp_temp
; On entry A contains the initial counter value (LO)
; Returns 16-bit length in A/X (A=LO, X=HI)
; Clobbers Y, zp_temp+0, zp_temp+1
.lz_fetch_count
{
    sta zp_temp+0
    ldx #0
    stx zp_temp+1
    cmp #15             ; >=15 signals byte extend
    bne done
.fetch
    jsr lz_fetch_byte
    tay
    clc
    adc zp_temp+0
    sta zp_temp+0
    lda zp_temp+1
    adc #0
    sta zp_temp+1
    cpy #255            ; 255 signals byte extend       
    beq fetch
    tax
    lda zp_temp+0
.done
    ; A/X now contain count (LO/HI)
    rts
}

; decode a byte from the currently selected register stream
; unlike typical lz style unpackers we are using a state machine
; because it is necessary for us to be able to decode a byte at a time from 8 separate streams
.lz_decode_byte
{
    ; decoder state is:
    ;  empty - fetch new token & prepare
    ;  literal - decode new literal
    ;  match - decode new match

    ; lz4 block format:
    ;  [TOKEN][LITERAL LENGTH][LITERALS][...][MATCH OFFSET][MATCH LENGTH]

; try fetching a literal byte from the stream
.try_literal

    lda zp_literal_cnt+0
    bne is_literal
    lda zp_literal_cnt+1
    beq try_match

.is_literal

    ; fetch a literal & stash in decode buffer
    jsr lz_fetch_byte     
    jsr lz_store_buffer
    sta zp_stash

    ; for all literals
    dec zp_literal_cnt+0
    bne end_literal
    lda zp_literal_cnt+1
    beq begin_matches
    dec zp_literal_cnt+1
    bne end_literal

.begin_matches

    ; literals run completed
    ; now fetch match offset & length

    ; get match offset LO
    jsr lz_fetch_byte     

    ; set buffer read ptr
    sta zp_temp
    lda zp_window_dst
    sec
    sbc zp_temp
    sta zp_window_src

IF LZ4_FORMAT
    ; fetch match offset HI, but ignore it.
    ; this implementation only supports 8-bit windows.
    jsr lz_fetch_byte    
ENDIF

    ; fetch match length
    lda zp_match_cnt+0
    jsr lz_fetch_count
    ; match length is always+4 (0=4)
    ; cant do this before because we need to detect 15
    stx zp_match_cnt+1
    clc
    adc #4
    sta zp_match_cnt+0
    lda zp_match_cnt+1
    adc #0
    sta zp_match_cnt+1

.end_literal
    lda zp_stash
    rts


; try fetching a matched byte from the stream
.try_match

    lda zp_match_cnt+1
    bne is_match
    lda zp_match_cnt+0
    ; all matches done, so get a new token.
    beq try_token

.is_match

    jsr lz_fetch_buffer    ; fetch matched byte from decode buffer
    jsr lz_store_buffer    ; stash in decode buffer
    sta zp_stash

    ; for all matches
    ; we know match cnt is at least 1
    lda zp_match_cnt+0
    bne skiphi
    dec zp_match_cnt+1
.skiphi
    dec zp_match_cnt+0

.end_match
    lda zp_stash
    rts


; then token parser
.try_token


    ; fetch a token
    jsr lz_fetch_byte     

    tax
    ldy #0

    ; unpack match length from token (bottom 4 bits)
    and #&0f
    sta zp_match_cnt+0
    sty zp_match_cnt+1

    ; unpack literal length from token (top 4 bits)
    txa
    lsr a
    lsr a
    lsr a
    lsr a

    ; fetch literal extended length
    jsr lz_fetch_count
    sta zp_literal_cnt+0
    stx zp_literal_cnt+1

    ; if no literals, begin the match sequence
    ; and fetch one match byte
    cmp #0
    bne has_literals

    jsr begin_matches
    jmp try_match

.has_literals
    ; ok now go back to literal parser so we can return a byte
    ; if no literals, logic will fall through to matches
    jmp try_literal
}

.lz_end
;-------------------------------------------


; vgm player
.vgm_start

; local vgm workspace
.vgm_streams ; decoder contexts - 8 bytes per stream, 8 streams (64 bytes)
    skip  8*8
    ;zp_stream_src   = VGM_ZP + 0    ; stream data ptr LO/HI
    ;zp_literal_cnt  = VGM_ZP + 2    ; literal count LO/HI
    ;zp_match_cnt    = VGM_ZP + 4    ; match count LO/HI
    ;zp_window_src   = VGM_ZP + 6    ; window read ptr - index
    ;zp_window_dst   = VGM_ZP + 7    ; window write ptr - index

.vgm_buffers  equb 0
.vgm_finished equb 0

; A contains data to be written to sound chip
; clobbers X
.sn_write
{
    sei
    ldx #255
    stx &fe43
    sta &fe41
    inx
    stx &fe40
    lda &fe40
    ora #8
    sta &fe40
    cli
    rts ; 21 bytes
}

IF FALSE
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
ENDIF

.sn_reset
{
	\\ Zero volume on all channels
	lda #&9f : jsr sn_write
	lda #&bf : jsr sn_write
	lda #&df : jsr sn_write
	lda #&ff : jsr sn_write
	rts
}







.vgm_stream_mount
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

    ; get address of block, store in vgm_streams[x*8]
    lda zp_block_data+0
    clc
    adc #4
    sta zp_block_data+0
    sta vgm_streams + 0, x
    lda zp_block_data+1
    adc #0
    sta zp_block_data+1
    sta vgm_streams + 1, x

    ; init the rest
    lda #0
    sta vgm_streams + 2, x  ; literal cnt 
    sta vgm_streams + 3, x  ; literal cnt 
    sta vgm_streams + 4, x  ; match cnt 
    sta vgm_streams + 5, x  ; match cnt 
    sta vgm_streams + 6, x  ; window src ptr 
    sta vgm_streams + 7, x  ; window dst ptr 


    ; move to next block
    lda zp_block_data+0
    clc
    adc zp_block_size+0
    sta zp_block_data+0
    lda zp_block_data+1
    adc zp_block_size+1
    sta zp_block_data+1

    ; for all 8 blocks
    txa
    clc
    adc #8
    tax
    cpx #8*8
    bne block_loop

    rts
}


; Select a register data stream where
;  X is stream id (0-7) * 8
;  clobbers A,Y
;  no return value
.vgm_load_register_context
{
    ldy #0
.loop
    lda vgm_streams, x
    sta lz_zp, y
    inx
    iny
    cpy #8
    bne loop
    rts
}

; Save the current register stream context
;  X is stream id (0-7) * 8
;  clobbers A,Y
;  no return value
.vgm_save_register_context
{
    ldy #0
.loop
    lda lz_zp, y
    sta vgm_streams, x
    inx
    iny
    cpy #8
    bne loop
    rts    
}

;----------------------------------------------------------------------
; fetch register data byte from register stream selected in A
;  A is register id (0-7)
;  clobbers X,Y
.vgm_get_register_data
{
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
    sta temp
    tax

    ; since we have 8 separately compressed register streams
    ; we have to load the required decoder context to ZP
    jsr vgm_load_register_context

    ; then fetch a decompressed byte
    jsr lz_decode_byte
    pha
    ldx temp
    ; then we save the decoder context from ZP back to main ram
    jsr vgm_save_register_context
    pla
    rts
.temp equb 0
}



.vgm_get_data
{
    ; SN76489 data register format is %1cctdddd where cc=channel, t=0=tone, t=1=volume, dddd=data

    ; Get Channel 3 tone first
    ; If it is 255 we have reached the EOF marker
    lda #3:jsr vgm_get_register_data
    cmp #&ff:bne no_eof
    jsr sn_reset
    sta vgm_finished
    rts
.no_eof
    ; skip tone3 updates if 15
    ; this prevents unwanted reset of the LSFR
    cmp #&0f:beq no_change3

    ora #&80 + (3<<5):jsr sn_write

.no_change3

    ; Channel 0 tone
    lda #0:jsr vgm_get_register_data:ora #&80 + (0<<5):jsr sn_write
    lda #0:jsr vgm_get_register_data:jsr sn_write

    ; Channel 1 tone
    lda #1:jsr vgm_get_register_data:ora #&80 + (1<<5):jsr sn_write
    lda #1:jsr vgm_get_register_data:jsr sn_write

    ; Channel 2 tone
    lda #2:jsr vgm_get_register_data:ora #&80 + (2<<5):jsr sn_write
    lda #2:jsr vgm_get_register_data:jsr sn_write


    ; Channel 0-3 volumes
    lda #4:jsr vgm_get_register_data:ora #&90 + (0<<5):jsr sn_write
    lda #5:jsr vgm_get_register_data:ora #&90 + (1<<5):jsr sn_write
    lda #6:jsr vgm_get_register_data:ora #&90 + (2<<5):jsr sn_write
    lda #7:jsr vgm_get_register_data:ora #&90 + (3<<5):jsr sn_write

    rts
}



;--------------------------------------------------
; user routines
;--------------------------------------------------



;-------------------------------------------
; vgm_init
;-------------------------------------------
; Initialise playback routine
;  A points to HI byte of 2Kb buffer address
;  X/Y point to VGM data stream
;-------------------------------------------
.vgm_init
{
    ; stash the 2kb buffer address
    sta vgm_buffers

    ; Prepare the data for streaming (passed in X/Y)
    jsr vgm_stream_mount

    ; clear vgm finished flag
    lda #0:sta vgm_finished

    ; reset soundchip
    jsr sn_reset
    rts
}

;-------------------------------------------
; vgm_update
;-------------------------------------------
;  call every 50Hz to play music
;  vgm_init must be called prior to this
;  returns non-zero when VGM is finished.
;-------------------------------------------
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



.vgm_end


.vgm_data
;INCBIN "data/nd-ui.bin.lz4"
;INCBIN "data/androids.bin.lz4"
;INCBIN "data/mongolia.bin.lz4"
;INCBIN "data/mongolia.bin.vgc"
INCBIN "data/darkside1.bin.vgc"
;INCBIN "data/CPCTL10A.bin.vgc"

PRINT ~vgm_data



PRINT "   lz code size is", (lz_end-lz_start), "bytes"
PRINT "  vgm code size is", (vgm_end-vgm_start), "bytes"
PRINT "vgm buffer size is", (vgm_buffer_end-vgm_buffer_start), "bytes"

PRINT "total vgm player size is", (vgm_buffer_end-vgm_buffer_start) + (lz_end-lz_start) + (vgm_end-vgm_start), "bytes"


.end

SAVE "Main", start, end, main

