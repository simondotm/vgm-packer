\ ******************************************************************
\ *	Headers
\ ******************************************************************

; VGM player uses 11 zero page vars from this address
VGM_ZP = &80
LZ4_FORMAT = FALSE
USE_HUFFMAN = FALSE

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

.decoder_start


;-------------------------------
; lz4 decoder
;-------------------------------

; declare zero page registers used for each compressed stream (they are context switched)
lz_zp = VGM_ZP + 0
zp_stream_src   = lz_zp + 0    ; stream data ptr LO/HI          *** ONLY USED ONCE PER FRAME ***, not worth ZP?
zp_literal_cnt  = lz_zp + 2    ; literal count LO/HI
zp_match_cnt    = lz_zp + 4    ; match count LO/HI
zp_window_src   = lz_zp + 6    ; window read ptr - index
zp_window_dst   = lz_zp + 7    ; window write ptr - index

IF USE_HUFFMAN
huff_bitbuffer  = lz_zp + 8    ; HUFF_ZP + 0   ; 1 byte
huff_bitsleft   = lz_zp + 9    ; HUFF_ZP + 1   ; 1 byte
lz_zp_size = 16  ; number of bytes total workspace for a stream
ELSE
lz_zp_size = 8  ; number of bytes total workspace for a stream
ENDIF

; these variables are not preserved across context switches, can be any Zero page
zp_buffer = lz_zp + 10          ; 2 bytes, current decode window buffer
zp_temp = lz_zp + 12           ; 2 bytes
zp_stash = lz_zp + 14          ; 1 byte


; If Huffman is enabled, lz_fetch_byte is declared below.
IF USE_HUFFMAN == FALSE
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
ENDIF

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




IF USE_HUFFMAN

;-------------------------------
; huffman decoder
;-------------------------------



; 6502 workspace - assuming max 16-bit codes
; init
; (2) table_bitlengths - only referenced once, can perhaps be self modified
; (2) table_symbols - only referenced once, can perhaps be self modified, implied +16 from table_bitlengths
; per stream
; (2) stream read ptr (can replace the one used by lz4, so no extra)
; (1) bitbuffer
; (1) bitsleft
; per symbol fetch
; (2) code
; (2) firstCodeWithNumBits
; (1) startIndexForCurrentNumBits
; (1) code_size
; (1) numCodes
; (1) indexForCurrentNumBits
; Note that table does not necessarily require MAX_SYMBOLS bytes now, will contain 16 entries plus N symbols. If few symbols occur.
; Could be an argument for separate tables per stream if compression ratio beats table overhead.
; we cant interleave the lz4 data because variable bytes needed per register stream per frame
; therefore we have to maintain 8 huffman contexts also.

; we use the LZ read ptr
huff_readptr    = zp_stream_src ; HUFF_ZP + 2 ; 2


HUFF_ZP = &70


huff_code       = HUFF_ZP + 4 ; 2
huff_firstcode  = HUFF_ZP + 6 ; 2
huff_startindex = HUFF_ZP + 8 ; 1
huff_codesize   = HUFF_ZP + 9 ; 1
huff_index      = HUFF_ZP + 10 ; 2
huff_numcodes   = HUFF_ZP + 12 ; 1
huff_temp       = HUFF_ZP + 13 ; 2

.length_table
.symbol_table


;.huff_init
;{
;    lda # 0
;    sta huff_bitbuffer  ;bitbuffer = 0
;    sta huff_bitsleft   ;numbitsbuffered = 0
;
;    lda #0
;    sta huff_readptr + 0
;    sta huff_readptr + 1
;    rts
;}

; fetch a byte from the currently selected huffman compressed register data stream
; returns byte in A, clobbers Y - same as lz_fetch_byte
.lz_fetch_byte
.huff_fetch_byte
{
    sta huff_code + 0       ;code = 0                            # word
    sta huff_code + 1
    sta huff_codesize ;code_size = 0                       # byte

    sta huff_firstcode + 0 ; firstCodeWithNumBits = 0            # word
    sta huff_firstcode + 1

    sta huff_startindex ; startIndexForCurrentNumBits = 0     # byte

    ;sourceindex = 0
    ;unpacked = 0
    ;while unpacked < unpacked_size: # currentbyte < len(data):

}
.decode_loop
{
    ;# keep the bitbuffer going
    ;if numbitsbuffered == 0:
    ;    # we're out of data, so any wip codes are invalid due to byte padding.
    ;    #if currentbyte >= len(data):
    ;    #    break
    ;
    ;   bitbuffer = data[currentbyte]
    ;   currentbyte += 1
    ;   numbitsbuffered += 8


    lda huff_bitsleft
    bne got_bits
    ; fetch more bits
    ldy #0
    lda (huff_readptr), y
    sta huff_bitbuffer
    lda #8
    sta huff_bitsleft
    inc huff_readptr + 0
    beq got_bits
    inc huff_readptr + 1
.got_bits

    ;# get a bit
    ;bit = (bitbuffer & 128) >> 7
    ;bitbuffer <<= 1
    ;numbitsbuffered -= 1

    ;# build code
    ;code = (code << 1) | bit
    ;code_size += 1

    ; build code
    dec huff_bitsleft
    asl huff_bitbuffer           ; bit7 -> C
    asl huff_code + 0       ; C -> bit0, bit7 -> C
    rol huff_code + 1       ; C -> bit8
    inc huff_codesize

    ;# how many canonical codes have this many bits
    ;assert code_size <= Huffman.MAX_CODE_BIT_LENGTH
    ;numCodes = length_table[code_size] # self.table_bitlengths[code_size] # byte
    ldy huff_codesize
}
.LOAD_LENGTH_TABLE
{
    lda length_table, Y     ; ** MODIFIED **
    sta huff_numcodes

    ;# if input code so far is within the range of the first code with the current number of bits, it's a match
    ;indexForCurrentNumBits = code - firstCodeWithNumBits

    sec
    lda huff_firstcode + 0
    sbc huff_code + 0
    sta huff_index + 0
    lda huff_firstcode + 1
    sbc huff_code + 1
    sta huff_index + 1

    ;if indexForCurrentNumBits < numCodes:
    
    ; if hi byte is non zero, is definitely > numcodes
    ; or numcodes >= index
    bne nextbit 
    lda huff_index
    cmp huff_numcodes
    bcs nextbit
    
    ; code = startIndexForCurrentNumBits + indexForCurrentNumBits
    lda huff_startindex
    clc
    adc huff_index
    tay
}
.LOAD_SYMBOL_TABLE
{
    ; symbol = symbol_table[code]
    lda symbol_table, Y     ; ** MODIFIED **
    rts
}
.nextbit
{
    ; otherwise, move to the next bit length
    ; firstCodeWithNumBits += numCodes
    lda huff_firstcode + 0
    clc
    adc huff_numcodes
    sta huff_firstcode + 0
    lda huff_firstcode + 1
    adc #0
    sta huff_firstcode + 1

    ; firstCodeWithNumBits <<= 1
    asl huff_firstcode + 0
    rol huff_firstcode + 1
    
    ; startIndexForCurrentNumBits += numCodes
    lda huff_startindex
    clc
    adc huff_numcodes
    sta huff_startindex
    
    ; keep going until we find a symbol
    jmp decode_loop
}

ENDIF ; USE_HUFFMAN


.decoder_end
;-------------------------------------------


; vgm player
.vgm_start

; local vgm workspace
.vgm_streams ; decoder contexts - 8 bytes per stream, 8 streams (64 bytes)
    skip  8*lz_zp_size
    ;zp_stream_src   = VGM_ZP + 0    ; stream data ptr LO/HI
    ;zp_literal_cnt  = VGM_ZP + 2    ; literal count LO/HI
    ;zp_match_cnt    = VGM_ZP + 4    ; match count LO/HI
    ;zp_window_src   = VGM_ZP + 6    ; window read ptr - index
    ;zp_window_dst   = VGM_ZP + 7    ; window write ptr - index

.vgm_buffers  equb 0    ; the HI byte of the address where the buffers are stored
.vgm_finished equb 0    ; a flag to indicate player has reached the end of the vgm stream

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

; when mounting a VGM file we use these two variables as temporaries
zp_block_data = VGM_ZP+0
zp_block_size = VGM_ZP+2

; on entry zp_block_data points to current block (header)
; on exit zp_block_data points to next block
.vgm_next_block
{
    ; read 16-bit block size to zp_block_size
    ; +4 to compensate for block header
    ldy #0
    lda (zp_block_data),Y
    clc
    adc #4
    sta zp_block_size+0
    iny
    lda (zp_block_data),Y
    adc #0
    sta zp_block_size+1

    ; move to next block
    lda zp_block_data+0
    clc
    adc zp_block_size+0
    sta zp_block_data+0
    lda zp_block_data+1
    adc zp_block_size+1
    sta zp_block_data+1
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

    ; Skip frame header, and move to first block
    txa
    clc
    adc #7
    sta zp_block_data+0
    tya
    adc #0
    sta zp_block_data+1


IF USE_HUFFMAN
    ; first block contains the bitlength and symbol tables

zp_symbol_table_size = zp_stream_src + 0
zp_length_table_size = zp_stream_src + 1

    ; stash table sizes for later
    ldy #8
    lda (zp_block_data),Y   ; symbol table size
    sta zp_symbol_table_size    
    iny
    lda (zp_block_data),Y   ; bitlength table size
    sta zp_length_table_size    ;; table size + 6 will never be > 256

    ; store the address of the bitlengths table directly in the huff_fetch_byte routine
    lda zp_block_data + 0
    clc
    adc #4+4+1        ; skip lz blocksize, huff block size and symbol count byte
    sta LOAD_LENGTH_TABLE + 0
    lda zp_block_data + 0
    adc #0
    sta LOAD_LENGTH_TABLE + 1

    ; store the address of the symbols table directly in the huff_fetch_byte routine
    lda LOAD_LENGTH_TABLE + 0
    clc
    adc zp_length_table_size
    sta LOAD_SYMBOL_TABLE + 0
    lda LOAD_LENGTH_TABLE + 1
    adc #0
    sta LOAD_SYMBOL_TABLE + 1

    ; skip to next block
    jsr vgm_next_block


ENDIF ; USE_HUFFMAN


    ; read the block headers (size)
    ldx #0
.block_loop
 
    ; get address of block, store in vgm_streams[x*8]
    lda zp_block_data+0
    clc
    adc #4  ; skip block header
    ;sta zp_block_data+0
    sta vgm_streams + 0, x
    lda zp_block_data+1
    adc #0
    ;sta zp_block_data+1
    sta vgm_streams + 1, x

    ; init the rest
    lda #0
    sta vgm_streams + 2, x  ; literal cnt 
    sta vgm_streams + 3, x  ; literal cnt 
    sta vgm_streams + 4, x  ; match cnt 
    sta vgm_streams + 5, x  ; match cnt 
    sta vgm_streams + 6, x  ; window src ptr 
    sta vgm_streams + 7, x  ; window dst ptr 
IF USE_HUFFMAN
    sta vgm_streams + 8, x  ; huff bitbuffer
    sta vgm_streams + 9, x  ; huff bitsleft
ENDIF

    jsr vgm_next_block

    ; for all 8 blocks
    txa
    clc
    adc #lz_zp_size
    tax
    cpx #8*lz_zp_size
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
    cpy #lz_zp_size
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
    cpy #lz_zp_size
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
IF USE_HUFFMAN
    asl a
ENDIF
    sta temp
    tax

    ; since we have 8 separately compressed register streams
    ; we have to load the required decoder context to ZP
    jsr vgm_load_register_context   ; TODO:inline

    ; then fetch a decompressed byte
    jsr lz_decode_byte
    pha
    ldx temp
    ; then we save the decoder context from ZP back to main ram
    jsr vgm_save_register_context   ; TODO:inline
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
IF USE_HUFFMAN
INCBIN "data/outruneu.bin.vgc"
;INCBIN "data/darkside1.bin.vgc"
;INCBIN "data/androids.bin.lz4"
ELSE
;INCBIN "data/CPCTL10A.bin.vgc"
INCBIN "data/mongolia.bin.vgc"
ENDIF
;INCBIN "data/nd-ui.bin.lz4"
;INCBIN "data/mongolia.bin.lz4"


PRINT ~vgm_data



PRINT "    decoder code size is", (decoder_end-decoder_start), "bytes"
PRINT " vgm player code size is", (vgm_end-vgm_start), "bytes"
PRINT "      vgm buffer size is", (vgm_buffer_end-vgm_buffer_start), "bytes"

PRINT "total vgm player size is", (vgm_buffer_end-vgm_buffer_start) + (decoder_end-decoder_start) + (vgm_end-vgm_start), "bytes"


.end

SAVE "Main", start, end, main

