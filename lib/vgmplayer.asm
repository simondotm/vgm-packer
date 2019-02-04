;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-packer
;******************************************************************



;---------------------------------------------------------------
; VGM library code
;---------------------------------------------------------------

.decoder_start



;-------------------------------
; lz4 decoder
;-------------------------------



; fetch a byte from the current decode buffer at the current read ptr offset
; returns byte in A, clobbers Y
.lz_fetch_buffer
{
IF OPTIMIZE_WINDOW
    lda &ffff           ; *** SELF MODIFIED ***
    inc lz_fetch_buffer+1
ELSE
    ldy zp_window_src
    lda (zp_buffer),Y
    inc zp_window_src
ENDIF
    rts
}

; push byte into decode buffer
; clobbers Y, preserves A
.lz_store_buffer    ; called twice - 4 byte overhead, 6 byte function. Cheaper to inline.
{
IF OPTIMIZE_WINDOW
    sta &ffff   ; *** SELF MODIFIED ***
    inc lz_store_buffer+1
ELSE
    ldy zp_window_dst   ; [3 zp, 4 abs] (2 zp, 3 abs)
    sta (zp_buffer),Y   ; [6]           (2)
    inc zp_window_dst   ; [5 zp, 4 abs] (2)
ENDIF
    rts                 ; [6] (1)
}

; Calculate a multi-byte lz4 style length into zp_temp
; On entry A contains the initial counter value (LO)
; Returns 16-bit length in A/X (A=LO, X=HI)
; Clobbers Y, zp_temp+0, zp_temp+1
.lz_fetch_count
{
    ldx #0
    cmp #15             ; >=15 signals byte extend
    bne done
    sta zp_temp+0
    stx zp_temp+1
.fetch
    jsr lz_fetch_byte
    tay
    clc
    adc zp_temp+0
    sta zp_temp+0

    lda zp_temp+1   ; [3zp 4abs](2)
    adc #0          ; [2](2)
    sta zp_temp+1   ; [3zp 4abs](2)

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

    lda zp_literal_cnt+0        ; [3 zp][4 abs]
    bne is_literal              ; [2, +1, +2]
    lda zp_literal_cnt+1        ; [3 zp][4 abs]
    beq try_match               ; [2, +1, +2]

.is_literal

    ; fetch a literal & stash in decode buffer
    jsr lz_fetch_byte           ; [6] +6 RTS
    jsr lz_store_buffer         ; [6] +6 RTS
    sta zp_stash

    ; for all literals
    dec zp_literal_cnt+0        ; [5 zp][6 abs]
    bne end_literal             ; [2, +1, +2]
    lda zp_literal_cnt+1        ; [3 zp][4 abs]
    beq begin_matches           ; [2, +1, +2]
    dec zp_literal_cnt+1        ; [5 zp][6 abs]
    bne end_literal

.begin_matches

    ; literals run completed
    ; now fetch match offset & length

    ; get match offset LO
    jsr lz_fetch_byte     

    ; set buffer read ptr
    sta zp_temp
IF OPTIMIZE_WINDOW
    lda lz_store_buffer + 1 ; *** SELF MODIFYING CODE *** (zp_window_dst)
ELSE
    lda zp_window_dst
ENDIF
    sec
    sbc zp_temp
IF OPTIMIZE_WINDOW 
    sta lz_fetch_buffer + 1 ; *** SELF MODIFYING CODE *** (zp_window_src)
ELSE
    sta zp_window_src
ENDIF

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


IF FALSE ; 13 bytes, 18 cycles
    stx zp_match_cnt+1 ; [3 zp, 4 abs](2)
    clc                ; [2](1)
    adc #4             ; [2](2)
    sta zp_match_cnt+0 ; [3 zp, 4 abs](2)
    lda zp_match_cnt+1 ; [3 zp, 4 abs](2)
    adc #0             ; [2](2)
    sta zp_match_cnt+1 ; [3 zp, 4 abs](2)
ELSE ; 12 bytes, 14-16 cycles
    clc                  ; [2] (1)
    adc #4               ; [2] (2)
    sta zp_match_cnt+0   ; [3 zp, 4 abs] (2)
    bcc store_hi         ; [2, +1, +2]    (2)
    inx                  ; [2] (1)
    ;inc zp_match_cnt+1  ; [5 zp, 6 abs]  (2)
.store_hi
    stx zp_match_cnt+1   ; [3 zp, 4 abs](2)
ENDIF

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

    ; fetch literal extended length, passing in A
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





; fetch a byte from the currently selected compressed register data stream
; either huffman encoded or plain data
; returns byte in A, clobbers Y
.lz_fetch_byte
{
IF USE_HUFFMAN == TRUE
    ; if bit7 of vgm_flags is set, its a huffman stream
    bit vgm_flags        ; [3 zp, 4 abs] (2)
    bmi huff_fetch_byte  ; [2, +1, +2] (2)
    ; 
ENDIF
    ; otherwise plain LZ4 byte fetch
    ldy #0
    lda (zp_stream_src),y
    inc zp_stream_src+0
    bne ok
    inc zp_stream_src+1
.ok
    rts
}


IF USE_HUFFMAN

;-------------------------------
; huffman decoder
;-------------------------------

; TODO: Optimize with a peek buffer.
; 70-90% of codes are 7 bits or less. At 8 bits the % is higher.
; Peek buffer translates to two lookups and no loops.
; http://cbloomrants.blogspot.com/2010/08/08-11-10-huffman-arithmetic-equivalence.html
; 7 bits peek would require 2x 128 byte tables (256 bytes total extra to data stream).

; other optimization ideas:
; bitbuffer is filled at most twice per byte fetch.

; this routine must be located within branch reach of lz_fetch_byte()
.huff_fetch_byte
{
    lda #0
    sta huff_code + 0       ;code = 0                            # word
    sta huff_code + 1
    sta huff_codesize ;code_size = 0                       # byte

    sta huff_firstcode + 0 ; firstCodeWithNumBits = 0            # word
    sta huff_firstcode + 1

    sta huff_startindex ; startIndexForCurrentNumBits = 0     # byte

    ;sourceindex = 0
    ;unpacked = 0
    ;while unpacked < unpacked_size: # currentbyte < len(data):
    ; skip over nextbit - this layout saves a jmp per bitlength
    jmp decode_loop
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
    ;REMOVED: jmp decode_loop
    ; falls into decode_loop
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
    bne got_bits
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
    rol huff_code + 0       ; C -> bit0, bit7 -> C
    rol huff_code + 1       ; C -> bit8, bit15 -> C
    inc huff_codesize

    ;# how many canonical codes have this many bits
    ;assert code_size <= Huffman.MAX_CODE_BIT_LENGTH
    ;numCodes = length_table[code_size] # self.table_bitlengths[code_size] # byte
    ldy huff_codesize
}
.LOAD_LENGTH_TABLE
{
    lda &FFFF, Y     ; ** MODIFIED ** See vgm_stream_mount
    sta huff_numcodes

    ;# if input code so far is within the range of the first code with the current number of bits, it's a match
    ;indexForCurrentNumBits = code - firstCodeWithNumBits

    sec
    lda huff_code + 0
    sbc huff_firstcode + 0
    sta huff_index + 0
    lda huff_code + 1
    sbc huff_firstcode + 1
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
    lda &FFFF, Y     ; ** MODIFIED ** See vgm_stream_mount
    rts
}


ENDIF ; USE_HUFFMAN


.decoder_end


;-------------------------------------------
; vgm player
;-------------------------------------------


.vgm_start

;-------------------------------------------
; local vgm workspace
;-------------------------------------------

;ALIGN 16 ; doesnt have to be aligned, just for debugging ease
.vgm_streams ; decoder contexts - 8 bytes per stream, 8 streams (64 bytes)
    skip  NUM_VGM_STREAMS*lz_zp_size
    ;zp_stream_src   = VGM_ZP + 0    ; stream data ptr LO/HI
    ;zp_literal_cnt  = VGM_ZP + 2    ; literal count LO/HI
    ;zp_match_cnt    = VGM_ZP + 4    ; match count LO/HI
    ;zp_window_src   = VGM_ZP + 6    ; window read ptr - index
    ;zp_window_dst   = VGM_ZP + 7    ; window write ptr - index



.vgm_buffers  equb 0    ; the HI byte of the address where the buffers are stored
.vgm_finished equb 0    ; a flag to indicate player has reached the end of the vgm stream
.vgm_flags  equb 0      ; flags for current vgm file. bit7 set stream is huffman coded. bit 6 set if stream is 16-bit LZ4 offsets
.vgm_temp equb 0
.vgm_temp2 equb 0 ; TODO:shared temp?


; 8 counters for VGM register update counters (RLE)
.vgm_register_counts
    SKIP 8


; Table of SN76489 flags for the 8 LATCH/DATA registers
; %1cctdddd 
.vgm_register_headers
    EQUB &80 + (0<<5)   ; Tone 0
    EQUB &80 + (1<<5)   ; Tone 1
    EQUB &80 + (2<<5)   ; Tone 2
    EQUB &80 + (3<<5)   ; Tone 3
    EQUB &90 + (0<<5)   ; Volume 0
    EQUB &90 + (1<<5)   ; Volume 1
    EQUB &90 + (2<<5)   ; Volume 2
    EQUB &90 + (3<<5)   ; Volume 3





;-------------------------------------------
; Sound chip routines
;-------------------------------------------



; Write data to SN76489 sound chip
; A contains data to be written to sound chip
; clobbers X, A is non-zero on exit
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

; Reset SN76489 sound chip to a default (silent) state
.sn_reset
{
	\\ Zero volume on all channels
	lda #&9f : jsr sn_write
	lda #&bf : jsr sn_write
	lda #&df : jsr sn_write
	lda #&ff : jsr sn_write
	rts
}


;-------------------------------------------
; VGM routines
;-------------------------------------------


; VGC file parsing - Skip to the next block. 
; on entry zp_block_data points to current block (header)
; on exit zp_block_data points to next block
; Clobbers Y
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

; VGC file parsing - Initialise the system for the provided in-memory VGC data stream.
; On entry X/Y point to Lo/Hi address of the vgc data
.vgm_stream_mount
{
    ; parse data stream
    ; VGC broadly uses LZ4 frame & block formats for convenience
    ; however there are assumptions for format:
    ;  Magic number[4], Flags[1], MaxBlockSize[1], Header checksum[1]
    ;  Contains 8 blocks
    ; Obviously since this is an 8-bit CPU no files or blocks can be > 64Kb in size

    ; VGC streams have a different magic number to LZ4
    ; [56 47 43 XX]
    ; where XX:
    ; bit 6 - LZ 8 bit (0) or 16 bit (1) [unsupported atm]
    ; bit 7 - Huffman (1) or no huffman (0)

    stx zp_block_data+0
    sty zp_block_data+1

    ; get the stream flags (huffman/8 or 16 bit offsets)
    ldy #3
    lda (zp_block_data), y
    sta vgm_flags

    ; Skip frame header, and move to first block
    lda zp_block_data+0
    clc
    adc #7
    sta zp_block_data+0
    lda zp_block_data+1
    adc #0
    sta zp_block_data+1


IF USE_HUFFMAN
    ; first block contains the bitlength and symbol tables
    bit vgm_flags
    bpl skip_hufftable

    ; stash table sizes for later
    ldy #8
    lda (zp_block_data),Y   ; symbol table size
    sta zp_symbol_table_size    
    iny
    lda (zp_block_data),Y   ; bitlength table size
    sta zp_length_table_size    
    inc zp_length_table_size    ; compensate for the first byte (range is 0-nbits inclusive), will never wrap a byte

    ; store the address of the bitlengths table directly in the huff_fetch_byte routine
    lda zp_block_data + 0
    clc
    adc #4+4+1        ; skip lz blocksize, huff block size and symbol count byte
    sta LOAD_LENGTH_TABLE + 1   ; ** SELF MODIFICATION ***
    lda zp_block_data + 1
    adc #0
    sta LOAD_LENGTH_TABLE + 2   ; ** SELF MODIFICATION ***

    ; store the address of the symbols table directly in the huff_fetch_byte routine
    lda LOAD_LENGTH_TABLE + 1
    clc
    adc zp_length_table_size
    sta LOAD_SYMBOL_TABLE + 1   ; ** SELF MODIFICATION ***
    lda LOAD_LENGTH_TABLE + 2
    adc #0
    sta LOAD_SYMBOL_TABLE + 2   ; ** SELF MODIFICATION ***

    ; skip to next block
    jsr vgm_next_block

.skip_hufftable
ENDIF ; USE_HUFFMAN


    ; read the block headers (size)
    ldx #0
.block_loop
 
    ; get address of block, store in vgm_streams[x*8]
    lda zp_block_data+0
    clc
    adc #4  ; skip block header
    ;sta zp_block_data+0
    sta vgm_streams + NUM_VGM_STREAMS*0, x
    lda zp_block_data+1
    adc #0
    ;sta zp_block_data+1
    sta vgm_streams + NUM_VGM_STREAMS*1, x

    ; init the rest
    lda #0
    sta vgm_streams + NUM_VGM_STREAMS*2, x  ; literal cnt 
    sta vgm_streams + NUM_VGM_STREAMS*3, x  ; literal cnt 
    sta vgm_streams + NUM_VGM_STREAMS*4, x  ; match cnt 
    sta vgm_streams + NUM_VGM_STREAMS*5, x  ; match cnt 
    sta vgm_streams + NUM_VGM_STREAMS*6, x  ; window src ptr 
    sta vgm_streams + NUM_VGM_STREAMS*7, x  ; window dst ptr 
IF USE_TABLE16 ;USE_HUFFMAN
    sta vgm_streams + NUM_VGM_STREAMS*8, x  ; huff bitbuffer
    sta vgm_streams + NUM_VGM_STREAMS*9, x  ; huff bitsleft
ENDIF

    jsr vgm_next_block

    ; for all 8 blocks
    txa
    clc
    adc #1 ;lz_zp_size
    tax
    cpx #8 ;*lz_zp_size
    bne block_loop

    ; clear vgm finished flag
    lda #0:sta vgm_finished

    ; setup RLE tables
    ldx #7
    lda #1
.cloop
    sta vgm_register_counts, X
    dex
    bpl cloop

    rts
}


; Select a register data stream where
;  X is stream id (0-7) ;* lz_zp_size
;  clobbers A,Y
;  no return value
.vgm_load_register_context
{
    lda vgm_streams + NUM_VGM_STREAMS*0, x
    sta zp_stream_src + 0
    lda vgm_streams + NUM_VGM_STREAMS*1, x
    sta zp_stream_src + 1

    lda vgm_streams + NUM_VGM_STREAMS*2, x
    sta zp_literal_cnt + 0
    lda vgm_streams + NUM_VGM_STREAMS*3, x
    sta zp_literal_cnt + 1

    lda vgm_streams + NUM_VGM_STREAMS*4, x
    sta zp_match_cnt + 0
    lda vgm_streams + NUM_VGM_STREAMS*5, x
    sta zp_match_cnt + 1

    lda vgm_streams + NUM_VGM_STREAMS*6, x
    sta zp_window_src

    lda vgm_streams + NUM_VGM_STREAMS*7, x
    sta zp_window_dst

    lda vgm_streams + NUM_VGM_STREAMS*8, x
    sta huff_bitbuffer
    lda vgm_streams + NUM_VGM_STREAMS*9, x
    sta huff_bitsleft
    rts

}

; Save the current register stream context
;  X is stream id (0-7) * lz_zp_size
;  clobbers A,Y
;  no return value
.vgm_save_register_context
{
    lda zp_stream_src + 0
    sta vgm_streams + NUM_VGM_STREAMS*0, x
    lda zp_stream_src + 1
    sta vgm_streams + NUM_VGM_STREAMS*1, x

    lda zp_literal_cnt + 0
    sta vgm_streams + NUM_VGM_STREAMS*2, x
    lda zp_literal_cnt + 1
    sta vgm_streams + NUM_VGM_STREAMS*3, x

    lda zp_match_cnt + 0
    sta vgm_streams + NUM_VGM_STREAMS*4, x
    lda zp_match_cnt + 1
    sta vgm_streams + NUM_VGM_STREAMS*5, x

    lda zp_window_src
    sta vgm_streams + NUM_VGM_STREAMS*6, x

    lda zp_window_dst
    sta vgm_streams + NUM_VGM_STREAMS*7, x

    lda huff_bitbuffer
    sta vgm_streams + NUM_VGM_STREAMS*8, x
    lda huff_bitsleft
    sta vgm_streams + NUM_VGM_STREAMS*9, x
    rts    
}

;----------------------------------------------------------------------
; fetch register data byte from register stream selected in A
; This byte will be LZ4 encoded
;  A is register id (0-7)
;  clobbers X,Y
.vgm_get_register_data
{
    ; set the LZ4 decoder stream workspace buffer (initialised by vgm_stream_mount)
    tax
    clc
    adc vgm_buffers ; hi byte of where the 2kb vgm stream buffer is located
    sta zp_buffer+1
    lda #0
    sta zp_buffer+0
    txa
    
    ; calculate the stream buffer context
IF FALSE
    asl a
    asl a
    asl a
IF USE_TABLE16 ;USE_HUFFMAN
    asl a   ; *16 = lz_zp_size
ENDIF
ENDIF
    sta vgm_temp2
    tax

    ; since we have 8 separately compressed register streams
    ; we have to load the required decoder context to ZP
    jsr vgm_load_register_context   ; TODO:inline

    ; then fetch a decompressed byte
    jsr lz_decode_byte
    pha
    ldx vgm_temp2
    ; then we save the decoder context from ZP back to main ram
    jsr vgm_save_register_context   ; TODO:inline
    pla
    rts
}

; Fetch 1 register data byte from the encoded stream and send to sound chip (volumes & tone3)
; A is register to update
; on exit:
;    C is set if an update happened and Y contains last register value
;    C is clear if no updated happened and Y is preserved
;    X contains register provided in A on entry (0-7)

.vgm_update_register1
{
    tax
    clc
    dec vgm_register_counts,x ; no effect on C
    bne skip_register_update

    ; decode a byte & send to psg
    stx vgm_temp
    jsr vgm_get_register_data
    tay
    and #&0f
    ldx vgm_temp
    ora vgm_register_headers,x
    ; check if it's a tone3 skip command (&ef) before we play it
    cmp #&ef
    beq skip_tone3
    jsr sn_write ; clobbers X
.skip_tone3
    ; get run length (top 4-bits + 1)
    tya
    lsr a
    lsr a
    lsr a
    lsr a
    clc
    adc #1
    ldx vgm_temp
    sta vgm_register_counts,x
    sec
.skip_register_update
    rts
}

; Fetch 2 register bytes (LATCH+DATA) from the encoded stream and send to sound chip (tone0, tone1, tone2)
; Same parameters as vgm_update_register1
.vgm_update_register2
{
    jsr vgm_update_register1
    bcc skip_register_update

    ; decode 2nd byte and send to psg as (DATA)
    txa
    jsr vgm_get_register_data
    jsr sn_write ; clobbers X
.skip_register_update
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
    bne exit

    ; SN76489 data register format is %1cctdddd where cc=channel, t=0=tone, t=1=volume, dddd=data
    ; The data is run length encoded.
    ; Get Channel 3 tone first because that contains the EOF marker

    ; Update Tone3
    lda#3:jsr vgm_update_register1  ; on exit C set if data changed, Y is last value
    bcc more_updates

    cpy #&08     ; EOF marker? (0x08 is an invalid tone 3 value)
    beq finished

.more_updates
    lda#7:jsr vgm_update_register1  ; Volume3
    lda#1:jsr vgm_update_register2  ; Tone1
    lda#2:jsr vgm_update_register2  ; Tone2
    lda#4:jsr vgm_update_register1  ; Volume0
    lda#5:jsr vgm_update_register1  ; Volume1
    lda#6:jsr vgm_update_register1  ; Volume2
    ; do tone0 last so we can use X output as the Zero return flag
    lda#0:jsr vgm_update_register2  ; Tone0, returns 0 in X
    txa ; return Z=0, still playing
.exit
    rts

.finished
    ; end of tune reached. set flag & stop PSG
    sty vgm_finished    ; any NZ value is fine, in this case 0x08
    jmp sn_reset ; also returns non-zero in A
}



.vgm_end


PRINT "    decoder code size is", (decoder_end-decoder_start), "bytes"
PRINT " vgm player code size is", (vgm_end-vgm_start), "bytes"
PRINT "total vgm player size is", (decoder_end-decoder_start) + (vgm_end-vgm_start), "bytes"
PRINT "      vgm buffer size is", (vgm_buffer_end-vgm_buffer_start), "bytes"


