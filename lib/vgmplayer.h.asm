;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-packer
;******************************************************************



LZ4_FORMAT = FALSE
USE_HUFFMAN = TRUE

USE_TABLE16 = TRUE ; only needed for huffman

OPTIMIZE_WINDOW = FALSE
OPTIMIZE_WORKSPACE = TRUE

;-------------------------------
; workspace/zeropage vars
;-------------------------------

VGM_ZP = &80


; declare zero page registers used for each compressed stream (they are context switched)
lz_zp = VGM_ZP + 0
zp_stream_src   = lz_zp + 0    ; stream data ptr LO/HI          *** ONLY USED 1-2 TIMES PER FRAME ***, not worth ZP?
; none of the following have to be zp for indirect addressing reasons.
zp_literal_cnt  = lz_zp + 2    ; literal count LO/HI, 7 references
zp_match_cnt    = lz_zp + 4    ; match count LO/HI, 10 references
zp_window_src   = lz_zp + 6    ; window read ptr - index, 3 references
zp_window_dst   = lz_zp + 7    ; window write ptr - index, 3 references

IF USE_TABLE16 
huff_bitbuffer  = lz_zp + 8    ; HUFF_ZP + 0   ; 1 byte, referenced by inner loop
huff_bitsleft   = lz_zp + 9    ; HUFF_ZP + 1   ; 1 byte, referenced by inner loop
lz_zp_size = 16  ; number of bytes total workspace for a stream
ELSE
lz_zp_size = 8  ; number of bytes total workspace for a stream
ENDIF

IF USE_HUFFMAN

HUFF_ZP = &70

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

; we use the LZ read ptr, since the huffman routine replaces the lz_fetch_byte
huff_readptr    = zp_stream_src ; HUFF_ZP + 2 ; 2

; these variables are only used during a byte fetch - ie. not stream specific.
; ZP only for byte & cycle speed. Could be absolute memory.
huff_code       = HUFF_ZP + 4 ; 2
huff_firstcode  = HUFF_ZP + 6 ; 2
huff_startindex = HUFF_ZP + 8 ; 1
huff_codesize   = HUFF_ZP + 9 ; 1
huff_index      = HUFF_ZP + 10 ; 2
huff_numcodes   = HUFF_ZP + 12 ; 1
huff_temp       = HUFF_ZP + 13 ; 2

ENDIF ; USE_HUFFMAN


; VGM player uses 11 zero page vars from this address


; these variables are not preserved across context switches, can be any Zero page
zp_buffer = &6a ; lz_zp + 10          ; 2 bytes, current decode window buffer, used by lz_fetch_buffer and lz_store_buffer, must be zp
zp_temp = &6c ; lz_zp + 12           ; 2 bytes ; used only by lz_decode_byte and lz_fetch_count, does not need to be zp 
zp_stash = &6e ;lz_zp + 14          ; 2 bytes ; used only by lz_decode_byte

; when mounting a VGM file we use these four variables as temporaries
; they are not speed or memory critical.
; they may need to be zero page due to indirect addressing
zp_block_data = zp_buffer+0 ; must be zp
zp_block_size = zp_temp+0 ; does not need to be zp
zp_symbol_table_size = zp_stash + 0 ; OPTIMIZATION - can be removed
zp_length_table_size = zp_stash + 1 ; OPTIMIZATION - only used once, can be absolute memory temp

; OPTIMIZATIONS
; Cheaper to store JSR huffman_fetch directly once per init, than check every byte?
; Unroll the context copy loop
; Make the context non contiguous so no need to *16 each stream
; minimize context vars from 16 to <16


; load/save the work buffer address directly per stream (no need for copy or zp indirect)
;  also- inc buffer address directly too? this way copy back is free
;   and no need for zp_window_src or zp_window_dst
;   may even help us implement 16-bit offsets
; reducing number of vars in context saves memory anyway, so 16*8 = 128 bytes