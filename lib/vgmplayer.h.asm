;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-packer
;******************************************************************



LZ4_FORMAT = FALSE
USE_HUFFMAN = TRUE

USE_TABLE16 = TRUE ; only needed for huffman

;-------------------------------
; workspace/zeropage vars
;-------------------------------

VGM_ZP = &80


; declare zero page registers used for each compressed stream (they are context switched)
lz_zp = VGM_ZP + 0
zp_stream_src   = lz_zp + 0    ; stream data ptr LO/HI          *** ONLY USED ONCE PER FRAME ***, not worth ZP?
zp_literal_cnt  = lz_zp + 2    ; literal count LO/HI
zp_match_cnt    = lz_zp + 4    ; match count LO/HI
zp_window_src   = lz_zp + 6    ; window read ptr - index
zp_window_dst   = lz_zp + 7    ; window write ptr - index

IF USE_TABLE16 
huff_bitbuffer  = lz_zp + 8    ; HUFF_ZP + 0   ; 1 byte
huff_bitsleft   = lz_zp + 9    ; HUFF_ZP + 1   ; 1 byte
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

; these variables are only used during a byte fetch
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
zp_buffer = &6a ; lz_zp + 10          ; 2 bytes, current decode window buffer
zp_temp = &6c ; lz_zp + 12           ; 2 bytes
zp_stash = &6e ;lz_zp + 14          ; 1 byte


; when mounting a VGM file we use these two variables as temporaries
zp_block_data = zp_buffer+0
zp_block_size = zp_temp+0

zp_symbol_table_size = zp_stash + 0
zp_length_table_size = zp_stash + 1