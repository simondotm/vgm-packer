;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-packer
;******************************************************************



LZ4_FORMAT = FALSE
USE_HUFFMAN = TRUE


;-------------------------------
; workspace/zeropage vars
;-------------------------------

VGM_ZP = &70

; declare zero page registers used for each compressed stream (they are context switched)
lz_zp = VGM_ZP + 0
zp_stream_src   = lz_zp + 0    ; stream data ptr LO/HI          *** ONLY USED 1-2 TIMES PER FRAME ***, not worth ZP?
; none of the following have to be zp for indirect addressing reasons.
zp_literal_cnt  = lz_zp + 2    ; literal count LO/HI, 7 references
zp_match_cnt    = lz_zp + 4    ; match count LO/HI, 10 references
; huffman decoder stream context - CHECK:needs to be in ZP? prefix with zp if so
zp_huff_bitbuffer  = lz_zp + 6    ; huffman decoder bit buffer - 1 byte, referenced by inner loop
zp_huff_bitsleft   = lz_zp + 7    ; huffman decoder bit buffer size - 1 byte, referenced by inner loop

; these variables are not preserved across context switches, can be any Zero page
zp_temp = lz_zp + 8 ; 2 bytes ; used only by lz_decode_byte and lz_fetch_count, does not need to be zp 



VGM_STREAM_CONTEXT_SIZE = 10 ; number of bytes total workspace for a stream
NUM_VGM_STREAMS = 8

IF USE_HUFFMAN

; we re-use the LZ read ptr, since the huffman routine replaces the lz_fetch_byte
huff_readptr    = zp_stream_src 


HUFF_ZP = lz_zp + 10

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


; these variables are only used during a byte fetch - ie. not stream specific.
; ZP only for byte & cycle speed. Could be absolute memory.
huff_code       = HUFF_ZP + 0 ; 2
huff_firstcode  = HUFF_ZP + 2 ; 2
huff_startindex = HUFF_ZP + 4 ; 1
huff_codesize   = HUFF_ZP + 5 ; 1
huff_index      = HUFF_ZP + 6 ; 2
huff_numcodes   = HUFF_ZP + 8 ; 1
huff_temp       = HUFF_ZP + 9 ; 2

ENDIF ; USE_HUFFMAN


; when mounting a VGM file we use these four variables as temporaries
; they are not speed or memory critical.
; they may need to be zero page due to indirect addressing
zp_block_data = zp_stream_src ; re-uses zp_stream_src, must be zp ; zp_buffer+0 ; must be zp
zp_block_size = zp_temp+0 ; does not need to be zp

; OPTIMIZATIONS
; Cheaper to store JSR huffman_fetch directly once per init, than check every byte?
; [DONE] Unroll the context copy loop
; [DONE] Make the context non contiguous so no need to *16 each stream
; [DONE] minimize context vars from 16 to <16


; [DONE] load/save the work buffer address directly per stream (no need for copy or zp indirect)
; [DONE] also- inc buffer address directly too? this way copy back is free
;   and no need for zp_window_src or zp_window_dst
;   may even help us implement 16-bit offsets
; reducing number of vars in context saves memory anyway, so 16*8 = 128 bytes