# vgm-packer
Python based compression tool for packing SN76489 based VGM music data into an optimal format for use on 8-bit CPU's.

Full writeup coming soon, but basic tool chain is:

`vgmpacker.py <vgmfile>` to create a `.vgc` file which can be loaded and processed by the code in `vgmdemo.asm` and `lib\vgmplayer.h.asm`. The tool only works with VGM files for the SN76489 PSG.

### Command Line Usage

```
VgmPacker.py : VGM music compressor for 8-bit CPUs
Written in 2019 by Simon Morris, https://github.com/simondotm/vgm-packer

usage: vgmpacker.py [-h] [-o <output>] [-b <n>] [-n] [-v] input

positional arguments:
  input                 VGM source file (must be single SN76489 PSG format)
                        [input]

optional arguments:
  -h, --help            show this help message and exit
  -o <output>, --output <output>
                        write VGC file <output> (default is '[input].vgc')
  -b <n>, --buffer <n>  Set decoder buffer size to <n> bytes, default: 255
  -n, --nohuffman       Disable huffman compression
  -v, --verbose         Enable verbose mode

Notes:
 Buffer size <256 bytes emits 8-bit LZ4 offsets, medium compression, faster decoding, 2Kb workspace
 Buffer size >255 bytes emits 16-bit LZ4 offsets, higher compression, slower decoding, Size*8 workspace
 Disabling huffman will result in slightly worse compression, but faster and less variable decoding speed

```
### 6502 Player Usage
Import `lib\vgmplayer.h.asm` and `lib\vgmplayer.asm` into your BeebAsm project. Note that you should `INCLUDE "lib/vgmplayer.h.asm"` at the point where you declare your zeropage vars - see `vgmdemo.asm` for an example.

#### `vgmplayer.asm`

You can assemble this module a couple of ways and there are defines in `vgmplayer.h.asm` as follows:

`ENABLE_HUFFMAN`
* If set to TRUE, the decoder will be able to load `.vgc` files compiled by `vgmpacker.py` with Huffman enabled (this is the default setting)
* If set to FALSE, the decoder will not include any code to decode huffman `.vgc` files so make sure you compress your `.vgm` files using the `-n` option with `vgmpacker.py`.

Huffman decoding is a fair bit slower, but does save 5-10% in compression ratio, so I recommend only using it for situations where you need maximum compression and CPU runtime & code size is not an constraint.

There are 2 main user routines:

`vgm_init()` - to initialize the player with a VGC data stream

`vgm_update()` - to refresh the player every 50Hz

See `vgmplayer.asm` for more information.

#### Memory Requirements
The Huffman enabled decoder requires 19 zero page vars.

The non-Huffman enabled decoder requires 8 zero page vars.

Both decoders require a 2Kb page aligned workspace ram buffer which can be passed to the decoder via `vgm_init()`.

