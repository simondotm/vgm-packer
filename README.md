# vgm-packer
Python based compression tool for packing SN76489 based VGM music data into an optimal format for use on 8-bit CPU's.

Written by [Simon Morris](https://github.com/simondotm).

Release under MIT License, Copyright (c) 2019 Simon Morris. All rights reserved.

## Introduction

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
  -n, --huffman         Enable huffman compression
  -v, --verbose         Enable verbose mode

Notes:
 Buffer size <256 bytes emits 8-bit LZ4 offsets, medium compression, faster decoding, 2Kb workspace
 Buffer size >255 bytes emits 16-bit LZ4 offsets, higher compression, slower decoding, Size*8 workspace
 Enabling huffman will result in slightly better compression, but slower and more variable decoding speed

```

Notes:
* The script is only compatible with Python 2.x at the moment. 
* The script includes other Python modules from the `modules` folder. For this reason it is not particularly portable as a standalone script so I recommend cloning the repo locally and creating your own subfolder for any vgm's you want to convert. 
* If you need to deploy it as part of a build process in a separate project you can copy `vgmpacker.py` but just make sure you also copy the `modules` folder too. _(I'm looking into ways to make it more self contained as a single script/pyc file)_
* The `modules` folder contains copies of my Python compression scripts from https://github.com/simondotm/lz4enc-python which is where I'm maintaining the release versions. 
* The 6502 decoder currently only supports buffer sizes of 255 bytes, so while you can use the `--buffer` option for experimentation it won't yield `.vgc` files that can be used on the 6502 at the moment. If you attempt this, know that: 1) buffer size > 255 will crash the decoder, and 2) buffer size < 255 will work ok, but you'll just get worse compression for no benefit on the decoder side. _I might look into supporting buffer sizes of 128 or 64 later since it may possible allow reduction of the 2kb workspace to 1kb or 0.5KB._


### 6502 Player Usage
Import `lib\vgmplayer.h.asm` and `lib\vgmplayer.asm` into your BeebAsm project. Note that you should `INCLUDE "lib/vgmplayer.h.asm"` at the point where you declare your zeropage vars - see `vgmdemo.asm` for an example.

#### `vgmplayer.asm`

You can assemble this module a couple of ways and there are defines in `vgmplayer.h.asm` as follows:

`ENABLE_HUFFMAN`
* If set to TRUE, the decoder will be able to load standard `.vgc` files as well as ones that have been huffman compressed by `vgmpacker.py`
* If set to FALSE, the decoder will not include any code to decode huffman `.vgc` files so make sure you do not compress your `.vgm` files using the `-n` option with `vgmpacker.py`.

Huffman decoding is a fair bit slower, but does save 5-10% in compression ratio, so I recommend only using it for situations where you need maximum compression and CPU runtime & code size is not an constraint. 

There are 2 main user routines:

`vgm_init()` - to initialize the player with a VGC data stream

`vgm_update()` - to refresh the player every 50Hz

See `vgmplayer.asm` for more information.

#### `vgmdemo.asm`
This is a BeebAsm project with some example 6502 code for the `vgmplayer.asm` library. Feel free to create a `.vgc` file from your favourite `.vgm`, and then just `INCBIN` it accordingly.

#### Memory Requirements
The standard decoder requires 8 zero page vars and ~740 bytes of code ram.

The Huffman enabled decoder requires 19 zero page vars and ~924 bytes of code ram.


Both decoders require a 2Kb page aligned workspace ram buffer which can be passed to the decoder via `vgm_init()`.

