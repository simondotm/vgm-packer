# vgm-packer
Python based compression tool for packing SN76489-based VGM file format music data into a much more optimized format for use on 8-bit CPU's.

Written by [Simon Morris](https://github.com/simondotm).

Release under MIT License, Copyright (c) 2019 Simon Morris. All rights reserved.

## Introduction

VGM files are a popular way to capture and record chiptune music. Since most sound chips work by having values programmed into their registers to create various tones and noises, the VGM file format is essentially a captured stream of these register data updates over time.

VGM file formats support a variety of different sound chips, and provide a sample rate of upto 44100Hz. In practice however, most systems typically update the sound chip at 50 or 60Hz.

For preserving music across a range of different sound chips, VGM is an excellent solution. However, for actually playing back these chip tunes on vintage hardware, VGM is not so great because the format is not designed for runtime environments where memory and cpu time is in short supply.

This projects addresses that problem by creating a new file format `.VGC` that incorporates a more efficient data storage approach, that can compress VGM files to **9-11%** of their original size.

**Please note - This tool is only designed to work with VGM files for the SN76489 PSG.**

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
### Command Line Example

Basic tool chain to create a `.vgc` file from a `.vgm` file is:

```
vgmpacker.py music.vgm -o music.vgc
```
*For an example of how a `.vgc` file can be loaded and processed for playback, some `6502` example code can be found in my [vgm-player-bbc](https://github.com/simondotm/vgm-player-bbc) project.*

---

## How it works
### An Overview of VGM files for SN76489

The SN76489 has 8 basic registers:
* Four tone registers (3x squarewave tone generators and 1 noise generator)
  * tone channels are 10-bit precision and written as two bytes
  * noise channel is 3-bit precision and written as one byte
* Four volume registers (4 bit resolution)
  * volumes are 4-bit precision and written as one byte.

Therefore a total of 11 bytes need to be sent to the soundchip to fully update *all* of the sound chip's registers. 

Within VGM files, these register updates are stored as "data" bytes in-between "command" bytes (which control how the VGM file should be parsed). The most common VGM command is "WAIT", which instructs the decoder to pause for one or more 1/44100Hz intervals before sending more data to the sound chip.

This is an elegant solution and very flexible, but for systems with limited RAM, we need a more optimal format.

### Optimizing VGM files

The first step we can take to simplify the VGM data format is to simply make use of assumptions:
1. That we are interested only in SN76489 data, so we can strip out other data
2. We do not need header information or metadata
3. We are going to be streaming data to the sound chip at a fixed 50Hz or 60Hz rate, so we do not need interval data

Just taking these initial steps, we can simply store 11 bytes of data per 50Hz or 60Hz frame, and reduce filesizes of VGMs by around 25%. (See `VGM Dump` below).

However, to gain maximum compression, we need to deploy a few additional tricks, so here is the full recipe for `.VGC`.

**Step 1 - Distill**

We load the VGM, strip out all unwanted information, and boil it down to a single stream of 11 byte data packets per 50Hz or 60Hz update.

**Step 2 - Reformat**

We then de-interleave and re-arrange this register data so that we have 8 separate serialised streams of data arrays as follows:
1. 16-bit tone register data for Tone Channel 0
2. 16-bit tone register data for Tone Channel 1
3. 16-bit tone register data for Tone Channel 2
4. 8-bit tone register data for Noise Channel 3
5. 8-bit volume register data for Tone Channel 0
6. 8-bit volume register data for Tone Channel 1
7. 8-bit volume register data for Tone Channel 2
8. 8-bit volume register data for Noise Channel 3

Because the data is now laid out in a homogenous way - always 11 bytes per frame and always in the same sequence - we no longer need to transmit the SN76489 "register flags" that are usually stored in the upper bits of the inital LATCH bytes that indicate flags such as data type, channel, tone or volume etc. because they are now implied. So we strip these out also.

**Step 3 - Run Length Encode**

Next, we perform a simple 4-bit run-length encoding scheme over each of the first three 16-bit data streams and the last five 8-bit data streams. This typically reduces the dataset by a significant amount depending on the music.

Whilst doing this, we also run a delta scheme on the noise tone data to ensure the decoder does not write to the noise tone register unless it needs to be updated. This is important because unlike the other registers which can be repeatedly set to the same value without any effect, writing to the noise tone register resets the state of the sound chip's LFSR which will introduce audible artifacts if not handled appropriately.

A pleasant side effect of run-length encoding is that the decoder side can save CPU cycles by not updating the sound chip for repeated runs of the same data to the same sound chip register.

***Special cases***

As a final bit of process, `.VGC` files store certain special byte codes in the Noise channel stream:
* `0x08` - Means End Of File (chosen because 0x08 is an invalid setting for the noise register)
* `0x0f` - Means do not update the noise channel this frame (to prevent LFSR reset)

EOF can be also detected by tracking decoder offsets against stream size, but it's handy to have a marker too.

**Step 4 - LZ4 Compression**

Steps 1,2, and 3 are all designed to maximise the amount of data redundancy that exists in the overall data (since individual registers tend to change infrequently between frames), so we can now use a compression algorithm to take advantage of this.

In an ideal world we would simply zip up the output of steps 1,2 and 3, and be happy, but once again, with 8-bit computers memory is at a premium and zipfiles are not an option. 

More significantly, we need *streamed random access* to these 8 separate data blocks because we have to feed the soundchip every frame with new data for all 8 registers, and it's simply not feasible to unpack the whole data set in order to do that - and anyway, the whole idea of the project is to be able to fit these compressed tunes into memory for playback purposes.

So instead, I decided to use a modified LZ4 (dictionary based) compression algorithm to compress each of the eight data streams *individually*.

I chose LZ4 because it is byte oriented, it is fast, and the encoder/decoder process is relatively simple. *(More info on this can be found in my [LZ4 compression project](https://github.com/simondotm/lz4enc-python) which formed the groundwork for this project)*.

The LZ4 algorithm we use has been tweaked to only use a 256 byte 'sliding window' and also to store its dictionary references as 8-bit values instead of 16-bit values.

Each of the 8 streams are then output as LZ4 Blocks. (VGC is essentially an LZ4 file format at a block/frame level).

The end result at this stage is that the data is usually compressed exceptionally well, typically saving upto 90% over the original VGM raw data.

**Step 5 - Huffman**

LZ4 doesn't do any bit-level encoding, as it is byte-oriented for speed. This means the resulting data does have a little bit of entropy we can squeeze. As an optional post-process I added a simple byte-based huffman encoder that takes advantage of the LZ4 byte-based token format and will typically reduce file size by a further 10-20%. It's not trivial to decode however, so it's optional - for those times when you need every last byte of RAM and unpack performance isn't the priority! 

**Step 6 - Decoding**

Finally, decoding the data from `.VGC` files requires the 8 streams to be individually addressed and decoded as 8 seperate (modified) LZ4 streams. For each 50Hz or 60Hz frame, the decoder pulls the bytes it needs for each sound chip register from the eight LZ4 streams (taking care to observe the run-length encoding) and sends them to the soundchip (with the correct register flags added back at runtime) as 16-bit tone commands , 8-bit noise commands, or 8-bit volume commands.

Since there are 8 LZ4 streams, the decoder needs 256 bytes of RAM for each stream - a total of 2048 bytes workspace. 


---
## `VGC` File Format

```
<LZ4 Frame Header>
  [Magic Number - 4 bytes]
    Either  0x56 0x47 0x43 0x00
    OR      0x56 0x47 0x43 0x80 (if Huffman encoded)
  [Flags - 1 byte] 
    0x40 (LZ4 version 1, no other header fields set)
  [Max Blocksize - 1 byte]
  [Header Checksum - 1 byte]
</LZ4 Frame Header>

If Huffman Encoded:
  <LZ4 Block Header>
    [LZ4 Block Size - 4 bytes] (bit 0x80000000 is set = Not LZ4 compressed)
    <Huffman Header Data Block>
      [Huffman data size - 4 bytes]
      [Huffman symbol count (S) - 1 byte] (0 means 256)
      [Huffman bit lengths table size (N) - 1 byte] (also counts as bit length table entry 0)
      [Huffman bit lengths table - N bytes] (number of codes of bitlength N)
      [Huffman symbol table - S bytes]
    </Huffman Header Data Block>
  </LZ4 BlockHeader>

For each of the 8 streams:
  <LZ4 Block Header>
    [LZ4 Block Size (N) - 4 bytes]  (bit 0x80000000 is clear = LZ4 compressed)
    <LZ4 Compressed Data Block>
      [LZ4 Compressed Data - N bytes]
    </LZ4 Compressed Data Block>
  </LZ4 BlockHeader>
    

<LZ4 Block Header>
  [Block Size - 4 bytes]
    0x00000000
</LZ4 BlockHeader>
```
*Note that the Magic Number for LZ4 files is `0x04 0x22 0x4D 0x18`, so VGC files will not be parsable by LZ4. This is intentional.*





---

## VGM Dump Utility

This repo also contains a utility script called `vgmdump.py` which will take a `.VGM` file containing `SN76489` music and export a `.RAW` file which is essentially the 'raw` SN76489 register data stored as 11 bytes x N frames (where 1 frame is a 50Hz or 60Hz update as defined as the playback rate in the source VGM file).

This is useful for testing purposes or if you ever need to stream raw VGM data to a chip.

`vgmdump` does not strip the register flags, so the data it outputs can be streamed directly to an SN76489.

### Command Line
```
VgmDump.py : VGM file dump utility
Written in 2019 by Simon Morris, https://github.com/simondotm/vgm-packer

usage: vgmdump.py [-h] [-o <output>] [-m] [-v] input

positional arguments:
  input                 VGM source file (must be single SN76489 PSG format)
                        [input]

optional arguments:
  -h, --help            show this help message and exit
  -o <output>, --output <output>
                        write VGC file <output> (default is '[input].raw')
  -m, --meta            Output metadata header
  -v, --verbose         Enable verbose mode
  ```

### Example Usage

```
vgmdump.py music.vgm -o music.raw
```

`vgmdump.py` will add a small meta data header to the raw file if `-m` option is selected.

---

## Technical Notes:
* The script is only compatible with Python 2.x at the moment. 
* The script includes other Python modules from the `modules` folder. For this reason it is not particularly portable as a standalone script so I recommend cloning the repo locally and creating your own subfolder for any vgm's you want to convert. 
* If you need to deploy it as part of a build process in a separate project you can copy `vgmpacker.py` but just make sure you also copy the `modules` folder too. _(I'm looking into ways to make it more self contained as a single script/pyc file)_
* The `modules` folder contains copies of my Python compression scripts from https://github.com/simondotm/lz4enc-python which is where I'm maintaining the release versions. 
* The [6502 decoder](https://github.com/simondotm/vgm-player-bbc) currently only supports buffer sizes of 255 bytes, so while you can use the `--buffer` option for experimentation it won't yield `.vgc` files that can be used on the 6502 at the moment. If you attempt this, know that: 1) buffer size > 255 will crash the decoder, and 2) buffer size < 255 will work ok, but you'll just get worse compression for no benefit on the decoder side. _I might look into supporting buffer sizes of 128 or 64 later since it may possible allow reduction of the 2kb workspace to 1kb or 0.5KB._

## Related Projects

* [Vgm Converter](https://github.com/simondotm/vgm-converter) - My VGM conversion utility 
* [Vgm Player](https://github.com/simondotm/vgm-player-bbc) - My Playback routines for `VGC` files on the 6502 BBC Micro
* [LZ4 Encoder For Python](https://github.com/simondotm/lz4enc-python) - My LZ4 compression utilities
* [YM2149 to SN76489 conversion](https://github.com/simondotm/ym2149f) - Conversion of YM2149 chiptunes to SN76489 VGM files