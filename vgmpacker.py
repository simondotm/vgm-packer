#!/usr/bin/env python
# vgmpacker.py
# Compression tool for optimal packing of SN76489-based PSG VGM data for use on 8-bit CPUs
# By Simon Morris (https://github.com/simondotm/)
# See https://github.com/simondotm/vgm-packer
#
# Copyright (c) 2019 Simon Morris. All rights reserved.
#
# "MIT License":
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software
# is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



# Packing SN76489 VGM data into the most efficient storage format requires:
#  1. Interleaved data unpacked into serialized data per register
#  2. Tone registers 0/1/2 packed as three separate 16-bit data series
#  3. Tone register 3 and volumes 0,1,2,3 packed as five separate 4-bit data series
#  This can achieve over 50% size reduction over the interleaved format
#  However, it requires 8 separately compressed data blocks, and also, a compression scheme that supports streamed decoding
#  Since most traditional compression schemes are 'in-place' decoders that back reference the previously unpacked data,
#   in order to support streamed decoding on 8-bit systems, our compression scheme has to use local decompression buffers.
# This packer deploys a number of techniques that provide the best compression for lowest ram overhead.
#   
# It utilises LZ4 and Huffman encoders from https://github.com/simondotm/lz4enc-python

import functools
import itertools
import struct
import sys
import time
import binascii
import math
import operator

import gzip
from os.path import basename

if (sys.version_info > (3, 0)):
	from io import BytesIO as ByteBuffer
else:
	from StringIO import StringIO as ByteBuffer

#from lz4enc import LZ4 
#from huffman import Huffman




#-------------------------------------------------------------------------------
# INCLUDE 'huffman.py' from https://github.com/simondotm/lz4enc-python
#-------------------------------------------------------------------------------

#!/usr/bin/env python
# Huffman.py 
# Succinct Huffman encoder with canonical code output
# By Simon Morris (https://github.com/simondotm/)
# See https://github.com/simondotm/lz4enc-python
# 
# based on https://github.com/adamldoyle/Huffman
#
# Modifications in this version 
# Copyright (c) 2019 Simon Morris. All rights reserved.
#
# "MIT License":
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software
# is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


from heapq import *
import array
import argparse
import os
import sys
from collections import defaultdict

# Notes about this implementation:
#  1) It does not support EOF huffman codes. This makes it simpler for use with 8-bit/byte based alphabets.
#     Instead we transmit the unpacked size as an indicator for how many symbols exist in the file. We also transmit the number of padding bits.
#  2) We only support huffman code sizes upto and including 16 bits in length.
#  3) Intended for use on small files (ie. < 10Mb), since much of the code uses in-memory manipulation. 
#  4) It is binary byte based rather than text based
#  5) It generates a canonical code table, and emits a header as follows:
#       [4 bytes][Uncompressed data size]
#       [1 byte][Number of symbols Ns in symbol table, 0 means 256]
#       [1 byte][Number of entries Nb in the bitlength table]
#       [Nb bytes][bit length table]
#       [Ns bytes][symbol table]
#       [Data...]
#  6) See decode() for example parsing
#
# TODO: add a peek table

if sys.version_info[0] > 2:
    print("Python 2 only")
    sys.exit()


class Huffman:

    MAX_CODE_BIT_LENGTH = 20    # change this if you need to check the codes are within a specific bit length range
    MAX_SYMBOLS = 256           # just for clarity of code. 
    VERBOSE = False

    def __init__(self):
        self.key = {}
        self.rKey = {}
        self.table_bitlengths = []
        self.table_symbols = []

    def build(self, phrase):
        self.setFrequency(phrase)
        self.buildTree()
        self.buildKey()
        self.buildCanonical()   # convert tree to canonical codes.        

    def setFrequency(self, phrase):
        self.frequency = defaultdict(int)
        for c in phrase:
            self.frequency[c] += 1
        

    def buildTree(self):
        self.heap = [[v, k] for k, v in self.frequency.iteritems()]
        heapify(self.heap)
        while len(self.heap) > 1:
            left, right = heappop(self.heap), heappop(self.heap)
            heappush(self.heap, [left[0] + right[0], left, right])

    def buildKey(self, root=None, code=''):
        if root is None:
            self.buildKey(self.heap[0])
            for k,v in self.key.iteritems():
                self.rKey[v] = k
        elif len(root) == 2:
            self.key[root[1]] = code
        else:
            self.buildKey(root[1], code+'0')
            self.buildKey(root[2], code+'1')

    # replace the previously calculated huffman tree codes with canonical codes
    def buildCanonical(self):

        # convert the tree to an array of (bitlength, symbol) tuples
        ktable = []
        for n in range(self.MAX_SYMBOLS):
            if n in self.key:
                ktable.append( (len(self.key[n]), n ) )

        # sort them into bitlength then symbol order
        ktable.sort( key=lambda x: (x[0], x[1]) )

        # get bit range
        minbits = ktable[0][0]
        maxbits = ktable[-1][0]
        # make sure our codes comply with the length constraints
        assert minbits > 0
        assert maxbits <= self.MAX_CODE_BIT_LENGTH

        # now we build the canonical codes, replacing the previously calculated codes as we go.
        bitlength = ktable[0][0] # start with smallest code length, always the first entry since sort
        code = 0
        numsymbols = len(ktable)
        for n in range(numsymbols):
            k = ktable[n] # tuple (bitlength, symbol)
            bitlength = k[0]
            codestring = format(code, '0' + str(bitlength) + 'b') # convert the code to a binary format string, leading zeros set to bitlength                
            self.key[k[1]] = codestring
            code = (code + 1) 
            if n < (numsymbols - 1):
                code <<= ( ktable[n+1][0] - bitlength )
            if self.VERBOSE:
                print("code=" + str(n) + ", bitlength=" + str(k[0]) + ", symbol=" + str(k[1]) + ", code=" + codestring + ", check=" + str(len(codestring)==bitlength))

        # build the tables needed for decoding 
        # - a sorted array where array[n] is the number of symbols with bitlength n
        # - an array of the symbols, in sorted ascending order 
        # create a local table for the sorted bitlengths and tables
        self.table_bitlengths = [0] * (self.MAX_CODE_BIT_LENGTH+1)
        self.table_symbols = []
        for k in ktable:
            self.table_bitlengths[k[0]] += 1
            self.table_symbols.append(k[1])

        if self.VERBOSE:
            print("decoder tables (size=" + str(len(self.table_bitlengths)+len(self.table_symbols)) + ")")
            print(self.table_bitlengths)
            print(self.table_symbols)



    def addHeader(self, src_data, cmp_data, wastedBits = 0):

        block = bytearray()

        # emit table header for the decoder
        # 4 byte header, representing:
        #  4 bytes unpacked size with top 3 bits being number of wasted bits in the stream. 
        #  this informs the decoder of the size of the uncompressed stream (ie. number of symbols to decode) and how many bits were wasted
        data_size = len(src_data)
        block.append( data_size & 255 )
        block.append( (data_size >> 8) & 255 )
        block.append( (data_size >> 16) & 255 )
        block.append( ((data_size >> 24) & 31) )

        # 1 byte symbol count
        # Note: this could be alternatively calculated as the sum of the non-zero bitlengths.  
        block.append( (len(self.table_symbols) & 255) ) # size of symbol table (0 means 256)          
    
        # emit N bytes for the code bit lengths (ie. the number of symbols that have a code of the given bit length)
        assert len(self.table_bitlengths) == (self.MAX_CODE_BIT_LENGTH+1)

        mincodelen = 65536
        maxcodelen = 0
        for v in self.key:
            codelen = len(self.key[v])
            mincodelen = min(mincodelen, codelen)
            maxcodelen = max(maxcodelen, codelen)

        #print(" codes from " + str(mincodelen) + " to " + str(maxcodelen) + " bits in length")
        # make sure our codes comply with the length constraint
        #assert maxcodelen <= self.MAX_CODE_BIT_LENGTH

        # We exploit the fact that no codes have a bit length of zero, so we use that field to transmit how long the bit length table is (in bytes)
        # This way we have a variable length header, and transmit the minimum amount of header data.
        self.table_bitlengths[0] = maxcodelen #len(self.table_symbols)
        for n in range(maxcodelen+1):
            block.append(self.table_bitlengths[n])

        # emit N bytes for the symbols table
        for n in self.table_symbols:
            block.append(n & 255)

        block += cmp_data
        return block

    # Huffman compress the given bytearray 'phrase' using the tree calculated by build()
    # Returns a bytearray() of the encoded data, with optional header data
    def encode(self, phrase, header = True):

        output = bytearray()

        # huffman encode and transmit the data stream
        currentbyte = 0  # The accumulated bits for the current byte, always in the range [0x00, 0xFF]
        numbitsfilled = 0  # Number of accumulated bits in the current byte, always between 0 and 7 (inclusive)

        sz = 0
        # for each symbol in the input data, fetch the assigned code and emit it to the output bitstream
        fastcount = 0
        bitsize_to_count = 8
        for c in phrase:
            k = self.key[c]
            sz += len(k)
            if len(k) <= bitsize_to_count:
                fastcount += 1
            for b in k:
                bit = int(b)
                assert bit == 0 or bit == 1
                currentbyte = (currentbyte << 1) | bit
                numbitsfilled += 1
                if numbitsfilled == 8:  # full byte, flush to output
                    output.append(currentbyte)
                    currentbyte = 0
                    numbitsfilled = 0                  

        if self.VERBOSE:
            print(" " + str(fastcount) + " of " + str(len(phrase)) + " symbols were " + str(bitsize_to_count) + " bits or less in size (" + str(fastcount*100/len(phrase)) + "%)")

        # align to byte. we could emit code >7 bits in length to prevent decoder finding a spurious code at the end, but its likely
        # some data sets may contain codes <7 bits. Easier to just pad wasted bytes.
        wastedbits = (8 - numbitsfilled) & 7
        while (numbitsfilled < 8) and wastedbits:
            currentbyte = (currentbyte << 1) | 1
            numbitsfilled += 1
        output.append(currentbyte)

        # add headers if required.
        if header:
            output = self.addHeader(phrase, output, wastedBits = wastedbits)

        if header:
            # test decode
            self.decode(output, phrase)

        return output

    # test decoder
    def decode(self, data, source):

        # read the header
        if self.VERBOSE:
            print("Checking data...")

        # get the unpacked size - this tells us how many symbols to decode
        unpacked_size = data[0] + (data[1]<<8) + (data[2]<<16) + ((data[3] & 31)<<24) # uncompressed size
        wastedbits = data[3] >> 5
        
        symbol_table_size = data[4]      # fetch the number of symbols in the symbol table
        length_table_size = data[5] + 1  # fetch the number of entries in the bit length table (+1 because we include zero)

        # interpret 0 as 256
        if symbol_table_size == 0:
            symbol_table_size = 256

        length_table = data[5:5+length_table_size]
        symbol_table = data[5+length_table_size:5+length_table_size+symbol_table_size]

        # decode the stream
        currentbyte = 5 + length_table_size + symbol_table_size

        output = bytearray()

        bitbuffer = 0
        numbitsbuffered = 0
        code = 0
        code_size = 0

        firstCodeWithNumBits = 0
        startIndexForCurrentNumBits = 0

        sourceindex = 0
        unpacked = 0
        while unpacked < unpacked_size:

            # keep the bitbuffer going
            if numbitsbuffered == 0:
                # we're out of data, so any wip codes are invalid due to byte padding.
                bitbuffer = data[currentbyte]
                currentbyte += 1
                numbitsbuffered += 8

            # get a bit
            bit = (bitbuffer & 128) >> 7
            bitbuffer <<= 1
            numbitsbuffered -= 1

            # build code
            code = (code << 1) | bit
            code_size += 1

            # how many canonical codes have this many bits
            assert code_size <= self.MAX_CODE_BIT_LENGTH
            numCodes = length_table[code_size]

            # if input code so far is within the range of the first code with the current number of bits, it's a match
            indexForCurrentNumBits = code - firstCodeWithNumBits
            if indexForCurrentNumBits < numCodes:
                code = startIndexForCurrentNumBits + indexForCurrentNumBits

                symbol = symbol_table[code]
                output.append(symbol)
                expected = source[sourceindex]
                assert symbol == expected
                sourceindex += 1

                code = 0
                code_size = 0

                firstCodeWithNumBits = 0
                startIndexForCurrentNumBits = 0      

                unpacked += 1          

            else:
                # otherwise, move to the next bit length
                firstCodeWithNumBits = (firstCodeWithNumBits + numCodes) << 1
                startIndexForCurrentNumBits += numCodes

        assert len(output) == len(source)
        assert output == source

        if self.VERBOSE:
            print(" Test decode OK.")



#-------------------------------------------------------------------------------
# INCLUDE 'lz4enc.py' from https://github.com/simondotm/lz4enc-python 
#-------------------------------------------------------------------------------

#!/usr/bin/env python
# lz4enc.py
# Python LZ4 compression module optimized for 8-bit CPU usage
# By Simon Morris (https://github.com/simondotm/)
# See https://github.com/simondotm/lz4enc-python
#
# Based on/derived from smallz4 
#
# Smallz4
# Copyright (c) 2016-2018 Stephan Brumme. All rights reserved.
# see https://create.stephan-brumme.com/smallz4/ and
#     https://github.com/stbrumme/smallz4
#
# Modifications in this version:
# Copyright (c) 2019 Simon Morris. All rights reserved.
#
# "MIT License":
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software
# is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import struct
import os
import sys
import argparse

from timeit import default_timer as timer
import profile

# LZ4 compression with optimal parsing, based on smallz4, refactored for more general purpose use within Python scripts.
class LZ4():

  # version string
  Version = "1.3"

  # compression level thresholds, made public because I display them in the help screen ...

  # greedy mode for short chains (compression level <= 3) instead of optimal parsing / lazy evaluation
  ShortChainsGreedy = 3
  # lazy evaluation for medium-sized chains (compression level > 3 and <= 6)
  ShortChainsLazy   = 6

  # ----- constants and types -----
  # a block can be 4 MB
  # matches must start within the most recent 64k

  # each match's length must be >= 4
  MinMatch          =  4
  # last match must not be closer than 12 bytes to the end
  BlockEndNoMatch   = 12
  # last 5 bytes must be literals, no matching allowed
  BlockEndLiterals  =  5
  # match finder's hash table size (2^HashBits entries, must be less than 32)
  HashBits          = 20
  # input buffer size, can be any number but zero ;-)
  BufferSize     = 64*1024
  # maximum match distance
  MaxDistance    = 65535
  # marker for "no match"
  NoPrevious     =       0
  # stop match finding after MaxChainLength steps (default is unlimited => optimal parsing)
  MaxChainLength = NoPrevious
  # significantly speed up parsing if the same byte is repeated a lot, may cause sub-optimal compression
  MaxSameLetter  =   19 + 255*256 # was: 19 + 255
  # refer to location of the previous match (implicit hash chain)
  PreviousSize   = 1 << 16
  # maximum block size as defined in LZ4 spec: { 0,0,0,0,64*1024,256*1024,1024*1024,4*1024*1024 }
  # I only work with the biggest maximum block size (7)
  #  note: xxhash header checksum is precalculated only for 7, too
  MaxBlockSizeId = 7
  MaxBlockSize   = 4*1024*1024

  # Number of bytes used to store match distances.
  # 2 bytes (16-bits) is the LZ4 default
  # If MaxDistance < 256, this can be set to 1 and the compressor will emit only 1 byte for distance offset rather than 2.
  # Note that changing this value will create non-compliant LZ4 data streams and require a customized decompressor
  DistanceByteSize = 2

  # Verbose mode
  Verbose = False

  # Debug mode
  Debug = False

  #  ----- code -----
  
  #-------------------------------------------------------------------------------------------------
  # create new compressor instance
  # Sets default compression level of 9
  #-------------------------------------------------------------------------------------------------
  def __init__(self, level = 9):
    self.setCompression(level)
    self.stats = {}

  #-------------------------------------------------------------------------------------------------
  # The encoder tracks compression statistics to enable output analysis
  # LZ4.resetStats() is called by LZ4.beginFrame()
  # it can also be called manually if compressing using LZ4.compressBlock()
  #-------------------------------------------------------------------------------------------------
  def resetStats(self):
    self.stats = {}
    # OLD COMMENT matchLength can be 4 + 14 + 254 in 12-bits = 272
    self.stats["tokenCount"] = 0      # number of tokens in this block
    self.stats["largestOffset"] = 0   # largest stored match distance in this block
    self.stats["largestLength"] = 0   # largest stored match length in this block
    self.stats["byteOffsetCount"] = 0 # number of match distances that were 255 or less
    self.stats["sameOffsetCount"] = 0 # number of match distances that were the same as the previous one
    self.stats["lastOffset"] = -1     # temporary var for tracking the last match distance

    # Gather LZ4 output data streams as separate blocks rather than the usual interleaved output (useful for creating custom formats)
    # Inspired by LZ5, more optimal non-LZ4 compatible output formats can be created by huffman encoding this data. 
    self.stats["tokens"] = []         # array of tokens stored in this block
    self.stats["offsets"] = []        # array of offsets (match distances) stored in this block
    self.stats["lengths"] = []        # array of match lengths stored in this block
    self.stats["literal_bytes"] = []  # array of literals byte stream data (as per LZ5)
    self.stats["lengths_bytes"] = []  # array of lengths byte stream data (as per LZ5) - literal lengths first, then match lengths (where length stored in the token was 15)
    

  #-------------------------------------------------------------------------------------------------
  # match struct
  #-------------------------------------------------------------------------------------------------
  class Match:
  
    def __init__(self):
      # length of match
      self.length = 0
      # start of match
      self.distance = 0

    # true, if long enough
    def isMatch(self):
      return self.length >= LZ4.MinMatch


  #-------------------------------------------------------------------------------------------------
  # find longest match of data[pos] between data[begin] and data[end], use match chain stored in previous
  # returns a Match object
  #
  # data - bytearray
  # pos, begin, end - int
  # previous - Distance list/array
  #-------------------------------------------------------------------------------------------------
  def findLongestMatch(self, data, pos, begin, end, previous):
  
    # return true, if the four bytes at data[a] and data[b] match
    def match4(a, b):
      # bytewise equivalence is fine
      return data[a:a+4] == data[b:b+4]
  
    result = self.Match()
    result.length = 1

    # compression level: look only at the first n entries of the match chain
    stepsLeft = self.maxChainLength

    # pointer to position that is matched against everything in data
    current = pos - begin

    # don't match beyond this point
    stop    = current + end - pos

    # get distance to previous match, abort if 0 => not existing
    distance = previous[pos % self.PreviousSize]
    totalDistance = 0
    while (distance != self.NoPrevious):
      # too far back ?
      totalDistance += distance
      if (totalDistance > self.MaxDistance):
        break

      # prepare next position
      distance = previous[(pos - totalDistance) % self.PreviousSize]
      
      # stop searching on lower compression levels
      if (stepsLeft <= 0):
        break
      stepsLeft -= 1


      # let's introduce a new pointer atLeast that points to the first "new" byte of a potential longer match
      atLeast = current + result.length + 1

      # the idea is to split the comparison algorithm into 2 phases
      # (1) scan backward from atLeast to current, abort if mismatch
      # (2) scan forward  until a mismatch is found and store length/distance of this new best match
      # current                  atLeast
      #    |                        |
      #    -<<<<<<<< phase 1 <<<<<<<<
      #                              >>> phase 2 >>>
      # impossible to find a longer match because not enough bytes left ?
      if (atLeast > stop):
        break
      # all bytes between current and atLeast shall be identical, compare 4 bytes at once
      compare = atLeast - 4

      INLINE_MATCH4 = True

      ok = True
      while (compare > current):

        # mismatch ?

        if INLINE_MATCH4:
          a = compare
          b = compare - totalDistance
          if data[a:a+4] != data[b:b+4]:
            ok = False
            break
        else:
          if (not match4(compare, compare - totalDistance)):
            ok = False
            break

        # keep going ...
        compare -= 4
        # note: - the first four bytes always match
        #       - in the last iteration, compare is either current + 1 or current + 2 or current + 3
        #       - therefore we compare a few bytes twice => but a check to skip these checks is more expensive
      

      # mismatch ?
      if (not ok):
        continue

      # we have a new best match, now scan forward from the end
      compare = atLeast

      # fast loop: check four bytes at once
      if INLINE_MATCH4:
        compare2 = compare - totalDistance
        while (compare + 4 <= stop and data[compare:compare+4] == data[compare2:compare2+4]):
          compare += 4
          compare2 += 4
      else:
        while (compare + 4 <= stop and match4(compare,     compare - totalDistance)):
          compare += 4



      # slow loop: check the last 1/2/3 bytes
      while (compare < stop and data[compare] == data[compare - totalDistance]):
        compare += 1

      # store new best match
      result.distance = totalDistance
      result.length   = compare - current
    
    return result
  
  #-------------------------------------------------------------------------------------------------
  # create shortest output
  #  data points to block's begin; we need it to extract literals
  #
  # returns bytearray
  #-------------------------------------------------------------------------------------------------
  def selectBestMatches(self, matches, data, index):
    # store encoded data
    result = bytearray()

    # indices of current literal run
    literalsFrom = 0
    literalsTo   = 0 # point beyond last literal of the current run

    # walk through the whole block
    offset = 0
    while (offset < len(matches)): # increment inside of loop

      # get best cost-weighted match
      match = self.Match()
      match.length = matches[offset].length
      match.distance = matches[offset].distance
      
      # debug output
      if self.Debug:
        print("offset="+str(offset)+", length="+str(match.length)+", distance="+str(match.distance))
      
      # if no match, then count literals instead
      if (not match.isMatch()):
      
        # first literal
        if (literalsFrom == literalsTo):
          literalsFrom = literalsTo = offset

        # one more literal
        literalsTo += 1
        # ... and definitely no match
        match.length = 1
      
      offset += match.length

      lastToken = (offset == len(matches))
      # continue if simple literal
      if (not match.isMatch() and not lastToken):
        continue

      # emit token
      # count literals
      numLiterals = literalsTo - literalsFrom

      # store literals' length
      if (numLiterals < 15):
        token = numLiterals
      else:
        token = 15

      token <<= 4

      # store match length (4 is implied because it's the minimum match length)
      matchLength = match.length - 4
      if (not lastToken):
        if (matchLength < 15):
          token |= matchLength
        else:
          token |= 15

      result.append( token ) #struct.pack('B', token) )
      self.stats["tokens"].append( token )

      self.stats["tokenCount"] += 1    
      self.stats["offsets"].append( match.distance )
      self.stats["lengths"].append( matchLength )


      # >= 15 literals ? (extra bytes to store length)
      if (numLiterals >= 15):
      
        # 15 is already encoded in token
        numLiterals -= 15
        # emit 255 until remainder is below 255
        while (numLiterals >= 255):       
          result.append(255)
          self.stats["lengths_bytes"].append(255)
          numLiterals -= 255
        
        # and the last byte (can be zero, too)
        result.append(numLiterals)
        self.stats["lengths_bytes"].append(numLiterals)
      
      # copy literals
      if (literalsFrom != literalsTo):
      
        subset = data[index + literalsFrom:index + literalsTo]
        result.extend( subset )
        for z in subset:
          self.stats["literal_bytes"].append(z)

        literalsFrom = 0
        literalsTo = 0
      
      # last token doesn't have a match
      if (lastToken):
        break

      # stats
      if match.distance > self.stats["largestOffset"]:
        self.stats["largestOffset"] = match.distance
      if matchLength > self.stats["largestLength"]:
        self.stats["largestLength"] = matchLength
      if match.distance < 256:
        self.stats["byteOffsetCount"] += 1
      if match.distance == self.stats["lastOffset"]:
        self.stats["sameOffsetCount"] += 1
      self.stats["lastOffset"] = match.distance

      # distance stored as 1 or two bytes in data stream
      if self.DistanceByteSize == 1:
        assert match.distance < 256
        # distance stored in 8 bits (non LZ4 compliant, but optimal data size - requires modified decoder)
        result.append( match.distance & 0xFF )
      else:
        # distance stored in 16 bits / little endian
        result.append( match.distance & 0xFF )
        result.append( (match.distance >> 8) & 0xFF )



      # >= 15+4 bytes matched (4 is implied because it's the minimum match length)
      if (matchLength >= 15):
        # 15 is already encoded in token
        matchLength -= 15
        # emit 255 until remainder is below 255
        while (matchLength >= 255):
          result.append(255)
          self.stats["lengths_bytes"].append(255)
          matchLength -= 255
        
        # and the last byte (can be zero, too)
        result.append(matchLength)
        self.stats["lengths_bytes"].append(matchLength)
      

    # debug output
    if LZ4.Debug:
      print("    largestOffset=" + str(largestOffset))
      print("    largestLength=" + str(largestLength))
      print("       tokenCount=" + str(tokenCount))
      print("  byteOffsetCount=" + str(byteOffsetCount) + " (ie. offsets were <256)")
      print("  sameOffsetCount=" + str(sameOffsetCount) + " (ie. number of offsets that were repeated)")

    return result
  
  #-------------------------------------------------------------------------------------------------
  # walk backwards through all matches and compute number of compressed bytes from current position to the end of the block
  #  note: matches are modified (shortened length) if necessary
  #-------------------------------------------------------------------------------------------------
  def estimateCosts(self, matches):
    blockEnd = len(matches)

    # minimum cost from this position to the end of the current block
    cost = [0] * len(matches)
    
    # "cost" represents the number of bytes needed
    # backwards optimal parsing
    posLastMatch = blockEnd

    # ignore the last 5 bytes, they are always literals
    blockRange = blockEnd - (1 + self.BlockEndLiterals)
    for i in range(blockRange, -1, -1 ): # lower range is -1 so we hit 0

      # show progress
      if (i & 511) == 0:
        sys.stdout.write("   Calculating cost data " + str(100-int(i*100/(blockRange))) + "%...\r")
        sys.stdout.flush()

      # watch out for long literal strings that need extra bytes
      numLiterals = posLastMatch - i
      # assume no match
      minCost = cost[i + 1] + 1
      # an extra byte for every 255 literals required to store length (first 14 bytes are "for free")
      if (numLiterals >= 15 and (numLiterals - 15) % 255 == 0):
        minCost += 1

      # if encoded as a literal
      bestLength = 1

      # analyze longest match
      match = self.Match()
      match.length = matches[i].length
      match.distance = matches[i].distance    
      
      # match must not cross block borders
      if (match.isMatch() and i + match.length + self.BlockEndLiterals > blockEnd):
        match.length = blockEnd - (i + self.BlockEndLiterals)

      # try all match lengths (first short ones)
      for length in range(self.MinMatch, match.length+1):
      
        # token (1 byte) + offset (1 or 2 bytes)
        currentCost = cost[i + length] + 1 + self.DistanceByteSize # might be 1 byte offset cost instead of 2

        # very long matches need extra bytes for encoding match length
        if (length >= 19):
          currentCost += 1 + (length - 19) / 255
        
        # better choice ?
        if (currentCost <= minCost):
        
          # regarding the if-condition:
          # "<"  prefers literals and shorter matches
          # "<=" prefers longer matches
          # they should produce the same number of bytes (because of the same cost)
          # ... but every now and then it doesn't !
          # that's why: too many consecutive literals require an extra length byte
          # (which we took into consideration a few lines above)
          # but we only looked at literals beyond the current position
          # if there are many literal in front of the current position
          # then it may be better to emit a match with the same cost as the literals at the current position
          # => it "breaks" the long chain of literals and removes the extra length byte
          minCost    = currentCost
          bestLength = length
          # performance-wise, a long match is usually faster during decoding than multiple short matches
          # on the other hand, literals are faster than short matches as well (assuming same cost)
        
        # workaround: very long self-referencing matches can slow down the program A LOT
        if (match.distance == 1 and match.length >= self.MaxSameLetter):
        
          # assume that longest match is always the best match
          # however, this assumption might not be optimal
          bestLength = match.length
          minCost    = cost[i + match.length] + 1 + self.DistanceByteSize + 1 + (match.length - 19) / 255
          break
        
      
      # remember position of last match to detect number of consecutive literals
      if (bestLength >= self.MinMatch):
        posLastMatch = i

      # store lowest cost so far
      cost[i] = minCost
      # and adjust best match
      matches[i].length = bestLength
      if (bestLength == 1):
        matches[i].distance = self.NoPrevious

      # note: if bestLength is smaller than the previous matches[i].length then there might be a closer match
      #       which could be more cache-friendly (=> faster decoding)
    
  #--------------------------------------------------------------------------------------------------------------------------------
  # create an LZ4 compressed block from the input buffer
  # return the compressed block as an output buffer
  # improve compression with a predefined dictionary
  #--------------------------------------------------------------------------------------------------------------------------------
  # inputData, and dictionary are bytearray's
  # returns a bytearray containing the compressed LZ4 stream
  def compressBlock(self, inputData, dictionary = bytearray()):

    outputData = bytearray()

    # write a byte array to the output buffer, data can be a byte or a byte array
    def sendBytes(data):
      if len(data) == 1:
        outputData.append(data)
      else:
        outputData.extend(data)

    # read upto count bytes from the input buffer, returned in a new bytearray 'buffer'. Returns an empty buffer if no more data available.
    def getBytes(count):
      ptr = getBytes.inputPointer
      if ptr >= len(inputData):
        return bytearray()
      else:
        if ptr+count >= len(inputData):
          count = len(inputData) - ptr

        buf = inputData[ptr:ptr+count]
        getBytes.inputPointer = ptr + count
        return buf

    # initialise getByte read stream
    getBytes.inputPointer = 0
   
    # ==================== declarations ====================
    # read the file in chunks/blocks, data will contain only bytes which are relevant for the current block
    data = bytearray()
    # file position corresponding to data[0]
    dataZero = 0
    # last already read position
    numRead  = 0
    # passthru data (but still wrap in LZ4 format)
    uncompressed = (self.maxChainLength == 0)
    # last time we saw a hash
    HashSize   = 1 << self.HashBits
    NoLastHash = 0x7FFFFFFF

    lastHash = [NoLastHash] * HashSize

    HashMultiplier = 22695477 # taken from https:#en.wikipedia.org/wiki/Linear_congruential_generator
    HashShift  = 32 - self.HashBits # uint8
    
    # previous position which starts with the same bytes
    previousHash = [self.NoPrevious] * self.PreviousSize
    previousExact = [self.NoPrevious] * self.PreviousSize
    
    
    # change buffer size as you like
    buffer = bytearray(self.BufferSize)

    # first and last offset of a block (next is end-of-block plus 1)
    lastBlock = 0
    nextBlock = 0
    parseDictionary = len(dictionary) > 0

    while (True):
    
      # ==================== start new block ====================
      # first byte of the currently processed block (std::vector data may contain the last 64k of the previous block, too)

      # dataBlock is an offset within data[] - see below

      # prepend dictionary
      if (parseDictionary):

        if LZ4.Verbose:
          print(" Loading Dictionary...")

        # prepend exactly 64k
        MaxDictionary = 65536
        if (len(dictionary) < MaxDictionary):
          # add garbage data
          unused = 65536 - len(dictionary)
          data.extend( bytearray(unused) )
        else:
          # copy only the most recent 64k of the dictionary
          doffset = len(dictionary) - MaxDictionary
          data.extend( bytearray( dictionary[doffset:]) )

        nextBlock = len(data)
        numRead   = len(data)
      
      # read more bytes from input
      maxBlockSize = self.MaxBlockSize



      while (numRead - nextBlock < maxBlockSize):
      
        # buffer can be significantly smaller than MaxBlockSize, that's the only reason for this while-block
        buffer = getBytes(self.BufferSize)
        incoming = len(buffer)
        if (incoming == 0):
          break
        numRead += incoming

        data.extend( buffer )
      
      # no more data ? => WE'RE DONE !
      if (nextBlock == numRead):
        break

      if LZ4.Verbose:
        print(" Processing Block... " + str(numRead>>10) + "Kb, (maxBlockSize=" + str(maxBlockSize>>10) + "Kb, windowSize=" + str(self.MaxDistance>>10) + "Kb)")

      # determine block borders
      lastBlock  = nextBlock
      nextBlock += maxBlockSize

      # not beyond end-of-file
      if (nextBlock > numRead):
        nextBlock = numRead

      # first byte of the currently processed block (std::vector data may contain the last 64k of the previous block, too)

      # dataBlock is an offset into data[]
      dataBlock = lastBlock - dataZero
      blockSize = nextBlock - lastBlock

      # ==================== full match finder ====================
      if LZ4.Verbose:
        print("  Finding matches...")
      # greedy mode is much faster but produces larger output
      isGreedy = (self.maxChainLength <= self.ShortChainsGreedy)
      # lazy evaluation: if there is a (match, then try running match finder on next position, too, but not after that
      isLazy   = (isGreedy == False) and (self.maxChainLength <= self.ShortChainsLazy)

      # skip match finding on the next x bytes in greedy mode
      skipMatches = 0
      # allow match finding on the next byte but skip afterwards (in lazy mode)
      lazyEvaluation = False

      # the last literals of the previous block skipped matching, so they are missing from the hash chains
      lookback = dataZero
      if (lookback > self.BlockEndNoMatch and (parseDictionary == False)):
        lookback = self.BlockEndNoMatch

      if (parseDictionary):
        lookback = len(dictionary)

      # so let's go back a few bytes
      lookback = -lookback
 
      matches = [ self.Match() for i in range(blockSize) ]

      # find longest matches for each position
      for i in range(lookback, blockSize):

        # show progress
        if (i & 511) == 0 or i == (blockSize - 1):
          sys.stdout.write("   Scanning block data " + str(int(i*100/(blockSize-1))) + "%...\r")
          sys.stdout.flush()

        # no matches at the end of the block (or matching disabled by command-line option -0 )
        if (i + self.BlockEndNoMatch > blockSize or uncompressed):
          continue
      
        # detect self-matching
        if (i > 0 and data[dataBlock + i] == data[dataBlock + i - 1]):

          prevMatch = matches[i - 1]  # Python version of prevMatch is a reference not an instance
          
          # predecessor had the same match ?
          if (prevMatch.distance == 1 and prevMatch.length > self.MaxSameLetter): # TODO: handle very long self-referencing matches          
            # just copy predecessor without further (expensive) optimizations
            matches[i].length = prevMatch.length - 1
            matches[i].distance = prevMatch.distance
            continue
          
        def getLong(buffer, offset):
          end = offset + 4
          buf = buffer[offset:end]
          four = struct.unpack('>L', buf)[0]
          return four

        # read next four bytes
        four = getLong(data, dataBlock + i)

        # convert to a shorter hash
        hash = ((four * HashMultiplier) >> HashShift) & (HashSize - 1)
        
        # get last occurrence of these bits
        last = lastHash[hash]
        
        # and store current position
        lastHash[hash] = i + lastBlock
        
        # remember: i could be negative, too
        prevIndex = (i + self.PreviousSize) % self.PreviousSize
        
        # no predecessor or too far away ?
        distance = i + lastBlock - last
        if (last == NoLastHash or distance > self.MaxDistance):
          previousHash[prevIndex] = self.NoPrevious
          previousExact[prevIndex] = self.NoPrevious
          continue
        
        # build hash chain, i.e. store distance to last match
        previousHash[prevIndex] = distance

        # skip pseudo-matches (hash collisions) and build a second chain where the first four bytes must match exactly
        while (distance != self.NoPrevious):
          curFour = getLong(data, last - dataZero)  # may be in the previous block, too

          # actual match found, first 4 bytes are identical
          if (curFour == four):
            break

          # prevent from accidently hopping on an old, wrong hash chain
          curHash = ((curFour * HashMultiplier) >> HashShift) & (HashSize - 1)
          if (curHash != hash):
            distance = self.NoPrevious
            break
          
          # try next pseudo-match
          next = previousHash[last % self.PreviousSize]

          # pointing to outdated hash chain entry ?
          distance += next

          if (distance > self.MaxDistance):
            previousHash[last % self.PreviousSize] = self.NoPrevious
            distance = self.NoPrevious
            break
          
          # closest match is out of range ?
          last -= next
          if (next == self.NoPrevious or last < dataZero):
            distance = self.NoPrevious
            break
          
        
        # no match at all ?
        if (distance == self.NoPrevious):
          previousExact[prevIndex] = self.NoPrevious
          continue
        
        # store distance to previous match
        previousExact[prevIndex] = distance

        # no matching if crossing block boundary, just update hash tables
        if (i < 0):
          continue

        # skip match finding if in greedy mode
        if (skipMatches > 0):
          skipMatches -= 1
          if (not lazyEvaluation):
            continue

          lazyEvaluation = False
        
        # and look for longest match
        longest = self.findLongestMatch(data, i + lastBlock, dataZero, nextBlock - self.BlockEndLiterals + 1, previousExact)
        matches[i] = longest

        # no match finding needed for the next few bytes in greedy/lazy mode
        if (longest.isMatch() and (isLazy or isGreedy)):
          lazyEvaluation = (skipMatches == 0)
          skipMatches = longest.length
        
      
      # dictionary applies only to the first block
      parseDictionary = False
      
      # ==================== estimate costs (number of compressed bytes) ====================
      if LZ4.Verbose:
        print("")
        print("  Estimating costs...")

      # not needed in greedy mode and/or very short blocks
      if (len(matches) > self.BlockEndNoMatch and self.maxChainLength > self.ShortChainsGreedy):
        self.estimateCosts(matches)

      # ==================== select best matches ====================
      if LZ4.Verbose:
        print("")
        print("  Selecting best matches...")
      
      block = bytearray()
      if (not uncompressed):
        block = self.selectBestMatches(matches, data, lastBlock - dataZero )
      
      # ==================== output ====================
      # automatically decide whether compressed or uncompressed
      uncompressedSize = nextBlock - lastBlock

      # did compression do harm ?
      useCompression   = len(block) < uncompressedSize and not uncompressed

      if LZ4.Verbose:
        print(" Writing output block - uncompressed (" + str(uncompressedSize) + "), compressed (" + str(len(block)) + ") ...")
        if useCompression:
          print("  Compressed data selected for this block.")
        else:
          print("  Uncompressed data selected for this block.")
     
      # block size
      if useCompression:
        numBytes = len(block)
      else:
        numBytes = uncompressedSize

      numBytesTagged = numBytes
      if (not useCompression):
        numBytesTagged |= 0x80000000

      num1 =  numBytesTagged         & 0xFF
      sendBytes( struct.pack('B', num1) )
      num2 = (numBytesTagged >>  8)  & 0xFF
      sendBytes( struct.pack('B', num2) )
      num3 = (numBytesTagged >> 16)  & 0xFF
      sendBytes( struct.pack('B', num3) )
      num4 = (numBytesTagged >> 24)  & 0xFF
      sendBytes( struct.pack('B', num4) )
      
      if (useCompression):
        sendBytes(block)
      else: # uncompressed ? => copy input data
        index = lastBlock - dataZero
        sendBytes( data[index:index + numBytes] )

      # disable matching across blocks if True (was a legacy format code path)
      if (False):
        dataZero += len(data)
        data = bytearray()

        # clear hash tables
        for i in range(len(previousHash)):
          previousHash[i] = self.NoPrevious
          previousExact[i] = self.NoPrevious

        for i in range(len(lastHash)):
          lastHash[i] = self.NoLastHash
    
      else:
      
        # remove already processed data except for the last 64kb which could be used for intra-block matches
        if (len(data) > self.MaxDistance):
          remove = len(data) - self.MaxDistance
          dataZero += remove
          data = data[remove:]

    return outputData
    
  #-------------------------------------------------------------------------------------------------
  # Emit an LZ4 compatible frame header to the outputBuffer bytearray  
  #-------------------------------------------------------------------------------------------------
  def beginFrame(self, outputBuffer):

    # ==================== write LZ4 header ====================
    # magic bytes
    outputBuffer.extend( bytearray([0x04, 0x22, 0x4D, 0x18]) )
      
    # flags
    # (7-6) FieldName	Version (5)	B.Indep (4)	B.Checksum (3)	C.Size (2)	C.Checksum (1) Reserved (0)	DictID
    flags = 1 << 6 # Version, dependent blocks, no block checksum, no size, no content checksum, no dict ID 
    outputBuffer.append( struct.pack('B', flags) )

    # max blocksize
    maxBlockSizeId = self.MaxBlockSizeId << 4
    outputBuffer.append( struct.pack('B', maxBlockSizeId) )
    
    # header checksum (precomputed)
    checksum = 0xDF
    outputBuffer.append( struct.pack('B', checksum) )

    # reset stats for each frame
    # (can be manually called per-block also if desired)
    self.resetStats()


  
  #-------------------------------------------------------------------------------------------------
  # Emit an LZ4 compatible frame end signal (a block with size 0)
  #-------------------------------------------------------------------------------------------------
  def endFrame(self, outputBuffer):
    # add an empty block
    outputBuffer.extend(struct.pack('i', 0))    

  #-------------------------------------------------------------------------------------------------
  # setCompression
  # Set the encoder match parameter for compression ratio/speed tradeoff
  # compressionLevel can be 0 (uncompressed), or 1 (low compression, high encoding speed) to 9 (optimal compression, low encoding speed) 
  #-------------------------------------------------------------------------------------------------
  def setCompression(self, compressionLevel, windowSize = 65535):
    assert windowSize < 65536
    if (compressionLevel >= 9):
      newMaxChainLength = 65536  # "unlimited" because search window contains only 2^16 bytes 
    else:
      newMaxChainLength = compressionLevel

    # how many matches are checked in findLongestMatch, lower values yield faster encoding at the cost of worse compression ratio
    self.maxChainLength = newMaxChainLength
    # => no limit, but can be changed by setMaxChainLength

    # set the sliding window size, 65535 is the default since that is the maximum supported by the 16-bit offset format (ignoring 0, which is an invalid value)
    self.MaxDistance = windowSize

  #-------------------------------------------------------------------------------------------------
  # enable a byte optimized compression mode
  # LZ4 offsets will be output as 8-bits rather than 16-bits, and windowSize will be set to 255
  # For many data streams this will add a few % to the compression ratio
  # Note that this will create output that is no longer LZ4 format compatible and the decoder is assumed to be modified accordingly
  #-------------------------------------------------------------------------------------------------
  def optimizedCompression(self, enable = True):
    if enable:
      self.DistanceByteSize = 1
      if self.MaxDistance > 255:
        self.MaxDistance = 255
    else:
        self.DistanceByteSize = 2



  def getCompressionLevel(self):
    return self.maxChainLength

  def getWindowSize(self):
    return self.MaxDistance

  #--------------------------------------------------------------------------------------------------------------------------------
  # compress everything in input stream (accessed via getByte) and write to complete LZ4 output stream
  # improve compression with a predefined dictionary
  #--------------------------------------------------------------------------------------------------------------------------------
  # inputData, and dictionary are bytearray's
  # returns a bytearray containing the compressed LZ4 stream
  def compress(self, inputData, dictionary = bytearray()):
    outputBuffer = bytearray()
    self.beginFrame(outputBuffer)
    compressedBlock = self.compressBlock(inputData, dictionary)
    outputBuffer.extend(compressedBlock)
    self.endFrame(outputBuffer)
    return outputBuffer


#-------------------------
# main()
#-------------------------

def main(args):

  start_time = timer()  

  src = args.input
  dst = args.output
  if dst == None:
    dst = src + ".lz4"

  # enable verbose mode
  LZ4.Verbose = args.verbose

  # check for missing files
  if not os.path.isfile(src):
    print("ERROR: File '" + src + "' not found")
    sys.exit()

  # create the instance
  compressor = LZ4()

  # set the compression parameters
  compressor.setCompression(args.compress, args.window)

  # load the input file into memory
  fh = open(src, 'rb')
  file_in = bytearray(fh.read())
  fh.close()

  # compress into LZ4 file stream
  if LZ4.Verbose:
    print("Compressing file '" + src + "' to '" + dst + "', using compression level " + str(args.compress) )
  file_out = compressor.compress(file_in, bytearray())

  # write the LZ4 stream to output file
  fh = open(dst, 'wb')
  fh.write(file_out)
  fh.close()

  # stats
  src_size = os.path.getsize(src)
  dst_size = os.path.getsize(dst)
  if src_size == 0:
    ratio = 0
  else:
    ratio = 100 - (int)((dst_size*100 / src_size))

  print("Compressed " + str(src_size) + " bytes into " + str(dst_size) + " bytes => " + str(ratio) + "%" )

  end_time = timer()

  if LZ4.Verbose:
    t = '{:.2f}'.format(end_time-start_time)
    print("Completed in " + t + "s.")


#-------------------------------------------------------------------------------------------------
# INCLUDE 'vgmconverter.py' from https://github.com/simondotm/vgm-converter
#-------------------------------------------------------------------------------------------------


class FatalError(Exception):
	pass

class VgmStream:


	# VGM commands:
	# 0x50	[dd]	= PSG SN76489 write value dd
	# 0x61	[nnnn]	= WAIT n cycles (0-65535)
	# 0x62			= WAIT 735 samples (1/60 sec)
	# 0x63			= WAIT 882 samples (1/50 sec)
	# 0x66			= END
	# 0x7n			= WAIT n+1 samples (0-15)

	#--------------------------------------------------------------------------------------------------------------------------------
	# SN76489 register writes
	# If bit 7 is 1 then the byte is a LATCH/DATA byte.
	#  %1cctdddd
	#	cc - channel (0-3)
	#	t - type (1 to latch volume, 1 to latch tone/noise)
	#	dddd - placed into the low 4 bits of the relevant register. For the three-bit noise register, the highest bit is discarded.
	#
	# If bit 7 is 0 then the byte is a DATA byte.
	#  %0-DDDDDD
	# If the currently latched register is a tone register then the low 6 bits of the byte (DDDDDD) 
	#	are placed into the high 6 bits of the latched register. If the latched register is less than 6 bits wide 
	#	(ie. not one of the tone registers), instead the low bits are placed into the corresponding bits of the 
	#	register, and any extra high bits are discarded.
	#
	# Tone registers
	#	DDDDDDdddd = cccccccccc
	#	DDDDDDdddd gives the 10-bit half-wave counter reset value.
	#
	# Volume registers
	#	(DDDDDD)dddd = (--vvvv)vvvv
	#	dddd gives the 4-bit volume value.
	#	If a data byte is written, the low 4 bits of DDDDDD update the 4-bit volume value. However, this is unnecessary.
	#
	# Noise register
	#	(DDDDDD)dddd = (---trr)-trr
	#	The low 2 bits of dddd select the shift rate and the next highest bit (bit 2) selects the mode (white (1) or "periodic" (0)).
	#	If a data byte is written, its low 3 bits update the shift rate and mode in the same way.
	#--------------------------------------------------------------------------------------------------------------------------------

	# script vars / configs

	VGM_FREQUENCY = 44100


	# script options
	RETUNE_PERIODIC = True	# [TO BE REMOVED] if true will attempt to retune any use of the periodic noise effect
	VERBOSE = False
	STRIP_GD3 = False	
	LENGTH = 0 # required output length (in seconds)
	
	# VGM file identifier
	vgm_magic_number = b'Vgm '

	disable_dual_chip = True # [TODO] handle dual PSG a bit better

	vgm_source_clock = 0
	vgm_target_clock = 0
	vgm_filename = ''
	vgm_loop_offset = 0
	vgm_loop_length = 0
	
	# Supported VGM versions
	supported_ver_list = [
		0x00000101,
		0x00000110,
		0x00000150,
		0x00000151,
		0x00000160,
		0x00000161,
	]

	# VGM metadata offsets
	metadata_offsets = {
		# SDM Hacked version number 101 too
		0x00000101: {
			'vgm_ident': {'offset': 0x00, 'size': 4, 'type_format': None},
			'eof_offset': {'offset': 0x04, 'size': 4, 'type_format': '<I'},
			'version': {'offset': 0x08, 'size': 4, 'type_format': '<I'},
			'sn76489_clock': {'offset': 0x0c, 'size': 4, 'type_format': '<I'},
			'ym2413_clock': {'offset': 0x10, 'size': 4, 'type_format': '<I'},
			'gd3_offset': {'offset': 0x14, 'size': 4, 'type_format': '<I'},
			'total_samples': {'offset': 0x18, 'size': 4, 'type_format': '<I'},
			'loop_offset': {'offset': 0x1c, 'size': 4, 'type_format': '<I'},
			'loop_samples': {'offset': 0x20, 'size': 4, 'type_format': '<I'},
			'rate': {'offset': 0x24, 'size': 4, 'type_format': '<I'},
			'sn76489_feedback': {
				'offset': 0x28,
				'size': 2,
				'type_format': '<H',
			},
			'sn76489_shift_register_width': {
				'offset': 0x2a,
				'size': 1,
				'type_format': 'B',
			},
			'ym2612_clock': {'offset': 0x2c, 'size': 4, 'type_format': '<I'},
			'ym2151_clock': {'offset': 0x30, 'size': 4, 'type_format': '<I'},
			'vgm_data_offset': {
				'offset': 0x34,
				'size': 4,
				'type_format': '<I',
			},
		},

		# Version 1.10`
		0x00000110: {
			'vgm_ident': {'offset': 0x00, 'size': 4, 'type_format': None},
			'eof_offset': {'offset': 0x04, 'size': 4, 'type_format': '<I'},
			'version': {'offset': 0x08, 'size': 4, 'type_format': '<I'},
			'sn76489_clock': {'offset': 0x0c, 'size': 4, 'type_format': '<I'},
			'ym2413_clock': {'offset': 0x10, 'size': 4, 'type_format': '<I'},
			'gd3_offset': {'offset': 0x14, 'size': 4, 'type_format': '<I'},
			'total_samples': {'offset': 0x18, 'size': 4, 'type_format': '<I'},
			'loop_offset': {'offset': 0x1c, 'size': 4, 'type_format': '<I'},
			'loop_samples': {'offset': 0x20, 'size': 4, 'type_format': '<I'},
			'rate': {'offset': 0x24, 'size': 4, 'type_format': '<I'},
			'sn76489_feedback': {
				'offset': 0x28,
				'size': 2,
				'type_format': '<H',
			},
			'sn76489_shift_register_width': {
				'offset': 0x2a,
				'size': 1,
				'type_format': 'B',
			},
			'ym2612_clock': {'offset': 0x2c, 'size': 4, 'type_format': '<I'},
			'ym2151_clock': {'offset': 0x30, 'size': 4, 'type_format': '<I'},
			'vgm_data_offset': {
				'offset': 0x34,
				'size': 4,
				'type_format': '<I',
			},
		},
		# Version 1.50`
		0x00000150: {
			'vgm_ident': {'offset': 0x00, 'size': 4, 'type_format': None},
			'eof_offset': {'offset': 0x04, 'size': 4, 'type_format': '<I'},
			'version': {'offset': 0x08, 'size': 4, 'type_format': '<I'},
			'sn76489_clock': {'offset': 0x0c, 'size': 4, 'type_format': '<I'},
			'ym2413_clock': {'offset': 0x10, 'size': 4, 'type_format': '<I'},
			'gd3_offset': {'offset': 0x14, 'size': 4, 'type_format': '<I'},
			'total_samples': {'offset': 0x18, 'size': 4, 'type_format': '<I'},
			'loop_offset': {'offset': 0x1c, 'size': 4, 'type_format': '<I'},
			'loop_samples': {'offset': 0x20, 'size': 4, 'type_format': '<I'},
			'rate': {'offset': 0x24, 'size': 4, 'type_format': '<I'},
			'sn76489_feedback': {
				'offset': 0x28,
				'size': 2,
				'type_format': '<H',
			},
			'sn76489_shift_register_width': {
				'offset': 0x2a,
				'size': 1,
				'type_format': 'B',
			},
			'ym2612_clock': {'offset': 0x2c, 'size': 4, 'type_format': '<I'},
			'ym2151_clock': {'offset': 0x30, 'size': 4, 'type_format': '<I'},
			'vgm_data_offset': {
				'offset': 0x34,
				'size': 4,
				'type_format': '<I',
			},
		},
		# SDM Hacked version number, we are happy enough to parse v1.51 as if it were 1.50 since the 1.51 updates dont apply to us anyway
		0x00000151: {
			'vgm_ident': {'offset': 0x00, 'size': 4, 'type_format': None},
			'eof_offset': {'offset': 0x04, 'size': 4, 'type_format': '<I'},
			'version': {'offset': 0x08, 'size': 4, 'type_format': '<I'},
			'sn76489_clock': {'offset': 0x0c, 'size': 4, 'type_format': '<I'},
			'ym2413_clock': {'offset': 0x10, 'size': 4, 'type_format': '<I'},
			'gd3_offset': {'offset': 0x14, 'size': 4, 'type_format': '<I'},
			'total_samples': {'offset': 0x18, 'size': 4, 'type_format': '<I'},
			'loop_offset': {'offset': 0x1c, 'size': 4, 'type_format': '<I'},
			'loop_samples': {'offset': 0x20, 'size': 4, 'type_format': '<I'},
			'rate': {'offset': 0x24, 'size': 4, 'type_format': '<I'},
			'sn76489_feedback': {
				'offset': 0x28,
				'size': 2,
				'type_format': '<H',
			},
			'sn76489_shift_register_width': {
				'offset': 0x2a,
				'size': 1,
				'type_format': 'B',
			},
			'ym2612_clock': {'offset': 0x2c, 'size': 4, 'type_format': '<I'},
			'ym2151_clock': {'offset': 0x30, 'size': 4, 'type_format': '<I'},
			'vgm_data_offset': {
				'offset': 0x34,
				'size': 4,
				'type_format': '<I',
			},
		},
		# SDM Hacked version number, we are happy enough to parse v1.60 as if it were 1.50 since the 1.51 updates dont apply to us anyway
		0x00000160: {
			'vgm_ident': {'offset': 0x00, 'size': 4, 'type_format': None},
			'eof_offset': {'offset': 0x04, 'size': 4, 'type_format': '<I'},
			'version': {'offset': 0x08, 'size': 4, 'type_format': '<I'},
			'sn76489_clock': {'offset': 0x0c, 'size': 4, 'type_format': '<I'},
			'ym2413_clock': {'offset': 0x10, 'size': 4, 'type_format': '<I'},
			'gd3_offset': {'offset': 0x14, 'size': 4, 'type_format': '<I'},
			'total_samples': {'offset': 0x18, 'size': 4, 'type_format': '<I'},
			'loop_offset': {'offset': 0x1c, 'size': 4, 'type_format': '<I'},
			'loop_samples': {'offset': 0x20, 'size': 4, 'type_format': '<I'},
			'rate': {'offset': 0x24, 'size': 4, 'type_format': '<I'},
			'sn76489_feedback': {
				'offset': 0x28,
				'size': 2,
				'type_format': '<H',
			},
			'sn76489_shift_register_width': {
				'offset': 0x2a,
				'size': 1,
				'type_format': 'B',
			},
			'ym2612_clock': {'offset': 0x2c, 'size': 4, 'type_format': '<I'},
			'ym2151_clock': {'offset': 0x30, 'size': 4, 'type_format': '<I'},
			'vgm_data_offset': {
				'offset': 0x34,
				'size': 4,
				'type_format': '<I',
			},
			
		},		
		# SDM Hacked version number, we are happy enough to parse v1.61 as if it were 1.50 since the 1.51 updates dont apply to us anyway
		0x00000161: {
			'vgm_ident': {'offset': 0x00, 'size': 4, 'type_format': None},
			'eof_offset': {'offset': 0x04, 'size': 4, 'type_format': '<I'},
			'version': {'offset': 0x08, 'size': 4, 'type_format': '<I'},
			'sn76489_clock': {'offset': 0x0c, 'size': 4, 'type_format': '<I'},
			'ym2413_clock': {'offset': 0x10, 'size': 4, 'type_format': '<I'},
			'gd3_offset': {'offset': 0x14, 'size': 4, 'type_format': '<I'},
			'total_samples': {'offset': 0x18, 'size': 4, 'type_format': '<I'},
			'loop_offset': {'offset': 0x1c, 'size': 4, 'type_format': '<I'},
			'loop_samples': {'offset': 0x20, 'size': 4, 'type_format': '<I'},
			'rate': {'offset': 0x24, 'size': 4, 'type_format': '<I'},
			'sn76489_feedback': {
				'offset': 0x28,
				'size': 2,
				'type_format': '<H',
			},
			'sn76489_shift_register_width': {
				'offset': 0x2a,
				'size': 1,
				'type_format': 'B',
			},
			'ym2612_clock': {'offset': 0x2c, 'size': 4, 'type_format': '<I'},
			'ym2151_clock': {'offset': 0x30, 'size': 4, 'type_format': '<I'},
			'vgm_data_offset': {
				'offset': 0x34,
				'size': 4,
				'type_format': '<I',
			},
		}
	}

	
	# constructor - pass in the filename of the VGM
	def __init__(self, vgm_filename):

		self.vgm_filename = vgm_filename
		print "  VGM file loaded : '" + vgm_filename + "'"
		
		# open the vgm file and parse it
		vgm_file = open(vgm_filename, 'rb')
		vgm_data = vgm_file.read()
		
		# Store the VGM data and validate it
		self.data = ByteBuffer(vgm_data)
		
		vgm_file.close()
		
		# parse
		self.validate_vgm_data()

		# Set up the variables that will be populated
		self.command_list = []
		self.data_block = None
		self.gd3_data = {}
		self.metadata = {}

		# Parse the VGM metadata and validate the VGM version
		self.parse_metadata()
		
		# Display info about the file
		self.vgm_loop_offset = self.metadata['loop_offset']
		self.vgm_loop_length = self.metadata['loop_samples']
		
		print "      VGM Version : " + "%x" % int(self.metadata['version'])
		print "VGM SN76489 clock : " + str(float(self.metadata['sn76489_clock'])/1000000) + " MHz"
		print "         VGM Rate : " + str(float(self.metadata['rate'])) + " Hz"
		print "      VGM Samples : " + str(int(self.metadata['total_samples'])) + " (" + str(int(self.metadata['total_samples'])/self.VGM_FREQUENCY) + " seconds)"
		print "  VGM Loop Offset : " + str(self.vgm_loop_offset)
		print "  VGM Loop Length : " + str(self.vgm_loop_length)




		# Validation to check we can parse it
		self.validate_vgm_version()

		# Sanity check this VGM is suitable for this script - must be SN76489 only
		if self.metadata['sn76489_clock'] == 0 or self.metadata['ym2413_clock'] !=0 or self.metadata['ym2413_clock'] !=0 or self.metadata['ym2413_clock'] !=0:
			raise FatalError("This script only supports VGM's for SN76489 PSG")		
		
		# see if this VGM uses Dual Chip mode
		if (self.metadata['sn76489_clock'] & 0x40000000) == 0x40000000:
			self.dual_chip_mode_enabled = True
		else:
			self.dual_chip_mode_enabled = False
			
		print "    VGM Dual Chip : " + str(self.dual_chip_mode_enabled)
		

		# override/disable dual chip commands in the output stream if required
		if (self.disable_dual_chip == True) and (self.dual_chip_mode_enabled == True) :
			# remove the clock flag that enables dual chip mode
			self.metadata['sn76489_clock'] = self.metadata['sn76489_clock'] & 0xbfffffff
			self.dual_chip_mode_enabled = False
			print "Dual Chip Mode Disabled - DC Commands will be removed"

		# take a copy of the clock speed for the VGM processor functions
		self.vgm_source_clock = self.metadata['sn76489_clock']
		self.vgm_target_clock = self.vgm_source_clock
		
		# Parse GD3 data and the VGM commands
		self.parse_gd3()
		self.parse_commands()
		
		print "   VGM Commands # : " + str(len(self.command_list))
		print ""


	def validate_vgm_data(self):
		# Save the current position of the VGM data
		original_pos = self.data.tell()

		# Seek to the start of the file
		self.data.seek(0)

		# Perform basic validation on the given file by checking for the VGM
		# magic number ('Vgm ')
		if self.data.read(4) != self.vgm_magic_number:
			# Could not find the magic number. The file could be gzipped (e.g.
			# a vgz file). Try un-gzipping the file and trying again.
			self.data.seek(0)
			self.data = gzip.GzipFile(fileobj=self.data, mode='rb')

			try:
				if self.data.read(4) != self.vgm_magic_number:
					print "Error: Data does not appear to be a valid VGM file"
					raise ValueError('Data does not appear to be a valid VGM file')
			except IOError:
				print "Error: Data does not appear to be a valid VGM file"
				# IOError will be raised if the file is not a valid gzip file
				raise ValueError('Data does not appear to be a valid VGM file')

		# Seek back to the original position in the VGM data
		self.data.seek(original_pos)
		
	def parse_metadata(self):
		# Save the current position of the VGM data
		original_pos = self.data.tell()

		# Create the list to store the VGM metadata
		self.metadata = {}

		# Iterate over the offsets and parse the metadata
		for version, offsets in self.metadata_offsets.items():
			for value, offset_data in offsets.items():

				# Seek to the data location and read the data
				self.data.seek(offset_data['offset'])
				data = self.data.read(offset_data['size'])

				# Unpack the data if required
				if offset_data['type_format'] is not None:
					self.metadata[value] = struct.unpack(
						offset_data['type_format'],
						data,
					)[0]
				else:
					self.metadata[value] = data

		# Seek back to the original position in the VGM data
		self.data.seek(original_pos)

	def validate_vgm_version(self):
		if self.metadata['version'] not in self.supported_ver_list:
			print "VGM version is not supported"
			raise FatalError('VGM version is not supported')

	def parse_gd3(self):
		# Save the current position of the VGM data
		original_pos = self.data.tell()

		# Seek to the start of the GD3 data
		self.data.seek(
			self.metadata['gd3_offset'] +
			self.metadata_offsets[self.metadata['version']]['gd3_offset']['offset']
		)

		# Skip 8 bytes ('Gd3 ' string and 4 byte version identifier)
		self.data.seek(8, 1)

		# Get the length of the GD3 data, then read it
		gd3_length = struct.unpack('<I', self.data.read(4))[0]
		gd3_data = ByteBuffer(self.data.read(gd3_length))

		# Parse the GD3 data
		gd3_fields = []
		current_field = b''
		while True:
			# Read two bytes. All characters (English and Japanese) in the GD3
			# data use two byte encoding
			char = gd3_data.read(2)

			# Break if we are at the end of the GD3 data
			if char == b'':
				break

			# Check if we are at the end of a field, if not then continue to
			# append to "current_field"
			if char == b'\x00\x00':
				gd3_fields.append(current_field)
				current_field = b''
			else:
				current_field += char

		# Once all the fields have been parsed, create a dict with the data
		# some Gd3 tags dont have notes section
		gd3_notes = ''
		gd3_title_eng = basename(self.vgm_filename).encode("utf_16")
		if len(gd3_fields) > 10:
			gd3_notes = gd3_fields[10]
			
		if len(gd3_fields) > 8:
		
			if len(gd3_fields[0]) > 0:
				gd3_title_eng = gd3_fields[0]

				
			self.gd3_data = {
				'title_eng': gd3_title_eng,
				'title_jap': gd3_fields[1],
				'game_eng': gd3_fields[2],
				'game_jap': gd3_fields[3],
				'console_eng': gd3_fields[4],
				'console_jap': gd3_fields[5],
				'artist_eng': gd3_fields[6],
				'artist_jap': gd3_fields[7],
				'date': gd3_fields[8],
				'vgm_creator': gd3_fields[9],
				'notes': gd3_notes
			}		
		else:
			print "WARNING: Malformed/missing GD3 tag"
			self.gd3_data = {
				'title_eng': gd3_title_eng,
				'title_jap': '',
				'game_eng': '',
				'game_jap': '',
				'console_eng': '',
				'console_jap': '',
				'artist_eng': 'Unknown'.encode("utf_16"),
				'artist_jap': '',
				'date': '',
				'vgm_creator': '',
				'notes': ''
			}				


		# Seek back to the original position in the VGM data
		self.data.seek(original_pos)

	#-------------------------------------------------------------------------------------------------

	def parse_commands(self):
		# Save the current position of the VGM data
		original_pos = self.data.tell()

		# Seek to the start of the VGM data
		self.data.seek(
			self.metadata['vgm_data_offset'] +
			self.metadata_offsets[self.metadata['version']]['vgm_data_offset']['offset']
		)

		while True:
			# Read a byte, this will be a VGM command, we will then make
			# decisions based on the given command
			command = self.data.read(1)

			# Break if we are at the end of the file
			if command == '':
				break

			# 0x4f dd - Game Gear PSG stereo, write dd to port 0x06
			# 0x50 dd - PSG (SN76489/SN76496) write value dd
			if command in [b'\x4f', b'\x50']:
				self.command_list.append({
					'command': command,
					'data': self.data.read(1),
				})

			# 0x51 aa dd - YM2413, write value dd to register aa
			# 0x52 aa dd - YM2612 port 0, write value dd to register aa
			# 0x53 aa dd - YM2612 port 1, write value dd to register aa
			# 0x54 aa dd - YM2151, write value dd to register aa
			elif command in [b'\x51', b'\x52', b'\x53', b'\x54']:
				self.command_list.append({
					'command': command,
					'data': self.data.read(2),
				})

			# 0x61 nn nn - Wait n samples, n can range from 0 to 65535
			elif command == b'\x61':
				self.command_list.append({
					'command': command,
					'data': self.data.read(2),
				})

			# 0x62 - Wait 735 samples (60th of a second)
			# 0x63 - Wait 882 samples (50th of a second)
			# 0x66 - End of sound data
			elif command in [b'\x62', b'\x63', b'\x66']:
				self.command_list.append({'command': command, 'data': None})

				# Stop processing commands if we are at the end of the music
				# data
				if command == b'\x66':
					break

			# 0x67 0x66 tt ss ss ss ss - Data block
			elif command == b'\x67':
				# Skip the compatibility and type bytes (0x66 tt)
				self.data.seek(2, 1)

				# Read the size of the data block
				data_block_size = struct.unpack('<I', self.data.read(4))[0]

				# Store the data block for later use
				self.data_block = ByteBuffer(self.data.read(data_block_size))

			# 0x7n - Wait n+1 samples, n can range from 0 to 15
			# 0x8n - YM2612 port 0 address 2A write from the data bank, then
			#        wait n samples; n can range from 0 to 15
			elif b'\x70' <= command <= b'\x8f':
				self.command_list.append({'command': command, 'data': None})

			# 0xe0 dddddddd - Seek to offset dddddddd (Intel byte order) in PCM
			#                 data bank
			elif command == b'\xe0':
				self.command_list.append({
					'command': command,
					'data': self.data.read(4),
				})
				
			# 0x30 dd - dual chip command
			elif command == b'\x30':
				if self.dual_chip_mode_enabled:
					self.command_list.append({
						'command': command,
						'data': self.data.read(1),
					})
			

		# Seek back to the original position in the VGM data
		self.data.seek(original_pos)


	#-------------------------------------------------------------------------------------------------
	
	# returns bytearray containing the raw data version of the vgm
	def as_binary(self, rawheader = True):
		print "   VGM Processing : Output binary file "

		byte_size = 1
		packet_size = 0
		play_rate = self.metadata['rate']
		play_interval = self.VGM_FREQUENCY / play_rate
		data_block = bytearray()
		packet_block = bytearray()

		packet_count = 0

		# emit the packet data
		for q in self.command_list:
			
			command = q["command"]
			if command != struct.pack('B', 0x50):
			
				# non-write command, so flush any pending packet data
				if self.VERBOSE: print "Packet length " + str(len(packet_block))

				data_block.append(struct.pack('B', len(packet_block)))
				data_block.extend(packet_block)
				packet_count += 1
				
				#if packet_count > 30*play_rate:
				#	break

				# start new packet
				packet_block = bytearray()
				
				if self.VERBOSE: print "Command " + str(binascii.hexlify(command))
				
				

				# see if command is a wait longer than one interval and emit empty packets to compensate
				wait = 0
				if command == struct.pack('B', 0x61):
					t = int(binascii.hexlify(q["data"]), 16)
					wait = ((t & 255) * 256) + (t>>8)
				else:
					if command == struct.pack('B', 0x62):
						wait = 735
					else:
						if command == struct.pack('B', 0x63):
							wait = 	882
					
				if wait != 0:	
					intervals = wait / (self.VGM_FREQUENCY / play_rate)
					if intervals == 0:
						print "ERROR in data stream, wait value (" + str(wait) + ") was not divisible by play_rate (" + str((self.VGM_FREQUENCY / play_rate)) + "), bailing"
						return
					else:
						if self.VERBOSE: print "WAIT " + str(intervals) + " intervals"
						
					# emit empty packet headers to simulate wait commands
					intervals -= 1
					while intervals > 0:
						data_block.append(0)
						if self.VERBOSE: print "Packet length 0"
						intervals -= 1
						packet_count += 1

				
				
			else:
				if self.VERBOSE: print "Data " + str(binascii.hexlify(command))			
				packet_block.extend(q['data'])

		# eof
		data_block.append(0x00)	# append one last wait
		data_block.append(0xFF)	# signal EOF


		header_block = bytearray()
		# emit the play rate
		print "play rate is " + str(play_rate)
		header_block.append(struct.pack('B', play_rate & 0xff))
		header_block.append(struct.pack('B', packet_count & 0xff))		
		header_block.append(struct.pack('B', (packet_count >> 8) & 0xff))	

		print "    Num packets " + str(packet_count)
		duration = packet_count / play_rate
		duration_mm = int(duration / 60.0)
		duration_ss = int(duration % 60.0)
		print "    Song duration " + str(duration) + " seconds, " + str(duration_mm) + "m" + str(duration_ss) + "s"
		header_block.append(struct.pack('B', duration_mm))	# minutes		
		header_block.append(struct.pack('B', duration_ss))	# seconds

		# output the final byte stream
		output_block = bytearray()	

		# send header
		output_block.append(struct.pack('B', len(header_block)))
		output_block.extend(header_block)

		# send title
		title = self.gd3_data['title_eng'].decode("utf_16")
		title = title.encode('ascii', 'ignore')

		if len(title) > 254:
			title = title[:254]
		output_block.append(struct.pack('B', len(title) + 1))	# title string length
		output_block.extend(title)
		output_block.append(struct.pack('B', 0))				# zero terminator

		# send author
		author = self.gd3_data['artist_eng'].decode("utf_16")
		author = author.encode('ascii', 'ignore')

		# use filename if no author listed
		if len(author) == 0:
			author = basename(self.vgm_filename)

		if len(author) > 254:
			author = author[:254]
		output_block.append(struct.pack('B', len(author) + 1))	# author string length
		output_block.extend(author)
		output_block.append(struct.pack('B', 0))				# zero terminator

		# send data with or without header
		if rawheader:
			output_block.extend(data_block)
		else:
			output_block = data_block

		# write file
		print "Compressed VGM is " + str(len(output_block)) + " bytes long"

		return output_block





#----------------------------------------------------------
# Utilities
#----------------------------------------------------------


# split the packed raw data into 11 separate streams
# returns array of 11 bytearrays
def split_raw(rawData, stripCommands = True):

	registers = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
	registers_opt = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

	latched_channel = -1

	output_block = bytearray()
	output_blocks = []

	for o in range(11):
		output_blocks.append( bytearray() )

	if stripCommands:
		register_mask = 15
	else:
		register_mask = 255

	# unpack the raw binary data in 11 arrays of register data without any deltas between them
	# eg. the raw chip writes to all 11 registers every frame
	n = 0
	Packet = True
	verbose = False

	while (Packet):
		packet_size = rawData[n]
		if verbose:
			print("packet_size=" + str(packet_size))
		n += 1
		if packet_size == 255:
			Packet = False
		else:
			for x in range(packet_size):
				d = rawData[n+x]
				#if verbose:
				#   print "  frame byte number=" +str(x)
				#   print "    frame byte=" +str(d)
				if d & 128:
					# latch
					c = (d>>5)&3
					latched_channel = c
					if d & 16:
						# volume
						if verbose:
							print(" volume on channel " + str(c))
						registers[c+7] = d & register_mask

					else:
						# tone
						if verbose:
							print(" tone on channel " + str(c))

						registers[c*2+0] = d & register_mask                    

				else:
					if verbose:
						print(" tone data on latched channel " + str(latched_channel))
					registers[latched_channel*2+1] = d & 63 # tone data only contains 6 bits of info anyway, so no need for mask
					if latched_channel == 3:
						print("ERROR CHANNEL")





			# emit current state of each of the 11 registers to 11 different bytearrays
			for x in range(11):
				output_blocks[x].append( registers[x] )

			 # next packet                
			n += packet_size

	#print(output_blocks[6])

	#IGNORE we no longer do this - let the decoder do it instead.
	if False:
		# make sure we only emit tone3 when it changes, or 15 for no-change
		# this prevents the LFSR from being reset
		lastTone3 = 255  
		for x in range(len(output_blocks[6])):
			t = output_blocks[6][x]
			if t == lastTone3:
				output_blocks[6][x] = 15
			lastTone3 = t

	#    print(output_blocks[6])

	# Add EOF marker (0x08) to tone3 byte stream
	output_blocks[6].append(0x08)	# 0x08 is an invalid noise tone.

	# return the split blocks
	return output_blocks



# return string with byte overhead of n blocks on decoder side
def overhead(n):
	return " (" + str(n*lz4.getWindowSize()) + " bytes overhead)"

# report stats from block_in and block_out compression ration, block_count indicates overhead, and msg is the description
def report(block_in, block_out, block_count, msg=""):
	
	src_size = len(block_in)
	dst_size = len(block_out)
	if src_size == 0:
		ratio = 0
	else:
		ratio = 100 - (int)((dst_size*100 / src_size))

	ws = lz4.getWindowSize()
	if ws < 1024:
		window_size = str(ws) + "b"
	else:
		window_size = str(ws>>10) + "Kb"

	# outputs with multiple blocks will have overhead on decoder side
	overhead = block_count * lz4.getWindowSize()
	total_size = dst_size + overhead

	msg = "{:87}".format(msg)
	print(" Compressed '" + msg + "', " + str(src_size) + " into " + str(dst_size) + " bytes => " + str(ratio) + "%, level=" + str(lz4.getCompressionLevel()) + ", window=" + window_size + ", overhead=" + str(overhead) + ", size=" + str(total_size) + ", tokens=" + str(lz4.stats["tokenCount"]))


# from the 11 registers array, return a new byte array which is all 11 registers sets combined into one buffer
def combine_parts(registers):
	buffer = bytearray()
	for x in range(len(registers)):
		buffer += registers[x]
	return buffer

# from the 11 registers array, return a new bytearray of the registers combined from the given array
#  where combination is an array, eg. [0,3,1]
def combine_registers(registers, combination):
	buffer = bytearray()
	for x in range(len(registers[0])):
		for y in range(len(combination)):
			r = combination[y]
			buffer.append( registers[r][x] )
	return buffer




# given a block of bytes of 4-bit values, compress two bytes to 1
def pack4(block):
	packed_block = bytearray()

	for x in range(0, len(block), 2):
		a = block[x+0] & 15
		if x+1 >= len(block):
			b = 0
		else:
			b = block[x+1] & 15
		c = (a << 4) + b
		packed_block.append(c)
	return packed_block

# given a block of bytes, return a new version with 'marker' replacing bytes that are unchanged
# assumed 8-bit data series on input. used for tone3 differentials to prevent LFSR reset.
def diff(block, marker = 255):
	input_block = block
	diff_block = bytearray()
	for n in range(len(input_block)):
		if n == 0:
			diff_block.append(input_block[0])
		else:
			if input_block[n] == input_block[n-1]:
				diff_block.append(marker)
			else:
				diff_block.append(input_block[n])
	return diff_block

# given a block of bytes, return a new version with deltas applied to each byte
def delta(block):
	input_block = block
	diff_block = bytearray()
	for n in range(1, len(input_block)):
		a = input_block[n-1]
		b = input_block[n]
		diff_block.append( (b-a) & 255 )
	return diff_block


# apply simple RLE encoding to a block of 4-bit tone or volume data
# run length encoded into top 4-bits. 0=no repeat, 15=15 repeats.
def rle(block):
	#return block
	if not RLE:
		return block

	rle_block = bytearray()
	n = 0
	while (n < len(block)):
		#print('offset ' + str(n))
		offset = n
		count = 0
		while ((offset < len(block)-1) and (count < 15)):
			#print('diff[' + str(offset+1) + ']='+str(block[offset+1]))
			if block[offset+1] == block[n]:
				count += 1 
				offset += 1
			else:
				#print('ack')
				break

		out = ((count&15)<<4) | (block[n] & 15)
		rle_block.append( out )
		n += count + 1
		#if count > 0:
		#		print('run length ' + str(count) + " of " + format(out, 'x'))


	# test unpack
	test = bytearray()
	for n in rle_block:
		count = n>>4
		token = n & 15
		#print("byte=" + format(n, "x") + ", count=" + str(count) + ", token=" + str(token))
		for l in range(count+1):
			test.append(token)

	if len(test) != len(block):	
		print("ERROR: output size fault after RLE, testblocksize=" + str(len(test)) + ", inblocksize=" + str(len(block)))
	
	for j in range(len(block)):
		if test[j] != block[j]:
			print("ERROR: difference at offset=" + str(j) + " expected=" + format(block[j],'x') + ", got " + format(test[j],'x'))

	assert test == block

	print('   RLE Pack size in=' + str(len(block)) + ', out=' + str(len(rle_block)) + ", saving=" + str(len(block)-len(rle_block)) )
	return rle_block

# apply simple RLE encoding to a block of 12-bit tone data (stored as 16-bit words)
# run length encoded into top 4-bits. 0=no repeat, 15=15 repeats.
def rle2(block):

	if not RLE:
		return block

	rle_block = bytearray()
	n = 0
	while (n < len(block)):
		#print('offset ' + str(n))
		offset = n
		count = 0
		while ((offset < len(block)-2) and (count < 15)):
			if block[offset+2] == block[n] and block[offset+3] == block[n+1]:
				count += 1 
				offset += 2
			else:
				break

		out = (block[n]<<8) + block[n+1]
		out |= ((count&15)<<12)
		rle_block.append( (out>>8) & 255 )
		rle_block.append( out & 255 )

		n += count*2 + 2
		#if count > 0:
		#		print('run length ' + str(count) + " of " + format(out, 'x'))

	# test unpack
	test = bytearray()
	for i in range(0, len(rle_block), 2):
		n = rle_block[i]
		count = n>>4
		token = n & 15
		#print("byte=" + format(n, "x") + ", count=" + str(count) + ", token=" + str(token))
		for l in range(count+1):
			test.append(token)
			test.append(rle_block[i+1])

	if len(test) != len(block):	
		print("ERROR: output size fault after RLE, testblocksize=" + str(len(test)) + ", inblocksize=" + str(len(block)))
	
	for j in range(len(block)):
		if test[j] != block[j]:
			print("ERROR: difference at offset=" + str(j) + " expected=" + format(block[j],'x') + ", got " + format(test[j],'x'))

	assert test == block


	print('   RLE Pack in=' + str(len(block)) + ', out=' + str(len(rle_block)) + ", saving=" + str(len(block)-len(rle_block)) )
	return rle_block



def frequencies(showData):
	tokens = lz4.stats["tokens"]
	offsets = lz4.stats["offsets"]
	lengths = lz4.stats["lengths"]

	token_dict = {}
	offsets_dict = {}
	lengths_dict = {}

	for t in tokens:
		if t in token_dict:
			token_dict[t] += 1
		else:
			token_dict[t] = 1

	for o in offsets:
		if o in offsets_dict:
			offsets_dict[o] += 1
		else:
			offsets_dict[o] = 1


	for l in lengths:
		if l in lengths_dict:
			lengths_dict[l] += 1
		else:
			lengths_dict[l] = 1

	print("    tokenCount=" + str(lz4.stats["tokenCount"]))
	print(" largestOffset=" + str(lz4.stats["largestOffset"]))
	print(" largestLength=" + str(lz4.stats["largestLength"]))

	print(" There are " + str(len(token_dict)) + " unique tokens.")
	if showData:
		sorted_dict = sorted(token_dict.items(), key=operator.itemgetter(1))
		print(sorted_dict)

	print(" There are " + str(len(offsets_dict)) + " unique offsets.")
	if showData:
		sorted_dict = sorted(offsets_dict.items(), key=operator.itemgetter(1))
		print(sorted_dict)

	print(" There are " + str(len(lengths_dict)) + " unique match lengths.")
	if showData:
		sorted_dict = sorted(lengths_dict.items(), key=operator.itemgetter(1))
		print(sorted_dict)



# given an array of data points, serialize it to a bytearray
# size is the number of bytes to be used to represent each element in the source array.
def toByteArray(array, size = 1):
	r = bytearray()
	for v in array:
		if size < 2:
			r.append(v & 255)
		else:
			r.append(v & 255)
			r.append(v >> 8)
	return r




#------------------------------------------------------------------------
# Main()
#------------------------------------------------------------------------

import argparse


argv = sys.argv
filename = argv[1]



# load the VGM file, or alternatively interpret as a binary
if filename.lower()[-4:] == ".vgm":
	vgm = VgmStream(filename)
	data_block = vgm.as_binary()
else:
	fh = open(filename, 'rb')
	data_block = bytearray(fh.read())
	fh.close()	

data_offset = 0

# parse the header
header_size = data_block[0]       # header size
play_rate = data_block[1]       # play rate

if header_size == 5 and play_rate == 50:
	packet_count = data_block[2] + data_block[3]*256       # packet count LO
	duration_mm = data_block[4]       # duration mm
	duration_ss = data_block[5]       # duration ss
	
	data_offset = header_size+1
	data_offset += data_block[data_offset]+1
	data_offset += data_block[data_offset]+1


	print("header_size=" +str(header_size))
	print("play_rate="+str(play_rate))
	print("packet_count="+str(packet_count))
	print("duration_mm="+str(duration_mm))
	print("duration_ss="+str(duration_ss))
	print("data_offset="+str(data_offset))
else:
	print("No header.")

print("")

# Trim off the header data. The rest is raw data.
data_block = data_block[data_offset:]

#----------------------------------------------------------
# Begin VGM packer suite
#----------------------------------------------------------

# Ok the definitive packed VGM format is:
# 1. Register data split into 8 streams, 3x 16-bit tones, 1x 8-bit channel3 tones 4x 8-bit volumes.
# 2. Register command bits are stripped
# 3. Channel3 tone stream replaces runs with 0x0F to signal no change, plus 0x08 is appended as an EOF marker
# 4. All 8 streams are RLE compressed, using top 4bits as run length
# 5. Output stream is LZ4 frame/block format
# 6. All 8 streams are LZ4 compressed using 255 match distance and 8-bit offsets at maximum optimal parser setting
# 7. All 8 streams are optionally huffman compressed
# 8. The LZ4 magic number is altered from [04 22 4d 18] to [56 47 43 00] (so that it is no longer seen as LZ4 compatible) [byte 3 bit6=1=LZ4-16bit, =0=LZ4-8bit]
# 9. If huffman is applied, the magic number is [56 47 43 80] [byte 3 bit7=1=+Huffman]
# We might be able to support 16-bit offsets later. WIP/TODO. Magic number would be [56 47 43 40] (plain LZ4) or [56 47 43 C0] with huffman

# pack options
HIGH_COMPRESSION = False # enable 2kb sliding window with 16-bits instead of 255 byte, overridden by LZ48
LZ48 = True	# enable 8 bit LZ4 mode
OUTPUT_RAWDATA = False # output raw dumps of the data that was compressed by LZ4/Huffman
RLE = True # always set now.
ENABLE_HUFFMAN = True # optional

lz4 = LZ4()
level = 9
#window = 255 # this is for 8-bit machines after all
lz4.setCompression(level)#, window)
# enable the high compression mode
if LZ48:
	lz4.optimizedCompression(True)
else:
	# high compression mode, requires 16Kb workspace but crunches like a boss.
	if HIGH_COMPRESSION: 
		windowsize = 2048
		lz4.setCompression(level, windowsize)
		lz4.optimizedCompression(False)




#----------------------------------------------------------
# Unpack the register data into 11 separate data streams
#----------------------------------------------------------
registers = split_raw(data_block, True)

#------------------------------------------------------------------------------
# Construct the optimal VGC file format output
#------------------------------------------------------------------------------

# Step 1 - reformat the register data streams
streams = []
streams.append( rle2( combine_registers(registers, [0, 1]) ) ) # tone0 HI/LO
streams.append( rle2( combine_registers(registers, [2, 3]) ) ) # tone1 HI/LO
streams.append( rle2( combine_registers(registers, [4, 5]) ) ) # tone2 HI/LO
streams.append( rle( diff( registers[6], 0x0f ) ) ) # tone3 (is diffed also so we create skip commands - 0x0f)
streams.append( rle( registers[7] ) ) # v0
streams.append( rle( registers[8] ) ) # v1
streams.append( rle( registers[9] ) ) # v2
streams.append( rle( registers[10] ) ) # v3

if OUTPUT_RAWDATA:
	# write a raw data version of the file in the most optimal data format
	# (so we can see how other compressors compare with it)
	count = 0
	for s in streams:
		open(filename+"." + str(count) + ".part", "wb").write( s )
		count += 1

# Step 2 - LZ4 compress these streams

# Output the LZ4 frame header
output = bytearray()
lz4.beginFrame(output)

# re-write LZ4 magic number if incompatible
if LZ48 or ENABLE_HUFFMAN:
	n = 0x00
	if ENABLE_HUFFMAN:
		n |= 0x80
	output[0] = 0x56
	output[1] = 0x47
	output[2] = 0x43
	output[3] = n

# LZ4 Compress the 8 data streams
for i in range(len(streams)):
	streams[i] = lz4.compressBlock( streams[i] )


# Step 3 - Huffcode these streams (optional - better ratio, lower decoder performance)
if ENABLE_HUFFMAN:

	huffman = Huffman()
	
	# our decoder only supports upto 16-bit codes.
	huffman.MAX_CODE_BIT_LENGTH = 16

	# analyse the compressed data stream
	compressed_data = bytearray()
	for s in streams:
		compressed_data += s[4:] # skip block headers so we dont add unwanted symbols to the alphabet
	# build the optimal code tree
	huffman.build(compressed_data)

	# Create an uncompressed huffman table LZ4 block
	header_block = huffman.addHeader(bytearray(), bytearray())
	lz4.setCompression(0)
	output += lz4.compressBlock( header_block )

	# Emit huffman encoded blocks as uncompressed LZ4 blocks
	for i in range(len(streams)):
		s = streams[i][4:]
		huffdata = huffman.encode( s, header = False ) # we skip the first 4 bytes of the LZ4 block (the block header)
		print('   HUF Pack in=' + str(len(s)) + ', out=' + str(len(huffdata)) + ", saving=" + str(len(s)-len(huffdata)) )

		streams[i] = lz4.compressBlock( huffdata )

# Step 4 - Serialise the blocks
for s in streams:
	output += s

# Step 5 - write the output file
lz4.endFrame(output)
report(data_block, output, 8, "Paired 8 register blocks [01][23][45][6][7][8][9][A] WITH register masks ")

ofilename = filename+".vgc"

# write the lz4 compressed file.
open(ofilename, "wb").write( output )
