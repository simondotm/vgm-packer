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

from modules.lz4enc import LZ4 
from modules.huffman import Huffman
from modules.vgmparser import VgmStream

class VgmPacker:

	# pack options
	HIGH_COMPRESSION = False # enable 2kb sliding window with 16-bits instead of 255 byte, overridden by LZ48
	LZ48 = True	# enable 8 bit LZ4 mode
	OUTPUT_RAWDATA = False # output raw dumps of the data that was compressed by LZ4/Huffman
	RLE = True # always set now.
	ENABLE_HUFFMAN = True # optional


	def __init__(self):
		print("init")

			
	#----------------------------------------------------------
	# Utilities
	#----------------------------------------------------------


	# split the packed raw data into 11 separate streams
	# returns array of 11 bytearrays
	def split_raw(self, rawData, stripCommands = True):

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
	def overhead(self, n):
		return " (" + str(n*lz4.getWindowSize()) + " bytes overhead)"

	# report stats from block_in and block_out compression ration, block_count indicates overhead, and msg is the description
	def report(self, lz4, block_in, block_out, block_count, msg=""):
		
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
	def combine_parts(self, registers):
		buffer = bytearray()
		for x in range(len(registers)):
			buffer += registers[x]
		return buffer

	# from the 11 registers array, return a new bytearray of the registers combined from the given array
	#  where combination is an array, eg. [0,3,1]
	def combine_registers(self, registers, combination):
		buffer = bytearray()
		for x in range(len(registers[0])):
			for y in range(len(combination)):
				r = combination[y]
				buffer.append( registers[r][x] )
		return buffer




	# given a block of bytes of 4-bit values, compress two bytes to 1
	def pack4(self, block):
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
	def diff(self, block, marker = 255):
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
	def delta(self, block):
		input_block = block
		diff_block = bytearray()
		for n in range(1, len(input_block)):
			a = input_block[n-1]
			b = input_block[n]
			diff_block.append( (b-a) & 255 )
		return diff_block


	# apply simple RLE encoding to a block of 4-bit tone or volume data
	# run length encoded into top 4-bits. 0=no repeat, 15=15 repeats.
	def rle(self, block):
		#return block
		if not self.RLE:
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
	def rle2(self, block):

		if not self.RLE:
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



	def frequencies(self, showData):
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
	def toByteArray(self, array, size = 1):
		r = bytearray()
		for v in array:
			if size < 2:
				r.append(v & 255)
			else:
				r.append(v & 255)
				r.append(v >> 8)
		return r




	#----------------------------------------------------------
	# Process(filename)
	# Convert the given VGM file to a compressd VGC file
	#----------------------------------------------------------
	def process(self, filename):

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



		lz4 = LZ4()
		level = 9
		#window = 255 # this is for 8-bit machines after all
		lz4.setCompression(level)#, window)
		# enable the high compression mode
		if self.LZ48:
			lz4.optimizedCompression(True)
		else:
			# high compression mode, requires 16Kb workspace but crunches like a boss.
			if self.HIGH_COMPRESSION: 
				windowsize = 2048
				lz4.setCompression(level, windowsize)
				lz4.optimizedCompression(False)




		#----------------------------------------------------------
		# Unpack the register data into 11 separate data streams
		#----------------------------------------------------------
		registers = self.split_raw(data_block, True)

		#------------------------------------------------------------------------------
		# Construct the optimal VGC file format output
		#------------------------------------------------------------------------------

		# Step 1 - reformat the register data streams
		streams = []
		streams.append( self.rle2( self.combine_registers( registers, [0, 1]) ) ) # tone0 HI/LO
		streams.append( self.rle2( self.combine_registers( registers, [2, 3]) ) ) # tone1 HI/LO
		streams.append( self.rle2( self.combine_registers( registers, [4, 5]) ) ) # tone2 HI/LO
		streams.append( self.rle( self.diff( registers[6], 0x0f ) ) ) # tone3 (is diffed also so we create skip commands - 0x0f)
		streams.append( self.rle( registers[7] ) ) # v0
		streams.append( self.rle( registers[8] ) ) # v1
		streams.append( self.rle( registers[9] ) ) # v2
		streams.append( self.rle( registers[10] ) ) # v3

		if self.OUTPUT_RAWDATA:
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
		if self.LZ48 or self.ENABLE_HUFFMAN:
			n = 0x00
			if self.ENABLE_HUFFMAN:
				n |= 0x80
			output[0] = 0x56
			output[1] = 0x47
			output[2] = 0x43
			output[3] = n

		# LZ4 Compress the 8 data streams
		for i in range(len(streams)):
			streams[i] = lz4.compressBlock( streams[i] )


		# Step 3 - Huffcode these streams (optional - better ratio, lower decoder performance)
		if self.ENABLE_HUFFMAN:

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
		self.report(lz4, data_block, output, 8, "Paired 8 register blocks [01][23][45][6][7][8][9][A] WITH register masks ")

		ofilename = filename+".vgc"

		# write the lz4 compressed file.
		open(ofilename, "wb").write( output )


#------------------------------------------------------------------------
# Main()
#------------------------------------------------------------------------

import argparse


argv = sys.argv
filename = argv[1]

packer = VgmPacker()
packer.process(filename)
