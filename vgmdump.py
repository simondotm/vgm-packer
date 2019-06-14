#!/usr/bin/env python
# vgmdump.py
# Tool for dumping the contents of a VGM file in an interleaved raw register dump format
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

import functools
import itertools
import struct
import sys
import time
import binascii
import math
import operator
import os

from modules.vgmparser import VgmStream


class VgmDump:

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
						registers[latched_channel*2+1] = d # we no longer do any masking here # d & 63 # tone data only contains 6 bits of info anyway, so no need for mask
						if latched_channel == 3:
							print("ERROR CHANNEL")





				# emit current state of each of the 11 registers to 11 different bytearrays
				for x in range(11):
					output_blocks[x].append( registers[x] )

				# next packet                
				n += packet_size

		# Add EOF marker (0x08) to tone3 byte stream
		output_blocks[6].append(0x08)	# 0x08 is an invalid noise tone.

		# return the split blocks
		return output_blocks


	# from the 11 registers array, return a new bytearray of the registers combined from the given array
	#  where combination is an array, eg. [0,3,1]
	def combine_registers(self, registers, combination):
		buffer = bytearray()
		for x in range(len(registers[0])):
			for y in range(len(combination)):
				r = combination[y]
				buffer.append( registers[r][x] )
		return buffer

	# from the 11 registers array, return a new byte array which is all 11 registers sets combined into one buffer
	def combine_parts(self, registers):
		buffer = bytearray()
		for x in range(len(registers)):
			buffer += registers[x]
		return buffer

	#----------------------------------------------------------
	# Process(filename)
	# Convert the given VGM file to a dump file
	#----------------------------------------------------------
	def process(self, src_filename, dst_filename, add_header = False):

		# load the VGM file, or alternatively interpret as a binary
		if src_filename.lower()[-4:] == ".vgm":
			vgm = VgmStream(src_filename)
			data_block = vgm.as_binary()
		else:
			fh = open(src_filename, 'rb')
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

		# stash the header
		header = data_block[:data_offset]

		# Trim off the header data. The rest is raw data.
		data_block = data_block[data_offset:]


		#----------------------------------------------------------
		# Begin VGM dump
		#----------------------------------------------------------


		#----------------------------------------------------------
		# Unpack the register data into 11 separate data streams
		#----------------------------------------------------------
		registers = self.split_raw(data_block, False)
		dump = self.combine_registers(registers, [0,1,2,3,4,5,6,7,8,9,10])

		# write a raw data version of the file 
		fh = open(dst_filename, "wb")
		if add_header:
			print("Writing Header...")
			fh.write(header)

		fh.write( dump )


#------------------------------------------------------------------------
# Main()
#------------------------------------------------------------------------

import argparse



# Determine if running as a script
if __name__ == '__main__':

	print("VgmDump.py : VGM file dump utility")
	print("Written in 2019 by Simon Morris, https://github.com/simondotm/vgm-packer")
	print("")

	epilog_string = ""

	parser = argparse.ArgumentParser(
		formatter_class=argparse.RawDescriptionHelpFormatter,
		epilog=epilog_string)

	parser.add_argument("input", help="VGM source file (must be single SN76489 PSG format) [input]")
	parser.add_argument("-o", "--output", metavar="<output>", help="write VGC file <output> (default is '[input].raw')")
	parser.add_argument("-m", "--meta", help="Output metadata header", action="store_true")
	parser.add_argument("-v", "--verbose", help="Enable verbose mode", action="store_true")
	args = parser.parse_args()


	src = args.input
	dst = args.output
	if dst == None:
		dst = os.path.splitext(src)[0] + ".raw"

	# check for missing files
	if not os.path.isfile(src):
		print("ERROR: File '" + src + "' not found")
		sys.exit()

	dumper = VgmDump()
	dumper.VERBOSE = args.verbose
	dumper.process(src, dst, args.meta)



