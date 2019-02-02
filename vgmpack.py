#!/usr/bin/env python
# Experiments with compressing SN76489 based VGM data

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

from lz4enc import LZ4 
from huffman import Huffman



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

