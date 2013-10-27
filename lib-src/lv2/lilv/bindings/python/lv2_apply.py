#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import lilv
import sys
import wave
import numpy

# Read command line arguments
if len(sys.argv) != 4:
    print 'USAGE: lv2_apply.py PLUGIN_URI INPUT_WAV OUTPUT_WAV'
    sys.exit(1)

# Initialise Lilv
world = lilv.World()
world.load_all()

plugin_uri   = world.new_uri(sys.argv[1])
wav_in_path  = sys.argv[2]
wav_out_path = sys.argv[3]


# Find plugin
plugin = world.get_all_plugins().get_by_uri(plugin_uri)
if not plugin:
    print "Unknown plugin `%s'\n" % plugin_uri
    sys.exit(1)

lv2_InputPort  = world.new_uri(lilv.LILV_URI_INPUT_PORT)
lv2_OutputPort = world.new_uri(lilv.LILV_URI_OUTPUT_PORT)
lv2_AudioPort  = world.new_uri(lilv.LILV_URI_AUDIO_PORT)

n_audio_in  = plugin.get_num_ports_of_class(lv2_InputPort,  lv2_AudioPort)
n_audio_out = plugin.get_num_ports_of_class(lv2_OutputPort, lv2_AudioPort)
if n_audio_out == 0:
    print "Plugin has no audio outputs\n"
    sys.exit(1)

# Open input file
wav_in = wave.open(wav_in_path, 'r')
if not wav_in:
    print "Failed to open input `%s'\n" % wav_in_path
    sys.exit(1)
if wav_in.getnchannels() != n_audio_in:
    print "Input has %d channels, but plugin has %d audio inputs\n" % (
        wav_in.getnchannels(), n_audio_in)
    sys.exit(1)

# Open output file
wav_out = wave.open(wav_out_path, 'w')
if not wav_out:
    print "Failed to open output `%s'\n" % wav_out_path
    sys.exit(1)

# Set output file to same format as input (except possibly nchannels)
wav_out.setparams(wav_in.getparams())
wav_out.setnchannels(n_audio_out)

rate    = wav_in.getframerate()
nframes = wav_in.getnframes()

# Instantiate plugin
instance = lilv.Instance(plugin, rate)

def read_float(wf, nframes):
    wav = wf.readframes(nframes)
    if wf.getsampwidth() == 4:
        wav = wave.struct.unpack("<%ul" % (len(wav) / 4), wav)
        wav = [ i / float(math.pow(2, 32)) for i in wav ]
    elif wf.getsampwidth() == 2:
        wav = wave.struct.unpack("<%uh" % (len(wav) / 2), wav)
        wav = [ i / float(math.pow(2, 16)) for i in wav ]
    else:
        wav = wave.struct.unpack("%uB"  % (len(wav)),     wav)
        wav = [ s - 128 for s in wav ]
        wav = [ i / float(math.pow(2, 8)) for i in wav ]

    n_channels = wf.getnchannels()
    wavs       = []
    if n_channels > 1:
        for i in xrange(n_channels):
            wavs.append([ wav[j] for j in xrange(0, len(wav), n_channels) ])
    else:
        wavs = [ wav ]

    return wavs

in_buf = read_float(wav_in, nframes)

# TODO: buffer marshaling
#instance.connect_port(3, in_buf)

print '%s => %s => %s @ %d Hz' % (wav_in_path, plugin.get_name(), wav_out_path, rate)

instance.connect_port(3, in_buf)
