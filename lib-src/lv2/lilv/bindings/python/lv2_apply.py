#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import lilv
import sys
import wave
import numpy

class WavFile(object):
    """Helper class for accessing wav file data. Should work on the most common
    formats (8 bit unsigned, 16 bit signed, 32 bit signed). Audio data is
    converted to float32."""

    # (struct format code, is_signedtype) for each sample width:
    WAV_SPECS = {
        1: ("B", False),
        2: ("h", True),
        4: ("l", True),
    }

    def __init__(self, wav_in_path):
        self.wav_in = wave.open(wav_in_path, 'r')
        self.framerate = self.wav_in.getframerate()
        self.nframes = self.wav_in.getnframes()
        self.nchannels = self.wav_in.getnchannels()
        self.sampwidth = self.wav_in.getsampwidth()
        wav_spec = self.WAV_SPECS[self.sampwidth]
        self.struct_fmt_code, self.signed = wav_spec
        self.range = 2 ** (8*self.sampwidth)

    def read(self):
        """Read data from an open wav file. Return a list of channels, where each
        channel is a list of floats."""
        raw_bytes = self.wav_in.readframes(self.nframes)
        struct_fmt = "%u%s" % (len(raw_bytes) / self.sampwidth, self.struct_fmt_code)
        data = wave.struct.unpack(struct_fmt, raw_bytes)
        if self.signed:
            data = [i / float(self.range/2) for i in data]
        else:
            data = [(i - float(range/2)) / float(range/2) for i in data]

        channels = []
        for i in range(self.nchannels):
            channels.append([data[j] for j in range(0, len(data), self.nchannels) ])

        return channels

    def close(self):
        self.wav_in.close()

def main():
    # Read command line arguments
    if len(sys.argv) != 4:
        print('USAGE: lv2_apply.py PLUGIN_URI INPUT_WAV OUTPUT_WAV')
        sys.exit(1)

    # Initialise Lilv
    world = lilv.World()
    ns    = world.ns
    world.load_all()

    plugin_uri   = sys.argv[1]
    wav_in_path  = sys.argv[2]
    wav_out_path = sys.argv[3]

    # Find plugin
    plugin_uri_node = world.new_uri(plugin_uri)
    plugins         = world.get_all_plugins()
    if plugin_uri_node not in plugins:
        print("Unknown plugin `%s'" % plugin_uri)
        sys.exit(1)

    plugin      = plugins[plugin_uri_node]
    n_audio_in  = plugin.get_num_ports_of_class(ns.lv2.InputPort,  ns.lv2.AudioPort)
    n_audio_out = plugin.get_num_ports_of_class(ns.lv2.OutputPort, ns.lv2.AudioPort)
    if n_audio_out == 0:
        print("Plugin has no audio outputs\n")
        sys.exit(1)

    # Open input file
    try:
        wav_in = WavFile(wav_in_path)
    except:
        print("Failed to open input `%s'\n" % wav_in_path)
        sys.exit(1)

    if wav_in.nchannels != n_audio_in:
        print("Input has %d channels, but plugin has %d audio inputs\n" % (
            wav_in.nchannels, n_audio_in))
        sys.exit(1)

    # Open output file
    wav_out = wave.open(wav_out_path, 'w')
    if not wav_out:
        print("Failed to open output `%s'\n" % wav_out_path)
        sys.exit(1)

    # Set output file to same format as input (except possibly nchannels)
    wav_out.setparams(wav_in.wav_in.getparams())
    wav_out.setnchannels(n_audio_out)

    print('%s => %s => %s @ %d Hz'
          % (wav_in_path, plugin.get_name(), wav_out_path, wav_in.framerate))

    instance = lilv.Instance(plugin, wav_in.framerate)

    channels = wav_in.read()
    wav_in.close()

    # Connect all ports to buffers. NB if we fail to connect any buffer, lilv
    # will segfault.
    audio_input_buffers    = []
    audio_output_buffers   = []
    control_input_buffers  = []
    control_output_buffers = []
    for index in range(plugin.get_num_ports()):
        port = plugin.get_port_by_index(index)
        if port.is_a(ns.lv2.InputPort):
            if port.is_a(ns.lv2.AudioPort):
                audio_input_buffers.append(numpy.array(channels[len(audio_input_buffers)], numpy.float32))
                instance.connect_port(index, audio_input_buffers[-1])
            elif port.is_a(ns.lv2.ControlPort):
                default = float(port.get(ns.lv2.default))
                control_input_buffers.append(numpy.array([default], numpy.float32))
                instance.connect_port(index, control_input_buffers[-1])
            else:
                raise ValueError("Unhandled port type")
        elif port.is_a(ns.lv2.OutputPort):
            if port.is_a(ns.lv2.AudioPort):
                audio_output_buffers.append(numpy.array([0] * wav_in.nframes, numpy.float32))
                instance.connect_port(index, audio_output_buffers[-1])
            elif port.is_a(ns.lv2.ControlPort):
                control_output_buffers.append(numpy.array([0], numpy.float32))
                instance.connect_port(index, control_output_buffers[-1])
            else:
                raise ValueError("Unhandled port type")

    # Run the plugin:
    instance.run(wav_in.nframes)

    # Interleave output buffers:
    data = numpy.dstack(audio_output_buffers).flatten()

    # Return to original int range:
    if wav_in.signed:
        data = data * float(wav_in.range / 2)
    else:
        data = (data + 1) * float(wav_in.range/2)

    # Write output file in chunks to stop memory usage getting out of hand:
    CHUNK_SIZE = 8192
    for chunk in numpy.array_split(data, CHUNK_SIZE):
        wav_out.writeframes(wave.struct.pack("%u%s" % (len(chunk), wav_in.struct_fmt_code), *chunk.astype(int)))
    wav_out.close()


if __name__ == "__main__":
    main()
