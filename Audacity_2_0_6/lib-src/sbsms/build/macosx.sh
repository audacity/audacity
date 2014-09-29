#!/bin/bash

export SNDFILE_LIBS=/usr/local/lib/libsndfile.a
export SNDFILE_CFLAGS=-I/usr/local/include

export PORTAUDIO_LIBS="/usr/local/lib/libportaudio.a -framework CoreAudio -framework AudioToolbox -framework AudioUnit -framework Carbon"
export PORTAUDIO_CFLAGS=-I/usr/local/include

export MAD_LIBS=/usr/local/lib/libmad.a

./configure --enable-mp3 --enable-sndfile --enable-portaudio --enable-wx --enable-static --disable-dependency-tracking --enable-universal_binary --enable-multithreaded
make