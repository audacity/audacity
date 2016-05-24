/************************************************************************
*
* Midi codes
* Copyright 1989 Carnegie Mellon University
*
*************************************************************************
*       Change Log
*  Date | Change
*-----------+------------------------------------------------------------
* 11-Mar-94 | PLu : Port to IRI
************************************************************************/

#define MIDI_DATA(d) (0x7f & (d))
#define MIDI_CHANNEL(c) (0x0f & ((c) - 1))
#define MIDI_PORT(c) (((c) - 1) >> 4)
#define MIDI_PROGRAM(p) MIDI_DATA((p) - 1)

#define MIDI_STATUS_BIT 0x80
#define MIDI_COMMON	0x70
#define MIDI_CODE_MASK  0xf0
#define MIDI_CHN_MASK   0x0f
#define MIDI_REALTIME   0xf8
#define MIDI_CHAN_MODE  0xfa

#define MIDI_OFF_NOTE   0x80
#define MIDI_ON_NOTE    0x90
#define MIDI_POLY_TOUCH 0xa0
#define MIDI_CTRL       0xb0
#define MIDI_CH_PROGRAM 0xc0
#define MIDI_TOUCH      0xd0
#define MIDI_BEND       0xe0

#ifdef UNIX_IRIX_MIDIFNS
#define CMT_MIDI_SYSEX  0xf0
#define CMT_MIDI_EOX    0xf7
#else
#define MIDI_SYSEX      0xf0
#define MIDI_EOX        0xf7
#endif
#define MIDI_Q_FRAME	0xf1
#define MIDI_SONG_POINTER 0xf2
#define MIDI_SONG_SELECT 0xf3
#define MIDI_F4		0xf4
#define MIDI_F5		0xf5
#define MIDI_TUNE_REQ	0xf6
#define MIDI_TIME_CLOCK 0xf8
#define MIDI_F9		0xf9
#define MIDI_START      0xfa
#define MIDI_CONTINUE	0xfb
#define MIDI_STOP       0xfc
#define MIDI_FD		0xfd
#define MIDI_ACTIVE_SENSING 0xfe
#define MIDI_SYS_RESET  0xff

#define MIDI_LOCAL	0x7a
#define MIDI_LOCAL_OFF	0x00
#define MIDI_LOCAL_ON	0x7f
#define MIDI_ALL_OFF	0x7b
#define MIDI_OMNI_OFF	0x7c
#define MIDI_OMNI_ON	0x7d
#define MIDI_MONO_ON	0x7e
#define MIDI_POLY_ON	0x7f
