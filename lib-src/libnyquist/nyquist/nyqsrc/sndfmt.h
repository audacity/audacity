/*
 * sndfmt.h -- format constants for Nyquist programs
 */
/*
 * converted by Roger Dannenberg from snd.h, Jul 08
 */
#ifdef SND_H
error here
#endif
#define SND_H


/* header formats */

#define SND_HEAD_NONE 0
/* LISP-SRC: (setf snd-head-none 0) */
#define SND_HEAD_AIFF 1
/* LISP-SRC: (setf snd-head-AIFF 1) */
#define SND_HEAD_IRCAM 2
/* LISP-SRC: (setf snd-head-IRCAM 2) */
#define SND_HEAD_NEXT 3
/* LISP-SRC: (setf snd-head-NeXT 3) */
#define SND_HEAD_WAVE 4
/* LISP-SRC: (setf snd-head-Wave 4) */
#define SND_HEAD_PAF 5
/* LISP-SRC: (setf snd-head-PAF 5) */
#define SND_HEAD_SVX 6
/* LISP-SRC: (setf snd-head-SVX 6) */
#define SND_HEAD_NIST 7
/* LISP-SRC: (setf snd-head-NIST 7) */
#define SND_HEAD_VOC 8
/* LISP-SRC: (setf snd-head-VOC 8) */
#define SND_HEAD_W64 9
/* LISP-SRC: (setf snd-head-W64 9) */
#define SND_HEAD_MAT4 10
/* LISP-SRC: (setf snd-head-MAT4 10) */
#define SND_HEAD_MAT5 11
/* LISP-SRC: (setf snd-head-MAT5 11) */
#define SND_HEAD_PVF 12
/* LISP-SRC: (setf snd-head-PVF 12) */
#define SND_HEAD_XI 13
/* LISP-SRC: (setf snd-head-XI 13) */
#define SND_HEAD_HTK 14
/* LISP-SRC: (setf snd-head-HTK 14) */
#define SND_HEAD_SDS 15
/* LISP-SRC: (setf snd-head-SDS 15) */
#define SND_HEAD_AVR 16
/* LISP-SRC: (setf snd-head-AVR 16) */
#define SND_HEAD_SD2 17
/* LISP-SRC: (setf snd-head-SD2 17) */
#define SND_HEAD_FLAC 18
/* LISP-SRC: (setf snd-head-FLAC 18) */
#define SND_HEAD_CAF 19
/* LISP-SRC: (setf snd-head-CAF 19) */
#define SND_HEAD_RAW 20
/* LISP-SRC: (setf snd-head-raw 20) */
#define SND_HEAD_OGG 21
/* LISP-SRC: (setf snd-head-OGG 21) */
#define SND_HEAD_WAVEX 22
/* LISP-SRC: (setf snd-head-WAVEX 22) */
#define SND_NUM_HEADS 23

/* bitfields for soundheaders */
#define SND_HEAD_CHANNELS (1<<0)
/* LISP-SRC: (setf snd-head-channels 1) */
#define SND_HEAD_MODE (1<<1)
/* LISP-SRC: (setf snd-head-mode 2) */
#define SND_HEAD_BITS (1<<2)
/* LISP-SRC: (setf snd-head-bits 4) */
#define SND_HEAD_SRATE (1<<3)
/* LISP-SRC: (setf snd-head-srate 8) */

/* when returned from lisp, len (samples) is converted to time (seconds) */
#define SND_HEAD_LEN (1<<4)
/* LISP-SRC: (setf snd-head-dur 16) */

#define SND_HEAD_LATENCY (1<<5)
/* LISP-SRC: (setf snd-head-latency 32) */
#define SND_HEAD_TYPE (1<<6)
/* LISP-SRC: (setf snd-head-type 64) */

/* modes */
/* IMA ADPCM */
#define SND_MODE_ADPCM 0
/* LISP-SRC: (setf snd-mode-adpcm 0) */
#define SND_MODE_PCM   1
/* LISP-SRC: (setf snd-mode-pcm 1) */
#define SND_MODE_ULAW 2
/* LISP-SRC: (setf snd-mode-ulaw 2) */
#define SND_MODE_ALAW  3
/* LISP-SRC: (setf snd-mode-alaw 3) */
#define SND_MODE_FLOAT 4
/* LISP-SRC: (setf snd-mode-float 4) */
/* unsigned pcm (e.g. Microsoft 8-bit wav format): */
#define SND_MODE_UPCM 5
/* LISP-SRC: (setf snd-mode-upcm 5) */
#define SND_MODE_UNKNOWN 6
/* LISP-SRC: (setf snd-mode-unknown 6) */
#define SND_MODE_DOUBLE 7
/* LISP-SRC: (setf snd-mode-double 7) */
#define SND_MODE_GSM610 8
/* LISP-SRC: (setf snd-mode-GSM610 8) */
#define SND_MODE_DWVW 9
/* LISP-SRC: (setf snd-mode-DWVW 9) */
#define SND_MODE_DPCM 10
/* LISP-SRC: (setf snd-mode-DPCM 10) */
/* microsoft ADPCM */
#define SND_MODE_MSADPCM 11
/* LISP-SRC: (setf snd-mode-msadpcm 11) */
#define SND_MODE_VORBIS 12
/* LISP-SRC: (setf snd-mode-vorbis 11) */
#define SND_NUM_MODES 13


#define SND_LOOP_NONE 0
#define SND_LOOP_FORWARD 1
#define SND_LOOP_FORWARD_BACKWARD 2

typedef struct {
    int mode;
    long begin;
    long end;
} loop_node, *loop_type;

