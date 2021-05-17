/* sndread.c -- read sound files */

/* CHANGELOG
 *
 * 29Jun95  RBD  ULAW fixed problems with signed chars
 * 28Apr03  dm   explicitly declare sndread_file_open_count as int
 * 24Jul08  RBD & Judy Hawkins -- replace snd with PortAudio and libsndfile
 */

#include "switches.h"
#include "stdio.h"
#include "string.h"
#ifdef UNIX
#include "sys/file.h"
#else
/* #include <unistd.h> */
#ifdef WINDOWS
#include <sys/stat.h>
#include "io.h"
#else
#include <stat.h>
#endif /* WINDOWS */
#define L_SET SEEK_SET
#define L_INCR SEEK_CUR
#define PROTECTION 
#endif /* UNIX */
#ifndef mips
#include "stdlib.h"
#endif
#include "sndfile.h"
#include "xlisp.h"
#include "sound.h"
#include "sndfmt.h"
#include "falloc.h"
#include "sndread.h"
#include "multiread.h"

/* file.h doesn't define O_RDONLY under RS6K AIX */
#ifndef O_RDONLY
#define O_RDONLY 0
#endif

static int sndread_file_open_count = 0;

void read__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    read_susp_type susp = (read_susp_type) a_susp;
    long n; /* jlh Changed type to long, trying to make move_samples_... work */
    sample_block_type out;
    register sample_block_values_type out_ptr;
    /* allow up to 4 bytes/sample:  jlh -- does this need to be 8? */
    /* FIX -- why 8? for doubles? Maybe it should be sizeof(sample). I think
       this buffer was here to allow you to input any format and convert to
       float. The assumption was no sample would be longer than 4 bytes and
       after conversion, samples would be 4 byte floats. 
    */
    long in_count; /* jlh Trying to make move_samples_... work */

    falloc_sample_block(out, "read__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    in_count = (long) sf_readf_float(susp->sndfile, out_ptr,
                                     max_sample_block_len);

    n = in_count;

    /* don't read too many */
    if (n > (susp->cnt - susp->susp.current)) {
        n = (long) (susp->cnt - susp->susp.current);
    }

    snd_list->block_len = (short) n;
    susp->susp.current += n;

    if (n == 0) {
        /* we didn't read anything, but can't return length zero, so
           convert snd_list to pointer to zero block */
        snd_list_terminate(snd_list);
    } else if (n < max_sample_block_len) {
        /* this should close file and free susp */
        snd_list_unref(snd_list->u.next);
        /* if something is in buffer, terminate by pointing to zero block */
        snd_list->u.next = zero_snd_list;
    }
} /* read__fetch */


void read_free(snd_susp_type a_susp)
{
    read_susp_type susp = (read_susp_type) a_susp;
    sf_close(susp->sndfile);
    sndread_file_open_count--;
    ffree_generic(susp, sizeof(read_susp_node), "read_free");
}


void read_print_tree(snd_susp_type a_susp, int n)
{
}


LVAL snd_make_read(
  unsigned char *filename, 	/* file to read */
  time_type offset, 	/* offset to skip (in seconds) */
  time_type t0,		/* start time of resulting sound */
  long *format,		/* AIFF, IRCAM, NeXT, etc. */
  long *channels,	/* number of channels */
  long *mode, 		/* sample format: PCM, ALAW, etc. */
  long *bits,		/* BPS: bits per sample */
  long *swap,           /* swap bytes */
  double *srate,	/* srate: sample rate */
  double *dur,		/* duration (in seconds) to read */
  long *flags)		/* which parameters have been set */
{
    register read_susp_type susp;
    /* srate specified as input parameter */
    sample_type scale_factor = 1.0F;
    sf_count_t frames;
    double actual_dur;

    falloc_generic(susp, read_susp_node, "snd_make_read");
    memset(&(susp->sf_info), 0, sizeof(SF_INFO));

    susp->sf_info.samplerate = ROUND32(*srate);
    susp->sf_info.channels = *channels;

    switch (*mode) {
    case SND_MODE_ADPCM:
        susp->sf_info.format = SF_FORMAT_IMA_ADPCM;
        break;
    case SND_MODE_PCM:
        if (*bits == 8) susp->sf_info.format = SF_FORMAT_PCM_S8;
        else if (*bits == 16) susp->sf_info.format = SF_FORMAT_PCM_16;
        else if (*bits == 24) susp->sf_info.format = SF_FORMAT_PCM_24;
        else if (*bits == 32) susp->sf_info.format = SF_FORMAT_PCM_32;
        else {
            susp->sf_info.format = SF_FORMAT_PCM_16;
            *bits = 16;
        }
        break;
    case SND_MODE_ULAW:
        susp->sf_info.format = SF_FORMAT_ULAW;
        break;
    case SND_MODE_ALAW:
        susp->sf_info.format = SF_FORMAT_ALAW;
        break;
    case SND_MODE_FLOAT:
        susp->sf_info.format = SF_FORMAT_FLOAT;
        break;
    case SND_MODE_UPCM:
        susp->sf_info.format = SF_FORMAT_PCM_U8;
        *bits = 8;
        break;
    }

    if (*format == SND_HEAD_RAW) susp->sf_info.format |= SF_FORMAT_RAW;

    if (*swap) {
        /* set format to perform a byte swap (change from cpu endian-ness) */
        /* write the code so it will only compile if one and only one 
           ENDIAN setting is defined */
#ifdef XL_LITTLE_ENDIAN
        susp->sf_info.format |= SF_ENDIAN_BIG;
#endif
#ifdef XL_BIG_ENDIAN
        susp->sf_info.format |= SF_ENDIAN_LITTLE;
#endif
    }

    susp->sndfile = NULL;
    if (ok_to_open((const char *) filename, "rb"))
        susp->sndfile = sf_open((const char *) filename, SFM_READ,
                                &(susp->sf_info));

    if (!susp->sndfile) {
        char error[240];
        snprintf(error, 240, "SND-READ: Cannot open file '%s' because of %s",
                 filename, sf_strerror(susp->sndfile));
        xlfail(error);
    }
    if (susp->sf_info.channels < 1) {
        sf_close(susp->sndfile);
        xlfail("Must specify 1 or more channels");
    }

    /* report samplerate from file, but if user provided a double
     * as sample rate, don't replace it with an integer.
     */
    if ((susp->sf_info.format & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW) {
        *srate = susp->sf_info.samplerate;
    }
    /* compute dur */
    frames = susp->sf_info.frames;
    actual_dur = ((double) frames) / *srate;
    if (offset < 0) offset = 0;
    /* round offset to an integer frame count */
    frames = (sf_count_t) (offset * *srate + 0.5);
    offset = ((double) frames) / *srate;
    actual_dur -= offset;
    if (actual_dur < 0) {
        sf_close(susp->sndfile);
        xlfail("SND-READ: offset is beyond end of file");
    }
    if (actual_dur < *dur) *dur = actual_dur;

    sf_seek(susp->sndfile, frames, SEEK_SET); /* return to read loc in file */
    /* initialize susp state */
    susp->susp.sr = *srate;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = read_print_tree; /*jlh empty function... */
    susp->susp.current = 0;
    susp->susp.log_stop_cnt = UNKNOWN;
    /* watch for overflow */
    if (*dur * *srate + 0.5 > (unsigned long) 0xFFFFFFFF) {
        susp->cnt = 0x7FFFFFFFFFFFFFFF;
    } else {
        susp->cnt = ROUNDBIG((*dur) * *srate);
    }

    switch (susp->sf_info.format & SF_FORMAT_TYPEMASK) {
    case SF_FORMAT_AIFF: *format = SND_HEAD_AIFF; break;
    case SF_FORMAT_IRCAM: *format = SND_HEAD_IRCAM; break;
    case SF_FORMAT_AU: *format = SND_HEAD_NEXT; break;
    case SF_FORMAT_WAV: *format = SND_HEAD_WAVE; break;
    case SF_FORMAT_PAF: *format = SND_HEAD_PAF; break;
    case SF_FORMAT_SVX: *format = SND_HEAD_SVX; break;
    case SF_FORMAT_NIST: *format = SND_HEAD_NIST; break;
    case SF_FORMAT_VOC: *format = SND_HEAD_VOC; break;
    case SF_FORMAT_W64: *format = SND_HEAD_W64; break;
    case SF_FORMAT_MAT4: *format = SND_HEAD_MAT4; break;
    case SF_FORMAT_MAT5: *format = SND_HEAD_MAT5; break;
    case SF_FORMAT_PVF: *format = SND_HEAD_PVF; break;
    case SF_FORMAT_XI: *format = SND_HEAD_XI; break;
    case SF_FORMAT_HTK: *mode = SND_HEAD_HTK; break;
    case SF_FORMAT_SDS: *mode = SND_HEAD_SDS; break;
    case SF_FORMAT_AVR: *mode = SND_HEAD_AVR; break;
    case SF_FORMAT_SD2: *format = SND_HEAD_SD2; break;
    case SF_FORMAT_FLAC: *format = SND_HEAD_FLAC; break;
    case SF_FORMAT_CAF: *format = SND_HEAD_CAF; break;
    case SF_FORMAT_RAW: *format = SND_HEAD_RAW; break;
    case SF_FORMAT_OGG: *format = SND_HEAD_OGG; break;
    case SF_FORMAT_WAVEX: *format = SND_HEAD_WAVEX; break;
    default: *format = SND_HEAD_NONE; break;
    }
    *channels = susp->sf_info.channels;
    switch (susp->sf_info.format & SF_FORMAT_SUBMASK) {
    case SF_FORMAT_PCM_S8: *bits = 8; *mode = SND_MODE_PCM; break;
    case SF_FORMAT_PCM_16: *bits = 16; *mode = SND_MODE_PCM; break;
    case SF_FORMAT_PCM_24: *bits = 24; *mode = SND_MODE_PCM; break;
    case SF_FORMAT_PCM_32: *bits = 32; *mode = SND_MODE_PCM; break;
    case SF_FORMAT_PCM_U8: *bits = 8; *mode = SND_MODE_UPCM; break;
    case SF_FORMAT_FLOAT: *bits = 32; *mode = SND_MODE_FLOAT; break;
    case SF_FORMAT_DOUBLE: *bits = 64; *mode = SND_MODE_DOUBLE; break;
    case SF_FORMAT_ULAW: *bits = 8; *mode = SND_MODE_ULAW; break;
    case SF_FORMAT_ALAW: *bits = 8; *mode = SND_MODE_ALAW; break;
    case SF_FORMAT_IMA_ADPCM: *bits = 16; *mode = SND_MODE_ADPCM; break;
    case SF_FORMAT_MS_ADPCM: *bits = 16; *mode = SND_MODE_ADPCM; break;
    case SF_FORMAT_GSM610: *bits = 16; *mode = SND_MODE_GSM610; break;
    case SF_FORMAT_VOX_ADPCM: *bits = 16; *mode = SND_MODE_ADPCM; break;
    case SF_FORMAT_G721_32: *bits = 16; *mode = SND_MODE_ADPCM; break;
    case SF_FORMAT_G723_24: *bits = 16; *mode = SND_MODE_ADPCM; break;
    case SF_FORMAT_G723_40: *bits = 16; *mode = SND_MODE_ADPCM; break;
    case SF_FORMAT_DWVW_12: *bits = 12; *mode = SND_MODE_DWVW; break;
    case SF_FORMAT_DWVW_16: *bits = 16; *mode = SND_MODE_DWVW; break;
    case SF_FORMAT_DWVW_24: *bits = 24; *mode = SND_MODE_DWVW; break;
    case SF_FORMAT_DWVW_N: *bits = 32; *mode = SND_MODE_DWVW; break;
    case SF_FORMAT_DPCM_8: *bits = 8; *mode = SND_MODE_DPCM; break;
    case SF_FORMAT_DPCM_16: *bits = 16; *mode = SND_MODE_DPCM; break;
    default: *mode = SND_MODE_UNKNOWN; break;
    }
    sndread_file_open_count++;
#ifdef MACINTOSH
    if (sndread_file_open_count > 24) {
        nyquist_printf("Warning: more than 24 sound files are now open\n");
    }
#endif
    /* report info back to caller */
    if ((susp->sf_info.format & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW) {
        *flags = SND_HEAD_CHANNELS | SND_HEAD_MODE | SND_HEAD_BITS |
                 SND_HEAD_SRATE | SND_HEAD_LEN | SND_HEAD_TYPE;
    }    
    if (susp->sf_info.channels == 1) {
        susp->susp.fetch = read__fetch;
        susp->susp.free = read_free;
        susp->susp.name = "read";
        return cvsound(sound_create((snd_susp_type)susp, t0, *srate, 
                                    scale_factor));
    } else {
        susp->susp.fetch = multiread_fetch;
        susp->susp.free = multiread_free;
        susp->susp.name = "multiread";
        return multiread_create(susp);
    }
}




