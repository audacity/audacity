/* sndwrite.c -- write sounds to files */

#include "stdlib.h"
#include "switches.h"
#include "string.h"
#ifdef UNIX
#include "sys/types.h"
#endif
#ifdef WINDOWS
#include <io.h>
#endif
#include <stdio.h>
/* include sound.h first because it includes math.h 
 * which declares abs(). cext.h #defines abs()!
 * sound.h depends on xlisp.h
 */
#include "xlisp.h"
#include "sound.h"
#include "cext.h"
#include "userio.h"
#include "falloc.h"
#include "sndfmt.h"
#include "sndwrite.h"
#include "extern.h"
#include "sndfile.h"

#ifdef UNIX
#include "sys/file.h"
/* #include <sys/stat.h>*/
#include <netinet/in.h>
#else
#ifdef MACINTOSH
#include <unistd.h>
#include <stat.h>
#define L_SET SEEK_SET
#define L_INCR SEEK_CUR
#endif
#endif

/* Previously, Nyquist would wrap samples that 
 * overflowed -- this produces horrible output, 
 * but makes it really easy to detect clipping,
 * which I found helpful in my own work and good 
 * for students too since the effect is impossible
 * to ignore. Now that Nyquist is doing IO to
 * libraries that clip, we're going to artificially
 * generate the wrapping here. This is floating point
 * wrapping, so +1.0 does not wrap (it would if it
 * were an integer since the maximum sample value for
 * 16-bit data is a bit less than 1.) Since this is extra
 * overhead, I'm trying to be a bit clever by using
 * the compare to max_sample to eliminate compares
 * for clipping in the common case.
 * 
 * INPUTS: max_sample -- initially 0.0
 *         threshold -- initially 0.0
 *         s -- the value of the current sample
 *         x -- if s has to be wrapped, put the value here
 */
#define COMPUTE_MAXIMUM_AND_WRAP(x) \
    if (s > threshold) { \
        if (s > max_sample) { \
            max_sample = s; \
            threshold = min(1.0F, s); \
        } \
        if (s > 1.0) { \
            s = (sample_type) (fmodf(s + 1.0F, 2.0F) - 1.0F); \
            (x) = s; \
        } \
    } else if (s < -threshold) { \
        if (s < -max_sample) { \
            max_sample = -s; \
            threshold = min(1.0F, -s); \
        } \
        if (s < -1.0) { \
            s = (sample_type) -(fmodf(-s + 1.0F, 2.0F) - 1.0F); \
            (x) = s; \
        } \
    }
// the s < -threshold case is tricky: 
//    flip the signal, do the wrap, flip again
//    in order to pass positive values to fmod


/* When not using PCM encodings, we do not wrap
 * samples -- therefore float sample formats do 
 * not wrap or clip when written to sound files
 */
#define COMPUTE_MAXIMUM() \
        if (s > max_sample) { \
            max_sample = s; \
        } else if (s < -max_sample) { \
            max_sample = -s; \
        }

// should be looking for local portaudio
#include "portaudio.h"

void finish_audio();

long flush_count = 0; /* how many samples to write to finish */

#define D if (0) 

int sndwrite_trace = 0;	/* debugging */

static int portaudio_initialized = false; /* is PortAudio initialized? */

void portaudio_exit()
{
    if (portaudio_initialized) {
        Pa_Terminate();
    }
}


// sound_save_sound - implements s-save for mono sounds
sample_type sound_save_sound(LVAL s_as_lval, int64_t n, SF_INFO *sf_info,
                             SNDFILE *snd_file, float *buf, int64_t *ntotal,
                             int64_t progress);

// sound_save_array - implements s-save for multi-channel sounds
sample_type sound_save_array(LVAL sa, int64_t n, SF_INFO *sf_info,
                             SNDFILE *snd_file, float *buf, int64_t *ntotal,
                             int64_t progress);

unsigned char st_linear_to_ulaw(int sample);/* jlh not used anywhere */


typedef struct {
    sound_type sound;
    int cnt;
    sample_block_values_type ptr;
    double scale;
    int terminated;
} sound_state_node, *sound_state_type;


static int portaudio_error(PaError err, char *problem)
{
    char msgbuffer[256];
    if (err != paNoError) {
        snprintf(msgbuffer, sizeof(msgbuffer), 
                 "%s, error %d, %s.", problem, (int) err, 
                                Pa_GetErrorText(err));
        xlerrprint("warning", NULL, msgbuffer, s_unbound);
        return true;
    }
    return false;
}


PaStream *audio_stream = NULL;


/* whenever we jump to toplevel, make sure audio is closed */
void local_toplevel(void)
{
    if (audio_stream) {
        finish_audio();
    }
}


LVAL prepare_audio(LVAL play, SF_INFO *sf_info)
{
    PaStreamParameters output_parameters;
    int i, j = -1;
    int num_devices;
    const PaDeviceInfo *device_info = NULL;
    const PaHostApiInfo *host_info;
    // list tells us to list devices
    LVAL list = xlenter("*SND-LIST-DEVICES*");
    // pref tells us which device to open
    LVAL pref = xlenter("*SND-DEVICE*");
    int pref_num = -1;
    unsigned char *pref_string = NULL;
    list = getvalue(list);
    if (list == s_unbound) list = NULL;
    pref = getvalue(pref);
    if (pref == s_unbound) pref = NULL;
    if (stringp(pref)) pref_string = getstring(pref);
    else if (fixp(pref)) pref_num = (int) getfixnum(pref);

    if (!portaudio_initialized) {
        if (portaudio_error(Pa_Initialize(), 
                            "could not initialize portaudio")) {
            return NIL;
        }
        portaudio_initialized = TRUE;
    }

    output_parameters.device = Pa_GetDefaultOutputDevice(); 
    output_parameters.channelCount = sf_info->channels;
    output_parameters.sampleFormat = paFloat32;
    output_parameters.hostApiSpecificStreamInfo = NULL;
    /* remember that Nyquist has to do GC */
    output_parameters.suggestedLatency = sound_latency;

    // Initialize the audio stream for output
    // If this is Linux, prefer to open ALSA device
    num_devices = Pa_GetDeviceCount();
    // nyquist_printf("num_devices %d\n", num_devices);
    for (i = 0; i < num_devices; i++) {
        device_info = Pa_GetDeviceInfo(i);
        host_info = Pa_GetHostApiInfo(device_info->hostApi);
        if (list) {
            nyquist_printf("PortAudio %d: %s -- %s\n", i,
                           device_info->name, host_info->name);
        }
        if (j == -1) {
            if (pref_num >= 0 && pref_num == i) j = i;
            else if (pref_string &&
                     strstr(device_info->name, (char *) pref_string)) j = i;
        }
        // giving preference to first ALSA device seems to be a bad idea
        // if (j == -1 && host_info->type == paALSA) {
        //     j = i;
        // }
    }
    if (j != -1) {
        output_parameters.device = j;
    }
    if (list) {
        nyquist_printf("... Default device is %d\n",
                       Pa_GetDefaultOutputDevice());
        nyquist_printf("... Selected device %d for output\n",
                       output_parameters.device);
    }
    if (device_info) {
        if (portaudio_error(
                Pa_OpenStream(&audio_stream, NULL /* input */, &output_parameters,
                    sf_info->samplerate, max_sample_block_len, 
                    paClipOff, NULL /* callback */, NULL /* userdata */),
                "could not open audio")) {
            nyquist_printf("audio device name: %s\n", device_info->name);
            audio_stream = NULL;
            return NIL;
        }
    } else {
        nyquist_printf("warning: no audio device found\n");
        return NIL;
    }
    flush_count = (long) (sf_info->samplerate * (sound_latency + 0.2));

    if (portaudio_error(Pa_StartStream(audio_stream),
                        "could not start audio")) {
        return NIL;
    }

    return play;
}


/* finish_audio -- flush the remaining samples, then close */
/**/
void finish_audio(void)
{
    /* portaudio_error(Pa_StopStream(audio_stream), "could not stop stream"); */
    /* write Latency frames of audio to make sure all samples are played */
    float zero[MAX_SND_CHANNELS * 16];
    int i;
    for (i = 0; i < MAX_SND_CHANNELS * 16; i++) zero[i] = 0.0F;
    while (flush_count > 0) {
        Pa_WriteStream(audio_stream, zero, 16);
        flush_count -= 16;
    }
    portaudio_error(Pa_CloseStream(audio_stream), "could not close audio");
    audio_stream = NULL;
}


long lookup_format(long format, long mode, long bits, long swap)
{
    long sf_mode;
    long sf_format;

    switch (format) {
    case SND_HEAD_NONE: return 0; break; // get info from file
    case SND_HEAD_AIFF: sf_format = SF_FORMAT_AIFF; break;
    case SND_HEAD_IRCAM: sf_format = SF_FORMAT_IRCAM; break;
    case SND_HEAD_NEXT: sf_format = SF_FORMAT_AU; break;
    case SND_HEAD_WAVE: sf_format = SF_FORMAT_WAV; break;
    case SND_HEAD_PAF: sf_format = SF_FORMAT_PAF; break;
    case SND_HEAD_SVX: sf_format = SF_FORMAT_SVX; break;
    case SND_HEAD_NIST: sf_format = SF_FORMAT_NIST; break;
    case SND_HEAD_VOC: sf_format = SF_FORMAT_VOC; break;
    case SND_HEAD_W64: sf_format = SF_FORMAT_W64; break;
    case SND_HEAD_MAT4: sf_format = SF_FORMAT_MAT4; break;
    case SND_HEAD_MAT5: sf_format = SF_FORMAT_MAT5; break;
    case SND_HEAD_PVF: sf_format = SF_FORMAT_PVF; break;
    case SND_HEAD_XI: sf_format = SF_FORMAT_XI; break;
    case SND_HEAD_HTK: sf_format = SF_FORMAT_HTK; break;
    case SND_HEAD_SDS: sf_format = SF_FORMAT_SDS; break;
    case SND_HEAD_AVR: sf_format = SF_FORMAT_AVR; break;
    case SND_HEAD_SD2: sf_format = SF_FORMAT_SD2; break;
    case SND_HEAD_FLAC: sf_format = SF_FORMAT_FLAC; break;
    case SND_HEAD_CAF: sf_format = SF_FORMAT_CAF; break;
    case SND_HEAD_RAW:
        sf_format = SF_FORMAT_RAW; 
#ifdef XL_BIG_ENDIAN
        sf_format |= (swap ? SF_ENDIAN_LITTLE : SF_ENDIAN_BIG);
#endif
#ifdef XL_LITTLE_ENDIAN
        sf_format |= (swap ? SF_ENDIAN_BIG : SF_ENDIAN_LITTLE);
#endif        
        break;
    case SND_HEAD_OGG: sf_format = SF_FORMAT_OGG; mode = SND_MODE_VORBIS; break;
    case SND_HEAD_WAVEX: sf_format = SF_FORMAT_WAVEX; break;
    default: 
        sf_format = SF_FORMAT_WAV; 
        nyquist_printf("s-save: unrecognized format, using SND_HEAD_WAVE\n");
        break;
    }

    switch (mode) {
    case SND_MODE_ADPCM: sf_mode = SF_FORMAT_IMA_ADPCM; break;
    case SND_MODE_UPCM: 
        if (bits <= 8) {
            sf_mode = SF_FORMAT_PCM_U8; break;
        } else {
            nyquist_printf("s-save: SND_MODE_UPCM is for 8-bit samples only, "
                           "using PCM instead\n");
        } /* no break here, fall through to SND_MODE_PCM... */
    default:
        nyquist_printf("s-save: unrecognized mode (%ld), using PCM\n",
                       mode);
        /* no break, fall through as SND_MODE_PCM */
    case SND_MODE_PCM: 
        if (bits <= 8) sf_mode = SF_FORMAT_PCM_S8;
        else if (bits <= 16) sf_mode = SF_FORMAT_PCM_16;
        else if (bits <= 24) sf_mode = SF_FORMAT_PCM_24;
        else if (bits <= 32) sf_mode = SF_FORMAT_PCM_32;
        else {
            sf_mode = SF_FORMAT_PCM_16;
            nyquist_printf(
                    "s-save: bad bits parameter (%ld), using 16-bit PCM\n",
                    bits);
        }
        break;
    case SND_MODE_ULAW: sf_mode = SF_FORMAT_ULAW; break;
    case SND_MODE_ALAW: sf_mode = SF_FORMAT_ALAW; break;
    case SND_MODE_FLOAT: sf_mode = SF_FORMAT_FLOAT; break;
    case SND_MODE_DOUBLE: sf_mode = SF_FORMAT_DOUBLE; break;
    case SND_MODE_UNKNOWN: sf_mode = SF_FORMAT_PCM_16; break;
    case SND_MODE_GSM610: sf_mode = SF_FORMAT_GSM610; break; 
    case SND_MODE_DWVW: 
        if (bits <= 12) sf_mode = SF_FORMAT_DWVW_12;
        else if (bits <= 16) sf_mode = SF_FORMAT_DWVW_16;
        else if (bits <= 24) sf_mode = SF_FORMAT_DWVW_24;
        else sf_mode = SF_FORMAT_DWVW_N;
        break;
    case SND_MODE_DPCM:
        if (bits <= 8) sf_mode = SF_FORMAT_DPCM_8;
        else if (bits <= 16) sf_mode = SF_FORMAT_DPCM_16;
        else {
            sf_mode = SF_FORMAT_DPCM_16;
            nyquist_printf(
                    "s-save: bad bits parameter (%ld), using 16-bit DPCM\n",
                    bits);
        }
        break;
    case SND_MODE_MSADPCM: sf_mode = SF_FORMAT_MS_ADPCM; break;
    case SND_MODE_VORBIS: sf_mode = SF_FORMAT_VORBIS; break;
    }
    return sf_format | sf_mode;
}


// sound_save - implements s-save. As files are written, the sample count
//    is printed at intervals of progress. Typically every 10s of sound.
double sound_save(LVAL snd_expr, int64_t n, unsigned char *filename,
                  long format, long mode, long bits, long swap, double *sr,
                  long *nchans, double *duration, LVAL play, int64_t progress)
{
    LVAL result;
    float *buf = NULL;
    int64_t ntotal;
    double max_sample;
    SNDFILE *sndfile = NULL;
    SF_INFO sf_info;
    if (SAFE_NYQUIST) play = FALSE;
    
    gc();
    
    memset(&sf_info, 0, sizeof(sf_info));
    sf_info.format = lookup_format(format, mode, bits, swap);

    // printf("in sound_save: calling xleval on %p\n", snd_expr);
    result = xleval(snd_expr);
    // printf("in sound_save: got result of xleval %p\n", result);
    /* BE CAREFUL - DO NOT ALLOW GC TO RUN WHILE RESULT IS UNPROTECTED */
    if (vectorp(result)) {
        /* make sure all elements are of type a_sound */
        long i = getsize(result);
        *nchans = sf_info.channels = i;
        while (i > 0) {
            i--;
            if (!exttypep(getelement(result, i), a_sound)) {
                xlerror("S-SAVE: array has non-sound element",
                         result);
            }
        }
        /* assume all are the same: */
        *sr = sf_info.samplerate = ROUND32(getsound(getelement(result, 0))->sr); 

        /* note: if filename is "", then don't write file; therefore,
         * write the file if (filename[0])
         */ 
        if (filename[0]) {
            sndfile = NULL;
            if (ok_to_open((char *) filename, "wb"))
                sndfile = sf_open((char *) filename, SFM_WRITE, &sf_info);
            if (sndfile) {
                /* use proper scale factor: 8000 vs 7FFF */
                sf_command(sndfile, SFC_SET_CLIPPING, NULL, SF_TRUE);
            }
        }
        
        if (play) 
            play = prepare_audio(play, &sf_info);

        if ((buf = (float *) malloc(max_sample_block_len * sf_info.channels *
                                    sizeof(float))) == NULL) {
            xlabort("snd_save -- couldn't allocate memory");
        }

        max_sample = sound_save_array(result, n, &sf_info, sndfile, 
                                      buf, &ntotal, progress);
        *duration = ntotal / *sr;
        if (sndfile) sf_close(sndfile);
        if (play != NIL) finish_audio();
    } else if (exttypep(result, a_sound)) {
        *nchans = sf_info.channels = 1;
        sf_info.samplerate = ROUND32((getsound(result))->sr);
        *sr = sf_info.samplerate;
        if (filename[0]) {
            sndfile = NULL;
            if (ok_to_open((char *) filename, "wb")) {
                sndfile = sf_open((char *) filename, SFM_WRITE, &sf_info);
                if (sndfile) {
                    /* use proper scale factor: 8000 vs 7FFF */
                    sf_command(sndfile, SFC_SET_CLIPPING, NULL, SF_TRUE);
                } else {
                    char error[240];
                    sprintf(error, "snd_save -- %s", sf_error_number(sf_error(sndfile)));
                    xlabort(error);
                }
            } else {
                xlabort("snd_save -- write not permitted by -W option");
            }
        }
        if (play)
            play = prepare_audio(play, &sf_info);

        if ((buf = (float *) malloc(max_sample_block_len * 
                                    sizeof(float))) == NULL) {
            xlabort("snd_save -- couldn't allocate memory");
        }

        max_sample = sound_save_sound(result, n, &sf_info, sndfile,
                                      buf, &ntotal, progress);
        *duration = ntotal / *sr;
        if (sndfile) sf_close(sndfile);
        if (play != NIL) finish_audio();
    } else {
        xlprot1(result);
        xlerror("sound_save: expression did not return a sound",
                 result);
        xlpop();
        max_sample = 0.0;
    }
    if (buf) free(buf);
    return max_sample;
}


/* open_for_write -- helper function for sound_overwrite */
/*
 * open the file and prepare to read it if it matches the expected
 * sample rate and number of channels
 */
SNDFILE *open_for_write(unsigned char *filename, long direction,
                        SF_INFO *sf_info, int channels, int srate,
                        double offset, float **buf)
/* channels and srate are based on the sound we're writing to the file */
{
    SNDFILE *sndfile;
    sf_count_t frames; // frame count passed into sf_seek
    char error[140];   // error messages are formatted here
    sf_count_t rslt;   // frame count returned from sf_seek

    sndfile = NULL;
    if (ok_to_open((char *) filename, "w"))
        sndfile = sf_open((const char *) filename, direction, sf_info);

    if (!sndfile) {
        snprintf(error, sizeof(error), "snd_overwrite: cannot open file %s",
                 filename);
        xlabort(error);
    }
    /* use proper scale factor: 8000 vs 7FFF */
    sf_command(sndfile, SFC_SET_CLIPPING, NULL, SF_TRUE);
    
    frames = ROUNDBIG(offset * sf_info->samplerate);
    rslt = sf_seek(sndfile, frames, SEEK_SET);
    if (rslt < 0) {
        snprintf(error, sizeof(error),
				 "snd_overwrite: cannot seek to frame %lld of %s",
                 (long long int) frames, filename);
        xlabort(error);
    }
    if (sf_info->channels != channels) {
        snprintf(error, sizeof(error), "%s%d%s%d%s",
                "snd_overwrite: number of channels in sound (",
                channels,
                ") does not match\n    number of channels in file (",
                sf_info->channels, ")");
        sf_close(sndfile);
        xlabort(error);
    }
    if (sf_info->samplerate != srate) {
        snprintf(error, sizeof(error), "%s%d%s%d%s",
                "snd_overwrite: sample rate in sound (",
                srate,
                ") does not match\n    sample rate in file (",
                sf_info->samplerate,
                ")");
        sf_close(sndfile);
        xlabort(error);
    }
    if ((*buf = (float *) malloc(max_sample_block_len * sf_info->channels *
                                 sizeof(float))) == NULL) {
        xlabort("snd_overwrite: couldn't allocate memory");
    }
    return sndfile;
}


double sound_overwrite(LVAL snd_expr, int64_t n, unsigned char *filename,
                       double offset_secs, double *duration, int64_t progress)
{
    LVAL result;       // the SOUND to be evaluated
    SF_INFO sf_info;   // info about the sound file
    double max_sample; // return value
    int64_t ntotal;       // how many samples were overwritten
    /*
    long flags;
    */
    // first check if sound file exists, do not create new file
    FILE *file = NULL;
    if (ok_to_open((char *) filename, "rb"))
        file = fopen((char *) filename, "rb");
    // if not then fail
    if (!file) {
        *duration = 0;
        return 0.0;
    } else {
        fclose(file);
    }
    memset(&sf_info, 0, sizeof(sf_info));
    result = xleval(snd_expr);
    /* BE CAREFUL - DO NOT ALLOW GC TO RUN WHILE RESULT IS UNPROTECTED */
    if (vectorp(result)) {
        SNDFILE *sndfile;  // opened sound file 
        float *buf; // buffer for samples read in from sound file
        /* make sure all elements are of type a_sound */
        long i = getsize(result);
        long channels = i;
        while (i > 0) {
            i--;
            if (!exttypep(getelement(result, i), a_sound)) {
                xlerror("sound_save: array has non-sound element",
                         result);
            }
        }
        sndfile = open_for_write(filename, SFM_RDWR, &sf_info, channels,
                                 ROUND32(getsound(getelement(result, 0))->sr),
                                 offset_secs, &buf);

        max_sample = sound_save_array(result, n, &sf_info, sndfile, 
                                      buf, &ntotal, progress);
        *duration = ntotal / (double) sf_info.samplerate;
        free(buf);
        sf_close(sndfile);
    } else if (exttypep(result, a_sound)) {
        SNDFILE *sndfile;  // opened sound file 
        float *buf; // buffer for samples read in from sound file
        sndfile = open_for_write(filename, SFM_RDWR, &sf_info, 1,
                                 ROUND32(getsound(result)->sr), 
                                 offset_secs, &buf);
        max_sample = sound_save_sound(result, n, &sf_info, sndfile, buf, 
                                      &ntotal, progress);
        *duration = ntotal / (double) sf_info.samplerate;
        free(buf);
        sf_close(sndfile);
    } else {
        xlerror("sound_save: expression did not return a sound",
                 result);
        max_sample = 0.0;
    }
    return max_sample;
}

int is_pcm(SF_INFO *sf_info)
{
    long subtype = sf_info->format & SF_FORMAT_SUBMASK;
    return (subtype == SF_FORMAT_PCM_S8 || subtype == SF_FORMAT_PCM_16 ||
            subtype == SF_FORMAT_PCM_24 || subtype == SF_FORMAT_PCM_32);
}


sample_type sound_save_sound(LVAL s_as_lval, int64_t n, SF_INFO *sf_info,
                             SNDFILE *sndfile, float *buf, int64_t *ntotal,
                             int64_t progress)
{
    int blocklen;
    sound_type s;
    int i;
    sample_type *samps;
    int64_t debug_unit;  /* print messages at intervals of this many samples */
    int64_t debug_count; /* next point at which to print a message */
    sample_type max_sample = 0.0F;
    sample_type threshold = 0.0F;
    double sound_srate;

    *ntotal = 0;
    /* if snd_expr was simply a symbol, then s now points to
        a shared sound_node.  If we read samples from it, then
        the sound bound to the symbol will be destroyed, so
        copy it first.  If snd_expr was a real expression that
        computed a new value, then the next garbage collection
        will reclaim the sound_node.  We need to make the new
        sound reachable by the garbage collector to that any
        lisp data reachable from the sound do not get collected.
        To make the sound reachable, we need to allocate a node,
        and the GC might run, so we need to protect the OLD s
        but then make it unreachable.
        We will let the GC collect the sound in the end.
    */
    xlprot1(s_as_lval);
    s = sound_copy(getsound(s_as_lval));
    s_as_lval = cvsound(s);	/* destroys only ref. to original */

    /* for debugging */
/*    printing_this_sound = s;*/

    if (progress < 10000) {
        progress = 10000;
    }
    debug_unit = debug_count =
            (int64_t) max(10 * sf_info->samplerate, progress);

    sound_frames = 0;
    sound_srate = sf_info->samplerate;

    while (n > 0) {
        long togo;
        sample_block_type sampblock = sound_get_next(s, &blocklen);
        oscheck();
#ifdef SNAPSHOTS
        stdputstr(".");
        if (sound_created_flag) {
            stdputstr("SNAPSHOT: ");
            sound_print_tree(printing_this_sound);
            sound_created_flag = false;
        }
        fflush(stdout);
#endif
        if (sampblock == zero_block || blocklen == 0) {
            break;
        }
        togo = (int) min(blocklen, n);
        if (s->scale != 1) { /* copy/scale samples into buf */
            for (i = 0; i < togo; i++) {
                buf[i] = s->scale * sampblock->samples[i];
            }
            samps = buf;
        } else {
            samps = sampblock->samples;
        }
        if (is_pcm(sf_info)) {
            for (i = 0; i < togo; i++) {
                sample_type s = samps[i];
                COMPUTE_MAXIMUM_AND_WRAP(samps[i]);
            }
        } else {
            for (i = 0; i < togo; i++) {
                sample_type s = samps[i];
                COMPUTE_MAXIMUM();
            }
        }
        if (sndfile) {
            sf_writef_float(sndfile, samps, togo);
        }
        if (audio_stream) {
            PaError err = Pa_WriteStream(audio_stream, samps, togo);
			if (err != paNoError) gprintf(TRANS, "Pa_WriteStream %d\n", err);
            sound_frames += togo;
        }

        n -= togo;
        *ntotal += togo;
        if (*ntotal > debug_count) {
            gprintf(TRANS, " %" PRId64 " ", *ntotal);
            fflush(stdout);
            debug_count += debug_unit;
        }
    }
    gprintf(TRANS, "\ntotal samples: %ld (%g seconds)\n", 
            *ntotal, *ntotal / sound_srate);
    xlpop();
    return max_sample;
}


sample_type sound_save_array(LVAL sa, int64_t n, SF_INFO *sf_info,
                             SNDFILE *sndfile, float *buf, int64_t *ntotal,
                             int64_t progress)
{
    long i, chans;
    float *float_bufp;
    sound_state_type state;
    double start_time = HUGE_VAL;
    LVAL sa_copy;
    int64_t debug_unit;    /* print messages at intervals of this many samples */
    int64_t debug_count;   /* next point at which to print a message */
    sample_type max_sample = 0.0F;
    sample_type threshold = 0.0F;
    double sound_srate;

    *ntotal = 0;

    /* THE ALGORITHM: first merge floating point samples from N channels
     * into consecutive multi-channel frames in buf.  Then, treat buf
     * as just one channel and use one of the cvt_to_* functions to
     * convert the data IN PLACE in the buffer (this is ok because the
     * converted data will never take more space than the original 32-bit
     * floats, so the converted data will not overwrite any floats before
     * the floats are converted
     */

    /* if snd_expr was simply a symbol, then sa now points to
        a shared sound_node.  If we read samples from it, then
        the sounds bound to the symbol will be destroyed, so
        copy it first.  If snd_expr was a real expression that
        computed a new value, then the next garbage collection
        will reclaim the sound array.  See also sound_save_sound()
    */

    chans = getsize(sa);
    if (chans > MAX_SND_CHANNELS) {
        xlerror("sound_save: too many channels", sa);
        free(buf);
        sf_close(sndfile);
    }
    // printf("DEBUG: xlstack on entry to sound_save_array: %p\n", xlstack);
    xlprot1(sa);
    sa_copy = newvector(chans);
    xlprot1(sa_copy);

    /* Why do we copy the array into an xlisp array instead of just
     * the state[i] array? Because some of these sounds may reference
     * the lisp heap. We must put the sounds in an xlisp array so that
     * the gc will find and mark them. xlprot1(sa_copy) makes the array
     * visible to gc.
     */
    for (i = 0; i < chans; i++) {
        sound_type s = getsound(getelement(sa, i));
        setelement(sa_copy, i, cvsound(sound_copy(s)));
    }
    sa = sa_copy;	/* destroy original reference to allow GC */

    state = (sound_state_type) malloc(sizeof(sound_state_node) * chans);
    for (i = 0; i < chans; i++) {
        state[i].sound = getsound(getelement(sa, i));
        state[i].scale = state[i].sound->scale;
D       nyquist_printf("save scale factor %ld = %g\n", i, state[i].scale);
        state[i].terminated = false;
        state[i].cnt = 0;   /* force a fetch */
        start_time = min(start_time, state[i].sound->t0);
    }

    for (i = 0; i < chans; i++) {
        if (state[i].sound->t0 > start_time)
            sound_prepend_zeros(state[i].sound, start_time);
    }

    if (progress < 10000) {
        progress = 10000;
    }
    debug_unit = debug_count =
            (int64_t) max(10 * sf_info->samplerate, progress);

    sound_frames = 0;
    sound_srate = sf_info->samplerate;
    while (n > 0) {
        /* keep the following information for each sound:
            has it terminated?
            pointer to samples
            number of samples remaining in block
           scan to find the minimum remaining samples and
           output that many in an inner loop.  Stop outer
           loop if all sounds have terminated
         */
        int terminated = true;
        int64_t togo = n; /* because of this, togo must be int64_t here */
        int j;

        oscheck();

        for (i = 0; i < chans; i++) {
            if (state[i].cnt == 0) {
                if (sndwrite_trace) {
                    nyquist_printf("CALLING SOUND_GET_NEXT ON "
                            "CHANNEL %ld (%p)\n",
				            i, state[i].sound); /* jlh 64 bit issue */
                            sound_print_tree(state[i].sound);
                }
                state[i].ptr = sound_get_next(state[i].sound,
                                   &(state[i].cnt))->samples;
                if (sndwrite_trace) {
                    nyquist_printf("RETURNED FROM CALL TO SOUND_GET_NEXT "
                                   "ON CHANNEL %ld\n", i);
                }
                if (state[i].ptr == zero_block->samples) {
                    state[i].terminated = true;
                }
            }
            if (!state[i].terminated) terminated = false;
            togo = min(togo, state[i].cnt);
        }
        /* note that now, togo is well within range of int because it is the
           min of state[i].cnt */

        if (terminated) break;

        float_bufp = (float *) buf;
        if (is_pcm(sf_info)) {
            for (j = 0; j < togo; j++) {
                for (i = 0; i < chans; i++) {
                    float s = (float) (*(state[i].ptr++) * (float) state[i].scale);
                    COMPUTE_MAXIMUM_AND_WRAP(s);
                    *float_bufp++ = s;
                }
            }
        } else {
            for (j = 0; j < togo; j++) {
                for (i = 0; i < chans; i++) {
                    float s = (float) (*(state[i].ptr++) * (float) state[i].scale);
                    COMPUTE_MAXIMUM();
                    *float_bufp++ = s;
                }
            }
        }
        /* Here we have interleaved floats. Before converting to the sound
           file format, call PortAudio to play them. */
        if (audio_stream) {
            PaError err = Pa_WriteStream(audio_stream, buf,
                                         (unsigned long) togo);
            if (err) {
                printf("Pa_WriteStream error %d\n", err);
            }
            sound_frames += togo;
        }
        if (sndfile) sf_writef_float(sndfile, buf, togo);

        n -= togo;
        for (i = 0; i < chans; i++) {
            state[i].cnt -= (int) togo;
        }
        *ntotal += togo;
        if (*ntotal > debug_count) {
            gprintf(TRANS, " %ld ", *ntotal);
            fflush(stdout);
            debug_count += debug_unit;
        }
    }
    gprintf(TRANS, "\ntotal samples: %ld x %ld channels (%g seconds)\n",
            *ntotal, chans, *ntotal / sound_srate);

    /* references to sounds are shared by sa_copy and state[].
     * here, we dispose of state[], allowing GC to do the
     * sound_unref call that frees the sounds. (Freeing them now
     * would be a bug.)
     */
    free(state);
    xlpopn(2);
    return max_sample;
}


