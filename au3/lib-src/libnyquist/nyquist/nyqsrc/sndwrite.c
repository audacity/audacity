/* sndwrite.c -- write sounds to files */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

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
#include "sndwrite.h"
#include "extern.h"
#include "snd.h"
#ifdef UNIX
#include "sys/file.h"
/* #include <sys/stat.h>*/
/* #include <netinet/in.h> */
#else
#ifdef MACINTOSH
#include <unistd.h>
#include <stat.h>
#define L_SET SEEK_SET
#define L_INCR SEEK_CUR
#endif
#endif

#define D if (0) 

int sndwrite_trace = 0;	/* debugging */

sample_type sound_save_sound(LVAL s_as_lval, long n, snd_type snd,
                             char *buf, long *ntotal, snd_type player);

sample_type sound_save_array(LVAL sa, long n, snd_type snd, 
                             char *buf, long *ntotal, snd_type player);

unsigned char st_linear_to_ulaw(int sample);


typedef struct {
    sound_type sound;
    long cnt;
    sample_block_values_type ptr;
    double scale;
    int terminated;
} sound_state_node, *sound_state_type;


LVAL prepare_audio(LVAL play, snd_type snd, snd_type player)
{
    long flags;
    if (play == NIL) return NIL;
    player->format = snd->format;
    player->u.audio.devicename[0] = 0;
    player->u.audio.interfacename[0] = 0;
    if (snd_open(player, &flags) != SND_SUCCESS) {
        xlabort("snd_save -- could not open audio output");
    }
    /* make sure player and snd are compatible -- if not, set player to NULL
     * and print a warning message
     */
    if (player->format.channels == snd->format.channels &&
        player->format.mode == snd->format.mode &&
        player->format.bits == snd->format.bits) {
        /* ok so far, check out the sample rate */
        if (player->format.srate != snd->format.srate) {
            char msg[100];
            sprintf(msg, "%s(%g)%s(%g).\n",
                    "Warning: file sample rate ", snd->format.srate,
                    " differs from audio playback sample rate ",
                    player->format.srate);
            stdputstr(msg);
        }
    } else {
        stdputstr("File format not supported by audio output.\n");
        return NIL;
    }
    return play;
}


/* finish_audio -- flush the remaining samples, then close */
/**/
void finish_audio(snd_type player)
{
    /* note that this is a busy loop! */
    while (snd_flush(player) != SND_SUCCESS) ;
    snd_close(player);
}


/* write_to_audio -- handle audio output from buffer */
/*
 * We want to write as soon as space is available so that
 * a full sound buffer can be queued up for output. This
 * may require transferring only part of buf, so we keep
 * track of progress and output whenever space is available.
 */
void write_to_audio(snd_type player, void *buf, long buflen)
{
    long rslt;
    while (buflen) {
            /* this loop is a busy-wait loop! */
        rslt = snd_poll(player); /* wait for buffer space */
        rslt = min(rslt, buflen);
        if (rslt) {
            snd_write(player, buf, rslt);
            buf = (void *) ((char *) buf + 
                            (rslt * snd_bytes_per_frame(player)));
            buflen -= rslt;
        }
    }    
}


double sound_save(
  LVAL snd_expr,
  long n,
  unsigned char *filename,
  long format,
  long mode,
  long bits,
  long swap,
  double *sr,
  long *nchans,
  double *duration,
  LVAL play)
{
    LVAL result;
    char *buf;
    long ntotal;
    double max_sample;
    snd_node snd;
    snd_node player;
    long flags;

    snd.device = SND_DEVICE_FILE;
    snd.write_flag = SND_WRITE;
    strcpy(snd.u.file.filename, (char *) filename);
    snd.u.file.file = -1;	/* this is a marker that snd is unopened */
    snd.u.file.header = format;
    snd.format.mode = mode;
    snd.format.bits = bits;
    snd.u.file.swap = swap;

    player.device = SND_DEVICE_AUDIO;
    player.write_flag = SND_WRITE;
    player.u.audio.devicename[0] = '\0';
    player.u.audio.descriptor = NULL;
    player.u.audio.protocol = SND_COMPUTEAHEAD;
    player.u.audio.latency = 1.0;
    player.u.audio.granularity = 0.0;

    if ((buf = (char *) malloc(max_sample_block_len * MAX_SND_CHANNELS *
                                  sizeof(float))) == NULL) {
        xlabort("snd_save -- couldn't allocate memory");
    }

    result = xleval(snd_expr);
    /* BE CAREFUL - DO NOT ALLOW GC TO RUN WHILE RESULT IS UNPROTECTED */
    if (vectorp(result)) {
        /* make sure all elements are of type a_sound */
        long i = getsize(result);
        *nchans = snd.format.channels = i;
        while (i > 0) {
            i--;
            if (!exttypep(getelement(result, i), a_sound)) {
                xlerror("sound_save: array has non-sound element",
                         result);
            }
        }
        /* assume all are the same: */
        *sr = snd.format.srate = getsound(getelement(result, 0))->sr; 

        /* note: if filename is "", then don't write file; therefore,
         * write the file if (filename[0])
         */ 
        if (filename[0] && snd_open(&snd, &flags) != SND_SUCCESS) {
            xlabort("snd_save -- could not open sound file");
        }
        
        play = prepare_audio(play, &snd, &player);

        max_sample = sound_save_array(result, n, &snd,
                         buf, &ntotal, (play == NIL ? NULL : &player));
        *duration = ntotal / *sr;
        if (filename[0]) snd_close(&snd);
        if (play != NIL) finish_audio(&player);
    } else if (exttypep(result, a_sound)) {
        *nchans = snd.format.channels = 1;
        *sr = snd.format.srate = (getsound(result))->sr;
        if (filename[0] && snd_open(&snd, &flags) != SND_SUCCESS) {
            xlabort("snd_save -- could not open sound file");
        }

        play = prepare_audio(play, &snd, &player);

        max_sample = sound_save_sound(result, n, &snd,
                        buf, &ntotal, (play == NIL ? NULL : &player));
        *duration = ntotal / *sr;
        if (filename[0]) snd_close(&snd);
        if (play != NIL) finish_audio(&player);
    } else {
        xlerror("sound_save: expression did not return a sound",
                 result);
        max_sample = 0.0;
    }
    free(buf);
    return max_sample;
}


double sound_overwrite(
  LVAL snd_expr,
  long n,
  unsigned char *filename,
  long byte_offset,
  long header,
  long mode,
  long bits,
  long swap,
  double sr,
  long nchans,
  double *duration)
{
    LVAL result;
    char *buf;
    char error[140];
    long ntotal;
    double max_sample;
    snd_node snd;
    long flags;

    snd.device = SND_DEVICE_FILE;
    snd.write_flag = SND_OVERWRITE;
    strcpy(snd.u.file.filename, (char *) filename);
    snd.u.file.header = header;
    snd.u.file.byte_offset = byte_offset;
    snd.format.channels = nchans;
    snd.format.mode = mode;
    snd.format.bits = bits;
    snd.u.file.swap = swap;
    snd.format.srate = sr;

    if ((buf = (char *) malloc(max_sample_block_len * MAX_SND_CHANNELS *
                                  sizeof(float))) == NULL) {
        xlabort("snd_overwrite: couldn't allocate memory");
    }

    if (snd_open(&snd, &flags) != SND_SUCCESS) {
        sprintf(error,
                "snd_overwrite: cannot open file %s and seek to %d", 
                filename, (int)byte_offset);
        free(buf);
        xlabort(error);
    }

    result = xleval(snd_expr);
    /* BE CAREFUL - DO NOT ALLOW GC TO RUN WHILE RESULT IS UNPROTECTED */
    if (vectorp(result)) {
        /* make sure all elements are of type a_sound */
        long i = getsize(result);
        if (nchans != i) {
            sprintf(error, "%s%d%s%d%s", 
                    "snd_overwrite: number of channels in sound (",
                    (int)i,
                    ") does not match\n    number of channels in file (",
                    (int)nchans,
                    ")");
            free(buf);
            snd_close(&snd);
            xlabort(error);
        }
        while (i > 0) {
            i--;
            if (!exttypep(getelement(result, i), a_sound)) {
                free(buf);
                snd_close(&snd);
                xlerror("sound_save: array has non-sound element",
                         result);
            }
        }
        /* assume all are the same: */
        if (sr != getsound(getelement(result, 0))->sr) {
            sprintf(error, "%s%g%s%g%s",
                    "snd_overwrite: sample rate in sound (",
                    getsound(getelement(result, 0))->sr,
                    ") does not match\n    sample rate in file (",
                    sr,
                    ")"); 
            free(buf);
            snd_close(&snd);
            xlabort(error);
        }
        
        max_sample = sound_save_array(result, n, &snd, buf, &ntotal, NULL);
        *duration = ntotal / sr;
    } else if (exttypep(result, a_sound)) {
        if (nchans != 1) {
            sprintf(error, "%s%s%d%s", 
                    "snd_overwrite: number of channels in sound (1",
                    ") does not match\n    number of channels in file (",
                    (int)nchans,
                    ")");
            free(buf);
            snd_close(&snd);
            xlabort(error);
        }
            
        if (sr != getsound(result)->sr) {
            sprintf(error, "%s%g%s%g%s",
                    "snd_overwrite: sample rate in sound (",
                    getsound(result)->sr,
                    ") does not match\n    sample rate in file (",
                    sr,
                    ")"); 
            free(buf);
            snd_close(&snd);
            xlabort(error);
        }
        
        max_sample = sound_save_sound(result, n, &snd, buf, &ntotal, NULL);
        *duration = ntotal / sr;
    } else {
        free(buf);
        snd_close(&snd);
        xlerror("sound_save: expression did not return a sound",
                 result);
        max_sample = 0.0;
    }
    free(buf);
    snd_close(&snd);
    return max_sample;
}


cvtfn_type find_cvt_to_fn(snd_type snd, char *buf)
{
    cvtfn_type cvtfn;
    /* find the conversion function */
    if (snd->format.bits == 8) cvtfn = cvt_to_8[snd->format.mode];
    else if (snd->format.bits == 16) cvtfn = cvt_to_16[snd->format.mode];
    else if (snd->format.bits == 24) cvtfn = cvt_to_24[snd->format.mode];
    else if (snd->format.bits == 32) cvtfn = cvt_to_32[snd->format.mode];
    else cvtfn = cvt_to_unknown;

    if (cvtfn == cvt_to_unknown) {
        char error[50];
        sprintf(error, "Cannot write %d-bit samples in mode %s",
                (int)snd->format.bits, snd_mode_to_string(snd->format.mode));
        free(buf);
        snd_close(snd);
        xlabort(error);
    }
    return cvtfn;
}


sample_type sound_save_sound(LVAL s_as_lval, long n, snd_type snd,
                             char *buf, long *ntotal, snd_type player)
{
    int blocklen;
    long buflen;
    sound_type s;
    long debug_unit;    /* print messages at intervals of this many samples */
    long debug_count;   /* next point at which to print a message */
    sample_type max_sample = 0.0F;
    cvtfn_type cvtfn;
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


    debug_unit = debug_count = (long) max(snd->format.srate, 10000.0);

    cvtfn = find_cvt_to_fn(snd, buf);

#ifdef MACINTOSH
    if (player) {
        gprintf(TRANS, "Playing audio: Click and hold mouse button to stop playback.\n");
    }
#endif

    while (n > 0) {
        long togo;
        float peak;
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
        togo = min(blocklen, n);

        buflen = (*cvtfn)((void *) buf, (void *) sampblock->samples,
                          togo, s->scale, &peak);
        if (peak > max_sample) max_sample = peak;

#ifdef MACINTOSH
        if (Button()) {
            if (player) {
                snd_reset(player);
            }
            gprintf(TRANS, "\n\nStopping playback...\n\n\n");
            break;
        }
#endif

        if (snd->u.file.file != -1) snd_write(snd, (void *) buf, buflen);
        if (player) write_to_audio(player, (void *) buf, buflen);

        n -= togo;
        *ntotal += togo;
        if (*ntotal > debug_count) {
            gprintf(TRANS, " %d ", *ntotal);
            fflush(stdout);
            debug_count += debug_unit;
        }
    }
    gprintf(TRANS, "\ntotal samples: %d (%g seconds)\n",
                   *ntotal, *ntotal / snd->format.srate);
    xlpop();
    return max_sample;
}


sample_type sound_save_array(LVAL sa, long n, snd_type snd, 
                             char *buf, long *ntotal, snd_type player)
{
    long i, chans;
    long buflen;
    sound_state_type state;
    double start_time = HUGE_VAL;
    float *float_bufp;
    LVAL sa_copy;
    long debug_unit;    /* print messages at intervals of this many samples */
    long debug_count;   /* next point at which to print a message */
    sample_type max_sample = 0.0F;
    cvtfn_type cvtfn;

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
        snd_close(snd);
    }
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
D       nyquist_printf("save scale factor %d = %g\n", (int)i, state[i].scale);
        state[i].terminated = false;
        state[i].cnt = 0;   /* force a fetch */
        start_time = min(start_time, state[i].sound->t0);
    }

    for (i = 0; i < chans; i++) {
        if (state[i].sound->t0 > start_time)
            sound_prepend_zeros(state[i].sound, start_time);
    }

    /* for debugging */
/*    printing_this_sound = s;*/

    cvtfn = find_cvt_to_fn(snd, buf);

#ifdef MACINTOSH
    if (player) {
        gprintf(TRANS, "Playing audio: Click and hold mouse button to stop playback.\n");
    }
#endif

    debug_unit = debug_count = (long) max(snd->format.srate, 10000.0);

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
        int togo = n;
        int j;
        float peak;

        oscheck();

        for (i = 0; i < chans; i++) {
            if (state[i].cnt == 0) {
                if (sndwrite_trace) {
                    nyquist_printf("CALLING SOUND_GET_NEXT "
                                   "ON CHANNEL %d (%p)\n",
                                   (int)i, state[i].sound);
                    sound_print_tree(state[i].sound);
                }
                state[i].ptr = sound_get_next(state[i].sound,
                                   &(state[i].cnt))->samples;
                if (sndwrite_trace) {
                    nyquist_printf("RETURNED FROM CALL TO SOUND_GET_NEXT "
                                   "ON CHANNEL %d\n", (int)i);
                }
                if (state[i].ptr == zero_block->samples) {
                    state[i].terminated = true;
                }
            }
            if (!state[i].terminated) terminated = false;
            togo = min(togo, state[i].cnt);
        }

        if (terminated) break;

        float_bufp = (float *) buf;
        for (j = 0; j < togo; j++) {
            for (i = 0; i < chans; i++) {
                double s = *(state[i].ptr++) * state[i].scale; 
                *float_bufp++ = (float) s;
            }
        }
        // we're treating sound as mono for the conversion, so multiply
        // togo by chans to get proper number of samples, and divide by
        // chans to convert back to frame count required by snd_write
        buflen = (*cvtfn)((void *) buf, (void *) buf, togo * chans, 1.0F, 
                          &peak) / chans;
        if (peak > max_sample) max_sample = peak;
#ifdef MACINTOSH
        if (Button()) {
            if (player) {
                snd_reset(player);
            }
            gprintf(TRANS, "\n\nStopping playback...\n\n\n");
            break;
        }
#endif

        if (snd->u.file.file != -1) snd_write(snd, (void *) buf, buflen);
        if (player) write_to_audio(player, (void *) buf, buflen);

        n -= togo;
        for (i = 0; i < chans; i++) {
            state[i].cnt -= togo;
        }
        *ntotal += togo;
        if (*ntotal > debug_count) {
            gprintf(TRANS, " %d ", *ntotal);
            fflush(stdout);
            debug_count += debug_unit;
        }
    }
    gprintf(TRANS, "total samples: %d x %d channels (%g seconds)\n",
            *ntotal, chans, *ntotal / snd->format.srate);

    /* references to sounds are shared by sa_copy and state[].
     * here, we dispose of state[], allowing GC to do the
     * sound_unref call that frees the sounds. (Freeing them now
     * would be a bug.)
     */
    free(state);
    xlpopn(2);
    return max_sample;
}


