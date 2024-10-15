/* resamp.c -- resample signal using sinc interpolation */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  min->MIN, max->MAX
 */



#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "assert.h"

#include "falloc.h"
#include "cext.h"
#include "resamp.h"
#include "fresample.h"
#include "ffilterkit.h"
#include "fsmallfilter.h"

/* Algorithm:
   To resample, we convolve a sinc function with the input stream at
times corresponding to the output samples.  This requires a sliding
window on the input samples. Since samples are accessed a block at a
time, the places where the sliding window would span two blocks are
too tricky for me. Instead of trying to manage the breaks across 
blocks, I copy the blocks into another buffer (called X).  When the
sliding window reaches the end of X, the samples at the end of X
are copied to the beginning of X, the remainder of X is filled with
new samples, and the computation continues.  The trickiest part of
all this is keeping all the pointers and phase accumulators correct
when the sliding window is relocated from the end of X to the 
beginning.
    Although there are different ways to do this, I decided that the
output would always go directly to a Nyquist sample block, so the
resampling routines (SrcUp and SrcUD) are always called upon to 
compute max_sample_block_len samples (except that a partial block
may be computed when the input sound terminates).
    To compute max_sample_block_len samples, the input buffer needs:

  - max_sample_block_len/factor samples, where factor is the ratio of
    the new sample rate to the old one. I.e. if upsampling by a factor
    of 2, the input buffer needs half the samples of the output block
    size.

  - additional samples the size of the sliding window.  Since the
    output is taken from the center of the window, we can't take
    samples from the first or last windowsize/2 samples.

  - to simplify rounding, we throw in some extra samples. This costs
    only a bit of space and an extra copy for each spare sample.

    The window size is determined by the particular filter used and 
by factor (the sample rate ratio).  The filter size is Nmult, the 
number of filter coefficients. When upsampling, this is the window
size (the filter acts as a reconstruction filter for the additional
samples).  When downsampling, the filter is stretched by 1/factor
(the filter acts as an anti-aliasing low-pass filter).

*/

void resample_free(snd_susp_type a_susp);

typedef struct resample_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s;
    int s_cnt;
    sample_block_values_type s_ptr;
    float *X;
    long Xsize;
    double Time; /* location (offset) in X of next output sample */
    double LpScl;
    double factor;
    sample_type *Imp;
    sample_type *ImpD;
    boolean interpFilt;
    int Nmult;
    int Nwing;
    int Xp; /* first free location at end of X */
    int Xoff; /* number of extra samples at beginning and end of X */
} resample_susp_node, *resample_susp_type;

/* Sampling rate up-conversion only subroutine;
 * Slightly faster than down-conversion;
 */
static int SrcUp(float X[], float Y[], double factor, double *Time,
                 int Nx, int Nwing, double LpScl,
                 float Imp[], float ImpD[], boolean Interp)
{
    mem_float *Xp, *Ystart;
    fast_float v;
    
    double dt;                  /* Step through input signal */ 
    mem_float *Yend;             /* When Y reaches Yend, return to user */
    
/*    nyquist_printf("SrcUp: interpFilt %d\n", Interp);*/

    dt = 1.0/factor;            /* Output sampling period */
    
    Ystart = Y;
    Yend = Y + Nx;
    while (Y < Yend) {
        long iTime = (long) *Time;
        Xp = &X[iTime];      /* Ptr to current input sample */
        /* Perform left-wing inner product */
        v = FilterUp(Imp, ImpD, Nwing, Interp, Xp, *Time - iTime, -1);
        /* Perform right-wing inner product */
        v += FilterUp(Imp, ImpD, Nwing, Interp, Xp+1, 
                      (1 + iTime) - *Time, 1);
        v *= LpScl;		/* Normalize for unity filter gain */
/*	nyquist_printf("SrcUp output sample %g\n", v); */
        *Y++ = (float) v;
        *Time += dt;		/* Move to next sample by time increment */
    }
    return (int) (Y - Ystart);        /* Return the number of output samples */
}


/* Sampling rate conversion subroutine */

static int SrcUD(float X[], float Y[], double factor, double *Time,
                 int Nx, int Nwing, double LpScl,
                 float Imp[], float ImpD[], boolean Interp)
{
    mem_float *Xp, *Ystart;
    fast_float v;
    
    double dh;                  /* Step through filter impulse response */
    double dt;                  /* Step through input signal */
    mem_float *Yend;             /* When Y reaches Yend, return to user */
    
    dt = 1.0/factor;            /* Output sampling period */
    
    dh = MIN(Npc, factor*Npc);  /* Filter sampling period */
    
    Ystart = Y;
    Yend = Y + Nx;
    while (Y < Yend) {
        long iTime = (long) *Time;
        Xp = &X[iTime];		/* Ptr to current input sample */
        v = FilterUD(Imp, ImpD, Nwing, Interp, Xp, *Time - iTime,
                     -1, dh);	/* Perform left-wing inner product */
        v += FilterUD(Imp, ImpD, Nwing, Interp, Xp+1, (1 + iTime) - *Time,
                      1, dh);	/* Perform right-wing inner product */
        v *= LpScl;		/* Normalize for unity filter gain */
        *Y++ = (float) v;
        *Time += dt;		/* Move to next sample by time increment */
    }
    return (int) (Y - Ystart);        /* Return the number of output samples */
}


void resample__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    resample_susp_type susp = (resample_susp_type) a_susp;
    int togo;
    int Nout;
    sample_block_type out;
    /* note that in this fetch routine, out_ptr is used to remember where
     * to put the "real" output, while X_ptr_reg is used in the inner
     * loop that copies input samples into X, a buffer
     */
    register sample_block_values_type out_ptr;
    falloc_sample_block(out, "resample__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

/* Algorithm:
    Fetch samples until X (the buffered input) is full. X stores enough
contiguous samples that a sliding window convolving with the filter
coefficients can output a full block without sliding beyond the range
of X. Every time we reenter resample__fetch, we take the remaining
samples at the end of X, shift them to the beginning, and refill.
    After X is full, call on SrcUp or SrcUD to compute an output block.
    The first time resample__fetch is called, the fill pointer Xp will
point near the beginning of X, indicating that no previously read
samples need to be shifted from the end of X to the beginning.
*/

    /* first, shift samples from end of X to beginning if necessary */
    if (susp->Xp > 2 * susp->Xoff) {
        int i;
        int shiftcount = (long) (susp->Time) - susp->Xoff;

/*	nyquist_printf("shift %d from %d to %lx\n", susp->Xsize + susp->Xoff - susp->Xp, susp->Xp - susp->Xoff, susp->X); */
        for (i = 0; i < susp->Xp - shiftcount; i++) { 
            susp->X[i] = susp->X[i + shiftcount];
/*	    if (susp->X[i] == 0.0) nyquist_printf("shifted zero to X[%d]\n", i);*/
        }
        susp->Time -= shiftcount;
        susp->Xp -= shiftcount;
    }

    while (susp->Xp < susp->Xsize) { /* outer loop */
        /* read samples from susp->s into X */
        togo = (int) (susp->Xsize - susp->Xp);
        /* don't run past the s input sample block. If termination or
         * logical stop info become available, translate to susp->terminate_cnt
         * and susp->log_stop_cnt.
         */
        susp_check_term_log_samples(s, s_ptr, s_cnt);
        togo = (int) MIN(togo, susp->s_cnt);
        
        memcpy(susp->X + susp->Xp, susp->s_ptr, togo * sizeof(sample_type));
        susp->s_ptr += togo;
        susp_took(s_cnt, togo);
        susp->Xp += togo;
    } /* outer loop */

    /* second, compute samples to output, this is done in one pass because
     * we have enough data in X
     */

    /* don't run past terminate time */
    togo = max_sample_block_len;
    if (susp->terminate_cnt != UNKNOWN &&
        susp->terminate_cnt <= susp->susp.current + max_sample_block_len) {
        togo = (int) (susp->terminate_cnt - susp->susp.current);
    }
    if (!susp->logically_stopped &&
        susp->susp.log_stop_cnt != UNKNOWN) {
        int64_t to_stop = susp->susp.log_stop_cnt - susp->susp.current;
        assert(to_stop >= 0);
        if (to_stop < togo) {
            if (to_stop == 0) susp->logically_stopped = true;
            else togo = (int) to_stop;
        }
    }
    if (togo == 0) {
/*	stdputstr("resamp calling snd_list_terminate\n"); */
        snd_list_terminate(snd_list);
    } else {
        if (susp->factor >= 1) { /* SrcUp() is faster if we can use it */
            Nout = SrcUp(susp->X, out_ptr, susp->factor, &susp->Time, 
                         togo, susp->Nwing, susp->LpScl, susp->Imp, 
                         susp->ImpD, susp->interpFilt);
        } else {
           Nout = SrcUD(susp->X, out_ptr, susp->factor, &susp->Time, 
                        togo, susp->Nwing, susp->LpScl, susp->Imp, 
                        susp->ImpD, susp->interpFilt);
        }
        snd_list->block_len = togo;
        susp->susp.current += togo;
    }
#ifdef RESAMPTEST
    for (n = 0; n < snd_list->block_len; n++) {
        if (out->samples[n] == 0.0) {
            nyquist_printf("resamp: zero at samples[%d]\n", n);
        }
    }
#endif
/*
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
 */
} /* resample__fetch */


void resample_mark(snd_susp_type a_susp)
{
    resample_susp_type susp = (resample_susp_type) a_susp;
    sound_xlmark(susp->s);
}


void resample_free(snd_susp_type a_susp)
{
    resample_susp_type susp = (resample_susp_type) a_susp;
    sound_unref(susp->s);
    free(susp->X);
    ffree_generic(susp, sizeof(resample_susp_node), "resample_free");
}


void resample_print_tree(snd_susp_type a_susp, int n)
{
    resample_susp_type susp = (resample_susp_type) a_susp;
    indent(n);
    stdputstr("s:");
    sound_print_tree_1(susp->s, n);
}


sound_type snd_make_resample(sound_type s, rate_type sr)
{
    register resample_susp_type susp;
    int i;

    falloc_generic(susp, resample_susp_node, "snd_make_resample");
    susp->susp.fetch = resample__fetch;

    susp->Nmult = SMALL_FILTER_NMULT;
    susp->Imp = SMALL_FILTER_IMP;
    susp->ImpD = SMALL_FILTER_IMPD;
    /* these scale factors are here because filter coefficients
     are expressed as integers, and so is SMALL_FILTER_SCALE: */
    susp->LpScl = SMALL_FILTER_SCALE / 32768.0;
    susp->LpScl /= 16384.0;
    /* this is just a fudge factor, is SMALL_FILTER_SCALE  wrong? */
    susp->LpScl /= 1.0011;

    susp->Nwing = SMALL_FILTER_NWING;
    susp->factor = sr / s->sr;
    if (susp->factor < 1) susp->LpScl *= susp->factor;

    /* factor in the scale factor of s, since resample is linear */
    susp->LpScl *= s->scale;

    susp->terminate_cnt = UNKNOWN;
    /* initialize susp state */
    susp->susp.free = resample_free;
    susp->susp.sr = sr;
    susp->susp.t0 = s->t0;
    susp->susp.mark = resample_mark;
    susp->susp.print_tree = resample_print_tree;
    susp->susp.name = "resample";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s);
    susp->susp.current = 0;
    susp->s = s;
    susp->s_cnt = 0;
    susp->Xoff = (int) (((susp->Nmult + 1) / 2.0) * MAX(1.0, 1.0 / susp->factor) + 10);
    susp->Xsize = (long) ((max_sample_block_len / susp->factor) + 2 * susp->Xoff);
    susp->X = calloc(susp->Xsize, sizeof(sample_type));
    susp->Xp = susp->Xoff;
    susp->Time = susp->Xoff;
    susp->interpFilt = true;
    for (i = 0; i < susp->Xoff; i++) susp->X[i] = 0.0F;
    
    return sound_create((snd_susp_type)susp, susp->susp.t0,
                        susp->susp.sr, 1.0 /* scale factor */);
}


sound_type snd_resample(sound_type s, rate_type sr)
{
    sound_type s_copy = sound_copy(s);
    return snd_make_resample(s_copy, sr);
}
