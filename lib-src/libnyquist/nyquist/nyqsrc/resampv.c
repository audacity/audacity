/* resampv.c -- use sinc interpolation to resample at a time-varying sample rate */


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

#include "falloc.h"
#include "cext.h"
#include "resampv.h"
#include "fresample.h"
#include "ffilterkit.h"
#include "fsmallfilter.h"
#include "assert.h"


/* Algorithm:
 First compute a factor = ratio of new sample rate to original sample rate.
 We have Time, the offset into X
 We want Xoff = ((susp->Nmult + 1) / 2.0) * MAX(1.0, 1.0 / factor) + 10
 samples on either side of Time before we interpolate.
 If Xoff * 2 > Xsize, then we're in trouble because X is not big enough.
 Assume this is a pathalogical case, raise the cutoff frequency to
 reduce Xoff to less than Xsize/2.
 If Time is too small, then we're in trouble because we've lost the
 beginning of the buffer. Raise the cutoff frequency until Xoff is
 less than Time. This should only happen if factor suddenly drops.
 If Time is too big, we can handle it: shift X down and load X with new
 samples. When X is shifted by N samples, N is subtracted from Time.
 To minimize the "Time is too small" case, don't shift too far: leave
 a cushion of Xoff * 2 samples rather than the usual Xoff.

 Now compute a sample at Time using SrcUD and output it.

 What is Time?
 Time is the offset into X, so Time is g_of_now - (sum of all X shifts)
 So, let Time = g_of_now - shift_sum
 Whenever shift_sum or g_of_now is updated, recompute Time

 To compute the next g_of_now, do a lookup of g at now + 1/sr,
 using linear interpolation (be sure to do computation with
 doubles to minimize sampling time jitter).

 */

/* maximum ratio for downsampling (downsampling will still take place,
 * but the lowest prefilter cutoff frequency will be
 * (original_sample_rate/2) / MAX_FACTOR_INVERSE
 */
#define MAX_FACTOR_INVERSE 64

typedef struct resamplev_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type f;
    int f_cnt;
    sample_block_values_type f_ptr;

    sound_type g;
    int g_cnt;
    sample_block_values_type g_ptr;
    double prev_g;	/* data for interpolation: */
    double next_g;
    double phase_in_g;
    double phase_in_g_increment;
    double g_of_now;
    
    float *X;
    long Xsize;
    double Time; /* location (offset) in X of next output sample */
    double shift_sum; /* total amount by which we have shifted X; also, the
                         sample number of X[0] */
    double LpScl;
    double factor_inverse; /* computed at every sample from g */
    /* this is the amount by which we step through the input signal, so
       factor_inverse is the output_sample_rate / input_sample_rate, and
       factor is the input_sample_rate / output_sample_rate. Alternatively,
       factor is the amount to downsample and
       factor_inverse is the amount to upsample. */
    /* double factor; -- computed from factor_inverse */
    sample_type *Imp;
    sample_type *ImpD;
    boolean interpFilt;
    int Nmult;
    int Nwing;
    int Xp; /* first free location at end of X */
    int Xoff; /* number of extra samples at beginning and end of X */
} resamplev_susp_node, *resamplev_susp_type;

void resamplev_free(snd_susp_type a_susp);
void resampv_refill(resamplev_susp_type susp);

/* Sampling rate conversion subroutine
 * Note that this is not the same as SrcUD in resamp.c!
 * X[] is the input signal to be resampled,
 * dt is the ratio of sample rates; when dt=1, the skip size is
 *    Npc/dt = Npc, where Npc is how many filter coefficients to
 *    get the cutoff frequency equal to the Nyquist rate. As dt
 *    gets larger, we step through the filter more slowly, so low-pass
 *    filtering occurs. As dt gets smaller, it is X[] that limits
 *    frequency, and we use the filter to interpolate samples (upsample).
 *    Therefore, dt>1 means downsample, dt<1 means upsample.
 *    dt is how much we increment Time to compute each output sample.
 * Time is the offset in samples, including fractional samples, of X
 * Nwing is the size of one wing of the filter
 * LpScl is a corrective scale factor to make the gain == 1 or whatever
 *    (Nyquist uses a gain of 0.95 to minimize clipping when peaks are
 *    interpolated.)
 * Imp[] and ImpD[] are the filter coefficient table and table differences
 *    (for interpolation)
 * Interp is true to interpolate filter coefficient lookup
 */
static float SrcUD(float X[], double dt, double Time,
                 int Nwing, double LpScl,
                 float Imp[], float ImpD[], boolean Interp)
{
    mem_float *Xp;
    fast_float v;
    
    double dh;           /* Step through filter impulse response */
    long iTime = (long) Time;
    
    dh = MIN(Npc, Npc/dt);  /* Filter sampling period */
    
    Xp = &X[iTime];		/* Ptr to current input sample */
    v = FilterUD(Imp, ImpD, Nwing, Interp, Xp, Time - iTime,
                 -1, dh);	/* Perform left-wing inner product */
    v += FilterUD(Imp, ImpD, Nwing, Interp, Xp+1, (1 + iTime) - Time,
                  1, dh);	/* Perform right-wing inner product */
    v *= LpScl;		/* Normalize for unity filter gain */
    return (float) v;
}


void resamplev__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    resamplev_susp_type susp = (resamplev_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_block_type out;
    /* note that in this fetch routine, out_ptr is used to remember where
     * to put the "real" output, while X_ptr_reg is used in the inner
     * loop that copies input samples into X, a buffer
     */
    register sample_block_values_type out_ptr;

    falloc_sample_block(out, "resamplev__fetch");
    out_ptr = out->samples;
    snd_list->block = out;


    while (cnt < max_sample_block_len) { /* outer loop */
        /* fetch g until we have points to interpolate */
        while (susp->phase_in_g >= 1.0) {
            susp->prev_g = susp->next_g;
            if (susp->g_cnt == 0) {
                susp_get_samples(g, g_ptr, g_cnt);
                if (susp->g->logical_stop_cnt == susp->g->current - susp->g_cnt) {
                    if (susp->susp.log_stop_cnt == UNKNOWN) {
                        susp->susp.log_stop_cnt = susp->susp.current + cnt;
                    }
                }
                if (susp->g_ptr == zero_block->samples &&
                    susp->terminate_cnt == UNKNOWN) {
                    susp->terminate_cnt = susp->susp.current + cnt;
                }
            }
            susp->next_g = susp_fetch_sample(g, g_ptr, g_cnt);
            susp->phase_in_g -= 1.0;

            if (susp->next_g < susp->prev_g) {
                susp->next_g = susp->prev_g; // prevent time from going backward
            }
            /* factor_inverse = 1/factor = how many samples of f per
             * output sample = change-in-g / output-samples-per-g-sample
             * = change-in-g * phase_in_g_increment
             */
            susp->factor_inverse = susp->phase_in_g_increment *
                                   (susp->next_g - susp->prev_g);
            if (susp->factor_inverse > MAX_FACTOR_INVERSE)
                susp->factor_inverse = MAX_FACTOR_INVERSE;

            /* update Xoff, which depends upon factor_inverse: */
            susp->Xoff = (int) (((susp->Nmult + 1) / 2.0) *
                         MAX(1.0, susp->factor_inverse)) + 10;
            if (susp->Xoff * 2 > susp->Xsize) {
                /* bad because X is not big enough for filter, so we'll
                 * raise the cutoff frequency as necessary
                 */
                susp->factor_inverse = ((susp->Xsize / 2) - 10 ) /
                  ((susp->Nmult + 1) / 2.0);
                susp->Xoff = (susp->Xsize / 2) - 2 /* fudge factor */;
            }
        }
        susp->g_of_now = susp->prev_g +
          susp->phase_in_g * (susp->next_g - susp->prev_g);
        susp->Time = susp->g_of_now - susp->shift_sum;
        susp->phase_in_g += susp->phase_in_g_increment;

        /* now we have a position (g_of_now) and a factor */
        /* See if enough of f is in X */
        if (susp->Xoff > susp->Time) {
            /* there are not enough samples before Time in X, so
             * modify factor_inverse to fix it
             */
            susp->factor_inverse = (susp->Time - 10.0) /
                                   ((susp->Nmult + 1) / 2.0);
                           
        } else if ((susp->Xsize - susp->Xoff) < susp->Time) {
            /* Time is too close to the end of the buffer, slide the samples
               down. If there's room, leave 2*Xoff samples at beginning of
             * buffer. Otherwise leave as little as Xoff: */
            int i, dist, ntime;
            ntime = susp->Xoff * 2; /* shift Time near to this index in X */
            dist = ((int) susp->Time) - ntime;
            if (dist < 1 && (ntime * 2 < susp->Xsize)) {
                /* not enough room, so leave at least Xoff. */
                ntime = susp->Xoff;
                if (susp->Xsize / 2 - ntime > 2) {
                    /* There is some extra space. Use half to extend ntime, allowing
                       for a possible increase in Xoff that will require more history;
                       the other half reduces the amount of buffer copying needed. */
                    ntime += (susp->Xsize / 2 - ntime) / 2;
                }
                dist = ((int) susp->Time) - ntime;
            }
            /* shift everything in X by dist, adjust Time etc. */
            for (i = 0; i < susp->Xsize - dist; i++) { 
                susp->X[i] = susp->X[i + dist];
            }
            susp->Xp -= dist;
            resampv_refill(susp);
            susp->shift_sum += dist;
            susp->Time = susp->g_of_now - susp->shift_sum;
        }

        /* second, compute a sample to output */

        /* don't run past terminate time */
        if (susp->terminate_cnt == susp->susp.current + cnt) {
            snd_list->block_len = cnt;
            if (cnt > 0) {
                susp->susp.current += cnt;
                snd_list = snd_list->u.next;
                snd_list->u.next = snd_list_create(&susp->susp);
                snd_list->block = internal_zero_block;
                snd_list_terminate(snd_list);
            } else {
                snd_list_terminate(snd_list);
            }
            return;
        } else {
            double scale = susp->LpScl;
            float tmp;
            if (susp->factor_inverse > 1) scale /= susp->factor_inverse;
            tmp = SrcUD(susp->X, susp->factor_inverse,
                         susp->Time, susp->Nwing, scale, susp->Imp,
                         susp->ImpD, susp->interpFilt);
            *out_ptr++ = tmp;
        }
        cnt++;
    }
    snd_list->block_len = cnt;
    susp->susp.current += cnt;
    assert(cnt > 0);
} /* resamplev__fetch */


void resampv_refill(resamplev_susp_type susp) {
    int togo, n;
    register sample_type *f_ptr_reg;
    register sample_type *X_ptr_reg;
    
    while (susp->Xp < susp->Xsize) { /* outer loop */

        /* read samples from susp->f into X */
        togo = susp->Xsize - susp->Xp;

        /* don't run past the f input sample block: */
        susp_check_samples(f, f_ptr, f_cnt);
        togo = MIN(togo, susp->f_cnt);

        n = togo;
        f_ptr_reg = susp->f_ptr;
        X_ptr_reg = &(susp->X[susp->Xp]);
        if (n) do { /* the inner sample computation loop */
            *X_ptr_reg++ = *f_ptr_reg++;
        } while (--n); /* inner loop */

        /* using f_ptr_reg is a bad idea on RS/6000: */
        susp->f_ptr += togo;
        susp_took(f_cnt, togo);
        susp->Xp += togo;
    } /* outer loop */
}



void resamplev_mark(snd_susp_type a_susp)
{
    resamplev_susp_type susp = (resamplev_susp_type) a_susp;
    sound_xlmark(susp->f);
    sound_xlmark(susp->g);
}


void resamplev_free(snd_susp_type a_susp)
{
    resamplev_susp_type susp = (resamplev_susp_type) a_susp;
    sound_unref(susp->f);
    sound_unref(susp->g);
    free(susp->X);
    ffree_generic(susp, sizeof(resamplev_susp_node), "resamplev_free");
}


void resamplev_print_tree(snd_susp_type a_susp, int n)
{
    resamplev_susp_type susp = (resamplev_susp_type) a_susp;
    indent(n);
    nyquist_printf("f:");
    sound_print_tree_1(susp->f, n);

    indent(n);
    nyquist_printf("g:");
    sound_print_tree_1(susp->g, n);
}


sound_type snd_make_resamplev(sound_type f, rate_type sr, sound_type g)
{
    register resamplev_susp_type susp;
    int i;

    falloc_generic(susp, resamplev_susp_node, "snd_make_resamplev");
    susp->susp.fetch = resamplev__fetch;

    susp->Nmult = SMALL_FILTER_NMULT;
    susp->Imp = SMALL_FILTER_IMP;
    susp->ImpD = SMALL_FILTER_IMPD;
    susp->LpScl = SMALL_FILTER_SCALE / 32768.0;
    susp->LpScl /= 16384.0;
    /* this is just a fudge factor, is SMALL_FILTER_SCALE wrong? */
    susp->LpScl /= 1.0011;
    susp->Nwing = SMALL_FILTER_NWING;

    susp->terminate_cnt = UNKNOWN;
    /* initialize susp state */
    susp->susp.free = resamplev_free;
    susp->susp.sr = sr;
    susp->susp.t0 = f->t0;
    susp->susp.mark = resamplev_mark;
    susp->susp.print_tree = resamplev_print_tree;
    susp->susp.name = "resamplev";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(f);
    susp->susp.current = 0;
    susp->f = f;
    susp->f_cnt = 0;
    susp->g = g;
    susp->g_cnt = 0;
    susp->next_g = 0;
    susp->phase_in_g_increment = g->sr / sr;
    susp->phase_in_g = 2.0;
    /* can't use susp->factor because it is unknown and variable */
    /* assume at most a down-sample by a factor of 2.0 and compute Xoff accordingly */
    susp->Xoff = (int) (((susp->Nmult + 1) / 2.0) * 2.0) /* MAX(1.0, 1.0 / susp->factor) */ + 10;
    /* this size is not critical unless it is too small */
    /* allow the block size plus a buffer of 2*Xoff at both ends for the tails of the filter */
    susp->Xsize = max_sample_block_len + 4 * susp->Xoff;
    susp->X = calloc(susp->Xsize, sizeof(sample_type));
    susp->Xp = susp->Xsize;
    susp->shift_sum = -susp->Xsize;
    susp->interpFilt = true;
    for (i = 0; i < susp->Xoff; i++) susp->X[i] = 0.0F;
    susp->LpScl *= 0.95; /* reduce probability of clipping */
    
    return sound_create((snd_susp_type)susp, susp->susp.t0, susp->susp.sr, 
                        1.0 /* scale factor */);
}


sound_type snd_resamplev(sound_type f, rate_type sr, sound_type g)
{
    sound_type f_copy = sound_copy(f);
    sound_type g_copy = sound_copy(g);
    g_copy->scale *= (float) sr;	/* put g_copy in units of samples */
    return snd_make_resamplev(f_copy, sr, g_copy);
}
