/* phasevocoder.c -- this is a time-stretching phase vocoder.

Design notes: we will use the "absolute" interface of the cmupv library.
"Absolute" means that rather than giving the phase vocoder a hop size
with which to move through the input (thus input position is
"relative"), a callback gives an exact input location. Thus, the input
parameters to the phase vocoder will be the input sound and a mapping
from output time to input time. For each frame of output, we'll get a 
callback asking for input. In the callback, we'll evaluate the mapping
up to the frame's center time, using interpolation if necessary, and
then evaluate the input sound as needed to find an input frame at the
mapped time.

Because Nyquist sounds are computed incrementally, the phase vocoder
input position must be non-decreasing. This will result in an
interface similar to the sound-warp function. The documentation for
sound-warp has a detailed explanation of the warp-fn parameter that
maps between "score" time and real time.

It should be possible to build on the phase vocoder to provide pitch
manipulation as well as time stretching, including using high quality
resampling provided in sound-warp. Extending the sound-warp interface,
suppose we have two control functions. One, called warp-fn maps from
"score" time to real time (as in sound-warp). Another, called pitch-fn
provides a frequency scale factor (>0) as a function of "score" time.

For example, supposed the input is 10s long. To transpose continuously
from 0 to 12 semitones, the pitch-fn can be pwev(1, 10, 2), generating
an exponential sweep from 1 to 2 over the course of the sound. To
simultaneously slow the tempo gradually by a factor of 2, let's use
the same function, pwev(1, 10, 2). If we integrate, we get a function
from score time to real time (warp-fn), so taking the inverse, we get
a function from real time to score time, which can be used to find the
location of each input frame for processing. 

This is not really the solution because we want to incorporate
additional stretching to allow for resampling to change the pitch.
Consider the construction of time stretching and pitch shifting
functions that simultaneously produce the right time mappings and
pitch shifting. We will first apply the phase vocoder, then apply
resampling. 
Let V(u) be the mapping from phase vocoder output time to input time,
and let Vinv(t) be the inverse ov V(u).
Let W(u) be the mapping from phase vocoder output time to final output
time, i.e. this is the sound-warp mapping.
Let S(t) be the stretch factor to be applied at time t (of the input).
Let P(t) be the pitch transposition to be applied at time t (of the input).
We can compute Vinv(t) by considering the following: At time t, the 
signal must be stretched by the product S(t)*P(t) because S(t) is the 
stretch factor, and because we need to stretch by an additional factor
of P(t) so that when we resample to achieve pitch transposition,
effectively stretching by 1/P(t), the net stretch due to transposition
will be 1.
Thus, Vinv(t) = Integral[S(t)*P(t)]. V(u) is derived by taking the 
inverse of Vinv(t), a primitive operation in Nyquist.
Now we need W(u). The input to the sound-warp (resampling) function
will have the pitch of the original signal because the phase vocoder
preserves pitch. At each point u, the pitch change applied to the
signal will be the inverse of the derivative of W(u):
   Pitch change at u = 1/W'(u)
The "pitch change at u" is P(V(u)) and we know V, so we can write
W'(u) = 1/P(V(u))
thus, W(u) = Integral[1/P(V(u))]


INTERFACE WTIH CMUPV
--------------------

f is the input sound
g is the map from output to input

Samples are computed by pv_fetch which has a state[] field
available as well as an interface to get samples from input
signals. The state[] is only accessible to pv_fetch because
it is inside a pvshell_susp_node, which is local within 
pvshell.c. (This may not be the best design.) Therefore, to
create the phase vocoder object and save a pointer in the 
state, we test for the first call to pv_fetch and do some
initialization there instead of in snd_vocoder where the 
suspension is created.

Output is taken from OUTPUT as needed until REMAINING is zero.
Then, pv_get_output2() is called to generate more samples.
pv_get_output2() calls the callback, which does most of the complex work.
(1) The callback must figure out the "time" of the next frame it will
    generate. This will be based on out_count provided to the callback.
(2) Map this time via g to an input time for f and convert to samples.
(3) Subtract framesize / 2 to get the first_sample we need from f.
(4) f_count is the total sample count for the end of input so the beginning
    of input is at f_count - fftsize.  first_sample is the place we want
    to start the next frame, so we need to skip over 
    first_sample - (f_count - fftsize) samples. 
(5) fill the rest of input from f.

Logical Stop and Terminate Logic
--------------------------------
The logical stop time should be the logical stop time of the input (f)
mapped to the output. Since g is a map from output time to input time, we want
        g(output.lst) = f.lst, or output.lst = g-inverse(f.lst)
    In practice, we're not given g-inverse and would like not to compute
it. We iterate through g to find g(t) for each fft frame center time. When
we reach the logical stop time of the input, detected by PVSHELL_TEST_F
returning PVSHELL_FLAG_LOGICAL_STOP, we can set the logical stop time of
the output by linearly interpolation. We save previous time points in g as
t0,g0 and t1,g1, where g0 = g(t0) and g1 = g(t1). We have the logical stop
time of f that we'll call g2 and we want the corresponding t2:
        (t1 - t0)/(t2 - t0) = (g1 - g0)/(g2 - g0), so
        (t1 - t0) = (t2 - t0) * (g1 - g0)/(g2 - g0), so
        t1 = t0 + (t2 - t0) * (g1 - g0)/(g2 - g0), where
        t1 is the logical stop time.
    The logical stop time can also be the terminate time of g -- if g 
terminates, we must terminate the output (otherwise we'll be reading from
time 0 of the input, but we're not allowed to go backward.)

The terminate time is when the remaining output will be zero. Since the
phase vocoder output continues for half a window beyond the last point
mapped from input to output, we really don't want to try to do any mapping.
Instead, we just wait until the input is all zeros and figure out when the
output will be all zeros.

Input becomes all zero when either we get a frame past the terminate time
of the input f, or we reach beyond the terminate time of g. Either way, we
should set a flag saying input has terminated and will be all zero.

Output becomes all zero fftsize / 2 - hopsize beyond the time point of the
first all-zero frame: Let's say we see the flag saying the input is all
zero because we've terminated on the input side. The *previous* frame was
therefore the last non-zero signal, and it extends for fftsize/2, but it was
one hopsize ago, so the non-zero signal extends fftsize/2 - hopsize from the
time of the all-zero frame.

Access to PV state
------------------

Things start with a call to snd_phasevocoder(f, g, fftsize, hopsize). The
info is put into pv_state_node, which is passed to pvshell and copied into
susp->pvshell.state. The fetch function is pvshell_fetch, which calls 
pv_fetch through the pointer susp->pvshell.h. h (which is pv_fetch) returns
flags to indicate logical stop and terminate, and it returns n, the number
of samples computed. If the terminate flag is set, the output is assumed to
be zero and the zero block is used.

The susp info and the pv_state_node info can be accessed in pv_fetch, but
the phase vocoder computation is in a callback. However, the parameter to
the callback is the susp pointer, so in the callback we can access the
pvshell_type and the pvstate_type data.

To return the flags, we have to stuff data into the
pvstate_type struct and read it back out in pv_fetch after calling
pv_get_output2(), which is the phase vocoder calculation that calls the
callback.

TODO: if g0 and t0 are not initialized because of early logical stop, 
what do we do?
*/

#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "pvshell.h"

#include "phasevocoder.h"
#include "cmupv.h"

typedef struct pvstate_struct {
    int64_t f_count;  /* how many samples have we taken from f? */
    int64_t g_count;  /* how many samples have we taken from g? */
    double g_prev; /* the previous value of g (at g_count - 2) */
    double g_next; /* the current value of g (at g_count - 1) */
    int64_t sample_count; /* how many total samples computed, specifically
                        * the number of samples copied into Nyquist
                        * sample blocks via *out++ = pvs->output[index++];
                        */
    Phase_vocoder *pv;   /* the phase vocoder object */
    sample_type *input;  /* a frame of samples to go into fft */
    int64_t input_count;    /* sample number of first sample in input */
    sample_type *output; /* output from phase vocoder */
    long output_count;   /* since we deliver samples on demand, 
            output_count keeps track of how much is left in output.
            ouput[OUTPUT_SIZE - output_count] is the next sample to deliver */
    int fftsize;         /* the length of an fft frame */
    int hopsize;         /* the hopsize -- not used */
    int mode;            /* the mode -- see cmupv.h */
    /* data to compute logical stop time */
    int64_t t0; /* output sample count of previous frame */
    double g0; /* input time of previous frame center */
    /* data to detect termination */
    long f_terminated;   /* set when f terminates */
    int64_t f_terminate_count; /* sample count of f when it terminates */
    long g_terminated;   /* set when g terminates */
    int64_t g_terminate_count; /* sample count of g when it terminates */
    /* return values from pv_callback */
    long flags; /* logical stop and terminate flags */
    int64_t logical_stop_count; /* sample count of output logical stop */
    int64_t terminate_count; /* sample count of output terminate time */
} pvstate_node, *pvstate_type;

#define OUTPUT_SIZE 256

int pv_callback(long out_count, float *samples, int len, void *rock)
{
    pvshell_type susp = (pvshell_type) rock;
    pvstate_type pvs = (pvstate_type) susp->state;

    /* (1) figure out the "time" of the start of next frame */
    double out_time = out_count / susp->f->sr;
    /* (2) Map this time via g to an input time for f. */
    /* compute g count that is past the time; at 0th sample, 
     * pvs->g_count is 1, so we add 1 to g_count to make the loop and 
     * interpolation math work right */
    double g_count = out_time * susp->g->sr + 1.0;
    double g; /* the value of g at g_count which is at the time of out_count */
    int64_t f_start; /* the start sample of input f for the next frame */
    int hop; /* the hopsize from the previous frame to this frame, thus the
                offset into input buffer of the data we want to keep */
    int got_from_f; /* samples already in input */
    int needed_from_f; /* samples to get from f this time */
    sample_type *input = pvs->input;
    int i;
    int f_logically_stopped = FALSE;
    int64_t f_logical_stop_count;
    pvs->flags = 0;
    /* before loop: 
     *    pvs->g_count <= g_count,
     * loop invariant: 
     *    pvs->g_prev == g(pvs->g_count - 2),
     *    pvs->g_prev == g(pvs->g_count - 1)
     * after loop:
     *    pvs->g_count > g_count
     *    pvs->g_count <= g_count + 1
     */
    while (pvs->g_count <= g_count) {
        long flags = PVSHELL_TEST_G(susp); /* prepare to get a sample */
        if (!pvs->g_terminated && (flags & PVSHELL_FLAG_TERMINATE)) {
            pvs->g_terminated = TRUE;
            pvs->g_terminate_count = susp->g->current - susp->g_cnt;
        }
        pvs->g_prev = pvs->g_next;
        pvs->g_next = PVSHELL_FETCH_G(susp);
        pvs->g_count++;
    }
    /* fetch frame by mapping with g unless we've gone beyond g's
       termination time */
    if (!pvs->g_terminated) {
        /* now interpolate to get the value of g at g_count */
        g = pvs->g_prev + (pvs->g_next - pvs->g_prev) *
                          (g_count - (pvs->g_count - 1));
        /* (3) get the first sample we need from f. */
        /* g is now the sample time we want for center of f window */
        f_start = ROUNDBIG(g * susp->f->sr) - pvs->fftsize / 2;

        /* f_start is now the first sample position of the window */
        /* (4) shift INPUT */
        hop = (int) (f_start - pvs->input_count);
        if (hop < 0) {
            hop = 0;
        }

        /* printf("pv_callback f_start %ld hop %d\n", f_start, hop); */

        got_from_f = pvs->fftsize - hop;
        needed_from_f = pvs->fftsize; /* unless we can resuse samples */
        if (hop == 0) {
            ; /* nothing to do, the samples are already in input */
        } else if (hop < pvs->fftsize) {
            memmove(input, input + hop,
                    got_from_f * sizeof(sample_type));
            needed_from_f = hop;
        } else { /* skip over some samples of f */
            int skip = hop - pvs->fftsize;
            int i;
            got_from_f = 0;
            for (i = 0; i < skip; i++) {
                long flags = PVSHELL_TEST_F(susp);
                if (flags) { /* normal case is all flags zero, so I think it
                                is faster to test for either and only if we
                                know one is set do we test individual flags */
                    if (flags | PVSHELL_FLAG_LOGICAL_STOP) {
                        f_logically_stopped = TRUE;
                        f_logical_stop_count = susp->f->current - susp->f_cnt;
                    }
                    if (flags | PVSHELL_FLAG_TERMINATE && !pvs->f_terminated) {
                        pvs->f_terminated = TRUE;
                        pvs->f_terminate_count = susp->f->current - susp->f_cnt;
                    }
                }
                PVSHELL_FETCH_F(susp);
            }
        }
        pvs->input_count = f_start;
        /* (5) fill the rest of input from f */
        for (i = 0; i < needed_from_f; i++) {
            long flags = PVSHELL_TEST_F(susp);
            if (!f_logically_stopped && (flags | PVSHELL_FLAG_LOGICAL_STOP)) {
                f_logically_stopped = TRUE;
                pvs->logical_stop_count = susp->f->current - susp->f_cnt;
            }
            input[got_from_f++] = PVSHELL_FETCH_F(susp);
        }
        memmove(samples, input, pvs->fftsize * sizeof(float));
        /* did we terminate? If window is all zeros, we can compute 
           terminate time */
        if ((!(pvs->flags & PVSHELL_FLAG_TERMINATE)) && pvs->f_terminated && 
            pvs->f_terminate_count <= f_start) {
            /* new window is all zero, so output terminates soon ... */
            pvs->flags |= PVSHELL_FLAG_TERMINATE;
            pvs->terminate_count = out_count - hop + pvs->fftsize / 2;
            /* printf("pv_callback terminated by f at %ld\n", pvs->terminate_count); */
        }
        pvs->t0 = out_count;
        pvs->g0 = g;
    } else { /* g has terminated, so we just fill input with zeros */
             /* hopsize does not matter, so we'll set it to fftsize/8 */
        memset(samples, 0, pvs->fftsize * sizeof(*samples));
        hop = pvs->fftsize / 8;
        /* printf("filled samples with 0, hop %d\n", hop); */
    }
    /* there are two sources of logical stop: f and g. If f, then
       f_logically_stopped is TRUE, and we need to map using g-inverse.
       We'll do that first to get a candidate logical stop time. (This
       is skipped if g has terminated, because the variable g would not
       be defined in that case.)
       Then, test if g is terminated. If so, g_terminate_time is the other
       candidate logical stop time. If not g_terminated, we do nothing
       (letting the mapped f logical stop time stand if applicable).
       Otherwise, if g_terminated then {
           if f_logically_stopped, take the minimum of the two candidates,
           else take the terminate time of g }
       (See comments at top of file for more about the computation here.)
    */
    if (f_logically_stopped && !pvs->g_terminated) {
        pvs->logical_stop_count = (int64_t) (pvs->t0 + (out_count - pvs->t0) *
            ((f_logical_stop_count / susp->f->sr - pvs->g0) / (g - pvs->g0)));
    }
    if (pvs->g_terminated) {
        int64_t term_cnt_from_g = 
			    ROUNDBIG((pvs->g_terminate_count / susp->g->sr) * susp->f->sr);
        if (f_logically_stopped) { /* take min of g and f log. stop cnt */
            pvs->logical_stop_count = MIN(pvs->logical_stop_count, 
                                          term_cnt_from_g);
        } else {
            f_logically_stopped = TRUE;
            pvs->logical_stop_count = term_cnt_from_g;
        }
        /* maybe output has terminated */
        if (pvs->g_terminate_count < out_count + pvs->fftsize / 2) {
            if (pvs->flags & PVSHELL_FLAG_TERMINATE) {
                pvs->terminate_count = MIN(pvs->terminate_count, 
                                           term_cnt_from_g);
            } else {
                pvs->flags |= PVSHELL_FLAG_TERMINATE;
                pvs->terminate_count = term_cnt_from_g;
            }
            /* printf("pv_callback terminated by g at %ld\n", term_cnt_from_g); */
        }
    }
    if (f_logically_stopped) {
        pvs->flags |= PVSHELL_FLAG_LOGICAL_STOP;
    }

    return hop;
}


/* pv_fetch -- f is the signal. g is the map from output to input
 *
 * g has an arbitrary sample rate with respect to f, and will interpolate.
 * out is where to put samples,
 * n is how many samples to compute (maximum)
 * sample_count is how many output samples we have computed
 */
long pv_fetch(pvshell_type susp, 
              sample_block_values_type out, long *n,
              int64_t sample_count)
{
    pvstate_type pvs = (pvstate_type) susp->state;
    int i;
    int flags = 0;
    int count = 0; /* how many samples computed? */
    /* initialize phase vocoder if this is the first call */
    if (pvs->sample_count == 0) {
        Phase_vocoder pv = pv_create2(malloc, free, pv_callback, susp);
        pv_set_blocksize(pv, OUTPUT_SIZE);
        pv_set_fftsize(pv, pvs->fftsize);
        pv_set_syn_hopsize(pv, pvs->hopsize);
        pv_set_mode(pv, pvs->mode);
        pv_initialize(pv);
        pvs->pv = pv;
        pvs->input = (float *) malloc(pvs->fftsize * sizeof(float));
        pvs->input_count = -pvs->fftsize; /* no valid samples in input yet */
    }
    while (count < *n) {
        int take = *n - count; /* how many to take from (pv) output */
        int remaining;
        int index;
        if (pvs->output_count <= 0) {
            pvs->output = pv_get_output2(pvs->pv);
            pvs->output_count = OUTPUT_SIZE;
        }
        remaining = pvs->output_count;
        /* printf("pv_fetch take %ld remaining %ld\n", take, remaining); */
        if (take > remaining) take = remaining;
        if (pvs->flags) {
            if (pvs->flags & PVSHELL_FLAG_TERMINATE) {
                int64_t to_term = pvs->terminate_count - sample_count;
                if (to_term < take) take = (int) to_term;
                if (take == 0) {
                    /* we want to set the terminate flag at the beginning
                       of the sample block, i.e. only if count == 0; if
                       there are samples in the block already, we just
                       return them and we'll set the terminate flag next time
                    */
                    if (count == 0) {
                        flags |= PVSHELL_FLAG_TERMINATE;
                    }
                }
            }
            if (pvs->flags & PVSHELL_FLAG_LOGICAL_STOP) {
                int64_t to_stop = pvs->logical_stop_count - sample_count;
                /* if we're exactly at the logical stop block, then
                   set the logical stop flag and compute the block as 
                   normal. Otherwise, if we have not reached the logical
                   stop sample yet (to_stop > 0) and we have room to go
                   past it (to_stop < take), then take only up to logical
                   stop sample.
                */
                if (to_stop == 0 && count == 0) {
                    flags |= PVSHELL_FLAG_LOGICAL_STOP;
                } else if (to_stop > 0 && to_stop < take) {
                    take = (int) to_stop;
                }
            }
        }
        if (take == 0) break; /* no more samples; we now terminate */

        index = OUTPUT_SIZE - pvs->output_count;
        for (i = 0; i < take; i++) {
            *out++ = pvs->output[index++];
        }
        count += take;
        sample_count += take;
        pvs->output_count -= take;
        pvs->sample_count += take;
    }
    *n = count;
    /* printf("pv_fetch output_count %ld flags %ld\n", 
              pvs->sample_count, susp->flags); */
    return flags;
}


void pv_free(struct pvshell_struct *susp)
{
    pvstate_type pvs = (pvstate_type) susp->state;
    if (pvs->pv) pv_end(pvs->pv);
    if (pvs->input) free(pvs->input);
}


sound_type snd_phasevocoder(sound_type f, sound_type g, long fftsize, long hopsize, long mode)
{
    /* we're using 5 doubles of state. The first is a parameter,
     * and the rest are initialized to zero except for state[2],
     * aka G_COUNT. This is the number of samples we have read 
     * from G. Since we're interpolating we need a one-sample
     * lookahead, and initializing the count to -1 causes an 
     * extra fetch and hence 1-sample lookahead. This state is copied
     * into the pvshell structure, so we don't need to allocate
     * a vector on the heap.
     */
    long temp;
    if (fftsize == -1) 
        fftsize = 2048;
    if (hopsize == -1) 
        hopsize = fftsize / 8;
    pvstate_node state = {
        0 /* f_count */,
        0 /* g_count */,
        0 /* g_prev */,
        0 /* g_next */,
        0 /* sample_count */,
        NULL, /* pv */
        NULL, /* input */
        0, /* input_count */
        NULL, /* output */
        0, /* output_count */
        fftsize, /* fftsize */
        hopsize, /* hopsize */
        mode };

    /* If f and g do not start at the same time, we should really
     * should do something about it, but we'll just throw an error.
     * Be careful to allow small differences (within one sample).
     */
    if (fabs(f->t0 - g->t0) * f->sr > 0.5) {
        xlfail("phasevocoder inputs must start at the same time");
    }
    /* fftsize should be a power of 2, hopsize should be a power of
     * 2 smaller than fftsize. 
     */
    if (fftsize <= 0) {
        xlfail("phasevocoder fftsize must be > 0");
    }
    /* Test for power of 2. Subtract 1 and a power of 2 will change
     * from 0...010...0 to 0...001...1, and the "and" will be zero.
     * But a non-power of 2 will go from 0...01?...? to 0...01?...?"
     * and the "and" will be non-zero.
     */
    temp = fftsize - 1;
    if ((temp & fftsize) != 0) {
        xlfail("phasevocoder fftsize must be a power of 2");
    }
    /* Test that hopsize is a power of 2 smaller than fftsize: */
    temp = fftsize / 2;
    while (temp && temp != hopsize) temp >>= 1;
    if (!temp) {
        xlfail("phasevocoder hopsize must be a power of 2 smaller than fftsize");
    }
    /* output the same sample rate and start time as f */
    sound_type pv = snd_make_pvshell("snd_phasevocoder", f->sr, f->t0,
                                     &pv_fetch, &pv_free, f, g, 
                                     (void *) &state, sizeof(state));
    return pv;
}                       
