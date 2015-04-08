/* phasevocoder.c -- this is a stub showing how you might hook a
   phase vocoder into Nyquist using pvshell
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

/* use the state[] info for sample interpolation */
#define X_VALUE state[0] /* a parameter value */
#define F_COUNT state[1] /* counts samples of f */
#define G_COUNT state[2] /* counts samples of g */
#define G_PREV  state[3] /* previous value from g */
#define G_NEXT  state[4] /* next (current?) value from g */
/* invariant: G_NEXT is the G_COUNT'th sample of g */

/* pv_fetch -- this is an example, but it doesn't really do
 * phase vocoding. Instead, it will just multiply f, g, and x
 *
 * To make things a bit more interesting, we will assume g has
 * an arbitrary sample rate with respect to f, and will interpolate.
 * 
 */
long pv_fetch(pvshell_type susp, 
              sample_block_values_type out, long *n)
{
    int i;
    for (i = 0; i < *n; i++) {
        long new_flags;
        sample_type f;
        double g;
        /* NOTE: in DSP terms, this is poor code because of the
         * division operations -- it could be made faster 
         */
        /* To get a value from g, first compute the time */
        double f_time = susp->F_COUNT / susp->f->sr;
        /* Now compute g count that is past the time */
        double g_count = f_time * susp->g->sr;
        while (susp->G_COUNT < g_count) {
            PVSHELL_TEST_G(susp); /* prepare to get a sample */
            /* ignore flags from g -- we could, if we wanted,
             * terminate when either f or g terminated, etc.
             */
            susp->G_PREV = susp->G_NEXT;
            susp->G_NEXT = PVSHELL_FETCH_G(susp);
            susp->G_COUNT++;
        }
        /* now interpolate to get the value of g at f_time */
        g = susp->G_PREV + (susp->G_NEXT - susp->G_PREV) *
                           (g_count - (susp->G_COUNT - 1));
        new_flags = PVSHELL_TEST_F(susp);
        susp->flags |= new_flags;
        if (new_flags) break;
        f = PVSHELL_FETCH_F(susp);
        susp->F_COUNT++; /* count how many samples we have taken */
        
        /* now we have f, g, x */
        *out++ = f * g * susp->X_VALUE;
    }
    /* i is the number of samples we acutally computed */
    *n = i;
    /* if we computed samples, we want to return them before
     * returning flags that say we're done or stopped
     */
    return (i ? 0 : susp->flags);
}


sound_type snd_phasevocoder(sound_type f, sound_type g, double x)
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
    double state[5] = {0, 0, -1, 0, 0};
    state[0] = x;
    /* If f and g do not start at the same time, we should really
     * should do something about it, but we'll just throw an error.
     * Be careful to allow small differences (within one sample).
     */
    if (fabs(f->t0 - g->t0) * f->sr > 0.5) {
        xlfail("phasevocoder inputs must start at the same time");
    }
    /* output the same sample rate and start time as f */
    return snd_make_pvshell("snd_phasevocoder", f->sr, f->t0,
                       &pv_fetch, f, g, 
                       state, sizeof(state) / sizeof(state[0]));
}                       
