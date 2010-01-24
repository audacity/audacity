/* samples.c -- fugue sound data type */

#include <stdio.h>
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "fft.h"


/* NOTE: this code does not properly handle start times that do not
 * correspond to the time of the first actual sample
 */


/* snd_fetch_array -- fetch a lisp array of samples */
/*
 * storage layout: the extra field points to extra state that we'll use
 * extra[0] -> length of extra storage
 * extra[1] -> CNT (number of samples in current block)
 * extra[2] -> INDEX (current sample index in current block)
 * extra[3] -> FILLCNT (how many samples in buffer)
 * extra[4] -> TERMCNT (how many samples until termination)
 * extra[4 .. 4+len-1] -> samples (stored as floats)
 * 
 * Termination details:
 *   Return NIL when the sound terminates.
 *   Termination is defined as the point where all original
 * signal samples have been shifted out of the samples buffer
 * so that all that's left are zeros from beyond the termination
 * point.
 *   Implementation: when termination is discovered, set TERMCNT
 * to the number of samples to be shifted out. TERMCNT is initially
 * -1 as a flag that we haven't seen the termination yet. 
 * Each time samples are shifted, decrement TERMCNT by the shift amount.
 * When TERMCNT goes to zero, return NULL.
 */

#define CNT extra[1]
#define INDEX extra[2]
#define FILLCNT extra[3]
#define TERMCNT extra[4]
#define OFFSET 5
#define SAMPLES list->block->samples

LVAL snd_fft(sound_type s, long len, long step /* more parameters may belong here */)
{
    long i, maxlen, skip, fillptr;
    float *samples;
    LVAL result;
    
    if (len < 1) xlfail("len < 1");

    if (!s->extra) { /* this is the first call, so fix up s */
        /* note: any storage required by fft must be allocated here in a contiguous
         * block of memory who's size is given by the first long in the block.
         * Here, there are 4 more longs after the size, and then room for len floats
         * (assumes that floats and longs take equal space).
         *
         * The reason for this storage restriction is that when a sound is freed, the
         * block of memory pointed to by extra is also freed. There is no function
         * call that might free a more complex structure (this could be added in sound.c
         * however if it's really necessary).
         */
        falloc_generic_n(s->extra, long, len + OFFSET, "snd_fft");
        s->extra[0] = sizeof(long) * (len + OFFSET);
        s->CNT = s->INDEX = s->FILLCNT = 0;
        s->TERMCNT = -1;
        maxlen = len;
    } else {
        maxlen = (s->extra[0] / sizeof(long)) - OFFSET;
        if (maxlen < 1) xlfail("sound in use by another iterator");
        if (maxlen < len) xlfail("len grew");
    }
    samples = (float *) &(s->extra[OFFSET]);
    
    /* step 1: refill buffer with samples */
    fillptr = s->FILLCNT;
    while (fillptr < maxlen) {
        if (s->INDEX == s->CNT) {
            sound_get_next(s, &(s->CNT));
            if (s->SAMPLES == zero_block->samples) {
                if (s->TERMCNT < 0) s->TERMCNT = fillptr;
            }	
            s->INDEX = 0;
        }
        samples[fillptr++] = s->SAMPLES[s->INDEX++] * s->scale;
    }
    s->FILLCNT = fillptr;

    /* it is important to test here AFTER filling the buffer, because
     * if fillptr WAS 0 when we hit the zero_block, then filling the 
     * buffer will set TERMCNT to 0.
     */
    if (s->TERMCNT == 0) return NULL;
    
    /* logical stop time is ignored by this code -- to fix this,
     * you would need a way to return the logical stop time to 
     * the caller.
     */

    /* HERE IS WHERE THE FFT SHOULD TAKE PLACE ON samples. DO NOT
     * DESTROY SAMPLES IF YOU WANT TO ALLOW OVERLAPPED FFT'S. THE
     * CURRENT CODE RETURNS SAMPLES, BUT A REAL FFT WOULD RETURN
     * THE RESULT OF THE FFT IN STEP 2, WHICH FOLLOWS:
     */

    /* step 2: construct an array and return it */
    xlsave1(result);
    result = newvector(len);

    for (i = 0; i < len; i++) {
        setelement(result, i, cvflonum(samples[i]));
    }

    /* step 3: shift samples by step */
    if (step < 0) xlfail("step < 0");
    s->FILLCNT -= step;
    if (s->FILLCNT < 0) s->FILLCNT = 0;
    for (i = 0; i < s->FILLCNT; i++) {
        samples[i] = samples[i + step];
    }
    

    if (s->TERMCNT >= 0) {
        s->TERMCNT -= step;
        if (s->TERMCNT < 0) s->TERMCNT = 0;
    }


    /* step 4: advance in sound to next sample we need
     *   (only does work if step > size of buffer)
     */
    skip = step - maxlen;
    while (skip > 0) {
        long remaining = s->CNT - s->INDEX;
        if (remaining >= skip) {
            s->INDEX += skip;
            skip = 0;
        } else {
            skip -= remaining;
            sound_get_next(s, &(s->CNT));
            s->INDEX = 0;
        }
    }
    
    /* restore the stack */
    xlpop();
    return result;
} /* snd_fetch_array */




