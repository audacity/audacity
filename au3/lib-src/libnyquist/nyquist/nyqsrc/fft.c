/* fft.c -- implement snd_fft */

#define _USE_MATH_DEFINES 1 /* for Visual C++ to get M_LN2 */
#include <math.h>
#include <stdio.h>
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "fft.h"
#include "fftext.h"

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  change for portability: min->MIN
 * 28May15  rd  swap time domain signal before FFT to get correct phase
 */


/* NOTE: this code does not properly handle start times that do not
 * correspond to the time of the first actual sample
 */


/* The snd_fft function is based on snd_fetch_array */
/*
 * storage layout: the extra field points to extra state that we'll use
 * extra[0] -> length of extra storage
 * extra[1] -> CNT (number of samples in current block)
 * extra[2] -> INDEX (current sample index in current block)
 * extra[3] -> FILLCNT (how many samples in buffer)
 * extra[4] -> TERMCNT (how many samples until termination)
 * extra[5 .. 5+len-1] -> samples (stored as floats)
 * extra[5+len .. 5+2*len-1] -> array of samples to fft
 * extra[5+2*len ... 5+3*len-1] -> window coefficients
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

/* DEBUGGING PRINT FUNCTION:
    void printfloats(char *caption, float *data, int len)
    {
        int i;
        printf("%s: ", caption);
        for (i = 0; i < len; i++) {
            printf("%d:%g ", i, data[i]);
        }
        printf("\n");
    }
*/

void fft_shift(float *x, int len)
{
    int j = len / 2;
    int i;
    for (i = 0; i < len / 2; i++) {
        float temp = x[i];
        x[i] = x[j];
        x[j++] = temp;
    }
}


void n_samples_from_sound(sound_type s, long n, float *table)
{
    int blocklen;
    sample_type scale_factor = s->scale;
    s = sound_copy(s);
    while (n > 0) {
        sample_block_type sampblock = sound_get_next(s, &blocklen);
        long togo = MIN(blocklen, n);
        long i;
        sample_block_values_type sbufp = sampblock->samples;
        for (i = 0; i < togo; i++) {
            *table++ = (float) (*sbufp++ * scale_factor);
        }
        n -= togo;
    }
    sound_unref(s);
}


LVAL snd_fft(sound_type s, long len, long step, LVAL winval)
{
    long i, m, maxlen, skip, fillptr;
    float *samples;
    float *temp_fft;
    float *window;
    LVAL result;
    float *float_base;
    
    if (len < 1) xlfail("len < 1");

    if (!s->extra) { /* this is the first call, so fix up s */
        sound_type w = NULL;
        long bytes = sizeof(s->extra[0]) * OFFSET + sizeof(float) * 3 * len;
        if (winval) {
            if (soundp(winval)) {
                w = getsound(winval);
            } else {
                xlerror("expected a sound", winval);
            }
        }
        /* note: any storage required by fft must be allocated here in a 
         * contiguous block of memory whose size is given by the first long
         * in the block. Here, there are 4 more longs after the size, and 
         * then room for 3*len floats
         *
         * The reason for 3*len floats is to provide space for:
         *    the samples to be transformed (len)
         *    the complex FFT result (len)
         *    the window coefficients (len)
         *
         * The reason for this storage restriction is that when a sound is 
         * freed, the block of memory pointed to by extra is also freed. 
         * There is no function call that might free a more complex 
         * structure (this could be added in sound.c, however, if it's 
         * really necessary).
         */
        
        s->extra = (int64_t *) malloc(bytes);
        s->extra[0] = bytes;
        s->CNT = s->INDEX = s->FILLCNT = 0;
        s->TERMCNT = -1;
        maxlen = len;
        // float_base is where the floats start, after the longs
        float_base = (float *) &(s->extra[OFFSET]);
        window = float_base + 2 * len;
        /* fill the window from w */
        if (!w) {
            for (i = 0; i < len; i++) *window++ = 1.0F;
        } else {
            n_samples_from_sound(w, len, window);
        }
    } else {
        maxlen = (long) ((s->extra[0] - sizeof(s->extra[0]) * OFFSET) /
                         (sizeof(float) * 3));
        if (maxlen != len) xlfail("len changed from initial value");
        float_base = (float *) &(s->extra[OFFSET]);
    }
    samples = float_base;
    temp_fft = float_base + len;
    // this code computes window location 
    window = float_base + 2 * len;
    /* step 1: refill buffer with samples */
    fillptr = (long) s->FILLCNT;
    while (fillptr < maxlen) {
        int icnt = (int) s->CNT;  /* need this to be type int */
        if (s->INDEX == icnt) {
            sound_get_next(s, &icnt);
            s->CNT = icnt;  /* save the count back to s->extra */
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

    /* step 2: construct an array and return it */
    xlsave1(result);
    result = newvector(len);

    /* first len floats will be real part, second len floats imaginary
     * copy buffer to temp_fft with windowing
     */
    for (i = 0; i < len; i++) {
        temp_fft[i] = samples[i] * *window++;
    }
    /* perform the fft: */
    m = ROUND32(log(len) / M_LN2); /* compute log-base-2(len) */
    if (m > 27) {  /* 27 comes from fftext.c and seems big enough */
        xlfail("FFT len greater than 2^27");
    }
    if (1 << m != len) {
        xlfail("FFT len is not a power of two");
    }
    /* to get correct phase, you need to swap the left and right halves
       of the time domain signal before the FFT */
    fft_shift(temp_fft, len);
    if (!fftInit(m)) rffts(temp_fft, m, 1);
    else xlfail("FFT initialization error");

    /* move results to Lisp array */
    setelement(result, 0, cvflonum(temp_fft[0]));
    setelement(result, len - 1, cvflonum(temp_fft[1]));
    for (i = 2; i < len; i++) {
        setelement(result, i - 1, cvflonum(temp_fft[i]));
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
        long remaining = (long) (s->CNT - s->INDEX);
        if (remaining >= skip) {
            s->INDEX += skip;
            skip = 0;
        } else {
            skip -= remaining;
            int icnt = (int) s->CNT;  /* need this to be type int */
            sound_get_next(s, &icnt);
            s->CNT = icnt;      /* save count back into s->extra */
            s->INDEX = 0;
        }
    }
    
    /* restore the stack */
    xlpop();
    return result;
} /* snd_fetch_array */
