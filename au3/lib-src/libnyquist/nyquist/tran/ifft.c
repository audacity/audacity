#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "ifft.h"

void ifft_free(snd_susp_type a_susp);


typedef struct ifft_susp_struct {
    snd_susp_node susp;

    long index;
    long length;
    LVAL array;
    long window_len;
    sample_type *outbuf;
    LVAL src;
    long stepsize;
    sample_type *window;
    sample_type *samples;
    table_type table;
} ifft_susp_node, *ifft_susp_type;


/* index: index into outbuf whree we get output samples
 * length: size of the frame, window, and outbuf; half size of samples
 * array: spectral frame goes here (why not a local var?)
 * window_len: size of window, should equal length
 * outbuf: real part of samples are multiplied by window and added to
 *          outbuf (after shifting)
 * src: send :NEXT to this object to get next frame
 * stepsize: shift by this many and add each frame
 * samples: result of ifft goes here, real and imag
 * window: multiply samples by window if any 
 *
 * IMPLEMENTATION NOTE:
 * The src argument is an XLisp object that returns either an
 * array of samples or NIL. The output of ifft is simply the
 * concatenation of the samples taken from the array. Later,
 * an ifft will be plugged in and this will return overlapped
 * adds of the ifft's.
 *
 * OVERLAP: stepsize must be less than or equal to the length
 * of real part of the transformed spectrum. A transform step
 * works like this: 
 * (1) shift the output buffer by stepsize samples, filling
 *     the end of the buffer with zeros
 * (2) get and transform an array of spectral coefficients
 * (3) multiply the result by a window
 * (4) add the result to the output buffer
 * (5) output the first stepsize samples of the buffer
 * 
 * DATA FORMAT: the DC component goes in array elem 0
 * Cosine part is in elements 2*i-1
 * Sine part is in elements 2*i
 * Nyquist frequency is in element length-1
 */

#include "samples.h"
#include "fftext.h"
#include "fft.h"

#define MUST_BE_FLONUM(e) \
    if (!(e) || ntype(e) != FLONUM) { xlerror("in IFFT: flonum expected", (e)); }

table_type get_window_samples(LVAL window, sample_type **samples, long *len)
{
    table_type result = NULL;
    if (soundp(window)) {
        sound_type window_sound = getsound(window);
        xlprot1(window); /* maybe not necessary */
        result = sound_to_table(window_sound);
        xlpop();
        *samples = result->samples;
        *len = (long) (result->length + 0.5);
    }
    return result;
}


void ifft__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    ifft_susp_type susp = (ifft_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long index_reg;
    register sample_type * outbuf_reg;
    falloc_sample_block(out, "ifft__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;


        if (susp->src == NULL) {
out:        togo = 0;   /* indicate termination */
            break;      /* we're done */
        }
        if (susp->index >= susp->stepsize) {
            long i;
            long m, n;
            LVAL elem;
            susp->index = 0;
            susp->array = 
                xleval(cons(s_send, cons(susp->src, consa(s_next))));
            if (susp->array == NULL) {
                susp->src = NULL;
                goto out;
            } else if (!vectorp(susp->array)) {
                xlerror("in IFFT: array expected", susp->array);
            } else if (susp->samples == NULL) {
                /* assume arrays are all the same size as first one;
                   now that we know the size, we just have to do this
                   first allocation.
                 */
                susp->length = getsize(susp->array);
                if (susp->length < 1) 
                    xlerror("in IFFT: array has no elements", susp->array);
                if (susp->window && (susp->window_len != susp->length))
                    xlerror("in IFFT: window size and spectrum size differ", 
                            susp->array);
                /* tricky non-power of 2 detector: only if this is a
                 * power of 2 will the highest 1 bit be cleared when
                 * we subtract 1 ...
                 */
                if (susp->length & (susp->length - 1))
                    xlfail("spectrum size must be a power of 2");
                if (susp->stepsize < 1) 
                    xlfail("in IFFT: step size must be greater than zero");
                if (susp->length < susp->stepsize) 
                    xlerror("in IFFT: step size must be smaller than spectrum size", 
                            susp->array);
                susp->samples = (sample_type *) calloc(susp->length,
                                                       sizeof(sample_type));
                susp->outbuf = (sample_type *) calloc(susp->length, 
                                                      sizeof(sample_type));
            } else if (getsize(susp->array) != susp->length) {
                xlerror("in IFFT: arrays must all be the same length", susp->array);
            }

            /* at this point, we have a new array to put samples */
            /* the incoming array format is [DC, R1, I1, R2, I2, ... RN]
             * where RN is the real coef at the Nyquist frequency
             * but susp->samples should be organized as [DC, RN, R1, I1, ...]
             */
            n = susp->length;
            /* get the DC (real) coef */
            elem = getelement(susp->array, 0);
            MUST_BE_FLONUM(elem)
            susp->samples[0] = (sample_type) getflonum(elem);

            /* get the Nyquist (real) coef */
            elem = getelement(susp->array, n - 1);
            MUST_BE_FLONUM(elem);
            susp->samples[1] = (sample_type) getflonum(elem);

            /* get the remaining coef */
            for (i = 1; i < n - 1; i++) {
                elem = getelement(susp->array, i);
                MUST_BE_FLONUM(elem)
                susp->samples[i + 1] = (sample_type) getflonum(elem);
            }
            susp->array = NULL; /* free the array */

            /* here is where the IFFT and windowing should take place */
            //fftnf(1, &n, susp->samples, susp->samples + n, -1, 1.0);
            m = ROUND32(log2(n));
            if (!fftInit(m)) riffts(susp->samples, m, 1);
            else xlfail("FFT initialization error");
            fft_shift(susp->samples, n);
            if (susp->window) {
                n = susp->length;
                for (i = 0; i < n; i++) {
                    susp->samples[i] *= susp->window[i];
                }
            }

            /* shift the outbuf */
            n = susp->length - susp->stepsize;
            for (i = 0; i < n; i++) {
                susp->outbuf[i] = susp->outbuf[i + susp->stepsize];
            }

            /* clear end of outbuf */
            for (i = n; i < susp->length; i++) {
                susp->outbuf[i] = 0;
            }

            /* add in the ifft result */
            n = susp->length;
            for (i = 0; i < n; i++) {
                susp->outbuf[i] += susp->samples[i];
            }
        }
        togo = min(togo, susp->stepsize - susp->index);

        n = togo;
        index_reg = susp->index;
        outbuf_reg = susp->outbuf;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = outbuf_reg[index_reg++];
        } while (--n); /* inner loop */

        susp->index = index_reg;
        susp->outbuf = outbuf_reg;
        out_ptr += togo;
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* ifft__fetch */


void ifft_mark(snd_susp_type a_susp)
{
    ifft_susp_type susp = (ifft_susp_type) a_susp;
    if (susp->src) mark(susp->src);
    if (susp->array) mark(susp->array);
}


void ifft_free(snd_susp_type a_susp)
{
    ifft_susp_type susp = (ifft_susp_type) a_susp;
    if (susp->samples) free(susp->samples);
    if (susp->table) table_unref(susp->table);
    if (susp->outbuf) free(susp->outbuf);
    ffree_generic(susp, sizeof(ifft_susp_node), "ifft_free");
}


void ifft_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_ifft(time_type t0, rate_type sr, LVAL src, long stepsize, LVAL window)
{
    register ifft_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, ifft_susp_node, "snd_make_ifft");
    susp->index = stepsize;
    susp->length = 0;
    susp->array = NULL;
    susp->window_len = 0;
    susp->outbuf = NULL;
    susp->src = src;
    susp->stepsize = stepsize;
    susp->window = NULL;
    susp->samples = NULL;
    susp->table = get_window_samples(window, &susp->window, &susp->window_len);
    susp->susp.fetch = ifft__fetch;

    /* initialize susp state */
    susp->susp.free = ifft_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = ifft_mark;
    susp->susp.print_tree = ifft_print_tree;
    susp->susp.name = "ifft";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_ifft(time_type t0, rate_type sr, LVAL src, long stepsize, LVAL window)
{
    return snd_make_ifft(t0, sr, src, stepsize, window);
}
