#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "fromarraystream.h"

void fromarraystream_free(snd_susp_type a_susp);


typedef struct fromarraystream_susp_struct {
    snd_susp_node susp;

    long index;
    long length;
    LVAL array;
    LVAL src;
    sample_type *samples;
} fromarraystream_susp_node, *fromarraystream_susp_type;


/* IMPLEMENTATION NOTE:
 * The src argument is an XLisp object that returns either an
 * array of samples or NIL. The output of ifft is simply the
 * concatenation of the samples taken from the array. Later,
 * an ifft will be plugged in and this will return overlapped
 * adds of the ifft's.
 */

#include "samples.h"


void fromarraystream__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fromarraystream_susp_type susp = (fromarraystream_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long index_reg;
    register sample_type * samples_reg;
    falloc_sample_block(out, "fromarraystream__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;


        if (susp->src == NULL) {
out:	    togo = 0;	/* indicate termination */
            break;	/* we're done */
        }
        if (susp->index >= susp->length) {
            long i;
            susp->index = 0;
            susp->array = xleval(cons(s_send, cons(susp->src, consa(s_next))));
            susp->index = 0;
            if (susp->array == NULL) {
                susp->src = NULL;
                goto out;
            } else if (!vectorp(susp->array)) {
                xlerror("array expected", susp->array);
            } else if (susp->samples == NULL) {
                /* assume arrays are all the same size as first one;
                   now that we know the size, we just have to do this
                   first allocation.
                 */
                susp->length = getsize(susp->array);
                if (susp->length < 1) xlerror("array has no elements", susp->array);
                susp->samples = 
                    (sample_type *) calloc(susp->length,
                                           sizeof(sample_type));
            } else if (getsize(susp->array) != susp->length) {
                xlerror("arrays must all be the same length", susp->array);
            }
            /* at this point, we have a new array and a place to put samples */
            for (i = 0; i < susp->length; i++) {
                LVAL elem = getelement(susp->array, i);
                if (ntype(elem) != FLONUM) {
                    xlerror("flonum expected", elem);
                }
                susp->samples[i] = (sample_type) getflonum(elem);
            }
            susp->array = NULL; /* free the array */
        }
        togo = min(togo, susp->length - susp->index);

        n = togo;
        index_reg = susp->index;
        samples_reg = susp->samples;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = samples_reg[index_reg++];
        } while (--n); /* inner loop */

        susp->index = index_reg;
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
} /* fromarraystream__fetch */


void fromarraystream_mark(snd_susp_type a_susp)
{
    fromarraystream_susp_type susp = (fromarraystream_susp_type) a_susp;
    if (susp->src) mark(susp->src);
    if (susp->array) mark(susp->array);
}


void fromarraystream_free(snd_susp_type a_susp)
{
    fromarraystream_susp_type susp = (fromarraystream_susp_type) a_susp;
    free(susp->samples);
    ffree_generic(susp, sizeof(fromarraystream_susp_node), "fromarraystream_free");
}


void fromarraystream_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_fromarraystream(time_type t0, rate_type sr, LVAL src)
{
    register fromarraystream_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, fromarraystream_susp_node, "snd_make_fromarraystream");
    susp->index = 0;
    susp->length = 0;
    susp->array = NULL;
    susp->src = src;
    susp->samples = NULL;;
    susp->susp.fetch = fromarraystream__fetch;

    /* initialize susp state */
    susp->susp.free = fromarraystream_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = fromarraystream_mark;
    susp->susp.print_tree = fromarraystream_print_tree;
    susp->susp.name = "fromarraystream";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_fromarraystream(time_type t0, rate_type sr, LVAL src)
{
    return snd_make_fromarraystream(t0, sr, src);
}
