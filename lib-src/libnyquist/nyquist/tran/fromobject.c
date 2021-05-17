#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "fromobject.h"

void fromobject_free(snd_susp_type a_susp);


typedef struct fromobject_susp_struct {
    snd_susp_node susp;

    boolean done;
    LVAL src;
} fromobject_susp_node, *fromobject_susp_type;


/* IMPLEMENTATION NOTE:
 * The src argument is an XLisp object that returns either a
 *  FLONUM sample or NIL. The output of fromobj is simply the
 * sequence of the samples. 
 */

#include "samples.h"


void fromobject__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fromobject_susp_type susp = (fromobject_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register boolean done_reg;
    register LVAL src_reg;
    falloc_sample_block(out, "fromobject__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        if (susp->done) {
            togo = 0; /* indicate termination */
            break;    /* we're done */
        }

        n = togo;
        done_reg = susp->done;
        src_reg = susp->src;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            LVAL rslt = xleval(cons(s_send, cons(src_reg,
                                                 consa(s_next))));
            if (floatp(rslt)) {
                *out_ptr_reg++ = (sample_type) getflonum(rslt);
            } else {
                done_reg = true;
                /* adjust togo to what it should have been */
                break;
            };
        } while (--n); /* inner loop */

        togo -= n;
        susp->done = done_reg;
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
} /* fromobject__fetch */


void fromobject_mark(snd_susp_type a_susp)
{
    fromobject_susp_type susp = (fromobject_susp_type) a_susp;
    if (susp->src) mark(susp->src);
}


void fromobject_free(snd_susp_type a_susp)
{
    fromobject_susp_type susp = (fromobject_susp_type) a_susp;
    ffree_generic(susp, sizeof(fromobject_susp_node), "fromobject_free");
}


void fromobject_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_fromobject(time_type t0, rate_type sr, LVAL src)
{
    register fromobject_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, fromobject_susp_node, "snd_make_fromobject");
    susp->done = false;
    susp->src = src;
    susp->susp.fetch = fromobject__fetch;

    /* initialize susp state */
    susp->susp.free = fromobject_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = fromobject_mark;
    susp->susp.print_tree = fromobject_print_tree;
    susp->susp.name = "fromobject";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_fromobject(time_type t0, rate_type sr, LVAL src)
{
    return snd_make_fromobject(t0, sr, src);
}
