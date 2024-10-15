#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "exp.h"

void exp_free(snd_susp_type a_susp);


typedef struct exp_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type in;
    int in_cnt;
    sample_block_values_type in_ptr;
} exp_susp_node, *exp_susp_type;


void exp_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    exp_susp_type susp = (exp_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type in_scale_reg = susp->in->scale;
    register sample_block_values_type in_ptr_reg;
    falloc_sample_block(out, "exp_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the in input sample block: */
        susp_check_term_log_samples(in, in_ptr, in_cnt);
        togo = min(togo, susp->in_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        in_ptr_reg = susp->in_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) exp((in_scale_reg * *in_ptr_reg++));
        } while (--n); /* inner loop */

        /* using in_ptr_reg is a bad idea on RS/6000: */
        susp->in_ptr += togo;
        out_ptr += togo;
        susp_took(in_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* exp_s_fetch */


void exp_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    exp_susp_type susp = (exp_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from in up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->in->t0) * susp->in->sr)) >=
           susp->in->current)
        susp_get_samples(in, in_ptr, in_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->in->t0) * susp->in->sr -
         (susp->in->current - susp->in_cnt));
    susp->in_ptr += n;
    susp_took(in_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void exp_mark(snd_susp_type a_susp)
{
    exp_susp_type susp = (exp_susp_type) a_susp;
    sound_xlmark(susp->in);
}


void exp_free(snd_susp_type a_susp)
{
    exp_susp_type susp = (exp_susp_type) a_susp;
    sound_unref(susp->in);
    ffree_generic(susp, sizeof(exp_susp_node), "exp_free");
}


void exp_print_tree(snd_susp_type a_susp, int n)
{
    exp_susp_type susp = (exp_susp_type) a_susp;
    indent(n);
    stdputstr("in:");
    sound_print_tree_1(susp->in, n);
}


sound_type snd_make_exp(sound_type in)
{
    register exp_susp_type susp;
    rate_type sr = in->sr;
    time_type t0 = in->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, exp_susp_node, "snd_make_exp");
    susp->susp.fetch = exp_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < in->t0) sound_prepend_zeros(in, t0);
    /* minimum start time over all inputs: */
    t0_min = min(in->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = exp_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = exp_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = exp_mark;
    susp->susp.print_tree = exp_print_tree;
    susp->susp.name = "exp";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(in);
    susp->susp.current = 0;
    susp->in = in;
    susp->in_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_exp(sound_type in)
{
    sound_type in_copy = sound_copy(in);
    return snd_make_exp(in_copy);
}
