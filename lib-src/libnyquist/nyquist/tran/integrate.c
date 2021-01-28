#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "integrate.h"

void integrate_free(snd_susp_type a_susp);


typedef struct integrate_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type input;
    int input_cnt;
    sample_block_values_type input_ptr;

    double integral;
} integrate_susp_node, *integrate_susp_type;


void integrate_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    integrate_susp_type susp = (integrate_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double integral_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "integrate_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_log_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

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
        integral_reg = susp->integral;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) integral_reg; integral_reg += *input_ptr_reg++;
        } while (--n); /* inner loop */

        susp->integral = integral_reg;
        /* using input_ptr_reg is a bad idea on RS/6000: */
        susp->input_ptr += togo;
        out_ptr += togo;
        susp_took(input_cnt, togo);
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
} /* integrate_n_fetch */


void integrate_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    integrate_susp_type susp = (integrate_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from input up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->input->t0) * susp->input->sr)) >=
           susp->input->current)
        susp_get_samples(input, input_ptr, input_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->input->t0) * susp->input->sr -
         (susp->input->current - susp->input_cnt));
    susp->input_ptr += n;
    susp_took(input_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void integrate_mark(snd_susp_type a_susp)
{
    integrate_susp_type susp = (integrate_susp_type) a_susp;
    sound_xlmark(susp->input);
}


void integrate_free(snd_susp_type a_susp)
{
    integrate_susp_type susp = (integrate_susp_type) a_susp;
    sound_unref(susp->input);
    ffree_generic(susp, sizeof(integrate_susp_node), "integrate_free");
}


void integrate_print_tree(snd_susp_type a_susp, int n)
{
    integrate_susp_type susp = (integrate_susp_type) a_susp;
    indent(n);
    stdputstr("input:");
    sound_print_tree_1(susp->input, n);
}


sound_type snd_make_integrate(sound_type input)
{
    register integrate_susp_type susp;
    rate_type sr = input->sr;
    time_type t0 = input->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    /* combine scale factors of linear inputs (INPUT) */
    scale_factor *= input->scale;
    input->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (input->sr < sr) { input->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, integrate_susp_node, "snd_make_integrate");
    susp->integral = 0.0;
    scale_factor = (sample_type) (scale_factor / input->sr);
    susp->susp.fetch = integrate_n_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < input->t0) sound_prepend_zeros(input, t0);
    /* minimum start time over all inputs: */
    t0_min = min(input->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = integrate_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = integrate_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = integrate_mark;
    susp->susp.print_tree = integrate_print_tree;
    susp->susp.name = "integrate";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(input);
    susp->susp.current = 0;
    susp->input = input;
    susp->input_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_integrate(sound_type input)
{
    sound_type input_copy = sound_copy(input);
    return snd_make_integrate(input_copy);
}
