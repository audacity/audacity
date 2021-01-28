#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "chase.h"

void chase_free(snd_susp_type a_susp);


typedef struct chase_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type input;
    int input_cnt;
    sample_block_values_type input_ptr;

    double level;
    double upslope;
    double downslope;
} chase_susp_node, *chase_susp_type;


void chase_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    chase_susp_type susp = (chase_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double level_reg;
    register double upslope_reg;
    register double downslope_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "chase_n_fetch");
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
        level_reg = susp->level;
        upslope_reg = susp->upslope;
        downslope_reg = susp->downslope;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double x = *input_ptr_reg++;
            if (x > level_reg) {
                level_reg += upslope_reg;
                if (x < level_reg) level_reg = x;
            } else {
                level_reg -= downslope_reg;
                if (x > level_reg) level_reg = x;
            }
            *out_ptr_reg++ = (sample_type) level_reg;
        } while (--n); /* inner loop */

        susp->level = level_reg;
        susp->upslope = upslope_reg;
        susp->downslope = downslope_reg;
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
} /* chase_n_fetch */


void chase_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    chase_susp_type susp = (chase_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double level_reg;
    register double upslope_reg;
    register double downslope_reg;
    register sample_type input_scale_reg = susp->input->scale;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "chase_s_fetch");
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
        level_reg = susp->level;
        upslope_reg = susp->upslope;
        downslope_reg = susp->downslope;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double x = (input_scale_reg * *input_ptr_reg++);
            if (x > level_reg) {
                level_reg += upslope_reg;
                if (x < level_reg) level_reg = x;
            } else {
                level_reg -= downslope_reg;
                if (x > level_reg) level_reg = x;
            }
            *out_ptr_reg++ = (sample_type) level_reg;
        } while (--n); /* inner loop */

        susp->level = level_reg;
        susp->upslope = upslope_reg;
        susp->downslope = downslope_reg;
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
} /* chase_s_fetch */


void chase_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    chase_susp_type susp = (chase_susp_type) a_susp;
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


void chase_mark(snd_susp_type a_susp)
{
    chase_susp_type susp = (chase_susp_type) a_susp;
    sound_xlmark(susp->input);
}


void chase_free(snd_susp_type a_susp)
{
    chase_susp_type susp = (chase_susp_type) a_susp;
    sound_unref(susp->input);
    ffree_generic(susp, sizeof(chase_susp_node), "chase_free");
}


void chase_print_tree(snd_susp_type a_susp, int n)
{
    chase_susp_type susp = (chase_susp_type) a_susp;
    indent(n);
    stdputstr("input:");
    sound_print_tree_1(susp->input, n);
}


sound_type snd_make_chase(sound_type input, double risetime, double falltime)
{
    register chase_susp_type susp;
    rate_type sr = input->sr;
    time_type t0 = input->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, chase_susp_node, "snd_make_chase");
    susp->level = 0.0;
    susp->upslope = 1.0 / (input->sr * risetime);
    susp->downslope = 1.0 / (input->sr * falltime);

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(input, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = chase_n_fetch; break;
      case INTERP_s: susp->susp.fetch = chase_s_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < input->t0) sound_prepend_zeros(input, t0);
    /* minimum start time over all inputs: */
    t0_min = min(input->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = chase_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = chase_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = chase_mark;
    susp->susp.print_tree = chase_print_tree;
    susp->susp.name = "chase";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(input);
    susp->susp.current = 0;
    susp->input = input;
    susp->input_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_chase(sound_type input, double risetime, double falltime)
{
    sound_type input_copy = sound_copy(input);
    return snd_make_chase(input_copy, risetime, falltime);
}
