#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "congen.h"

void congen_free(snd_susp_type a_susp);


typedef struct congen_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    sound_type sndin;
    int sndin_cnt;
    sample_block_values_type sndin_ptr;

    double value;
    double rise_factor;
    double fall_factor;
} congen_susp_node, *congen_susp_type;


void congen_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    congen_susp_type susp = (congen_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double value_reg;
    register double rise_factor_reg;
    register double fall_factor_reg;
    register sample_block_values_type sndin_ptr_reg;
    falloc_sample_block(out, "congen_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the sndin input sample block: */
        susp_check_term_samples(sndin, sndin_ptr, sndin_cnt);
        togo = min(togo, susp->sndin_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        value_reg = susp->value;
        rise_factor_reg = susp->rise_factor;
        fall_factor_reg = susp->fall_factor;
        sndin_ptr_reg = susp->sndin_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            sample_type current = *sndin_ptr_reg++;
            if (current > value_reg) {
                value_reg = current - (current - value_reg) * rise_factor_reg;
            } else {
                value_reg = current - (current - value_reg) * fall_factor_reg;
            }
            *out_ptr_reg++ = (sample_type) value_reg;
        } while (--n); /* inner loop */

        susp->value = value_reg;
        /* using sndin_ptr_reg is a bad idea on RS/6000: */
        susp->sndin_ptr += togo;
        out_ptr += togo;
        susp_took(sndin_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* congen_n_fetch */


void congen_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    congen_susp_type susp = (congen_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double value_reg;
    register double rise_factor_reg;
    register double fall_factor_reg;
    register sample_type sndin_scale_reg = susp->sndin->scale;
    register sample_block_values_type sndin_ptr_reg;
    falloc_sample_block(out, "congen_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the sndin input sample block: */
        susp_check_term_samples(sndin, sndin_ptr, sndin_cnt);
        togo = min(togo, susp->sndin_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        value_reg = susp->value;
        rise_factor_reg = susp->rise_factor;
        fall_factor_reg = susp->fall_factor;
        sndin_ptr_reg = susp->sndin_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            sample_type current = (sndin_scale_reg * *sndin_ptr_reg++);
            if (current > value_reg) {
                value_reg = current - (current - value_reg) * rise_factor_reg;
            } else {
                value_reg = current - (current - value_reg) * fall_factor_reg;
            }
            *out_ptr_reg++ = (sample_type) value_reg;
        } while (--n); /* inner loop */

        susp->value = value_reg;
        /* using sndin_ptr_reg is a bad idea on RS/6000: */
        susp->sndin_ptr += togo;
        out_ptr += togo;
        susp_took(sndin_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* congen_s_fetch */


void congen_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    congen_susp_type susp = (congen_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from sndin up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->sndin->t0) * susp->sndin->sr)) >=
           susp->sndin->current)
        susp_get_samples(sndin, sndin_ptr, sndin_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->sndin->t0) * susp->sndin->sr -
         (susp->sndin->current - susp->sndin_cnt));
    susp->sndin_ptr += n;
    susp_took(sndin_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void congen_mark(snd_susp_type a_susp)
{
    congen_susp_type susp = (congen_susp_type) a_susp;
    sound_xlmark(susp->sndin);
}


void congen_free(snd_susp_type a_susp)
{
    congen_susp_type susp = (congen_susp_type) a_susp;
    sound_unref(susp->sndin);
    ffree_generic(susp, sizeof(congen_susp_node), "congen_free");
}


void congen_print_tree(snd_susp_type a_susp, int n)
{
    congen_susp_type susp = (congen_susp_type) a_susp;
    indent(n);
    stdputstr("sndin:");
    sound_print_tree_1(susp->sndin, n);
}


sound_type snd_make_congen(sound_type sndin, double risetime, double falltime)
{
    register congen_susp_type susp;
    rate_type sr = sndin->sr;
    time_type t0 = sndin->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, congen_susp_node, "snd_make_congen");
    susp->value = 0;
    susp->rise_factor = exp(log(0.5) / (sndin->sr * risetime));
    susp->fall_factor = exp(log(0.5) / (sndin->sr * falltime));

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(sndin, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = congen_n_fetch; break;
      case INTERP_s: susp->susp.fetch = congen_s_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < sndin->t0) sound_prepend_zeros(sndin, t0);
    /* minimum start time over all inputs: */
    t0_min = min(sndin->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = congen_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = congen_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = congen_mark;
    susp->susp.print_tree = congen_print_tree;
    susp->susp.name = "congen";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->sndin = sndin;
    susp->sndin_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_congen(sound_type sndin, double risetime, double falltime)
{
    sound_type sndin_copy = sound_copy(sndin);
    return snd_make_congen(sndin_copy, risetime, falltime);
}
