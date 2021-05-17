#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrclar.h"

void clarinet_free(snd_susp_type a_susp);


typedef struct clarinet_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    sound_type breath_env;
    int breath_env_cnt;
    sample_block_values_type breath_env_ptr;

    struct instr *clar;
    int temp_ret_value;
    float breath_scale;
} clarinet_susp_node, *clarinet_susp_type;

#include "instr.h"
#include "upsample.h"


void clarinet_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    clarinet_susp_type susp = (clarinet_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * clar_reg;
    register float breath_scale_reg;
    register sample_block_values_type breath_env_ptr_reg;
    falloc_sample_block(out, "clarinet_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the breath_env input sample block: */
        susp_check_term_samples(breath_env, breath_env_ptr, breath_env_cnt);
        togo = min(togo, susp->breath_env_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        clar_reg = susp->clar;
        breath_scale_reg = susp->breath_scale;
        breath_env_ptr_reg = susp->breath_env_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            controlChange(clar_reg, 128, breath_scale_reg * *breath_env_ptr_reg++);
            *out_ptr_reg++ = (sample_type) tick(clar_reg);
        } while (--n); /* inner loop */

        susp->clar = clar_reg;
        /* using breath_env_ptr_reg is a bad idea on RS/6000: */
        susp->breath_env_ptr += togo;
        out_ptr += togo;
        susp_took(breath_env_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* clarinet_n_fetch */


void clarinet_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    clarinet_susp_type susp = (clarinet_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from breath_env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->breath_env->t0) * susp->breath_env->sr)) >=
           susp->breath_env->current)
        susp_get_samples(breath_env, breath_env_ptr, breath_env_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->breath_env->t0) * susp->breath_env->sr -
         (susp->breath_env->current - susp->breath_env_cnt));
    susp->breath_env_ptr += n;
    susp_took(breath_env_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void clarinet_mark(snd_susp_type a_susp)
{
    clarinet_susp_type susp = (clarinet_susp_type) a_susp;
    sound_xlmark(susp->breath_env);
}


void clarinet_free(snd_susp_type a_susp)
{
    clarinet_susp_type susp = (clarinet_susp_type) a_susp;
    deleteInstrument(susp->clar);
    sound_unref(susp->breath_env);
    ffree_generic(susp, sizeof(clarinet_susp_node), "clarinet_free");
}


void clarinet_print_tree(snd_susp_type a_susp, int n)
{
    clarinet_susp_type susp = (clarinet_susp_type) a_susp;
    indent(n);
    stdputstr("breath_env:");
    sound_print_tree_1(susp->breath_env, n);
}


sound_type snd_make_clarinet(double freq, sound_type breath_env, rate_type sr)
{
    register clarinet_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = breath_env->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, clarinet_susp_node, "snd_make_clarinet");
    susp->clar = initInstrument(CLARINET, ROUND32(sr));
    controlChange(susp->clar, 1, 0.0);;
    susp->temp_ret_value = noteOn(susp->clar, freq, 1.0);
    susp->breath_scale = breath_env->scale * CLAR_CONTROL_CHANGE_CONST;

    /* make sure no sample rate is too high */
    if (breath_env->sr > sr) {
        sound_unref(breath_env);
        snd_badsr();
    } else if (breath_env->sr < sr) breath_env = snd_make_up(sr, breath_env);
    susp->susp.fetch = clarinet_n_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < breath_env->t0) sound_prepend_zeros(breath_env, t0);
    /* minimum start time over all inputs: */
    t0_min = min(breath_env->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = clarinet_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = clarinet_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = clarinet_mark;
    susp->susp.print_tree = clarinet_print_tree;
    susp->susp.name = "clarinet";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->breath_env = breath_env;
    susp->breath_env_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_clarinet(double freq, sound_type breath_env, rate_type sr)
{
    sound_type breath_env_copy = sound_copy(breath_env);
    return snd_make_clarinet(freq, breath_env_copy, sr);
}
