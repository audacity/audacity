#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrbowedfreq.h"

void bowed_freq_free(snd_susp_type a_susp);


typedef struct bowed_freq_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    sound_type bowpress_env;
    int bowpress_env_cnt;
    sample_block_values_type bowpress_env_ptr;
    sound_type freq_env;
    int freq_env_cnt;
    sample_block_values_type freq_env_ptr;

    struct instr *mybow;
    int temp_ret_value;
    float bow_scale;
    double frequency;
} bowed_freq_susp_node, *bowed_freq_susp_type;

#include "instr.h"
#include "upsample.h"


void bowed_freq_ns_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    bowed_freq_susp_type susp = (bowed_freq_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * mybow_reg;
    register float bow_scale_reg;
    register double frequency_reg;
    register sample_type freq_env_scale_reg = susp->freq_env->scale;
    register sample_block_values_type freq_env_ptr_reg;
    register sample_block_values_type bowpress_env_ptr_reg;
    falloc_sample_block(out, "bowed_freq_ns_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the bowpress_env input sample block: */
        susp_check_term_samples(bowpress_env, bowpress_env_ptr, bowpress_env_cnt);
        togo = min(togo, susp->bowpress_env_cnt);

        /* don't run past the freq_env input sample block: */
        susp_check_samples(freq_env, freq_env_ptr, freq_env_cnt);
        togo = min(togo, susp->freq_env_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        mybow_reg = susp->mybow;
        bow_scale_reg = susp->bow_scale;
        frequency_reg = susp->frequency;
        freq_env_ptr_reg = susp->freq_env_ptr;
        bowpress_env_ptr_reg = susp->bowpress_env_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            controlChange(mybow_reg, 128, bow_scale_reg * *bowpress_env_ptr_reg++);
            setFrequency(mybow_reg, frequency_reg + (freq_env_scale_reg * *freq_env_ptr_reg++));
            *out_ptr_reg++ = (sample_type) tick(mybow_reg);
        } while (--n); /* inner loop */

        susp->mybow = mybow_reg;
        /* using freq_env_ptr_reg is a bad idea on RS/6000: */
        susp->freq_env_ptr += togo;
        /* using bowpress_env_ptr_reg is a bad idea on RS/6000: */
        susp->bowpress_env_ptr += togo;
        out_ptr += togo;
        susp_took(bowpress_env_cnt, togo);
        susp_took(freq_env_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* bowed_freq_ns_fetch */


void bowed_freq_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    bowed_freq_susp_type susp = (bowed_freq_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from bowpress_env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->bowpress_env->t0) * susp->bowpress_env->sr)) >=
           susp->bowpress_env->current)
        susp_get_samples(bowpress_env, bowpress_env_ptr, bowpress_env_cnt);
    /* fetch samples from freq_env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->freq_env->t0) * susp->freq_env->sr)) >=
           susp->freq_env->current)
        susp_get_samples(freq_env, freq_env_ptr, freq_env_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->bowpress_env->t0) * susp->bowpress_env->sr -
         (susp->bowpress_env->current - susp->bowpress_env_cnt));
    susp->bowpress_env_ptr += n;
    susp_took(bowpress_env_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->freq_env->t0) * susp->freq_env->sr -
         (susp->freq_env->current - susp->freq_env_cnt));
    susp->freq_env_ptr += n;
    susp_took(freq_env_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void bowed_freq_mark(snd_susp_type a_susp)
{
    bowed_freq_susp_type susp = (bowed_freq_susp_type) a_susp;
    sound_xlmark(susp->bowpress_env);
    sound_xlmark(susp->freq_env);
}


void bowed_freq_free(snd_susp_type a_susp)
{
    bowed_freq_susp_type susp = (bowed_freq_susp_type) a_susp;
    deleteInstrument(susp->mybow);
    sound_unref(susp->bowpress_env);
    sound_unref(susp->freq_env);
    ffree_generic(susp, sizeof(bowed_freq_susp_node), "bowed_freq_free");
}


void bowed_freq_print_tree(snd_susp_type a_susp, int n)
{
    bowed_freq_susp_type susp = (bowed_freq_susp_type) a_susp;
    indent(n);
    stdputstr("bowpress_env:");
    sound_print_tree_1(susp->bowpress_env, n);

    indent(n);
    stdputstr("freq_env:");
    sound_print_tree_1(susp->freq_env, n);
}


sound_type snd_make_bowed_freq(double freq, sound_type bowpress_env, sound_type freq_env, rate_type sr)
{
    register bowed_freq_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = bowpress_env->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, bowed_freq_susp_node, "snd_make_bowed_freq");
    susp->mybow = initInstrument(BOWED, ROUND32(sr));
    controlChange(susp->mybow, 1, 0.0);;
    susp->temp_ret_value = noteOn(susp->mybow, freq, 1.0);
    susp->bow_scale = bowpress_env->scale * BOW_CONTROL_CHANGE_CONST;
    susp->frequency = freq;

    /* make sure no sample rate is too high */
    if (bowpress_env->sr > sr) {
        sound_unref(bowpress_env);
        snd_badsr();
    } else if (bowpress_env->sr < sr) bowpress_env = snd_make_up(sr, bowpress_env);
    if (freq_env->sr > sr) {
        sound_unref(freq_env);
        snd_badsr();
    } else if (freq_env->sr < sr) freq_env = snd_make_up(sr, freq_env);
    susp->susp.fetch = bowed_freq_ns_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < bowpress_env->t0) sound_prepend_zeros(bowpress_env, t0);
    if (t0 < freq_env->t0) sound_prepend_zeros(freq_env, t0);
    /* minimum start time over all inputs: */
    t0_min = min(bowpress_env->t0, min(freq_env->t0, t0));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = bowed_freq_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = bowed_freq_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = bowed_freq_mark;
    susp->susp.print_tree = bowed_freq_print_tree;
    susp->susp.name = "bowed_freq";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->bowpress_env = bowpress_env;
    susp->bowpress_env_cnt = 0;
    susp->freq_env = freq_env;
    susp->freq_env_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_bowed_freq(double freq, sound_type bowpress_env, sound_type freq_env, rate_type sr)
{
    sound_type bowpress_env_copy = sound_copy(bowpress_env);
    sound_type freq_env_copy = sound_copy(freq_env);
    return snd_make_bowed_freq(freq, bowpress_env_copy, freq_env_copy, sr);
}
