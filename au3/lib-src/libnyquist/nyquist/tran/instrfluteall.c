#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrfluteall.h"

void flute_all_free(snd_susp_type a_susp);


typedef struct flute_all_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    sound_type breath_env;
    int breath_env_cnt;
    sample_block_values_type breath_env_ptr;
    sound_type freq_env;
    int freq_env_cnt;
    sample_block_values_type freq_env_ptr;
    sound_type jet_delay;
    int jet_delay_cnt;
    sample_block_values_type jet_delay_ptr;
    sound_type noise_env;
    int noise_env_cnt;
    sample_block_values_type noise_env_ptr;

    struct instr *myflute;
    double frequency;
    float breath_scale;
    float jet_scale;
    float noise_scale;
} flute_all_susp_node, *flute_all_susp_type;

#include "instr.h"
#include "upsample.h"


void flute_all_nsnn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    flute_all_susp_type susp = (flute_all_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * myflute_reg;
    register double frequency_reg;
    register float breath_scale_reg;
    register float jet_scale_reg;
    register float noise_scale_reg;
    register sample_block_values_type noise_env_ptr_reg;
    register sample_block_values_type jet_delay_ptr_reg;
    register sample_type freq_env_scale_reg = susp->freq_env->scale;
    register sample_block_values_type freq_env_ptr_reg;
    register sample_block_values_type breath_env_ptr_reg;
    falloc_sample_block(out, "flute_all_nsnn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the breath_env input sample block: */
        susp_check_term_samples(breath_env, breath_env_ptr, breath_env_cnt);
        togo = min(togo, susp->breath_env_cnt);

        /* don't run past the freq_env input sample block: */
        susp_check_samples(freq_env, freq_env_ptr, freq_env_cnt);
        togo = min(togo, susp->freq_env_cnt);

        /* don't run past the jet_delay input sample block: */
        susp_check_samples(jet_delay, jet_delay_ptr, jet_delay_cnt);
        togo = min(togo, susp->jet_delay_cnt);

        /* don't run past the noise_env input sample block: */
        susp_check_samples(noise_env, noise_env_ptr, noise_env_cnt);
        togo = min(togo, susp->noise_env_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        myflute_reg = susp->myflute;
        frequency_reg = susp->frequency;
        breath_scale_reg = susp->breath_scale;
        jet_scale_reg = susp->jet_scale;
        noise_scale_reg = susp->noise_scale;
        noise_env_ptr_reg = susp->noise_env_ptr;
        jet_delay_ptr_reg = susp->jet_delay_ptr;
        freq_env_ptr_reg = susp->freq_env_ptr;
        breath_env_ptr_reg = susp->breath_env_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            controlChange(myflute_reg, 128, breath_scale_reg * *breath_env_ptr_reg++);
            controlChange(myflute_reg, 2, jet_scale_reg * *jet_delay_ptr_reg++);
            controlChange(myflute_reg, 4, noise_scale_reg * *noise_env_ptr_reg++);
            setFrequency(myflute_reg, frequency_reg + (freq_env_scale_reg * *freq_env_ptr_reg++));
            *out_ptr_reg++ = (sample_type) tick(myflute_reg);
        } while (--n); /* inner loop */

        susp->myflute = myflute_reg;
        /* using noise_env_ptr_reg is a bad idea on RS/6000: */
        susp->noise_env_ptr += togo;
        /* using jet_delay_ptr_reg is a bad idea on RS/6000: */
        susp->jet_delay_ptr += togo;
        /* using freq_env_ptr_reg is a bad idea on RS/6000: */
        susp->freq_env_ptr += togo;
        /* using breath_env_ptr_reg is a bad idea on RS/6000: */
        susp->breath_env_ptr += togo;
        out_ptr += togo;
        susp_took(breath_env_cnt, togo);
        susp_took(freq_env_cnt, togo);
        susp_took(jet_delay_cnt, togo);
        susp_took(noise_env_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* flute_all_nsnn_fetch */


void flute_all_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    flute_all_susp_type susp = (flute_all_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from breath_env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->breath_env->t0) * susp->breath_env->sr)) >=
           susp->breath_env->current)
        susp_get_samples(breath_env, breath_env_ptr, breath_env_cnt);
    /* fetch samples from freq_env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->freq_env->t0) * susp->freq_env->sr)) >=
           susp->freq_env->current)
        susp_get_samples(freq_env, freq_env_ptr, freq_env_cnt);
    /* fetch samples from jet_delay up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->jet_delay->t0) * susp->jet_delay->sr)) >=
           susp->jet_delay->current)
        susp_get_samples(jet_delay, jet_delay_ptr, jet_delay_cnt);
    /* fetch samples from noise_env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->noise_env->t0) * susp->noise_env->sr)) >=
           susp->noise_env->current)
        susp_get_samples(noise_env, noise_env_ptr, noise_env_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->breath_env->t0) * susp->breath_env->sr -
         (susp->breath_env->current - susp->breath_env_cnt));
    susp->breath_env_ptr += n;
    susp_took(breath_env_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->freq_env->t0) * susp->freq_env->sr -
         (susp->freq_env->current - susp->freq_env_cnt));
    susp->freq_env_ptr += n;
    susp_took(freq_env_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->jet_delay->t0) * susp->jet_delay->sr -
         (susp->jet_delay->current - susp->jet_delay_cnt));
    susp->jet_delay_ptr += n;
    susp_took(jet_delay_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->noise_env->t0) * susp->noise_env->sr -
         (susp->noise_env->current - susp->noise_env_cnt));
    susp->noise_env_ptr += n;
    susp_took(noise_env_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void flute_all_mark(snd_susp_type a_susp)
{
    flute_all_susp_type susp = (flute_all_susp_type) a_susp;
    sound_xlmark(susp->breath_env);
    sound_xlmark(susp->freq_env);
    sound_xlmark(susp->jet_delay);
    sound_xlmark(susp->noise_env);
}


void flute_all_free(snd_susp_type a_susp)
{
    flute_all_susp_type susp = (flute_all_susp_type) a_susp;
    deleteInstrument(susp->myflute);
    sound_unref(susp->breath_env);
    sound_unref(susp->freq_env);
    sound_unref(susp->jet_delay);
    sound_unref(susp->noise_env);
    ffree_generic(susp, sizeof(flute_all_susp_node), "flute_all_free");
}


void flute_all_print_tree(snd_susp_type a_susp, int n)
{
    flute_all_susp_type susp = (flute_all_susp_type) a_susp;
    indent(n);
    stdputstr("breath_env:");
    sound_print_tree_1(susp->breath_env, n);

    indent(n);
    stdputstr("freq_env:");
    sound_print_tree_1(susp->freq_env, n);

    indent(n);
    stdputstr("jet_delay:");
    sound_print_tree_1(susp->jet_delay, n);

    indent(n);
    stdputstr("noise_env:");
    sound_print_tree_1(susp->noise_env, n);
}


sound_type snd_make_flute_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type jet_delay, sound_type noise_env, rate_type sr)
{
    register flute_all_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = breath_env->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, flute_all_susp_node, "snd_make_flute_all");
    susp->myflute = initInstrument(FLUTE, ROUND32(sr));
    noteOn(susp->myflute, freq, 1.0);
    controlChange(susp->myflute, 11, FLUTE_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->myflute, 1, FLUTE_CONTROL_CHANGE_CONST * vibrato_gain);;
    susp->frequency = freq;
    susp->breath_scale = breath_env->scale * FLUTE_CONTROL_CHANGE_CONST;
    susp->jet_scale = jet_delay->scale * FLUTE_CONTROL_CHANGE_CONST;
    susp->noise_scale = noise_env->scale * FLUTE_CONTROL_CHANGE_CONST;

    /* make sure no sample rate is too high */
    if (breath_env->sr > sr) {
        sound_unref(breath_env);
        snd_badsr();
    } else if (breath_env->sr < sr) breath_env = snd_make_up(sr, breath_env);
    if (freq_env->sr > sr) {
        sound_unref(freq_env);
        snd_badsr();
    } else if (freq_env->sr < sr) freq_env = snd_make_up(sr, freq_env);
    if (jet_delay->sr > sr) {
        sound_unref(jet_delay);
        snd_badsr();
    } else if (jet_delay->sr < sr) jet_delay = snd_make_up(sr, jet_delay);
    if (noise_env->sr > sr) {
        sound_unref(noise_env);
        snd_badsr();
    } else if (noise_env->sr < sr) noise_env = snd_make_up(sr, noise_env);
    susp->susp.fetch = flute_all_nsnn_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < breath_env->t0) sound_prepend_zeros(breath_env, t0);
    if (t0 < freq_env->t0) sound_prepend_zeros(freq_env, t0);
    if (t0 < jet_delay->t0) sound_prepend_zeros(jet_delay, t0);
    if (t0 < noise_env->t0) sound_prepend_zeros(noise_env, t0);
    /* minimum start time over all inputs: */
    t0_min = min(breath_env->t0, min(freq_env->t0, min(jet_delay->t0, min(noise_env->t0, t0))));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = flute_all_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = flute_all_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = flute_all_mark;
    susp->susp.print_tree = flute_all_print_tree;
    susp->susp.name = "flute_all";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->breath_env = breath_env;
    susp->breath_env_cnt = 0;
    susp->freq_env = freq_env;
    susp->freq_env_cnt = 0;
    susp->jet_delay = jet_delay;
    susp->jet_delay_cnt = 0;
    susp->noise_env = noise_env;
    susp->noise_env_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_flute_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type jet_delay, sound_type noise_env, rate_type sr)
{
    sound_type breath_env_copy = sound_copy(breath_env);
    sound_type freq_env_copy = sound_copy(freq_env);
    sound_type jet_delay_copy = sound_copy(jet_delay);
    sound_type noise_env_copy = sound_copy(noise_env);
    return snd_make_flute_all(freq, breath_env_copy, freq_env_copy, vibrato_freq, vibrato_gain, jet_delay_copy, noise_env_copy, sr);
}
