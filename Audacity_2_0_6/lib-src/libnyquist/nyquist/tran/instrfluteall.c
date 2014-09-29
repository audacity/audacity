#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrfluteall.h"

void flute_all_free();


typedef struct flute_all_susp_struct {
    snd_susp_node susp;
    long terminate_cnt;
    sound_type breath_env;
    long breath_env_cnt;
    sample_block_values_type breath_env_ptr;
    sound_type freq_env;
    long freq_env_cnt;
    sample_block_values_type freq_env_ptr;
    sound_type jet_delay;
    long jet_delay_cnt;
    sample_block_values_type jet_delay_ptr;
    sound_type noise;
    long noise_cnt;
    sample_block_values_type noise_ptr;

    struct instr *myflute;
    double frequency;
} flute_all_susp_node, *flute_all_susp_type;


	    #include "instr.h"


void flute_all_ssss_fetch(register flute_all_susp_type susp, snd_list_type snd_list)
{
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * myflute_reg;
    register double frequency_reg;
    register sample_type noise_scale_reg = susp->noise->scale;
    register sample_block_values_type noise_ptr_reg;
    register sample_type jet_delay_scale_reg = susp->jet_delay->scale;
    register sample_block_values_type jet_delay_ptr_reg;
    register sample_type freq_env_scale_reg = susp->freq_env->scale;
    register sample_block_values_type freq_env_ptr_reg;
    register sample_type breath_env_scale_reg = susp->breath_env->scale;
    register sample_block_values_type breath_env_ptr_reg;
    falloc_sample_block(out, "flute_all_ssss_fetch");
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

	/* don't run past the noise input sample block: */
	susp_check_samples(noise, noise_ptr, noise_cnt);
	togo = min(togo, susp->noise_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo == 0) break;
	}

	n = togo;
	myflute_reg = susp->myflute;
	frequency_reg = susp->frequency;
	noise_ptr_reg = susp->noise_ptr;
	jet_delay_ptr_reg = susp->jet_delay_ptr;
	freq_env_ptr_reg = susp->freq_env_ptr;
	breath_env_ptr_reg = susp->breath_env_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */

	    controlChange(myflute_reg, 128, FLUTE_CONTROL_CHANGE_CONST * (breath_env_scale_reg * *breath_env_ptr_reg++));
	    controlChange(myflute_reg, 2, FLUTE_CONTROL_CHANGE_CONST * (jet_delay_scale_reg * *jet_delay_ptr_reg++));
	    controlChange(myflute_reg, 4, FLUTE_CONTROL_CHANGE_CONST * (noise_scale_reg * *noise_ptr_reg++));
	    setFrequency(myflute_reg, frequency_reg + (freq_env_scale_reg * *freq_env_ptr_reg++));
	    *out_ptr_reg++ = (sample_type) tick(myflute_reg);
	} while (--n); /* inner loop */

	susp->myflute = myflute_reg;
	/* using noise_ptr_reg is a bad idea on RS/6000: */
	susp->noise_ptr += togo;
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
	susp_took(noise_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* flute_all_ssss_fetch */


void flute_all_toss_fetch(susp, snd_list)
  register flute_all_susp_type susp;
  snd_list_type snd_list;
{
    long final_count = susp->susp.toss_cnt;
    time_type final_time = susp->susp.t0;
    long n;

    /* fetch samples from breath_env up to final_time for this block of zeros */
    while ((round((final_time - susp->breath_env->t0) * susp->breath_env->sr)) >=
	   susp->breath_env->current)
	susp_get_samples(breath_env, breath_env_ptr, breath_env_cnt);
    /* fetch samples from freq_env up to final_time for this block of zeros */
    while ((round((final_time - susp->freq_env->t0) * susp->freq_env->sr)) >=
	   susp->freq_env->current)
	susp_get_samples(freq_env, freq_env_ptr, freq_env_cnt);
    /* fetch samples from jet_delay up to final_time for this block of zeros */
    while ((round((final_time - susp->jet_delay->t0) * susp->jet_delay->sr)) >=
	   susp->jet_delay->current)
	susp_get_samples(jet_delay, jet_delay_ptr, jet_delay_cnt);
    /* fetch samples from noise up to final_time for this block of zeros */
    while ((round((final_time - susp->noise->t0) * susp->noise->sr)) >=
	   susp->noise->current)
	susp_get_samples(noise, noise_ptr, noise_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = round((final_time - susp->breath_env->t0) * susp->breath_env->sr -
         (susp->breath_env->current - susp->breath_env_cnt));
    susp->breath_env_ptr += n;
    susp_took(breath_env_cnt, n);
    n = round((final_time - susp->freq_env->t0) * susp->freq_env->sr -
         (susp->freq_env->current - susp->freq_env_cnt));
    susp->freq_env_ptr += n;
    susp_took(freq_env_cnt, n);
    n = round((final_time - susp->jet_delay->t0) * susp->jet_delay->sr -
         (susp->jet_delay->current - susp->jet_delay_cnt));
    susp->jet_delay_ptr += n;
    susp_took(jet_delay_cnt, n);
    n = round((final_time - susp->noise->t0) * susp->noise->sr -
         (susp->noise->current - susp->noise_cnt));
    susp->noise_ptr += n;
    susp_took(noise_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(susp, snd_list);
}


void flute_all_mark(flute_all_susp_type susp)
{
    sound_xlmark(susp->breath_env);
    sound_xlmark(susp->freq_env);
    sound_xlmark(susp->jet_delay);
    sound_xlmark(susp->noise);
}


void flute_all_free(flute_all_susp_type susp)
{

	    deleteInstrument(susp->myflute);
    sound_unref(susp->breath_env);
    sound_unref(susp->freq_env);
    sound_unref(susp->jet_delay);
    sound_unref(susp->noise);
    ffree_generic(susp, sizeof(flute_all_susp_node), "flute_all_free");
}


void flute_all_print_tree(flute_all_susp_type susp, int n)
{
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
    stdputstr("noise:");
    sound_print_tree_1(susp->noise, n);
}


sound_type snd_make_flute_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type jet_delay, sound_type noise, rate_type sr)
{
    register flute_all_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = breath_env->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, flute_all_susp_node, "snd_make_flute_all");
    susp->myflute = initInstrument(FLUTE, round(sr));
    noteOn(susp->myflute, freq, 1.0);
    controlChange(susp->myflute, 11, FLUTE_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->myflute, 1, FLUTE_CONTROL_CHANGE_CONST * vibrato_gain);;
    susp->frequency = freq;
    susp->susp.fetch = flute_all_ssss_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < breath_env->t0) sound_prepend_zeros(breath_env, t0);
    if (t0 < freq_env->t0) sound_prepend_zeros(freq_env, t0);
    if (t0 < jet_delay->t0) sound_prepend_zeros(jet_delay, t0);
    if (t0 < noise->t0) sound_prepend_zeros(noise, t0);
    /* minimum start time over all inputs: */
    t0_min = min(breath_env->t0, min(freq_env->t0, min(jet_delay->t0, min(noise->t0, t0))));
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
    susp->noise = noise;
    susp->noise_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_flute_all(double freq, sound_type breath_env, sound_type freq_env, double vibrato_freq, double vibrato_gain, sound_type jet_delay, sound_type noise, rate_type sr)
{
    sound_type breath_env_copy = sound_copy(breath_env);
    sound_type freq_env_copy = sound_copy(freq_env);
    sound_type jet_delay_copy = sound_copy(jet_delay);
    sound_type noise_copy = sound_copy(noise);
    return snd_make_flute_all(freq, breath_env_copy, freq_env_copy, vibrato_freq, vibrato_gain, jet_delay_copy, noise_copy, sr);
}
