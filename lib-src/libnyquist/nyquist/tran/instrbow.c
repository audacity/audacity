#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrbow.h"

void bowed_free();


typedef struct bowed_susp_struct {
    snd_susp_node susp;
    long terminate_cnt;
    sound_type bowpress_env;
    long bowpress_env_cnt;
    sample_block_values_type bowpress_env_ptr;

    struct instr *mybow;
    int temp_ret_value;
} bowed_susp_node, *bowed_susp_type;


	    #include "instr.h"


void bowed_s_fetch(register bowed_susp_type susp, snd_list_type snd_list)
{
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * mybow_reg;
    register sample_type bowpress_env_scale_reg = susp->bowpress_env->scale;
    register sample_block_values_type bowpress_env_ptr_reg;
    falloc_sample_block(out, "bowed_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the bowpress_env input sample block: */
	susp_check_term_samples(bowpress_env, bowpress_env_ptr, bowpress_env_cnt);
	togo = min(togo, susp->bowpress_env_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo == 0) break;
	}

	n = togo;
	mybow_reg = susp->mybow;
	bowpress_env_ptr_reg = susp->bowpress_env_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */

	    controlChange(mybow_reg, 128, BOW_CONTROL_CHANGE_CONST * (bowpress_env_scale_reg * *bowpress_env_ptr_reg++));
	    *out_ptr_reg++ = (sample_type) tick(mybow_reg);
	} while (--n); /* inner loop */

	susp->mybow = mybow_reg;
	/* using bowpress_env_ptr_reg is a bad idea on RS/6000: */
	susp->bowpress_env_ptr += togo;
	out_ptr += togo;
	susp_took(bowpress_env_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* bowed_s_fetch */


void bowed_toss_fetch(susp, snd_list)
  register bowed_susp_type susp;
  snd_list_type snd_list;
{
    long final_count = susp->susp.toss_cnt;
    time_type final_time = susp->susp.t0;
    long n;

    /* fetch samples from bowpress_env up to final_time for this block of zeros */
    while ((round((final_time - susp->bowpress_env->t0) * susp->bowpress_env->sr)) >=
	   susp->bowpress_env->current)
	susp_get_samples(bowpress_env, bowpress_env_ptr, bowpress_env_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = round((final_time - susp->bowpress_env->t0) * susp->bowpress_env->sr -
         (susp->bowpress_env->current - susp->bowpress_env_cnt));
    susp->bowpress_env_ptr += n;
    susp_took(bowpress_env_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(susp, snd_list);
}


void bowed_mark(bowed_susp_type susp)
{
    sound_xlmark(susp->bowpress_env);
}


void bowed_free(bowed_susp_type susp)
{

	    deleteInstrument(susp->mybow);
    sound_unref(susp->bowpress_env);
    ffree_generic(susp, sizeof(bowed_susp_node), "bowed_free");
}


void bowed_print_tree(bowed_susp_type susp, int n)
{
    indent(n);
    stdputstr("bowpress_env:");
    sound_print_tree_1(susp->bowpress_env, n);
}


sound_type snd_make_bowed(double freq, sound_type bowpress_env, rate_type sr)
{
    register bowed_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = bowpress_env->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, bowed_susp_node, "snd_make_bowed");
    susp->mybow = initInstrument(BOWED, round(sr));
    controlChange(susp->mybow, 1, 0.0);;
    susp->temp_ret_value = noteOn(susp->mybow, freq, 1.0);
    susp->susp.fetch = bowed_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < bowpress_env->t0) sound_prepend_zeros(bowpress_env, t0);
    /* minimum start time over all inputs: */
    t0_min = min(bowpress_env->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
	susp->susp.keep_fetch = susp->susp.fetch;
	susp->susp.fetch = bowed_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = bowed_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = bowed_mark;
    susp->susp.print_tree = bowed_print_tree;
    susp->susp.name = "bowed";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->bowpress_env = bowpress_env;
    susp->bowpress_env_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_bowed(double freq, sound_type bowpress_env, rate_type sr)
{
    sound_type bowpress_env_copy = sound_copy(bowpress_env);
    return snd_make_bowed(freq, bowpress_env_copy, sr);
}
