#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "partial.h"

void partial_free();


typedef struct partial_susp_struct {
    snd_susp_node susp;
    long terminate_cnt;
    boolean logically_stopped;
    sound_type env;
    long env_cnt;
    sample_block_values_type env_ptr;

    long phase;
    long ph_incr;
} partial_susp_node, *partial_susp_type;


#include "sine.h"


void partial_n_fetch(register partial_susp_type susp, snd_list_type snd_list)
{
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    register sample_block_values_type env_ptr_reg;
    falloc_sample_block(out, "partial_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the env input sample block: */
	susp_check_term_log_samples(env, env_ptr, env_cnt);
	togo = min(togo, susp->env_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo == 0) break;
	}


	/* don't run past logical stop time */
	if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
	    int to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
	    /* break if to_stop == 0 (we're at the logical stop)
	     * AND cnt > 0 (we're not at the beginning of the
	     * output block).
	     */
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
		    togo = to_stop;
	    }
	}

	n = togo;
	phase_reg = susp->phase;
	ph_incr_reg = susp->ph_incr;
	env_ptr_reg = susp->env_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
*out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT] * *env_ptr_reg++;
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;;
	} while (--n); /* inner loop */

	susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
	/* using env_ptr_reg is a bad idea on RS/6000: */
	susp->env_ptr += togo;
	out_ptr += togo;
	susp_took(env_cnt, togo);
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
} /* partial_n_fetch */


void partial_s_fetch(register partial_susp_type susp, snd_list_type snd_list)
{
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    register sample_type env_scale_reg = susp->env->scale;
    register sample_block_values_type env_ptr_reg;
    falloc_sample_block(out, "partial_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the env input sample block: */
	susp_check_term_log_samples(env, env_ptr, env_cnt);
	togo = min(togo, susp->env_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo == 0) break;
	}


	/* don't run past logical stop time */
	if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
	    int to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
	    /* break if to_stop == 0 (we're at the logical stop)
	     * AND cnt > 0 (we're not at the beginning of the
	     * output block).
	     */
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
		    togo = to_stop;
	    }
	}

	n = togo;
	phase_reg = susp->phase;
	ph_incr_reg = susp->ph_incr;
	env_ptr_reg = susp->env_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
*out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT] * (env_scale_reg * *env_ptr_reg++);
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;;
	} while (--n); /* inner loop */

	susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
	/* using env_ptr_reg is a bad idea on RS/6000: */
	susp->env_ptr += togo;
	out_ptr += togo;
	susp_took(env_cnt, togo);
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
} /* partial_s_fetch */


void partial_toss_fetch(susp, snd_list)
  register partial_susp_type susp;
  snd_list_type snd_list;
{
    long final_count = susp->susp.toss_cnt;
    time_type final_time = susp->susp.t0;
    long n;

    /* fetch samples from env up to final_time for this block of zeros */
    while ((round((final_time - susp->env->t0) * susp->env->sr)) >=
	   susp->env->current)
	susp_get_samples(env, env_ptr, env_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = round((final_time - susp->env->t0) * susp->env->sr -
         (susp->env->current - susp->env_cnt));
    susp->env_ptr += n;
    susp_took(env_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(susp, snd_list);
}


void partial_mark(partial_susp_type susp)
{
    sound_xlmark(susp->env);
}


void partial_free(partial_susp_type susp)
{
    sound_unref(susp->env);
    ffree_generic(susp, sizeof(partial_susp_node), "partial_free");
}


void partial_print_tree(partial_susp_type susp, int n)
{
    indent(n);
    stdputstr("env:");
    sound_print_tree_1(susp->env, n);
}


sound_type snd_make_partial(rate_type sr, double hz, sound_type env)
{
    register partial_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = env->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, partial_susp_node, "snd_make_partial");
    susp->phase = 0;
    susp->ph_incr = round((hz * SINE_TABLE_LEN) * (1 << SINE_TABLE_SHIFT) / sr);

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(env, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = partial_n_fetch; break;
      case INTERP_s: susp->susp.fetch = partial_s_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < env->t0) sound_prepend_zeros(env, t0);
    /* minimum start time over all inputs: */
    t0_min = min(env->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
	susp->susp.keep_fetch = susp->susp.fetch;
	susp->susp.fetch = partial_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = partial_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = partial_mark;
    susp->susp.print_tree = partial_print_tree;
    susp->susp.name = "partial";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(env);
    susp->susp.current = 0;
    susp->env = env;
    susp->env_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_partial(rate_type sr, double hz, sound_type env)
{
    sound_type env_copy = sound_copy(env);
    return snd_make_partial(sr, hz, env_copy);
}
