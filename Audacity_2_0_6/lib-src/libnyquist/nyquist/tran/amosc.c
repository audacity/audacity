#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "amosc.h"

void amosc_free();


typedef struct amosc_susp_struct {
    snd_susp_node susp;
    long terminate_cnt;
    boolean logically_stopped;
    sound_type amod;
    long amod_cnt;
    sample_block_values_type amod_ptr;

    double ph_incr;
    table_type the_table;
    sample_type *table_ptr;
    double table_len;
    double phase;
} amosc_susp_node, *amosc_susp_type;


void amosc_s_fetch(register amosc_susp_type susp, snd_list_type snd_list)
{
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double ph_incr_reg;
    register sample_type * table_ptr_reg;
    register double table_len_reg;
    register double phase_reg;
    register sample_type amod_scale_reg = susp->amod->scale;
    register sample_block_values_type amod_ptr_reg;
    falloc_sample_block(out, "amosc_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the amod input sample block: */
	susp_check_term_log_samples(amod, amod_ptr, amod_cnt);
	togo = min(togo, susp->amod_cnt);

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
	ph_incr_reg = susp->ph_incr;
	table_ptr_reg = susp->table_ptr;
	table_len_reg = susp->table_len;
	phase_reg = susp->phase;
	amod_ptr_reg = susp->amod_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */

            long table_index = (long) phase_reg;
            double x1 = (double) (table_ptr_reg[table_index]);
            *out_ptr_reg++ = (sample_type) (x1 + (phase_reg - table_index) * 
                          (table_ptr_reg[table_index + 1] - x1)) * (amod_scale_reg * *amod_ptr_reg++);
            phase_reg += ph_incr_reg;
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
;
	} while (--n); /* inner loop */

	susp->phase = phase_reg;
	/* using amod_ptr_reg is a bad idea on RS/6000: */
	susp->amod_ptr += togo;
	out_ptr += togo;
	susp_took(amod_cnt, togo);
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
} /* amosc_s_fetch */


void amosc_toss_fetch(susp, snd_list)
  register amosc_susp_type susp;
  snd_list_type snd_list;
{
    long final_count = susp->susp.toss_cnt;
    time_type final_time = susp->susp.t0;
    long n;

    /* fetch samples from amod up to final_time for this block of zeros */
    while ((round((final_time - susp->amod->t0) * susp->amod->sr)) >=
	   susp->amod->current)
	susp_get_samples(amod, amod_ptr, amod_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = round((final_time - susp->amod->t0) * susp->amod->sr -
         (susp->amod->current - susp->amod_cnt));
    susp->amod_ptr += n;
    susp_took(amod_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(susp, snd_list);
}


void amosc_mark(amosc_susp_type susp)
{
    sound_xlmark(susp->amod);
}


void amosc_free(amosc_susp_type susp)
{
    table_unref(susp->the_table);
    sound_unref(susp->amod);
    ffree_generic(susp, sizeof(amosc_susp_node), "amosc_free");
}


void amosc_print_tree(amosc_susp_type susp, int n)
{
    indent(n);
    stdputstr("amod:");
    sound_print_tree_1(susp->amod, n);
}


sound_type snd_make_amosc(sound_type input, double step, rate_type sr, double hz, time_type t0, sound_type amod, double phase)
{
    register amosc_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, amosc_susp_node, "snd_make_amosc");
    susp->ph_incr = 0;
    susp->the_table = sound_to_table(input);
    susp->table_ptr = susp->the_table->samples;
    susp->table_len = susp->the_table->length;
    susp->phase = compute_phase(phase, step, (long) susp->table_len,
        input->sr, sr, hz, &susp->ph_incr);
    susp->susp.fetch = amosc_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < amod->t0) sound_prepend_zeros(amod, t0);
    /* minimum start time over all inputs: */
    t0_min = min(amod->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
	susp->susp.keep_fetch = susp->susp.fetch;
	susp->susp.fetch = amosc_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = amosc_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = amosc_mark;
    susp->susp.print_tree = amosc_print_tree;
    susp->susp.name = "amosc";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(amod);
    susp->susp.current = 0;
    susp->amod = amod;
    susp->amod_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_amosc(sound_type input, double step, rate_type sr, double hz, time_type t0, sound_type amod, double phase)
{
    sound_type amod_copy = sound_copy(amod);
    return snd_make_amosc(input, step, sr, hz, t0, amod_copy, phase);
}
