#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "downproto.h"

void down_free();


typedef struct down_susp_struct {
    snd_susp_node susp;
    boolean started;
    long terminate_cnt;
    boolean logically_stopped;
    sound_type s;
    long s_cnt;
    sample_block_values_type s_ptr;

    /* support for interpolation of s */
    sample_type s_x1_sample;
    double s_pHaSe;
    double s_pHaSe_iNcR;

    /* support for ramp between samples of s */
    double output_per_s;
    long s_n;
} down_susp_node, *down_susp_type;


void down_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    down_susp_type susp = (down_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "down_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the s input sample block: */
	susp_check_term_log_samples(s, s_ptr, s_cnt);
	togo = min(togo, susp->s_cnt);

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
	s_ptr_reg = susp->s_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = *s_ptr_reg++;
	} while (--n); /* inner loop */

	/* using s_ptr_reg is a bad idea on RS/6000: */
	susp->s_ptr += togo;
	out_ptr += togo;
	susp_took(s_cnt, togo);
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
} /* down_n_fetch */


void down_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    down_susp_type susp = (down_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type s_scale_reg = susp->s->scale;
    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "down_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the s input sample block: */
	susp_check_term_log_samples(s, s_ptr, s_cnt);
	togo = min(togo, susp->s_cnt);

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
	s_ptr_reg = susp->s_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (s_scale_reg * *s_ptr_reg++);
	} while (--n); /* inner loop */

	/* using s_ptr_reg is a bad idea on RS/6000: */
	susp->s_ptr += togo;
	out_ptr += togo;
	susp_took(s_cnt, togo);
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
} /* down_s_fetch */


void down_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    down_susp_type susp = (down_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double s_pHaSe_iNcR_rEg = susp->s_pHaSe_iNcR;
    register double s_pHaSe_ReG;
    register sample_type s_x1_sample_reg;
    falloc_sample_block(out, "down_i_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp_check_term_log_samples(s, s_ptr, s_cnt);
	susp->s_x1_sample = susp_fetch_sample(s, s_ptr, s_cnt);
    }

    susp_check_term_log_samples(s, s_ptr, s_cnt);
    s_x2_sample = susp_current_sample(s, s_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

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
	s_pHaSe_ReG = susp->s_pHaSe;
	s_x1_sample_reg = susp->s_x1_sample;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
	    if (s_pHaSe_ReG >= 1.0) {
		s_x1_sample_reg = s_x2_sample;
		/* pick up next sample as s_x2_sample: */
		susp->s_ptr++;
		susp_took(s_cnt, 1);
		s_pHaSe_ReG -= 1.0;
		susp_check_term_log_samples_break(s, s_ptr, s_cnt, s_x2_sample);
	    }
            *out_ptr_reg++ = 
		(s_x1_sample_reg * (1 - s_pHaSe_ReG) + s_x2_sample * s_pHaSe_ReG);
	    s_pHaSe_ReG += s_pHaSe_iNcR_rEg;
	} while (--n); /* inner loop */

	togo -= n;
	susp->s_pHaSe = s_pHaSe_ReG;
	susp->s_x1_sample = s_x1_sample_reg;
	out_ptr += togo;
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
} /* down_i_fetch */


void down_r_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    down_susp_type susp = (down_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_DeLtA;
    sample_type s_val;
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    falloc_sample_block(out, "down_r_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp->s_pHaSe = 1.0;
    }

    susp_check_term_log_samples(s, s_ptr, s_cnt);
    s_x2_sample = susp_current_sample(s, s_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* grab next s_x2_sample when phase goes past 1.0; */
	/* we use s_n (computed below) to avoid roundoff errors: */
	if (susp->s_n <= 0) {
	    susp->s_x1_sample = s_x2_sample;
	    susp->s_ptr++;
	    susp_took(s_cnt, 1);
	    susp->s_pHaSe -= 1.0;
	    susp_check_term_log_samples(s, s_ptr, s_cnt);
	    s_x2_sample = susp_current_sample(s, s_ptr);
	    /* s_n gets number of samples before phase exceeds 1.0: */
	    susp->s_n = (long) ((1.0 - susp->s_pHaSe) *
					susp->output_per_s);
	}
	togo = min(togo, susp->s_n);
	s_DeLtA = (sample_type) ((s_x2_sample - susp->s_x1_sample) * susp->s_pHaSe_iNcR);
	s_val = (sample_type) (susp->s_x1_sample * (1.0 - susp->s_pHaSe) +
		 s_x2_sample * susp->s_pHaSe);

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
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = s_val;
	    s_val += s_DeLtA;
	} while (--n); /* inner loop */

	out_ptr += togo;
	susp->s_pHaSe += togo * susp->s_pHaSe_iNcR;
	susp->s_n -= togo;
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
} /* down_r_fetch */


void down_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
    {
    down_susp_type susp = (down_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    long n;

    /* fetch samples from s up to final_time for this block of zeros */
    while ((round((final_time - susp->s->t0) * susp->s->sr)) >=
	   susp->s->current)
	susp_get_samples(s, s_ptr, s_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = round((final_time - susp->s->t0) * susp->s->sr -
         (susp->s->current - susp->s_cnt));
    susp->s_ptr += n;
    susp_took(s_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void down_mark(snd_susp_type a_susp)
{
    down_susp_type susp = (down_susp_type) a_susp;
    sound_xlmark(susp->s);
}


void down_free(snd_susp_type a_susp)
{
    down_susp_type susp = (down_susp_type) a_susp;
    sound_unref(susp->s);
    ffree_generic(susp, sizeof(down_susp_node), "down_free");
}


void down_print_tree(snd_susp_type a_susp, int n)
{
    down_susp_type susp = (down_susp_type) a_susp;
    indent(n);
    stdputstr("s:");
    sound_print_tree_1(susp->s, n);
}


sound_type snd_make_down(rate_type sr, sound_type s)
{
    register down_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = s->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    if (s->sr < sr) {
        sound_unref(s);
        xlfail("snd-down: output sample rate must be lower than input");
    }
    falloc_generic(susp, down_susp_node, "snd_make_down");

    /* make sure no sample rate is too high */
    if (s->sr > sr) {
        sound_unref(s);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = down_n_fetch; break;
      case INTERP_s: susp->susp.fetch = down_s_fetch; break;
      case INTERP_i: susp->susp.fetch = down_i_fetch; break;
      case INTERP_r: susp->susp.fetch = down_r_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s->t0) sound_prepend_zeros(s, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = down_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = down_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = down_mark;
    susp->susp.print_tree = down_print_tree;
    susp->susp.name = "down";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s);
    susp->started = false;
    susp->susp.current = 0;
    susp->s = s;
    susp->s_cnt = 0;
    susp->s_pHaSe = 0.0;
    susp->s_pHaSe_iNcR = s->sr / sr;
    susp->s_n = 0;
    susp->output_per_s = sr / s->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_down(rate_type sr, sound_type s)
{
    sound_type s_copy = sound_copy(s);
    return snd_make_down(sr, s_copy);
}
