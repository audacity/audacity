#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "delaycv.h"

void delaycv_free(snd_susp_type a_susp);


typedef struct delaycv_susp_struct {
    snd_susp_node susp;
    boolean started;
    long terminate_cnt;
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
    sound_type feedback;
    long feedback_cnt;
    sample_block_values_type feedback_ptr;

    /* support for interpolation of feedback */
    sample_type feedback_x1_sample;
    double feedback_pHaSe;
    double feedback_pHaSe_iNcR;

    /* support for ramp between samples of feedback */
    double output_per_feedback;
    long feedback_n;

    long delaylen;
    sample_type *delaybuf;
    sample_type *delayptr;
    sample_type *endptr;
} delaycv_susp_node, *delaycv_susp_type;


void delaycv_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type feedback_ptr_reg;
    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "delaycv_nn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the s input sample block: */
	susp_check_term_samples(s, s_ptr, s_cnt);
	togo = min(togo, susp->s_cnt);

	/* don't run past the feedback input sample block: */
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	togo = min(togo, susp->feedback_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_ptr_reg = susp->feedback_ptr;
	s_ptr_reg = susp->s_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * *feedback_ptr_reg++ + *s_ptr_reg++;
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	} while (--n); /* inner loop */

	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using feedback_ptr_reg is a bad idea on RS/6000: */
	susp->feedback_ptr += togo;
	/* using s_ptr_reg is a bad idea on RS/6000: */
	susp->s_ptr += togo;
	out_ptr += togo;
	susp_took(s_cnt, togo);
	susp_took(feedback_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_nn_fetch */


void delaycv_ns_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_type feedback_scale_reg = susp->feedback->scale;
    register sample_block_values_type feedback_ptr_reg;
    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "delaycv_ns_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the s input sample block: */
	susp_check_term_samples(s, s_ptr, s_cnt);
	togo = min(togo, susp->s_cnt);

	/* don't run past the feedback input sample block: */
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	togo = min(togo, susp->feedback_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_ptr_reg = susp->feedback_ptr;
	s_ptr_reg = susp->s_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * (feedback_scale_reg * *feedback_ptr_reg++) + *s_ptr_reg++;
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	} while (--n); /* inner loop */

	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using feedback_ptr_reg is a bad idea on RS/6000: */
	susp->feedback_ptr += togo;
	/* using s_ptr_reg is a bad idea on RS/6000: */
	susp->s_ptr += togo;
	out_ptr += togo;
	susp_took(s_cnt, togo);
	susp_took(feedback_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_ns_fetch */


void delaycv_ni_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type feedback_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register double feedback_pHaSe_iNcR_rEg = susp->feedback_pHaSe_iNcR;
    register double feedback_pHaSe_ReG;
    register sample_type feedback_x1_sample_reg;
    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "delaycv_ni_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	susp->feedback_x1_sample = susp_fetch_sample(feedback, feedback_ptr, feedback_cnt);
    }

    susp_check_samples(feedback, feedback_ptr, feedback_cnt);
    feedback_x2_sample = susp_current_sample(feedback, feedback_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the s input sample block: */
	susp_check_term_samples(s, s_ptr, s_cnt);
	togo = min(togo, susp->s_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_pHaSe_ReG = susp->feedback_pHaSe;
	feedback_x1_sample_reg = susp->feedback_x1_sample;
	s_ptr_reg = susp->s_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
	    if (feedback_pHaSe_ReG >= 1.0) {
		feedback_x1_sample_reg = feedback_x2_sample;
		/* pick up next sample as feedback_x2_sample: */
		susp->feedback_ptr++;
		susp_took(feedback_cnt, 1);
		feedback_pHaSe_ReG -= 1.0;
		susp_check_samples_break(feedback, feedback_ptr, feedback_cnt, feedback_x2_sample);
	    }
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * 
		(feedback_x1_sample_reg * (1 - feedback_pHaSe_ReG) + feedback_x2_sample * feedback_pHaSe_ReG) + *s_ptr_reg++;
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	    feedback_pHaSe_ReG += feedback_pHaSe_iNcR_rEg;
	} while (--n); /* inner loop */

	togo -= n;
	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	susp->feedback_pHaSe = feedback_pHaSe_ReG;
	susp->feedback_x1_sample = feedback_x1_sample_reg;
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
} /* delaycv_ni_fetch */


void delaycv_nr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type feedback_DeLtA;
    sample_type feedback_val;
    sample_type feedback_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "delaycv_nr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp->feedback_pHaSe = 1.0;
    }

    susp_check_samples(feedback, feedback_ptr, feedback_cnt);
    feedback_x2_sample = susp_current_sample(feedback, feedback_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the s input sample block: */
	susp_check_term_samples(s, s_ptr, s_cnt);
	togo = min(togo, susp->s_cnt);

	/* grab next feedback_x2_sample when phase goes past 1.0; */
	/* we use feedback_n (computed below) to avoid roundoff errors: */
	if (susp->feedback_n <= 0) {
	    susp->feedback_x1_sample = feedback_x2_sample;
	    susp->feedback_ptr++;
	    susp_took(feedback_cnt, 1);
	    susp->feedback_pHaSe -= 1.0;
	    susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	    feedback_x2_sample = susp_current_sample(feedback, feedback_ptr);
	    /* feedback_n gets number of samples before phase exceeds 1.0: */
	    susp->feedback_n = (long) ((1.0 - susp->feedback_pHaSe) *
					susp->output_per_feedback);
	}
	togo = min(togo, susp->feedback_n);
	feedback_DeLtA = (sample_type) ((feedback_x2_sample - susp->feedback_x1_sample) * susp->feedback_pHaSe_iNcR);
	feedback_val = (sample_type) (susp->feedback_x1_sample * (1.0 - susp->feedback_pHaSe) +
		 feedback_x2_sample * susp->feedback_pHaSe);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	s_ptr_reg = susp->s_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * feedback_val + *s_ptr_reg++;
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	    feedback_val += feedback_DeLtA;
	} while (--n); /* inner loop */

	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using s_ptr_reg is a bad idea on RS/6000: */
	susp->s_ptr += togo;
	out_ptr += togo;
	susp_took(s_cnt, togo);
	susp->feedback_pHaSe += togo * susp->feedback_pHaSe_iNcR;
	susp->feedback_n -= togo;
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_nr_fetch */


void delaycv_in_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type feedback_ptr_reg;
    register double s_pHaSe_iNcR_rEg = susp->s_pHaSe_iNcR;
    register double s_pHaSe_ReG;
    register sample_type s_x1_sample_reg;
    falloc_sample_block(out, "delaycv_in_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp_check_term_samples(s, s_ptr, s_cnt);
	susp->s_x1_sample = susp_fetch_sample(s, s_ptr, s_cnt);
    }

    susp_check_term_samples(s, s_ptr, s_cnt);
    s_x2_sample = susp_current_sample(s, s_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the feedback input sample block: */
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	togo = min(togo, susp->feedback_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_ptr_reg = susp->feedback_ptr;
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
		susp_check_term_samples_break(s, s_ptr, s_cnt, s_x2_sample);
	    }
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * *feedback_ptr_reg++ + 
		(s_x1_sample_reg * (1 - s_pHaSe_ReG) + s_x2_sample * s_pHaSe_ReG);
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	    s_pHaSe_ReG += s_pHaSe_iNcR_rEg;
	} while (--n); /* inner loop */

	togo -= n;
	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using feedback_ptr_reg is a bad idea on RS/6000: */
	susp->feedback_ptr += togo;
	susp->s_pHaSe = s_pHaSe_ReG;
	susp->s_x1_sample = s_x1_sample_reg;
	out_ptr += togo;
	susp_took(feedback_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_in_fetch */


void delaycv_is_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_type feedback_scale_reg = susp->feedback->scale;
    register sample_block_values_type feedback_ptr_reg;
    register double s_pHaSe_iNcR_rEg = susp->s_pHaSe_iNcR;
    register double s_pHaSe_ReG;
    register sample_type s_x1_sample_reg;
    falloc_sample_block(out, "delaycv_is_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp_check_term_samples(s, s_ptr, s_cnt);
	susp->s_x1_sample = susp_fetch_sample(s, s_ptr, s_cnt);
    }

    susp_check_term_samples(s, s_ptr, s_cnt);
    s_x2_sample = susp_current_sample(s, s_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
	/* first compute how many samples to generate in inner loop: */
	/* don't overflow the output sample block: */
	togo = max_sample_block_len - cnt;

	/* don't run past the feedback input sample block: */
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	togo = min(togo, susp->feedback_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_ptr_reg = susp->feedback_ptr;
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
		susp_check_term_samples_break(s, s_ptr, s_cnt, s_x2_sample);
	    }
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * (feedback_scale_reg * *feedback_ptr_reg++) + 
		(s_x1_sample_reg * (1 - s_pHaSe_ReG) + s_x2_sample * s_pHaSe_ReG);
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	    s_pHaSe_ReG += s_pHaSe_iNcR_rEg;
	} while (--n); /* inner loop */

	togo -= n;
	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using feedback_ptr_reg is a bad idea on RS/6000: */
	susp->feedback_ptr += togo;
	susp->s_pHaSe = s_pHaSe_ReG;
	susp->s_x1_sample = s_x1_sample_reg;
	out_ptr += togo;
	susp_took(feedback_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_is_fetch */


void delaycv_rn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_DeLtA;
    sample_type s_val;
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type feedback_ptr_reg;
    falloc_sample_block(out, "delaycv_rn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp->s_pHaSe = 1.0;
    }

    susp_check_term_samples(s, s_ptr, s_cnt);
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
	    susp_check_term_samples(s, s_ptr, s_cnt);
	    s_x2_sample = susp_current_sample(s, s_ptr);
	    /* s_n gets number of samples before phase exceeds 1.0: */
	    susp->s_n = (long) ((1.0 - susp->s_pHaSe) *
					susp->output_per_s);
	}
	togo = min(togo, susp->s_n);
	s_DeLtA = (sample_type) ((s_x2_sample - susp->s_x1_sample) * susp->s_pHaSe_iNcR);
	s_val = (sample_type) (susp->s_x1_sample * (1.0 - susp->s_pHaSe) +
		 s_x2_sample * susp->s_pHaSe);

	/* don't run past the feedback input sample block: */
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	togo = min(togo, susp->feedback_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_ptr_reg = susp->feedback_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * *feedback_ptr_reg++ + s_val;
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	    s_val += s_DeLtA;
	} while (--n); /* inner loop */

	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using feedback_ptr_reg is a bad idea on RS/6000: */
	susp->feedback_ptr += togo;
	out_ptr += togo;
	susp->s_pHaSe += togo * susp->s_pHaSe_iNcR;
	susp->s_n -= togo;
	susp_took(feedback_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_rn_fetch */


void delaycv_rs_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_DeLtA;
    sample_type s_val;
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_type feedback_scale_reg = susp->feedback->scale;
    register sample_block_values_type feedback_ptr_reg;
    falloc_sample_block(out, "delaycv_rs_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
	susp->started = true;
	susp->s_pHaSe = 1.0;
    }

    susp_check_term_samples(s, s_ptr, s_cnt);
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
	    susp_check_term_samples(s, s_ptr, s_cnt);
	    s_x2_sample = susp_current_sample(s, s_ptr);
	    /* s_n gets number of samples before phase exceeds 1.0: */
	    susp->s_n = (long) ((1.0 - susp->s_pHaSe) *
					susp->output_per_s);
	}
	togo = min(togo, susp->s_n);
	s_DeLtA = (sample_type) ((s_x2_sample - susp->s_x1_sample) * susp->s_pHaSe_iNcR);
	s_val = (sample_type) (susp->s_x1_sample * (1.0 - susp->s_pHaSe) +
		 s_x2_sample * susp->s_pHaSe);

	/* don't run past the feedback input sample block: */
	susp_check_samples(feedback, feedback_ptr, feedback_cnt);
	togo = min(togo, susp->feedback_cnt);

	/* don't run past terminate time */
	if (susp->terminate_cnt != UNKNOWN &&
	    susp->terminate_cnt <= susp->susp.current + cnt + togo) {
	    togo = susp->terminate_cnt - (susp->susp.current + cnt);
	    if (togo < 0) togo = 0;  /* avoids rounding errros */
	    if (togo == 0) break;
	}

	n = togo;
	delayptr_reg = susp->delayptr;
	endptr_reg = susp->endptr;
	feedback_ptr_reg = susp->feedback_ptr;
	out_ptr_reg = out_ptr;
	if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = *delayptr_reg;
            *delayptr_reg = *delayptr_reg * (feedback_scale_reg * *feedback_ptr_reg++) + s_val;
            if (++delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
	    s_val += s_DeLtA;
	} while (--n); /* inner loop */

	susp->delayptr = delayptr_reg;
	susp->endptr = endptr_reg;
	/* using feedback_ptr_reg is a bad idea on RS/6000: */
	susp->feedback_ptr += togo;
	out_ptr += togo;
	susp->s_pHaSe += togo * susp->s_pHaSe_iNcR;
	susp->s_n -= togo;
	susp_took(feedback_cnt, togo);
	cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
	snd_list_terminate(snd_list);
    } else {
	snd_list->block_len = cnt;
	susp->susp.current += cnt;
    }
} /* delaycv_rs_fetch */


void delaycv_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
    {
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    long n;

    /* fetch samples from s up to final_time for this block of zeros */
    while ((round((final_time - susp->s->t0) * susp->s->sr)) >=
	   susp->s->current)
	susp_get_samples(s, s_ptr, s_cnt);
    /* fetch samples from feedback up to final_time for this block of zeros */
    while ((round((final_time - susp->feedback->t0) * susp->feedback->sr)) >=
	   susp->feedback->current)
	susp_get_samples(feedback, feedback_ptr, feedback_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = round((final_time - susp->s->t0) * susp->s->sr -
         (susp->s->current - susp->s_cnt));
    susp->s_ptr += n;
    susp_took(s_cnt, n);
    n = round((final_time - susp->feedback->t0) * susp->feedback->sr -
         (susp->feedback->current - susp->feedback_cnt));
    susp->feedback_ptr += n;
    susp_took(feedback_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void delaycv_mark(snd_susp_type a_susp)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    sound_xlmark(susp->s);
    sound_xlmark(susp->feedback);
}


void delaycv_free(snd_susp_type a_susp)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
free(susp->delaybuf);
    sound_unref(susp->s);
    sound_unref(susp->feedback);
    ffree_generic(susp, sizeof(delaycv_susp_node), "delaycv_free");
}


void delaycv_print_tree(snd_susp_type a_susp, int n)
{
    delaycv_susp_type susp = (delaycv_susp_type) a_susp;
    indent(n);
    stdputstr("s:");
    sound_print_tree_1(susp->s, n);

    indent(n);
    stdputstr("feedback:");
    sound_print_tree_1(susp->feedback, n);
}


sound_type snd_make_delaycv(sound_type s, time_type delay, sound_type feedback)
{
    register delaycv_susp_type susp;
    rate_type sr = max(s->sr, feedback->sr);
    time_type t0 = max(s->t0, feedback->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    /* combine scale factors of linear inputs (S) */
    scale_factor *= s->scale;
    s->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (s->sr < sr) { s->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, delaycv_susp_node, "snd_make_delaycv");
    susp->delaylen = round(s->sr * delay);
    susp->delaybuf = (sample_type *) calloc (sizeof(double), susp->delaylen);
    susp->delayptr = susp->delaybuf;
    susp->endptr = susp->delaybuf + susp->delaylen;

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s, sr);
    interp_desc = (interp_desc << 2) + interp_style(feedback, sr);
    switch (interp_desc) {
      case INTERP_nn: susp->susp.fetch = delaycv_nn_fetch; break;
      case INTERP_ns: susp->susp.fetch = delaycv_ns_fetch; break;
      case INTERP_ni: susp->susp.fetch = delaycv_ni_fetch; break;
      case INTERP_nr: susp->susp.fetch = delaycv_nr_fetch; break;
      case INTERP_in: susp->susp.fetch = delaycv_in_fetch; break;
      case INTERP_is: susp->susp.fetch = delaycv_is_fetch; break;
      case INTERP_rn: susp->susp.fetch = delaycv_rn_fetch; break;
      case INTERP_rs: susp->susp.fetch = delaycv_rs_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s->t0) sound_prepend_zeros(s, t0);
    if (t0 < feedback->t0) sound_prepend_zeros(feedback, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s->t0, min(feedback->t0, t0));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = delaycv_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = delaycv_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = delaycv_mark;
    susp->susp.print_tree = delaycv_print_tree;
    susp->susp.name = "delaycv";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->started = false;
    susp->susp.current = 0;
    susp->s = s;
    susp->s_cnt = 0;
    susp->s_pHaSe = 0.0;
    susp->s_pHaSe_iNcR = s->sr / sr;
    susp->s_n = 0;
    susp->output_per_s = sr / s->sr;
    susp->feedback = feedback;
    susp->feedback_cnt = 0;
    susp->feedback_pHaSe = 0.0;
    susp->feedback_pHaSe_iNcR = feedback->sr / sr;
    susp->feedback_n = 0;
    susp->output_per_feedback = sr / feedback->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_delaycv(sound_type s, time_type delay, sound_type feedback)
{
    sound_type s_copy = sound_copy(s);
    sound_type feedback_copy = sound_copy(feedback);
    return snd_make_delaycv(s_copy, delay, feedback_copy);
}
