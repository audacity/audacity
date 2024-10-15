#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "maxv.h"

void maxv_free(snd_susp_type a_susp);


typedef struct maxv_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s1;
    int s1_cnt;
    sample_block_values_type s1_ptr;
    sound_type s2;
    int s2_cnt;
    sample_block_values_type s2_ptr;

    /* support for interpolation of s2 */
    sample_type s2_x1_sample;
    double s2_pHaSe;
    double s2_pHaSe_iNcR;

    /* support for ramp between samples of s2 */
    double output_per_s2;
    int64_t s2_n;
} maxv_susp_node, *maxv_susp_type;


void maxv_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_block_values_type s2_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_nn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the s2 input sample block: */
        susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
        togo = min(togo, susp->s2_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s2_ptr_reg = susp->s2_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            {
		double x1 = *s1_ptr_reg++;
		double x2 = *s2_ptr_reg++;
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
        } while (--n); /* inner loop */

        /* using s2_ptr_reg is a bad idea on RS/6000: */
        susp->s2_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(s2_cnt, togo);
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
} /* maxv_nn_fetch */


void maxv_ns_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type s2_scale_reg = susp->s2->scale;
    register sample_block_values_type s2_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_ns_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the s2 input sample block: */
        susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
        togo = min(togo, susp->s2_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s2_ptr_reg = susp->s2_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            {
		double x1 = *s1_ptr_reg++;
		double x2 = (s2_scale_reg * *s2_ptr_reg++);
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
        } while (--n); /* inner loop */

        /* using s2_ptr_reg is a bad idea on RS/6000: */
        susp->s2_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(s2_cnt, togo);
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
} /* maxv_ns_fetch */


void maxv_ni_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s2_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double s2_pHaSe_iNcR_rEg = susp->s2_pHaSe_iNcR;
    register double s2_pHaSe_ReG;
    register sample_type s2_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_ni_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
        susp->s2_x1_sample = susp_fetch_sample(s2, s2_ptr, s2_cnt);
    }

    susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
    s2_x2_sample = susp_current_sample(s2, s2_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s2_pHaSe_ReG = susp->s2_pHaSe;
        s2_x1_sample_reg = susp->s2_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            if (s2_pHaSe_ReG >= 1.0) {
                s2_x1_sample_reg = s2_x2_sample;
                /* pick up next sample as s2_x2_sample: */
                susp->s2_ptr++;
                susp_took(s2_cnt, 1);
                s2_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(s2, s2_ptr, s2_cnt, s2_x2_sample);
            }
            {
		double x1 = *s1_ptr_reg++;
		double x2 = 
                (s2_x1_sample_reg * (1 - s2_pHaSe_ReG) + s2_x2_sample * s2_pHaSe_ReG);
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
            s2_pHaSe_ReG += s2_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->s2_pHaSe = s2_pHaSe_ReG;
        susp->s2_x1_sample = s2_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
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
} /* maxv_ni_fetch */


void maxv_nr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s2_DeLtA;
    sample_type s2_val;
    sample_type s2_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_nr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->s2_pHaSe = 1.0;
    }

    susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
    s2_x2_sample = susp_current_sample(s2, s2_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next s2_x2_sample when phase goes past 1.0; */
        /* we use s2_n (computed below) to avoid roundoff errors: */
        if (susp->s2_n <= 0) {
            susp->s2_x1_sample = s2_x2_sample;
            susp->s2_ptr++;
            susp_took(s2_cnt, 1);
            susp->s2_pHaSe -= 1.0;
            susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
            s2_x2_sample = susp_current_sample(s2, s2_ptr);
            /* s2_n gets number of samples before phase exceeds 1.0: */
            susp->s2_n = (int64_t) ((1.0 - susp->s2_pHaSe) *
                                        susp->output_per_s2);
        }
        togo = (int) min(togo, susp->s2_n);
        s2_DeLtA = (sample_type) ((s2_x2_sample - susp->s2_x1_sample) * susp->s2_pHaSe_iNcR);
        s2_val = (sample_type) (susp->s2_x1_sample * (1.0 - susp->s2_pHaSe) +
                 s2_x2_sample * susp->s2_pHaSe);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            {
		double x1 = *s1_ptr_reg++;
		double x2 = s2_val;
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
            s2_val += s2_DeLtA;
        } while (--n); /* inner loop */

        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->s2_pHaSe += togo * susp->s2_pHaSe_iNcR;
        susp->s2_n -= togo;
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
} /* maxv_nr_fetch */


void maxv_ss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type s2_scale_reg = susp->s2->scale;
    register sample_block_values_type s2_ptr_reg;
    register sample_type s1_scale_reg = susp->s1->scale;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_ss_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the s2 input sample block: */
        susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
        togo = min(togo, susp->s2_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s2_ptr_reg = susp->s2_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            {
		double x1 = (s1_scale_reg * *s1_ptr_reg++);
		double x2 = (s2_scale_reg * *s2_ptr_reg++);
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
        } while (--n); /* inner loop */

        /* using s2_ptr_reg is a bad idea on RS/6000: */
        susp->s2_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(s2_cnt, togo);
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
} /* maxv_ss_fetch */


void maxv_si_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s2_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double s2_pHaSe_iNcR_rEg = susp->s2_pHaSe_iNcR;
    register double s2_pHaSe_ReG;
    register sample_type s2_x1_sample_reg;
    register sample_type s1_scale_reg = susp->s1->scale;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_si_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
        susp->s2_x1_sample = susp_fetch_sample(s2, s2_ptr, s2_cnt);
    }

    susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
    s2_x2_sample = susp_current_sample(s2, s2_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s2_pHaSe_ReG = susp->s2_pHaSe;
        s2_x1_sample_reg = susp->s2_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            if (s2_pHaSe_ReG >= 1.0) {
                s2_x1_sample_reg = s2_x2_sample;
                /* pick up next sample as s2_x2_sample: */
                susp->s2_ptr++;
                susp_took(s2_cnt, 1);
                s2_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(s2, s2_ptr, s2_cnt, s2_x2_sample);
            }
            {
		double x1 = (s1_scale_reg * *s1_ptr_reg++);
		double x2 = 
                (s2_x1_sample_reg * (1 - s2_pHaSe_ReG) + s2_x2_sample * s2_pHaSe_ReG);
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
            s2_pHaSe_ReG += s2_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->s2_pHaSe = s2_pHaSe_ReG;
        susp->s2_x1_sample = s2_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
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
} /* maxv_si_fetch */


void maxv_sr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s2_DeLtA;
    sample_type s2_val;
    sample_type s2_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type s1_scale_reg = susp->s1->scale;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "maxv_sr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->s2_pHaSe = 1.0;
    }

    susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
    s2_x2_sample = susp_current_sample(s2, s2_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next s2_x2_sample when phase goes past 1.0; */
        /* we use s2_n (computed below) to avoid roundoff errors: */
        if (susp->s2_n <= 0) {
            susp->s2_x1_sample = s2_x2_sample;
            susp->s2_ptr++;
            susp_took(s2_cnt, 1);
            susp->s2_pHaSe -= 1.0;
            susp_check_term_log_samples(s2, s2_ptr, s2_cnt);
            s2_x2_sample = susp_current_sample(s2, s2_ptr);
            /* s2_n gets number of samples before phase exceeds 1.0: */
            susp->s2_n = (int64_t) ((1.0 - susp->s2_pHaSe) *
                                        susp->output_per_s2);
        }
        togo = (int) min(togo, susp->s2_n);
        s2_DeLtA = (sample_type) ((s2_x2_sample - susp->s2_x1_sample) * susp->s2_pHaSe_iNcR);
        s2_val = (sample_type) (susp->s2_x1_sample * (1.0 - susp->s2_pHaSe) +
                 s2_x2_sample * susp->s2_pHaSe);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
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
                    togo = (int) to_stop;
            }
        }

        n = togo;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            {
		double x1 = (s1_scale_reg * *s1_ptr_reg++);
		double x2 = s2_val;
		*out_ptr_reg++ = (sample_type) (x1 > x2 ? x1 : x2);
	    };
            s2_val += s2_DeLtA;
        } while (--n); /* inner loop */

        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->s2_pHaSe += togo * susp->s2_pHaSe_iNcR;
        susp->s2_n -= togo;
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
} /* maxv_sr_fetch */


void maxv_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from s1 up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr)) >=
           susp->s1->current)
        susp_get_samples(s1, s1_ptr, s1_cnt);
    /* fetch samples from s2 up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s2->t0) * susp->s2->sr)) >=
           susp->s2->current)
        susp_get_samples(s2, s2_ptr, s2_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr -
         (susp->s1->current - susp->s1_cnt));
    susp->s1_ptr += n;
    susp_took(s1_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->s2->t0) * susp->s2->sr -
         (susp->s2->current - susp->s2_cnt));
    susp->s2_ptr += n;
    susp_took(s2_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void maxv_mark(snd_susp_type a_susp)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    sound_xlmark(susp->s1);
    sound_xlmark(susp->s2);
}


void maxv_free(snd_susp_type a_susp)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    sound_unref(susp->s1);
    sound_unref(susp->s2);
    ffree_generic(susp, sizeof(maxv_susp_node), "maxv_free");
}


void maxv_print_tree(snd_susp_type a_susp, int n)
{
    maxv_susp_type susp = (maxv_susp_type) a_susp;
    indent(n);
    stdputstr("s1:");
    sound_print_tree_1(susp->s1, n);

    indent(n);
    stdputstr("s2:");
    sound_print_tree_1(susp->s2, n);
}


sound_type snd_make_maxv(sound_type s1, sound_type s2)
{
    register maxv_susp_type susp;
    rate_type sr = max(s1->sr, s2->sr);
    time_type t0 = max(s1->t0, s2->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    int64_t lsc;
    /* sort commutative signals: (S1 S2) */
    snd_sort_2(&s1, &s2, sr);

    falloc_generic(susp, maxv_susp_node, "snd_make_maxv");

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s1, sr);
    interp_desc = (interp_desc << 2) + interp_style(s2, sr);
    switch (interp_desc) {
      case INTERP_nn: susp->susp.fetch = maxv_nn_fetch; break;
      case INTERP_ns: susp->susp.fetch = maxv_ns_fetch; break;
      case INTERP_ni: susp->susp.fetch = maxv_ni_fetch; break;
      case INTERP_nr: susp->susp.fetch = maxv_nr_fetch; break;
      case INTERP_ss: susp->susp.fetch = maxv_ss_fetch; break;
      case INTERP_si: susp->susp.fetch = maxv_si_fetch; break;
      case INTERP_sr: susp->susp.fetch = maxv_sr_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s1->t0) sound_prepend_zeros(s1, t0);
    if (t0 < s2->t0) sound_prepend_zeros(s2, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s1->t0, min(s2->t0, t0));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = maxv_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = maxv_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = maxv_mark;
    susp->susp.print_tree = maxv_print_tree;
    susp->susp.name = "maxv";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s1);
    lsc = logical_stop_cnt_cvt(s2);
    if (susp->susp.log_stop_cnt > lsc)
        susp->susp.log_stop_cnt = lsc;
    susp->started = false;
    susp->susp.current = 0;
    susp->s1 = s1;
    susp->s1_cnt = 0;
    susp->s2 = s2;
    susp->s2_cnt = 0;
    susp->s2_pHaSe = 0.0;
    susp->s2_pHaSe_iNcR = s2->sr / sr;
    susp->s2_n = 0;
    susp->output_per_s2 = sr / s2->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_maxv(sound_type s1, sound_type s2)
{
    sound_type s1_copy = sound_copy(s1);
    sound_type s2_copy = sound_copy(s2);
    return snd_make_maxv(s1_copy, s2_copy);
}
