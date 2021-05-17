#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "upsample.h"

void up_free(snd_susp_type a_susp);


typedef struct up_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type input;
    int input_cnt;
    sample_block_values_type input_ptr;

    /* support for interpolation of input */
    sample_type input_x1_sample;
    double input_pHaSe;
    double input_pHaSe_iNcR;

    /* support for ramp between samples of input */
    double output_per_input;
    int64_t input_n;
} up_susp_node, *up_susp_type;


void up_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    up_susp_type susp = (up_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "up_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_log_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

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
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) *input_ptr_reg++;
        } while (--n); /* inner loop */

        /* using input_ptr_reg is a bad idea on RS/6000: */
        susp->input_ptr += togo;
        out_ptr += togo;
        susp_took(input_cnt, togo);
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
} /* up_n_fetch */


void up_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    up_susp_type susp = (up_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type input_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double input_pHaSe_iNcR_rEg = susp->input_pHaSe_iNcR;
    register double input_pHaSe_ReG;
    register sample_type input_x1_sample_reg;
    falloc_sample_block(out, "up_i_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(input, input_ptr, input_cnt);
        susp->input_x1_sample = susp_fetch_sample(input, input_ptr, input_cnt);
    }

    susp_check_term_log_samples(input, input_ptr, input_cnt);
    input_x2_sample = susp_current_sample(input, input_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

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
        input_pHaSe_ReG = susp->input_pHaSe;
        input_x1_sample_reg = susp->input_x1_sample;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            if (input_pHaSe_ReG >= 1.0) {
                input_x1_sample_reg = input_x2_sample;
                /* pick up next sample as input_x2_sample: */
                susp->input_ptr++;
                susp_took(input_cnt, 1);
                input_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(input, input_ptr, input_cnt, input_x2_sample);
            }
            *out_ptr_reg++ = (sample_type) 
                (input_x1_sample_reg * (1 - input_pHaSe_ReG) + input_x2_sample * input_pHaSe_ReG);
            input_pHaSe_ReG += input_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->input_pHaSe = input_pHaSe_ReG;
        susp->input_x1_sample = input_x1_sample_reg;
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
} /* up_i_fetch */


void up_r_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    up_susp_type susp = (up_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type input_DeLtA;
    sample_type input_val;
    sample_type input_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    falloc_sample_block(out, "up_r_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->input_pHaSe = 1.0;
    }

    susp_check_term_log_samples(input, input_ptr, input_cnt);
    input_x2_sample = susp_current_sample(input, input_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* grab next input_x2_sample when phase goes past 1.0; */
        /* we use input_n (computed below) to avoid roundoff errors: */
        if (susp->input_n <= 0) {
            susp->input_x1_sample = input_x2_sample;
            susp->input_ptr++;
            susp_took(input_cnt, 1);
            susp->input_pHaSe -= 1.0;
            susp_check_term_log_samples(input, input_ptr, input_cnt);
            input_x2_sample = susp_current_sample(input, input_ptr);
            /* input_n gets number of samples before phase exceeds 1.0: */
            susp->input_n = (int64_t) ((1.0 - susp->input_pHaSe) *
                                        susp->output_per_input);
        }
        togo = (int) min(togo, susp->input_n);
        input_DeLtA = (sample_type) ((input_x2_sample - susp->input_x1_sample) * susp->input_pHaSe_iNcR);
        input_val = (sample_type) (susp->input_x1_sample * (1.0 - susp->input_pHaSe) +
                 input_x2_sample * susp->input_pHaSe);

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
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) input_val;
            input_val += input_DeLtA;
        } while (--n); /* inner loop */

        out_ptr += togo;
        susp->input_pHaSe += togo * susp->input_pHaSe_iNcR;
        susp->input_n -= togo;
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
} /* up_r_fetch */


void up_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    up_susp_type susp = (up_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from input up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->input->t0) * susp->input->sr)) >=
           susp->input->current)
        susp_get_samples(input, input_ptr, input_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->input->t0) * susp->input->sr -
         (susp->input->current - susp->input_cnt));
    susp->input_ptr += n;
    susp_took(input_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void up_mark(snd_susp_type a_susp)
{
    up_susp_type susp = (up_susp_type) a_susp;
    sound_xlmark(susp->input);
}


void up_free(snd_susp_type a_susp)
{
    up_susp_type susp = (up_susp_type) a_susp;
    sound_unref(susp->input);
    ffree_generic(susp, sizeof(up_susp_node), "up_free");
}


void up_print_tree(snd_susp_type a_susp, int n)
{
    up_susp_type susp = (up_susp_type) a_susp;
    indent(n);
    stdputstr("input:");
    sound_print_tree_1(susp->input, n);
}


sound_type snd_make_up(rate_type sr, sound_type input)
{
    register up_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = input->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    /* combine scale factors of linear inputs (INPUT) */
    scale_factor *= input->scale;
    input->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (input->sr < sr) { input->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, up_susp_node, "snd_make_up");

    /* make sure no sample rate is too high */
    if (input->sr > sr) {
        sound_unref(input);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(input, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = up_n_fetch; break;
      case INTERP_i: susp->susp.fetch = up_i_fetch; break;
      case INTERP_r: susp->susp.fetch = up_r_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < input->t0) sound_prepend_zeros(input, t0);
    /* minimum start time over all inputs: */
    t0_min = min(input->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = up_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = up_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = up_mark;
    susp->susp.print_tree = up_print_tree;
    susp->susp.name = "up";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(input);
    susp->started = false;
    susp->susp.current = 0;
    susp->input = input;
    susp->input_cnt = 0;
    susp->input_pHaSe = 0.0;
    susp->input_pHaSe_iNcR = input->sr / sr;
    susp->input_n = 0;
    susp->output_per_input = sr / input->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_up(rate_type sr, sound_type input)
{
    sound_type input_copy = sound_copy(input);
    return snd_make_up(sr, input_copy);
}
