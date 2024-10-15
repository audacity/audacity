#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "fmfbv.h"

void fmfbv_free(snd_susp_type a_susp);


typedef struct fmfbv_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type index;
    int index_cnt;
    sample_block_values_type index_ptr;

    /* support for interpolation of index */
    sample_type index_x1_sample;
    double index_pHaSe;
    double index_pHaSe_iNcR;

    /* support for ramp between samples of index */
    double output_per_index;
    int64_t index_n;

    double yy;
    double sin_y;
    double phase;
    double ph_incr;
} fmfbv_susp_node, *fmfbv_susp_type;


void fmfbv_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double yy_reg;
    register double sin_y_reg;
    register double phase_reg;
    register double ph_incr_reg;
    register sample_block_values_type index_ptr_reg;
    falloc_sample_block(out, "fmfbv_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the index input sample block: */
        susp_check_term_log_samples(index, index_ptr, index_cnt);
        togo = min(togo, susp->index_cnt);

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
        yy_reg = susp->yy;
        sin_y_reg = susp->sin_y;
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        index_ptr_reg = susp->index_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            phase_reg += ph_incr_reg;
            if (phase_reg > SINE_TABLE_LEN) phase_reg -= SINE_TABLE_LEN;
            /* PHASE is incremented and INDEX scaled to table INDEX, and
               sin_y_reg is a signal (-1 to +1) */
            yy_reg = phase_reg + *index_ptr_reg++ * sin_y_reg;
            /* so yy_reg is a table index */
            while (yy_reg > SINE_TABLE_LEN) yy_reg -= SINE_TABLE_LEN;
            while (yy_reg < 0) yy_reg += SINE_TABLE_LEN;
            sin_y_reg = sine_table[(int) yy_reg]; /* truncation gets valid index */
            /* sin_y_reg is now a signal ready for table lookup */
            *out_ptr_reg++ =  (sample_type) sin_y_reg;
        } while (--n); /* inner loop */

        susp->yy = yy_reg;
        susp->sin_y = sin_y_reg;
        susp->phase = phase_reg;
        /* using index_ptr_reg is a bad idea on RS/6000: */
        susp->index_ptr += togo;
        out_ptr += togo;
        susp_took(index_cnt, togo);
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
} /* fmfbv_n_fetch */


void fmfbv_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double yy_reg;
    register double sin_y_reg;
    register double phase_reg;
    register double ph_incr_reg;
    register sample_type index_scale_reg = susp->index->scale;
    register sample_block_values_type index_ptr_reg;
    falloc_sample_block(out, "fmfbv_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the index input sample block: */
        susp_check_term_log_samples(index, index_ptr, index_cnt);
        togo = min(togo, susp->index_cnt);

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
        yy_reg = susp->yy;
        sin_y_reg = susp->sin_y;
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        index_ptr_reg = susp->index_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            phase_reg += ph_incr_reg;
            if (phase_reg > SINE_TABLE_LEN) phase_reg -= SINE_TABLE_LEN;
            /* PHASE is incremented and INDEX scaled to table INDEX, and
               sin_y_reg is a signal (-1 to +1) */
            yy_reg = phase_reg + (index_scale_reg * *index_ptr_reg++) * sin_y_reg;
            /* so yy_reg is a table index */
            while (yy_reg > SINE_TABLE_LEN) yy_reg -= SINE_TABLE_LEN;
            while (yy_reg < 0) yy_reg += SINE_TABLE_LEN;
            sin_y_reg = sine_table[(int) yy_reg]; /* truncation gets valid index */
            /* sin_y_reg is now a signal ready for table lookup */
            *out_ptr_reg++ =  (sample_type) sin_y_reg;
        } while (--n); /* inner loop */

        susp->yy = yy_reg;
        susp->sin_y = sin_y_reg;
        susp->phase = phase_reg;
        /* using index_ptr_reg is a bad idea on RS/6000: */
        susp->index_ptr += togo;
        out_ptr += togo;
        susp_took(index_cnt, togo);
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
} /* fmfbv_s_fetch */


void fmfbv_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double yy_reg;
    register double sin_y_reg;
    register double phase_reg;
    register double ph_incr_reg;
    register double index_pHaSe_iNcR_rEg = susp->index_pHaSe_iNcR;
    register double index_pHaSe_ReG;
    register sample_type index_x1_sample_reg;
    falloc_sample_block(out, "fmfbv_i_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(index, index_ptr, index_cnt);
        susp->index_x1_sample = susp_fetch_sample(index, index_ptr, index_cnt);
    }

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
        yy_reg = susp->yy;
        sin_y_reg = susp->sin_y;
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        index_pHaSe_ReG = susp->index_pHaSe;
        index_x1_sample_reg = susp->index_x1_sample;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            if (index_pHaSe_ReG >= 1.0) {
/* fixup-depends index */
                /* pick up next sample as index_x1_sample: */
                susp->index_ptr++;
                susp_took(index_cnt, 1);
                index_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(index, index_ptr, index_cnt, index_x1_sample_reg);
                index_x1_sample_reg = susp_current_sample(index, index_ptr);
            }
            phase_reg += ph_incr_reg;
            if (phase_reg > SINE_TABLE_LEN) phase_reg -= SINE_TABLE_LEN;
            /* PHASE is incremented and INDEX scaled to table INDEX, and
               sin_y_reg is a signal (-1 to +1) */
            yy_reg = phase_reg + index_x1_sample_reg * sin_y_reg;
            /* so yy_reg is a table index */
            while (yy_reg > SINE_TABLE_LEN) yy_reg -= SINE_TABLE_LEN;
            while (yy_reg < 0) yy_reg += SINE_TABLE_LEN;
            sin_y_reg = sine_table[(int) yy_reg]; /* truncation gets valid index */
            /* sin_y_reg is now a signal ready for table lookup */
            *out_ptr_reg++ =  (sample_type) sin_y_reg;
            index_pHaSe_ReG += index_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->yy = yy_reg;
        susp->sin_y = sin_y_reg;
        susp->phase = phase_reg;
        susp->index_pHaSe = index_pHaSe_ReG;
        susp->index_x1_sample = index_x1_sample_reg;
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
} /* fmfbv_i_fetch */


void fmfbv_r_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type index_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double yy_reg;
    register double sin_y_reg;
    register double phase_reg;
    register double ph_incr_reg;
    falloc_sample_block(out, "fmfbv_r_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->index_pHaSe = 1.0;
    }

    susp_check_term_log_samples(index, index_ptr, index_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* grab next index_x1_sample when phase goes past 1.0; */
        /* use index_n (computed below) to avoid roundoff errors: */
        if (susp->index_n <= 0) {
            susp_check_term_log_samples(index, index_ptr, index_cnt);
            susp->index_x1_sample = susp_fetch_sample(index, index_ptr, index_cnt);
            susp->index_pHaSe -= 1.0;
            /* index_n gets number of samples before phase exceeds 1.0: */
            susp->index_n = (int64_t) ((1.0 - susp->index_pHaSe) *
                                        susp->output_per_index);
        }
        togo = (int) min(togo, susp->index_n);
        index_val = susp->index_x1_sample;
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
        yy_reg = susp->yy;
        sin_y_reg = susp->sin_y;
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            phase_reg += ph_incr_reg;
            if (phase_reg > SINE_TABLE_LEN) phase_reg -= SINE_TABLE_LEN;
            /* PHASE is incremented and INDEX scaled to table INDEX, and
               sin_y_reg is a signal (-1 to +1) */
            yy_reg = phase_reg + index_val * sin_y_reg;
            /* so yy_reg is a table index */
            while (yy_reg > SINE_TABLE_LEN) yy_reg -= SINE_TABLE_LEN;
            while (yy_reg < 0) yy_reg += SINE_TABLE_LEN;
            sin_y_reg = sine_table[(int) yy_reg]; /* truncation gets valid index */
            /* sin_y_reg is now a signal ready for table lookup */
            *out_ptr_reg++ =  (sample_type) sin_y_reg;
        } while (--n); /* inner loop */

        susp->yy = yy_reg;
        susp->sin_y = sin_y_reg;
        susp->phase = phase_reg;
        out_ptr += togo;
        susp->index_pHaSe += togo * susp->index_pHaSe_iNcR;
        susp->index_n -= togo;
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
} /* fmfbv_r_fetch */


void fmfbv_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from index up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->index->t0) * susp->index->sr)) >=
           susp->index->current)
        susp_get_samples(index, index_ptr, index_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->index->t0) * susp->index->sr -
         (susp->index->current - susp->index_cnt));
    susp->index_ptr += n;
    susp_took(index_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void fmfbv_mark(snd_susp_type a_susp)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    sound_xlmark(susp->index);
}


void fmfbv_free(snd_susp_type a_susp)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    sound_unref(susp->index);
    ffree_generic(susp, sizeof(fmfbv_susp_node), "fmfbv_free");
}


void fmfbv_print_tree(snd_susp_type a_susp, int n)
{
    fmfbv_susp_type susp = (fmfbv_susp_type) a_susp;
    indent(n);
    stdputstr("index:");
    sound_print_tree_1(susp->index, n);
}


sound_type snd_make_fmfbv(time_type t0, double hz, rate_type sr, sound_type index)
{
    register fmfbv_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, fmfbv_susp_node, "snd_make_fmfbv");
    susp->yy = 0.0;
    susp->sin_y = 0.0;
    susp->phase = 0.0;
    susp->ph_incr = hz * SINE_TABLE_LEN / sr;
    index->scale *= SINE_TABLE_LEN / (sample_type) PI2
;

    /* make sure no sample rate is too high */
    if (index->sr > sr) {
        sound_unref(index);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(index, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = fmfbv_n_fetch; break;
      case INTERP_s: susp->susp.fetch = fmfbv_s_fetch; break;
      case INTERP_i: susp->susp.fetch = fmfbv_i_fetch; break;
      case INTERP_r: susp->susp.fetch = fmfbv_r_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < index->t0) sound_prepend_zeros(index, t0);
    /* minimum start time over all inputs: */
    t0_min = min(index->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = fmfbv_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = fmfbv_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = fmfbv_mark;
    susp->susp.print_tree = fmfbv_print_tree;
    susp->susp.name = "fmfbv";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(index);
    susp->started = false;
    susp->susp.current = 0;
    susp->index = index;
    susp->index_cnt = 0;
    susp->index_pHaSe = 0.0;
    susp->index_pHaSe_iNcR = index->sr / sr;
    susp->index_n = 0;
    susp->output_per_index = sr / index->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_fmfbv(time_type t0, double hz, rate_type sr, sound_type index)
{
    sound_type index_copy = sound_copy(index);
    return snd_make_fmfbv(t0, hz, sr, index_copy);
}
