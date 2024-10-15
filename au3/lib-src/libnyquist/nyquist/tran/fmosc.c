#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "fmosc.h"

void fmosc_free(snd_susp_type a_susp);


typedef struct fmosc_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s_fm;
    int s_fm_cnt;
    sample_block_values_type s_fm_ptr;

    /* support for interpolation of s_fm */
    sample_type s_fm_x1_sample;
    double s_fm_pHaSe;
    double s_fm_pHaSe_iNcR;

    /* support for ramp between samples of s_fm */
    double output_per_s_fm;
    int64_t s_fm_n;

    table_type the_table;
    double table_len;
    double ph_incr;
    sample_type *table_ptr;
    double phase;
} fmosc_susp_node, *fmosc_susp_type;


void fmosc_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double table_len_reg;
    register double ph_incr_reg;
    register sample_type * table_ptr_reg;
    register double phase_reg;
    register sample_type s_fm_scale_reg = susp->s_fm->scale;
    register sample_block_values_type s_fm_ptr_reg;
    falloc_sample_block(out, "fmosc_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s_fm input sample block: */
        susp_check_term_log_samples(s_fm, s_fm_ptr, s_fm_cnt);
        togo = min(togo, susp->s_fm_cnt);

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
        table_len_reg = susp->table_len;
        ph_incr_reg = susp->ph_incr;
        table_ptr_reg = susp->table_ptr;
        phase_reg = susp->phase;
        s_fm_ptr_reg = susp->s_fm_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index;
            double x1;
            table_index = (long) phase_reg;
            x1 = table_ptr_reg[table_index];
            *out_ptr_reg++ = (sample_type) (x1 + (phase_reg - table_index) * 
                          (table_ptr_reg[table_index + 1] - x1));
            phase_reg += ph_incr_reg + (s_fm_scale_reg * *s_fm_ptr_reg++);
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
            /* watch out for negative frequencies! */
            while (phase_reg < 0) phase_reg += table_len_reg;
        } while (--n); /* inner loop */

        susp->phase = phase_reg;
        /* using s_fm_ptr_reg is a bad idea on RS/6000: */
        susp->s_fm_ptr += togo;
        out_ptr += togo;
        susp_took(s_fm_cnt, togo);
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
} /* fmosc_s_fetch */


void fmosc_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double table_len_reg;
    register double ph_incr_reg;
    register sample_type * table_ptr_reg;
    register double phase_reg;
    register double s_fm_pHaSe_iNcR_rEg = susp->s_fm_pHaSe_iNcR;
    register double s_fm_pHaSe_ReG;
    register sample_type s_fm_x1_sample_reg;
    falloc_sample_block(out, "fmosc_i_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(s_fm, s_fm_ptr, s_fm_cnt);
        susp->s_fm_x1_sample = susp_fetch_sample(s_fm, s_fm_ptr, s_fm_cnt);
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
        table_len_reg = susp->table_len;
        ph_incr_reg = susp->ph_incr;
        table_ptr_reg = susp->table_ptr;
        phase_reg = susp->phase;
        s_fm_pHaSe_ReG = susp->s_fm_pHaSe;
        s_fm_x1_sample_reg = susp->s_fm_x1_sample;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index;
            double x1;
            if (s_fm_pHaSe_ReG >= 1.0) {
/* fixup-depends s_fm */
                /* pick up next sample as s_fm_x1_sample: */
                susp->s_fm_ptr++;
                susp_took(s_fm_cnt, 1);
                s_fm_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(s_fm, s_fm_ptr, s_fm_cnt, s_fm_x1_sample_reg);
                s_fm_x1_sample_reg = susp_current_sample(s_fm, s_fm_ptr);
            }
            table_index = (long) phase_reg;
            x1 = table_ptr_reg[table_index];
            *out_ptr_reg++ = (sample_type) (x1 + (phase_reg - table_index) * 
                          (table_ptr_reg[table_index + 1] - x1));
            phase_reg += ph_incr_reg + s_fm_x1_sample_reg;
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
            /* watch out for negative frequencies! */
            while (phase_reg < 0) phase_reg += table_len_reg;
            s_fm_pHaSe_ReG += s_fm_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->phase = phase_reg;
        susp->s_fm_pHaSe = s_fm_pHaSe_ReG;
        susp->s_fm_x1_sample = s_fm_x1_sample_reg;
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
} /* fmosc_i_fetch */


void fmosc_r_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_fm_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double table_len_reg;
    register double ph_incr_reg;
    register sample_type * table_ptr_reg;
    register double phase_reg;
    falloc_sample_block(out, "fmosc_r_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->s_fm_pHaSe = 1.0;
    }

    susp_check_term_log_samples(s_fm, s_fm_ptr, s_fm_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* grab next s_fm_x1_sample when phase goes past 1.0; */
        /* use s_fm_n (computed below) to avoid roundoff errors: */
        if (susp->s_fm_n <= 0) {
            susp_check_term_log_samples(s_fm, s_fm_ptr, s_fm_cnt);
            susp->s_fm_x1_sample = susp_fetch_sample(s_fm, s_fm_ptr, s_fm_cnt);
            susp->s_fm_pHaSe -= 1.0;
            /* s_fm_n gets number of samples before phase exceeds 1.0: */
            susp->s_fm_n = (int64_t) ((1.0 - susp->s_fm_pHaSe) *
                                        susp->output_per_s_fm);
        }
        togo = (int) min(togo, susp->s_fm_n);
        s_fm_val = susp->s_fm_x1_sample;
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
        table_len_reg = susp->table_len;
        ph_incr_reg = susp->ph_incr;
        table_ptr_reg = susp->table_ptr;
        phase_reg = susp->phase;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index;
            double x1;
            table_index = (long) phase_reg;
            x1 = table_ptr_reg[table_index];
            *out_ptr_reg++ = (sample_type) (x1 + (phase_reg - table_index) * 
                          (table_ptr_reg[table_index + 1] - x1));
            phase_reg += ph_incr_reg + s_fm_val;
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
            /* watch out for negative frequencies! */
            while (phase_reg < 0) phase_reg += table_len_reg;
        } while (--n); /* inner loop */

        susp->phase = phase_reg;
        out_ptr += togo;
        susp->s_fm_pHaSe += togo * susp->s_fm_pHaSe_iNcR;
        susp->s_fm_n -= togo;
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
} /* fmosc_r_fetch */


void fmosc_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from s_fm up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s_fm->t0) * susp->s_fm->sr)) >=
           susp->s_fm->current)
        susp_get_samples(s_fm, s_fm_ptr, s_fm_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->s_fm->t0) * susp->s_fm->sr -
         (susp->s_fm->current - susp->s_fm_cnt));
    susp->s_fm_ptr += n;
    susp_took(s_fm_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void fmosc_mark(snd_susp_type a_susp)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    sound_xlmark(susp->s_fm);
}


void fmosc_free(snd_susp_type a_susp)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    table_unref(susp->the_table);
    sound_unref(susp->s_fm);
    ffree_generic(susp, sizeof(fmosc_susp_node), "fmosc_free");
}


void fmosc_print_tree(snd_susp_type a_susp, int n)
{
    fmosc_susp_type susp = (fmosc_susp_type) a_susp;
    indent(n);
    stdputstr("s_fm:");
    sound_print_tree_1(susp->s_fm, n);
}


sound_type snd_make_fmosc(sound_type s, double step, rate_type sr, double hz, time_type t0, sound_type s_fm, double phase)
{
    register fmosc_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, fmosc_susp_node, "snd_make_fmosc");
    susp->the_table = sound_to_table(s);
    susp->table_len = susp->the_table->length;
    susp->ph_incr = 0;
    susp->table_ptr = susp->the_table->samples;
    susp->phase = compute_phase(phase, step, (long) susp->table_len,
        s->sr, sr, hz, &susp->ph_incr);
    s_fm->scale *= (sample_type) (hz != 0 ? (susp->ph_incr / hz)
                                          : s->sr / (sr * step_to_hz(step)));

    /* make sure no sample rate is too high */
    if (s_fm->sr > sr) {
        sound_unref(s_fm);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s_fm, sr);
    switch (interp_desc) {
      case INTERP_n: /* handled below */
      case INTERP_s: susp->susp.fetch = fmosc_s_fetch; break;
      case INTERP_i: susp->susp.fetch = fmosc_i_fetch; break;
      case INTERP_r: susp->susp.fetch = fmosc_r_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s_fm->t0) sound_prepend_zeros(s_fm, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s_fm->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = fmosc_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = fmosc_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = fmosc_mark;
    susp->susp.print_tree = fmosc_print_tree;
    susp->susp.name = "fmosc";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s_fm);
    susp->started = false;
    susp->susp.current = 0;
    susp->s_fm = s_fm;
    susp->s_fm_cnt = 0;
    susp->s_fm_pHaSe = 0.0;
    susp->s_fm_pHaSe_iNcR = s_fm->sr / sr;
    susp->s_fm_n = 0;
    susp->output_per_s_fm = sr / s_fm->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_fmosc(sound_type s, double step, rate_type sr, double hz, time_type t0, sound_type s_fm, double phase)
{
    sound_type s_fm_copy = sound_copy(s_fm);
    return snd_make_fmosc(s, step, sr, hz, t0, s_fm_copy, phase);
}
