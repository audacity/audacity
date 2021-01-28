#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "siosc.h"

void siosc_free(snd_susp_type a_susp);


typedef struct siosc_susp_struct {
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

    double table_len;
    double ph_incr;
    table_type table_a_ptr;
    table_type table_b_ptr_ptr;
    sample_type *table_a_samps;
    sample_type *table_b_samps;
    double table_sr;
    double phase;
    LVAL lis;
    int64_t next_breakpoint;
    double ampramp_a;
    double ampramp_b;
    double ampslope;
} siosc_susp_node, *siosc_susp_type;


char *siosc_bad_table_list = "bad table list in SIOSC";

/* sisosc_table_init -- set up first two tables for interpolation */
/**/
void siosc_table_init(siosc_susp_type susp)
{
    sound_type snd;
    if (!susp->lis || !consp(susp->lis) || !soundp(car(susp->lis)))
        xlfail(siosc_bad_table_list);
    snd = getsound(car(susp->lis));
    susp->table_b_ptr_ptr = sound_to_table(snd);
    susp->table_b_samps = susp->table_b_ptr_ptr->samples;
    susp->lis = cdr(susp->lis);
    susp->table_sr = snd->sr;
    susp->table_len = susp->table_b_ptr_ptr->length;
}

/* siosc_table_update -- outer loop processing, get next table */
/**/
int64_t siosc_table_update(siosc_susp_type susp, int64_t cur)
{
    int64_t n;

    /* swap ampramps: */
    susp->ampramp_a = 1.0;
    susp->ampramp_b = 0.0;

    /* swap tables: */
    table_unref(susp->table_a_ptr);
    susp->table_a_ptr = susp->table_b_ptr_ptr;
    susp->table_a_samps = susp->table_b_samps;
    susp->table_b_ptr_ptr = NULL; /* so we do not try to unref it */

    if (susp->lis) {
        sound_type snd;

        /* compute slope */
        if (!consp(susp->lis) || !fixp(car(susp->lis)))
            xlfail(siosc_bad_table_list);
        susp->next_breakpoint = getfixnum(car(susp->lis));
        susp->lis = cdr(susp->lis);
        n = susp->next_breakpoint - cur;
        susp->ampslope = 1.0 / n;

        /* build new table: */
        if (!susp->lis || !consp(susp->lis) || !soundp(car(susp->lis)))
            xlfail("bad table list in SIOSC");
        snd = getsound(car(susp->lis));
        susp->table_b_ptr_ptr = sound_to_table(snd);
        susp->table_b_samps = susp->table_b_ptr_ptr->samples;
        if (susp->table_b_ptr_ptr->length != susp->table_len || susp->table_sr != snd->sr)
            xlfail("mismatched tables passed to SIOSC") ;
        susp->lis = cdr(susp->lis);
    } else { /* use only table a */
        susp->ampslope = 0.0;
        susp->next_breakpoint = n = 0x7FFFFFFFFFFFFFFF;
    }
    return n;
}


void siosc_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double table_len_reg;
    register double ph_incr_reg;
    register sample_type * table_a_samps_reg;
    register sample_type * table_b_samps_reg;
    register double phase_reg;
    register double ampramp_a_reg;
    register double ampramp_b_reg;
    register double ampslope_reg;
    register sample_type s_fm_scale_reg = susp->s_fm->scale;
    register sample_block_values_type s_fm_ptr_reg;
    falloc_sample_block(out, "siosc_s_fetch");
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


        { int64_t cur = susp->susp.current + cnt;
          int64_t n64 = susp->next_breakpoint - cur;
          if (n64 == 0) n64 = siosc_table_update(susp, cur);
          if (n64 < togo) togo = (int) n64;
        }

        n = togo;
        table_len_reg = susp->table_len;
        ph_incr_reg = susp->ph_incr;
        table_a_samps_reg = susp->table_a_samps;
        table_b_samps_reg = susp->table_b_samps;
        phase_reg = susp->phase;
        ampramp_a_reg = susp->ampramp_a;
        ampramp_b_reg = susp->ampramp_b;
        ampslope_reg = susp->ampslope;
        s_fm_ptr_reg = susp->s_fm_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index;
            double xa, xb;
            table_index = (long) phase_reg;
            xa = table_a_samps_reg[table_index];
            xb = table_b_samps_reg[table_index];
            *out_ptr_reg++ = (sample_type) 
                      (ampramp_a_reg * (xa + (phase_reg - table_index) * 
                          (table_a_samps_reg[table_index + 1] - xa)) +
                       ampramp_b_reg * (xb + (phase_reg - table_index) * 
                          (table_b_samps_reg[table_index + 1] - xb)));
            ampramp_a_reg -= ampslope_reg;
            ampramp_b_reg += ampslope_reg;
            phase_reg += ph_incr_reg + (s_fm_scale_reg * *s_fm_ptr_reg++);
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
            /* watch out for negative frequencies! */
            while (phase_reg < 0) phase_reg += table_len_reg;
        } while (--n); /* inner loop */

        susp->phase = phase_reg;
        susp->ampramp_a = ampramp_a_reg;
        susp->ampramp_b = ampramp_b_reg;
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
} /* siosc_s_fetch */


void siosc_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double table_len_reg;
    register double ph_incr_reg;
    register sample_type * table_a_samps_reg;
    register sample_type * table_b_samps_reg;
    register double phase_reg;
    register double ampramp_a_reg;
    register double ampramp_b_reg;
    register double ampslope_reg;
    register double s_fm_pHaSe_iNcR_rEg = susp->s_fm_pHaSe_iNcR;
    register double s_fm_pHaSe_ReG;
    register sample_type s_fm_x1_sample_reg;
    falloc_sample_block(out, "siosc_i_fetch");
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


        { int64_t cur = susp->susp.current + cnt;
          int64_t n64 = susp->next_breakpoint - cur;
          if (n64 == 0) n64 = siosc_table_update(susp, cur);
          if (n64 < togo) togo = (int) n64;
        }

        n = togo;
        table_len_reg = susp->table_len;
        ph_incr_reg = susp->ph_incr;
        table_a_samps_reg = susp->table_a_samps;
        table_b_samps_reg = susp->table_b_samps;
        phase_reg = susp->phase;
        ampramp_a_reg = susp->ampramp_a;
        ampramp_b_reg = susp->ampramp_b;
        ampslope_reg = susp->ampslope;
        s_fm_pHaSe_ReG = susp->s_fm_pHaSe;
        s_fm_x1_sample_reg = susp->s_fm_x1_sample;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index;
            double xa, xb;
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
            xa = table_a_samps_reg[table_index];
            xb = table_b_samps_reg[table_index];
            *out_ptr_reg++ = (sample_type) 
                      (ampramp_a_reg * (xa + (phase_reg - table_index) * 
                          (table_a_samps_reg[table_index + 1] - xa)) +
                       ampramp_b_reg * (xb + (phase_reg - table_index) * 
                          (table_b_samps_reg[table_index + 1] - xb)));
            ampramp_a_reg -= ampslope_reg;
            ampramp_b_reg += ampslope_reg;
            phase_reg += ph_incr_reg + s_fm_x1_sample_reg;
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
            /* watch out for negative frequencies! */
            while (phase_reg < 0) phase_reg += table_len_reg;
            s_fm_pHaSe_ReG += s_fm_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->phase = phase_reg;
        susp->ampramp_a = ampramp_a_reg;
        susp->ampramp_b = ampramp_b_reg;
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
} /* siosc_i_fetch */


void siosc_r_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_fm_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double table_len_reg;
    register double ph_incr_reg;
    register sample_type * table_a_samps_reg;
    register sample_type * table_b_samps_reg;
    register double phase_reg;
    register double ampramp_a_reg;
    register double ampramp_b_reg;
    register double ampslope_reg;
    falloc_sample_block(out, "siosc_r_fetch");
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


        { int64_t cur = susp->susp.current + cnt;
          int64_t n64 = susp->next_breakpoint - cur;
          if (n64 == 0) n64 = siosc_table_update(susp, cur);
          if (n64 < togo) togo = (int) n64;
        }

        n = togo;
        table_len_reg = susp->table_len;
        ph_incr_reg = susp->ph_incr;
        table_a_samps_reg = susp->table_a_samps;
        table_b_samps_reg = susp->table_b_samps;
        phase_reg = susp->phase;
        ampramp_a_reg = susp->ampramp_a;
        ampramp_b_reg = susp->ampramp_b;
        ampslope_reg = susp->ampslope;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index;
            double xa, xb;
            table_index = (long) phase_reg;
            xa = table_a_samps_reg[table_index];
            xb = table_b_samps_reg[table_index];
            *out_ptr_reg++ = (sample_type) 
                      (ampramp_a_reg * (xa + (phase_reg - table_index) * 
                          (table_a_samps_reg[table_index + 1] - xa)) +
                       ampramp_b_reg * (xb + (phase_reg - table_index) * 
                          (table_b_samps_reg[table_index + 1] - xb)));
            ampramp_a_reg -= ampslope_reg;
            ampramp_b_reg += ampslope_reg;
            phase_reg += ph_incr_reg + s_fm_val;
            while (phase_reg > table_len_reg) phase_reg -= table_len_reg;
            /* watch out for negative frequencies! */
            while (phase_reg < 0) phase_reg += table_len_reg;
        } while (--n); /* inner loop */

        susp->phase = phase_reg;
        susp->ampramp_a = ampramp_a_reg;
        susp->ampramp_b = ampramp_b_reg;
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
} /* siosc_r_fetch */


void siosc_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
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


void siosc_mark(snd_susp_type a_susp)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
    if (susp->lis) mark(susp->lis);
    sound_xlmark(susp->s_fm);
}


void siosc_free(snd_susp_type a_susp)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
    table_unref(susp->table_a_ptr);
    table_unref(susp->table_b_ptr_ptr);
    sound_unref(susp->s_fm);
    ffree_generic(susp, sizeof(siosc_susp_node), "siosc_free");
}


void siosc_print_tree(snd_susp_type a_susp, int n)
{
    siosc_susp_type susp = (siosc_susp_type) a_susp;
    indent(n);
    stdputstr("s_fm:");
    sound_print_tree_1(susp->s_fm, n);
}


sound_type snd_make_siosc(LVAL lis, rate_type sr, double hz, time_type t0, sound_type s_fm)
{
    register siosc_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, siosc_susp_node, "snd_make_siosc");
    susp->table_len = 0.0;
    susp->ph_incr = 0.0;
    susp->table_a_ptr = NULL;
    susp->table_b_ptr_ptr = NULL;
    susp->table_a_samps = NULL;
    susp->table_b_samps = NULL;
    susp->table_sr = 0.0;
    susp->phase = 0.0;
    susp->lis = lis;
    susp->next_breakpoint = 0;
    susp->ampramp_a = 1.0;
    susp->ampramp_b = 0.0;
    susp->ampslope = 0.0;
    siosc_table_init(susp);
    susp->ph_incr = hz * susp->table_len / sr;
    s_fm->scale = (sample_type) (s_fm->scale * (susp->table_len / sr));

    /* make sure no sample rate is too high */
    if (s_fm->sr > sr) {
        sound_unref(s_fm);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s_fm, sr);
    switch (interp_desc) {
      case INTERP_n: /* handled below */
      case INTERP_s: susp->susp.fetch = siosc_s_fetch; break;
      case INTERP_i: susp->susp.fetch = siosc_i_fetch; break;
      case INTERP_r: susp->susp.fetch = siosc_r_fetch; break;
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
        susp->susp.fetch = siosc_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = siosc_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = siosc_mark;
    susp->susp.print_tree = siosc_print_tree;
    susp->susp.name = "siosc";
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


sound_type snd_siosc(LVAL lis, rate_type sr, double hz, time_type t0, sound_type s_fm)
{
    sound_type s_fm_copy = sound_copy(s_fm);
    return snd_make_siosc(lis, sr, hz, t0, s_fm_copy);
}
