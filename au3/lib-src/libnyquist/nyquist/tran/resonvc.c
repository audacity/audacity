#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "resonvc.h"

void resonvc_free(snd_susp_type a_susp);


typedef struct resonvc_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s1;
    int s1_cnt;
    sample_block_values_type s1_ptr;
    sound_type hz;
    int hz_cnt;
    sample_block_values_type hz_ptr;

    /* support for interpolation of hz */
    sample_type hz_x1_sample;
    double hz_pHaSe;
    double hz_pHaSe_iNcR;

    /* support for ramp between samples of hz */
    double output_per_hz;
    int64_t hz_n;

    double scale1;
    double c3co;
    double c3p1;
    double c3t4;
    double omc3;
    double c2;
    double c1;
    int normalization;
    double y1;
    double y2;
} resonvc_susp_node, *resonvc_susp_type;


void resonvc_ns_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double scale1_reg;
    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double c2_reg;
    register double c1_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_type hz_scale_reg = susp->hz->scale;
    register sample_block_values_type hz_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "resonvc_ns_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the hz input sample block: */
        susp_check_term_samples(hz, hz_ptr, hz_cnt);
        togo = min(togo, susp->hz_cnt);

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
        scale1_reg = susp->scale1;
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        hz_ptr_reg = susp->hz_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            c2_reg = c3t4_reg * cos((hz_scale_reg * *hz_ptr_reg++)) / c3p1_reg;
            c1_reg = (normalization_reg == 0 ? scale1_reg :
          (normalization_reg == 1 ? omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
              sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * omc3_reg / c3p1_reg)) * scale1_reg;
            { double y0 = c1_reg * *s1_ptr_reg++ + c2_reg * y1_reg - c3co_reg * y2_reg;
              *out_ptr_reg++ = (sample_type) y0;
              y2_reg = y1_reg; y1_reg = y0; };
        } while (--n); /* inner loop */

        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using hz_ptr_reg is a bad idea on RS/6000: */
        susp->hz_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(hz_cnt, togo);
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
} /* resonvc_ns_fetch */


void resonvc_ni_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double scale1_reg;
    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double c2_reg;
    register double c1_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register double hz_pHaSe_iNcR_rEg = susp->hz_pHaSe_iNcR;
    register double hz_pHaSe_ReG;
    register sample_type hz_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "resonvc_ni_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(hz, hz_ptr, hz_cnt);
        susp->hz_x1_sample = susp_fetch_sample(hz, hz_ptr, hz_cnt);
        susp->c2 = susp->c3t4 * cos(susp->hz_x1_sample) / susp->c3p1;
        susp->c1 = (susp->normalization == 0 ? susp->scale1 :
          (susp->normalization == 1 ? susp->omc3 * sqrt(1.0 - susp->c2 * susp->c2 / susp->c3t4) :
              sqrt(susp->c3p1 * susp->c3p1 - susp->c2 * susp->c2) * susp->omc3 / susp->c3p1)) * susp->scale1;
    }

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
        scale1_reg = susp->scale1;
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        hz_pHaSe_ReG = susp->hz_pHaSe;
        hz_x1_sample_reg = susp->hz_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            if (hz_pHaSe_ReG >= 1.0) {
/* fixup-depends hz */
                /* pick up next sample as hz_x1_sample: */
                susp->hz_ptr++;
                susp_took(hz_cnt, 1);
                hz_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(hz, hz_ptr, hz_cnt, hz_x1_sample_reg);
                hz_x1_sample_reg = susp_current_sample(hz, hz_ptr);
                c2_reg = c3t4_reg * cos(hz_x1_sample_reg) / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? scale1_reg :
          (normalization_reg == 1 ? omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
              sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * omc3_reg / c3p1_reg)) * scale1_reg;
            }
            { double y0 = c1_reg * *s1_ptr_reg++ + c2_reg * y1_reg - c3co_reg * y2_reg;
              *out_ptr_reg++ = (sample_type) y0;
              y2_reg = y1_reg; y1_reg = y0; };
            hz_pHaSe_ReG += hz_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        susp->hz_pHaSe = hz_pHaSe_ReG;
        susp->hz_x1_sample = hz_x1_sample_reg;
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
} /* resonvc_ni_fetch */


void resonvc_nr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type hz_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double scale1_reg;
    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double c2_reg;
    register double c1_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "resonvc_nr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->hz_pHaSe = 1.0;
    }

    susp_check_term_samples(hz, hz_ptr, hz_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next hz_x1_sample when phase goes past 1.0; */
        /* use hz_n (computed below) to avoid roundoff errors: */
        if (susp->hz_n <= 0) {
            susp_check_term_samples(hz, hz_ptr, hz_cnt);
            susp->hz_x1_sample = susp_fetch_sample(hz, hz_ptr, hz_cnt);
            susp->hz_pHaSe -= 1.0;
            /* hz_n gets number of samples before phase exceeds 1.0: */
            susp->hz_n = (int64_t) ((1.0 - susp->hz_pHaSe) *
                                        susp->output_per_hz);
            susp->c2 = susp->c3t4 * cos(susp->hz_x1_sample) / susp->c3p1;
            susp->c1 = (susp->normalization == 0 ? susp->scale1 :
          (susp->normalization == 1 ? susp->omc3 * sqrt(1.0 - susp->c2 * susp->c2 / susp->c3t4) :
              sqrt(susp->c3p1 * susp->c3p1 - susp->c2 * susp->c2) * susp->omc3 / susp->c3p1)) * susp->scale1;
        }
        togo = (int) min(togo, susp->hz_n);
        hz_val = susp->hz_x1_sample;
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
        scale1_reg = susp->scale1;
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            { double y0 = c1_reg * *s1_ptr_reg++ + c2_reg * y1_reg - c3co_reg * y2_reg;
              *out_ptr_reg++ = (sample_type) y0;
              y2_reg = y1_reg; y1_reg = y0; };
        } while (--n); /* inner loop */

        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->hz_pHaSe += togo * susp->hz_pHaSe_iNcR;
        susp->hz_n -= togo;
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
} /* resonvc_nr_fetch */


void resonvc_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from s1 up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr)) >=
           susp->s1->current)
        susp_get_samples(s1, s1_ptr, s1_cnt);
    /* fetch samples from hz up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->hz->t0) * susp->hz->sr)) >=
           susp->hz->current)
        susp_get_samples(hz, hz_ptr, hz_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr -
         (susp->s1->current - susp->s1_cnt));
    susp->s1_ptr += n;
    susp_took(s1_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->hz->t0) * susp->hz->sr -
         (susp->hz->current - susp->hz_cnt));
    susp->hz_ptr += n;
    susp_took(hz_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void resonvc_mark(snd_susp_type a_susp)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    sound_xlmark(susp->s1);
    sound_xlmark(susp->hz);
}


void resonvc_free(snd_susp_type a_susp)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    sound_unref(susp->s1);
    sound_unref(susp->hz);
    ffree_generic(susp, sizeof(resonvc_susp_node), "resonvc_free");
}


void resonvc_print_tree(snd_susp_type a_susp, int n)
{
    resonvc_susp_type susp = (resonvc_susp_type) a_susp;
    indent(n);
    stdputstr("s1:");
    sound_print_tree_1(susp->s1, n);

    indent(n);
    stdputstr("hz:");
    sound_print_tree_1(susp->hz, n);
}


sound_type snd_make_resonvc(sound_type s1, sound_type hz, double bw, int normalization)
{
    register resonvc_susp_type susp;
    rate_type sr = s1->sr;
    time_type t0 = max(s1->t0, hz->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, resonvc_susp_node, "snd_make_resonvc");
    susp->scale1 = s1->scale;
    susp->c3co = exp(bw * -PI2 / s1->sr);
    susp->c3p1 = susp->c3co + 1.0;
    susp->c3t4 = susp->c3co * 4.0;
    susp->omc3 = 1.0 - susp->c3co;
    susp->c2 = 0.0;
    susp->c1 = 0.0;
    susp->normalization = normalization;
    susp->y1 = 0.0;
    susp->y2 = 0.0;
    hz->scale = (sample_type) (hz->scale * (PI2 / s1->sr));

    /* make sure no sample rate is too high */
    if (hz->sr > sr) {
        sound_unref(hz);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s1, sr);
    interp_desc = (interp_desc << 2) + interp_style(hz, sr);
    switch (interp_desc) {
      case INTERP_sn: /* handled below */
      case INTERP_ss: /* handled below */
      case INTERP_nn: /* handled below */
      case INTERP_ns: susp->susp.fetch = resonvc_ns_fetch; break;
      case INTERP_si: /* handled below */
      case INTERP_ni: susp->susp.fetch = resonvc_ni_fetch; break;
      case INTERP_sr: /* handled below */
      case INTERP_nr: susp->susp.fetch = resonvc_nr_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s1->t0) sound_prepend_zeros(s1, t0);
    if (t0 < hz->t0) sound_prepend_zeros(hz, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s1->t0, min(hz->t0, t0));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = resonvc_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = resonvc_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = resonvc_mark;
    susp->susp.print_tree = resonvc_print_tree;
    susp->susp.name = "resonvc";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s1);
    susp->started = false;
    susp->susp.current = 0;
    susp->s1 = s1;
    susp->s1_cnt = 0;
    susp->hz = hz;
    susp->hz_cnt = 0;
    susp->hz_pHaSe = 0.0;
    susp->hz_pHaSe_iNcR = hz->sr / sr;
    susp->hz_n = 0;
    susp->output_per_hz = sr / hz->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_resonvc(sound_type s1, sound_type hz, double bw, int normalization)
{
    sound_type s1_copy = sound_copy(s1);
    sound_type hz_copy = sound_copy(hz);
    return snd_make_resonvc(s1_copy, hz_copy, bw, normalization);
}
