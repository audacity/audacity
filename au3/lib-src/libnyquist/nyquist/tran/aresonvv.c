#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "aresonvv.h"

void aresonvv_free(snd_susp_type a_susp);


typedef struct aresonvv_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s1;
    int s1_cnt;
    sample_block_values_type s1_ptr;
    sound_type hz1;
    int hz1_cnt;
    sample_block_values_type hz1_ptr;

    /* support for interpolation of hz1 */
    sample_type hz1_x1_sample;
    double hz1_pHaSe;
    double hz1_pHaSe_iNcR;

    /* support for ramp between samples of hz1 */
    double output_per_hz1;
    int64_t hz1_n;
    sound_type bw;
    int bw_cnt;
    sample_block_values_type bw_ptr;

    /* support for interpolation of bw */
    sample_type bw_x1_sample;
    double bw_pHaSe;
    double bw_pHaSe_iNcR;

    /* support for ramp between samples of bw */
    double output_per_bw;
    int64_t bw_n;

    double scale1;
    double c3co;
    double c3p1;
    double c3t4;
    double omc3;
    double coshz;
    double c2;
    double c1;
    boolean recompute;
    int normalization;
    double y1;
    double y2;
} aresonvv_susp_node, *aresonvv_susp_type;


void aresonvv_nss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_type bw_scale_reg = susp->bw->scale;
    register sample_block_values_type bw_ptr_reg;
    register sample_type hz1_scale_reg = susp->hz1->scale;
    register sample_block_values_type hz1_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nss_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the hz1 input sample block: */
        susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
        togo = min(togo, susp->hz1_cnt);

        /* don't run past the bw input sample block: */
        susp_check_term_samples(bw, bw_ptr, bw_cnt);
        togo = min(togo, susp->bw_cnt);

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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        bw_ptr_reg = susp->bw_ptr;
        hz1_ptr_reg = susp->hz1_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            coshz_reg = cos((hz1_scale_reg * *hz1_ptr_reg++));
            recompute_reg = true;
            c3co_reg = exp((bw_scale_reg * *bw_ptr_reg++));
            c3p1_reg = c3co_reg + 1.0;
            c3t4_reg = c3co_reg * 4.0;
            omc3_reg = 1.0 - c3co_reg;
            recompute_reg = true;
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
        } while (--n); /* inner loop */

        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using bw_ptr_reg is a bad idea on RS/6000: */
        susp->bw_ptr += togo;
        /* using hz1_ptr_reg is a bad idea on RS/6000: */
        susp->hz1_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(hz1_cnt, togo);
        susp_took(bw_cnt, togo);
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
} /* aresonvv_nss_fetch */


void aresonvv_nsi_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register double bw_pHaSe_iNcR_rEg = susp->bw_pHaSe_iNcR;
    register double bw_pHaSe_ReG;
    register sample_type bw_x1_sample_reg;
    register sample_type hz1_scale_reg = susp->hz1->scale;
    register sample_block_values_type hz1_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nsi_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(bw, bw_ptr, bw_cnt);
        susp->bw_x1_sample = susp_fetch_sample(bw, bw_ptr, bw_cnt);
        susp->c3co = exp(susp->bw_x1_sample);
        susp->c3p1 = susp->c3co + 1.0;
        susp->c3t4 = susp->c3co * 4.0;
        susp->omc3 = 1.0 - susp->c3co;
        susp->recompute = true;
    }

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the hz1 input sample block: */
        susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
        togo = min(togo, susp->hz1_cnt);

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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        bw_pHaSe_ReG = susp->bw_pHaSe;
        bw_x1_sample_reg = susp->bw_x1_sample;
        hz1_ptr_reg = susp->hz1_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            if (bw_pHaSe_ReG >= 1.0) {
/* fixup-depends bw */
                /* pick up next sample as bw_x1_sample: */
                susp->bw_ptr++;
                susp_took(bw_cnt, 1);
                bw_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(bw, bw_ptr, bw_cnt, bw_x1_sample_reg);
                bw_x1_sample_reg = susp_current_sample(bw, bw_ptr);
                c3co_reg = exp(bw_x1_sample_reg);
                c3p1_reg = c3co_reg + 1.0;
                c3t4_reg = c3co_reg * 4.0;
                omc3_reg = 1.0 - c3co_reg;
                recompute_reg = true;
            }
            coshz_reg = cos((hz1_scale_reg * *hz1_ptr_reg++));
            recompute_reg = true;
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
            bw_pHaSe_ReG += bw_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        susp->bw_pHaSe = bw_pHaSe_ReG;
        susp->bw_x1_sample = bw_x1_sample_reg;
        /* using hz1_ptr_reg is a bad idea on RS/6000: */
        susp->hz1_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(hz1_cnt, togo);
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
} /* aresonvv_nsi_fetch */


void aresonvv_nsr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type bw_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_type hz1_scale_reg = susp->hz1->scale;
    register sample_block_values_type hz1_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nsr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->bw_pHaSe = 1.0;
    }

    susp_check_term_samples(bw, bw_ptr, bw_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the hz1 input sample block: */
        susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
        togo = min(togo, susp->hz1_cnt);

        /* grab next bw_x1_sample when phase goes past 1.0; */
        /* use bw_n (computed below) to avoid roundoff errors: */
        if (susp->bw_n <= 0) {
            susp_check_term_samples(bw, bw_ptr, bw_cnt);
            susp->bw_x1_sample = susp_fetch_sample(bw, bw_ptr, bw_cnt);
            susp->bw_pHaSe -= 1.0;
            /* bw_n gets number of samples before phase exceeds 1.0: */
            susp->bw_n = (int64_t) ((1.0 - susp->bw_pHaSe) *
                                        susp->output_per_bw);
            susp->c3co = exp(susp->bw_x1_sample);
            susp->c3p1 = susp->c3co + 1.0;
            susp->c3t4 = susp->c3co * 4.0;
            susp->omc3 = 1.0 - susp->c3co;
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->bw_n);
        bw_val = susp->bw_x1_sample;
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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        hz1_ptr_reg = susp->hz1_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            coshz_reg = cos((hz1_scale_reg * *hz1_ptr_reg++));
            recompute_reg = true;
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
        } while (--n); /* inner loop */

        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using hz1_ptr_reg is a bad idea on RS/6000: */
        susp->hz1_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(hz1_cnt, togo);
        susp->bw_pHaSe += togo * susp->bw_pHaSe_iNcR;
        susp->bw_n -= togo;
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
} /* aresonvv_nsr_fetch */


void aresonvv_nis_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_type bw_scale_reg = susp->bw->scale;
    register sample_block_values_type bw_ptr_reg;
    register double hz1_pHaSe_iNcR_rEg = susp->hz1_pHaSe_iNcR;
    register double hz1_pHaSe_ReG;
    register sample_type hz1_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nis_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
        susp->hz1_x1_sample = susp_fetch_sample(hz1, hz1_ptr, hz1_cnt);
        susp->coshz = cos(susp->hz1_x1_sample);
        susp->recompute = true;
    }

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* don't run past the bw input sample block: */
        susp_check_term_samples(bw, bw_ptr, bw_cnt);
        togo = min(togo, susp->bw_cnt);

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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        bw_ptr_reg = susp->bw_ptr;
        hz1_pHaSe_ReG = susp->hz1_pHaSe;
        hz1_x1_sample_reg = susp->hz1_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            if (hz1_pHaSe_ReG >= 1.0) {
/* fixup-depends hz1 */
                /* pick up next sample as hz1_x1_sample: */
                susp->hz1_ptr++;
                susp_took(hz1_cnt, 1);
                hz1_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(hz1, hz1_ptr, hz1_cnt, hz1_x1_sample_reg);
                hz1_x1_sample_reg = susp_current_sample(hz1, hz1_ptr);
                coshz_reg = cos(hz1_x1_sample_reg);
                recompute_reg = true;
            }
            c3co_reg = exp((bw_scale_reg * *bw_ptr_reg++));
            c3p1_reg = c3co_reg + 1.0;
            c3t4_reg = c3co_reg * 4.0;
            omc3_reg = 1.0 - c3co_reg;
            recompute_reg = true;
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
            hz1_pHaSe_ReG += hz1_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using bw_ptr_reg is a bad idea on RS/6000: */
        susp->bw_ptr += togo;
        susp->hz1_pHaSe = hz1_pHaSe_ReG;
        susp->hz1_x1_sample = hz1_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(bw_cnt, togo);
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
} /* aresonvv_nis_fetch */


void aresonvv_nii_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register double bw_pHaSe_iNcR_rEg = susp->bw_pHaSe_iNcR;
    register double bw_pHaSe_ReG;
    register sample_type bw_x1_sample_reg;
    register double hz1_pHaSe_iNcR_rEg = susp->hz1_pHaSe_iNcR;
    register double hz1_pHaSe_ReG;
    register sample_type hz1_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nii_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
        susp->hz1_x1_sample = susp_fetch_sample(hz1, hz1_ptr, hz1_cnt);
        susp->coshz = cos(susp->hz1_x1_sample);
        susp->recompute = true;
        susp_check_term_samples(bw, bw_ptr, bw_cnt);
        susp->bw_x1_sample = susp_fetch_sample(bw, bw_ptr, bw_cnt);
        susp->c3co = exp(susp->bw_x1_sample);
        susp->c3p1 = susp->c3co + 1.0;
        susp->c3t4 = susp->c3co * 4.0;
        susp->omc3 = 1.0 - susp->c3co;
        susp->recompute = true;
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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        bw_pHaSe_ReG = susp->bw_pHaSe;
        bw_x1_sample_reg = susp->bw_x1_sample;
        hz1_pHaSe_ReG = susp->hz1_pHaSe;
        hz1_x1_sample_reg = susp->hz1_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            if (hz1_pHaSe_ReG >= 1.0) {
/* fixup-depends hz1 */
                /* pick up next sample as hz1_x1_sample: */
                susp->hz1_ptr++;
                susp_took(hz1_cnt, 1);
                hz1_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(hz1, hz1_ptr, hz1_cnt, hz1_x1_sample_reg);
                hz1_x1_sample_reg = susp_current_sample(hz1, hz1_ptr);
                coshz_reg = cos(hz1_x1_sample_reg);
                recompute_reg = true;
            }
            if (bw_pHaSe_ReG >= 1.0) {
/* fixup-depends bw */
                /* pick up next sample as bw_x1_sample: */
                susp->bw_ptr++;
                susp_took(bw_cnt, 1);
                bw_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(bw, bw_ptr, bw_cnt, bw_x1_sample_reg);
                bw_x1_sample_reg = susp_current_sample(bw, bw_ptr);
                c3co_reg = exp(bw_x1_sample_reg);
                c3p1_reg = c3co_reg + 1.0;
                c3t4_reg = c3co_reg * 4.0;
                omc3_reg = 1.0 - c3co_reg;
                recompute_reg = true;
            }
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
            hz1_pHaSe_ReG += hz1_pHaSe_iNcR_rEg;
            bw_pHaSe_ReG += bw_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        susp->bw_pHaSe = bw_pHaSe_ReG;
        susp->bw_x1_sample = bw_x1_sample_reg;
        susp->hz1_pHaSe = hz1_pHaSe_ReG;
        susp->hz1_x1_sample = hz1_x1_sample_reg;
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
} /* aresonvv_nii_fetch */


void aresonvv_nir_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type bw_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register double hz1_pHaSe_iNcR_rEg = susp->hz1_pHaSe_iNcR;
    register double hz1_pHaSe_ReG;
    register sample_type hz1_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nir_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
        susp->hz1_x1_sample = susp_fetch_sample(hz1, hz1_ptr, hz1_cnt);
        susp->coshz = cos(susp->hz1_x1_sample);
        susp->recompute = true;
        susp->bw_pHaSe = 1.0;
    }

    susp_check_term_samples(bw, bw_ptr, bw_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next bw_x1_sample when phase goes past 1.0; */
        /* use bw_n (computed below) to avoid roundoff errors: */
        if (susp->bw_n <= 0) {
            susp_check_term_samples(bw, bw_ptr, bw_cnt);
            susp->bw_x1_sample = susp_fetch_sample(bw, bw_ptr, bw_cnt);
            susp->bw_pHaSe -= 1.0;
            /* bw_n gets number of samples before phase exceeds 1.0: */
            susp->bw_n = (int64_t) ((1.0 - susp->bw_pHaSe) *
                                        susp->output_per_bw);
            susp->c3co = exp(susp->bw_x1_sample);
            susp->c3p1 = susp->c3co + 1.0;
            susp->c3t4 = susp->c3co * 4.0;
            susp->omc3 = 1.0 - susp->c3co;
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->bw_n);
        bw_val = susp->bw_x1_sample;
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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        hz1_pHaSe_ReG = susp->hz1_pHaSe;
        hz1_x1_sample_reg = susp->hz1_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            if (hz1_pHaSe_ReG >= 1.0) {
/* fixup-depends hz1 */
                /* pick up next sample as hz1_x1_sample: */
                susp->hz1_ptr++;
                susp_took(hz1_cnt, 1);
                hz1_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(hz1, hz1_ptr, hz1_cnt, hz1_x1_sample_reg);
                hz1_x1_sample_reg = susp_current_sample(hz1, hz1_ptr);
                coshz_reg = cos(hz1_x1_sample_reg);
                recompute_reg = true;
            }
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
            hz1_pHaSe_ReG += hz1_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        susp->hz1_pHaSe = hz1_pHaSe_ReG;
        susp->hz1_x1_sample = hz1_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->bw_pHaSe += togo * susp->bw_pHaSe_iNcR;
        susp->bw_n -= togo;
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
} /* aresonvv_nir_fetch */


void aresonvv_nrs_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type hz1_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_type bw_scale_reg = susp->bw->scale;
    register sample_block_values_type bw_ptr_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nrs_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->hz1_pHaSe = 1.0;
    }

    susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next hz1_x1_sample when phase goes past 1.0; */
        /* use hz1_n (computed below) to avoid roundoff errors: */
        if (susp->hz1_n <= 0) {
            susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
            susp->hz1_x1_sample = susp_fetch_sample(hz1, hz1_ptr, hz1_cnt);
            susp->hz1_pHaSe -= 1.0;
            /* hz1_n gets number of samples before phase exceeds 1.0: */
            susp->hz1_n = (int64_t) ((1.0 - susp->hz1_pHaSe) *
                                        susp->output_per_hz1);
            susp->coshz = cos(susp->hz1_x1_sample);
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->hz1_n);
        hz1_val = susp->hz1_x1_sample;
        /* don't run past the bw input sample block: */
        susp_check_term_samples(bw, bw_ptr, bw_cnt);
        togo = min(togo, susp->bw_cnt);

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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        bw_ptr_reg = susp->bw_ptr;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            c3co_reg = exp((bw_scale_reg * *bw_ptr_reg++));
            c3p1_reg = c3co_reg + 1.0;
            c3t4_reg = c3co_reg * 4.0;
            omc3_reg = 1.0 - c3co_reg;
            recompute_reg = true;
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
        } while (--n); /* inner loop */

        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using bw_ptr_reg is a bad idea on RS/6000: */
        susp->bw_ptr += togo;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->hz1_pHaSe += togo * susp->hz1_pHaSe_iNcR;
        susp->hz1_n -= togo;
        susp_took(bw_cnt, togo);
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
} /* aresonvv_nrs_fetch */


void aresonvv_nri_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type hz1_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c3p1_reg;
    register double c3t4_reg;
    register double omc3_reg;
    register double coshz_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register int normalization_reg;
    register double y1_reg;
    register double y2_reg;
    register double bw_pHaSe_iNcR_rEg = susp->bw_pHaSe_iNcR;
    register double bw_pHaSe_ReG;
    register sample_type bw_x1_sample_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nri_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->hz1_pHaSe = 1.0;
        susp_check_term_samples(bw, bw_ptr, bw_cnt);
        susp->bw_x1_sample = susp_fetch_sample(bw, bw_ptr, bw_cnt);
        susp->c3co = exp(susp->bw_x1_sample);
        susp->c3p1 = susp->c3co + 1.0;
        susp->c3t4 = susp->c3co * 4.0;
        susp->omc3 = 1.0 - susp->c3co;
        susp->recompute = true;
    }

    susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next hz1_x1_sample when phase goes past 1.0; */
        /* use hz1_n (computed below) to avoid roundoff errors: */
        if (susp->hz1_n <= 0) {
            susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
            susp->hz1_x1_sample = susp_fetch_sample(hz1, hz1_ptr, hz1_cnt);
            susp->hz1_pHaSe -= 1.0;
            /* hz1_n gets number of samples before phase exceeds 1.0: */
            susp->hz1_n = (int64_t) ((1.0 - susp->hz1_pHaSe) *
                                        susp->output_per_hz1);
            susp->coshz = cos(susp->hz1_x1_sample);
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->hz1_n);
        hz1_val = susp->hz1_x1_sample;
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
        c3co_reg = susp->c3co;
        c3p1_reg = susp->c3p1;
        c3t4_reg = susp->c3t4;
        omc3_reg = susp->omc3;
        coshz_reg = susp->coshz;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        normalization_reg = susp->normalization;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        bw_pHaSe_ReG = susp->bw_pHaSe;
        bw_x1_sample_reg = susp->bw_x1_sample;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            if (bw_pHaSe_ReG >= 1.0) {
/* fixup-depends bw */
                /* pick up next sample as bw_x1_sample: */
                susp->bw_ptr++;
                susp_took(bw_cnt, 1);
                bw_pHaSe_ReG -= 1.0;
                susp_check_term_samples_break(bw, bw_ptr, bw_cnt, bw_x1_sample_reg);
                bw_x1_sample_reg = susp_current_sample(bw, bw_ptr);
                c3co_reg = exp(bw_x1_sample_reg);
                c3p1_reg = c3co_reg + 1.0;
                c3t4_reg = c3co_reg * 4.0;
                omc3_reg = 1.0 - c3co_reg;
                recompute_reg = true;
            }
            if (recompute_reg) {
                recompute_reg = false;
                c2_reg = c3t4_reg * coshz_reg / c3p1_reg;
                c1_reg = (normalization_reg == 0 ? 0.0 :
                      (normalization_reg == 1 ? 
                       1.0 - omc3_reg * sqrt(1.0 - c2_reg * c2_reg / c3t4_reg) :
                       1.0 - sqrt(c3p1_reg * c3p1_reg - c2_reg * c2_reg) * 
                       omc3_reg / c3p1_reg));
            }
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
            bw_pHaSe_ReG += bw_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        susp->bw_pHaSe = bw_pHaSe_ReG;
        susp->bw_x1_sample = bw_x1_sample_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->hz1_pHaSe += togo * susp->hz1_pHaSe_iNcR;
        susp->hz1_n -= togo;
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
} /* aresonvv_nri_fetch */


void aresonvv_nrr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type hz1_val;
    sample_type bw_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double c3co_reg;
    register double c2_reg;
    register double c1_reg;
    register boolean recompute_reg;
    register double y1_reg;
    register double y2_reg;
    register sample_block_values_type s1_ptr_reg;
    falloc_sample_block(out, "aresonvv_nrr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->hz1_pHaSe = 1.0;
        susp->bw_pHaSe = 1.0;
    }

    susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);

    susp_check_term_samples(bw, bw_ptr, bw_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the s1 input sample block: */
        susp_check_term_log_samples(s1, s1_ptr, s1_cnt);
        togo = min(togo, susp->s1_cnt);

        /* grab next hz1_x1_sample when phase goes past 1.0; */
        /* use hz1_n (computed below) to avoid roundoff errors: */
        if (susp->hz1_n <= 0) {
            susp_check_term_samples(hz1, hz1_ptr, hz1_cnt);
            susp->hz1_x1_sample = susp_fetch_sample(hz1, hz1_ptr, hz1_cnt);
            susp->hz1_pHaSe -= 1.0;
            /* hz1_n gets number of samples before phase exceeds 1.0: */
            susp->hz1_n = (int64_t) ((1.0 - susp->hz1_pHaSe) *
                                        susp->output_per_hz1);
            susp->coshz = cos(susp->hz1_x1_sample);
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->hz1_n);
        hz1_val = susp->hz1_x1_sample;
        /* grab next bw_x1_sample when phase goes past 1.0; */
        /* use bw_n (computed below) to avoid roundoff errors: */
        if (susp->bw_n <= 0) {
            susp_check_term_samples(bw, bw_ptr, bw_cnt);
            susp->bw_x1_sample = susp_fetch_sample(bw, bw_ptr, bw_cnt);
            susp->bw_pHaSe -= 1.0;
            /* bw_n gets number of samples before phase exceeds 1.0: */
            susp->bw_n = (int64_t) ((1.0 - susp->bw_pHaSe) *
                                        susp->output_per_bw);
            susp->c3co = exp(susp->bw_x1_sample);
            susp->c3p1 = susp->c3co + 1.0;
            susp->c3t4 = susp->c3co * 4.0;
            susp->omc3 = 1.0 - susp->c3co;
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->bw_n);
        bw_val = susp->bw_x1_sample;
        if (susp->recompute) {
            susp->recompute = false;
            susp->c2 = susp->c3t4 * susp->coshz / susp->c3p1;
            susp->c1 = (susp->normalization == 0 ? 0.0 :
                  (susp->normalization == 1 ? 
                   1.0 - susp->omc3 * sqrt(1.0 - susp->c2 * susp->c2 / susp->c3t4) :
                   1.0 - sqrt(susp->c3p1 * susp->c3p1 - susp->c2 * susp->c2) * 
                   susp->omc3 / susp->c3p1));
        }
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
        c3co_reg = susp->c3co;
        c2_reg = susp->c2;
        c1_reg = susp->c1;
        recompute_reg = susp->recompute;
        y1_reg = susp->y1;
        y2_reg = susp->y2;
        s1_ptr_reg = susp->s1_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double y0, current;
            current = *s1_ptr_reg++;
            y0 = c1_reg * current + c2_reg * y1_reg - c3co_reg * y2_reg;
            *out_ptr_reg++ = (sample_type) y0;
            y2_reg = y1_reg; y1_reg = y0 - current;
        } while (--n); /* inner loop */

        susp->recompute = recompute_reg;
        susp->y1 = y1_reg;
        susp->y2 = y2_reg;
        /* using s1_ptr_reg is a bad idea on RS/6000: */
        susp->s1_ptr += togo;
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp->hz1_pHaSe += togo * susp->hz1_pHaSe_iNcR;
        susp->hz1_n -= togo;
        susp->bw_pHaSe += togo * susp->bw_pHaSe_iNcR;
        susp->bw_n -= togo;
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
} /* aresonvv_nrr_fetch */


void aresonvv_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from s1 up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr)) >=
           susp->s1->current)
        susp_get_samples(s1, s1_ptr, s1_cnt);
    /* fetch samples from hz1 up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->hz1->t0) * susp->hz1->sr)) >=
           susp->hz1->current)
        susp_get_samples(hz1, hz1_ptr, hz1_cnt);
    /* fetch samples from bw up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->bw->t0) * susp->bw->sr)) >=
           susp->bw->current)
        susp_get_samples(bw, bw_ptr, bw_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->s1->t0) * susp->s1->sr -
         (susp->s1->current - susp->s1_cnt));
    susp->s1_ptr += n;
    susp_took(s1_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->hz1->t0) * susp->hz1->sr -
         (susp->hz1->current - susp->hz1_cnt));
    susp->hz1_ptr += n;
    susp_took(hz1_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->bw->t0) * susp->bw->sr -
         (susp->bw->current - susp->bw_cnt));
    susp->bw_ptr += n;
    susp_took(bw_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void aresonvv_mark(snd_susp_type a_susp)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    sound_xlmark(susp->s1);
    sound_xlmark(susp->hz1);
    sound_xlmark(susp->bw);
}


void aresonvv_free(snd_susp_type a_susp)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    sound_unref(susp->s1);
    sound_unref(susp->hz1);
    sound_unref(susp->bw);
    ffree_generic(susp, sizeof(aresonvv_susp_node), "aresonvv_free");
}


void aresonvv_print_tree(snd_susp_type a_susp, int n)
{
    aresonvv_susp_type susp = (aresonvv_susp_type) a_susp;
    indent(n);
    stdputstr("s1:");
    sound_print_tree_1(susp->s1, n);

    indent(n);
    stdputstr("hz1:");
    sound_print_tree_1(susp->hz1, n);

    indent(n);
    stdputstr("bw:");
    sound_print_tree_1(susp->bw, n);
}


sound_type snd_make_aresonvv(sound_type s1, sound_type hz1, sound_type bw, int normalization)
{
    register aresonvv_susp_type susp;
    rate_type sr = s1->sr;
    time_type t0 = max(max(s1->t0, hz1->t0), bw->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    /* combine scale factors of linear inputs (S1) */
    scale_factor *= s1->scale;
    s1->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (s1->sr < sr) { s1->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, aresonvv_susp_node, "snd_make_aresonvv");
    susp->scale1 = s1->scale;
    susp->c3co = 0.0;
    susp->c3p1 = 0.0;
    susp->c3t4 = 0.0;
    susp->omc3 = 0.0;
    susp->coshz = 0.0;
    susp->c2 = 0.0;
    susp->c1 = 0.0;
    susp->recompute = false;
    susp->normalization = normalization;
    susp->y1 = 0.0;
    susp->y2 = 0.0;
    hz1->scale = (sample_type) (hz1->scale * (PI2 / s1->sr));
    bw->scale = (sample_type) (bw->scale * (-PI2 / s1->sr));;

    /* make sure no sample rate is too high */
    if (hz1->sr > sr) {
        sound_unref(hz1);
        snd_badsr();
    }
    if (bw->sr > sr) {
        sound_unref(bw);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(s1, sr);
    interp_desc = (interp_desc << 2) + interp_style(hz1, sr);
    interp_desc = (interp_desc << 2) + interp_style(bw, sr);
    switch (interp_desc) {
      case INTERP_nnn: /* handled below */
      case INTERP_nns: /* handled below */
      case INTERP_nsn: /* handled below */
      case INTERP_nss: susp->susp.fetch = aresonvv_nss_fetch; break;
      case INTERP_nni: /* handled below */
      case INTERP_nsi: susp->susp.fetch = aresonvv_nsi_fetch; break;
      case INTERP_nnr: /* handled below */
      case INTERP_nsr: susp->susp.fetch = aresonvv_nsr_fetch; break;
      case INTERP_nin: /* handled below */
      case INTERP_nis: susp->susp.fetch = aresonvv_nis_fetch; break;
      case INTERP_nii: susp->susp.fetch = aresonvv_nii_fetch; break;
      case INTERP_nir: susp->susp.fetch = aresonvv_nir_fetch; break;
      case INTERP_nrn: /* handled below */
      case INTERP_nrs: susp->susp.fetch = aresonvv_nrs_fetch; break;
      case INTERP_nri: susp->susp.fetch = aresonvv_nri_fetch; break;
      case INTERP_nrr: susp->susp.fetch = aresonvv_nrr_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s1->t0) sound_prepend_zeros(s1, t0);
    if (t0 < hz1->t0) sound_prepend_zeros(hz1, t0);
    if (t0 < bw->t0) sound_prepend_zeros(bw, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s1->t0, min(hz1->t0, min(bw->t0, t0)));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = aresonvv_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = aresonvv_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = aresonvv_mark;
    susp->susp.print_tree = aresonvv_print_tree;
    susp->susp.name = "aresonvv";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s1);
    susp->started = false;
    susp->susp.current = 0;
    susp->s1 = s1;
    susp->s1_cnt = 0;
    susp->hz1 = hz1;
    susp->hz1_cnt = 0;
    susp->hz1_pHaSe = 0.0;
    susp->hz1_pHaSe_iNcR = hz1->sr / sr;
    susp->hz1_n = 0;
    susp->output_per_hz1 = sr / hz1->sr;
    susp->bw = bw;
    susp->bw_cnt = 0;
    susp->bw_pHaSe = 0.0;
    susp->bw_pHaSe_iNcR = bw->sr / sr;
    susp->bw_n = 0;
    susp->output_per_bw = sr / bw->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_aresonvv(sound_type s1, sound_type hz1, sound_type bw, int normalization)
{
    sound_type s1_copy = sound_copy(s1);
    sound_type hz1_copy = sound_copy(hz1);
    sound_type bw_copy = sound_copy(bw);
    return snd_make_aresonvv(s1_copy, hz1_copy, bw_copy, normalization);
}
