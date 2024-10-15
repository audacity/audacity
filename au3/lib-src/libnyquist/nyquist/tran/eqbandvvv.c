#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "eqbandvvv.h"

void eqbandvvv_free(snd_susp_type a_susp);


typedef struct eqbandvvv_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type input;
    int input_cnt;
    sample_block_values_type input_ptr;
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
    sound_type gain;
    int gain_cnt;
    sample_block_values_type gain_ptr;

    /* support for interpolation of gain */
    sample_type gain_x1_sample;
    double gain_pHaSe;
    double gain_pHaSe_iNcR;

    /* support for ramp between samples of gain */
    double output_per_gain;
    int64_t gain_n;
    sound_type width;
    int width_cnt;
    sample_block_values_type width_ptr;

    /* support for interpolation of width */
    sample_type width_x1_sample;
    double width_pHaSe;
    double width_pHaSe_iNcR;

    /* support for ramp between samples of width */
    double output_per_width;
    int64_t width_n;

    double inp_scale;
    double w1;
    double sw;
    double cw;
    double J;
    double gg;
    double b0;
    double b1;
    double b2;
    double a0;
    double a1;
    double a2;
    double z1;
    double z2;
    boolean recompute;
    double inp_period;
} eqbandvvv_susp_node, *eqbandvvv_susp_type;

#define log_of_2_over_2 0.3465735902799726547086


void eqbandvvv_nsss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double w1_reg;
    register double sw_reg;
    register double cw_reg;
    register double J_reg;
    register double gg_reg;
    register double b0_reg;
    register double b1_reg;
    register double b2_reg;
    register double a0_reg;
    register double a1_reg;
    register double a2_reg;
    register double z1_reg;
    register double z2_reg;
    register boolean recompute_reg;
    register double inp_period_reg;
    register sample_type width_scale_reg = susp->width->scale;
    register sample_block_values_type width_ptr_reg;
    register sample_type gain_scale_reg = susp->gain->scale;
    register sample_block_values_type gain_ptr_reg;
    register sample_type hz_scale_reg = susp->hz->scale;
    register sample_block_values_type hz_ptr_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "eqbandvvv_nsss_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_log_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

        /* don't run past the hz input sample block: */
        susp_check_term_log_samples(hz, hz_ptr, hz_cnt);
        togo = min(togo, susp->hz_cnt);

        /* don't run past the gain input sample block: */
        susp_check_term_log_samples(gain, gain_ptr, gain_cnt);
        togo = min(togo, susp->gain_cnt);

        /* don't run past the width input sample block: */
        susp_check_term_log_samples(width, width_ptr, width_cnt);
        togo = min(togo, susp->width_cnt);

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
        w1_reg = susp->w1;
        sw_reg = susp->sw;
        cw_reg = susp->cw;
        J_reg = susp->J;
        gg_reg = susp->gg;
        b0_reg = susp->b0;
        b1_reg = susp->b1;
        b2_reg = susp->b2;
        a0_reg = susp->a0;
        a1_reg = susp->a1;
        a2_reg = susp->a2;
        z1_reg = susp->z1;
        z2_reg = susp->z2;
        recompute_reg = susp->recompute;
        inp_period_reg = susp->inp_period;
        width_ptr_reg = susp->width_ptr;
        gain_ptr_reg = susp->gain_ptr;
        hz_ptr_reg = susp->hz_ptr;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double z0;
            w1_reg = PI2 * (hz_scale_reg * *hz_ptr_reg++) * inp_period_reg;
            sw_reg = sin(w1_reg);
            cw_reg = cos(w1_reg);
            b1_reg = -2.0 * cw_reg;
            a1_reg = -b1_reg;
            J_reg = sqrt((gain_scale_reg * *gain_ptr_reg++));
            recompute_reg = true;
            recompute_reg = true;
            recompute_reg = true;
            if (recompute_reg) {
                /* a0_reg = 1.0 + gg_reg / J_reg; */
                double a_0_recip = J_reg / (J_reg + gg_reg);
                recompute_reg = false;
                gg_reg = sw_reg * sinh(log_of_2_over_2 * 
                               (width_scale_reg * *width_ptr_reg++) * w1_reg / sw_reg);
                b0_reg = (1.0 + gg_reg * J_reg) * a_0_recip;
                b1_reg *= a_0_recip;
                b2_reg = (1.0 - gg_reg * J_reg) * a_0_recip;
                a1_reg *= a_0_recip;
                a2_reg = (gg_reg / J_reg - 1.0) * a_0_recip;
            }
            z0 = *input_ptr_reg++ + a1_reg*z1_reg + a2_reg*z2_reg;
            *out_ptr_reg++ = (sample_type) (z0*b0_reg + z1_reg*b1_reg + z2_reg*b2_reg);
            z2_reg = z1_reg; z1_reg = z0;
        } while (--n); /* inner loop */

        susp->z1 = z1_reg;
        susp->z2 = z2_reg;
        susp->recompute = recompute_reg;
        /* using width_ptr_reg is a bad idea on RS/6000: */
        susp->width_ptr += togo;
        /* using gain_ptr_reg is a bad idea on RS/6000: */
        susp->gain_ptr += togo;
        /* using hz_ptr_reg is a bad idea on RS/6000: */
        susp->hz_ptr += togo;
        /* using input_ptr_reg is a bad idea on RS/6000: */
        susp->input_ptr += togo;
        out_ptr += togo;
        susp_took(input_cnt, togo);
        susp_took(hz_cnt, togo);
        susp_took(gain_cnt, togo);
        susp_took(width_cnt, togo);
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
} /* eqbandvvv_nsss_fetch */


void eqbandvvv_niii_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double w1_reg;
    register double sw_reg;
    register double cw_reg;
    register double J_reg;
    register double gg_reg;
    register double b0_reg;
    register double b1_reg;
    register double b2_reg;
    register double a0_reg;
    register double a1_reg;
    register double a2_reg;
    register double z1_reg;
    register double z2_reg;
    register boolean recompute_reg;
    register double inp_period_reg;
    register double width_pHaSe_iNcR_rEg = susp->width_pHaSe_iNcR;
    register double width_pHaSe_ReG;
    register sample_type width_x1_sample_reg;
    register double gain_pHaSe_iNcR_rEg = susp->gain_pHaSe_iNcR;
    register double gain_pHaSe_ReG;
    register sample_type gain_x1_sample_reg;
    register double hz_pHaSe_iNcR_rEg = susp->hz_pHaSe_iNcR;
    register double hz_pHaSe_ReG;
    register sample_type hz_x1_sample_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "eqbandvvv_niii_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(hz, hz_ptr, hz_cnt);
        susp->hz_x1_sample = susp_fetch_sample(hz, hz_ptr, hz_cnt);
        susp->w1 = PI2 * susp->hz_x1_sample * susp->inp_period;
        susp->sw = sin(susp->w1);
        susp->cw = cos(susp->w1);
        susp->b1 = -2.0 * susp->cw;
        susp->a1 = -susp->b1;
        susp->recompute = true;
        susp_check_term_log_samples(gain, gain_ptr, gain_cnt);
        susp->gain_x1_sample = susp_fetch_sample(gain, gain_ptr, gain_cnt);
        susp->J = sqrt(susp->gain_x1_sample);
        susp->recompute = true;
        susp_check_term_log_samples(width, width_ptr, width_cnt);
        susp->width_x1_sample = susp_fetch_sample(width, width_ptr, width_cnt);
        susp->recompute = true;
    }

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
        w1_reg = susp->w1;
        sw_reg = susp->sw;
        cw_reg = susp->cw;
        J_reg = susp->J;
        gg_reg = susp->gg;
        b0_reg = susp->b0;
        b1_reg = susp->b1;
        b2_reg = susp->b2;
        a0_reg = susp->a0;
        a1_reg = susp->a1;
        a2_reg = susp->a2;
        z1_reg = susp->z1;
        z2_reg = susp->z2;
        recompute_reg = susp->recompute;
        inp_period_reg = susp->inp_period;
        width_pHaSe_ReG = susp->width_pHaSe;
        width_x1_sample_reg = susp->width_x1_sample;
        gain_pHaSe_ReG = susp->gain_pHaSe;
        gain_x1_sample_reg = susp->gain_x1_sample;
        hz_pHaSe_ReG = susp->hz_pHaSe;
        hz_x1_sample_reg = susp->hz_x1_sample;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double z0;
            if (hz_pHaSe_ReG >= 1.0) {
/* fixup-depends hz */
                /* pick up next sample as hz_x1_sample: */
                susp->hz_ptr++;
                susp_took(hz_cnt, 1);
                hz_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(hz, hz_ptr, hz_cnt, hz_x1_sample_reg);
                hz_x1_sample_reg = susp_current_sample(hz, hz_ptr);
                w1_reg = PI2 * hz_x1_sample_reg * inp_period_reg;
                sw_reg = sin(w1_reg);
                cw_reg = cos(w1_reg);
                b1_reg = -2.0 * cw_reg;
                a1_reg = -b1_reg;
                recompute_reg = true;
            }
            if (gain_pHaSe_ReG >= 1.0) {
/* fixup-depends gain */
                /* pick up next sample as gain_x1_sample: */
                susp->gain_ptr++;
                susp_took(gain_cnt, 1);
                gain_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(gain, gain_ptr, gain_cnt, gain_x1_sample_reg);
                gain_x1_sample_reg = susp_current_sample(gain, gain_ptr);
                J_reg = sqrt(gain_x1_sample_reg);
                recompute_reg = true;
            }
            if (width_pHaSe_ReG >= 1.0) {
/* fixup-depends width */
                /* pick up next sample as width_x1_sample: */
                susp->width_ptr++;
                susp_took(width_cnt, 1);
                width_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(width, width_ptr, width_cnt, width_x1_sample_reg);
                width_x1_sample_reg = susp_current_sample(width, width_ptr);
                recompute_reg = true;
            }
            if (recompute_reg) {
                /* a0_reg = 1.0 + gg_reg / J_reg; */
                double a_0_recip = J_reg / (J_reg + gg_reg);
                recompute_reg = false;
                gg_reg = sw_reg * sinh(log_of_2_over_2 * 
                               width_x1_sample_reg * w1_reg / sw_reg);
                b0_reg = (1.0 + gg_reg * J_reg) * a_0_recip;
                b1_reg *= a_0_recip;
                b2_reg = (1.0 - gg_reg * J_reg) * a_0_recip;
                a1_reg *= a_0_recip;
                a2_reg = (gg_reg / J_reg - 1.0) * a_0_recip;
            }
            z0 = *input_ptr_reg++ + a1_reg*z1_reg + a2_reg*z2_reg;
            *out_ptr_reg++ = (sample_type) (z0*b0_reg + z1_reg*b1_reg + z2_reg*b2_reg);
            z2_reg = z1_reg; z1_reg = z0;
            hz_pHaSe_ReG += hz_pHaSe_iNcR_rEg;
            gain_pHaSe_ReG += gain_pHaSe_iNcR_rEg;
            width_pHaSe_ReG += width_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->z1 = z1_reg;
        susp->z2 = z2_reg;
        susp->recompute = recompute_reg;
        susp->width_pHaSe = width_pHaSe_ReG;
        susp->width_x1_sample = width_x1_sample_reg;
        susp->gain_pHaSe = gain_pHaSe_ReG;
        susp->gain_x1_sample = gain_x1_sample_reg;
        susp->hz_pHaSe = hz_pHaSe_ReG;
        susp->hz_x1_sample = hz_x1_sample_reg;
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
} /* eqbandvvv_niii_fetch */


void eqbandvvv_nrrr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type hz_val;
    sample_type gain_val;
    sample_type width_val;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double cw_reg;
    register double b0_reg;
    register double b1_reg;
    register double b2_reg;
    register double a1_reg;
    register double a2_reg;
    register double z1_reg;
    register double z2_reg;
    register double inp_period_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "eqbandvvv_nrrr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->hz_pHaSe = 1.0;
        susp->gain_pHaSe = 1.0;
        susp->width_pHaSe = 1.0;
    }

    susp_check_term_log_samples(hz, hz_ptr, hz_cnt);

    susp_check_term_log_samples(gain, gain_ptr, gain_cnt);

    susp_check_term_log_samples(width, width_ptr, width_cnt);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_log_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

        /* grab next hz_x1_sample when phase goes past 1.0; */
        /* use hz_n (computed below) to avoid roundoff errors: */
        if (susp->hz_n <= 0) {
            susp_check_term_log_samples(hz, hz_ptr, hz_cnt);
            susp->hz_x1_sample = susp_fetch_sample(hz, hz_ptr, hz_cnt);
            susp->hz_pHaSe -= 1.0;
            /* hz_n gets number of samples before phase exceeds 1.0: */
            susp->hz_n = (int64_t) ((1.0 - susp->hz_pHaSe) *
                                        susp->output_per_hz);
            susp->w1 = PI2 * susp->hz_x1_sample * susp->inp_period;
            susp->sw = sin(susp->w1);
            susp->cw = cos(susp->w1);
            susp->b1 = -2.0 * susp->cw;
            susp->a1 = -susp->b1;
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->hz_n);
        hz_val = susp->hz_x1_sample;
        /* grab next gain_x1_sample when phase goes past 1.0; */
        /* use gain_n (computed below) to avoid roundoff errors: */
        if (susp->gain_n <= 0) {
            susp_check_term_log_samples(gain, gain_ptr, gain_cnt);
            susp->gain_x1_sample = susp_fetch_sample(gain, gain_ptr, gain_cnt);
            susp->gain_pHaSe -= 1.0;
            /* gain_n gets number of samples before phase exceeds 1.0: */
            susp->gain_n = (int64_t) ((1.0 - susp->gain_pHaSe) *
                                        susp->output_per_gain);
            susp->J = sqrt(susp->gain_x1_sample);
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->gain_n);
        gain_val = susp->gain_x1_sample;
        /* grab next width_x1_sample when phase goes past 1.0; */
        /* use width_n (computed below) to avoid roundoff errors: */
        if (susp->width_n <= 0) {
            susp_check_term_log_samples(width, width_ptr, width_cnt);
            susp->width_x1_sample = susp_fetch_sample(width, width_ptr, width_cnt);
            susp->width_pHaSe -= 1.0;
            /* width_n gets number of samples before phase exceeds 1.0: */
            susp->width_n = (int64_t) ((1.0 - susp->width_pHaSe) *
                                        susp->output_per_width);
            susp->recompute = true;
        }
        togo = (int) min(togo, susp->width_n);
        width_val = susp->width_x1_sample;
        if (susp->recompute) {
            /* susp->a0 = 1.0 + susp->gg / susp->J; */
            double a_0_recip = susp->J / (susp->J + susp->gg);
            susp->recompute = false;
            susp->gg = susp->sw * sinh(log_of_2_over_2 * 
                           width_val * susp->w1 / susp->sw);
            susp->b0 = (1.0 + susp->gg * susp->J) * a_0_recip;
            susp->b1 *= a_0_recip;
            susp->b2 = (1.0 - susp->gg * susp->J) * a_0_recip;
            susp->a1 *= a_0_recip;
            susp->a2 = (susp->gg / susp->J - 1.0) * a_0_recip;
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
        cw_reg = susp->cw;
        b0_reg = susp->b0;
        b1_reg = susp->b1;
        b2_reg = susp->b2;
        a1_reg = susp->a1;
        a2_reg = susp->a2;
        z1_reg = susp->z1;
        z2_reg = susp->z2;
        inp_period_reg = susp->inp_period;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double z0;
            z0 = *input_ptr_reg++ + a1_reg*z1_reg + a2_reg*z2_reg;
            *out_ptr_reg++ = (sample_type) (z0*b0_reg + z1_reg*b1_reg + z2_reg*b2_reg);
            z2_reg = z1_reg; z1_reg = z0;
        } while (--n); /* inner loop */

        susp->z1 = z1_reg;
        susp->z2 = z2_reg;
        /* using input_ptr_reg is a bad idea on RS/6000: */
        susp->input_ptr += togo;
        out_ptr += togo;
        susp_took(input_cnt, togo);
        susp->hz_pHaSe += togo * susp->hz_pHaSe_iNcR;
        susp->hz_n -= togo;
        susp->gain_pHaSe += togo * susp->gain_pHaSe_iNcR;
        susp->gain_n -= togo;
        susp->width_pHaSe += togo * susp->width_pHaSe_iNcR;
        susp->width_n -= togo;
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
} /* eqbandvvv_nrrr_fetch */


void eqbandvvv_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from input up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->input->t0) * susp->input->sr)) >=
           susp->input->current)
        susp_get_samples(input, input_ptr, input_cnt);
    /* fetch samples from hz up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->hz->t0) * susp->hz->sr)) >=
           susp->hz->current)
        susp_get_samples(hz, hz_ptr, hz_cnt);
    /* fetch samples from gain up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->gain->t0) * susp->gain->sr)) >=
           susp->gain->current)
        susp_get_samples(gain, gain_ptr, gain_cnt);
    /* fetch samples from width up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->width->t0) * susp->width->sr)) >=
           susp->width->current)
        susp_get_samples(width, width_ptr, width_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->input->t0) * susp->input->sr -
         (susp->input->current - susp->input_cnt));
    susp->input_ptr += n;
    susp_took(input_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->hz->t0) * susp->hz->sr -
         (susp->hz->current - susp->hz_cnt));
    susp->hz_ptr += n;
    susp_took(hz_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->gain->t0) * susp->gain->sr -
         (susp->gain->current - susp->gain_cnt));
    susp->gain_ptr += n;
    susp_took(gain_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->width->t0) * susp->width->sr -
         (susp->width->current - susp->width_cnt));
    susp->width_ptr += n;
    susp_took(width_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void eqbandvvv_mark(snd_susp_type a_susp)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    sound_xlmark(susp->input);
    sound_xlmark(susp->hz);
    sound_xlmark(susp->gain);
    sound_xlmark(susp->width);
}


void eqbandvvv_free(snd_susp_type a_susp)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    sound_unref(susp->input);
    sound_unref(susp->hz);
    sound_unref(susp->gain);
    sound_unref(susp->width);
    ffree_generic(susp, sizeof(eqbandvvv_susp_node), "eqbandvvv_free");
}


void eqbandvvv_print_tree(snd_susp_type a_susp, int n)
{
    eqbandvvv_susp_type susp = (eqbandvvv_susp_type) a_susp;
    indent(n);
    stdputstr("input:");
    sound_print_tree_1(susp->input, n);

    indent(n);
    stdputstr("hz:");
    sound_print_tree_1(susp->hz, n);

    indent(n);
    stdputstr("gain:");
    sound_print_tree_1(susp->gain, n);

    indent(n);
    stdputstr("width:");
    sound_print_tree_1(susp->width, n);
}


sound_type snd_make_eqbandvvv(sound_type input, sound_type hz, sound_type gain, sound_type width)
{
    register eqbandvvv_susp_type susp;
    rate_type sr = input->sr;
    time_type t0 = min(min(min(input->t0, hz->t0), gain->t0), width->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    int64_t lsc;
    /* combine scale factors of linear inputs (INPUT) */
    scale_factor *= input->scale;
    input->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (input->sr < sr) { input->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, eqbandvvv_susp_node, "snd_make_eqbandvvv");
    susp->inp_scale = input->scale;
    susp->w1 = 0.0;
    susp->sw = 0.0;
    susp->cw = 0.0;
    susp->J = 0.0;
    susp->gg = 0.0;
    susp->b0 = 0.0;
    susp->b1 = 0.0;
    susp->b2 = 0.0;
    susp->a0 = 0.0;
    susp->a1 = 0.0;
    susp->a2 = 0.0;
    susp->z1 = 0.0;
    susp->z2 = 0.0;
    susp->recompute = false;
    susp->inp_period = 1.0 / input->sr;

    /* make sure no sample rate is too high */
    if (hz->sr > sr) {
        sound_unref(hz);
        snd_badsr();
    }
    if (gain->sr > sr) {
        sound_unref(gain);
        snd_badsr();
    }
    if (width->sr > sr) {
        sound_unref(width);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(input, sr);
    interp_desc = (interp_desc << 2) + interp_style(hz, sr);
    interp_desc = (interp_desc << 2) + interp_style(gain, sr);
    interp_desc = (interp_desc << 2) + interp_style(width, sr);
    switch (interp_desc) {
      case INTERP_nnnn: /* handled below */
      case INTERP_nnns: /* handled below */
      case INTERP_nnsn: /* handled below */
      case INTERP_nnss: /* handled below */
      case INTERP_nsnn: /* handled below */
      case INTERP_nsns: /* handled below */
      case INTERP_nssn: /* handled below */
      case INTERP_nsss: susp->susp.fetch = eqbandvvv_nsss_fetch; break;
      case INTERP_niii: susp->susp.fetch = eqbandvvv_niii_fetch; break;
      case INTERP_nrrr: susp->susp.fetch = eqbandvvv_nrrr_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < input->t0) sound_prepend_zeros(input, t0);
    if (t0 < hz->t0) sound_prepend_zeros(hz, t0);
    if (t0 < gain->t0) sound_prepend_zeros(gain, t0);
    if (t0 < width->t0) sound_prepend_zeros(width, t0);
    /* minimum start time over all inputs: */
    t0_min = min(input->t0, min(hz->t0, min(gain->t0, min(width->t0, t0))));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = eqbandvvv_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = eqbandvvv_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = eqbandvvv_mark;
    susp->susp.print_tree = eqbandvvv_print_tree;
    susp->susp.name = "eqbandvvv";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(input);
    lsc = logical_stop_cnt_cvt(hz);
    if (susp->susp.log_stop_cnt > lsc)
        susp->susp.log_stop_cnt = lsc;
    lsc = logical_stop_cnt_cvt(gain);
    if (susp->susp.log_stop_cnt > lsc)
        susp->susp.log_stop_cnt = lsc;
    lsc = logical_stop_cnt_cvt(width);
    if (susp->susp.log_stop_cnt > lsc)
        susp->susp.log_stop_cnt = lsc;
    susp->started = false;
    susp->susp.current = 0;
    susp->input = input;
    susp->input_cnt = 0;
    susp->hz = hz;
    susp->hz_cnt = 0;
    susp->hz_pHaSe = 0.0;
    susp->hz_pHaSe_iNcR = hz->sr / sr;
    susp->hz_n = 0;
    susp->output_per_hz = sr / hz->sr;
    susp->gain = gain;
    susp->gain_cnt = 0;
    susp->gain_pHaSe = 0.0;
    susp->gain_pHaSe_iNcR = gain->sr / sr;
    susp->gain_n = 0;
    susp->output_per_gain = sr / gain->sr;
    susp->width = width;
    susp->width_cnt = 0;
    susp->width_pHaSe = 0.0;
    susp->width_pHaSe_iNcR = width->sr / sr;
    susp->width_n = 0;
    susp->output_per_width = sr / width->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_eqbandvvv(sound_type input, sound_type hz, sound_type gain, sound_type width)
{
    sound_type input_copy = sound_copy(input);
    sound_type hz_copy = sound_copy(hz);
    sound_type gain_copy = sound_copy(gain);
    sound_type width_copy = sound_copy(width);
    return snd_make_eqbandvvv(input_copy, hz_copy, gain_copy, width_copy);
}
