#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "partial.h"

void partial_free(snd_susp_type a_susp);


typedef struct partial_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type env;
    int env_cnt;
    sample_block_values_type env_ptr;

    /* support for interpolation of env */
    sample_type env_x1_sample;
    double env_pHaSe;
    double env_pHaSe_iNcR;

    /* support for ramp between samples of env */
    double output_per_env;
    int64_t env_n;

    long phase;
    long ph_incr;
} partial_susp_node, *partial_susp_type;


#include "sine.h"


void partial_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    register sample_block_values_type env_ptr_reg;
    falloc_sample_block(out, "partial_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the env input sample block: */
        susp_check_term_log_samples(env, env_ptr, env_cnt);
        togo = min(togo, susp->env_cnt);

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
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        env_ptr_reg = susp->env_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT] * (sample_type) *env_ptr_reg++;
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;;
        } while (--n); /* inner loop */

        susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
        /* using env_ptr_reg is a bad idea on RS/6000: */
        susp->env_ptr += togo;
        out_ptr += togo;
        susp_took(env_cnt, togo);
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
} /* partial_n_fetch */


void partial_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    register sample_type env_scale_reg = susp->env->scale;
    register sample_block_values_type env_ptr_reg;
    falloc_sample_block(out, "partial_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the env input sample block: */
        susp_check_term_log_samples(env, env_ptr, env_cnt);
        togo = min(togo, susp->env_cnt);

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
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        env_ptr_reg = susp->env_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT] * (sample_type) (env_scale_reg * *env_ptr_reg++);
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;;
        } while (--n); /* inner loop */

        susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
        /* using env_ptr_reg is a bad idea on RS/6000: */
        susp->env_ptr += togo;
        out_ptr += togo;
        susp_took(env_cnt, togo);
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
} /* partial_s_fetch */


void partial_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type env_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    register double env_pHaSe_iNcR_rEg = susp->env_pHaSe_iNcR;
    register double env_pHaSe_ReG;
    register sample_type env_x1_sample_reg;
    falloc_sample_block(out, "partial_i_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(env, env_ptr, env_cnt);
        susp->env_x1_sample = susp_fetch_sample(env, env_ptr, env_cnt);
    }

    susp_check_term_log_samples(env, env_ptr, env_cnt);
    env_x2_sample = susp_current_sample(env, env_ptr);

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
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        env_pHaSe_ReG = susp->env_pHaSe;
        env_x1_sample_reg = susp->env_x1_sample;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            if (env_pHaSe_ReG >= 1.0) {
                env_x1_sample_reg = env_x2_sample;
                /* pick up next sample as env_x2_sample: */
                susp->env_ptr++;
                susp_took(env_cnt, 1);
                env_pHaSe_ReG -= 1.0;
                susp_check_term_log_samples_break(env, env_ptr, env_cnt, env_x2_sample);
            }
            *out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT] * (sample_type) 
                (env_x1_sample_reg * (1 - env_pHaSe_ReG) + env_x2_sample * env_pHaSe_ReG);
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;;
            env_pHaSe_ReG += env_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
        susp->env_pHaSe = env_pHaSe_ReG;
        susp->env_x1_sample = env_x1_sample_reg;
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
} /* partial_i_fetch */


void partial_r_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type env_DeLtA;
    sample_type env_val;
    sample_type env_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    falloc_sample_block(out, "partial_r_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->env_pHaSe = 1.0;
    }

    susp_check_term_log_samples(env, env_ptr, env_cnt);
    env_x2_sample = susp_current_sample(env, env_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* grab next env_x2_sample when phase goes past 1.0; */
        /* we use env_n (computed below) to avoid roundoff errors: */
        if (susp->env_n <= 0) {
            susp->env_x1_sample = env_x2_sample;
            susp->env_ptr++;
            susp_took(env_cnt, 1);
            susp->env_pHaSe -= 1.0;
            susp_check_term_log_samples(env, env_ptr, env_cnt);
            env_x2_sample = susp_current_sample(env, env_ptr);
            /* env_n gets number of samples before phase exceeds 1.0: */
            susp->env_n = (int64_t) ((1.0 - susp->env_pHaSe) *
                                        susp->output_per_env);
        }
        togo = (int) min(togo, susp->env_n);
        env_DeLtA = (sample_type) ((env_x2_sample - susp->env_x1_sample) * susp->env_pHaSe_iNcR);
        env_val = (sample_type) (susp->env_x1_sample * (1.0 - susp->env_pHaSe) +
                 env_x2_sample * susp->env_pHaSe);

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
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT] * (sample_type) env_val;
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;;
            env_val += env_DeLtA;
        } while (--n); /* inner loop */

        susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
        out_ptr += togo;
        susp->env_pHaSe += togo * susp->env_pHaSe_iNcR;
        susp->env_n -= togo;
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
} /* partial_r_fetch */


void partial_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from env up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->env->t0) * susp->env->sr)) >=
           susp->env->current)
        susp_get_samples(env, env_ptr, env_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->env->t0) * susp->env->sr -
         (susp->env->current - susp->env_cnt));
    susp->env_ptr += n;
    susp_took(env_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void partial_mark(snd_susp_type a_susp)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    sound_xlmark(susp->env);
}


void partial_free(snd_susp_type a_susp)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    sound_unref(susp->env);
    ffree_generic(susp, sizeof(partial_susp_node), "partial_free");
}


void partial_print_tree(snd_susp_type a_susp, int n)
{
    partial_susp_type susp = (partial_susp_type) a_susp;
    indent(n);
    stdputstr("env:");
    sound_print_tree_1(susp->env, n);
}


sound_type snd_make_partial(rate_type sr, double hz, sound_type env)
{
    register partial_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = env->t0;
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, partial_susp_node, "snd_make_partial");
    susp->phase = 0;
    susp->ph_incr = ROUND32((hz * SINE_TABLE_LEN) * (1 << SINE_TABLE_SHIFT) / sr);

    /* make sure no sample rate is too high */
    if (env->sr > sr) {
        sound_unref(env);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(env, sr);
    switch (interp_desc) {
      case INTERP_n: susp->susp.fetch = partial_n_fetch; break;
      case INTERP_s: susp->susp.fetch = partial_s_fetch; break;
      case INTERP_i: susp->susp.fetch = partial_i_fetch; break;
      case INTERP_r: susp->susp.fetch = partial_r_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < env->t0) sound_prepend_zeros(env, t0);
    /* minimum start time over all inputs: */
    t0_min = min(env->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = partial_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = partial_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = partial_mark;
    susp->susp.print_tree = partial_print_tree;
    susp->susp.name = "partial";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(env);
    susp->started = false;
    susp->susp.current = 0;
    susp->env = env;
    susp->env_cnt = 0;
    susp->env_pHaSe = 0.0;
    susp->env_pHaSe_iNcR = env->sr / sr;
    susp->env_n = 0;
    susp->output_per_env = sr / env->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_partial(rate_type sr, double hz, sound_type env)
{
    sound_type env_copy = sound_copy(env);
    return snd_make_partial(sr, hz, env_copy);
}
