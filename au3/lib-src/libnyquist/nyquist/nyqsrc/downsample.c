/* downsample.c -- linear interpolation to a lower sample rate */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */



#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "cext.h"
#include "downsample.h"

void down_free(snd_susp_type a_susp);


typedef struct down_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s;
    int s_cnt;
    sample_block_values_type s_ptr;

    /* support for interpolation of s */
    sample_type s_x1_sample;
    double s_pHaSe;
    double s_pHaSe_iNcR;

    /* support for ramp between samples of s */
    /*can we delete these?
    double output_per_s;
    long s_n; */
} down_susp_node, *down_susp_type;


void down_i_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    down_susp_type susp = (down_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type s_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double s_pHaSe_iNcR_rEg = susp->s_pHaSe_iNcR;
    register double s_pHaSe_ReG;
    register sample_type s_x1_sample_reg;
    falloc_sample_block(out, "down_i_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_term_log_samples(s, s_ptr, s_cnt);
        susp->s_x1_sample = susp_fetch_sample(s, s_ptr, s_cnt);
    }

    susp_check_term_log_samples(s, s_ptr, s_cnt);
    s_x2_sample = susp_current_sample(s, s_ptr);
    /* initially, s_x1_sample and s_x2_samples will be the first 2 samples 
     * and phase will be zero, so interpolation between these two will yield
     * s_x1_sample. */
    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;
        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo <= 0) {
                togo = 0;
                break;
            }
        }

        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
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
        s_pHaSe_ReG = susp->s_pHaSe;
        s_x1_sample_reg = susp->s_x1_sample;
        out_ptr_reg = out_ptr;
        if (n) do {
            while (s_pHaSe_ReG >= 1.0) {
                if (s_pHaSe_ReG < 2) { /* quick, just take one sample */
                    s_x1_sample_reg = s_x2_sample;
                    /* pick up next sample as s_x2_sample: */
                    susp->s_ptr++;
                    susp_took(s_cnt, 1);
                    s_pHaSe_ReG -= 1.0;
                } else { /* jump over as much input as possible */
                    int take = (int) s_pHaSe_ReG; /* rounds down */
                    take--; /* leave s_pHaSe_ReG > 1 so we stay in loop */
                    /* next iteration will set s_x1_sample_reg */
                    if (take > susp->s_cnt) take = susp->s_cnt;
                    susp->s_ptr += take;
                    susp_took(s_cnt, take);
                    s_pHaSe_ReG -= take;
                }
                /* derived from susp_check_term_log_samples_break, but with
                        a goto instead of a break */
                if (susp->s_cnt == 0) {
                    susp_get_samples(s, s_ptr, s_cnt);
                    terminate_test(s_ptr, s, susp->s_cnt);
                    /* see if newly discovered logical stop time: */
                    logical_stop_test(s, susp->s_cnt);
                    if ((susp->terminate_cnt != UNKNOWN &&
                         susp->terminate_cnt <
                           susp->susp.current + cnt + togo) ||
                        (!susp->logically_stopped && 
                         susp->susp.log_stop_cnt != UNKNOWN &&
                         susp->susp.log_stop_cnt < 
                           susp->susp.current + cnt + togo)) {
                        /* Because we are down sampling, we could have just
                           computed an output at sample N and be working on 
                           sample N+1, but then the next input sample is 
                           logically stopped. Bad because we cannot back up
                           and undo sample N to put it in the next block with
                           a logical stop flag set. Our only choice is to "fix"
                           the logical stop time to be on the next sample. */
                        if (susp->terminate_cnt != UNKNOWN &&
                            susp->terminate_cnt < susp->susp.current + togo - n) {
                            susp->terminate_cnt = susp->susp.current + togo - n;
                        }
                        if (susp->susp.log_stop_cnt != UNKNOWN &&
                            susp->susp.log_stop_cnt <
                            susp->susp.current + togo - n) {
                            susp->susp.log_stop_cnt = susp->susp.current + togo - n;
                        }
                        goto breakout;
                    }
                }
                s_x2_sample = susp_current_sample(s, s_ptr);
            }
            *out_ptr_reg++ = (sample_type) 
                (s_x1_sample_reg * (1 - s_pHaSe_ReG) + 
                 s_x2_sample * s_pHaSe_ReG);
            s_pHaSe_ReG += s_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */
      breakout:
        togo -= n;
        susp->s_pHaSe = s_pHaSe_ReG;
        susp->s_x1_sample = s_x1_sample_reg;
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
} /* down_i_fetch */


void down_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    down_susp_type susp = (down_susp_type) a_susp;
    int64_t final_count = MIN(susp->susp.current + max_sample_block_len,
                              susp->susp.toss_cnt);
    time_type final_time = susp->susp.t0 + final_count / susp->susp.sr;
    long n;

    /* fetch samples from s up to final_time for this block of zeros */
    while (((long) ((final_time - susp->s->t0) * susp->s->sr + 0.5)) >=
           susp->s->current)
        susp_get_samples(s, s_ptr, s_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    if (final_count == susp->susp.toss_cnt) {
        n = (long) ROUNDBIG((final_time - susp->s->t0) * susp->s->sr -
                            (susp->s->current - susp->s_cnt));
        susp->s_ptr += n;
        susp_took(s_cnt, n);
        susp->susp.fetch = susp->susp.keep_fetch;
    }
    snd_list->block_len = (short) (final_count - susp->susp.current);
    susp->susp.current = final_count;
    snd_list->u.next = snd_list_create((snd_susp_type) susp);
    snd_list->block = internal_zero_block;
}


void down_mark(snd_susp_type a_susp)
{
    down_susp_type susp = (down_susp_type) a_susp;
    sound_xlmark(susp->s);
}


void down_free(snd_susp_type a_susp)
{
    down_susp_type susp = (down_susp_type) a_susp;
    sound_unref(susp->s);
    ffree_generic(susp, sizeof(down_susp_node), "down_free");
}


void down_print_tree(snd_susp_type a_susp, int n)
{
    down_susp_type susp = (down_susp_type) a_susp;
    indent(n);
    stdputstr("s:");
    sound_print_tree_1(susp->s, n);
}


sound_type snd_make_down(rate_type sr, sound_type s)
{
    register down_susp_type susp;
    /* sr specified as input parameter */
    time_type t0 = s->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;

    if (s->sr < sr) {
        sound_unref(s);
        xlfail("snd-down: output sample rate must be lower than input");
    }
    falloc_generic(susp, down_susp_node, "snd_make_down");

    susp->susp.fetch = down_i_fetch;

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s->t0) sound_prepend_zeros(s, t0);
    /* minimum start time over all inputs: */
    t0_min = min(s->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = down_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = down_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = down_mark;
    susp->susp.print_tree = down_print_tree;
    susp->susp.name = "down";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s);
    susp->started = false;
    susp->susp.current = 0;
    susp->s = s;
    susp->s_cnt = 0;
    susp->s_pHaSe = 0.0;
    susp->s_pHaSe_iNcR = s->sr / sr;
    //susp->s_n = 0;
    //susp->output_per_s = sr / s->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_down(rate_type sr, sound_type s)
{
    sound_type s_copy = sound_copy(s);
    return snd_make_down(sr, s_copy);
}
