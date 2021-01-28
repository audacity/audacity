/* inverse.c -- compute the inverse of a sampled function */

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
#include "cext.h"

#include "falloc.h"
#include "inverse.h"

void inverse_free(snd_susp_type a_susp);


typedef struct inverse_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s;
    int s_cnt;
    sample_block_values_type s_ptr;
    double s_prev;
    double s_time;
    double s_time_increment;
    double out_time_increment;
    boolean started;
} inverse_susp_node, *inverse_susp_type;

void inverse_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    inverse_susp_type susp = (inverse_susp_type) a_susp;
    int cnt = 0; /* how many samples read from s */
    int out_cnt = 0; /* how many samples output */
    int togo = 0; /* how many more to read from s in inner loop */
    int n;
    sample_block_type out;
    double out_time = susp->susp.current * susp->out_time_increment;
    
    register sample_block_values_type out_ptr;

    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "inverse_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure we are primed with first value */
    /* This is a lot of work just to prefetch susp->s_prev! */
    if (!susp->started) {
        susp->started = true;
        /* see comments below about susp_check_term_log_samples() */
        if (susp->s_cnt == 0) {
            susp_get_samples(s, s_ptr, s_cnt);
            if (susp->s_ptr == zero_block->samples) {
                susp->terminate_cnt = susp->susp.current;
            }
        }
        susp->s_prev = susp_fetch_sample(s, s_ptr, s_cnt);
    }

    while (out_cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't run past the s input sample block: */
        /* most fetch routines call susp_check_term_log_samples() here
         * but we can't becasue susp_check_term_log_samples() assumes
          * that output time progresses at the same rate as input time.
         * Here, some time warping is going on, so this doesn't work.
         * Instead, check for termination of s and fix terminate_cnt to
         * be the current output count rather than the current input time.
         */
        if (susp->s_cnt == 0) {
            susp_get_samples(s, s_ptr, s_cnt);
            if (susp->s_ptr == zero_block->samples) {
                susp->terminate_cnt = susp->susp.current + out_cnt;
                /* we can't simply terminate here because we might have 
                 * some output samples computed already, in which case we
                 * want to return them now and terminate the NEXT time we're
                 * called.
                 */
            }
        }
        togo = susp->s_cnt;

        /* if we ran past terminate time, fix up output */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + out_cnt) {
            /* pretend like we computed the correct number of samples */
            togo = 0;
            out_cnt = (long) (susp->terminate_cnt - susp->susp.current);
            /* exit the loop to complete the termination */
            break;
        }
        n = togo;
        s_ptr_reg = susp->s_ptr;
        if (n) do { /* the inner sample computation loop */
            /* scan s_ptr_reg to time t, output and loop */
            register double next_value = *s_ptr_reg++;
            while (out_time < next_value) {
                *out_ptr++ = (float) (susp->s_time +
                             (out_time - susp->s_prev) /
                             (susp->s->sr * (next_value - susp->s_prev)));
                out_time += susp->out_time_increment;
                if (++out_cnt >= max_sample_block_len) goto output_full;
            }
            susp->s_prev = next_value;
            susp->s_time += susp->s_time_increment;
        } while (--n); /* inner loop */
  output_full:
        /* using s_ptr_reg is a bad idea on RS/6000: */
        susp->s_ptr += (togo - n);
        susp_took(s_cnt, (togo - n));
        cnt += (togo - n);
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && out_cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = out_cnt;
        susp->susp.current += out_cnt;
    }
} /* inverse_fetch */


void inverse_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    inverse_susp_type susp = (inverse_susp_type) a_susp;
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


void inverse_mark(snd_susp_type a_susp)
{
    inverse_susp_type susp = (inverse_susp_type) a_susp;
    sound_xlmark(susp->s);
}


void inverse_free(snd_susp_type a_susp)
{
    inverse_susp_type susp = (inverse_susp_type) a_susp;
    sound_unref(susp->s);
    ffree_generic(susp, sizeof(inverse_susp_node), "inverse_free");
}


void inverse_print_tree(snd_susp_type a_susp, int n)
{
    inverse_susp_type susp = (inverse_susp_type) a_susp;
    indent(n);
    stdputstr("s:");
    sound_print_tree_1(susp->s, n);
}


sound_type snd_make_inverse(sound_type s, time_type t0, rate_type sr)
{
    register inverse_susp_type susp;

    falloc_generic(susp, inverse_susp_node, "snd_make_inverse");
    susp->susp.fetch = inverse_fetch;
    susp->terminate_cnt = UNKNOWN;

    /* initialize susp state */
    susp->susp.free = inverse_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = inverse_mark;
    susp->susp.print_tree = inverse_print_tree;
    susp->susp.name = "inverse";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = UNKNOWN; /* log stop time = term time */
    susp->susp.current = 0;
    susp->s = s;
    susp->s_cnt = 0;
    susp->s_prev = 0;
    susp->s_time = 0;
    susp->s_time_increment = 1 / s->sr;
    susp->out_time_increment = 1 / (sr * s->scale);
    susp->started = false;
    return sound_create((snd_susp_type)susp, t0, sr, 1.0 /* scale */);
}


sound_type snd_inverse(sound_type s, time_type t0, rate_type sr)
{
    sound_type s_copy = sound_copy(s);
    return snd_make_inverse(s_copy, t0, sr);
}
