#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "compose.h"

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

void compose_free(snd_susp_type a_susp);


typedef struct compose_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type f;
    int f_cnt;
    sample_block_values_type f_ptr;
    sample_type f_prev;
    double f_time;
    double f_time_increment;
    boolean started;
    sound_type g;
    int g_cnt;
    sample_block_values_type g_ptr;
} compose_susp_node, *compose_susp_type;


/* compose_fetch -- computes f(g(t)) */
/**/
void compose_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    compose_susp_type susp = (compose_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo = 0;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_block_values_type g_ptr_reg;
    register sample_block_values_type f_ptr_reg;
    falloc_sample_block(out, "compose_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure we are primed with first value of f */
    /* This is a lot of work just to prefetch susp->f_prev! */
    if (!susp->started) {
        susp->started = true;
        /* see comments below about susp_check_term_log_samples() */
        if (susp->f_cnt == 0) {
            susp_get_samples(f, f_ptr, f_cnt);
            if (susp->f_ptr == zero_block->samples) {
                susp->terminate_cnt = susp->susp.current;
            }
        }
        susp->f_prev = susp_fetch_sample(f, f_ptr, f_cnt);
        susp->f_time += susp->f_time_increment;
    }

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the f input sample block: */
        /* most fetch routines call susp_check_term_log_samples() here
         * but we can't becasue susp_check_term_log_samples() assumes
          * that output time progresses at the same rate as input time.
         * Here, some time warping is going on, so this doesn't work.
         * Instead, check for termination of f and fix terminate_cnt to
         * be the current output count rather than the current input time.
         */
        if (susp->f_cnt == 0) {
            susp_get_samples(f, f_ptr, f_cnt);
            if (susp->f->logical_stop_cnt == susp->f->current - susp->f_cnt) {
                if (susp->susp.log_stop_cnt == UNKNOWN) {
                    susp->susp.log_stop_cnt = susp->susp.current + cnt;
                }
            }
            if (susp->f_ptr == zero_block->samples) {
                susp->terminate_cnt = susp->susp.current + cnt;
                /* we can't simply terminate here because we might have 
                 * some output samples computed already, in which case we
                 * want to return them now and terminate the NEXT time we're
                 * called.
                 */
            }
        }

#ifdef CUT
        /* don't run past the f input sample block: */
        susp_check_term_log_samples(f, f_ptr, f_cnt);
        togo = MIN(togo, susp->f_cnt);
#endif
        /* don't run past the g input sample block: */
        susp_check_term_samples(g, g_ptr, g_cnt);
        togo = MIN(togo, susp->g_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo == 0) break;
        }

        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt -
                              (susp->susp.current + cnt);
            if (to_stop < togo && ((togo = (int) to_stop) == 0)) break;
        }

        n = togo;

        g_ptr_reg = susp->g_ptr;
        f_ptr_reg = susp->f_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double g_of_t = *g_ptr_reg;
            #if 0
            float tmp;  /* for debugging */
            nyquist_printf("output sample %d, g_of_t %g", susp->susp.current + cnt, g_of_t);
            #endif
            /* now we scan f and interpolate at time point g_of_t */
            while (susp->f_time < g_of_t) {
                susp->f_time += susp->f_time_increment;
                susp->f_prev = *f_ptr_reg++;
/*                nyquist_printf(", (f_time %g, f %g)", susp->f_time, *f_ptr_reg); */
                susp->f_ptr++;
                susp->f_cnt--;
                if (susp->f_cnt == 0) {
                    togo -= n;
/*                    stdputstr("\n\tf out of samples...\n"); */
                    goto f_out_of_samples;
                }
            }
            g_ptr_reg++;
            *out_ptr_reg++ /* = tmp */ = 
                (sample_type) (*f_ptr_reg - (*f_ptr_reg - susp->f_prev) *
                                            (susp->f_time - g_of_t) * susp->f->sr);
/*            nyquist_printf(", output %g\n", tmp);*/
        } while (--n); /* inner loop */
f_out_of_samples:
        /* using g_ptr_reg is a bad idea on RS/6000: */
        susp->g_ptr += togo;
        out_ptr += togo;
        susp_took(g_cnt, togo);
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
} /* compose_fetch */


void compose_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    compose_susp_type susp = (compose_susp_type) a_susp;
    long final_count = (long) MIN(susp->susp.current + max_sample_block_len,
                                  susp->susp.toss_cnt);
    time_type final_time = susp->susp.t0 + final_count / susp->susp.sr;
    long n;

    /* fetch samples from f up to final_time for this block of zeros */
    while (((long) ((final_time - susp->f->t0) * susp->f->sr + 0.5)) >=
           susp->f->current)
        susp_get_samples(f, f_ptr, f_cnt);
    /* fetch samples from g up to final_time for this block of zeros */
    while (((long) ((final_time - susp->g->t0) * susp->g->sr + 0.5)) >=
           susp->g->current)
        susp_get_samples(g, g_ptr, g_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    if (final_count == susp->susp.toss_cnt) {
        n = ROUND32((final_time - susp->f->t0) * susp->f->sr -
                    (susp->f->current - susp->f_cnt));
        susp->f_ptr += n;
        susp_took(f_cnt, n);
        n = ROUND32((final_time - susp->g->t0) * susp->g->sr -
                    (susp->g->current - susp->g_cnt));
        susp->g_ptr += n;
        susp_took(g_cnt, n);
        susp->susp.fetch = susp->susp.keep_fetch;
    }
    snd_list->block_len = (short) (final_count - susp->susp.current);
    susp->susp.current = final_count;
    snd_list->u.next = snd_list_create((snd_susp_type) susp);
    snd_list->block = internal_zero_block;
}


void compose_mark(snd_susp_type a_susp)
{
    compose_susp_type susp = (compose_susp_type) a_susp;
    sound_xlmark(susp->f);
    sound_xlmark(susp->g);
}


void compose_free(snd_susp_type a_susp)
{
    compose_susp_type susp = (compose_susp_type) a_susp;
    sound_unref(susp->f);
    sound_unref(susp->g);
    ffree_generic(susp, sizeof(compose_susp_node), "compose_free");
}


void compose_print_tree(snd_susp_type a_susp, int n)
{
    compose_susp_type susp = (compose_susp_type) a_susp;
    indent(n);
    stdputstr("f:");
    sound_print_tree_1(susp->f, n);

    indent(n);
    stdputstr("g:");
    sound_print_tree_1(susp->g, n);
}


sound_type snd_make_compose(sound_type f, sound_type g)
{
    register compose_susp_type susp;
    rate_type sr = g->sr;
    time_type t0 = g->t0;

    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;

    /* combine scale factors of linear inputs (S1 S2) */
    scale_factor *= f->scale;
    f->scale = 1.0F;

    /* scale factor in g effectively scales sample rate of f: */
    f->sr *= g->scale;
/* BUG */
    /* probably need to correct f->t0, but I don't understand this,
        so I'll leave this until we have some test cases */

    falloc_generic(susp, compose_susp_node, "snd_make_compose");
    susp->susp.fetch = compose_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
/* BUG: do we need to prepend to f?
    if (t0 < f->t0) sound_prepend_zeros(f, t0); */
    if (t0 < g->t0) sound_prepend_zeros(g, t0);
    /* minimum start time over all inputs: */
    t0_min = MIN(g->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = ROUNDBIG((t0 - t0_min) * sr);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = compose_toss_fetch;
        t0 = t0_min;
    }

    /* initialize susp state */
    susp->susp.free = compose_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = compose_mark;
    susp->susp.print_tree = compose_print_tree;
    susp->susp.name = "compose";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = f->logical_stop_cnt;
    if (susp->susp.log_stop_cnt > g->logical_stop_cnt)
        susp->susp.log_stop_cnt = g->logical_stop_cnt;
    susp->susp.current = 0;
    susp->f = f;
    susp->f_cnt = 0;
    susp->f_time = 0;
    susp->f_time_increment = 1 / f->sr;
    susp->g = g;
    susp->g_cnt = 0;
    susp->started = false;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_compose(sound_type f, sound_type g)
{
    sound_type f_copy = sound_copy(f);
    sound_type g_copy = sound_copy(g);
    return snd_make_compose(f_copy, g_copy);
}
