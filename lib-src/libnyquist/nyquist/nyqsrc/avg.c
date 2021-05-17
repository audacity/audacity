#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "avg.h"

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */


void avg_free(snd_susp_type a_susp);

struct avg_susp_struct;

typedef sample_type (*process_block_type)(struct avg_susp_struct *susp);

typedef struct avg_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type s;
    int s_cnt;
    sample_block_values_type s_ptr;
    /* blocksize is how many input samples to process for an output sample */
    long blocksize;
    /* stepsize is how far to advance to get the next block of samples */
    long stepsize;
    sample_type *buffer;
    sample_type *fillptr;  /* samples are added to buffer at fillptr */
    sample_type *endptr;   /* until endptr is reached */
    process_block_type process_block;
} avg_susp_node, *avg_susp_type;


sample_type average_block(avg_susp_type susp)
{
    /* this version just computes average */
    double sum = 0.0;
    int i;
    for (i = 0; i < susp->blocksize; i++) {
        sum += susp->buffer[i];
    }
    for (i = susp->stepsize; i < susp->blocksize; i++) {
        susp->buffer[i - susp->stepsize] = susp->buffer[i];
    }
    return (sample_type) (sum / susp->blocksize);
}


sample_type peak_block(avg_susp_type susp)
{
    /* this version just computes average */
    sample_type peak = 0.0F;
    sample_type minus_peak = 0.0F;
    int i;
    for (i = 0; i < susp->blocksize; i++) {
        sample_type s = susp->buffer[i];
    if (s > peak) {
        peak = s; minus_peak = -s;
    } else if (s < minus_peak) {
        minus_peak = s; peak = -s;
    }
    }
    for (i = susp->stepsize; i < susp->blocksize; i++) {
        susp->buffer[i - susp->stepsize] = susp->buffer[i];
    }
    return peak;
}


void avg_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    avg_susp_type susp = (avg_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int64_t togo = 0;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_type *fillptr_reg;
    register sample_type *endptr_reg = susp->endptr;

    register sample_block_values_type s_ptr_reg;
    falloc_sample_block(out, "avg_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
    /* first compute how many samples to generate in inner loop: */
    /* don't overflow the output sample block: */
    togo = (max_sample_block_len - cnt) * susp->stepsize;

    /* don't run past the s input sample block: */
    susp_check_term_log_samples(s, s_ptr, s_cnt);
    togo = MIN(togo, susp->s_cnt);

    /* don't run past terminate time */
    if (susp->terminate_cnt != UNKNOWN &&
        susp->terminate_cnt <= susp->susp.current + cnt + togo/susp->stepsize) {
        togo = (susp->terminate_cnt - (susp->susp.current + cnt)) * susp->stepsize;
        if (togo == 0) break;
    }


    /* don't run past logical stop time */
    if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
        int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
        /* break if to_stop == 0 (we're at the logical stop)
         * AND cnt > 0 (we're not at the beginning of the
         * output block).
         */
        if (to_stop < togo/susp->stepsize) {
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
            togo = to_stop * susp->stepsize;
        }
    }

    n = (int) togo;
    s_ptr_reg = susp->s_ptr;
    fillptr_reg = susp->fillptr;
    if (n) do { /* the inner sample computation loop */
        *fillptr_reg++ = *s_ptr_reg++;
        if (fillptr_reg >= endptr_reg) {
           *out_ptr++ = (*(susp->process_block))(susp);
           cnt++;
           fillptr_reg -= susp->stepsize;
        }
    } while (--n); /* inner loop */

    /* using s_ptr_reg is a bad idea on RS/6000: */
    susp->s_ptr += togo;
    susp->fillptr = fillptr_reg;
    susp_took(s_cnt, (int) togo);
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
} /* avg_s_fetch */


void avg_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    avg_susp_type susp = (avg_susp_type) a_susp;
    int64_t final_count = MIN(susp->susp.current + max_sample_block_len,
                              susp->susp.toss_cnt);
    time_type final_time = susp->susp.t0 + final_count / susp->susp.sr;
    int64_t n;

    /* fetch samples from s up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->s->t0) * susp->s->sr)) >=
           susp->s->current)
        susp_get_samples(s, s_ptr, s_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    if (final_count == susp->susp.toss_cnt) {
        n = ROUNDBIG((final_time - susp->s->t0) * susp->s->sr -
                     (susp->s->current - susp->s_cnt));
        susp->s_ptr += n;
        susp_took(s_cnt, (int) n);
        susp->susp.fetch = susp->susp.keep_fetch;
    }
    snd_list->block_len = (short) (final_count - susp->susp.current);
    susp->susp.current = final_count;
    snd_list->u.next = snd_list_create((snd_susp_type) susp);
    snd_list->block = internal_zero_block;
}


void avg_mark(snd_susp_type a_susp)
{
    avg_susp_type susp = (avg_susp_type) a_susp;
    sound_xlmark(susp->s);
}


void avg_free(snd_susp_type a_susp)
{
    avg_susp_type susp = (avg_susp_type) a_susp;
    sound_unref(susp->s);
    free(susp->buffer);
    ffree_generic(susp, sizeof(avg_susp_node), "avg_free");
}


void avg_print_tree(snd_susp_type a_susp, int n)
{
    avg_susp_type susp = (avg_susp_type) a_susp;
    indent(n);
    stdputstr("s:");
    sound_print_tree_1(susp->s, n);
}


sound_type snd_make_avg(sound_type s, long blocksize, long stepsize, long op)
{
    long buffersize;
    register avg_susp_type susp;
    rate_type sr = s->sr;
    time_type t0 = s->t0;
    time_type t0_min = t0;

    /* ASSUME 32-BIT INTS */
    /* Later, we will compute togo, the number of input samples to process
       for one block of output. We read stepsize of input for each sample
       of output, so the total is stepsize * max_sample_block_len, but 
       this could be very big and cause integer overflow, so here, we
       prevent the overflow by limiting stepsize */
    if (stepsize > (0x7FFFFFFF / max_sample_block_len)) {
        xlfail("In SND-AVG, stepsize is too big");
    }

    falloc_generic(susp, avg_susp_node, "snd_make_avg");
    susp->susp.fetch = avg_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < s->t0) sound_prepend_zeros(s, t0);
    /* minimum start time over all inputs: */
    t0_min = MIN(s->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = ROUNDBIG((t0 - t0_min) * sr);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = avg_toss_fetch;
        t0 = t0_min;
    }

    /* initialize susp state */
    susp->susp.free = avg_free;
    susp->susp.sr = sr / stepsize;
    susp->susp.t0 = t0;
    susp->susp.mark = avg_mark;
    susp->susp.print_tree = avg_print_tree;
    susp->susp.name = "avg";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(s);
    susp->susp.current = 0;
    susp->s = s;
    susp->s_cnt = 0;
    susp->blocksize = blocksize;
    susp->stepsize = stepsize;
    /* We need at least blocksize samples in buffer, 
       but if stepsize > blocksize,
       it is convenient to put stepsize samples in buffer. This allows us to
       step ahead by stepsize samples just by flushing the buffer. */
    buffersize = MAX(blocksize, stepsize);
    susp->buffer = (sample_type *) malloc(buffersize * sizeof(sample_type));
    if (!susp->buffer) {
        sound_unref(susp->s);
        ffree_generic(susp, sizeof(avg_susp_node), "avg_free");
        xlfail("memory allocation failed in SND-AVG");
    }
    susp->fillptr = susp->buffer;
    susp->endptr = susp->buffer + buffersize;
    susp->process_block = average_block;
    if (op == op_peak) susp->process_block = peak_block;
    /* scale factor gets passed to output signal: */
    return sound_create((snd_susp_type) susp, t0, susp->susp.sr, susp->s->scale);
}


sound_type snd_avg(sound_type s, long blocksize, long stepsize, long op)
{
    sound_type s_copy = sound_copy(s);
    return snd_make_avg(s_copy, blocksize, stepsize, op);
}
