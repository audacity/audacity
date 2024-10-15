#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "follow.h"

void follow_free(snd_susp_type a_susp);


typedef struct follow_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    sound_type sndin;
    int sndin_cnt;
    sample_block_values_type sndin_ptr;

    long lookahead;
    sample_type *delaybuf;
    sample_type *delayptr;
    sample_type *prevptr;
    sample_type *endptr;
    double floor;
    double rise_factor;
    double fall_factor;
    double value;
} follow_susp_node, *follow_susp_type;

/* Description: this is a sophisticated envelope follower.
    The input is an envelope, e.g. something produced with
    the AVG function. The purpose of this function is to
    generate a smooth envelope that is generally not less
    than the input signal. In other words, we want to "ride"
    the peaks of the signal with a smooth function. The 
    algorithm is as follows: keep a current output value
    (called the "value"). The value is allowed to increase
    by at most rise_factor and decrease by at most fall_factor.
    Therefore, the next value should be between
    value * rise_factor and value * fall_factor. If the input
    is in this range, then the next value is simply the input.
    If the input is less than value * fall_factor, then the
    next value is just value * fall_factor, which will be greater
    than the input signal. If the input is greater than value *
    rise_factor, then we compute a rising envelope that meets
    the input value by working bacwards in time, changing the
    previous values to input / rise_factor, input / rise_factor^2,
    input / rise_factor^3, etc. until this new envelope intersects
    the previously computed values. There is only a limited buffer
    in which we can work backwards, so if the new envelope does not
    intersect the old one, then make yet another pass, this time
    from the oldest buffered value forward, increasing on each 
    sample by rise_factor to produce a maximal envelope. This will 
    still be less than the input.
    
    The value has a lower limit of floor to make sure value has a 
    reasonable positive value from which to begin an attack.
    
    Because this algorithm can make 2 passes through the buffer on
    sharply rising input signals, it is not particularly fast. The
    assumption is that it operates on fairly short buffers at low
    sample rates appropriate for gain control, so this should not
    matter.
    */

static sample_type *create_buf(double floor, long lookahead)
{
    sample_type *buf = (sample_type *) malloc(lookahead * sizeof(sample_type));
    int i;
    
    for (i = 0; i < lookahead; i++) buf[i] = (sample_type) floor;
    return buf;
}


void follow_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    follow_susp_type susp = (follow_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long lookahead_reg;
    register sample_type * delayptr_reg;
    register sample_type * prevptr_reg;
    register sample_type * endptr_reg;
    register double floor_reg;
    register double rise_factor_reg;
    register double fall_factor_reg;
    register sample_type sndin_scale_reg = susp->sndin->scale;
    register sample_block_values_type sndin_ptr_reg;
    falloc_sample_block(out, "follow_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the sndin input sample block: */
        susp_check_term_samples(sndin, sndin_ptr, sndin_cnt);
        togo = min(togo, susp->sndin_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        lookahead_reg = susp->lookahead;
        delayptr_reg = susp->delayptr;
        prevptr_reg = susp->prevptr;
        endptr_reg = susp->endptr;
        floor_reg = susp->floor;
        rise_factor_reg = susp->rise_factor;
        fall_factor_reg = susp->fall_factor;
        sndin_ptr_reg = susp->sndin_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            sample_type current = (sndin_scale_reg * *sndin_ptr_reg++);
            sample_type high = (sample_type) (*prevptr_reg * rise_factor_reg);
            sample_type low = (sample_type) (*prevptr_reg * fall_factor_reg);
            if (low < floor_reg) low = (sample_type) floor_reg;
            if (current < low) *delayptr_reg = (sample_type) low;
            else if (current < high) *delayptr_reg = current;
            else /* current > high */ {
                /* work back from current */
                double rise_inverse = 1.0 / rise_factor_reg;
                double temp = current * rise_inverse;
                boolean ok = false;
                sample_type *ptr = prevptr_reg;
                int i;
                               
                for (i = 0; i < lookahead_reg - 2; i++) {
                    if (*ptr < temp) {
                    *ptr-- = (sample_type) temp;
                    temp *= rise_inverse;
                    if (ptr < susp->delaybuf)
                        ptr = endptr_reg - 1;
                    } else {
                        ok = true;
                        break;
                    }
                }
                if (!ok && (*ptr < temp)) {
                    temp = *ptr;
                    for (i = 0; i < lookahead_reg - 1; i++) {
                        ptr++;
                        if (ptr == endptr_reg) ptr = susp->delaybuf;
                        temp *= rise_factor_reg;
                        *ptr = (sample_type) temp;
                    }
                } else *delayptr_reg = current;
            }
            prevptr_reg = delayptr_reg++;
            if (delayptr_reg == endptr_reg) delayptr_reg = susp->delaybuf;
            *out_ptr_reg++ = *delayptr_reg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->lookahead = lookahead_reg;
        susp->delayptr = delayptr_reg;
        susp->prevptr = prevptr_reg;
        susp->floor = floor_reg;
        /* using sndin_ptr_reg is a bad idea on RS/6000: */
        susp->sndin_ptr += togo;
        out_ptr += togo;
        susp_took(sndin_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* follow_s_fetch */


void follow_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    follow_susp_type susp = (follow_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from sndin up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->sndin->t0) * susp->sndin->sr)) >=
           susp->sndin->current)
        susp_get_samples(sndin, sndin_ptr, sndin_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->sndin->t0) * susp->sndin->sr -
         (susp->sndin->current - susp->sndin_cnt));
    susp->sndin_ptr += n;
    susp_took(sndin_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void follow_mark(snd_susp_type a_susp)
{
    follow_susp_type susp = (follow_susp_type) a_susp;
    sound_xlmark(susp->sndin);
}


void follow_free(snd_susp_type a_susp)
{
    follow_susp_type susp = (follow_susp_type) a_susp;
free(susp->delaybuf);    sound_unref(susp->sndin);
    ffree_generic(susp, sizeof(follow_susp_node), "follow_free");
}


void follow_print_tree(snd_susp_type a_susp, int n)
{
    follow_susp_type susp = (follow_susp_type) a_susp;
    indent(n);
    stdputstr("sndin:");
    sound_print_tree_1(susp->sndin, n);
}


sound_type snd_make_follow(sound_type sndin, double floor, double risetime, double falltime, long lookahead)
{
    register follow_susp_type susp;
    rate_type sr = sndin->sr;
    time_type t0 = sndin->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, follow_susp_node, "snd_make_follow");
    susp->lookahead = lookahead = lookahead + 1;
    susp->delaybuf = create_buf(floor, lookahead);
    susp->delayptr = susp->delaybuf;
    susp->prevptr = susp->delaybuf + lookahead - 1;
    *(susp->prevptr) = (sample_type) floor;;
    susp->endptr = susp->delaybuf + lookahead;
    susp->floor = floor; floor = log(floor);;
    susp->rise_factor = exp(- floor / (sndin->sr * risetime + 0.5));
    susp->fall_factor = exp(floor / (sndin->sr * falltime + 0.5));
    susp->value = susp->floor;
    susp->susp.fetch = follow_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < sndin->t0) sound_prepend_zeros(sndin, t0);
    /* minimum start time over all inputs: */
    t0_min = min(sndin->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = follow_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = follow_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = follow_mark;
    susp->susp.print_tree = follow_print_tree;
    susp->susp.name = "follow";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->sndin = sndin;
    susp->sndin_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_follow(sound_type sndin, double floor, double risetime, double falltime, long lookahead)
{
    sound_type sndin_copy = sound_copy(sndin);
    return snd_make_follow(sndin_copy, floor, risetime, falltime, lookahead);
}
