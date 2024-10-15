#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "alpass.h"

void alpass_free(snd_susp_type a_susp);


typedef struct alpass_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    sound_type input;
    int input_cnt;
    sample_block_values_type input_ptr;

    double feedback;
    long delaylen;
    sample_type *delaybuf;
    sample_type *delayptr;
    sample_type *endptr;
} alpass_susp_node, *alpass_susp_type;


void alpass_n_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    alpass_susp_type susp = (alpass_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double feedback_reg;
    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "alpass_n_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        feedback_reg = susp->feedback;
        delayptr_reg = susp->delayptr;
        endptr_reg = susp->endptr;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register sample_type y, z;
            y = *delayptr_reg;
            *delayptr_reg++ = z = (sample_type) (feedback_reg * y + *input_ptr_reg++);
            *out_ptr_reg++ = (sample_type) (y - feedback_reg * z);
            if (delayptr_reg >= endptr_reg) delayptr_reg = susp->delaybuf;
        } while (--n); /* inner loop */

        susp->delayptr = delayptr_reg;
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
} /* alpass_n_fetch */


void alpass_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    alpass_susp_type susp = (alpass_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from input up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->input->t0) * susp->input->sr)) >=
           susp->input->current)
        susp_get_samples(input, input_ptr, input_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->input->t0) * susp->input->sr -
         (susp->input->current - susp->input_cnt));
    susp->input_ptr += n;
    susp_took(input_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void alpass_mark(snd_susp_type a_susp)
{
    alpass_susp_type susp = (alpass_susp_type) a_susp;
    sound_xlmark(susp->input);
}


void alpass_free(snd_susp_type a_susp)
{
    alpass_susp_type susp = (alpass_susp_type) a_susp;
free(susp->delaybuf);    sound_unref(susp->input);
    ffree_generic(susp, sizeof(alpass_susp_node), "alpass_free");
}


void alpass_print_tree(snd_susp_type a_susp, int n)
{
    alpass_susp_type susp = (alpass_susp_type) a_susp;
    indent(n);
    stdputstr("input:");
    sound_print_tree_1(susp->input, n);
}


sound_type snd_make_alpass(sound_type input, time_type delay, double feedback)
{
    register alpass_susp_type susp;
    rate_type sr = input->sr;
    time_type t0 = input->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    /* combine scale factors of linear inputs (INPUT) */
    scale_factor *= input->scale;
    input->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (input->sr < sr) { input->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, alpass_susp_node, "snd_make_alpass");
    susp->feedback = feedback;
    susp->delaylen = max(1, ROUND32(input->sr * delay));
    susp->delaybuf = (sample_type *) calloc (susp->delaylen, sizeof(sample_type));
    susp->delayptr = susp->delaybuf;
    susp->endptr = susp->delaybuf + susp->delaylen;
    susp->susp.fetch = alpass_n_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < input->t0) sound_prepend_zeros(input, t0);
    /* minimum start time over all inputs: */
    t0_min = min(input->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = alpass_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = alpass_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = alpass_mark;
    susp->susp.print_tree = alpass_print_tree;
    susp->susp.name = "alpass";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    susp->input = input;
    susp->input_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_alpass(sound_type input, time_type delay, double feedback)
{
    sound_type input_copy = sound_copy(input);
    return snd_make_alpass(input_copy, delay, feedback);
}
