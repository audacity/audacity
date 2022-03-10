#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "alpassvc.h"

void alpassvc_free(snd_susp_type a_susp);


typedef struct alpassvc_susp_struct {
    snd_susp_node susp;
    boolean started;
    int64_t terminate_cnt;
    sound_type input;
    int input_cnt;
    sample_block_values_type input_ptr;
    sound_type delaysnd;
    int delaysnd_cnt;
    sample_block_values_type delaysnd_ptr;

    /* support for interpolation of delaysnd */
    sample_type delaysnd_x1_sample;
    double delaysnd_pHaSe;
    double delaysnd_pHaSe_iNcR;

    /* support for ramp between samples of delaysnd */
    double output_per_delaysnd;
    int64_t delaysnd_n;

    float delay_scale_factor;
    double feedback;
    long buflen;
    sample_type *delaybuf;
    sample_type *delayptr;
    sample_type *endptr;
} alpassvc_susp_node, *alpassvc_susp_type;


void alpassvc_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register float delay_scale_factor_reg;
    register double feedback_reg;
    register long buflen_reg;
    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type delaysnd_ptr_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "alpassvc_nn_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

        /* don't run past the delaysnd input sample block: */
        susp_check_samples(delaysnd, delaysnd_ptr, delaysnd_cnt);
        togo = min(togo, susp->delaysnd_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        delay_scale_factor_reg = susp->delay_scale_factor;
        feedback_reg = susp->feedback;
        buflen_reg = susp->buflen;
        delayptr_reg = susp->delayptr;
        endptr_reg = susp->endptr;
        delaysnd_ptr_reg = susp->delaysnd_ptr;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register sample_type y, z, delaysamp;
            register int delayi;
            register sample_type *yptr;
            /* compute where to read y, we want y to be delay_snd samples
             * after delay_ptr, where we write the new sample. First, 
             * convert from seconds to samples. Note: don't use actual sound_type
             * names in comments! The translator isn't smart enough.
             */
            delaysamp = (sample_type) *delaysnd_ptr_reg++ * delay_scale_factor_reg;
            delayi = (int) delaysamp; /* get integer part */
            delaysamp = delaysamp - delayi; /* get phase */
            yptr = delayptr_reg + buflen_reg - (delayi + 1);
            if (yptr >= endptr_reg) yptr -= buflen_reg;
            /* now get y, the out-put of the delay, using interpolation */
            /* note that as phase increases, we use more of yptr[0] because
               positive phase means longer buffer means read earlier sample */
            y = (float) ((yptr[0] * delaysamp) + (yptr[1] * (1.0 - delaysamp)));
            /* WARNING: no check to keep delaysamp in range, so 
               do this in LISP */

            *delayptr_reg++ = z = (sample_type) (feedback_reg * y + *input_ptr_reg++);
            /* Time out to update the buffer:
             * this is a tricky buffer: buffer[0] == buffer[bufflen]
             * the logical length is bufflen, but the actual length
             * is bufflen + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */
            if (delayptr_reg > endptr_reg) {
                delayptr_reg = susp->delaybuf;
                *delayptr_reg++ = *endptr_reg;
            }
            *out_ptr_reg++ = (sample_type) (y - feedback_reg * z);
        } while (--n); /* inner loop */

        susp->buflen = buflen_reg;
        susp->delayptr = delayptr_reg;
        /* using delaysnd_ptr_reg is a bad idea on RS/6000: */
        susp->delaysnd_ptr += togo;
        /* using input_ptr_reg is a bad idea on RS/6000: */
        susp->input_ptr += togo;
        out_ptr += togo;
        susp_took(input_cnt, togo);
        susp_took(delaysnd_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* alpassvc_nn_fetch */


void alpassvc_ni_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type delaysnd_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register float delay_scale_factor_reg;
    register double feedback_reg;
    register long buflen_reg;
    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register double delaysnd_pHaSe_iNcR_rEg = susp->delaysnd_pHaSe_iNcR;
    register double delaysnd_pHaSe_ReG;
    register sample_type delaysnd_x1_sample_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "alpassvc_ni_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp_check_samples(delaysnd, delaysnd_ptr, delaysnd_cnt);
        susp->delaysnd_cnt--;
        susp->delaysnd_x1_sample = *(susp->delaysnd_ptr);
    }

    susp_check_samples(delaysnd, delaysnd_ptr, delaysnd_cnt);
    delaysnd_x2_sample = *(susp->delaysnd_ptr);

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
        delay_scale_factor_reg = susp->delay_scale_factor;
        feedback_reg = susp->feedback;
        buflen_reg = susp->buflen;
        delayptr_reg = susp->delayptr;
        endptr_reg = susp->endptr;
        delaysnd_pHaSe_ReG = susp->delaysnd_pHaSe;
        delaysnd_x1_sample_reg = susp->delaysnd_x1_sample;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register sample_type y, z, delaysamp;
            register int delayi;
            register sample_type *yptr;
            if (delaysnd_pHaSe_ReG >= 1.0) {
                delaysnd_x1_sample_reg = delaysnd_x2_sample;
                /* pick up next sample as delaysnd_x2_sample: */
                susp->delaysnd_ptr++;
                susp_took(delaysnd_cnt, 1);
                delaysnd_pHaSe_ReG -= 1.0;
                susp_check_samples_break(delaysnd, delaysnd_ptr, delaysnd_cnt, delaysnd_x2_sample);
            }
            /* compute where to read y, we want y to be delay_snd samples
             * after delay_ptr, where we write the new sample. First, 
             * convert from seconds to samples. Note: don't use actual sound_type
             * names in comments! The translator isn't smart enough.
             */
            delaysamp = (sample_type) 
                (delaysnd_x1_sample_reg * (1 - delaysnd_pHaSe_ReG) + delaysnd_x2_sample * delaysnd_pHaSe_ReG) * delay_scale_factor_reg;
            delayi = (int) delaysamp; /* get integer part */
            delaysamp = delaysamp - delayi; /* get phase */
            yptr = delayptr_reg + buflen_reg - (delayi + 1);
            if (yptr >= endptr_reg) yptr -= buflen_reg;
            /* now get y, the out-put of the delay, using interpolation */
            /* note that as phase increases, we use more of yptr[0] because
               positive phase means longer buffer means read earlier sample */
            y = (float) ((yptr[0] * delaysamp) + (yptr[1] * (1.0 - delaysamp)));
            /* WARNING: no check to keep delaysamp in range, so 
               do this in LISP */

            *delayptr_reg++ = z = (sample_type) (feedback_reg * y + *input_ptr_reg++);
            /* Time out to update the buffer:
             * this is a tricky buffer: buffer[0] == buffer[bufflen]
             * the logical length is bufflen, but the actual length
             * is bufflen + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */
            if (delayptr_reg > endptr_reg) {
                delayptr_reg = susp->delaybuf;
                *delayptr_reg++ = *endptr_reg;
            }
            *out_ptr_reg++ = (sample_type) (y - feedback_reg * z);
            delaysnd_pHaSe_ReG += delaysnd_pHaSe_iNcR_rEg;
        } while (--n); /* inner loop */

        togo -= n;
        susp->buflen = buflen_reg;
        susp->delayptr = delayptr_reg;
        susp->delaysnd_pHaSe = delaysnd_pHaSe_ReG;
        susp->delaysnd_x1_sample = delaysnd_x1_sample_reg;
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
} /* alpassvc_ni_fetch */


void alpassvc_nr_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    sample_type delaysnd_DeLtA;
    sample_type delaysnd_val;
    sample_type delaysnd_x2_sample;
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register float delay_scale_factor_reg;
    register double feedback_reg;
    register long buflen_reg;
    register sample_type * delayptr_reg;
    register sample_type * endptr_reg;
    register sample_block_values_type input_ptr_reg;
    falloc_sample_block(out, "alpassvc_nr_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* make sure sounds are primed with first values */
    if (!susp->started) {
        susp->started = true;
        susp->delaysnd_pHaSe = 1.0;
    }

    susp_check_samples(delaysnd, delaysnd_ptr, delaysnd_cnt);
    delaysnd_x2_sample = *(susp->delaysnd_ptr);

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the input input sample block: */
        susp_check_term_samples(input, input_ptr, input_cnt);
        togo = min(togo, susp->input_cnt);

        /* grab next delaysnd_x2_sample when phase goes past 1.0; */
        /* we use delaysnd_n (computed below) to avoid roundoff errors: */
        if (susp->delaysnd_n <= 0) {
            susp->delaysnd_x1_sample = delaysnd_x2_sample;
            susp->delaysnd_ptr++;
            susp_took(delaysnd_cnt, 1);
            susp->delaysnd_pHaSe -= 1.0;
            susp_check_samples(delaysnd, delaysnd_ptr, delaysnd_cnt);
            delaysnd_x2_sample = *(susp->delaysnd_ptr);
            /* delaysnd_n gets number of samples before phase exceeds 1.0: */
            susp->delaysnd_n = (int64_t) ((1.0 - susp->delaysnd_pHaSe) *
                                        susp->output_per_delaysnd);
        }
        togo = (int) min(togo, susp->delaysnd_n);
        delaysnd_DeLtA = (sample_type) ((delaysnd_x2_sample - susp->delaysnd_x1_sample) * susp->delaysnd_pHaSe_iNcR);
        delaysnd_val = (sample_type) (susp->delaysnd_x1_sample * (1.0 - susp->delaysnd_pHaSe) +
                 delaysnd_x2_sample * susp->delaysnd_pHaSe);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        delay_scale_factor_reg = susp->delay_scale_factor;
        feedback_reg = susp->feedback;
        buflen_reg = susp->buflen;
        delayptr_reg = susp->delayptr;
        endptr_reg = susp->endptr;
        input_ptr_reg = susp->input_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register sample_type y, z, delaysamp;
            register int delayi;
            register sample_type *yptr;
            /* compute where to read y, we want y to be delay_snd samples
             * after delay_ptr, where we write the new sample. First, 
             * convert from seconds to samples. Note: don't use actual sound_type
             * names in comments! The translator isn't smart enough.
             */
            delaysamp = (sample_type) delaysnd_val * delay_scale_factor_reg;
            delayi = (int) delaysamp; /* get integer part */
            delaysamp = delaysamp - delayi; /* get phase */
            yptr = delayptr_reg + buflen_reg - (delayi + 1);
            if (yptr >= endptr_reg) yptr -= buflen_reg;
            /* now get y, the out-put of the delay, using interpolation */
            /* note that as phase increases, we use more of yptr[0] because
               positive phase means longer buffer means read earlier sample */
            y = (float) ((yptr[0] * delaysamp) + (yptr[1] * (1.0 - delaysamp)));
            /* WARNING: no check to keep delaysamp in range, so 
               do this in LISP */

            *delayptr_reg++ = z = (sample_type) (feedback_reg * y + *input_ptr_reg++);
            /* Time out to update the buffer:
             * this is a tricky buffer: buffer[0] == buffer[bufflen]
             * the logical length is bufflen, but the actual length
             * is bufflen + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */
            if (delayptr_reg > endptr_reg) {
                delayptr_reg = susp->delaybuf;
                *delayptr_reg++ = *endptr_reg;
            }
            *out_ptr_reg++ = (sample_type) (y - feedback_reg * z);
            delaysnd_val += delaysnd_DeLtA;
        } while (--n); /* inner loop */

        susp->buflen = buflen_reg;
        susp->delayptr = delayptr_reg;
        /* using input_ptr_reg is a bad idea on RS/6000: */
        susp->input_ptr += togo;
        out_ptr += togo;
        susp_took(input_cnt, togo);
        susp->delaysnd_pHaSe += togo * susp->delaysnd_pHaSe_iNcR;
        susp->delaysnd_n -= togo;
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* alpassvc_nr_fetch */


void alpassvc_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from input up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->input->t0) * susp->input->sr)) >=
           susp->input->current)
        susp_get_samples(input, input_ptr, input_cnt);
    /* fetch samples from delaysnd up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->delaysnd->t0) * susp->delaysnd->sr)) >=
           susp->delaysnd->current)
        susp_get_samples(delaysnd, delaysnd_ptr, delaysnd_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->input->t0) * susp->input->sr -
         (susp->input->current - susp->input_cnt));
    susp->input_ptr += n;
    susp_took(input_cnt, n);
    n = (int) ROUNDBIG((final_time - susp->delaysnd->t0) * susp->delaysnd->sr -
         (susp->delaysnd->current - susp->delaysnd_cnt));
    susp->delaysnd_ptr += n;
    susp_took(delaysnd_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void alpassvc_mark(snd_susp_type a_susp)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
    sound_xlmark(susp->input);
    sound_xlmark(susp->delaysnd);
}


void alpassvc_free(snd_susp_type a_susp)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
free(susp->delaybuf);
    sound_unref(susp->input);
    sound_unref(susp->delaysnd);
    ffree_generic(susp, sizeof(alpassvc_susp_node), "alpassvc_free");
}


void alpassvc_print_tree(snd_susp_type a_susp, int n)
{
    alpassvc_susp_type susp = (alpassvc_susp_type) a_susp;
    indent(n);
    stdputstr("input:");
    sound_print_tree_1(susp->input, n);

    indent(n);
    stdputstr("delaysnd:");
    sound_print_tree_1(susp->delaysnd, n);
}


sound_type snd_make_alpassvc(sound_type input, sound_type delaysnd, double feedback, double maxdelay)
{
    register alpassvc_susp_type susp;
    rate_type sr = input->sr;
    time_type t0 = max(input->t0, delaysnd->t0);
    int interp_desc = 0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    /* combine scale factors of linear inputs (INPUT) */
    scale_factor *= input->scale;
    input->scale = 1.0F;

    /* try to push scale_factor back to a low sr input */
    if (input->sr < sr) { input->scale = scale_factor; scale_factor = 1.0F; }

    falloc_generic(susp, alpassvc_susp_node, "snd_make_alpassvc");
    susp->delay_scale_factor = (float) (input->sr * delaysnd->scale);
    susp->feedback = feedback;
    susp->buflen = max(2, (long) (input->sr * maxdelay + 2.5));
    susp->delaybuf = (sample_type *) calloc (susp->buflen + 1, sizeof(sample_type));
    susp->delayptr = susp->delaybuf;
    susp->endptr = susp->delaybuf + susp->buflen;

    /* make sure no sample rate is too high */
    if (delaysnd->sr > sr) {
        sound_unref(delaysnd);
        snd_badsr();
    }

    /* select a susp fn based on sample rates */
    interp_desc = (interp_desc << 2) + interp_style(input, sr);
    interp_desc = (interp_desc << 2) + interp_style(delaysnd, sr);
    switch (interp_desc) {
      case INTERP_ns: /* handled below */
      case INTERP_nn: susp->susp.fetch = alpassvc_nn_fetch; break;
      case INTERP_ni: susp->susp.fetch = alpassvc_ni_fetch; break;
      case INTERP_nr: susp->susp.fetch = alpassvc_nr_fetch; break;
      default: snd_badsr(); break;
    }

    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < input->t0) sound_prepend_zeros(input, t0);
    if (t0 < delaysnd->t0) sound_prepend_zeros(delaysnd, t0);
    /* minimum start time over all inputs: */
    t0_min = min(input->t0, min(delaysnd->t0, t0));
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = alpassvc_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = alpassvc_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = alpassvc_mark;
    susp->susp.print_tree = alpassvc_print_tree;
    susp->susp.name = "alpassvc";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->started = false;
    susp->susp.current = 0;
    susp->input = input;
    susp->input_cnt = 0;
    susp->delaysnd = delaysnd;
    susp->delaysnd_cnt = 0;
    susp->delaysnd_pHaSe = 0.0;
    susp->delaysnd_pHaSe_iNcR = delaysnd->sr / sr;
    susp->delaysnd_n = 0;
    susp->output_per_delaysnd = sr / delaysnd->sr;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_alpassvc(sound_type input, sound_type delaysnd, double feedback, double maxdelay)
{
    sound_type input_copy = sound_copy(input);
    sound_type delaysnd_copy = sound_copy(delaysnd);
    return snd_make_alpassvc(input_copy, delaysnd_copy, feedback, maxdelay);
}
