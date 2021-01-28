#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "sine.h"

void sine_free(snd_susp_type a_susp);


typedef struct sine_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    long phase;
    long ph_incr;
} sine_susp_node, *sine_susp_type;


sample_type sine_table[SINE_TABLE_LEN + 1];

void sine_init()
{
    int i;
    for (i = 0; i <= SINE_TABLE_LEN; i++)
        sine_table[i] = (sample_type) (sin((PI * 2 * i) / SINE_TABLE_LEN));
}


void sine__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    sine_susp_type susp = (sine_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long phase_reg;
    register long ph_incr_reg;
    falloc_sample_block(out, "sine__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

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

        n = togo;
        phase_reg = susp->phase;
        ph_incr_reg = susp->ph_incr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = sine_table[phase_reg >> SINE_TABLE_SHIFT];
            phase_reg += ph_incr_reg;
            phase_reg &= SINE_TABLE_MASK;
        } while (--n); /* inner loop */

        susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK;
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
} /* sine__fetch */


void sine_free(snd_susp_type a_susp)
{
    sine_susp_type susp = (sine_susp_type) a_susp;
    ffree_generic(susp, sizeof(sine_susp_node), "sine_free");
}


void sine_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_sine(time_type t0, double hz, rate_type sr, time_type d)
{
    register sine_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, sine_susp_node, "snd_make_sine");
    susp->phase = 0;
    susp->ph_incr = ROUND32(((hz * SINE_TABLE_LEN) * (1 << SINE_TABLE_SHIFT) / sr));
    susp->susp.fetch = sine__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = sine_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = sine_print_tree;
    susp->susp.name = "sine";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_sine(time_type t0, double hz, rate_type sr, time_type d)
{
    return snd_make_sine(t0, hz, sr, d);
}
