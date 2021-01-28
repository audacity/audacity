#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrmandolin.h"

void mandolin_free(snd_susp_type a_susp);


typedef struct mandolin_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    struct instr *mymand;
    int temp_ret_value;
} mandolin_susp_node, *mandolin_susp_type;

#include "instr.h"


void mandolin__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    mandolin_susp_type susp = (mandolin_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * mymand_reg;
    falloc_sample_block(out, "mandolin__fetch");
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
        mymand_reg = susp->mymand;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) tick(mymand_reg);
        } while (--n); /* inner loop */

        susp->mymand = mymand_reg;
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
} /* mandolin__fetch */


void mandolin_free(snd_susp_type a_susp)
{
    mandolin_susp_type susp = (mandolin_susp_type) a_susp;
    deleteInstrument(susp->mymand);
    ffree_generic(susp, sizeof(mandolin_susp_node), "mandolin_free");
}


void mandolin_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_mandolin(time_type t0, double freq, time_type d, double body_size, double detune, rate_type sr)
{
    register mandolin_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, mandolin_susp_node, "snd_make_mandolin");
    susp->mymand = initInstrument(MANDOLIN, ROUND32(sr));
      controlChange(susp->mymand, 1, detune);
      controlChange(susp->mymand, 2, MAND_CONTROL_CHANGE_CONST * body_size);;
    susp->temp_ret_value = noteOn(susp->mymand, freq, 1.0);
    susp->susp.fetch = mandolin__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = mandolin_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = mandolin_print_tree;
    susp->susp.name = "mandolin";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_mandolin(time_type t0, double freq, time_type d, double body_size, double detune, rate_type sr)
{
    return snd_make_mandolin(t0, freq, d, body_size, detune, sr);
}
