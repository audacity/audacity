#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrmodalbar.h"

void modalbar_free(snd_susp_type a_susp);


typedef struct modalbar_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    struct instr *mymbar;
    int temp_ret_value;
} modalbar_susp_node, *modalbar_susp_type;

#include "instr.h"


void modalbar__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    modalbar_susp_type susp = (modalbar_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * mymbar_reg;
    falloc_sample_block(out, "modalbar__fetch");
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
        mymbar_reg = susp->mymbar;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) tick(mymbar_reg);
        } while (--n); /* inner loop */

        susp->mymbar = mymbar_reg;
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
} /* modalbar__fetch */


void modalbar_free(snd_susp_type a_susp)
{
    modalbar_susp_type susp = (modalbar_susp_type) a_susp;
    deleteInstrument(susp->mymbar);
    ffree_generic(susp, sizeof(modalbar_susp_node), "modalbar_free");
}


void modalbar_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_modalbar(time_type t0, double freq, int preset, time_type dur, rate_type sr)
{
    register modalbar_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, modalbar_susp_node, "snd_make_modalbar");
    susp->mymbar = initInstrument(MODALBAR, ROUND32(sr));
                  controlChange(susp->mymbar, 16, preset);;
    susp->temp_ret_value = noteOn(susp->mymbar, freq, 1.0);;
    susp->susp.fetch = modalbar__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((dur) * sr));
    /* initialize susp state */
    susp->susp.free = modalbar_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = modalbar_print_tree;
    susp->susp.name = "modalbar";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_modalbar(time_type t0, double freq, int preset, time_type dur, rate_type sr)
{
    return snd_make_modalbar(t0, freq, preset, dur, sr);
}
