#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "instrsitar.h"

void sitar_free(snd_susp_type a_susp);


typedef struct sitar_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    struct instr *mysitar;
    int temp_ret_value;
} sitar_susp_node, *sitar_susp_type;

#include "instr.h"


void sitar__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    sitar_susp_type susp = (sitar_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register struct instr * mysitar_reg;
    falloc_sample_block(out, "sitar__fetch");
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
        mysitar_reg = susp->mysitar;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) tick(mysitar_reg);
        } while (--n); /* inner loop */

        susp->mysitar = mysitar_reg;
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
} /* sitar__fetch */


void sitar_free(snd_susp_type a_susp)
{
    sitar_susp_type susp = (sitar_susp_type) a_susp;
    deleteInstrument(susp->mysitar);
    ffree_generic(susp, sizeof(sitar_susp_node), "sitar_free");
}


void sitar_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_sitar(time_type t0, double freq, time_type dur, rate_type sr)
{
    register sitar_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, sitar_susp_node, "snd_make_sitar");
    susp->mysitar = initInstrument(SITAR, ROUND32(sr));
    susp->temp_ret_value = noteOn(susp->mysitar, freq, 1.0);
    susp->susp.fetch = sitar__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((dur) * sr));
    /* initialize susp state */
    susp->susp.free = sitar_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = sitar_print_tree;
    susp->susp.name = "sitar";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_sitar(time_type t0, double freq, time_type dur, rate_type sr)
{
    return snd_make_sitar(t0, freq, dur, sr);
}
