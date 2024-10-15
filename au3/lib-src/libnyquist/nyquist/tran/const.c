#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "const.h"

void const_free(snd_susp_type a_susp);


typedef struct const_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    sample_type c;
} const_susp_node, *const_susp_type;


void const__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    const_susp_type susp = (const_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type c_reg;
    falloc_sample_block(out, "const__fetch");
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
        c_reg = susp->c;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = c_reg;
        } while (--n); /* inner loop */

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
} /* const__fetch */


void const_free(snd_susp_type a_susp)
{
    const_susp_type susp = (const_susp_type) a_susp;
    ffree_generic(susp, sizeof(const_susp_node), "const_free");
}


void const_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_const(double c, time_type t0, rate_type sr, time_type d)
{
    register const_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, const_susp_node, "snd_make_const");
    susp->c = (sample_type) c;
    susp->susp.fetch = const__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = const_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = const_print_tree;
    susp->susp.name = "const";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_const(double c, time_type t0, rate_type sr, time_type d)
{
    return snd_make_const(c, t0, sr, d);
}
