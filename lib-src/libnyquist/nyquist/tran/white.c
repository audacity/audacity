#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "white.h"

void white_free(snd_susp_type a_susp);


typedef struct white_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
} white_susp_node, *white_susp_type;


void white__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    white_susp_type susp = (white_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    falloc_sample_block(out, "white__fetch");
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
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) (rand() * rand_scale - 1.0);
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
} /* white__fetch */


void white_free(snd_susp_type a_susp)
{
    white_susp_type susp = (white_susp_type) a_susp;
    ffree_generic(susp, sizeof(white_susp_node), "white_free");
}


void white_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_white(time_type t0, rate_type sr, time_type d)
{
    register white_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, white_susp_node, "snd_make_white");
    susp->susp.fetch = white__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = white_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = white_print_tree;
    susp->susp.name = "white";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_white(time_type t0, rate_type sr, time_type d)
{
    return snd_make_white(t0, sr, d);
}
