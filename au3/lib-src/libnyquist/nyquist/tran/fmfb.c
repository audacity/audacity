#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "fmfb.h"

void fmfb_free(snd_susp_type a_susp);


typedef struct fmfb_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    double yy;
    double sin_y;
    double xx;
    double x_incr;
    double index;
} fmfb_susp_node, *fmfb_susp_type;


void fmfb__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    fmfb_susp_type susp = (fmfb_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double yy_reg;
    register double sin_y_reg;
    register double xx_reg;
    register double x_incr_reg;
    register double index_reg;
    falloc_sample_block(out, "fmfb__fetch");
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
        yy_reg = susp->yy;
        sin_y_reg = susp->sin_y;
        xx_reg = susp->xx;
        x_incr_reg = susp->x_incr;
        index_reg = susp->index;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            xx_reg += x_incr_reg;
            if (xx_reg > SINE_TABLE_LEN) xx_reg -= SINE_TABLE_LEN;
            /* xx_reg incremented and index_reg scaled to table index_reg, and
               sin_y_reg is a signal (-1 to +1) */
            yy_reg = xx_reg + index_reg * sin_y_reg;
            /* so yy_reg is a table index_reg */
            while (yy_reg > SINE_TABLE_LEN) yy_reg -= SINE_TABLE_LEN;
            while (yy_reg < 0) yy_reg += SINE_TABLE_LEN;
            sin_y_reg = sine_table[(int) yy_reg]; /* truncation gets valid index_reg */
            /* sin_y_reg is now a signal not ready for table lookup */
            *out_ptr_reg++ = (sample_type) sin_y_reg;
        } while (--n); /* inner loop */

        susp->yy = yy_reg;
        susp->sin_y = sin_y_reg;
        susp->xx = xx_reg;
        susp->index = index_reg;
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
} /* fmfb__fetch */


void fmfb_free(snd_susp_type a_susp)
{
    fmfb_susp_type susp = (fmfb_susp_type) a_susp;
    ffree_generic(susp, sizeof(fmfb_susp_node), "fmfb_free");
}


void fmfb_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_fmfb(time_type t0, double hz, rate_type sr, double index, time_type d)
{
    register fmfb_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, fmfb_susp_node, "snd_make_fmfb");
    susp->yy = 0.0;
    susp->sin_y = 0.0;
    susp->xx = 0.0;
    susp->x_incr = hz * SINE_TABLE_LEN / sr;
    susp->index = index * SINE_TABLE_LEN / PI2;
    susp->susp.fetch = fmfb__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = fmfb_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = fmfb_print_tree;
    susp->susp.name = "fmfb";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_fmfb(time_type t0, double hz, rate_type sr, double index, time_type d)
{
    return snd_make_fmfb(t0, hz, sr, index, d);
}
