#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "osc.h"

void osc_free(snd_susp_type a_susp);


typedef struct osc_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    double ph_incr;
    table_type the_table;
    sample_type *table_ptr;
    double table_len;
    double phase;
} osc_susp_node, *osc_susp_type;


void osc__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    osc_susp_type susp = (osc_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double ph_incr_reg;
    register sample_type * table_ptr_reg;
    register double table_len_reg;
    register double phase_reg;
    falloc_sample_block(out, "osc__fetch");
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
        ph_incr_reg = susp->ph_incr;
        table_ptr_reg = susp->table_ptr;
        table_len_reg = susp->table_len;
        phase_reg = susp->phase;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            long table_index = (long) phase_reg;
            double x1 = table_ptr_reg[table_index];
            *out_ptr_reg++ = (sample_type) (x1 + (phase_reg - table_index) * 
                  (table_ptr_reg[table_index + 1] - x1));
            phase_reg += ph_incr_reg;
            while (phase_reg >= table_len_reg) phase_reg -= table_len_reg;
        } while (--n); /* inner loop */

        susp->phase = phase_reg;
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
} /* osc__fetch */


void osc_free(snd_susp_type a_susp)
{
    osc_susp_type susp = (osc_susp_type) a_susp;
    table_unref(susp->the_table);
    ffree_generic(susp, sizeof(osc_susp_node), "osc_free");
}


void osc_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_osc(sound_type input, double step, rate_type sr, double hz, time_type t0, time_type d, double phase)
{
    register osc_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, osc_susp_node, "snd_make_osc");
    susp->ph_incr = 0;
    susp->the_table = sound_to_table(input);
    susp->table_ptr = susp->the_table->samples;
    susp->table_len = susp->the_table->length;
    susp->phase = compute_phase(phase, step, (long) susp->table_len,
    input->sr, sr, hz, &susp->ph_incr);
    susp->susp.fetch = osc__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = osc_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = osc_print_tree;
    susp->susp.name = "osc";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_osc(sound_type input, double step, rate_type sr, double hz, time_type t0, time_type d, double phase)
{
    return snd_make_osc(input, step, sr, hz, t0, d, phase);
}
