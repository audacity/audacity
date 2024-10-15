#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "shape.h"

void shape_free(snd_susp_type a_susp);


typedef struct shape_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type sin;
    int sin_cnt;
    sample_block_values_type sin_ptr;

    double time_to_index;
    double origin;
    table_type the_table;
    sample_type *fcn_table;
    double table_len;
} shape_susp_node, *shape_susp_type;


void shape_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    shape_susp_type susp = (shape_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double time_to_index_reg;
    register double origin_reg;
    register sample_type * fcn_table_reg;
    register double table_len_reg;
    register sample_type sin_scale_reg = susp->sin->scale;
    register sample_block_values_type sin_ptr_reg;
    falloc_sample_block(out, "shape_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the sin input sample block: */
        susp_check_term_log_samples(sin, sin_ptr, sin_cnt);
        togo = min(togo, susp->sin_cnt);

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }


        /* don't run past logical stop time */
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
            int64_t to_stop = susp->susp.log_stop_cnt - (susp->susp.current + cnt);
            /* break if to_stop == 0 (we're at the logical stop)
             * AND cnt > 0 (we're not at the beginning of the
             * output block).
             */
            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break;
                    } else /* keep togo as is: since cnt == 0, we
                            * can set the logical stop flag on this
                            * output block
                            */
                        susp->logically_stopped = true;
                } else /* limit togo so we can start a new
                        * block at the LST
                        */
                    togo = (int) to_stop;
            }
        }

        n = togo;
        time_to_index_reg = susp->time_to_index;
        origin_reg = susp->origin;
        fcn_table_reg = susp->fcn_table;
        table_len_reg = susp->table_len;
        sin_ptr_reg = susp->sin_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            register double offset, x1;
            register long table_index;
            register double phase = (sin_scale_reg * *sin_ptr_reg++);
            if (phase > 1.0) phase = 1.0;
            else if (phase < -1.0) phase = -1.0;
            offset = (phase + origin_reg) * time_to_index_reg;
            table_index = (long) offset;
            if (table_index < 0) { 
                table_index = 0; 
                offset = 0;
            }
            if (table_index >= table_len_reg) {
                offset = table_len_reg - 1;
                table_index = (long) offset;
            }
            x1 = fcn_table_reg[table_index];
            *out_ptr_reg++ = (sample_type) (x1 + (offset - table_index) * 
                     (fcn_table_reg[table_index + 1] - x1));
        } while (--n); /* inner loop */

        susp->origin = origin_reg;
        /* using sin_ptr_reg is a bad idea on RS/6000: */
        susp->sin_ptr += togo;
        out_ptr += togo;
        susp_took(sin_cnt, togo);
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
    /* test for logical stop */
    if (susp->logically_stopped) {
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
        susp->logically_stopped = true;
    }
} /* shape_s_fetch */


void shape_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    shape_susp_type susp = (shape_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from sin up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->sin->t0) * susp->sin->sr)) >=
           susp->sin->current)
        susp_get_samples(sin, sin_ptr, sin_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->sin->t0) * susp->sin->sr -
         (susp->sin->current - susp->sin_cnt));
    susp->sin_ptr += n;
    susp_took(sin_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void shape_mark(snd_susp_type a_susp)
{
    shape_susp_type susp = (shape_susp_type) a_susp;
    sound_xlmark(susp->sin);
}


void shape_free(snd_susp_type a_susp)
{
    shape_susp_type susp = (shape_susp_type) a_susp;
table_unref(susp->the_table);
    sound_unref(susp->sin);
    ffree_generic(susp, sizeof(shape_susp_node), "shape_free");
}


void shape_print_tree(snd_susp_type a_susp, int n)
{
    shape_susp_type susp = (shape_susp_type) a_susp;
    indent(n);
    stdputstr("sin:");
    sound_print_tree_1(susp->sin, n);
}


sound_type snd_make_shape(sound_type sin, sound_type fn, double origin)
{
    register shape_susp_type susp;
    rate_type sr = sin->sr;
    time_type t0 = sin->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, shape_susp_node, "snd_make_shape");
    susp->time_to_index = fn->sr;
    susp->origin = origin;
    susp->the_table = sound_to_table(fn);
    susp->fcn_table = susp->the_table->samples;
    susp->table_len = susp->the_table->length;
    susp->susp.fetch = shape_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < sin->t0) sound_prepend_zeros(sin, t0);
    /* minimum start time over all inputs: */
    t0_min = min(sin->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = shape_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = shape_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = shape_mark;
    susp->susp.print_tree = shape_print_tree;
    susp->susp.name = "shape";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(sin);
    susp->susp.current = 0;
    susp->sin = sin;
    susp->sin_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_shape(sound_type sin, sound_type fn, double origin)
{
    sound_type sin_copy = sound_copy(sin);
    return snd_make_shape(sin_copy, fn, origin);
}
