#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "allpoles.h"

void allpoles_free(snd_susp_type a_susp);


typedef struct allpoles_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;
    boolean logically_stopped;
    sound_type x_snd;
    int x_snd_cnt;
    sample_block_values_type x_snd_ptr;

    long ak_len;
    LVAL ak_array;
    double gain;
    double *ak_coefs;
    double *zk_buf;
    long index;
} allpoles_susp_node, *allpoles_susp_type;


void allpoles_s_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    allpoles_susp_type susp = (allpoles_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register long ak_len_reg;
    register double gain_reg;
    register double * ak_coefs_reg;
    register double * zk_buf_reg;
    register long index_reg;
    register sample_type x_snd_scale_reg = susp->x_snd->scale;
    register sample_block_values_type x_snd_ptr_reg;
    falloc_sample_block(out, "allpoles_s_fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past the x_snd input sample block: */
        susp_check_term_log_samples(x_snd, x_snd_ptr, x_snd_cnt);
        togo = min(togo, susp->x_snd_cnt);

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


      if (susp->ak_array == NULL) {
             togo = 0; /* indicate termination */
             break;    /* we're done */
      }
      else if (!vectorp(susp->ak_array))
           xlerror("array expected", susp->ak_array);
      else if (susp->ak_coefs == NULL)
           {
               long i;
               susp->ak_len = getsize(susp->ak_array);
               if (susp->ak_len < 1) xlerror("array has not elements", susp->ak_array);
               susp->ak_coefs = (double *) calloc(susp->ak_len, sizeof(double));
               susp->zk_buf   = (double *) calloc(susp->ak_len, sizeof(double));
 
              /* at this point we have a new array and a place to put ak coefs */
              for(i=0; i < susp->ak_len; i++) {
                 LVAL elem = getelement(susp->ak_array,i);
                 if (ntype(elem) != FLONUM) {
                      xlerror("flonum expected", elem);
                 }
                 susp->ak_coefs[i] = getflonum(elem);
              }

            }

        n = togo;
        ak_len_reg = susp->ak_len;
        gain_reg = susp->gain;
        ak_coefs_reg = susp->ak_coefs;
        zk_buf_reg = susp->zk_buf;
        index_reg = susp->index;
        x_snd_ptr_reg = susp->x_snd_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            double z0; long xi; long xj;            z0 = (x_snd_scale_reg * *x_snd_ptr_reg++)*gain_reg;
            for (xi=0; xi < ak_len_reg ; xi++) {
                xj = index_reg + xi; if (xj >= ak_len_reg) xj -= ak_len_reg;
                z0 +=  ak_coefs_reg[xi] * zk_buf_reg[xj];
            }
            zk_buf_reg[index_reg] = z0;
            index_reg++; if (index_reg == ak_len_reg) index_reg = 0;
            *out_ptr_reg++ = (sample_type) z0;
        } while (--n); /* inner loop */

        susp->zk_buf = zk_buf_reg;
        susp->index = index_reg;
        /* using x_snd_ptr_reg is a bad idea on RS/6000: */
        susp->x_snd_ptr += togo;
        out_ptr += togo;
        susp_took(x_snd_cnt, togo);
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
} /* allpoles_s_fetch */


void allpoles_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    allpoles_susp_type susp = (allpoles_susp_type) a_susp;
    time_type final_time = susp->susp.t0;
    int n;

    /* fetch samples from x_snd up to final_time for this block of zeros */
    while ((ROUNDBIG((final_time - susp->x_snd->t0) * susp->x_snd->sr)) >=
           susp->x_snd->current)
        susp_get_samples(x_snd, x_snd_ptr, x_snd_cnt);
    /* convert to normal processing when we hit final_count */
    /* we want each signal positioned at final_time */
    n = (int) ROUNDBIG((final_time - susp->x_snd->t0) * susp->x_snd->sr -
         (susp->x_snd->current - susp->x_snd_cnt));
    susp->x_snd_ptr += n;
    susp_took(x_snd_cnt, n);
    susp->susp.fetch = susp->susp.keep_fetch;
    (*(susp->susp.fetch))(a_susp, snd_list);
}


void allpoles_mark(snd_susp_type a_susp)
{
    allpoles_susp_type susp = (allpoles_susp_type) a_susp;
    if (susp->ak_array) mark(susp->ak_array);
    sound_xlmark(susp->x_snd);
}


void allpoles_free(snd_susp_type a_susp)
{
    allpoles_susp_type susp = (allpoles_susp_type) a_susp;

     free(susp->zk_buf);
     free(susp->ak_coefs);
     susp->ak_array = NULL;  /* free array */
    sound_unref(susp->x_snd);
    ffree_generic(susp, sizeof(allpoles_susp_node), "allpoles_free");
}


void allpoles_print_tree(snd_susp_type a_susp, int n)
{
    allpoles_susp_type susp = (allpoles_susp_type) a_susp;
    indent(n);
    stdputstr("x_snd:");
    sound_print_tree_1(susp->x_snd, n);
}


sound_type snd_make_allpoles(sound_type x_snd, LVAL ak_array, double gain)
{
    register allpoles_susp_type susp;
    rate_type sr = x_snd->sr;
    time_type t0 = x_snd->t0;
    sample_type scale_factor = 1.0F;
    time_type t0_min = t0;
    falloc_generic(susp, allpoles_susp_node, "snd_make_allpoles");
    susp->ak_len = 0;
    susp->ak_array = ak_array;
    susp->gain = gain;
    susp->ak_coefs = NULL;
    susp->zk_buf = NULL;
    susp->index = 0;
    susp->susp.fetch = allpoles_s_fetch;
    susp->terminate_cnt = UNKNOWN;
    /* handle unequal start times, if any */
    if (t0 < x_snd->t0) sound_prepend_zeros(x_snd, t0);
    /* minimum start time over all inputs: */
    t0_min = min(x_snd->t0, t0);
    /* how many samples to toss before t0: */
    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + 0.5);
    if (susp->susp.toss_cnt > 0) {
        susp->susp.keep_fetch = susp->susp.fetch;
        susp->susp.fetch = allpoles_toss_fetch;
    }

    /* initialize susp state */
    susp->susp.free = allpoles_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = allpoles_mark;
    susp->susp.print_tree = allpoles_print_tree;
    susp->susp.name = "allpoles";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(x_snd);
    susp->susp.current = 0;
    susp->x_snd = x_snd;
    susp->x_snd_cnt = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_allpoles(sound_type x_snd, LVAL ak_array, double gain)
{
    sound_type x_snd_copy = sound_copy(x_snd);
    return snd_make_allpoles(x_snd_copy, ak_array, gain);
}
