#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "pwl.h"

void pwl_free(snd_susp_type a_susp);


typedef struct pwl_susp_struct {
    snd_susp_node susp;

    LVAL bpt_ptr;
    double incr;
    double lvl;
} pwl_susp_node, *pwl_susp_type;


/* IMPLEMENTATION NOTE:
 * The lis argument is a list of alternating sample numbers and values
 * which are taken in pairs, with an implicit (0, 0) starting point.  The
 * last point  is a sample number only, the value being implicitly 0.
 * The bpt_ptr points to the next sample number in the list.
 * The incr is set to the increment per sample, and lvl is the next
 * sample value.
 *
 * The list is assumed to be well-formed, so it should be checked by
 * the caller (users should not call this directly).
 */

char *pwl_bad_breakpoint_list = "bad breakpoint list";

/* compute_lvl -- setup the susp with level, advance bpt_ptr */
/*
 * returns true if it is time to terminate
 *
 * Note: compute_lvl gets called in the outer loop to skip over
 * a breakpoint pair before starting the compute_incr loop, which
 * searches for a breakpoint that is some number of samples in the
 * future.  This code is not embedded in compute_incr because it is
 * NOT called from the initialization, where it would be wrong to
 * skip over the first breakpoint.
 */
boolean compute_lvl(pwl_susp_type susp)
{
    LVAL lval = susp->bpt_ptr;
    if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
    lval = cdr(lval);
    if (!lval) return true;
    if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
    lval = car(lval);
    if (!floatp(lval)) xlfail(pwl_bad_breakpoint_list);
    susp->lvl = getflonum(lval);
    susp->bpt_ptr = cdr(cdr(susp->bpt_ptr));
    return !susp->bpt_ptr;
}


/* compute_incr -- setup the susp with level and increment */
/*
 * returns true if it is time to terminate
 */
boolean compute_incr(pwl_susp_type susp, int64_t *n, int64_t cur)
{
    double target;
    while (*n == 0) {
        LVAL lval = susp->bpt_ptr;
        if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
        lval = car(lval);
        if (!fixp(lval)) xlfail(pwl_bad_breakpoint_list);
        *n = getfixnum(lval) - cur;
        /* if there is a 2nd element of the pair, get the target */
        lval = cdr(susp->bpt_ptr);
        if (lval) {
            if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
            lval = car(lval);
            if (!floatp(lval)) xlfail(pwl_bad_breakpoint_list);
            target = getflonum(lval);
        } else target = 0.0;
        if (*n > 0) susp->incr = (target - susp->lvl) / *n;
        else if (compute_lvl(susp)) return true;
    }
    return false;
}


void pwl__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    pwl_susp_type susp = (pwl_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double incr_reg;
    register double lvl_reg;
    falloc_sample_block(out, "pwl__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;


        if (susp->bpt_ptr == NULL) {
out:	    togo = 0;	/* indicate termination */
            break;	/* we're done */
        }
        { int64_t cur = susp->susp.current + cnt;
          int64_t nn = getfixnum(car(susp->bpt_ptr)) - cur;
          if (nn == 0) {
              if (compute_lvl(susp) || compute_incr(susp, &nn, cur)) goto out;
          }
          togo = (int) min(nn, togo);
        }

        n = togo;
        incr_reg = susp->incr;
        lvl_reg = susp->lvl;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = (sample_type) lvl_reg; lvl_reg += incr_reg;
        } while (--n); /* inner loop */

        susp->lvl += susp->incr * togo;
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
} /* pwl__fetch */


void pwl_mark(snd_susp_type a_susp)
{
    pwl_susp_type susp = (pwl_susp_type) a_susp;
    if (susp->bpt_ptr) mark(susp->bpt_ptr);
}


void pwl_free(snd_susp_type a_susp)
{
    pwl_susp_type susp = (pwl_susp_type) a_susp;
    ffree_generic(susp, sizeof(pwl_susp_node), "pwl_free");
}


void pwl_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_pwl(time_type t0, rate_type sr, LVAL lis)
{
    register pwl_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, pwl_susp_node, "snd_make_pwl");
    susp->bpt_ptr = lis;
    susp->incr = 0.0;
    susp->lvl = 0.0; 
    { int64_t temp = 0; compute_incr(susp, &temp, 0); };
    susp->susp.fetch = pwl__fetch;

    /* initialize susp state */
    susp->susp.free = pwl_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = pwl_mark;
    susp->susp.print_tree = pwl_print_tree;
    susp->susp.name = "pwl";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_pwl(time_type t0, rate_type sr, LVAL lis)
{
    return snd_make_pwl(t0, sr, lis);
}
