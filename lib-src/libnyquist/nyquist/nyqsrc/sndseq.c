/* sndseq.c -- return a signal until its logical stop, then
   evaluate a closure to get a signal and convert to an add
   of two signals */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "scale.h"
#include "add.h"
#include "extern.h"
#include "cext.h"
#include "assert.h"

#define SNDSEQDBG 0
#define D if (SNDSEQDBG) 

/* Note: this structure is identical to an add_susp structure up
   to the field output_per_s2 so that we can convert this into
   an add after eval'ing the closure.  Since this struct is bigger
   than an add, make sure not to clobber the "free" routine 
   (sndseq_free) or else we'll leak memory.
 */
typedef struct sndseq_susp_struct {
    snd_susp_node		susp;
    boolean			started;
    int                         terminate_bits;
    long			terminate_cnt;
    int                         logical_stop_bits;
    boolean			logically_stopped;
    sound_type			s1;
    long			s1_cnt;
    sample_block_type		s1_bptr;	/* block pointer */
    sample_block_values_type	s1_ptr;
    sound_type			s2;
    long			s2_cnt;
    sample_block_type		s2_bptr;	/* block pointer */
    sample_block_values_type	s2_ptr;

    /* support for interpolation of s2 */
    sample_type s2_x1_sample;
    double s2_phase;
    double s2_phase_incr;

    /* support for ramp between samples of s2 */
    double output_per_s2;

    /* sndseq-specific data starts here */
    LVAL closure;

} sndseq_susp_node, *sndseq_susp_type;


void sndseq_fetch(sndseq_susp_type, snd_list_type);
void sndseq_zero_fill_fetch(sndseq_susp_type, snd_list_type);
void sndseq_free();

extern LVAL s_stdout;

void sndseq_mark(sndseq_susp_type susp)
{
/*    nyquist_printf("sndseq_mark(%x)\n", susp);*/
/*    nyquist_printf("marking s1@%x in sndseq@%x\n", susp->s1, susp); */
    sound_xlmark(susp->s1);
    if (susp->closure) mark(susp->closure);
}



/* sndseq_fetch returns blocks of s1 until the logical stop time of s1 */
/**/
void sndseq_fetch(susp, snd_list)
  register sndseq_susp_type susp;
  snd_list_type snd_list;
{
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

/*    nyquist_printf("sndseq_fetch called: s1_cnt %d\n", susp->s1_cnt); */
    /*
     * first compute how many samples to copy (or transfer)
     */

    /* get next samples; in add, the call is:
     *   susp_check_term_log_block_samples(s1, s1_bptr, s1_ptr, s1_cnt, 1, 3);
     *
     * the plan here is tricky: if s1 has logically stopped, then evaluate
     * the closure to get signal s2.  Then convert sndseq into an add.
     */
    if (susp->s1_cnt == 0) {
        susp_get_block_samples(s1, s1_bptr, s1_ptr, s1_cnt);
        if (susp->s1_ptr == zero_block->samples) {
            susp->terminate_bits = 1;   /* mark s1 as terminated */
        }
/*	nyquist_printf("sndseq_fetch: s1-lsc %d, current %d cnt %d\n",
               susp->s1->logical_stop_cnt, susp->s1->current, susp->s1_cnt); */
    }

    if (susp->s1->logical_stop_cnt != UNKNOWN &&
        susp->s1->logical_stop_cnt == susp->s1->current - susp->s1_cnt) {
        time_type now = susp->susp.t0 + susp->susp.current / susp->susp.sr;
        /* note: cons args are protected from GC: */
        LVAL result;
        long delay;	/* sample delay to s2 */
/*	stats();gc();stats();*/

        xlsave1(result);

D      nyquist_printf("sndseq_fetch: about to eval closure at %g, "
                      "susp->susp.t0 %g, susp.current %d:\n",
                      now, susp->susp.t0, (int)susp->susp.current);
        result = xleval(cons(susp->closure, consa(cvflonum(now))));

        susp->logical_stop_bits = 1;   /* mark s1 as logically stopped */
        if (exttypep(result, a_sound)) {
            susp->s2 = sound_copy(getsound(result));
D           nyquist_printf("sndseq: copied result from closure is %p\n",
                           susp->s2);
        } else xlerror("closure did not return a (monophonic) sound", result);
D        nyquist_printf("in sndseq: logically stopped; "
                        "%p returned from evform\n",
                        susp->s2);
        susp->closure = NULL;   /* allow garbage collection now */
        result = NIL;

        /**** Now convert to add ****/
        susp->susp.mark = add_mark;
        susp->susp.log_stop_cnt = UNKNOWN; /* will be recomputed by add */
        susp->susp.print_tree = add_print_tree;

        /* assume sample rates are the same */
        if (susp->s1->sr != susp->s2->sr) 
            xlfail("in sndseq: sample rates must match");

        /* take care of scale factor, if any */
        if (susp->s2->scale != 1.0) {
            // stdputstr("normalizing next sound in a seq\n");
            susp->s2 = snd_make_normalize(susp->s2);
        }

        /* figure out which add fetch routine to use */
        delay = ROUND((susp->s2->t0 - now) * susp->s1->sr);
        if (susp->terminate_bits) {     /* s1 is done, just get s2 now */
            sound_unref(susp->s1);
            susp->s1 = NULL;
            if (delay > 0) {   /* need to fill zeros */
                susp->susp.fetch = add_zero_fill_nn_fetch;
                susp->susp.name = "sndseq:add_zero_fill_nn_fetch";
            } else {
                susp->susp.fetch = add_s2_nn_fetch;
                susp->susp.name = "sndseq:add_s2_nn_fetch";
            }		
        } else if (delay > 0) {    /* fill hole between s1 and s2 */
D	    stdputstr("using add_s1_nn_fetch\n");
            susp->susp.fetch = add_s1_nn_fetch;
            susp->susp.name = "sndseq:add_s1_nn_fetch";
        } else {
            susp->susp.fetch = add_s1_s2_nn_fetch;
            susp->susp.name = "sndseq:add_s1_s2_nn_fetch";
        }

        susp->s2_phase_incr = susp->s2->sr / susp->susp.sr;
        susp->output_per_s2 = susp->susp.sr / susp->s2->sr;

D        stdputstr("in sndseq: calling add's fetch\n");
        (*(susp->susp.fetch))(susp, snd_list);
D        stdputstr("in sndseq: returned from add's fetch\n");
/*	gc();*/
        xlpop();
        return;
    }

    /* don't run past the s1 input sample block: */
    togo = susp->s1_cnt;
/*    nyquist_printf("sndseq_fetch: togo initially %d then ", togo); */
    
    /* don't run past terminate time */
    if (susp->terminate_cnt != UNKNOWN &&
        susp->terminate_cnt <= susp->susp.current + togo) {
        togo = susp->terminate_cnt - susp->susp.current;
    }
    
    /* don't run past logical stop time */
    if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
        int to_stop = susp->susp.log_stop_cnt - susp->susp.current;
        togo = MIN(togo, to_stop);
    }
    assert(togo >= 0);

/*    nyquist_printf("%d\n", togo);*/
    /*
     * two cases: copy a partial block or manipulate pointers for copyless
     * transfer of whole block (may not be full block):
     *
     * copy partial block when:
     *   o samples begin in middle of block
     *   o stopping time is before end of block (when other signal splits
     *     the block for this signal).  This happens if the logical
     *     stop time was externally dictated and falls mid-block.
     * transfer (copyless) block when:
     *   o the block is of maximum size
     *   o the block is small due to logical stop time or termination time
     */
    if (susp->s1_ptr == susp->s1_bptr->samples && susp->s1_cnt == togo) {
        /*
         * we want to copy this whole block (starting at the beginning
         * and going to the rest of the block) -- just do pointers.
         */

        /* just fetch and pass blocks on */
/*	nyquist_printf("sndseq (s1_nn) %x starting uncopy, togo %d\n", susp, togo); */
        snd_list->block = susp->s1_bptr;
        /* the zero_block indicates termination, don't copy it! Use
         * internal_zero_block instead.  It is also filled with zeros,
         * but does not indicate termination.  We must check for zero_block
         * because the signal may have a logical stop time specified that
         * extends beyond its termination time.
         */
        if (snd_list->block == zero_block)
            snd_list->block = internal_zero_block;
        (snd_list->block->refcnt)++;
/*	nyquist_printf("sndseq (s1_nn) %x shared block %x\n", susp, susp->s1_bptr);*/

        susp_took(s1_cnt, togo);
        snd_list->block_len = togo;
    } else {
        /*
         * we want to copy a partial block
         */

        /* snd_list is the one with a null block */
        /* put a fresh, clean block in the snd_list (get new snd_list later) */
        falloc_sample_block(out, "sndseq_fetch");
        snd_list->block = out;
        out_ptr = out->samples;
        /* nyquist_printf("sndseq (s1_nn) %x new block %x\n", susp, out); */

        n = togo;
        /* nyquist_printf("sndseq (s1_nn) %x starting copy loop, togo %d\n", susp, togo); */
        while (n--) { /* the inner sample computation loop */
            /* scale? */
            *out_ptr++ = *(susp->s1_ptr++);
        } /* inner loop */

        susp_took(s1_cnt, togo);
        snd_list->block_len = togo;
    }

    /* add a new snd_list for the susp */
    susp->susp.current += togo;

} /* sndseq_fetch */


void sndseq_free(sndseq_susp_type susp)
{
    sound_unref(susp->s1);
    sound_unref(susp->s2);
    ffree_generic(susp, sizeof(sndseq_susp_node), "sndseq_free");
}


void sndseq_print_tree(sndseq_susp_type susp, int n)
{
    indent(n);
    stdputstr("s1:");
    sound_print_tree_1(susp->s1, n);

    indent(n);
    stdputstr("closure:");
    stdprint(susp->closure);

    indent(n);
    stdputstr("s2:");
    sound_print_tree_1(susp->s2, n);
}




sound_type snd_make_sndseq(s1, closure)
  sound_type s1;
  LVAL closure;
{
    register sndseq_susp_type susp;
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    sound_type result;

    xlprot1(closure);
    falloc_generic(susp, sndseq_susp_node, "snd_make_sndseq");

    if (s1->scale != 1.0) {
        /* stdputstr("normalizing first sound in a seq\n"); */
        s1 = snd_make_normalize(s1);
    }

    susp->susp.fetch = sndseq_fetch;

    susp->terminate_cnt = UNKNOWN;
    susp->terminate_bits = 0;   /* bits for s1 and s2 termination */
    susp->logical_stop_bits = 0;    /* bits for s1 and s2 logical stop */

    /* initialize susp state */
    susp->susp.free = sndseq_free;
    susp->susp.sr = s1->sr;
    susp->susp.t0 = s1->t0;
    susp->susp.mark = sndseq_mark;
    susp->susp.print_tree = sndseq_print_tree;
    susp->susp.name = "sndseq";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = s1->logical_stop_cnt;
    if (!(susp->susp.log_stop_cnt >= 0 || susp->susp.log_stop_cnt == UNKNOWN)) {
        xlerror("Behaviors in SEQ must appear in chronological order", closure);
    }
    susp->started = false;
    susp->susp.current = 0;
    susp->s1 = s1;
    susp->s1_cnt = 0;
    susp->s2 = NULL;
    susp->s2_cnt = 0;
    susp->s2_phase = 0.0;
/*  susp->s2_phase_incr = ??
    susp->output_per_s2 = ?? */
    susp->closure = closure;
    result = sound_create((snd_susp_type)susp, susp->susp.t0, susp->susp.sr, scale_factor);
    xlpopn(1);
    return result;
}


sound_type snd_sndseq(s1, closure)
  sound_type s1;
  LVAL closure;
{
    sound_type s1_copy;
    s1_copy = sound_copy(s1);
    return snd_make_sndseq(s1_copy, closure);
}
