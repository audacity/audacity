/* add.c -- add two signals */
/* CHANGE LOG
 * 19May92 rbd  fix t0 to mean time rather than samples
                fix to logically stop and terminate at MAX of 2 inputs
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

/* DOCUMENTATION:
     Most DSP modules in Nyquist select a single fetch routine
and use it until the signal terminates.  The ADD operation
instead can use a number of different fetch routines in sequence.
This allows ADD to do the most efficient computation, such as
simply copying pointers when only one input signal is defined
(the other is zero.)
     Here's what the functions assume and do:
add_s1_s2_nn_fetch: both arguments (s1, s2) have signals; add
    them.
add_s1_nn_fetch: only s1 is active, so pass along pointers if 
    possible.  Revert to add_s1_s2_nn_fetch when s2 becomes active.
add_s2_nn_fetch: symetric with add_s1_nn_fetch.
add_zero_fill_nn_fetch: fill in when one input has terminated and
    the other hasn't begun.

An important optimization (we think) is the ability to collapse
ADD operations.  When one operand goes to zero, the ADD just
passes along pointers to blocks from the other operand.  In some
cases, we can just splice out the ADD suspension and link
directly to the suspension of the second operand.

Doing this requires that there be no scale factors, so ADD does
not deal with scaling.  If an operand comes in with a scale
factor, ADD will create a rescaling of the operand.
*/

#include "switches.h"
#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "cext.h"
#include "scale.h"
#include "multiseq.h"
#include "add.h"
#include "assert.h"

#pragma warning(disable: 4068) // unknown pragma (MSVS)
#pragma clang diagnostic ignored "-Wunreachable-code"

#define debugA 0
#define A if (debugA)
/* I don't know how these debug switches (A and D) differ: */
#define D if (debugA)

/* switch B is/was to look for a particular zero block length bug */
#define debugB debugA
#define B if (debugB)

/* #define GC_DEBUG 1 */


void add_s1_s2_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    add_susp_type susp = (add_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;
    register sample_block_values_type s1_ptr_reg;
    register sample_block_values_type s2_ptr_reg;
    register sample_block_values_type out_ptr_reg;

#ifdef GC_DEBUG
    snd_list_report(snd_list, "add_s1_s2_nn_fetch");
#endif
    /* assume the snd_list is the one with a null block */
    /* put a fresh, clean block in the snd_list (get new snd_list later) */
    falloc_sample_block(out, "add_s1_s2_nn_fetch");
    snd_list->block = out;
    out_ptr = out->samples;
A   nyquist_printf("add[%p,%p] (s1_s2_nn) %p new block %p\n",
                   susp->s1, susp->s2, susp, out);
    
    /* fill up the new block */
    while (cnt < max_sample_block_len && susp->terminate_bits == 0) {
A       nyquist_printf("add[%p,%p] (s1_s2_nn) %p starting outer loop, cnt %d\n",
               susp->s1, susp->s2, susp, cnt);
        
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;
        
        /* don't run past the s1 input sample block: */
A	nyquist_printf("add[%p,%p]: look for samples (for s1) \n", susp->s1, susp->s2);
/*        if (!susp->s1->list->block) watch_susp(susp->s1->list->u.susp); */
        susp_check_term_log_block_samples(s1, s1_bptr, s1_ptr, s1_cnt, 1, 3);
A       nyquist_printf("add[%p,%p]: found samples (for s1) s1_cnt=%d\n",
               susp->s1, susp->s2, (int)susp->s1_cnt);
        togo = MIN(togo, susp->s1_cnt);
        if (susp->terminate_bits & 1) {
A	    nyquist_printf("add[%p,%p]: terminate bits on (for s1) togo=%d\n",
                   susp->s1, susp->s2, togo);
        }
        
        /* don't run past the s2 input sample block: */
A       nyquist_printf("add[%p,%p]: look for samples (for s2) \n", susp->s1, susp->s2);
        susp_check_term_log_block_samples(s2, s2_bptr, s2_ptr, s2_cnt, 2, 3);
A       nyquist_printf("add[%p,%p]: found samples (for s2) s2_cnt=%d\n",
               susp->s1, susp->s2, (int)susp->s2_cnt);
        togo = MIN(togo, susp->s2_cnt);
A       if (susp->terminate_bits & 2) {
            nyquist_printf("add[%p,%p]: terminate bits on (for s2) togo=%d\n",
                   susp->s1, susp->s2, togo);
        }
        
        /* don't run past logical stop time (need to check this even
         * if a sound has terminated)
         */
A	nyquist_printf("add[%p,%p] (s1_s2_nn) %p: logically_stopped %d, "
            "logical_stop_cnt %d, s1 logical_stop_cnt %" PRId64
            ", s2 logical_stop_cnt %" PRId64 "\n",
            susp->s1, susp->s2, susp, susp->logically_stopped,
            (int) susp->susp.log_stop_cnt,
            susp->s1->logical_stop_cnt, susp->s2->logical_stop_cnt);
        if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN &&
            (susp->logical_stop_bits == 3)) {
            int to_stop = (int) (susp->susp.log_stop_cnt -
                                 (susp->susp.current + cnt));
A	        nyquist_printf("add[%p,%p]: to_stop = %d\n", susp->s1, susp->s2,
                           to_stop);
            /* logical stops have to be indicated on block boundaries */
            if (to_stop < togo) {
                if (to_stop == 0) {
                    if (cnt) {
                        togo = 0;
                        break; /* block is non-empty, log-stop on next block */
                    } else /* to_stop is 0, indicate logical stop immediately */
                        susp->logically_stopped = true;
                } else {
                    /* logical stop will take place on the following block,
                     * so compute up to logical stop and return partial block
                     */
                    togo = to_stop;
                }
            }
        }
        
        /* check please */
        if (susp->terminate_bits) {
            break;
        }
        
        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt -
                          (susp->susp.current + cnt));
D	    nyquist_printf("add[%p,%p]: togo = %d\n", susp->s1, susp->s2, togo);
            if (togo == 0) break;
        }
        
        n = togo;
A	nyquist_printf("add[%p,%p] (s1_s2_nn) %p starting inner loop, n %d\n", 
               susp->s1, susp->s2, susp, n);
        s1_ptr_reg = susp->s1_ptr;
        s2_ptr_reg = susp->s2_ptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            /* scale? */
/*A 	    nyquist_printf("add_s1_s2_nn: %g + %g\n", *s1_ptr_reg, *s2_ptr_reg); */
            *out_ptr_reg++ = *(s1_ptr_reg++) + *(s2_ptr_reg++);
        } while (--n); /* inner loop */
        /* using s1_ptr_reg is a bad idea on RS/6000 */
        susp->s1_ptr += togo;
        /* using s2_ptr_reg is a bad idea on RS/6000 */
        susp->s2_ptr += togo;
        /* using out_ptr_reg is a bad idea on RS/6000 */
        out_ptr += togo;
        susp_took(s1_cnt, togo);
        susp_took(s2_cnt, togo);
        cnt += togo;
    } /* outer loop */
    
A   nyquist_printf("add[%p,%p] (s1_s2_nn) %p ending outer loop, cnt %d\n",
           susp->s1, susp->s2, susp, cnt);
    
    snd_list->block_len = cnt;
    
    /* test for logical stop  - normally this is detected by 
     * susp.log_stop_cnt == susp->susp.current, but then the logical
     * stop flag is set on the NEXT block. To remember to set on the
     * NEXT block, set susp->logically_stopped, which is also tested
     * below.  One special case is if the current block should indicate
     * logically stopped (this happens sometimes when the sounds have
     * zero logical length) then susp->logically_stopped will be set
     * (see above) and we just never test susp->susp.log_stop_cnt.
     */
    if (susp->logically_stopped) {
A   nyquist_printf("add[%p,%p] (s1_s2_nn) %p->logically_stopped already true\n",
           susp->s1, susp->s2, susp);
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current &&
               susp->logical_stop_bits == 3) {
A   nyquist_printf("add[%p,%p] (s1_s2_nn) %p->logically_stopped set to true\n",
           susp->s1, susp->s2, susp);
        susp->logically_stopped = true;
    }
    
    /* test for termination of s1 */
    if (susp->terminate_bits == 3) {
D	nyquist_printf("add[%p,%p] (s1_s2_nn) s1 and s2 terminated, unrefed\n",
               susp->s1, susp->s2);
        /* free susp and point to terminal zeros (leaving pending snd_lists)*/
        if (cnt) {
            /* we have samples, put zero_block at end */
            snd_list_unref(snd_list->u.next);
            snd_list->u.next = zero_snd_list;
        } else {
            /* no samples generated */
            snd_list_terminate(snd_list);
        }
D	nyquist_printf("add[%p,%p] (s1_s2_nn) %p terminated.\n",
               susp->s1, susp->s2, susp);
    } else {
        if (susp->terminate_bits & 1) {
D	    nyquist_printf("add[%p,%p] (s1_s2_nn) s1 terminated, unrefed\n",
                   susp->s1, susp->s2);
            sound_unref(susp->s1);
            susp->s1 = NULL;
            susp->susp.fetch = add_s2_nn_fetch;
D           nyquist_printf("add_s1_s2_nn_fetch: add_s2_nn_fetch installed\n");
            if (cnt == 0) {
D		nyquist_printf("add[%p,%p]: calling add_s2_nn_fetch\n",
                       susp->s1, susp->s2);
                add_s2_nn_fetch(a_susp, snd_list);
            }
        }
        else if (susp->terminate_bits & 2) {
D	    nyquist_printf("add[%p,%p] (s1_s2_nn) s2 terminated, unrefed\n",
                   susp->s1, susp->s2);
            sound_unref(susp->s2);
            susp->s2 = NULL;
            susp->susp.fetch = add_s1_nn_fetch;
D           stdputstr("add_s1_s2_nn_fetch: add_s1_nn_fetch installed\n");
            if (cnt == 0) {
D		nyquist_printf("add[%p,%p]: calling add_s1_nn_fetch\n",
                       susp->s1, susp->s2);
                add_s1_nn_fetch(a_susp, snd_list);
            }
        }
        
        /* add a new snd_list for the susp */
        susp->susp.current += cnt;
    }

} /* add_s1_s2_nn_fetch */



/* Note that add_s1_nn_fetch and add_s2_nn_fetch are symetric.
 * They should probably be made into one routine, but for now,
 * any changes to one should be made to the other.
 */
void add_s1_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    add_susp_type susp = (add_susp_type) a_susp;
    /* expansion of add_s_nn_fetch(snd_list,s1,s2,1); follows: */
    int togo;
    int64_t s2_start = 0;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

D   nyquist_printf("add_s1_nn_fetch(susp %p, snd_list %p, s1_cnt %d)\n",
           susp, snd_list, (int)susp->s1_cnt);

#ifdef GC_DEBUG
    snd_list_report(snd_list, "add_s1_nn_fetch");
#endif
    /*
     * first compute how many samples to copy (or transfer)
     */

    /* see what the next samples look like */
    susp_check_term_log_block_samples(s1, s1_bptr,
                                      s1_ptr, s1_cnt, 1, 3);
B   if (susp->terminate_bits & 1)
        nyquist_printf("add[%p,%p]: s1 terminates\n", susp->s1, susp->s2);

    /* don't run past the s1 input sample block: */
    togo = susp->s1_cnt;
B   if (togo == 0) stdputstr("togo is zero at checkpoint 1\n");

    /* don't run past terminate time of this signal */
/*    if (susp->s1_ptr == zero_block->samples) { -sep21 RBD*/
    if (susp->terminate_bits & 1) {
        if (susp->s2) {
            s2_start = (int64_t) ((susp->s2->t0 - susp->susp.t0) *
                                  susp->s2->sr + 0.5);
D 	    nyquist_printf("add_s_nn_fetch: s2_start %" PRId64 "\n", s2_start);
        }
        togo = 0;
B       if (togo == 0) stdputstr("togo is zero at checkpoint 2\n");
        if (susp->s2 && susp->susp.current == s2_start) {
            /* s2 starting and s1 stops */
            /* go to s2 alone state */
            sound_unref(susp->s1);
            susp->s1 = NULL;
            susp->susp.fetch = add_s2_nn_fetch;
D            stdputstr("add_s_nn_fetch: other installed, calling now...\n");
            add_s2_nn_fetch(a_susp, snd_list);
        } else if (susp->s2 && susp->susp.current < s2_start) {
            /* s2 not started and s1 stops */
            /* go to zero-fill state */
            sound_unref(susp->s1);
            susp->s1 = NULL;
            susp->susp.fetch = add_zero_fill_nn_fetch;
B           stdputstr("add_s_nn_fetch: zero_fill installed\n");
            add_zero_fill_nn_fetch(a_susp, snd_list);
        } else if (susp->s2) {
D	    stdputstr("add_s_nn_fetch: unexpected condition\n");
            EXIT(1);
        } else /* no s2 */ {
            snd_list_terminate(snd_list);
        }
D	nyquist_printf("add_s_nn_fetch: special return, susp %p\n", susp);
        return; /* fetching taken care of by another routine */
    }
/*    if (susp->terminate_cnt != UNKNOWN &&
        susp->terminate_cnt <= susp->susp.current + togo) {
        togo = susp->terminate_cnt - susp->susp.current;
    }
 */
    /* don't run past logical stop time */
    if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN &&
        susp->logical_stop_bits == 3) {
        int64_t to_stop = susp->susp.log_stop_cnt - susp->susp.current;
        if (to_stop < togo) {
            if (to_stop == 0) {
                susp->logically_stopped = true;
            } else togo = (int) to_stop;
        }
B       if (togo == 0) stdputstr("togo is zero at checkpoint 3\n");
D	nyquist_printf("add_s1_nn_fetch: to_stop %" PRId64 " togo %d\n",
		       to_stop, togo);
    }

    /* consider other signal? don't run past its start time... */
    if (susp->s2) {
        s2_start = ROUND32((susp->s2->t0 - susp->susp.t0) *
            susp->s2->sr);
        if (s2_start < susp->susp.current + togo)
            togo = (int) MIN(togo, s2_start - susp->susp.current);
B           if (togo == 0) stdputstr("togo is zero at checkpoint 4\n");
    }

    /*
     * two cases: copy a partial block or manipulate pointers for
     * copyless transfer of whole block (may not be full block):
     *
     * copy partial block when:
     *   o samples begin in middle of block
     *   o stopping time is before end of block (when other signal
     *     splits the block for this signal)
     * transfer (copyless) block when:
     *   o the block is of maximum size
     *   o the block is small due to logical stop time or termination
     *     time
     */
    if (susp->s1_ptr == susp->s1_bptr->samples &&
        susp->s1_cnt == togo) {
        /*
         * we want to copy this whole block (starting at the beginning
         * and going to the rest of the block) -- just do pointers.
         */

        /* just fetch and pass blocks on */
        if (0) nyquist_printf("add[%p,%p] (s%d_nn) %p starting uncopy, togo %d\n", susp->s1, susp->s2,
               1, susp, togo);
        snd_list->block = susp->s1_bptr;
        (susp->s1_bptr->refcnt)++;
        if (0) nyquist_printf("add[%p,%p] (s%d_nn) %p shared block %p zero_block %p\n",susp->s1, susp->s2,
               1, susp, susp->s1_bptr, zero_block);

        susp_took(s1_cnt, togo);
        snd_list->block_len = togo;

        /* if other is terminated and sound_types match, collapse */
        /* NOTE: in order to collapse, we need s1 to be generating
         * blocks and linking them onto a sound list.  This is true
         * when the get_next fn is SND_get_next.  (A counterexample is
         * SND_get_zeros, which returns zero blocks but does not link
         * them onto the sound list.
         */
        if (0) nyquist_printf("s2 %p thissr %g suspsr %g get_next %d lsc %d\n",
                susp->s2, susp->s1->sr, susp->susp.sr,
                susp->s1->get_next == SND_get_next,
                susp->s1->logical_stop_cnt == UNKNOWN);
        if (susp->s2 == NULL && susp->s1->sr == susp->susp.sr &&
            susp->s1->get_next == SND_get_next &&
            susp->s1->logical_stop_cnt == UNKNOWN) {
            snd_list_type addend_list;
D	    nyquist_printf("add[%p,%p]: collapsing! LSC %d\n", susp->s1, susp->s2,
                      (int)susp->s1->logical_stop_cnt);
D	    sound_print_tree(susp->s1);
            /* will "current" values match? */
            /* test for logical stop */
            if (susp->logically_stopped) {
                snd_list->logically_stopped = true;
            }
            else if (susp->susp.log_stop_cnt == susp->susp.current) {
                susp->logically_stopped = true;
            }
            /* free the superfluous sound_type and susp */
            addend_list = susp->s1->list->u.next;
            snd_list_ref(addend_list);
            snd_list_unref(snd_list->u.next);
            snd_list->u.next = addend_list;
            return;
        }
    } else {
        /*
         * we want to copy a partial block
         */

        /* assume the snd_list is the one with a null block */
        /*
         * put a fresh, clean block in the snd_list
         * (get new snd_list later)
         */
        falloc_sample_block(out, "add_s1_nn_fetch");
        snd_list->block = out;
        out_ptr = out->samples;
B       nyquist_printf("add[%p,%p] (s1_nn) %p new block %p, s1_ptr %p "
                        "block %p s1_cnt %d togo %d\n", susp->s1, susp->s2,
                        susp, out, susp->s1_ptr, susp->s1_bptr->samples,
                        (int)susp->s1_cnt, togo);
        n = togo;
B       if (togo == 0) stdputstr("togo is zero at checkpoint 5\n");
B	    if (togo == 0) nyquist_printf(
                "add[%p,%p] (s%d_nn) %p starting copy loop, togo %d\n",
                susp->s1, susp->s2, 1, susp, togo);
        while (n--) { /* the inner sample computation loop */
            /* scale? */
            *out_ptr++ = *(susp->s1_ptr++);
        } /* inner loop */

        susp_took(s1_cnt, togo);
        snd_list->block_len = togo;
    }

    /* add a new snd_list for the susp */
    susp->susp.current += togo;

D   stdputstr("testing...");
    /*
     * test for termination or change of state,
     * note s2_start computed earlier
     */
    if (susp->s2 && susp->susp.current == s2_start &&
        susp->s1->list != zero_snd_list) {
        /* s2 starting and s1 continues */
        /* go to s1+s2 state */
        susp->susp.fetch = add_s1_s2_nn_fetch;
D        stdputstr("add_s_nn_fetch: add_s1_s2_fetch installed\n");
    } else if (susp->terminate_bits == 3) {
        /* s2 finished and s1 stops */
        /* go to terminal state */
        susp->s1 = NULL;
D        nyquist_printf("add_s_nn_fetch: go to terminal state.  susp->s2 %p, "
               "susp->susp.current %" PRId64 ", s2_start %" PRId64
	       ", susp->s1->list %p, zero_snd_list %p\n",
	       susp->s2, susp->susp.current,
               s2_start, susp->s1->list, zero_snd_list);
        /* !!! free resources and set up pointers to terminal snd_list */
        /* !!! logically stopped? */
    }

    /* test for logical stop */
    if (susp->logically_stopped) {
D        stdputstr("add_s_nn_fetch: snd_list->logically_stopped\n");
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current &&
               susp->logical_stop_bits == 3) {
D        stdputstr("add_s_nn_fetch: susp->logically_stopped\n");
        susp->logically_stopped = true;
    }
D    {
        if (susp->logically_stopped || snd_list->logically_stopped) 
            stdputstr("STOPPED\n");
        else nyquist_printf("ok: current %d\n", (int)susp->susp.current); }
}


void add_s2_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    add_susp_type susp = (add_susp_type) a_susp;
    int togo, s1_start=0;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

D   nyquist_printf("add_s2_nn_fetch(susp %p, snd_list %p)\n",
           susp, snd_list);

#ifdef GC_DEBUG
    snd_list_report(snd_list, "add_s2_nn_fetch");
#endif

    /*
     * first compute how many samples to copy (or transfer)
     */

    /* see what the next samples look like */
    susp_check_term_log_block_samples(s2, s2_bptr,
                                      s2_ptr, s2_cnt, 2, 3);

    /* don't run past the s2 input sample block: */
    togo = susp->s2_cnt;
    assert(togo > 0);

    /* don't run past terminate time of this signal */
    /* if (susp->s2_ptr == zero_block->samples) { -sep21 RBD*/
    if (susp->terminate_bits & 2) {
        if (susp->s1) {
            s1_start = ROUND32((susp->s1->t0 - susp->susp.t0) *
                susp->s1->sr);
             if (0) nyquist_printf("add_s_nn_fetch: s1_start %d\n", s1_start);
        }
        togo = 0;
        if (susp->s1 && susp->susp.current == s1_start) {
            /* s1 starting and s2 stops */
            /* go to s1 alone state */
            sound_unref(susp->s2);
            susp->s2 = NULL;
            susp->susp.fetch = add_s1_nn_fetch;
D            stdputstr("add_s_nn_fetch: other installed, calling now...\n");
            add_s1_nn_fetch(a_susp, snd_list);
        } else if (susp->s1 && susp->susp.current < s1_start) {
            /* s1 not started and s2 stops */
            /* go to zero-fill state */
            sound_unref(susp->s2);
            susp->s2 = NULL;
            susp->susp.fetch = add_zero_fill_nn_fetch;
D            stdputstr("add_s_nn_fetch: zero_fill installed\n");
            add_zero_fill_nn_fetch(a_susp, snd_list);
        } else if (susp->s1) {
D	    stdputstr("add_s_nn_fetch: unexpected condition\n");
            EXIT(1);
        } else /* no s1 */ {
            snd_list_terminate(snd_list);
        }
D	nyquist_printf("add_s_nn_fetch: special return, susp %p\n", susp);
        return; /* fetching taken care of by another routine */
    }
/*    if (susp->terminate_cnt != UNKNOWN &&
        susp->terminate_cnt <= susp->susp.current + togo) {
        togo = susp->terminate_cnt - susp->susp.current;
    }
 */
    /* don't run past logical stop time */
    if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN &&
        /* check if we've seen the logical stop from s2. If so then
           log_stop_cnt is max of s1 and s2 stop times */
        (susp->logical_stop_bits & 2)) {
        int64_t to_stop;
D       nyquist_printf("add_s2_nn_fetch: susp->susp.log_stop_cnt %" PRId64 "\n",
                       susp->susp.log_stop_cnt);
D       nyquist_printf("add_s2_nn_fetch: susp->susp.current %" PRId64 "\n",
                       susp->susp.current);
        to_stop = susp->susp.log_stop_cnt - susp->susp.current;
        // to_stop can be less than zero if we've been adding in sounds with
        // t0 less than the time when the sound is added. E.g. if the user
        // wants a sequence of two sounds that start at 0, the second sound
        // will be spliced onto the first because we don't look at it until
        // the first finishes -- we cannot go back in time and start adding
        // from time 0. This creates a mismatch between the sample count and
        // the logical time, so we could actually set a logical stop time that
        // is back in history, and therefore before susp.current, resulting
        // in a negative to_stop. The problem is really with trying to 
        // sequence two sounds rather than two behaviors, and a warning has
        // already been issued, so we'll just try not to crash here. It's too
        // late to compute the correct answer, which would respect t0 of both
        // sounds.
        if (to_stop < 0) to_stop = 0;
        if (to_stop < togo) {
            if (to_stop == 0) {
                susp->logically_stopped = true;
            } else togo = (int) to_stop;
        }
B       if (togo == 0) stdputstr("togo is zero at checkpoint 3\n");
D	nyquist_printf("add_s2_nn_fetch: to_stop %" PRId64 " togo %d\n",
		       to_stop, togo);
    }

    /* consider other signal? don't run past its start time... */
    if (susp->s1) {
        s1_start = ROUND32((susp->s1->t0 - susp->susp.t0) *
            susp->s1->sr);
        if (s1_start < susp->susp.current + togo)
            togo = (int) MIN(togo, s1_start - susp->susp.current);
            assert(togo > 0);
    }

    /*
     * two cases: copy a partial block or manipulate pointers for
     * copyless transfer of whole block (may not be full block):
     *
     * copy partial block when:
     *   o samples begin in middle of block
     *   o stopping time is before end of block (when other signal
     *     splits the block for this signal)
     * transfer (copyless) block when:
     *   o the block is of maximum size
     *   o the block is small due to logical stop time or termination
     *     time
     */
    if (susp->s2_ptr == susp->s2_bptr->samples &&
        susp->s2_cnt == togo) {
        /*
         * we want to copy this whole block (starting at the beginning
         * and going to the rest of the block) -- just do pointers.
         */

        /* just fetch and pass blocks on */
D	nyquist_printf("add[%p,%p] (s%d_nn) %p starting uncopy, togo %d\n",
                   susp->s2, susp->s1, 2, susp, togo);
        snd_list->block = susp->s2_bptr;
        (susp->s2_bptr->refcnt)++;
D	nyquist_printf("add[%p,%p] (s%d_nn) %p shared block %p zero_block %p\n",
                   susp->s2, susp->s1, 2, susp, susp->s2_bptr, zero_block);

        susp_took(s2_cnt, togo);
        snd_list->block_len = togo;

        /* if other is terminated and sound_types match, collapse */
        /* NOTE: in order to collapse, we need s2 to be generating
         * blocks and linking them onto a sound list.  This is true
         * when the get_next fn is SND_get_next.  (A counterexample is
         * SND_get_zeros, which returns zero blocks but does not link
         * them onto the sound list.
         */
        if (0) nyquist_printf("s1 %p thissr %g suspsr %g get_next %d lsc %d\n",
                susp->s1, susp->s2->sr, susp->susp.sr,
                susp->s2->get_next == SND_get_next,
                susp->s2->logical_stop_cnt == UNKNOWN);
        if (susp->s1 == NULL && susp->s2->sr == susp->susp.sr &&
            susp->s2->get_next == SND_get_next &&
            susp->s2->logical_stop_cnt == UNKNOWN) {
            snd_list_type addend_list;
D           nyquist_printf("add[%p,%p]: collapsing! LSC %d\n",
                      susp->s2, susp->s1, (int)susp->s2->logical_stop_cnt);
D           sound_print_tree(susp->s2);
            /* will "current" values match? */
            /* test for logical stop */
            if (susp->logically_stopped) {
                snd_list->logically_stopped = true;
            }
            else if (susp->susp.log_stop_cnt == susp->susp.current) {
                susp->logically_stopped = true;
            }
            /* free the superfluous sound_type and susp */
            addend_list = susp->s2->list->u.next;
            snd_list_ref(addend_list);
            snd_list_unref(snd_list->u.next);
            snd_list->u.next = addend_list;
            return;
        } else {
D           nyquist_printf("s1 == NULL, but no collapse, lsc %" PRId64 "\n",
                           susp->s2->logical_stop_cnt);
        }
    } else {
        /*
         * we want to copy a partial block
         */

        /* assume the snd_list is the one with a null block */
        /*
         * put a fresh, clean block in the snd_list
         * (get new snd_list later)
         */
        falloc_sample_block(out, "add_s2_nn_fetch");
        snd_list->block = out;
        out_ptr = out->samples;
B       nyquist_printf("add[%p,%p] (s2_nn) %p new block %p\n",
               susp->s2, susp->s1, susp, out);
        n = togo;
        if (n == 0)
            stdputstr("zero block length error in add_s2_nn_fetch\n");
        assert(n > 0);
B       nyquist_printf(
        "add[%p,%p] (s2_nn) %p starting copy loop, togo %d\n",
               susp->s2, susp->s1, susp, togo);
        while (n--) { /* the inner sample computation loop */
            /* scale? */
            *out_ptr++ = *(susp->s2_ptr++);
        } /* inner loop */

        susp_took(s2_cnt, togo);
        snd_list->block_len = togo;
    }

    /* add a new snd_list for the susp */
    susp->susp.current += togo;

    if (/* DISABLES CODE */ (0)) stdputstr("testing...");
    /*
     * test for termination or change of state,
     * note s1_start computed earlier
     */
    if (susp->s1 && susp->susp.current == s1_start &&
        susp->s2->list != zero_snd_list) {
        /* s1 starting and s2 continues */
        /* go to s1+s2 state */
        susp->susp.fetch = add_s1_s2_nn_fetch;
D        stdputstr("add_s_nn_fetch: add_s1_s2_fetch installed\n");
    }
/*    else if (!susp->s1 && susp->s2->list == zero_snd_list) { */
      else if (susp->terminate_bits == 3) {
        /* s1 finished and s2 stops */
        /* go to terminal state */
        susp->s2 = NULL;
D        nyquist_printf("add_s_nn_fetch: go to terminal state.  susp->s1 %p, \
               susp->susp.current %d, s1_start %d, susp->s2->list %p, \
               zero_snd_list %p\n", susp->s1, (int)susp->susp.current,
               s1_start, susp->s2->list, zero_snd_list);
        /* !!! free resources and set up pointers to terminal snd_list */
        /* !!! logically stopped? */
    }

    /* test for logical stop */
    if (susp->logically_stopped) {
D        stdputstr("add_s_nn_fetch: snd_list->logically_stopped\n");
        snd_list->logically_stopped = true;
    } else if (susp->susp.log_stop_cnt == susp->susp.current &&
               (susp->logical_stop_bits & 2)) {
D        stdputstr("add_s_nn_fetch: susp->logically_stopped\n");
        susp->logically_stopped = true;
    }
    if (/* DISABLES CODE */ (0)) {
        if (susp->logically_stopped || snd_list->logically_stopped) 
            stdputstr("STOPPED\n");
        else
            nyquist_printf("ok: current %d\n", (int)susp->susp.current);
    }
}



void add_zero_fill_nn_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    add_susp_type susp = (add_susp_type) a_susp;
    int togo, s_start=0;

#ifdef GC_DEBUG
    snd_list_report(snd_list, "add_zero_fill_nn_fetch");
#endif
    togo = max_sample_block_len;

    if (/* DISABLES CODE */ (0)) fprintf(STDERR,
                   "add_zero_fill_nn_fetch, susp.current %d\n",
                   (int)susp->susp.current);
    /* don't run past start time ... */
    if (susp->s1) {
        s_start = ROUND32((susp->s1->t0 - susp->susp.t0) * susp->s1->sr);
        if (s_start < susp->susp.current + togo) {
            togo = (int) (s_start - susp->susp.current);
        }
    } else if (susp->s2) {
        s_start = ROUND32((susp->s2->t0 - susp->susp.t0) * susp->s2->sr);
        if (s_start < susp->susp.current + togo) {
            togo = (int) (s_start - susp->susp.current);
        }
    }

    snd_list->block_len = togo;
    susp->susp.current += togo;
    /*
     * test for change of state,
     * note s_start computed earlier
     */
    if (susp->s1 && susp->susp.current == s_start) {
        /* s1 starting, go to s1 state */
        susp->susp.fetch = add_s1_nn_fetch;
D        stdputstr("add_zero_fill_nn_fetch: add_s1_nn_fetch installed\n");
    } else if (susp->s2 && susp->susp.current == s_start) {
        /* s2 starting, go to s2 state */
        susp->susp.fetch = add_s2_nn_fetch;
D       stdputstr("add_zero_fill_nn_fetch: add_s2_nn_fetch installed\n");
    }
} /* add_zero_fill_nn_fetch */


void add_free(snd_susp_type a_susp)
{
    add_susp_type susp = (add_susp_type) a_susp;
    sound_unref(susp->s1);
    sound_unref(susp->s2);
    ffree_generic(susp, sizeof(add_susp_node), "add_free");
}


void add_mark(snd_susp_type a_susp)
{
    add_susp_type susp = (add_susp_type) a_susp;
/*    nyquist_printf("add_mark(%p)\n", susp);*/
/*    nyquist_printf("marking s1@%p in add@%p\n", susp->s1, susp);*/
    sound_xlmark(susp->s1);
/*    nyquist_printf("marking s2@%p in add@%p\n", susp->s2, susp);*/
    sound_xlmark(susp->s2);

}


void add_print_tree(snd_susp_type a_susp, int n)
{
    add_susp_type susp = (add_susp_type) a_susp;
    indent(n);
    nyquist_printf("logically_stopped %d logical_stop_bits %d terminate_bits %d\n", 
           susp->logically_stopped, susp->logical_stop_bits, susp->terminate_bits);
    indent(n);
    stdputstr("s1:");
    if (susp->s1) sound_print_tree_1(susp->s1, n);
    else stdputstr(" NULL\n");

    indent(n);
    stdputstr("s2:");
    if (susp->s2) sound_print_tree_1(susp->s2, n);
    else stdputstr(" NULL\n");
}


sound_type snd_make_add(sound_type s1, sound_type s2)
{
    register add_susp_type susp;
    rate_type sr = MAX(s1->sr, s2->sr);
    time_type t0 = MIN(s1->t0, s2->t0);
    int interp_desc = 0;
    double sample_offset;

    /* sort commutative signals: (S1 S2) */
    snd_sort_2(&s1, &s2, sr);

    falloc_generic(susp, add_susp_node, "snd_make_add");

    /* select a susp fn based on sample rates */
    interp_desc = (interp_style(s1, sr) << 2) + interp_style(s2, sr);
    switch (interp_desc) {
      case INTERP_nn:
      case INTERP_ns:
      case INTERP_ss:
        /* eliminate scale factor on s1 if any */
        if (((interp_desc >> INTERP_SHIFT) & INTERP_MASK) == INTERP_s) {
            /* stdputstr("add: prescaling s1\n");*/
            s1 = snd_make_normalize(s1);
        }
        /* eliminate scale factor on s2 if any */
        if ((interp_desc & INTERP_MASK) == INTERP_s) {
            /* stdputstr("add: prescaling s2\n"); */
            s2 = snd_make_normalize(s2);
        }
        sample_offset = (s2->t0 - s1->t0) * sr;
        if (sample_offset >= 0.5) { /* s1 starts first */
            susp->susp.fetch = add_s1_nn_fetch;
D            stdputstr("snd_make_add: add_s1_nn_fetch installed\n");
        } else if (sample_offset < -0.5) { /* s2 starts first */
            susp->susp.fetch = add_s2_nn_fetch;
D            stdputstr("snd_make_add: add_s2_nn_fetch installed\n");
        } else {	/* equal start times */
            susp->susp.fetch = add_s1_s2_nn_fetch;
D            stdputstr("snd_make_add: add_s1_s2_nn_fetch installed\n");
        }
        break;
      case INTERP_ni:
      case INTERP_nr:
        errputstr("add: can't interpolate!\n");
        EXIT(1);
      default:
        errputstr("add: can't add these operands!\n");
        EXIT(1);
    }

    susp->terminate_cnt = UNKNOWN;
    susp->terminate_bits = 0;   /* bits for s1 and s2 termination */
    susp->logical_stop_bits = 0;    /* bits for s1 and s2 logical stop */

    /* initialize susp state */
    susp->susp.free = add_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = add_mark;
    susp->susp.print_tree = add_print_tree;
    susp->susp.name = "add";
    susp->logically_stopped = false;
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->started = false;
    susp->susp.current = 0;
    susp->s1 = s1;
    susp->s1_cnt = 0;
    susp->s2 = s2;
    susp->s2_cnt = 0;
#ifdef UPSAMPLECODE
    susp->susp.s2_phase = 0.0;
    susp->susp.s2_phase_incr = s2->sr / sr;
    susp->susp.output_per_s2 = sr / s2->sr;
#endif
    return sound_create((snd_susp_type)susp, t0, sr, 1.0);
}


sound_type snd_add(sound_type s1, sound_type s2)
{
    sound_type s1_copy = sound_copy(s1);
    sound_type s2_copy = sound_copy(s2);
/*    nyquist_printf("snd_add %p %p copied to %p %p\n", s1, s2, s1_copy,  s2_copy); */
    return snd_make_add(s1_copy, s2_copy);
}
