/* multiseq.c -- return a multichannel signal until its logical stop, then
   evaluate a closure to get another signal and convert to adds
   of two multichannel signals */

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
#include "multiseq.h"
#include "add.h"
#include "scale.h"
#include "extern.h"
#include "cext.h"
#include "sndseq.h"

/* #define MULTISEQ_GC_DEBUG */
#ifdef MULTISEQ_GC_DEBUG
extern snd_list_type snd_list_to_watch;
#endif

/* #define GC_DEBUG */
#ifdef GC_DEBUG
extern sound_type sound_to_watch;
#endif


#define D if(0)

/* Design:

This operator implements sequences of multichannel signals.
A single data structure manages an array of susps that
initially are used to fetch blocks from the first multichannel
signal.  When the LAST logical stop is reached, a closure
is evaluated to yield a new multichannel signal.  The component
sounds of this are stored into the susps which are then
converted into add suspensions.  The other managing structures
are then freed.

The main constraint here is that the conversion to add susps
must take place at the same time across all channels, so before
the conversion, a fetch from the susp can only be made if it is
known that the samples returned happen BEFORE the conversion will
take place.  Since the conversion takes place at the maximum of
the logical stop times of all channels, we have to advance all
channels synchronously.  We keep track of a greatest lower bound,
refered to as the horizon, on the maximum logical stop time.  It
is safe to fetch blocks up to the horizon, but not beyond.

This synchronous fetching is done by a single routine and an
auxilliarly structure that manages the whole multichannel array
of susps.  The basic idea is that a fetch from a 
suspension gets forwarded to the managing structure, which
uses its array of susps to fetch from ALL suspensions up to
the requested time or until the logical stop, whichever comes
first.  These "synchronous" fetches are not made by calling the
fetch routines on the suspensions to avoid infinite recursion.

At any time, there will be some set of channels whose logical 
stop time is unknown.  The "s1" fields (s1, s1_ptr, s1_bptr) of
these suspensions are used to look ahead by geting a block from
s1.  If no logical stop is indicated, then we can append the block
to the snd_list and update the horizon, allowing fetches from other
susps.  In other words, the s1_bptr of each susp provides a one
buffer lookahead by which we can obtain advance knowledge of the
maximum logical stop time.

The algorithm is as follows:

1. When fetch is called on a suspension, compute when any
prefetched samples will end (if there are none, then fetch
a block from s1 and compute the time at which the block ends).
This becomes the target time for other fetches.

2. Call multiseq_advance(), passing the target time and the
manager structure (which has pointers to all other channels).
(Note: every susp has a pointer to the manager).
The function of multiseq_advance() is to push the horizon for
the logical stop forward.  This is done by 
iterating over the array of susps until the target is reached.
Actually, the array contains pointers to the snd_list_node that
points to each susp and where the next block will be linked.
The goal of this loop is to satisfy the original fetch, which
means we have to push low_water to greater than or equal to the
target.  (Low water is the minimum time of the next sample to
be returned by any multiseq susp.) This goal will be met unless
we reach the last logical stop time, in which case we evaluate
the closure for the next multichannel sound, convert 
everything to add's and let the additions take care of returning
blocks.

3. The Iteration Loop:
low_water is the lowest sample count of the next sample 
horizon is the greatest lower bound on the maximum logical stop time
Iterate over susps until low_water >= target
(Note: whenever data is fetched for a sound whose logical stop time
is unknown, update the horizon.  If a logical stop time becomes known,
then test if the final maximum logical stop time is known (by keeping
a count of how many are still unknown), and if the count goes to zero,
evaluate the continuation and convert to multiple adds.)
(Another Note: we may reach the logical stop time and convert to 
multiple adds before the loop terminates, in which case we return
without finishing the loop.  Take care that the caller does the right
thing to produce a sample block in this case.)

3a. If a block hasn't been prefetched, do it.

3b. While the susp has prefetched a block that ends at or before horizon,
put the block on the snd_list and prefetch another block.

3c. If the susp hasn't a known logical stop time, set new_horizon to
the end time of the last sample prefetched in 3b.

3d. If new_horizon == horizon, signal an error, no progress was made.

3d. Set horizon to new_horizon and repeat the loop.

NOTE ON A BUG FIX (1 Jul 95): old code assumed that when a logical stop
was detected it was at the beginning of the next block, but if logical
stop is explicit, then it may be way in the future.  We could convert
to adds at this point, but that would force early evaluation of the
closure, which we'd like to delay (be lazy when possible). Therefore,
we want to ignore knowledge of a logical stop time until the logical
stop time falls within the currently known block of samples. By "currently
known", I mean somewhere in the block referenced by ->s1_ptr and ->s1_cnt.

*/

/* extern LVAL s_stdout; */

void multiseq_convert(multiseq_type ms);
void multiseq_free(snd_susp_type a_susp);
sample_block_type multiseq_get_next(sound_type snd, long * cnt);
void multiseq_print_tree(snd_susp_type a_susp, int n);


#define susp_cnt_time(ssp, ms, cnt) (ssp->susp.t0 - ms->t0 + (cnt)/ssp->s1->sr)
#define susp_time(ssp, ms) susp_cnt_time(ssp, ms, \
                                         (ssp->susp.current + ssp->s1_cnt))
#define susp_low_water(ssp, ms) susp_cnt_time(ssp, ms, ssp->susp.current)
#define susp_log_stop_time(ssp, ms) susp_cnt_time(ssp, ms, ssp->susp.log_stop_cnt)


/* multiseq_advance fetches from each channel to advance to target time */
/*
 * If a channel terminates early, we must be careful: continuing to
 * fetch will return pointers to the zero_block, but this will
 * indicate termination to whoever is fetching from multiseq. We
 * must check the pointers and substitute internal_zero_block to
 * avoid premature termination.
 */
void multiseq_advance(multiseq_type ms, time_type target)
{
    int i;
    time_type new_horizon;
    time_type new_low_water;

D    nyquist_printf("multiseq_advance: %p->low_water %g, target %g\n", 
            ms, ms->low_water, target);
    while (ms->low_water < target - 0.000001) {
        new_horizon = 0.0;
D        nyquist_printf("multiseq_advance loop: target %g low_water %g horizon %g\n",
                target, ms->low_water, ms->horizon);
        /* new_low_water will be a minimum over every
         * channel, so start with a big number */
        new_low_water = target;
        for (i = 0; i < ms->nchans; i++) {
            snd_list_type snd_list = ms->chans[i];
            add_susp_type susp = (add_susp_type) snd_list->u.susp;
            time_type my_hor;
            time_type my_low_water;
D            nyquist_printf("chans[%d]: ", i);

            /* fetch up to horizon */

            /* see if susp has an unprocessed block (test on susp->s1_ptr
             * is probably not necessary, in fact, it isn't initialized
             * until the first block is fetched, but s1_cnt is
             */
            if (susp->s1_cnt && susp->s1_ptr && 
                susp->s1_ptr == susp->s1_bptr->samples) {
                /* do nothing, unprocessed block already there as a
                 * result of the initiating fetch
                 */
            } else if (susp->s1_cnt != 0) {
                stdputstr("multiseq_advance: s1_cnt != 0\n");
                EXIT(1);	/* this should never happen */
            } else { /* otherwise fetch it */
D                stdputstr("prefetching samples ");
                susp_get_block_samples(s1, s1_bptr, s1_ptr, s1_cnt);
                if (susp->s1_ptr == zero_block->samples) {
                    susp->terminate_bits = 1;
                    susp->s1_bptr = internal_zero_block;
                    susp->s1_ptr = internal_zero_block->samples;
                }
                /* see if we've reached a logical stop
                 * (I can't believe this code block is in 3 places -
                 *  there must be a better way... RBD)
                 */
                if (!susp->logical_stop_bits) {
                    if (susp->s1->logical_stop_cnt != UNKNOWN) {
                        if (susp->susp.current + susp->s1_cnt >=
                            susp->s1->logical_stop_cnt) {
                            susp->logical_stop_bits = 1;
                            susp->susp.log_stop_cnt = 
                                susp->s1->logical_stop_cnt;
                            ms->not_logically_stopped_cnt--;
D			    nyquist_printf(
  "snd_make_multiseq: Logical stop reached, not_logically_stopped_cnt %d\n",
                                   ms->not_logically_stopped_cnt);
                        }
                    }
                }
            }
D           nyquist_printf(" current %d cnt %d ", 
                (int)susp->susp.current, (int)susp->s1_cnt);

            /* while the susp has prefetched a block that ends at or
             * before horizon, put the block on the snd_list and
             * prefetch another block
             */
            while (susp_time(susp, ms) < ms->horizon + 0.000001) {
                snd_list->block = susp->s1_bptr;
                snd_list->block_len = (short) susp->s1_cnt;
                susp->susp.current += susp->s1_cnt;
                (susp->s1_bptr->refcnt)++;
                susp->s1_cnt = 0;
#ifdef MULTISEQ_GC_DEBUG
                nyquist_printf(
                "multiseq: output block %p%s on snd_list %p to chan %d\n",
                       susp->s1_bptr,
                       (susp->s1_bptr == internal_zero_block ?
                        " (INTERNAL ZERO BLOCK)" : ""), 
                       snd_list, i);
#endif
                snd_list->u.next = snd_list_create(&(susp->susp));
#ifdef MULTISEQ_GC_DEBUG
                snd_list_debug(snd_list, "multiseq_advance");
#endif
                ms->chans[i] = snd_list = snd_list->u.next;
                susp_get_block_samples(s1, s1_bptr, s1_ptr, s1_cnt);
                if (susp->s1_ptr == zero_block->samples) {
                    susp->terminate_bits = 1;
                    susp->s1_bptr = internal_zero_block;
                    susp->s1_ptr = internal_zero_block->samples;
                }
                if (susp->s1_ptr != susp->s1_bptr->samples) {
                    stdputstr("bug in multiseq_advance\n");
                    EXIT(1);
                }
                /* see if we've reached a logical stop
                 * (I can't believe this code block is in 3 places -
                 *  there must be a better way... RBD)
                 */
                if (!susp->logical_stop_bits) {
                    if (susp->s1->logical_stop_cnt != UNKNOWN) {
                        if (susp->susp.current + susp->s1_cnt >=
                            susp->s1->logical_stop_cnt) {
                            susp->logical_stop_bits = 1;
                            susp->susp.log_stop_cnt = 
                                susp->s1->logical_stop_cnt;
                            ms->not_logically_stopped_cnt--;
D			    nyquist_printf(
  "snd_make_multiseq: Logical stop reached, not_logically_stopped_cnt %d\n",
                                   ms->not_logically_stopped_cnt);
                        }
                    }
                }
D               nyquist_printf("\n\toutput block, current %d cnt %d ",
                               (int)susp->susp.current, (int)susp->s1_cnt);
            }
            if (!susp->logical_stop_bits)
                 my_hor = susp_time(susp, ms);
            else my_hor = susp_log_stop_time(susp, ms);
            if (new_horizon < my_hor) {
D                nyquist_printf("new_horizon %g ", my_hor);
                new_horizon = my_hor;
            }
            if (ms->not_logically_stopped_cnt == 0) {
                ms->horizon = new_horizon; /* pass t0 to multiseq_convert */
D		stdputstr("Calling multiseq_convert\n");
                multiseq_convert(ms);
                return;
            }
            my_low_water = susp_low_water(susp, ms);
            if (my_low_water < new_low_water) {
                new_low_water = my_low_water;
            }
D            stdputstr("\n");
        }
        ms->low_water = new_low_water;
        if (new_horizon <= ms->horizon) {
            stdputstr("no progress in multiseq_advance\n");
            EXIT(1);
        } else {
            ms->horizon = new_horizon;
        }
    }
}


/* multiseq_convert -- eval closure and convert to adds */
/**/
void multiseq_convert(multiseq_type ms)
{
    LVAL result, new;
    sound_type snd;
    time_type now = ms->t0 + ms->horizon;
    int i;
    long size;

    xlsave1(result);
    result = xleval(cons(ms->closure, consa(cvflonum(now))));
    if (exttypep(result, a_sound)) {
        snd = sound_copy(getsound(result));
        result = newvector(ms->nchans);
        setelement(result, 0, cvsound(snd));
        for (i = 1; i < ms->nchans; i++) {
            setelement(result, i, cvsound(sound_zero(now, ms->sr)));
        }
    } else if (vectorp(result)) {
        if (getsize(result) > ms->nchans) {
            xlerror("too few channels", result);
        } else if (getsize(result) < ms->nchans) {
            new = newvector(ms->nchans);
            for (i = 1; i < getsize(result); i++) {
                setelement(new, i, getelement(result, i));
            }
            for (i = getsize(result); i < ms->nchans; i++) {
                setelement(new, i, cvsound(sound_zero(now, ms->sr)));
            }
            result = new;
        }
    } else xlerror("closure did not return a (multi-channel) sound", result);

    /* now result holds a vector of nchans, insert them into add_susp's */
    for (i = 0; i < ms->nchans; i++) {
        snd_list_type snd_list = ms->chans[i];
        add_susp_type susp = (add_susp_type) snd_list->u.susp;
        int64_t sother_start;

        /* remove backpointer to ms */
        susp->multiseq = NULL;
        susp->susp.print_tree = add_print_tree;
        susp->susp.free = add_free;
        susp->susp.mark = add_mark;

        susp->s2 = sound_copy(getsound(getelement(result, i)));
        if (susp->s1->sr != susp->s2->sr)
            xlfail("multiseq: sample rates must match");

        if (susp->s2->scale != 1.0) {
            susp->s2 = snd_make_normalize(susp->s2);
        }

        sother_start = ROUNDBIG((susp->s2->t0 - susp->susp.t0) * susp->s2->sr);
D	    nyquist_printf("sother_start computed for %p: %" PRId64 "\n",
			   susp, sother_start);
        if (sother_start > susp->susp.current) {
D	    nyquist_printf("susp %p using add_s1_nn_fetch\n", susp);
            susp->susp.fetch = add_s1_nn_fetch;
            susp->susp.name = "multiseq:add_s1_nn_fetch";
        } else if (susp->terminate_bits) { /* s1 is done, just get s2 now */
            sound_unref(susp->s1);
            susp->s1 = NULL;
D	    nyquist_printf("susp %p using add_s2_nn_fetch\n", susp);
            susp->susp.fetch = add_s2_nn_fetch;
            susp->susp.name = "multiseq:add_s2_nn_fetch";
        } else {
D	    nyquist_printf("susp %p using add_s1_s2_nn_fetch\n", susp);
            susp->susp.fetch = add_s1_s2_nn_fetch;
            susp->susp.name = "multiseq:add_s1_s2_nn_fetch";
        }

        /* fix up logical stop info */
        /* BUG: what if s2 is already stopped? */
        susp->susp.log_stop_cnt = UNKNOWN;
        susp->logically_stopped = false;

        /* we need to compute at least 1 sample
         * (at this point we don't really know if we've
         * computed anything or not, so to be safe, do it.
         */
        snd_list->u.next = snd_list_create(&(susp->susp));
        snd_list->block = internal_zero_block;
        (*(susp->susp.fetch))((snd_susp_type) susp, snd_list);
    }
    
    /* now free the multiseq struct */
    size = sizeof(snd_list_type) * ms->nchans;
    ffree_generic(ms->chans, size, "multiseq_convert");
    ffree_generic(ms, sizeof(multiseq_node), "multiseq_convert(2)");

    ms->closure = NIL;	/* allow garbage collection now */
    xlpop();
}


/* multiseq_fetch returns blocks of s1 until the logical stop time of s1's */
/*
 * Fetch routines (in particular, the add_*_fetch routines that will
 * be installed on this susp at a later time) expect to be called with
 * a new snd_list installed and ready for a new block.  However, since
 * we are going to call multiseq_advance to pull blocks out of susps 
 * that will not be set up with a fresh snd_list in this way, it is 
 * simpler to dispose of the preallocated snd_list so that all susps
 * look alike to multiseq_advance.  Of course, multiseq_advance will
 * redo the work of allocating a snd_list.
 *
 * If a channel terminates early, we must be careful: continuing to
 * fetch will return pointers to the zero_block, but this will
 * indicate termination to whoever is fetching from multiseq. We
 * must check the pointers and substitute internal_zero_block to
 * avoid premature termination.
 */
void multiseq_fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    add_susp_type susp = (add_susp_type) a_susp;
    time_type block_end_time;

    /* undo the preallocation of a snd_list_node */
    /* we can bypass the reference counting code because we
     * know that this snd_list was just allocated and has no
     * other references
     */
#ifdef MULTISEQ_GC_DEBUG
    if (snd_list_to_watch == snd_list->u.next) {
        nyquist_printf("multiseq_fetch: backing out snd_list_to_watch from %p\n",
               snd_list_to_watch);
        watch_snd_list(snd_list);
    }
#endif
    ffree_snd_list(snd_list->u.next, "multiseq_fetch");
    snd_list->u.susp = (snd_susp_type) susp;
    snd_list->block = NULL;

D    nyquist_printf("multiseq_fetch called: susp %p s1_cnt %d\n",
                    susp, (int)susp->s1_cnt);

    /* first compute how many samples we can generate from s1: */
    if (susp->s1_cnt == 0) {
        susp_get_block_samples(s1, s1_bptr, s1_ptr, s1_cnt);
        if (susp->s1_ptr == zero_block->samples) {
            susp->terminate_bits = 1;   /* mark s1 as terminated */
            susp->s1_bptr = internal_zero_block;
            susp->s1_ptr = internal_zero_block->samples;
        }
        /* see if we've reached a logical stop
         * (I can't believe this code block is in 3 places -
         *  there must be a better way... RBD)
         */
        if (!susp->logical_stop_bits) {
            if (susp->s1->logical_stop_cnt != UNKNOWN) {
                if (susp->susp.current + susp->s1_cnt >=
                    susp->s1->logical_stop_cnt) {
                    susp->logical_stop_bits = 1;
                    susp->susp.log_stop_cnt = 
                      susp->s1->logical_stop_cnt;
                    susp->multiseq->not_logically_stopped_cnt--;
D		    nyquist_printf(
 "snd_make_multiseq: Logical stop reached, not_logically_stopped_cnt %d\n",
                          susp->multiseq->not_logically_stopped_cnt);
                }
            }
        }
    }
    /* s1_cnt has the number of samples we can return */

    /* now compute time of the last sample */
    block_end_time = susp_time(susp, susp->multiseq);
D   nyquist_printf("block_end_time of %p: %g\n", susp, block_end_time);
    multiseq_advance(susp->multiseq,  block_end_time);
}


/* multiseq_mark -- mark routine for multiseq susps */
/**/
void multiseq_mark(snd_susp_type a_susp)
{
    add_susp_type susp = (add_susp_type) a_susp;
    int i;
    multiseq_type ms = susp->multiseq;
D    nyquist_printf("multiseq_mark(%p)\n", susp);
/*    nyquist_printf("marking s1@%p in add@%p\n", susp->s1, susp);*/
    if (ms->closure) mark(ms->closure);

    /* mark s1 of each susp in multiseq */
    for (i = 0; i < ms->nchans; i++) {
        snd_list_type snd_list = ms->chans[i];
        if (snd_list) {
            while (snd_list->block != NULL) {
                if (snd_list == zero_snd_list) break;
                snd_list = snd_list->u.next;
            }
            sound_xlmark(((add_susp_type) snd_list->u.susp)->s1);
        }
    }
}


/* snd_make_multiseq -- make a multiseq from an array and a closure */
/*
 * NOTE: the resulting array of sounds will not use the normal
 * SND_get_first and SND_get_next routines to fetch new blocks
 * because these extend the snd_list of the sound immediately,
 * and this would confuse multiseq_advance() which has to extend
 * multiple snd_lists synchronously.  So, we use multiseq_get_next()
 * instead.
 */
LVAL snd_make_multiseq(LVAL s1, LVAL closure)
{
    multiseq_type ms;
    int i;
    LVAL result;

    xlsave1(result);

    /* allocate multiseq */
    falloc_generic(ms, multiseq_node, "snd_make_multiseq");

    /* install its array of snd_list_type */
    if (!vectorp(s1) || getsize(s1) == 0) {
        ffree_generic(ms, sizeof(multiseq_node), "snd_make_multiseq");
        xlerror("bad argument type", s1);
    }
    ms->nchans = getsize(s1);
    ms->closure = closure;
    ms->not_logically_stopped_cnt = 0;
    ms->low_water = 0.0;
    ms->horizon = 0.0;
    falloc_generic_n(ms->chans, snd_list_type, ms->nchans,
                     "snd_make_multiseq");

    /* allocate sounds to return */
    result = newvector(ms->nchans);

    /* ms->t0 will be the minimum of all t0's in array */
    ms->t0 = (getsound(getelement(s1, 0)))->t0;

    /* create sounds to return */
    for (i = 0; i < ms->nchans; i++) {
        add_susp_type susp;
        sound_type snd;
        falloc_generic(susp, add_susp_node, "snd_make_multiseq(add_susp)");
        susp->s1 = sound_copy(getsound(getelement(s1, i)));
        /* we used to only incr this if lsc was UNKNOWN, but
           that's wrong. Should move this out of the loop now.
         */
        if (susp->s1->scale != 1.0) {
            /* stdputstr("normalizing first sound in a seq\n"); */
            susp->s1 = snd_make_normalize(susp->s1);
        }

        ms->not_logically_stopped_cnt++;
D	nyquist_printf("snd_make_multiseq: not_logically_stopped_cnt %d\n",
               ms->not_logically_stopped_cnt);
        susp->s1_cnt = 0;
        susp->s2 = NULL;
        susp->s2_cnt = 0;
        susp->susp.fetch = multiseq_fetch;
        susp->susp.free = multiseq_free;
        susp->susp.sr = susp->s1->sr;
        susp->susp.mark = multiseq_mark;
        susp->susp.print_tree = multiseq_print_tree;
        susp->susp.name = "multiseq";
        susp->susp.t0 = susp->s1->t0;
        susp->terminate_bits = 0;   /* bits for s1 and s2 termination */
        susp->terminate_cnt = UNKNOWN;
        susp->logical_stop_bits = 0;    /* bits for s1 and s2 log. stop */
        susp->susp.log_stop_cnt = UNKNOWN;
        susp->logically_stopped = false;
        susp->started = false;
        susp->susp.current = 0;
        susp->multiseq = ms;
        snd = sound_create((snd_susp_type) susp, susp->s1->t0, susp->susp.sr,
                           1.0);
#ifdef GC_DEBUG
        if (snd == sound_to_watch) {
            nyquist_printf("watched sound is channel %d\n", i);
        }
#endif	
        setelement(result, i, cvsound(snd));
        if (snd->list->block || !snd->list->u.susp) {
            stdputstr("data inconsistency in snd_make_seq\n");
            EXIT(1);
        }
        ms->chans[i] = snd->list;
D        nyquist_printf("ms->chans[%d] = %p, %p->u.susp = %p\n",
                i, snd->list, snd->list, snd->list->u.susp);
        ms->t0 = MIN(ms->t0, susp->s1->t0);
        ms->sr = susp->s1->sr;	/* assume all samp rates are equal */
D        nyquist_printf("Multiseq sound[%d]: \n", i);
D        sound_print_tree(susp->s1);
    }
D    nyquist_printf("ms->t0 == %g\n", ms->t0);
    xlpop();
    return result;
}


LVAL snd_multiseq(LVAL snd, LVAL closure)
{
    return snd_make_multiseq(sound_array_copy(snd), closure);
}


void multiseq_free(snd_susp_type a_susp)
{
    add_susp_type susp = (add_susp_type) a_susp;
    int i;
    multiseq_type ms = susp->multiseq;
    boolean dead = true;
    sound_unref(susp->s1);
    sound_unref(susp->s2);  /* probably not necessary */
    /* tricky part: remove pointer from ms->chans */
    for (i = 0; i < ms->nchans; i++) {
        if (ms->chans[i]) {
            dead = false;
            /* 
             * note that ms->chans is still a valid 
             * pointer (see snd_list_unref)
             */
            if (ms->chans[i]->u.susp == (snd_susp_type) susp) {
                ms->chans[i] = NULL;
D                nyquist_printf("susp %p freed, ms@%p->chans[%d] = NULL\n",
                        susp, ms, i);
            }
        }
    }

    /* if last element is freed, free the multiseq struct too */
    if (dead) {
        i = sizeof(snd_list_type) * ms->nchans;
        ffree_generic(ms->chans, i, "multiseq_free");
        ffree_generic(ms, sizeof(multiseq_node), "multiseq_free(2)");
    }

    susp->multiseq = NULL;  /* just to be safe */
    ffree_generic(susp, sizeof(add_susp_node), "multiseq_free(3)");
}


void multiseq_print_tree(snd_susp_type a_susp, int n)
{
    add_susp_type susp = (add_susp_type) a_susp;
    int i;

    indent(n);
    if (!susp->multiseq) {
        xlfail("internal error: missing multiseq structure");
    }
    nyquist_printf("multiseq@%p = [ ", susp->multiseq);
    for (i = 0; i < susp->multiseq->nchans; i++) {
        if (susp->multiseq->chans[i]) {
            nyquist_printf("%p", susp->multiseq->chans[i]->u.susp);
        } else {
            stdputstr("NULL");
        }
    }

    indent(n);
    stdputstr("s1:");
    sound_print_tree_1(susp->s1, n);

    indent(n);
    stdputstr("closure:");
    stdprint(susp->multiseq->closure);

    indent(n);
}


