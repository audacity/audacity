/* sndmax.c -- computes the maximum amplitude in a sound */


/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  min->MIN; fix compiler warning
 * 31Jan07 rbd  handle negative scale factors
 */

#ifdef UNIX
#include "sys/types.h"
#endif	
#include <stdio.h>
/* #include "snd.h" */
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "sndmax.h"
#include "extern.h"

double sound_max(LVAL snd_expr, int64_t n)
{
    LVAL s_as_lval;
    sound_type s = NULL;
    int blocklen;
    sample_block_values_type sbufp;
    register double maximum = 0;

    s_as_lval = xleval(snd_expr);
    /* BE CAREFUL - DO NOT ALLOW GC TO RUN WHILE LVAL IS UNPROTECTED */
    if (exttypep(s_as_lval, a_sound)) {
        /* if snd_expr was simply a symbol, then s now points to
        a shared sound_node.  If we read samples from it, then
        the sound bound to the symbol will be destroyed, so
        copy it first.  If snd_expr was a real expression that
        computed a new value, then the next garbage collection
        will reclaim the sound_node.  We need to make the new
        sound reachable by the garbage collector to that any
        lisp data reachable from the sound do not get collected.
        To make the sound reachable, we need to allocate a node,
        and the GC might run, so we need to protect the OLD s
        but then make it unreachable.
        We will let the GC collect the sound in the end.
        */
        xlprot1(s_as_lval);
        s = sound_copy(getsound(s_as_lval));
        s_as_lval = cvsound(s);	/* destroys only ref. to original */
        /* printf("sound_max: copy is %x, lval %x\n", s, s_as_lval); */
        while (n > 0) {
            long togo, j;
            sample_block_type sampblock = 
              sound_get_next(s, &blocklen);
            if (sampblock == zero_block || blocklen == 0) {
                break;
            }
            togo = (long) MIN(blocklen, n);
            sbufp = sampblock->samples;
            for (j = 0; j < togo; j++) {
                register double samp = *sbufp++;
                if (samp > maximum) maximum = samp;
                else if (-samp > maximum) maximum = -samp;
            }
            n -= togo;
        }
        xlpop();
    } else {
        xlerror("sound_max: expression did not return a sound",
                 s_as_lval);
    }
    return fabs(maximum * s->scale);
}


