/* local.c -- call initialization code for all extensions */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

#include "xlisp.h"
#include "sound.h"
#include "samples.h"
#ifdef CMTSTUFF
#include "seqext.h"
#endif
#include "falloc.h"
#include "sine.h"
#include "stkinit.h"

LVAL RSLT_sym;

void localinit(void)
{
    falloc_init();
/*    probe_init(true);*/
    sound_init();
#ifdef CMTSTUFF
    seqext_init();
#endif
    sine_init();
    stk_init();
}


void localsymbols(void)
{
    RSLT_sym = xlenter("*RSLT*");
    sound_symbols();
    samples_symbols();
#ifdef CMTSTUFF
    seqext_symbols();
#endif
}

extern int sample_block_total;
extern int sample_block_used;

void print_local_gc_info(void)
{
    char buf[50];
    /* print sample blocks */
    sprintf(buf, "; samples %zdKB, %zdKB free",
            (sample_block_total * max_sample_block_len * sizeof(sample_type)) / 1024,
            ((sample_block_total - sample_block_used) *
             max_sample_block_len * sizeof(sample_type)) / 1024);
    stdputstr(buf);
}
