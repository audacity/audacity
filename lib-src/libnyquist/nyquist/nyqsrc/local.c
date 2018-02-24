/* local.c -- call initialization code for all extensions */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 * 12Feb18  jkc added aud-do function, that is only available in Audacity.
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
    sprintf(buf, "; samples %dKB, %dKB free",
            (sample_block_total * max_sample_block_len) / 1024,
            ((sample_block_total - sample_block_used) *
             max_sample_block_len) / 1024);
    stdputstr(buf);
}




/*--------------------Audacity Automation -------------------------*/
/* These functions may later move to their own source file. */
extern void * ExecForLisp( char * pIn );
extern void * nyq_make_opaque_string( int size, unsigned char *src );


void * nyq_make_opaque_string( int size, unsigned char *src ){
    LVAL dst;
    unsigned char * dstp;
    dst = new_string((int)(size+2));
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (size-- > 0)
        *dstp++ = *src++;
    *dstp = '\0';

    return (void*)dst;
}

/* xlc_aud_do -- interface to C routine aud_do */
/**/
LVAL xlc_aud_do(void)
{
// Based on string-trim...
    unsigned char *leftp,*rightp;
    LVAL src,dst;

    /* get the string */
    src = xlgastring();
    xllastarg();

    /* setup the string pointers */
    leftp = getstring(src);
    rightp = leftp + getslength(src) - 2;

    // Go call my real function here...
    dst = (LVAL)ExecForLisp( leftp );

    /* return the new string */
    return (dst);
}

