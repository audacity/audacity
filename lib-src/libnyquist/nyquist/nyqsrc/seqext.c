/* seqext.c -- seq extensions for xlisp */
/*
This file extends xlisp with the data type SEQ, including functions
to print and free SEQ type objects.
 */

/* (c) Copyright Carnegie Mellon University 1991, 1994
 * For a statement of limited permission to use, see Permission.doc
 */


/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  portability fix: use %p instead of %lx
 */


#include "stdio.h"
#include "xlisp.h"
#include "cext.h"
#include "userio.h"
#include "midifns.h"
#include "timebase.h"
#include "seq.h"
#include "moxc.h"
#include "seqread.h"
#include "seqext.h"
#include "extern.h"

LVAL s_seq;

xtype_desc seq_desc;

static void xlseq_print(LVAL fptr, void *sequence);

void nop(void) {}

boolean seqp(LVAL s)
{
    return exttypep(s, s_seq);
}


/* xlseq_free gets called by xlisp when the GC frees a seq object.
 * seq_free is a macro, so here we make it into a function pointer.
 */
static void xlseq_free(void *sequence)
{
    seq_free((seq_type)sequence);
}


static void xlseq_print(LVAL fptr, void *sequence)
{
    char s[32];
    sprintf(s, "#<SEQ:0x%p>", sequence);
    xlputstr(fptr, s);
}

static void xlseq_save(FILE *fp, void *sequence)
{
    errputstr("xlseq_save called\n");
}


static unsigned char *xlseq_restore(FILE *fp)
{
   errputstr("xlseq_restore called\n");
   return 0;
}


void seqext_init(void)
{
/*    printf("localinit called\n"); */
    seq_desc = create_desc("SEQ", xlseq_free, xlseq_print, xlseq_save, 
                           xlseq_restore, NULL);
    moxcinit(0, (char **) NULL);
}


void seqext_symbols(void)
{
        s_seq = xlenter("SEQ");
}
