/****************************************************************************
                cext.c
 Copyright 1989 Carnegie Mellon University

 August 3, 1987
 Author: Frits Habermann
----------------------------------------------------------------------------
 02-May-1988 | JCD : portable & AMIGA version.
 17-Oct-1988 | JCD : more portability (FREE).
 28-Apr-2003 | DM  : changed includes for portability
****************************************************************************/

#include "switches.h"

#include <stdio.h>
#include <stdlib.h>

#include "cext.h"
#include "userio.h"

#define calc_middle(top, bot)    (((top - bot) / 2 ) + bottom )

#define kbyte 1000
#define outof_mem(blocksize)     (blocksize == 0 )
#define done_search(top, bot, middle)    ( (( (top - bot) < kbyte ) && \
( !toomuch_mem(middle)) ) || \
                      ( outof_mem( middle ))    )

private boolean toomuch_mem(ushort maximum) 
{
    char *test;
    boolean istoo_much;
    istoo_much = ( (test = (char *) MALLOC(maximum)) == NULL );
    if (test) FREE(test);
    return( istoo_much );
}

private boolean toolittle_mem(maximum) 
ushort maximum;
{
    char *test;
    boolean istoo_little;
    istoo_little = !( (test = (char *) MALLOC(maximum)) == NULL );
    if (test) FREE( test );
    return(istoo_little);
}

private ushort get_biggest_block( maximum )
ushort maximum;
{
    ushort maxblock;
    ushort top = maximum;
    ushort bottom = 0;
    if (!toomuch_mem(maximum)) return(maximum); /* If there's enough memory */
    else {
        gprintf(ERROR, "Running out of memory...\n");
        maxblock = calc_middle( top, bottom );
        while( !done_search(top, bottom, maxblock) ) {
            if( toomuch_mem(maxblock) ) {
                top = maxblock;
                maxblock = calc_middle(top,bottom);
            }
            else if (toolittle_mem(maxblock)) {
                bottom = maxblock;
                maxblock = calc_middle(top,bottom);
            }
        }
    }
    return( maxblock );
}

public ulong MyMaxMem(ushort *growbytes) 
{
    ulong x;
    if( growbytes != NULL ) *growbytes = 0;
    x=( (ulong)get_biggest_block((ushort)BIGGEST_BLOCK));
/*  gprintf(TRANS,"cext: MyMaxMem %ld\n",x); */
    return x;
}

/* note: EXIT is defined to be cmt_exit */

void cmt_exit(n)
  int n;
{
    cu_cleanup();
/* For protection, exit is #defined to hide it.  Expose it and call it. */
#undef exit
    exit(n);
}


#ifdef AMIGA
#ifdef LATTICE
/* for some reason, these don't seem to be defined 
   anywhere in the standard libraries
 */
#include "signal.h"

int _FPERR;
int (*_SIGFPE)(int) = SIG_DFL;

int _oserr;

#endif
#endif

