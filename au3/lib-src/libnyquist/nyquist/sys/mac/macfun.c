/* macfun.c - macintosh user interface functions for xlisp */
/* Written by Brian Kendig. */

#include <Quickdraw.h>
#include <Windows.h>
#include <Memory.h>
#include "xlisp.h"
#include "macint.h"

/* externals */
extern WindowPtr gCommandWin, gGraphicsWin;
extern Boolean hasColorQD;
extern unsigned long startupTicks;
extern void ShowGrafWin (void);

unsigned long ticks_per_second (void)	{ return 60; }
unsigned long run_tick_count (void)		{ return ((unsigned long) TickCount ()) - startupTicks; }
unsigned long real_tick_count (void)	{ return (unsigned long) TickCount (); }

LVAL xrealtime (void)	{ return cvfixnum ((FIXTYPE)real_tick_count()); }	/* get-internal-real-time */
LVAL xruntime (void)	{ return cvfixnum ((FIXTYPE)run_tick_count()); }	/* get-internal-run-time */
LVAL xtime (void)		{ return cvfixnum ((FIXTYPE)real_tick_count()); }	/* time */

/* get an integer parameter */
LOCAL int getNumber () {
    LVAL num = xlgafixnum ();
    return ((int) getfixnum (num));
}

/* handle commands that require integer arguments */
LOCAL LVAL GrafCmd (char funct, int nArgs) {
    short x, y, z;
    if (nArgs > 0) x = getNumber ();
    if (nArgs > 1) y = getNumber ();
    if (nArgs > 2) z = getNumber ();
    xllastarg ();
    SetPort (gGraphicsWin);
    switch (funct) {
    case 'G': ShowGrafWin ();	break;
    case 'g': HideGrafWin ();	break;
    case 'x': EraseRect (&gGraphicsWin->portRect);	break;
    case 's': ShowPen ();		break;
    case 'h': HidePen ();		break;
    case 'd': PenMode (x);		break;
    case 'M': Move (x, y);		break;
    case 'm': MoveTo (x, y);	break;
    case 'L': Line (x, y);		break;
    case 'l': LineTo (x, y);	break;
    case 'S': PenSize (x, y);	break;
    case 'p': PenNormal ();		break;
    case 'c':
        if (hasColorQD) {
            RGBColor col;  col.red = x;  col.green = y;  col.blue = z;
            RGBForeColor (&col);
        } break;
    }
    SetPort (gCommandWin);
    return NIL;
}

LVAL xshowgraphics (void)	{ return GrafCmd ('G', 0); }  /* show graphics win */
LVAL xhidegraphics (void)	{ return GrafCmd ('g', 0); }  /* hide graphics win */
LVAL xcleargraphics (void)	{ return GrafCmd ('x', 0); }  /* clear graphics win */
LVAL xshowpen (void)	{ return GrafCmd ('s', 0); }  /* show the pen */
LVAL xhidepen (void)	{ return GrafCmd ('h', 0); }  /* hide the pen */
LVAL xpenmode (void)	{ return GrafCmd ('d', 1); }  /* set the pen mode */
LVAL xmove (void)		{ return GrafCmd ('M', 2); }  /* move pen in a specified direction */
LVAL xmoveto (void)		{ return GrafCmd ('m', 2); }  /* move pen to a screen location */
LVAL xdraw (void)		{ return GrafCmd ('L', 2); }  /* draw a line in a specified direction */
LVAL xdrawto (void)		{ return GrafCmd ('l', 2); }  /* draw a line to a screen location */
LVAL xpensize (void)	{ return GrafCmd ('S', 2); }  /* set the pen size */
LVAL xpennormal (void)	{ return GrafCmd ('p', 0); }  /* set the pen to normal */
LVAL xcolor (void)		{ return GrafCmd ('c', 3); }  /* set RGB color of pen */


LVAL xgetpen (void) {  /* get the pen position */
    LVAL val;
    Point p;
    xllastarg ();
    SetPort ((GrafPtr)gGraphicsWin);
    GetPen (&p);
    SetPort (gCommandWin);
    xlsave1 (val);
    val = consa (NIL);
    rplaca (val,cvfixnum ((FIXTYPE)p.h));
    rplacd (val,cvfixnum ((FIXTYPE)p.v));
    xlpop ();
    return val;
}

LVAL xpenpat (void) {  /* set the pen pattern */
    LVAL plist;
    Pattern pat;
    int i;
    plist = xlgalist ();
    xllastarg ();
    for (i = 0; i < 8 && consp (plist); ++i, plist = cdr (plist))
//		if (fixp (car (plist))) pat[i] = getfixnum (car (plist));
    SetPort ((GrafPtr)gGraphicsWin);
    PenPat (&pat);
    SetPort (gCommandWin);
    return NIL;
}


/* The functions below are not yet implemented. */

LVAL xtool (void) {  /* call the toolbox */
    int trap = getNumber ();
    LVAL val;

/*	asm {
        move.l	args(A6),D0
        beq	L2
    L1:	move.l	D0,A0
        move.l	2(A0),A1
        move.w	4(A1),-(A7)
        move.l	6(A0),D0
        bne	L1
    L2:	lea	L3,A0
        move.w	trap(A6),(A0)
    L3:	dc.w	0xA000
        clr.l	val(A6)
    }

    return val; */
    return cvfixnum ((FIXTYPE) trap);
}

LVAL xtool16 (void) {  /* call the toolbox with a 16 bit result */
    int trap = getNumber ();
    int val;

/*	asm {
        clr.w	-(A7)
        move.l	args(A6), D0
        beq		L2
    L1:	move.l	D0, A0
        move.l	2(A0), A1
        move.w	4(A1), -(A7)
        move.l	6(A0), D0
        bne		L1
    L2:	lea		L3, A0
        move.w	trap(A6), (A0)
    L3:	dc.w	0xA000
        move.w	(A7)+, val(A6)
    }

    return cvfixnum ((FIXTYPE) val); */
    return cvfixnum ((FIXTYPE) trap);
}

LVAL xtool32 (void) {  /* call the toolbox with a 32 bit result */
    int trap = getNumber ();
    long val;

/*	asm {
        clr.l	-(A7)
        move.l	args(A6),D0
        beq	L2
    L1:	move.l	D0,A0
        move.l	2(A0),A1
        move.w	4(A1),-(A7)
        move.l	6(A0),D0
        bne	L1
    L2:	lea	L3,A0
        move.w	trap(A6),(A0)
    L3:	dc.w	0xA000
        move.l	(A7)+,val(A6)
    }

    return cvfixnum ((FIXTYPE) val); */
    return cvfixnum ((FIXTYPE) trap);
}

LVAL xnewhandle (void) {  /* allocate a new handle */
    LVAL num = xlgafixnum ();
    long size = getfixnum (num);
    xllastarg ();
    return cvfixnum ((FIXTYPE) NewHandle (size));
}

LVAL xnewptr (void) {  /* allocate memory */
    LVAL num = xlgafixnum ();
    long size = getfixnum (num);
    xllastarg ();
    return cvfixnum ((FIXTYPE) NewPtr (size));
}

LVAL xhiword (void) {  /* return the high order 16 bits of an integer */
    unsigned int val = (unsigned int) (getNumber () >> 16);
    xllastarg ();
    return cvfixnum ((FIXTYPE) val);
}

LVAL xloword (void) {  /* return the low order 16 bits of an integer */
    unsigned int val = (unsigned int) getNumber ();
    xllastarg ();
    return cvfixnum ((FIXTYPE) val);
}

LVAL xrdnohang (void) {  /* get the next character in the look-ahead buffer */
    int ch = 0;
    xllastarg ();
/*    if ((ch = scrnextc ()) == EOF) return NIL; */
    return cvfixnum ((FIXTYPE) ch);
}

void ossymbols (void) {  /* ossymbols - enter important symbols */
    LVAL sym;

    /* setup globals for the window handles */
    sym = xlenter ("*COMMAND-WINDOW*");
    setvalue (sym, cvfixnum ((FIXTYPE) gCommandWin));
    sym = xlenter ("*GRAPHICS-WINDOW*");
    setvalue (sym, cvfixnum ((FIXTYPE) gGraphicsWin));
}

void xoserror (char *msg) { /* do nothing */ }

LVAL xsystem (V) { return NIL; }
LVAL xgetkey (V) { return NIL; }
