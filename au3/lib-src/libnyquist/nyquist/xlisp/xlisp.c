/* xlisp.c - a small implementation of lisp with object-oriented programming */
/*	Copyright (c) 1987, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* CHANGELOG:
  8 Oct 90 (Dannenberg) changed main() to xlisp_main_init and xlisp_main.
               made xlisp run as a module that evaluates expressions and
               retains state
 */

#include "switches.h"
#include "stdlib.h"	/* declares exit() */
#include "cext.h"
#include "xlisp.h"
#ifdef MACINTOSH
#include "Memory.h"
#endif

FORWARD void xlisp_wrapup(void);

/* define the banner line string */
#ifdef EXT
#ifdef NYQUIST
#define BANNER "XLISP version 2.0, Copyright (c) 1986, by David Betz"
#else
#define BANNER "Music Editor, Copyright (c) 1987, by Roger B. Dannenberg\n\
XLISP version 2.0, Copyright (c) 1986, by David Betz"
#endif
#else
#ifdef CMTSTUFF
#define BANNER "XLISP version 2.0, Copyright (c) 1986, by David Betz\n\
CMU MIDI Toolkit, Copyright (c) 1993,1994, by Roger B. Dannenberg"
#else
#define BANNER "XLISP version 2.0, Copyright (c) 1986, by David Betz"
#endif
#endif

/* global variables */
jmp_buf top_level;
int in_a_context = FALSE;
int xl_main_loop = FALSE;

/* external variables */
extern LVAL s_stdin,s_evalhook,s_applyhook;
extern LVAL s_1plus,s_2plus,s_3plus,s_1star,s_2star,s_3star,s_minus;
extern int xltrcindent;
extern int xldebug;
extern LVAL s_true;
extern char buf[];
extern FILE *tfp;

/* external routines */
extern FILE *osaopen(const char *name, const char *mode);

#ifdef USE_RANDOM

/* use a fast (but not particularly good) random number generator */

long randomseed = 1L;

long random() {
// note that this takes a seed and returns a big number,
// whereas I think XLisp's RANDOM is defined differently
    long k1;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = randomseed / 127773L;
    if ((randomseed = 16807L * (randomseed - k1 * 127773L) - k1 * 2836L) < 0L)
      randomseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return randomseed;
}
#endif

/* xlrand - return next random number in sequence */
long xlrand (long range) {
    if (range == 0) return 0;
#ifdef USE_RAND
    return rand() % range;
#endif
#ifdef USE_RANDOM
    return random() % range;
#endif
}

long xlsrand(long seed) {
    srand((unsigned int) seed);
    return seed;
}

/* xlrealrand - return random number in [0, 1] */
double xlrealrand() {
    /* always use the random generator from the C library,
       (do not use random() even if USE_RANDOM is defined */
    return (double) rand() / RAND_MAX;
}

/* xlisp_main_init - the main initialization routine */
void xlisp_main_init(int argc, char *argv[])
{
    char *transcript;
    XLCONTEXT cntxt;
    int verbose,i;
         
    /* setup default argument values */
    transcript = NULL;
    verbose = FALSE;

    /* parse the argument list switches */
#ifndef LSC
    for (i = 1; i < argc; ++i)
        if (argv[i][0] == '-')
            switch(argv[i][1]) {
            case 't':
            case 'T':
                transcript = &argv[i][2];
                break;
            case 'v':
            case 'V':
                verbose = TRUE;
                break;
            case 'r':
            case 'R':
                secure_read_path = &argv[i][2];
                break;
            case 'w':
            case 'W':
                safe_write_path = &argv[i][2];
                break;
            case 'l':
            case 'L':
                run_time_limit = atoi(&argv[i][2]);
                break;
            case 'm':
            case 'M':
                memory_limit = atoi(&argv[i][2]);
                break;
            }
#endif

    /* initialize and print the banner line */
    osinit(BANNER);

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
    if (_setjmp(cntxt.c_jmpbuf))
        xlfatal("fatal initialization error");
    if (_setjmp(top_level))
        xlfatal("RESTORE not allowed during initialization");

    /* initialize xlisp */
    xlinit();
    xlend(&cntxt);

#ifdef EXT
    /* special initialization */
#include "xlextstart.c"
#endif

    /* reset the error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,s_true);

    /* open the transcript file */
    if (transcript && (tfp = osaopen(transcript,"w")) == NULL) {
        snprintf(buf, STRMAX, "error: can't open transcript file: %s", transcript);
        stdputstr(buf);
    }

    /* load "init.lsp" */
	if (_setjmp(cntxt.c_jmpbuf) == 0) {
        xlload("init.lsp",TRUE,FALSE);
	}

    /* load any files mentioned on the command line */
    if (_setjmp(cntxt.c_jmpbuf) == 0)
        for (i = 1; i < argc; i++)
            if (argv[i][0] != '-' && !xlload(argv[i],TRUE,verbose))
                xlerror("can't load file",cvstring(argv[i]));
    xlend(&cntxt);
    if (_setjmp(top_level))
        xlfatal("RESTORE not allowed out of read-eval-print loop");
}


/* xlisp_eval -- evaluate an expression created externally */
LVAL xlisp_eval(LVAL expr)
{
    int was_in_a_context = in_a_context;
    XLCONTEXT cntxt;

    if (in_a_context == FALSE) {
        /* create an execution context */
        xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,s_true);
        if (_setjmp(cntxt.c_jmpbuf)) {
            setvalue(s_evalhook,NIL);
            setvalue(s_applyhook,NIL);
            xltrcindent = 0;
            xldebug = 0;
            xlflush();
            oserror("xlisp_eval returning NIL to caller");
            in_a_context = FALSE;
            return NIL;
        }
        in_a_context = TRUE;
    }

    expr = xleval(expr);

    if (!was_in_a_context) {
        xlend(&cntxt);
        in_a_context = FALSE;
    }
    return expr;
}


/* xlisp_main -- run normal lisp read-eval-print loop */
void xlisp_main()
{
    LVAL expr;
    XLCONTEXT cntxt;

    /* build an outer-most context */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,s_true);
    in_a_context = TRUE;

    /* target for restore */
    if (_setjmp(top_level))
        xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,s_true);

    /* protect some pointers */
    xlsave1(expr);

    /* main command processing loop */
    for (xl_main_loop = TRUE; xl_main_loop;) {

        /* setup the error return */
        if (_setjmp(cntxt.c_jmpbuf)) {
            setvalue(s_evalhook,NIL);
            setvalue(s_applyhook,NIL);
            xltrcindent = 0;
            xldebug = 0;
            xlflush();
        }

        #ifndef READ_LINE
        /* print a prompt */
        stdputstr("> ");
        #endif

        /* read an expression */
        if (!xlread(getvalue(s_stdin),&expr,FALSE))
            break;

        /* save the input expression */
        xlrdsave(expr);

        /* evaluate the expression */
        expr = xleval(expr);

        /* save the result */
        xlevsave(expr);

        /* print it */
        stdprint(expr);
    }
    xlend(&cntxt);
    in_a_context = FALSE;
}

/* #include "alloca.h" -- what was this for? -RBD */

#ifndef EXT
int main(int argc, char *argv[])
{
    xlisp_main_init(argc,argv);
    xlisp_main();

    /* clean up */
    xlisp_wrapup();
    return 0;
}
#endif


/* xlrdsave - save the last expression returned by the reader */
void xlrdsave(LVAL expr)
{
    setvalue(s_3plus,getvalue(s_2plus));
    setvalue(s_2plus,getvalue(s_1plus));
    setvalue(s_1plus,getvalue(s_minus));
    setvalue(s_minus,expr);
}

/* xlevsave - save the last expression returned by the evaluator */
void xlevsave(LVAL expr)
{
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,expr);
}

/* xlfatal - print a fatal error message and exit */
void xlfatal(const char *msg)
{
    oserror(msg);
    xlisp_wrapup();
}

/* wrapup - clean up and exit to the operating system */
void xlisp_wrapup(void)
{
    if (tfp)
        osclose(tfp);
    osfinish();
#ifdef CMTSTUFF
    EXIT(0);
#else
    exit(0);
#endif
}

