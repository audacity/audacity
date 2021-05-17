/* xlinit.c - xlisp initialization module */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use

 * HISTORY
 * 3-Apr-88	Dale Amon at CMU-CSD
 *	Added EXTERN to xlisp 2.0
 *
 * 18-Oct-87	Dale Amon at CMU-CSD
 *	Added initialization for new EXTERN node type.
 *
 * 12 Oct 90	RBD Added s_profile declaration
 *
 *  5 Jun 95    EAD took out extern init (put back when building nyquest)
 */


#include "xlisp.h"

/* external variables */
extern LVAL s_true,s_dot,s_unbound;
extern LVAL s_quote,s_function,s_bquote,s_comma,s_comat;
extern LVAL s_lambda,s_macro;
extern LVAL s_stdin,s_stdout,s_stderr,s_debugio,s_traceout;
extern LVAL s_evalhook,s_applyhook,s_tracelist;
extern LVAL s_tracenable,s_tlimit,s_breakenable, s_profile;
extern LVAL s_setf,s_car,s_cdr,s_nth,s_aref,s_get,s_eql;
extern LVAL s_svalue,s_sfunction,s_splist;
extern LVAL s_rtable,k_wspace,k_const,k_nmacro,k_tmacro;
extern LVAL k_sescape,k_mescape;
extern LVAL s_ifmt,s_ffmt,s_printcase;
extern LVAL s_1plus,s_2plus,s_3plus,s_1star,s_2star,s_3star,s_minus;
extern LVAL k_test,k_tnot;
extern LVAL k_direction,k_input,k_output;
extern LVAL k_start,k_end,k_1start,k_1end,k_2start,k_2end;
extern LVAL k_verbose,k_print,k_count,k_key,k_upcase,k_downcase;
extern LVAL lk_optional,lk_rest,lk_key,lk_aux,lk_allow_other_keys;
extern LVAL a_subr,a_fsubr,a_cons,a_symbol;
extern LVAL a_fixnum,a_flonum,a_string,a_stream,a_object;
extern LVAL a_vector,a_closure,a_char,a_ustream,a_extern;
extern LVAL s_gcflag,s_gchook;
extern LVAL s_search_path;
extern FUNDEF *funtab;

/* forward declarations */
FORWARD LOCAL void initwks(void);


/* xlinit - xlisp initialization routine */
void xlinit(void)
{
    /* initialize xlisp (must be in this order) */
    xlminit();	/* initialize xldmem.c */
    xldinit();	/* initialize xldbug.c */

    /* finish initializing */
#ifdef SAVERESTORE
    if (!xlirestore("xlisp.wks"))
#endif
	{
        initwks();
	}
    /* note: localinit creates type descriptors, but these
       may be necessary for xlirestore to work.  On the other
       hand, localinit creates atoms, so it needs obarray to
       be in place, and obarray is created by initwks.  We may
       have a circular dependency to break before xlirestore
       will work
     */
    localinit();	 /* initialize lisp extensions */ 
}

/* initwks - build an initial workspace */
LOCAL void initwks(void)
{
    FUNDEF *p;
    int i;
    
    xlsinit();	/* initialize xlsym.c */
    xlsymbols();/* enter all symbols used by the interpreter */
    xlrinit();	/* initialize xlread.c */
    xloinit();	/* initialize xlobj.c */

    /* setup defaults */
    setvalue(s_evalhook,NIL);		/* no evalhook function */
    setvalue(s_applyhook,NIL);		/* no applyhook function */
    setvalue(s_tracelist,NIL);		/* no functions being traced */
    setvalue(s_tracenable,NIL);		/* traceback disabled */
    setvalue(s_tlimit,NIL); 		/* trace limit infinite */
    setvalue(s_breakenable,NIL);	/* don't enter break loop on errors */
    setvalue(s_loadingfiles,NIL);       /* not loading any files initially */
    setvalue(s_profile,NIL);		/* don't do profiling */
    setvalue(s_gcflag,NIL);		/* don't show gc information */
    setvalue(s_gchook,NIL);		/* no gc hook active */
    setvalue(s_ifmt,cvstring(IFMT));	/* integer print format */
    setvalue(s_ffmt,cvstring("%g"));	/* float print format */
    setvalue(s_printcase,k_upcase);	/* upper case output of symbols */

    /* install the built-in functions and special forms */
    for (i = 0, p = funtab; p->fd_subr != NULL; ++i, ++p)
        if (p->fd_name)
            xlsubr(p->fd_name,p->fd_type,p->fd_subr,i);

    /* add some synonyms */
    setfunction(xlenter("NOT"),getfunction(xlenter("NULL")));
    setfunction(xlenter("FIRST"),getfunction(xlenter("CAR")));
    setfunction(xlenter("SECOND"),getfunction(xlenter("CADR")));
    setfunction(xlenter("THIRD"),getfunction(xlenter("CADDR")));
    setfunction(xlenter("FOURTH"),getfunction(xlenter("CADDDR")));
    setfunction(xlenter("REST"),getfunction(xlenter("CDR")));
}

/* xlsymbols - enter all of the symbols used by the interpreter */
void xlsymbols(void)
{
    LVAL sym;

    /* enter the unbound variable indicator (must be first) */
    s_unbound = xlenter("*UNBOUND*");
    setvalue(s_unbound,s_unbound);

    /* enter the 't' symbol */
    s_true = xlenter("T");
    setvalue(s_true,s_true);

    /* enter some important symbols */
    s_dot	= xlenter(".");
    s_quote	= xlenter("QUOTE");
    s_function	= xlenter("FUNCTION");
    s_bquote	= xlenter("BACKQUOTE");
    s_comma	= xlenter("COMMA");
    s_comat	= xlenter("COMMA-AT");
    s_lambda	= xlenter("LAMBDA");
    s_macro	= xlenter("MACRO");
    s_eql	= xlenter("EQL");
    s_ifmt	= xlenter("*INTEGER-FORMAT*");
    s_ffmt	= xlenter("*FLOAT-FORMAT*");

    /* symbols set by the read-eval-print loop */
    s_1plus	= xlenter("+");
    s_2plus	= xlenter("++");
    s_3plus	= xlenter("+++");
    s_1star	= xlenter("*");
    s_2star	= xlenter("**");
    s_3star	= xlenter("***");
    s_minus	= xlenter("-");

    /* enter setf place specifiers */
    s_setf	= xlenter("*SETF*");
    s_car	= xlenter("CAR");
    s_cdr	= xlenter("CDR");
    s_nth	= xlenter("NTH");
    s_aref	= xlenter("AREF");
    s_get	= xlenter("GET");
    s_svalue	= xlenter("SYMBOL-VALUE");
    s_sfunction	= xlenter("SYMBOL-FUNCTION");
    s_splist	= xlenter("SYMBOL-PLIST");

    /* enter the readtable variable and keywords */
    s_rtable	= xlenter("*READTABLE*");
    k_wspace	= xlenter(":WHITE-SPACE");
    k_const	= xlenter(":CONSTITUENT");
    k_nmacro	= xlenter(":NMACRO");
    k_tmacro	= xlenter(":TMACRO");
    k_sescape	= xlenter(":SESCAPE");
    k_mescape	= xlenter(":MESCAPE");

    /* enter parameter list keywords */
    k_test	= xlenter(":TEST");
    k_tnot	= xlenter(":TEST-NOT");

    /* "open" keywords */
    k_direction = xlenter(":DIRECTION");
    k_input     = xlenter(":INPUT");
    k_output    = xlenter(":OUTPUT");

    /* enter *print-case* symbol and keywords */
    s_printcase = xlenter("*PRINT-CASE*");
    k_upcase	= xlenter(":UPCASE");
    k_downcase  = xlenter(":DOWNCASE");

    /* other keywords */
    k_start	= xlenter(":START");
    k_end	= xlenter(":END");
    k_1start	= xlenter(":START1");
    k_1end	= xlenter(":END1");
    k_2start	= xlenter(":START2");
    k_2end	= xlenter(":END2");
    k_verbose	= xlenter(":VERBOSE");
    k_print	= xlenter(":PRINT");
    k_count	= xlenter(":COUNT");
    k_key	= xlenter(":KEY");

    /* enter lambda list keywords */
    lk_optional	= xlenter("&OPTIONAL");
    lk_rest	= xlenter("&REST");
    lk_key	= xlenter("&KEY");
    lk_aux	= xlenter("&AUX");
    lk_allow_other_keys = xlenter("&ALLOW-OTHER-KEYS");

    /* enter *standard-input*, *standard-output* and *error-output* */
    s_stdin = xlenter("*STANDARD-INPUT*");
    setvalue(s_stdin,cvfile(stdin));
    s_stdout = xlenter("*STANDARD-OUTPUT*");
    setvalue(s_stdout,cvfile(stdout));
    s_stderr = xlenter("*ERROR-OUTPUT*");
    setvalue(s_stderr,cvfile(STDERR));

    /* enter *debug-io* and *trace-output* */
    s_debugio = xlenter("*DEBUG-IO*");
    setvalue(s_debugio,getvalue(s_stderr));
    s_traceout = xlenter("*TRACE-OUTPUT*");
    setvalue(s_traceout,getvalue(s_stderr));

    /* enter the eval and apply hook variables */
    s_evalhook = xlenter("*EVALHOOK*");
    s_applyhook = xlenter("*APPLYHOOK*");

    /* enter the symbol pointing to the list of functions being traced */
    s_tracelist = xlenter("*TRACELIST*");

    /* enter the error traceback and the error break enable flags */
    s_tracenable = xlenter("*TRACENABLE*");
    s_tlimit = xlenter("*TRACELIMIT*");
    s_breakenable = xlenter("*BREAKENABLE*");
    s_profile = xlenter("*PROFILE*");

    /* enter the symbol pointing to the list of loading files */
    s_loadingfiles = xlenter("*LOADINGFILES*");

    /* enter a symbol to control printing of garbage collection messages */
    s_gcflag = xlenter("*GC-FLAG*");
    s_gchook = xlenter("*GC-HOOK*");

    /* enter the symbol for the search path */
    s_search_path = xlenter("*SEARCH-PATH*");

    /* enter a copyright notice into the oblist */
    sym = xlenter("**Copyright-1988-by-David-Betz**");
    setvalue(sym,s_true);

    /* enter type names */
    a_subr	= xlenter("SUBR");
    a_fsubr	= xlenter("FSUBR");
    a_cons	= xlenter("CONS");
    a_symbol	= xlenter("SYMBOL");
    a_fixnum	= xlenter("FIXNUM");
    a_flonum	= xlenter("FLONUM");
    a_string	= xlenter("STRING");
    a_object	= xlenter("OBJECT");
    a_stream	= xlenter("FILE-STREAM");
    a_vector	= xlenter("ARRAY");
    a_extern	= xlenter("EXTERN");
    a_closure	= xlenter("CLOSURE");
    a_char      = xlenter("CHARACTER");
    a_ustream	= xlenter("UNNAMED-STREAM");

    /* add the object-oriented programming symbols and os specific stuff */
    obsymbols();	/* object-oriented programming symbols */
    ossymbols();	/* os specific symbols */
    localsymbols();	/*  lisp extension symbols */
}

