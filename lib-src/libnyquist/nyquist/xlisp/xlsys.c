/* xlsys.c - xlisp builtin system functions */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* HISTORY
 *
 * 11-Dec-09    Roger Dannenberg
 *  Added getenv
 *
 * 28-Apr-03	Dominic Mazzoni
 *  Eliminated some compiler warnings
 *
 * 25-Oct-87	Roger Dannenberg at NeXT
 *  profiling code added: enable with (PROFILE t), disable with
 *  (PROFILE nil).  While enabled, the profile code counts evals
 *  within functions and macros.  The count is only for evals
 *  directly within the form; i.e. only the count of the most
 *  top-most function or macro form on the stack is incremented.
 *  Also, counts are only maintained for named functions and macros
 *  because the count itself is on the property list of the function
 *  or macro name under the *PROFILE* property.  If a function or
 *  macro is entered and the *PROFILE* does not exist, the property
 *  is created with initial value 0, and the name is inserted at the
 *  head of the list stored as the value of *PROFILE*.  Thus, *PROFILE*
 *  will list the functions that were touched, and the *PROFILE* property
 *  of each function gives some idea of how much time it consumed.
 *  See the file profile.lsp for helpful profiling functions.
 */

#include "xlisp.h"

/* profile variables */
static FIXTYPE invisible_counter;
FIXTYPE *profile_count_ptr = &invisible_counter;
FIXTYPE profile_flag = FALSE;


/* external variables */
extern jmp_buf top_level;
extern FILE *tfp;
extern int xl_main_loop;

/* external symbols */
extern LVAL a_subr,a_fsubr,a_cons,a_symbol;
extern LVAL a_fixnum,a_flonum,a_string,a_object,a_stream;
extern LVAL a_vector,a_closure,a_char,a_ustream;
extern LVAL k_verbose,k_print;
extern LVAL s_true;

/* external routines */
extern FILE *osaopen(const char *name, const char *mode);
extern LVAL exttype(LVAL x);

/* xget_env - get the value of an environment variable */
LVAL xget_env(void)
{
    const char *name = (char *) getstring(xlgetfname());
    char *val;

    /* check for too many arguments */
    xllastarg();

    /* get the value of the environment variable */
    val = getenv(name);
    return (val ? cvstring(val) : NULL);
}

/* xload - read and evaluate expressions from a file */
LVAL xload(void)
{
    const char *name;
    int vflag,pflag;
    LVAL arg;

    /* get the file name, converting unsigned char to char */
    name = (const char *) getstring(xlgetfname());

    /* get the :verbose flag */
    if (xlgetkeyarg(k_verbose,&arg))
        vflag = (arg != NIL);
    else
        vflag = TRUE;

    /* get the :print flag */
    if (xlgetkeyarg(k_print,&arg))
        pflag = (arg != NIL);
    else
        pflag = FALSE;

    /* load the file */
    return (xlload(name, vflag, pflag) ? s_true : NIL);
}

/* xtranscript - open or close a transcript file */
LVAL xtranscript(void)
{
    unsigned char *name;

    /* get the transcript file name */
    name = (moreargs() ? getstring(xlgetfname()) : NULL);
    xllastarg();

    /* close the current transcript */
    if (tfp) osclose(tfp);

    /* open the new transcript */
    tfp = (name ? osaopen((char *) name,"w") : NULL);

    /* return T if a transcript is open, NIL otherwise */
    return (tfp ? s_true : NIL);
}

/* xtype - return type of a thing */
LVAL xtype(void)
{
    LVAL arg;

    if (!(arg = xlgetarg()))
        return (NIL);

    switch (ntype(arg)) {
    case SUBR:		return (a_subr);
    case FSUBR:		return (a_fsubr);
    case CONS:		return (a_cons);
    case SYMBOL:	return (a_symbol);
    case FIXNUM:	return (a_fixnum);
    case FLONUM:	return (a_flonum);
    case STRING:	return (a_string);
    case OBJECT:	return (a_object);
    case STREAM:	return (a_stream);
    case VECTOR:	return (a_vector);
    case CLOSURE:	return (a_closure);
    case CHAR:		return (a_char);
    case USTREAM:	return (a_ustream);
    case EXTERN:	return (exttype(arg));
    default:		xlfail("bad node type");
       return NIL; /* never happens */    
    }
}

/* xbaktrace - print the trace back stack */
LVAL xbaktrace(void)
{
    LVAL num;
    int n;

    if (moreargs()) {
        num = xlgafixnum();
        n = (int) getfixnum(num);
    }
    else
        n = -1;
    xllastarg();
    xlbaktrace(n);
    return (NIL);
}

/* xquit - get out of read/eval/print loop */
LVAL xquit(void)
{
    xllastarg();
    xl_main_loop = FALSE;
    return NIL;
}


/* xexit does not return anything, so turn off "no return value" warning" */
/* #pragma warning(disable: 4035) */

/* xexit - get out of xlisp */
LVAL xexit(void)
{
    xllastarg();
    xlisp_wrapup();
    return NIL; /* never happens */
}

#ifdef PEEK_AND_POKE
/* xpeek - peek at a location in memory */
LVAL xpeek(void)
{
    LVAL num;
    int *adr;

    /* get the address */
    num = xlgafixnum(); adr = (int *)getfixnum(num);
    xllastarg();

    /* return the value at that address */
    return (cvfixnum((FIXTYPE)*adr));
}

/* xpoke - poke a value into memory */
LVAL xpoke(void)
{
    LVAL val;
    int *adr;

    /* get the address and the new value */
    val = xlgafixnum(); adr = (int *)getfixnum(val);
    val = xlgafixnum();
    xllastarg();

    /* store the new value */
    *adr = (int)getfixnum(val);

    /* return the new value */
    return (val);
}

/* xaddrs - get the address of an XLISP node */
LVAL xaddrs(void)
{
    LVAL val;

    /* get the node */
    val = xlgetarg();
    xllastarg();

    /* return the address of the node */
    return (cvfixnum((FIXTYPE)val));
}
#endif /* PEEK_AND_POKE */

/* xprofile - turn profiling on and off */
LVAL xprofile(void)
{
    LVAL flag, result;

    /* get the argument */
    flag = xlgetarg();
    xllastarg();

    result = (profile_flag ? s_true : NIL);
    profile_flag = !null(flag);
    /* turn off profiling right away: */
    if (!profile_flag) profile_count_ptr = &invisible_counter;
    return result;
}


#ifdef DEBUG_INPUT
FILE *debug_input_fp = NULL;

FILE *to_input_buffer = NULL;
FILE *read_by_xlisp = NULL;

LVAL xstartrecordio(void)
{
    to_input_buffer = NULL;
    if (ok_to_open("to-input-buffer.txt", "w"))
	to_input_buffer = fopen("to-input-buffer.txt", "w");
    read_by_xlisp = NULL;
    if (ok_to_open("read-by-xlisp.txt", "w"))
	read_by_xlisp = fopen("read-by-xlisp.txt", "w");
	if (!to_input_buffer || !read_by_xlisp) {
		return NIL;
	}
	return s_true;
}


LVAL xstoprecordio(void)
{
	if (to_input_buffer) fclose(to_input_buffer);
	if (read_by_xlisp) fclose(read_by_xlisp);
	to_input_buffer = NULL;
	read_by_xlisp = NULL;
	return NIL;
}

#endif

/* xgetruntime - get current run_time */
LVAL xgetruntime(void)
{
    /* return the value of run_time variable as integer */
    return cvfixnum((FIXTYPE) run_time);
}
