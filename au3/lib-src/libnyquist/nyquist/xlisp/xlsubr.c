/* xlsubr - xlisp builtin function support routines */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  eliminate some compiler warnings
 */

#include "string.h"
#include "xlisp.h"

/* external variables */
extern LVAL k_test,k_tnot,s_eql;

/* xlsubr - define a builtin function */
LVAL xlsubr(const char *sname, int type, LVAL (*fcn)(void), int offset)
{
    LVAL sym;
    sym = xlenter(sname);
    setfunction(sym,cvsubr(fcn,type,offset));
    return (sym);
}

/* xlgetkeyarg - get a keyword argument */
int xlgetkeyarg(LVAL key, LVAL *pval)
{
    LVAL *argv=xlargv;
    int argc=xlargc;
    for (argv = xlargv, argc = xlargc; (argc -= 2) >= 0; argv += 2) {
        if (*argv == key) {
            *pval = *++argv;
            return (TRUE);
        }
    }
    return (FALSE);
}

/* xlgkfixnum - get a fixnum keyword argument */
int xlgkfixnum(LVAL key, LVAL *pval)
{
    if (xlgetkeyarg(key,pval)) {
        if (!fixp(*pval))
            xlbadtype(*pval);
        return (TRUE);
    }
    return (FALSE);
}

/* xltest - get the :test or :test-not keyword argument */
void xltest(LVAL *pfcn, int *ptresult)
{
    if (xlgetkeyarg(k_test,pfcn))	/* :test */
        *ptresult = TRUE;
    else if (xlgetkeyarg(k_tnot,pfcn))	/* :test-not */
        *ptresult = FALSE;
    else {
        *pfcn = getfunction(s_eql);
        *ptresult = TRUE;
    }
}

/* xlgetfile - get a file or stream */
LVAL xlgetfile(void)
{
    LVAL arg;

    /* get a file or stream (cons) or nil */
    if ((arg = xlgetarg())) {
        if (streamp(arg)) {
            if (getfile(arg) == NULL)
                xlfail("file not open");
        }
        else if (!ustreamp(arg))
            xlerror("bad argument type",arg);
    }
    return (arg);
}

/* xlgetfname - get a filename */
LVAL xlgetfname(void)
{
    LVAL name;

    /* get the next argument */
    name = xlgetarg();

    /* get the filename string */
    if (symbolp(name))
        name = getpname(name);
    else if (!stringp(name))
        xlerror("bad argument type",name);

    /* return the name */
    return (name);
}

/* needsextension - check if a filename needs an extension */
int needsextension(const char *name)
{
    const char *p;

    /* check for an extension */
    for (p = &name[strlen(name)]; --p >= &name[0]; )
        if (*p == '.')
            return (FALSE);
        else if (!islower(*p) && !isupper(*p) && !isdigit(*p))
            return (TRUE);

    /* no extension found */
    return (TRUE);
}

/* the next three functions must be declared as LVAL because they
 * are used in LVAL expressions, but they do not return anything
 * warning 4035 is "no return value"
 */
/* #pragma warning(disable: 4035) */

/* xlbadtype - report a "bad argument type" error */
LVAL xlbadtype(LVAL arg)
{
    xlerror("bad argument type",arg);
    return NIL; /* never happens */
}

/* xltoofew - report a "too few arguments" error */
LVAL xltoofew(void)
{
    xlfail("too few arguments");
    return NIL; /* never happens */
}

/* xltoomany - report a "too many arguments" error */
LVAL xltoomany(void)
{
    xlfail("too many arguments");
    return NIL; /* never happens */
}

/* eq - internal eq function */
int eq(LVAL arg1, LVAL arg2)
{
    return (arg1 == arg2);
}

/* eql - internal eql function */
int eql(LVAL arg1, LVAL arg2)
{
    /* compare the arguments */
    if (arg1 == arg2)
        return (TRUE);
    else if (arg1) {
        switch (ntype(arg1)) {
        case FIXNUM:
            return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
        case FLONUM:
            return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
        default:
            return (FALSE);
        }
    }
    else
        return (FALSE);
}

/* lval_equal - internal equal function */
int lval_equal(LVAL arg1, LVAL arg2)
{
    /* compare the arguments */
    if (arg1 == arg2)
        return (TRUE);
    else if (arg1) {
        switch (ntype(arg1)) {
        case FIXNUM:
            return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
        case FLONUM:
            return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
        case STRING:
            return (stringp(arg2) ? strcmp((char *) getstring(arg1),
                                           (char *) getstring(arg2)) == 0 : FALSE);
        case CONS:
            return (consp(arg2) ? lval_equal(car(arg1),car(arg2))
                               && lval_equal(cdr(arg1),cdr(arg2)) : FALSE);
        default:
            return (FALSE);
        }
    }
    else
        return (FALSE);
}
