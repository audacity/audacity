/* xlbfun.c - xlisp basic built-in functions */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

#include "xlisp.h"
#include "string.h"

/* forward declarations */
FORWARD LOCAL LVAL makesymbol(int iflag);

/* xeval - the built-in function 'eval' */
LVAL xeval(void)
{
    LVAL expr;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    /* evaluate the expression */
    return (xleval(expr));
}

/* xapply - the built-in function 'apply' */
LVAL xapply(void)
{
    LVAL fun,arglist;

    /* get the function and argument list */
    fun = xlgetarg();
    arglist = xlgalist();
    xllastarg();

    /* apply the function to the arguments */
    return (xlapply(pushargs(fun,arglist)));
}

/* xfuncall - the built-in function 'funcall' */
LVAL xfuncall(void)
{
    LVAL *newfp;
    int argc;
    
    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(xlgetarg());
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; moreargs(); ++argc)
        pusharg(nextarg());

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function to the arguments */
    return (xlapply(argc));
}

/* xmacroexpand - expand a macro call repeatedly */
LVAL xmacroexpand(void)
{
    LVAL form;
    form = xlgetarg();
    xllastarg();
    return (xlexpandmacros(form));
}

/* x1macroexpand - expand a macro call */
LVAL x1macroexpand(void)
{
    LVAL form,fun,args;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    /* get the form */
    form = xlgetarg();
    xllastarg();

    /* expand until the form isn't a macro call */
    if (consp(form)) {
        fun = car(form);		/* get the macro name */
        args = cdr(form);		/* get the arguments */
        if (symbolp(fun) && fboundp(fun)) {
            fun = xlgetfunction(fun);	/* get the expansion function */
            macroexpand(fun,args,&form);
        }
    }

    /* restore the stack and return the expansion */
    xlpopn(2);
    return (form);
}

/* xatom - is this an atom? */
LVAL xatom(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (atomp(arg) ? s_true : NIL);
}

/* xsymbolp - is this an symbol? */
LVAL xsymbolp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (arg == NIL || symbolp(arg) ? s_true : NIL);
}

/* xnumberp - is this a number? */
LVAL xnumberp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (fixp(arg) || floatp(arg) ? s_true : NIL);
}

/* xintegerp - is this an integer? */
LVAL xintegerp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (fixp(arg) ? s_true : NIL);
}

/* xfloatp - is this a float? */
LVAL xfloatp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? s_true : NIL);
}

/* xcharp - is this a character? */
LVAL xcharp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? s_true : NIL);
}

/* xstringp - is this a string? */
LVAL xstringp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? s_true : NIL);
}

/* xarrayp - is this an array? */
LVAL xarrayp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (vectorp(arg) ? s_true : NIL);
}

/* xstreamp - is this a stream? */
LVAL xstreamp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (streamp(arg) || ustreamp(arg) ? s_true : NIL);
}

/* xobjectp - is this an object? */
LVAL xobjectp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? s_true : NIL);
}

/* xboundp - is this a value bound to this symbol? */
LVAL xboundp(void)
{
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (boundp(sym) ? s_true : NIL);
}

/* xfboundp - is this a functional value bound to this symbol? */
LVAL xfboundp(void)
{
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (fboundp(sym) ? s_true : NIL);
}

/* xnull - is this null? */
LVAL xnull(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (null(arg) ? s_true : NIL);
}

/* xlistp - is this a list? */
LVAL xlistp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? s_true : NIL);
}

/* xendp - is this the end of a list? */
LVAL xendp(void)
{
    LVAL arg;
    arg = xlgalist();
    xllastarg();
    return (null(arg) ? s_true : NIL);
}

/* xconsp - is this a cons? */
LVAL xconsp(void)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? s_true : NIL);
}

/* xeq - are these equal? */
LVAL xeq(void)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (arg1 == arg2 ? s_true : NIL);
}

/* xeql - are these equal? */
LVAL xeql(void)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (eql(arg1,arg2) ? s_true : NIL);
}

/* xequal - are these equal? (recursive) */
LVAL xequal(void)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (lval_equal(arg1,arg2) ? s_true : NIL);
}

/* xset - built-in function set */
LVAL xset(void)
{
    LVAL sym,val;

    /* get the symbol and new value */
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    /* assign the symbol the value of argument 2 and the return value */
    setvalue(sym,val);

    /* return the result value */
    return (val);
}

/* xgensym - generate a symbol */
LVAL xgensym(void)
{
    char sym[STRMAX+11]; /* enough space for prefix and number */
    LVAL x;

    /* get the prefix or number */
    if (moreargs()) {
        x = xlgetarg();
        switch (ntype(x)) {
        case SYMBOL:
                x = getpname(x);
        case STRING:
                strncpy(gsprefix, (char *) getstring(x),STRMAX);
                gsprefix[STRMAX] = '\0';
                break;
        case FIXNUM:
                gsnumber = (int) getfixnum(x);
                break;
        default:
                xlerror("bad argument type",x);
        }
    }
    xllastarg();

    /* create the pname of the new symbol */
    sprintf(sym,"%s%d",gsprefix,gsnumber++);

    /* make a symbol with this print name */
    return (xlmakesym(sym));
}

/* xmakesymbol - make a new uninterned symbol */
LVAL xmakesymbol(void)
{
    return (makesymbol(FALSE));
}

/* xintern - make a new interned symbol */
LVAL xintern(void)
{
    return (makesymbol(TRUE));
}

/* makesymbol - make a new symbol */
LOCAL LVAL makesymbol(int iflag)
{
    LVAL pname;

    /* get the print name of the symbol to intern */
    pname = xlgastring();
    xllastarg();

    /* make the symbol */
    return (iflag ? xlenter((char *) getstring(pname))
                      : xlmakesym((char *) getstring(pname)));
}

/* xsymname - get the print name of a symbol */
LVAL xsymname(void)
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the print name */
    return (getpname(sym));
}

/* xsymvalue - get the value of a symbol */
LVAL xsymvalue(void)
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* get the global value */
    while ((val = getvalue(sym)) == s_unbound)
        xlunbound(sym);

    /* return its value */
    return (val);
}

/* xsymfunction - get the functional value of a symbol */
LVAL xsymfunction(void)
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* get the global value */
    while ((val = getfunction(sym)) == s_unbound)
        xlfunbound(sym);

    /* return its value */
    return (val);
}

/* xsymplist - get the property list of a symbol */
LVAL xsymplist(void)
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the property list */
    return (getplist(sym));
}

/* xget - get the value of a property */
LVAL xget(void)
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgasymbol();
    xllastarg();

    /* retrieve the property value */
    return (xlgetprop(sym,prp));
}

/* xputprop - set the value of a property */
LVAL xputprop(void)
{
    LVAL sym,val,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    val = xlgetarg();
    prp = xlgasymbol();
    xllastarg();

    /* set the property value */
    xlputprop(sym,val,prp);

    /* return the value */
    return (val);
}

/* xremprop - remove a property value from a property list */
LVAL xremprop(void)
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgasymbol();
    xllastarg();

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}

/* xhash - compute the hash value of a string or symbol */
LVAL xhash(void)
{
    unsigned char *str;
    LVAL len,val;
    int n;

    /* get the string and the table length */
    val = xlgetarg();
    len = xlgafixnum(); n = (int)getfixnum(len);
    xllastarg();

    /* get the string */
    if (symbolp(val))
        str = getstring(getpname(val));
    else if (stringp(val))
        str = getstring(val);
    else {
        xlerror("bad argument type",val);
        str = NULL;
    }

    /* return the hash index */
    return (cvfixnum((FIXTYPE)hash((char *) str, n)));
}

/* xaref - array reference function */
LVAL xaref(void)
{
    LVAL array,index;
    int i;

    /* get the array and the index */
    array = xlgavector();
    index = xlgafixnum(); i = (int)getfixnum(index);
    xllastarg();

    /* range check the index */
    if (i < 0 || i >= getsize(array))
        xlerror("array index out of bounds",index);

    /* return the array element */
    return (getelement(array,i));
}

/* xmkarray - make a new array */
LVAL xmkarray(void)
{
    LVAL size;
    int n;

    /* get the size of the array */
    size = xlgafixnum() ; n = (int)getfixnum(size);
    xllastarg();

    /* create the array */
    return (newvector(n));
}

/* xvector - make a vector */
LVAL xvector(void)
{
    LVAL val;
    int i;

    /* make the vector */
    val = newvector(xlargc);

    /* store each argument */
    for (i = 0; moreargs(); ++i)
        setelement(val,i,nextarg());
    xllastarg();

    /* return the vector */
    return (val);
}


/* xerror - special form 'error' */
LVAL xerror(void)
{
    LVAL emsg,arg;

    /* get the error message and the argument */
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlerror((char *) getstring(emsg),arg);
    return NIL; /* won't ever happen */
}

/* xcerror - special form 'cerror' */
LVAL xcerror(void)
{
    LVAL cmsg,emsg,arg;

    /* get the correction message, the error message, and the argument */
    cmsg = xlgastring();
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlcerror((char *) getstring(cmsg), (char *) getstring(emsg),arg);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak(void)
{
    LVAL emsg,arg;

    /* get the error message */
    emsg = (moreargs() ? xlgastring() : NIL);
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* enter the break loop */
    xlbreak((emsg ? (char *) getstring(emsg) : "**BREAK**"),arg);

    /* return nil */
    return (NIL);
}

#pragma warning(disable: 4716 4068) // return type and unknown pragma
#pragma clang diagnostic ignored "-Wreturn-type"
/* xcleanup - special form 'clean-up' */
LVAL xcleanup(void)
{
    xllastarg();
    xlcleanup();
    /* compiler might (wrongly) complain there is no return value */
}

/* xtoplevel - special form 'top-level' */
LVAL xtoplevel(void)
{
    xllastarg();
    xltoplevel();
    /* this point will never be reached because xltoplevel() does a
       _longjmp(). The return is added to avoid false positive 
       error messages from static analyzers and compilers */
    return (NIL); 
}

/* xcontinue - special form 'continue' */
LVAL xcontinue(void)
{
    xllastarg();
    xlcontinue();
    return (NIL); 
}

/* xevalhook - eval hook function */
LVAL xevalhook(void)
{
    LVAL expr,newehook,newahook,newenv,oldenv,oldfenv,olddenv,val;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(newenv);

    /* get the expression, the new hook functions and the environment */
    expr = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    newenv = (moreargs() ? xlgalist() : NIL);
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* establish the environment for the hook function */
    if (newenv) {
        oldenv = xlenv;
        oldfenv = xlfenv;
        xlenv = car(newenv);
        xlfenv = cdr(newenv);
    }

    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* restore the old environment */
    xlunbind(olddenv);
    if (newenv) {
        xlenv = oldenv;
        xlfenv = oldfenv;
    }

    /* restore the stack */
    xlpopn(3);

    /* return the result */
    return (val);
}

