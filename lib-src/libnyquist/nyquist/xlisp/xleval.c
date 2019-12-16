/* xleval - xlisp evaluator */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

/* HISTORY 
  28 Apr 03  DM   eliminated some compiler warnings
  12 Oct 90  RBD  added profiling support 
 */

#include "string.h"
#include "xlisp.h"

/* macro to check for lambda list keywords */
#define iskey(s) ((s) == lk_optional \
               || (s) == lk_rest \
               || (s) == lk_key \
               || (s) == lk_aux \
               || (s) == lk_allow_other_keys)

/* macros to handle tracing */
#define trenter(sym,argc,argv) {if (sym) doenter(sym,argc,argv);}
#define trexit(sym,val) {if (sym) doexit(sym,val);}



/* forward declarations */
FORWARD LOCAL LVAL evalhook(LVAL expr);
FORWARD LOCAL LVAL evform(LVAL form);
FORWARD LOCAL LVAL evfun(LVAL fun, int argc, LVAL *argv);
FORWARD LVAL xlclose(LVAL name, LVAL type, LVAL fargs, LVAL body, LVAL env, LVAL fenv);
FORWARD LOCAL int member( LVAL x,  LVAL list);
FORWARD LOCAL int evpushargs(LVAL fun, LVAL args);
FORWARD LOCAL void doenter(LVAL sym, int argc, LVAL *argv);
FORWARD LOCAL void doexit(LVAL sym, LVAL val);
FORWARD LOCAL void badarglist(void);

/* profiling extensions by RBD */
extern LVAL s_profile, profile_fixnum;
extern FIXTYPE *profile_count_ptr, profile_flag;

/* xleval - evaluate an xlisp expression (checking for *evalhook*) */
LVAL xleval(LVAL expr)
{
    /* check for control codes */
    if (--xlsample <= 0) {
        xlsample = SAMPLE;
        run_time++;
        oscheck();
    }

    /* check for *evalhook* */
    if (getvalue(s_evalhook))
        return (evalhook(expr));

    /* check for nil */
    if (null(expr))
        return (NIL);

    /* dispatch on the node type */
    switch (ntype(expr)) {
    case CONS:
        return (evform(expr));
    case SYMBOL:
        return (xlgetvalue(expr));
    default:
        return (expr);
    }
}

/* xlxeval - evaluate an xlisp expression (bypassing *evalhook*) */
LVAL xlxeval(LVAL expr)
{
    /* check for nil */
    if (null(expr))
        return (NIL);

    /* dispatch on node type */
    switch (ntype(expr)) {
    case CONS:
        return (evform(expr));
    case SYMBOL:
        return (xlgetvalue(expr));
    default:
        return (expr);
    }
}

/* xlapply - apply a function to arguments (already on the stack) */
LVAL xlapply(int argc)
{
    LVAL *oldargv,fun,val=NULL;
    LVAL funname;
    LVAL old_profile_fixnum = profile_fixnum;
    FIXTYPE *old_profile_count_ptr = profile_count_ptr;
    int oldargc;

    /* get the function */
    fun = xlfp[1];

    /* get the functional value of symbols */
    if (symbolp(fun)) {
        funname = fun;  /* save it */
        while ((val = getfunction(fun)) == s_unbound)
            xlfunbound(fun);
        fun = xlfp[1] = val;

        if (profile_flag && atomp(funname)) {
            LVAL profile_prop = findprop(funname, s_profile);
            if (null(profile_prop)) {
                /* make a new fixnum, don't use cvfixnum because
                   it would return shared pointer to zero, but we
                   are going to modify this integer in place --
                   dangerous but efficient.
                 */
                profile_fixnum = newnode(FIXNUM);
                profile_fixnum->n_fixnum = 0;
                setplist(funname, cons(s_profile,
                                       cons(profile_fixnum,
                                            getplist(funname))));
                setvalue(s_profile, cons(funname, getvalue(s_profile)));
            } else profile_fixnum = car(profile_prop);
            profile_count_ptr = &getfixnum(profile_fixnum);
        }
    }

    /* check for nil */
    if (null(fun))
        xlerror("bad function",fun);

    /* dispatch on node type */
    switch (ntype(fun)) {
    case SUBR:
        oldargc = xlargc;
        oldargv = xlargv;
        xlargc = argc;
        xlargv = xlfp + 3;
        val = (*getsubr(fun))();
        xlargc = oldargc;
        xlargv = oldargv;
        break;
    case CONS:
        if (!consp(cdr(fun)))
            xlerror("bad function",fun);
        if (car(fun) == s_lambda) {
            fun = xlclose(NIL,
                          s_lambda,
                          car(cdr(fun)),
                          cdr(cdr(fun)),
                          xlenv,xlfenv);
        } else
            xlerror("bad function",fun);
        /**** fall through into the next case ****/
    case CLOSURE:
        if (gettype(fun) != s_lambda)
            xlerror("bad function",fun);
        val = evfun(fun,argc,xlfp+3);
        break;
    default:
        xlerror("bad function",fun);
    }

    /* restore original profile counting state */
    profile_fixnum = old_profile_fixnum;
    profile_count_ptr = old_profile_count_ptr;

    /* remove the call frame */
    xlsp = xlfp;
    xlfp = xlfp - (int)getfixnum(*xlfp);

    /* return the function value */
    return (val);
}

/* evform - evaluate a form */
LOCAL LVAL evform(LVAL form)
{
    LVAL fun,args,val=NULL,type;
    LVAL tracing=NIL;
    LVAL *argv;
    LVAL old_profile_fixnum = profile_fixnum;
    FIXTYPE *old_profile_count_ptr = profile_count_ptr;
    LVAL funname;
    int argc;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    (*profile_count_ptr)++; /* increment profile counter */

    /* get the function and the argument list */
    fun = car(form);
    args = cdr(form);

    funname = fun;

    /* get the functional value of symbols */
    if (symbolp(fun)) {
        if (getvalue(s_tracelist) && member(fun,getvalue(s_tracelist)))
            tracing = fun;
        fun = xlgetfunction(fun);
    }

    /* check for nil */
    if (null(fun))
        xlerror("bad function",NIL);

    /* dispatch on node type */
    switch (ntype(fun)) {
    case SUBR:
        argv = xlargv;
        argc = xlargc;
        xlargc = evpushargs(fun,args);
        xlargv = xlfp + 3;
        trenter(tracing,xlargc,xlargv);
        val = (*getsubr(fun))();
        trexit(tracing,val);
        xlsp = xlfp;
        xlfp = xlfp - (int)getfixnum(*xlfp);
        xlargv = argv;
        xlargc = argc;
        break;
    case FSUBR:
        argv = xlargv;
        argc = xlargc;
        xlargc = pushargs(fun,args);
        xlargv = xlfp + 3;
        val = (*getsubr(fun))();
        xlsp = xlfp;
        xlfp = xlfp - (int)getfixnum(*xlfp);
        xlargv = argv;
        xlargc = argc;
        break;
    case CONS:
        if (!consp(cdr(fun)))
            xlerror("bad function",fun);
        if ((type = car(fun)) == s_lambda)
             fun = xlclose(NIL,
                           s_lambda,
                           car(cdr(fun)),
                           cdr(cdr(fun)),
                           xlenv,xlfenv);
        else
            xlerror("bad function",fun);
        /**** fall through into the next case ****/
    case CLOSURE:
        /* do profiling */
        if (profile_flag && atomp(funname)) {
            LVAL profile_prop = findprop(funname, s_profile);
            if (null(profile_prop)) {
                /* make a new fixnum, don't use cvfixnum because
                   it would return shared pointer to zero, but we
                   are going to modify this integer in place --
                   dangerous but efficient.
                 */
                profile_fixnum = newnode(FIXNUM);
                profile_fixnum->n_fixnum = 0;
                setplist(funname, cons(s_profile,
                                       cons(profile_fixnum,
                                            getplist(funname))));
                setvalue(s_profile, cons(funname, getvalue(s_profile)));
            } else profile_fixnum = car(profile_prop);
            profile_count_ptr = &getfixnum(profile_fixnum);
        }

        if (gettype(fun) == s_lambda) {
            argc = evpushargs(fun,args);
            argv = xlfp + 3;
            trenter(tracing,argc,argv);
            val = evfun(fun,argc,argv);
            trexit(tracing,val);
            xlsp = xlfp;
            xlfp = xlfp - (int)getfixnum(*xlfp);
        }
        else {
            macroexpand(fun,args,&fun);
            val = xleval(fun);
        }
        profile_fixnum = old_profile_fixnum;
        profile_count_ptr = old_profile_count_ptr;
        break;
    default:
        xlerror("bad function",fun);
    }

    /* restore the stack */
    xlpopn(2);

    /* return the result value */
    return (val);
}

/* xlexpandmacros - expand macros in a form */
LVAL xlexpandmacros(LVAL form)
{
    LVAL fun,args;
    
    /* protect some pointers */
    xlstkcheck(3);
    xlprotect(form);
    xlsave(fun);
    xlsave(args);

    /* expand until the form isn't a macro call */
    while (consp(form)) {
        fun = car(form);                /* get the macro name */
        args = cdr(form);               /* get the arguments */
        if (!symbolp(fun) || !fboundp(fun))
            break;
        fun = xlgetfunction(fun);       /* get the expansion function */
        if (!macroexpand(fun,args,&form))
            break;
    }

    /* restore the stack and return the expansion */
    xlpopn(3);
    return (form);
}

/* macroexpand - expand a macro call */
int macroexpand(LVAL fun, LVAL args, LVAL *pval)
{
    LVAL *argv;
    int argc;
    
    /* make sure it's really a macro call */
    if (!closurep(fun) || gettype(fun) != s_macro)
        return (FALSE);
        
    /* call the expansion function */
    argc = pushargs(fun,args);
    argv = xlfp + 3;
    *pval = evfun(fun,argc,argv);
    xlsp = xlfp;
    xlfp = xlfp - (int)getfixnum(*xlfp);
    return (TRUE);
}

/* evalhook - call the evalhook function */
LOCAL LVAL evalhook(LVAL expr)
{
    LVAL *newfp,olddenv,val;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getvalue(s_evalhook));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(expr);
    pusharg(cons(xlenv,xlfenv));
    xlfp = newfp;

    /* rebind the hook functions to nil */
    olddenv = xldenv;
    xldbind(s_evalhook,NIL);
    xldbind(s_applyhook,NIL);

    /* call the hook function */
    val = xlapply(2);

    /* unbind the symbols */
    xlunbind(olddenv);

    /* return the value */
    return (val);
}

/* evpushargs - evaluate and push a list of arguments */
LOCAL int evpushargs(LVAL fun, LVAL args)
{
    LVAL *newfp;
    int argc;
    
    /* protect the argument list */
    xlprot1(args);

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL); /* will be argc */
    /* evaluate and push each argument */
    for (argc = 0; consp(args); args = cdr(args), ++argc) {
        pusharg(xleval(car(args)));
    }
    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;
    
    /* restore the stack */
    xlpop();

    /* return the number of arguments */
    return (argc);
}

/* pushargs - push a list of arguments */
int pushargs(LVAL fun, LVAL args)
{
    LVAL *newfp;
    int argc;
    
    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; consp(args); args = cdr(args), ++argc)
        pusharg(car(args));

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* return the number of arguments */
    return (argc);
}

/* makearglist - make a list of the remaining arguments */
LVAL makearglist(int argc, LVAL *argv)
{
    LVAL list,this,last;
    xlsave1(list);
    for (last = NIL; --argc >= 0; last = this) {
        this = cons(*argv++,NIL);
        if (last) rplacd(last,this);
        else list = this;
        last = this;
    }
    xlpop();
    return (list);
}

/* evfun - evaluate a function */
LOCAL LVAL evfun(LVAL fun, int argc, LVAL *argv)
{
    LVAL oldenv,oldfenv,cptr,name,val;
    XLCONTEXT cntxt;

    /* protect some pointers */
    xlstkcheck(4);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(cptr);
    xlprotect(fun);     /* (RBD) Otherwise, fun is unprotected */

    /* create a new environment frame */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlframe(closure_getenv(fun));
    xlfenv = getfenv(fun);

    /* bind the formal parameters */
    xlabind(fun,argc,argv);

    /* setup the implicit block */
    if ((name = getname(fun)))
        xlbegin(&cntxt,CF_RETURN,name);

    /* execute the block */
    if (name && _setjmp(cntxt.c_jmpbuf))
        val = xlvalue;
    else
        for (val = NIL, cptr = getbody(fun); consp(cptr); cptr = cdr(cptr))
            val = xleval(car(cptr));

    /* finish the block context */
    if (name)
        xlend(&cntxt);

    /* restore the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(4);

    /* return the result value */
    return (val);
}

/* xlclose - create a function closure */
LVAL xlclose(LVAL name, LVAL type, LVAL fargs, LVAL body, LVAL env, LVAL fenv)
{
    LVAL closure,key=NULL,arg,def,svar,new,last;
    char keyname[STRMAX+2];

    /* protect some pointers */
    xlsave1(closure);

    /* create the closure object */
    closure = newclosure(name,type,env,fenv);
    setlambda(closure,fargs);
    setbody(closure,body);

    /* handle each required argument */
    last = NIL;
    while (consp(fargs) && (arg = car(fargs)) && !iskey(arg)) {

        /* make sure the argument is a symbol */
        if (!symbolp(arg))
            badarglist();

        /* create a new argument list entry */
        new = cons(arg,NIL);

        /* link it into the required argument list */
        if (last)
            rplacd(last,new);
        else
            setargs(closure,new);
        last = new;

        /* move the formal argument list pointer ahead */
        fargs = cdr(fargs);
    }

    /* check for the '&optional' keyword */
    if (consp(fargs) && car(fargs) == lk_optional) {
        fargs = cdr(fargs);

        /* handle each optional argument */
        last = NIL;
        while (consp(fargs) && (arg = car(fargs)) && !iskey(arg)) {

            /* get the default expression and specified-p variable */
            def = svar = NIL;
            if (consp(arg)) {
                if ((def = cdr(arg))) {
                    if (consp(def)) {
                        if ((svar = cdr(def))) {
                            if (consp(svar)) {
                                svar = car(svar);
                                if (!symbolp(svar))
                                    badarglist();
                            }
                            else
                                badarglist();
                        }
                        def = car(def);
                    }
                    else
                        badarglist();
                }
                arg = car(arg);
            }

            /* make sure the argument is a symbol */
            if (!symbolp(arg))
                badarglist();

            /* create a fully expanded optional expression */
            new = cons(cons(arg,cons(def,cons(svar,NIL))),NIL);

            /* link it into the optional argument list */
            if (last)
                rplacd(last,new);
            else
                setoargs(closure,new);
            last = new;
                
            /* move the formal argument list pointer ahead */
            fargs = cdr(fargs);
        }
    }

    /* check for the '&rest' keyword */
    if (consp(fargs) && car(fargs) == lk_rest) {
        fargs = cdr(fargs);

        /* get the &rest argument */
        if (consp(fargs) && (arg = car(fargs)) && !iskey(arg) && symbolp(arg))
            setrest(closure,arg);
        else
            badarglist();

        /* move the formal argument list pointer ahead */
        fargs = cdr(fargs);
    }

    /* check for the '&key' keyword */
    if (consp(fargs) && car(fargs) == lk_key) {
        fargs = cdr(fargs);

         /* handle each key argument */
        last = NIL;
        while (consp(fargs) && (arg = car(fargs)) && !iskey(arg)) {

            /* get the default expression and specified-p variable */
            def = svar = NIL;
            if (consp(arg)) {
                if ((def = cdr(arg))) {
                    if (consp(def)) {
                        if ((svar = cdr(def))) {
                            if (consp(svar)) {
                                svar = car(svar);
                                if (!symbolp(svar))
                                    badarglist();
                            }
                            else
                                badarglist();
                        }
                        def = car(def);
                    }
                    else
                        badarglist();
                }
                arg = car(arg);
            }

            /* get the keyword and the variable */
            if (consp(arg)) {
                key = car(arg);
                if (!symbolp(key))
                    badarglist();
                if ((arg = cdr(arg))) {
                    if (consp(arg))
                        arg = car(arg);
                    else
                        badarglist();
                }
            }
            else if (symbolp(arg)) {
                strcpy(keyname,":");
                strcat(keyname,(char *) getstring(getpname(arg)));
                key = xlenter(keyname);
            }

            /* make sure the argument is a symbol */
            if (!symbolp(arg))
                badarglist();

            /* create a fully expanded key expression */
            new = cons(cons(key,cons(arg,cons(def,cons(svar,NIL)))),NIL);

            /* link it into the optional argument list */
            if (last)
                rplacd(last,new);
            else
                setkargs(closure,new);
            last = new;

            /* move the formal argument list pointer ahead */
            fargs = cdr(fargs);
        }
    }

    /* check for the '&allow-other-keys' keyword */
    if (consp(fargs) && car(fargs) == lk_allow_other_keys)
        fargs = cdr(fargs);     /* this is the default anyway */

    /* check for the '&aux' keyword */
    if (consp(fargs) && car(fargs) == lk_aux) {
        fargs = cdr(fargs);

        /* handle each aux argument */
        last = NIL;
        while (consp(fargs) && (arg = car(fargs)) && !iskey(arg)) {

            /* get the initial value */
            def = NIL;
            if (consp(arg)) {
                if ((def = cdr(arg))) {
                    if (consp(def))
                        def = car(def);
                    else
                        badarglist();
                }
                arg = car(arg);
            }

            /* make sure the argument is a symbol */
            if (!symbolp(arg))
                badarglist();

            /* create a fully expanded aux expression */
            new = cons(cons(arg,cons(def,NIL)),NIL);

            /* link it into the aux argument list */
            if (last)
                rplacd(last,new);
            else
                setaargs(closure,new);
            last = new;

            /* move the formal argument list pointer ahead */
            fargs = cdr(fargs);
        }
    }

    /* make sure this is the end of the formal argument list */
    if (fargs)
        badarglist();

    /* restore the stack */
    xlpop();

    /* return the new closure */
    return (closure);
}

/* xlabind - bind the arguments for a function */
void xlabind(LVAL fun, int argc, LVAL *argv)
{
    LVAL *kargv,fargs,key,arg,def,svar,p;
    int rargc,kargc;
    /* protect some pointers */
    xlsave1(def);

    /* bind each required argument */
    for (fargs = getargs(fun); fargs; fargs = cdr(fargs)) {
        /* make sure there is an actual argument */
        if (--argc < 0)
            xlfail("too few arguments");
        
        /* bind the formal variable to the argument value */
        xlbind(car(fargs),*argv++);
    }

    /* bind each optional argument */
    for (fargs = getoargs(fun); fargs; fargs = cdr(fargs)) {

        /* get argument, default and specified-p variable */
        p = car(fargs);
        arg = car(p); p = cdr(p);
        def = car(p); p = cdr(p);
        svar = car(p);

        /* bind the formal variable to the argument value */
        if (--argc >= 0) {
            xlbind(arg,*argv++);
            if (svar) xlbind(svar,s_true);
        }

        /* bind the formal variable to the default value */
        else {
            if (def) def = xleval(def);
            xlbind(arg,def);
            if (svar) xlbind(svar,NIL);
        }
    }

    /* save the count of the &rest of the argument list */
    rargc = argc;
    
    /* handle '&rest' argument */
    if ((arg = getrest(fun))) {
        def = makearglist(argc,argv);
        xlbind(arg,def);
        argc = 0;
    }

    /* handle '&key' arguments */
    if ((fargs = getkargs(fun))) {
        for (; fargs; fargs = cdr(fargs)) {

            /* get keyword, argument, default and specified-p variable */
            p = car(fargs);
            key = car(p); p = cdr(p);
            arg = car(p); p = cdr(p);
            def = car(p); p = cdr(p);
            svar = car(p);

            /* look for the keyword in the actual argument list */
            for (kargv = argv, kargc = rargc; (kargc -= 2) >= 0; kargv += 2)
                if (*kargv == key)
                    break;

            /* bind the formal variable to the argument value */
            if (kargc >= 0) {
                xlbind(arg,*++kargv);
                if (svar) xlbind(svar,s_true);
            }

            /* bind the formal variable to the default value */
            else {
                if (def) def = xleval(def);
                xlbind(arg,def);
                if (svar) xlbind(svar,NIL);
            }
        }
        argc = 0;
    }

    /* check for the '&aux' keyword */
    for (fargs = getaargs(fun); fargs; fargs = cdr(fargs)) {

        /* get argument and default */
        p = car(fargs);
        arg = car(p); p = cdr(p);
        def = car(p);

        /* bind the auxiliary variable to the initial value */
        if (def) def = xleval(def);
        xlbind(arg,def);
    }

    /* make sure there aren't too many arguments */
    if (argc > 0)
        xlfail("too many arguments");

    /* restore the stack */
    xlpop();
}

/* doenter - print trace information on function entry */
LOCAL void doenter(LVAL sym, int argc, LVAL *argv)
{
    extern int xltrcindent;
    int i;
    
    /* indent to the current trace level */
    for (i = 0; i < xltrcindent; ++i)
        trcputstr(" ");
    ++xltrcindent;

    /* display the function call */
    snprintf(buf, STRMAX, "Entering: %s, Argument list: (", getstring(getpname(sym)));
    trcputstr(buf);
    while (--argc >= 0) {
        trcprin1(*argv++);
        if (argc) trcputstr(" ");
    }
    trcputstr(")\n");
}

/* doexit - print trace information for function/macro exit */
LOCAL void doexit(LVAL sym, LVAL val)
{
    extern int xltrcindent;
    int i;
    
    /* indent to the current trace level */
    --xltrcindent;
    for (i = 0; i < xltrcindent; ++i)
        trcputstr(" ");
    
    /* display the function value */
    snprintf(buf, STRMAX, "Exiting: %s, Value: ", getstring(getpname(sym)));
    trcputstr(buf);
    trcprin1(val);
    trcputstr("\n");
}

/* member - is 'x' a member of 'list'? */
LOCAL int member( LVAL x,  LVAL list)
{
    for (; consp(list); list = cdr(list))
        if (x == car(list))
            return (TRUE);
    return (FALSE);
}

/* xlunbound - signal an unbound variable error */
void xlunbound(LVAL sym)
{
    xlcerror("try evaluating symbol again","unbound variable",sym);
}

/* xlfunbound - signal an unbound function error */
void xlfunbound(LVAL sym)
{
    xlcerror("try evaluating symbol again","unbound function",sym);
}

/* xlstkoverflow - signal a stack overflow error */
void xlstkoverflow(void)
{
    xlabort("evaluation stack overflow");
}

/* xlargstkoverflow - signal an argument stack overflow error */
void xlargstkoverflow(void)
{
    xlabort("argument stack overflow");
}

/* badarglist - report a bad argument list error */
LOCAL void badarglist(void)
{
    xlfail("bad formal argument list");
}
