/* xlsym - symbol handling routines */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* HISTORY
 * 28-apr-03 DM  eliminate some compiler warnings
 * 12-oct-90 RBD added xlatomcount to keep track of how many atoms there are.
 *	(something I need for writing out score files).
 */

#include "string.h"
#include "xlisp.h"

extern int xlatomcount;

/* forward declarations */
FORWARD LVAL findprop(LVAL sym, LVAL prp);

#ifdef FRAME_DEBUG
/* these routines were used to debug a missing call to protect().
 * The routines can check for a consistent set of frames.  Note
 * that frames must be pushed on the stack declared here because
 * XLisp keeps frame pointers as local variables in C routines.
 * I deleted the calls to push_xlenv etc throughout the XLisp
 * sources, but decided to leave the following code for possible
 * future debugging. - RBD
 */
int envstack_top = 0;
LVAL envstack[envstack_max];
LVAL *fpstack[envstack_max];
extern long cons_count;

FORWARD LOCAL void test_one_env(LVAL environment, int i, char *s);

void push_xlenv(void)
{
    char s[10];
    /* sprintf(s, "<%d ", envstack_top);
    stdputstr(s); */
    if (envstack_top >= envstack_max) {
            xlabort("envstack overflow");
    } else {
            fpstack[envstack_top] = xlfp;
            envstack[envstack_top++] = xlenv;
    }
}


void pop_xlenv(void)
{
    char s[10];
    if (envstack_top <= 0) {
            sprintf(s, ", %d! ", envstack_top);
            stdputstr(s);
            xlabort("envstack underflow!");
    } else envstack_top--;
    /* sprintf(s, "%d> ", envstack_top);
    stdputstr(s); */
}


void pop_multiple_xlenv(void)
{
    int i;
    for (i = envstack_top - 1; i >= 0; i--) {
            if (envstack[i] == xlenv) {
                char s[10];
                envstack_top = i + 1;
                /* sprintf(s, "%d] ", envstack_top);
                stdputstr(s); */
                return;
            }
    }
}


void testenv(char *s)
{
    int i;
    
    for (i = envstack_top - 1; i >= 0; i--) {
        test_one_env(envstack[i], i, s);
    }
}

LOCAL void report_exit(char *msg, int i)
{
    sprintf(buf, "env stack index: %d, cons_count %ld, Function: ", i, cons_count);
    errputstr(buf);
    stdprint(fpstack[i][1]);
    xlabort(msg);
}

LOCAL void test_one_env(LVAL environment, int i, char *s)
{
    register LVAL fp,ep;
    LVAL val;

    /* check the environment list */
    for (fp = environment; fp; fp = cdr(fp)) {
            /* check that xlenv is good */
            if (!consp(fp)) {
                snprintf(buf, STRMAX, "%s: xlenv 0x%lx, frame 0x%lx, type(frame) %d\n",
                         s, xlenv, fp, ntype(fp));
            errputstr(buf);
            report_exit("xlenv points to a bad list", i);
        }
        
        /* check for an instance variable */
        if ((ep = car(fp)) && objectp(car(ep))) {
            /* do nothing */
        }

        /* check an environment stack frame */
        else {
            for (; ep; ep = cdr(ep)) {
                    /* check that ep is good */
                    if (!consp(ep)) {
                         snprintf(buf, STRMAX, "%s: fp 0x%lx, ep 0x%lx, type(ep) %d\n",
                                 s, fp, ep, ntype(ep));
                    errputstr(buf);
                    report_exit("car(fp) points to a bad list", i);
                }
                
                    /* check that car(ep) is nonnull */
                    if (!car(ep)) {
                         snprintf(buf, STRMAX, "%s: ep 0x%lx, car(ep) 0x%lx\n",
                                  s, ep, car(ep));
                    errputstr(buf);
                    report_exit("car(ep) (an association) is NULL", i);
                }
                    /* check that car(ep) is a cons */
                    if (!consp(car(ep))) {
                         snprintf(buf, STRMAX, "%s: ep 0x%lx, car(ep) 0x%lx, type(car(ep)) %d\n",
                                  s, ep, car(ep), ntype(car(ep)));
                    errputstr(buf);
                    report_exit("car(ep) (an association) is not a cons", i);
                }

                    /* check that car(car(ep)) is a symbol */
                    if (!symbolp(car(car(ep)))) {
                         snprintf(buf, STRMAX, "%s: ep 0x%lx, car(ep) 0x%lx, car(car(ep)) 0x%lx, type(car(car(ep))) %d\n",
                                  s, ep, car(ep), car(car(ep)), ntype(car(car(ep))));
                    errputstr(buf);
                    report_exit("car(car(ep)) is not a symbol", i);
                }
            }
        }
    }
}
#endif


/* xlenter - enter a symbol into the obarray */
LVAL xlenter(const char *name)
{
    LVAL sym,array;
    int i;

    /* check for nil */
    if (strcmp(name,"NIL") == 0)
        return (NIL);

    /* check for symbol already in table */
    array = getvalue(obarray);
    i = hash(name,HSIZE);
    for (sym = getelement(array,i); sym; sym = cdr(sym))
        if (strcmp(name,(char *) getstring(getpname(car(sym)))) == 0)
            return (car(sym));

    /* make a new symbol node and link it into the list */
    xlsave1(sym);
    sym = consd(getelement(array,i));
    rplaca(sym,xlmakesym(name));
    setelement(array,i,sym);
    xlpop();

    /* return the new symbol */
    return (car(sym));
}

/* xlmakesym - make a new symbol node */
LVAL xlmakesym(const char *name)
{
    LVAL sym;
    sym = cvsymbol(name);
    if (*name == ':')
        setvalue(sym,sym);
    return (sym);
}

/* xlgetvalue - get the value of a symbol (with check) */
LVAL xlgetvalue(LVAL sym)
{
    LVAL val;

    /* look for the value of the symbol */
    while ((val = xlxgetvalue(sym)) == s_unbound)
        xlunbound(sym);

    /* return the value */
    return (val);
}

/* xlxgetvalue - get the value of a symbol */
LVAL xlxgetvalue(LVAL sym)
{
    register LVAL fp,ep;
    LVAL val;

    /* check the environment list */
    for (fp = xlenv; fp; fp = cdr(fp))

        /* check for an instance variable */
        if ((ep = car(fp)) && objectp(car(ep))) {
            if (xlobgetvalue(ep,sym,&val))
                return (val);
        }

        /* check an environment stack frame */
        else {
            for (; ep; ep = cdr(ep))
                if (sym == car(car(ep)))
                    return (cdr(car(ep)));
        }

    /* return the global value */
    return (getvalue(sym));
}

/* xlsetvalue - set the value of a symbol */
void xlsetvalue(LVAL sym, LVAL val)
{
    register LVAL fp,ep;

    /* look for the symbol in the environment list */
    for (fp = xlenv; fp; fp = cdr(fp))

        /* check for an instance variable */
        if ((ep = car(fp)) && objectp(car(ep))) {
            if (xlobsetvalue(ep,sym,val))
                return;
        }

        /* check an environment stack frame */
        else {
            for (; ep; ep = cdr(ep))
                if (sym == car(car(ep))) {
                    rplacd(car(ep),val);
                    return;
                }
        }

    /* store the global value */
    setvalue(sym,val);
}

/* xlgetfunction - get the functional value of a symbol (with check) */
LVAL xlgetfunction(LVAL sym)
{
    LVAL val;

    /* look for the functional value of the symbol */
    while ((val = xlxgetfunction(sym)) == s_unbound)
        xlfunbound(sym);

    /* return the value */
    return (val);
}

/* xlxgetfunction - get the functional value of a symbol */
LVAL xlxgetfunction(LVAL sym)
{
    register LVAL fp,ep;

    /* check the environment list */
    for (fp = xlfenv; fp; fp = cdr(fp))
        for (ep = car(fp); ep; ep = cdr(ep))
            if (sym == car(car(ep)))
                return (cdr(car(ep)));

    /* return the global value */
    return (getfunction(sym));
}

/* xlsetfunction - set the functional value of a symbol */
void xlsetfunction(LVAL sym, LVAL val)
{
    register LVAL fp,ep;

    /* look for the symbol in the environment list */
    for (fp = xlfenv; fp; fp = cdr(fp))
        for (ep = car(fp); ep; ep = cdr(ep))
            if (sym == car(car(ep))) {
                rplacd(car(ep),val);
                return;
            }

    /* store the global value */
    setfunction(sym,val);
}

/* xlgetprop - get the value of a property */
LVAL xlgetprop(LVAL sym, LVAL prp)
{
    LVAL p;
    return ((p = findprop(sym,prp)) ? car(p) : NIL);
}

/* xlputprop - put a property value onto the property list */
void xlputprop(LVAL sym, LVAL val, LVAL prp)
{
    LVAL pair;
    if ((pair = findprop(sym,prp)))
        rplaca(pair,val);
    else
        setplist(sym,cons(prp,cons(val,getplist(sym))));
}

/* xlremprop - remove a property from a property list */
void xlremprop(LVAL sym, LVAL prp)
{
    LVAL last,p;
    last = NIL;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(last)) {
        if (car(p) == prp) {
            if (last)
                rplacd(last,cdr(cdr(p)));
            else
                setplist(sym,cdr(cdr(p)));
        }
        last = cdr(p);
    }
}

/* findprop - find a property pair */
LVAL findprop(LVAL sym, LVAL prp)
{
    LVAL p;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
        if (car(p) == prp)
            return (cdr(p));
    return (NIL);
}

/* hash - hash a symbol name string */
int hash(const char *str, int len)
{
    int i;
    for (i = 0; *str; )
        i = (i << 2) ^ *str++;
    i %= len;
    return (i < 0 ? -i : i);
}

/* xlsinit - symbol initialization routine */
void xlsinit(void)
{
    LVAL array,p;

    /* initialize the obarray */
    obarray = xlmakesym("*OBARRAY*");
    array = newvector(HSIZE);
    setvalue(obarray,array);

    /* add the symbol *OBARRAY* to the obarray */
    p = consa(obarray);
    setelement(array,hash("*OBARRAY*",HSIZE),p);
}
