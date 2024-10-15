/* xlpp.c - xlisp pretty printer */
/*	Copyright (c) 1985, by David Betz
        All Rights Reserved			*/

#include "xlisp.h"

/* external variables */
extern LVAL s_stdout;
extern int xlfsize;

/* local variables */
static int pplevel,ppmargin,ppmaxlen;
static LVAL ppfile;

LOCAL void pp(LVAL expr);
LOCAL void ppterpri(void);
LOCAL void pplist(LVAL expr);
LOCAL void ppexpr(LVAL expr);
LOCAL int flatsize(LVAL expr);
LOCAL void ppputc(int ch);


/* xpp - pretty-print an expression */
LVAL xpp(void)
{
    LVAL expr;

    /* get expression to print and file pointer */
    expr = xlgetarg();
    ppfile = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* pretty print the expression */
    pplevel = ppmargin = 0; ppmaxlen = 40;
    pp(expr); ppterpri();

    /* return nil */
    return (NIL);
}

/* pp - pretty print an expression */
LOCAL void pp(LVAL expr)
{
    if (consp(expr))
        pplist(expr);
    else
        ppexpr(expr);
}

/* pplist - pretty print a list */
LOCAL void pplist(LVAL expr)
{
    int n;

    /* if the expression will fit on one line, print it on one */
    if ((n = flatsize(expr)) < ppmaxlen) {
        xlprint(ppfile,expr,TRUE);
        pplevel += n;
    }

    /* otherwise print it on several lines */
    else {
        n = ppmargin;
        ppputc('(');
        if (atomp(car(expr))) {
            ppexpr(car(expr));
            ppputc(' ');
            ppmargin = pplevel;
            expr = cdr(expr);
        }
        else
            ppmargin = pplevel;
        for (; consp(expr); expr = cdr(expr)) {
            pp(car(expr));
            if (consp(cdr(expr)))
                ppterpri();
        }
        if (expr != NIL) {
            ppputc(' '); ppputc('.'); ppputc(' ');
            ppexpr(expr);
        }
        ppputc(')');
        ppmargin = n;
    }
}

/* ppexpr - print an expression and update the indent level */
LOCAL void ppexpr(LVAL expr)
{
    xlprint(ppfile,expr,TRUE);
    pplevel += flatsize(expr);
}

/* ppputc - output a character and update the indent level */
LOCAL void ppputc(int ch)
{
    xlputc(ppfile,ch);
    pplevel++;
}

/* ppterpri - terminate the print line and indent */
LOCAL void ppterpri(void)
{
    xlterpri(ppfile);
    for (pplevel = 0; pplevel < ppmargin; pplevel++)
        xlputc(ppfile,' ');
}

/* flatsize - compute the flat size of an expression */
LOCAL int flatsize(LVAL expr)
{
    xlfsize = 0;
    xlprint(NIL,expr,TRUE);
    return (xlfsize);
}
