/* xldebug - xlisp debugging support */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  eliminate some compiler warnings
 */


#include "stdlib.h"
#include "xlisp.h"


/* forward declarations */
FORWARD LVAL stacktop(void);
FORWARD LOCAL void breakloop(const char *hdr, const char *cmsg, 
                             const char *emsg, LVAL arg, int cflag);

/* xlabort - xlisp serious error handler */
void xlabort(const char *emsg)
{
    xlsignal(emsg,s_unbound);
    xlerrprint("error",(const char *) NULL,emsg,s_unbound);
    xlbrklevel();
}

/* xlbreak - enter a break loop */
void xlbreak(const char *emsg, LVAL arg)
{
    breakloop("break","return from BREAK",emsg,arg,TRUE);
}

/* xlfail - xlisp error handler */
void xlfail(const char *emsg)
{
    xlerror(emsg,s_unbound);
}

/* close_loadingfiles - close files we were loading from */
void close_loadingfiles()
{
    /* close open files that are being loaded so that user can
       overwrite bug fixes immediately. (Windows locks files
       until they are closed.)
     */
    while (consp(getvalue(s_loadingfiles)) && 
           consp(cdr(getvalue(s_loadingfiles))) &&
           streamp(car(cdr(getvalue(s_loadingfiles)))) &&
		   getfile(car(cdr(getvalue(s_loadingfiles))))) {
        osclose(getfile(car(cdr(getvalue(s_loadingfiles)))));
        /* make the file NULL so GC will not close it again */
        setfile(car(cdr(getvalue(s_loadingfiles))), NULL);
        setvalue(s_loadingfiles, cdr(cdr(getvalue(s_loadingfiles))));
    }
}

/* xlerror - handle a fatal error */
void xlerror(const char *emsg, LVAL arg)
{
    close_loadingfiles();
    if (getvalue(s_breakenable) != NIL)
        breakloop("error",NULL,emsg,arg,FALSE);
    else {
        xlsignal(emsg,arg);
        xlerrprint("error",NULL,emsg,arg);
        xlbrklevel();
    }
}

/* xlcerror - handle a recoverable error */
void xlcerror(const char *cmsg, const char *emsg, LVAL arg)
{
    if (getvalue(s_breakenable) != NIL)
        breakloop("error",cmsg,emsg,arg,TRUE);
    else {
        xlsignal(emsg,arg);
        xlerrprint("error",NULL,emsg,arg);
        xlbrklevel();
    }
}

/* xlerrprint - print an error message */
void xlerrprint(const char *hdr, const char *cmsg, const char *emsg, LVAL arg)
{
    /* print the error message */
    snprintf(buf, STRMAX, "%s: %s", hdr, emsg);
    errputstr(buf);

    /* print the argument */
    if (arg != s_unbound) {
        errputstr(" - ");
        errprint(arg);
    }

    /* no argument, just end the line */
    else
        errputstr("\n");

    /* print the continuation message */
    if (cmsg) {
        snprintf(buf, STRMAX, "if continued: %s\n", cmsg);
        errputstr(buf);
    }
}

/* breakloop - the debug read-eval-print loop */
LOCAL void breakloop(const char *hdr, const char *cmsg, 
                     const char *emsg, LVAL arg, int cflag)
{
    LVAL expr,val;
    XLCONTEXT cntxt;
    int type;

    /* print the error message */
    xlerrprint(hdr,cmsg,emsg,arg);

    /* flush the input buffer */
    xlflush();

    /* do the back trace */
    if (getvalue(s_tracenable)) {
        val = getvalue(s_tlimit);
        xlbaktrace(fixp(val) ? (int)getfixnum(val) : -1);
    }

    /* protect some pointers */
    xlsave1(expr);

    /* increment the debug level */
    ++xldebug;

    /* debug command processing loop */
    xlbegin(&cntxt,CF_BRKLEVEL|CF_CLEANUP|CF_CONTINUE,s_true);
    for (type = 0; type == 0; ) {

        /* setup the continue trap */
        if ((type = _setjmp(cntxt.c_jmpbuf)))
            switch (type) {
            case CF_CLEANUP:
                continue;
            case CF_BRKLEVEL:
                type = 0;
                break;
            case CF_CONTINUE:
                if (cflag) {
                    dbgputstr("[ continue from break loop ]\n");
                    continue;
                }
                else xlabort("this error can't be continued");
            }

        #ifndef READ_LINE
        /* print a prompt */
        sprintf(buf,"%d> ",xldebug);
        dbgputstr(buf);
        #endif

        /* read an expression and check for eof */
        if (!xlread(getvalue(s_debugio),&expr,FALSE)) {
            type = CF_CLEANUP;

            #ifdef READ_LINE
            dbgputstr("\n");
            #endif

            break;
        }

        /* save the input expression */
        xlrdsave(expr);

        /* evaluate the expression */
        expr = xleval(expr);

        /* save the result */
        xlevsave(expr);

        /* print it */
        dbgprint(expr);
    }
    xlend(&cntxt);

    /* decrement the debug level */
    --xldebug;

    /* restore the stack */
    xlpop();

    /* check for aborting to the previous level */
    if (type == CF_CLEANUP)
        xlbrklevel();
}

/* baktrace - do a back trace */
void xlbaktrace(int n)
{
    LVAL *fp,*p;
    int argc;
    for (fp = xlfp; (n < 0 || n--) && *fp; fp = fp - (int)getfixnum(*fp)) {
        p = fp + 1;
        errputstr("Function: ");
        errprint(*p++);
        if ((argc = (int)getfixnum(*p++)))
            errputstr("Arguments:\n");
        while (--argc >= 0) {
            errputstr("  ");
            errprint(*p++);
        }
    }
}

/* xldinit - debug initialization routine */
void xldinit(void)
{
    xlsample = 0;
    xldebug = 0;
}

