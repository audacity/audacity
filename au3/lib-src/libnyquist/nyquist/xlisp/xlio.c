/* xlio - xlisp i/o routines */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  eliminate some compiler warnings
 */

#include "xlisp.h"

/* external variables */
extern LVAL s_stdin,s_stdout,s_stderr,s_debugio,s_traceout,s_unbound;
extern int xlfsize;

#ifdef DEBUG_INPUT
extern FILE *read_by_xlisp;
#endif

/* xlgetc - get a character from a file or stream */
int xlgetc(LVAL fptr)
{
    LVAL lptr, cptr=NULL;
    FILE *fp;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
        ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
        if ((lptr = gethead(fptr)) == NIL)
            ch = EOF;
        else {
            if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
                xlfail("bad stream");
            sethead(fptr,lptr = cdr(lptr));
            if (lptr == NIL)
                settail(fptr,NIL);
            ch = getchcode(cptr);
        }
    }

    /* otherwise, check for a buffered character */
    else if ((ch = getsavech(fptr)))
        setsavech(fptr,'\0');

    /* otherwise, check for terminal input or file input */
    else {
        fp = getfile(fptr);
        if (fp == stdin || fp == STDERR)
            ch = ostgetc();
        else
            ch = osagetc(fp);
#ifdef DEBUG_INPUT
        if (read_by_xlisp && ch != -1) {
			putc(ch, read_by_xlisp);
		}
#endif
    }

    /* return the character */
    return (ch);
}

/* xlungetc - unget a character */
void xlungetc(LVAL fptr, int ch)
{
    LVAL lptr;
    
    /* check for ungetc from nil */
    if (fptr == NIL || ch == EOF)
        ;
        
    /* otherwise, check for ungetc to a stream */
    else if (ustreamp(fptr)) {
        if (ch != EOF) {
            lptr = cons(cvchar(ch),gethead(fptr));
            if (gethead(fptr) == NIL)
                settail(fptr,lptr);
            sethead(fptr,lptr);
        }
    }
    
    /* otherwise, it must be a file */
    else
        setsavech(fptr,ch);
}

/* xlpeek - peek at a character from a file or stream */
int xlpeek(LVAL fptr)
{
    LVAL lptr, cptr=NULL;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
        ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
        if ((lptr = gethead(fptr)) == NIL)
            ch = EOF;
        else {
            if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
                xlfail("bad stream");
            ch = getchcode(cptr);
        }
    }

    /* otherwise, get the next file character and save it */
    else {
        ch = xlgetc(fptr);
        setsavech(fptr,ch);
    }

    /* return the character */
    return (ch);
}

/* xlputc - put a character to a file or stream */
void xlputc(LVAL fptr, int ch)
{
    LVAL lptr;
    FILE *fp;

    /* count the character */
    ++xlfsize;

    /* check for output to nil */
    if (fptr == NIL)
        ;

    /* otherwise, check for output to an unnamed stream */
    else if (ustreamp(fptr)) {
        lptr = consa(cvchar(ch));
        if (gettail(fptr))
            rplacd(gettail(fptr),lptr);
        else
            sethead(fptr,lptr);
        settail(fptr,lptr);
    }

    /* otherwise, check for terminal output or file output */
    else {
        fp = getfile(fptr);
        if (!fp)
            xlfail("file not open");
        else if (fp == stdout || fp == STDERR)
            ostputc(ch);
        else
            osaputc(ch,fp);
    }
}

/* xloutflush -- flush output buffer */
void xloutflush(LVAL fptr)
{
    FILE *fp;

    /* check for output to nil or unnamed stream */
    if (fptr == NIL || ustreamp(fptr))
        ;

    /* otherwise, check for terminal output or file output */
    else {
        fp = getfile(fptr);
        if (!fp)
            xlfail("file not open");
        else if (fp == stdout || fp == STDERR)
            ostoutflush();
        else
            osoutflush(fp);
    }    
}

/* xlflush - flush the input buffer */
void xlflush(void)
{
    osflush();
}

/* stdprint - print to *standard-output* */
void stdprint(LVAL expr)
{
    xlprint(getvalue(s_stdout),expr,TRUE);
    xlterpri(getvalue(s_stdout));
}

/* stdputstr - print a string to *standard-output* */
void stdputstr(const char *str)
{
    xlputstr(getvalue(s_stdout),str);
}

/* stdflush - flush the *standard-output* buffer */
void stdflush()
{
    xloutflush(getvalue(s_stdout));
}

/* errprint - print to *error-output* */
void errprint(LVAL expr)
{
    xlprint(getvalue(s_stderr),expr,TRUE);
    xlterpri(getvalue(s_stderr));
}

/* errputstr - print a string to *error-output* */
void errputstr(const char *str)
{
    xlputstr(getvalue(s_stderr),str);
}

/* dbgprint - print to *debug-io* */
void dbgprint(LVAL expr)
{
    xlprint(getvalue(s_debugio),expr,TRUE);
    xlterpri(getvalue(s_debugio));
}

/* dbgputstr - print a string to *debug-io* */
void dbgputstr(const char *str)
{
    xlputstr(getvalue(s_debugio),str);
}

/* trcprin1 - print to *trace-output* */
void trcprin1(LVAL expr)
{
    xlprint(getvalue(s_traceout),expr,TRUE);
}

/* trcputstr - print a string to *trace-output* */
void trcputstr(const char *str)
{
    xlputstr(getvalue(s_traceout),str);
}


