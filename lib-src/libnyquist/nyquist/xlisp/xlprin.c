/* xlprint - xlisp print routine */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use

 * HISTORY
 * 28-Apr-03    Mazzoni
 *  Eliminated some compiler warnings
 *
 *  3-Apr-88	Dale Amon at CMU-CSD
 *	Added extern support to xlisp 2.0
 *
 * 18-Oct-87	Dale Amon at CMU-CSD
 *	Added print support for EXTERN nodes
 */


#include "string.h"
#include "xlisp.h"

/* external variables */
extern LVAL s_printcase,k_downcase,k_const,k_nmacro;
extern LVAL s_ifmt,s_ffmt;
extern FUNDEF *funtab;
extern char buf[];

LOCAL void putsymbol(LVAL fptr, char *str, int escflag);
LOCAL void putsubr(LVAL fptr, const char *tag, LVAL val);
LOCAL void putfixnum(LVAL fptr, FIXTYPE n);
LOCAL void putflonum(LVAL fptr, FLOTYPE n);
LOCAL void putchcode(LVAL fptr, int ch, int escflag);
LOCAL void putstring(LVAL fptr, LVAL str);
LOCAL void putqstring(LVAL fptr, LVAL str);
LOCAL void putclosure(LVAL fptr, LVAL val);
LOCAL void putoct(LVAL fptr, int n);


/* xlprint - print an xlisp value */
void xlprint(LVAL fptr, LVAL vptr, int flag)
{
    LVAL nptr,next;
    int n,i;

    /* print nil */
    if (vptr == NIL) {
        putsymbol(fptr,"NIL",flag);
        return;
    }

    /* check value type */
    switch (ntype(vptr)) {
    case SUBR:
            putsubr(fptr,"Subr",vptr);
            break;
    case FSUBR:
            putsubr(fptr,"FSubr",vptr);
            break;
    case CONS:
            xlputc(fptr,'(');
            for (nptr = vptr; nptr != NIL; nptr = next) {
                xlprint(fptr,car(nptr),flag);
                if ((next = cdr(nptr))) {
                    if (consp(next))
                        xlputc(fptr,' ');
                    else {
                        xlputstr(fptr," . ");
                        xlprint(fptr,next,flag);
                        break;
                    }
                }
            }
            xlputc(fptr,')');
            break;
    case SYMBOL:
            putsymbol(fptr,(char *) getstring(getpname(vptr)),flag);
            break;
    case FIXNUM:
            putfixnum(fptr,getfixnum(vptr));
            break;
    case FLONUM:
            putflonum(fptr,getflonum(vptr));
            break;
    case CHAR:
            putchcode(fptr,getchcode(vptr),flag);
            break;
    case STRING:
            if (flag)
                putqstring(fptr,vptr);
            else
                putstring(fptr,vptr);
            break;
    case STREAM:
            putatm(fptr,"File-Stream",vptr);
            break;
    case USTREAM:
            putatm(fptr,"Unnamed-Stream",vptr);
            break;
    case OBJECT:
            putatm(fptr,"Object",vptr);
            break;
    case VECTOR:
            xlputc(fptr,'#'); xlputc(fptr,'(');
            for (i = 0, n = getsize(vptr); n-- > 0; ) {
                xlprint(fptr,getelement(vptr,i++),flag);
                if (n) xlputc(fptr,' ');
            }
            xlputc(fptr,')');
            break;
    case CLOSURE:
            putclosure(fptr,vptr);
            break;
    case EXTERN:
            if (getdesc(vptr)) {
                (*(getdesc(vptr)->print_meth))(fptr, getinst(vptr));
            }
            break;
    case FREE_NODE:
            putatm(fptr,"Free",vptr);
            break;
    default:
            putatm(fptr,"Foo",vptr);
            break;
    }
}

/* xlterpri - terminate the current print line */
void xlterpri(LVAL fptr)
{
    xlputc(fptr,'\n');
}

/* xlputstr - output a string */
void xlputstr(LVAL fptr, const char *str)
{
    while (*str)
        xlputc(fptr,*str++);
}

/* putsymbol - output a symbol */
LOCAL void putsymbol(LVAL fptr, char *str, int escflag)
{
    int downcase;
    LVAL type;
    char *p;

    /* check for printing without escapes */
    if (!escflag) {
        xlputstr(fptr,str);
        return;
    }

    /* check to see if symbol needs escape characters */
    if (tentry(*str) == k_const) {
        for (p = str; *p; ++p)
            if (islower(*p)
            ||  ((type = tentry(*p)) != k_const
              && (!consp(type) || car(type) != k_nmacro))) {
                xlputc(fptr,'|');
                while (*str) {
                    if (*str == '\\' || *str == '|')
                        xlputc(fptr,'\\');
                    xlputc(fptr,*str++);
                }
                xlputc(fptr,'|');
                return;
            }
    }

    /* get the case translation flag */
    downcase = (getvalue(s_printcase) == k_downcase);

    /* check for the first character being '#' */
    if (*str == '#' || *str == '.' || xlisnumber(str,NULL))
        xlputc(fptr,'\\');

    /* output each character */
    while (*str) {
        /* don't escape colon until we add support for packages */
        if (*str == '\\' || *str == '|' /* || *str == ':' */)
            xlputc(fptr,'\\');
        xlputc(fptr,(downcase && isupper(*str) ? tolower(*str++) : *str++));
    }
}

/* putstring - output a string */
LOCAL void putstring(LVAL fptr, LVAL str)
{
    unsigned char *p;
    int ch;

    /* output each character */
    for (p = getstring(str); (ch = *p) != '\0'; ++p)
        xlputc(fptr,ch);
}

/* putqstring - output a quoted string */
LOCAL void putqstring(LVAL fptr, LVAL str)
{
    unsigned char *p;
    int ch;

    /* get the string pointer */
    p = getstring(str);

    /* output the initial quote */
    xlputc(fptr,'"');

    /* output each character in the string */
    for (p = getstring(str); (ch = *p) != '\0'; ++p)

        /* check for a control character */
        if (ch < 040 || ch == '\\' || ch > 0176 /* || ch == '"' */) {
            xlputc(fptr,'\\');
            switch (ch) {
            case '\011':
                    xlputc(fptr,'t');
                    break;
            case '\012':
                    xlputc(fptr,'n');
                    break;
            case '\014':
                    xlputc(fptr,'f');
                    break;
            case '\015':
                    xlputc(fptr,'r');
                    break;
            case '\\':
                    xlputc(fptr,'\\');
                    break;
            case '"':
                    xlputc(fptr, '"');
                    break;
            default:
                    putoct(fptr,ch);
                    break;
            }
        }

        /* output a normal character */
        else
            xlputc(fptr,ch);

    /* output the terminating quote */
    xlputc(fptr,'"');
}

/* putatm - output an atom */
void putatm(LVAL fptr, const char *tag, LVAL val)
{
    snprintf(buf, STRMAX, "#<%s: #", tag); xlputstr(fptr,buf);
    sprintf(buf,AFMT,val); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

/* putsubr - output a subr/fsubr */
LOCAL void putsubr(LVAL fptr, const char *tag, LVAL val)
{
    snprintf(buf, STRMAX, "#<%s-%s: #", tag, funtab[getoffset(val)].fd_name);
    xlputstr(fptr,buf);
    sprintf(buf,AFMT,val); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

/* putclosure - output a closure */
LOCAL void putclosure(LVAL fptr, LVAL val)
{
    LVAL name;
    if ((name = getname(val)))
        snprintf(buf, STRMAX, "#<Closure-%s: #",getstring(getpname(name)));
    else
        strcpy(buf,"#<Closure: #");
    xlputstr(fptr,buf);
    sprintf(buf,AFMT,val); xlputstr(fptr,buf);
    xlputc(fptr,'>');
/*
    xlputstr(fptr,"\nName:   "); xlprint(fptr,getname(val),TRUE);
    xlputstr(fptr,"\nType:   "); xlprint(fptr,gettype(val),TRUE);
    xlputstr(fptr,"\nLambda: "); xlprint(fptr,getlambda(val),TRUE);
    xlputstr(fptr,"\nArgs:   "); xlprint(fptr,getargs(val),TRUE);
    xlputstr(fptr,"\nOargs:  "); xlprint(fptr,getoargs(val),TRUE);
    xlputstr(fptr,"\nRest:   "); xlprint(fptr,getrest(val),TRUE);
    xlputstr(fptr,"\nKargs:  "); xlprint(fptr,getkargs(val),TRUE);
    xlputstr(fptr,"\nAargs:  "); xlprint(fptr,getaargs(val),TRUE);
    xlputstr(fptr,"\nBody:   "); xlprint(fptr,getbody(val),TRUE);
    xlputstr(fptr,"\nEnv:    "); xlprint(fptr,closure_getenv(val),TRUE);
    xlputstr(fptr,"\nFenv:   "); xlprint(fptr,getfenv(val),TRUE);
*/
}

/* putfixnum - output a fixnum */
LOCAL void putfixnum(LVAL fptr, FIXTYPE n)
{
    unsigned char *fmt;
    LVAL val;
    fmt = ((val = getvalue(s_ifmt)) && stringp(val) ? getstring(val)
                                                    : (unsigned char *)IFMT);
    snprintf(buf, STRMAX, (char *) fmt, n);
    xlputstr(fptr,buf);
}

/* putflonum - output a flonum */
LOCAL void putflonum(LVAL fptr, FLOTYPE n)
{
    unsigned char *fmt;
    LVAL val;
    fmt = ((val = getvalue(s_ffmt)) && stringp(val) ? getstring(val)
                                                    : (unsigned char *)"%g");
    snprintf(buf, STRMAX, (char *) fmt, n);
    xlputstr(fptr,buf);
}

/* putchcode - output a character */
LOCAL void putchcode(LVAL fptr, int ch, int escflag)
{
    if (escflag) {
        switch (ch) {
        case '\n':
            xlputstr(fptr,"#\\Newline");
            break;
        case ' ':
            xlputstr(fptr,"#\\Space");
            break;
        case '\t':
            xlputstr(fptr, "#\\Tab");
            break;
        default:
            sprintf(buf,"#\\%c",ch);
            xlputstr(fptr,buf);
            break;
        }
    }
    else
        xlputc(fptr,ch);
}

/* putoct - output an octal byte value */
LOCAL void putoct(LVAL fptr, int n)
{
    sprintf(buf,"%03o",n);
    xlputstr(fptr,buf);
}
