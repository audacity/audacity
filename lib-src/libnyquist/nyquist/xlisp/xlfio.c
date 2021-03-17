/* xlfio.c - xlisp file i/o */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 30Sep06  rbd added xbigendianp
 * 28Apr03  dm  eliminate some compiler warnings
 */


#include "switches.h"

#include <string.h>

#include "xlisp.h"

/* do some sanity checking: */
#ifndef XL_BIG_ENDIAN
#ifndef XL_LITTLE_ENDIAN
#error configuration error -- either XL_BIG_ or XL_LITTLE_ENDIAN must be defined in xlisp.h
#endif
#endif
#ifdef XL_BIG_ENDIAN
#ifdef XL_LITTLE_ENDIAN
#error configuration error -- both XL_BIG_ and XL_LITTLE_ENDIAN are defined!
#endif
#endif

/* forward declarations */
FORWARD LOCAL LVAL getstroutput(LVAL stream);
FORWARD LOCAL LVAL printit(int pflag, int tflag);
FORWARD LOCAL LVAL flatsize(int pflag);

/* xread - read an expression */
LVAL xread(void)
{
    LVAL fptr,eof,rflag,val;

    /* get file pointer and eof value */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    eof = (moreargs() ? xlgetarg() : NIL);
    rflag = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* read an expression */
    if (!xlread(fptr,&val,rflag != NIL))
        val = eof;

    /* return the expression */
    return (val);
}

/* xprint - built-in function 'print' */
LVAL xprint(void)
{
    return (printit(TRUE,TRUE));
}

/* xprin1 - built-in function 'prin1' */
LVAL xprin1(void)
{
    return (printit(TRUE,FALSE));
}

/* xprinc - built-in function princ */
LVAL xprinc(void)
{
    return (printit(FALSE,FALSE));
}

/* xterpri - terminate the current print line */
LVAL xterpri(void)
{
    LVAL fptr;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

/* printit - common print function */
LOCAL LVAL printit(int pflag, int tflag)
{
    LVAL fptr,val;

    /* get expression to print and file pointer */
    val = xlgetarg();
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* print the value */
    xlprint(fptr,val,pflag);

    /* terminate the print line if necessary */
    if (tflag)
        xlterpri(fptr);

    /* return the result */
    return (val);
}

/* xflatsize - compute the size of a printed representation using prin1 */
LVAL xflatsize(void)
{
    return (flatsize(TRUE));
}

/* xflatc - compute the size of a printed representation using princ */
LVAL xflatc(void)
{
    return (flatsize(FALSE));
}

/* flatsize - compute the size of a printed expression */
LOCAL LVAL flatsize(int pflag)
{
    LVAL val;

    /* get the expression */
    val = xlgetarg();
    xllastarg();

    /* print the value to compute its size */
    xlfsize = 0;
    xlprint(NIL,val,pflag);

    /* return the length of the expression */
    return (cvfixnum((FIXTYPE)xlfsize));
}

/* xlopen - open a text or binary file */
LVAL xlopen(int binaryflag)
{
    char *name,*mode=NULL;
    FILE *fp;
    LVAL dir;

    /* get the file name and direction */
    name = (char *)getstring(xlgetfname());
    if (!xlgetkeyarg(k_direction,&dir))
        dir = k_input;

    /* get the mode */
    if (dir == k_input)
        mode = "r";
    else if (dir == k_output)
        mode = "w";
    else
        xlerror("bad direction",dir);

    /* try to open the file */
    if (binaryflag) {
        fp = osbopen(name,mode);
    } else {
        fp = osaopen(name,mode);
    }
    return (fp ? cvfile(fp) : NIL);
}


/* xopen - open a file */
LVAL xopen(void)
{
    return xlopen(FALSE);
}

/* xbopen - open a binary file */
LVAL xbopen(void)
{
        return xlopen(TRUE);
}

/* xclose - close a file */
LVAL xclose(void)
{
    LVAL fptr;

    /* get file pointer */
    fptr = xlgastream();
    xllastarg();

    /* make sure the file exists */
    if (getfile(fptr) == NULL)
        xlfail("file not open");

    /* close the file */
    osclose(getfile(fptr));
    setfile(fptr,NULL);

    /* return nil */
    return (NIL);
}

/* xrdchar - read a character from a file */
LVAL xrdchar(void)
{
    LVAL fptr;
    int ch;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    xllastarg();

    /* get character and check for eof */
    return ((ch = xlgetc(fptr)) == EOF ? NIL : cvchar(ch));
}

/* xrdint - read an integer from a file */
/* positive byte count means big-endian, negative is little-endian */
LVAL xrdint(void)
{
    LVAL fptr;
    unsigned char b[4];
    long i;
    int n = 4;
    int index = 0; /* where to start in array */
    int incr = 1;  /* how to step through array */
    int rslt;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    /* get byte count */
    if (moreargs()) {
        LVAL count = typearg(fixp);
        n = (int) getfixnum(count);
        if (n < 0) {
            n = -n;
            index = n - 1;
            incr = -1;
        }
        if (n > 4) {
            xlerror("4-byte limit", count);
        }
    }
    xllastarg();
    for (i = 0; i < n; i++) {
        int ch = xlgetc(fptr);
        if (ch == EOF) 
            return NIL;
        b[index] = ch;
        index += incr;
    }
    /* build result, b is now big-endian */
    /* extend sign bit for short integers */
    rslt = ((b[0] & 0x80) ? -1 : 0);
    for (i = 0; i < n; i++) {
        rslt = (rslt << 8) + b[i];
    }
    /* return integer result */
    return cvfixnum(rslt);
}


/* xrdfloat - read a float from a file */
LVAL xrdfloat(void)
{
    LVAL fptr;
    union {
        char b[8];
        float f;
        double d;
    } rslt;
    int n = 4;
    int i;
    int index = 3;  /* where to start in array */
    int incr = -1;  /* how to step through array */

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    /* get byte count */
    if (moreargs()) {
        LVAL count =  typearg(fixp);
        n = (int) getfixnum(count);
        if (n < 0) {
            n = -n;
            index = 0;
            incr = 1;
        }
        if (n != 4 && n != 8) {
            xlerror("must be 4 or 8 bytes", count);
        }
    }
    xllastarg();

#ifdef XL_BIG_ENDIAN
    /* flip the bytes */
    index = n - 1 - index;
    incr = -incr;
#endif
    for (i = 0; i < n; i++) {
        int ch = xlgetc(fptr);
        if (ch == EOF) return NIL;
        rslt.b[index] = ch;
        index += incr;
    }
    /* return result */
    return cvflonum(n == 4 ? rslt.f : rslt.d);
}


/* xrdbyte - read a byte from a file */
LVAL xrdbyte(void)
{
    LVAL fptr;
    int ch;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    xllastarg();

    /* get character and check for eof */
    return ((ch = xlgetc(fptr)) == EOF ? NIL : cvfixnum((FIXTYPE)ch));
}

/* xpkchar - peek at a character from a file */
LVAL xpkchar(void)
{
    LVAL flag,fptr;
    int ch;

    /* peek flag and get file pointer */
    flag = (moreargs() ? xlgetarg() : NIL);
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    xllastarg();

    /* skip leading white space and get a character */
    if (flag)
        while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
            xlgetc(fptr);
    else
        ch = xlpeek(fptr);

    /* return the character */
    return (ch == EOF ? NIL : cvchar(ch));
}

/* xwrchar - write a character to a file */
LVAL xwrchar(void)
{
    LVAL fptr,chr;

    /* get the character and file pointer */
    chr = xlgachar();
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* put character to the file */
    xlputc(fptr,getchcode(chr));

    /* return the character */
    return (chr);
}

/* xwrbyte - write a byte to a file */
LVAL xwrbyte(void)
{
    LVAL fptr,chr;

    /* get the byte and file pointer */
    chr = xlgafixnum();
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* put byte to the file */
    xlputc(fptr,(int)getfixnum(chr));

    /* return the character */
    return (chr);
}

/* xwrint - write an integer to a file */
/* positive count means write big-endian */
LVAL xwrint(void)
{
    LVAL val, fptr;
    unsigned char b[4];
    long i;
    int n = 4;
    int index = 3;     /* where to start in array */
    int incr = -1;  /* how to step through array */
    int v; /* xwrint only allows up to 4 bytes, so int is enough */
    /* get the int and file pointer and optional byte count */
    val = xlgafixnum();
    v = (int) getfixnum(val);
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    if (moreargs()) {
        LVAL count = typearg(fixp);
        n = (int) getfixnum(count);
        index = n - 1;
        if (n < 0) {
            n = -n;
            index = 0;
            incr = 1;
        }
        if (n > 4) {
            xlerror("4-byte limit", count);
        }
    }
    xllastarg();
    /* build output b as little-endian */
    for (i = 0; i < n; i++) {
        b[i] = (unsigned char) v;
        v = v >> 8;
    }

    /* put bytes to the file */
    while (n) {
        n--;
        xlputc(fptr, b[index]);
        index += incr;
    }

    /* return the integer */
    return val;
}

/* xwrfloat - write a float to a file */
LVAL xwrfloat(void)
{
    LVAL val, fptr;
    union {
        char b[8];
        float f;
        double d;
    } v;
    int n = 4;
    int i;
    int index = 3;  /* where to start in array */
    int incr = -1;  /* how to step through array */

    /* get the float and file pointer and optional byte count */
    val = xlgaflonum();
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    if (moreargs()) {
        LVAL count = typearg(fixp);
        n = (int) getfixnum(count);
        if (n < 0) {
            n = -n;
            index = 0;
            incr = 1;
        }
        if (n != 4 && n != 8) {
            xlerror("must be 4 or 8 bytes", count);
        }
    }
    xllastarg();

#ifdef XL_BIG_ENDIAN
    /* flip the bytes */
    index = n - 1 - index;
    incr = -incr;
#endif
    /* build output v.b */
    if (n == 4) v.f = (float) getflonum(val);
    else v.d = getflonum(val);

    /* put bytes to the file */
    for (i = 0; i < n; i++) {
        xlputc(fptr, v.b[index]);
        index += incr;
    }

    /* return the flonum */
    return val;
}

/* xreadline - read a line from a file */
LVAL xreadline(void)
{
    unsigned char buf[STRMAX+1],*p,*sptr;
    LVAL fptr,str,newstr;
    int len,blen,ch;

    /* protect some pointers */
    xlsave1(str);

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
    xllastarg();

    /* get character and check for eof */
    len = blen = 0; p = buf;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n') {

        /* check for buffer overflow */
        if (blen >= STRMAX) {
             newstr = new_string(len + STRMAX + 1);
            sptr = getstring(newstr); *sptr = '\0';
            if (str) strcat((char *) sptr, (char *) getstring(str));
            *p = '\0'; strcat((char *) sptr, (char *) buf);
            p = buf; blen = 0;
            len += STRMAX;
            str = newstr;
        }

        /* store the character */
        *p++ = ch; ++blen;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
        xlpop();
        return (NIL);
    }

    /* append the last substring */
    if (str == NIL || blen) {
        newstr = new_string(len + blen + 1);
        sptr = getstring(newstr); *sptr = '\0';
        if (str) strcat((char *) sptr, (char *) getstring(str));
        *p = '\0'; strcat((char *) sptr, (char *) buf);
        str = newstr;
    }

    /* restore the stack */
    xlpop();

    /* return the string */
    return (str);
}


/* xmkstrinput - make a string input stream */
LVAL xmkstrinput(void)
{
    int start,end,len,i;
    unsigned char *str;
    LVAL string,val;

    /* protect the return value */
    xlsave1(val);
    
    /* get the string and length */
    string = xlgastring();
    str = getstring(string);
    len = getslength(string) - 1;

    /* get the starting offset */
    if (moreargs()) {
        val = xlgafixnum();
        start = (int)getfixnum(val);
    }
    else start = 0;

    /* get the ending offset */
    if (moreargs()) {
        val = xlgafixnum();
        end = (int)getfixnum(val);
    }
    else end = len;
    xllastarg();

    /* check the bounds */
    if (start < 0 || start > len)
        xlerror("string index out of bounds",cvfixnum((FIXTYPE)start));
    if (end < 0 || end > len)
        xlerror("string index out of bounds",cvfixnum((FIXTYPE)end));

    /* make the stream */
    val = newustream();

    /* copy the substring into the stream */
    for (i = start; i < end; ++i)
        xlputc(val,str[i]);

    /* restore the stack */
    xlpop();

    /* return the new stream */
    return (val);
}

/* xmkstroutput - make a string output stream */
LVAL xmkstroutput(void)
{
    return (newustream());
}

/* xgetstroutput - get output stream string */
LVAL xgetstroutput(void)
{
    LVAL stream;
    stream = xlgaustream();
    xllastarg();
    return (getstroutput(stream));
}

/* xgetlstoutput - get output stream list */
LVAL xgetlstoutput(void)
{
    LVAL stream,val;

    /* get the stream */
    stream = xlgaustream();
    xllastarg();

    /* get the output character list */
    val = gethead(stream);

    /* empty the character list */
    sethead(stream,NIL);
    settail(stream,NIL);

    /* return the list */
    return (val);
}

/* xformat - formatted output function */
LVAL xformat(void)
{
    unsigned char *fmt;
    LVAL stream,val;
    int ch;

    /* protect stream in case it is a new ustream */
    xlsave1(stream);

    /* get the stream and format string */
    stream = xlgetarg();
    if (stream == NIL)
        val = stream = newustream();
    else {
        if (stream == s_true)
            stream = getvalue(s_stdout);
        else if (!streamp(stream) && !ustreamp(stream))
            xlbadtype(stream);
        val = NIL;
    }
    fmt = getstring(xlgastring());

    /* process the format string */
    while ((ch = *fmt++))
        if (ch == '~') {
            switch (*fmt++) {
            case '\0':
                xlerror("expecting a format directive",cvstring((char *) (fmt-1)));
            case 'a': case 'A':
                xlprint(stream,xlgetarg(),FALSE);
                break;
            case 's': case 'S':
                xlprint(stream,xlgetarg(),TRUE);
                break;
            case '%':
                xlterpri(stream);
                break;
            case '~':
                xlputc(stream,'~');
                break;
            case '\n':
			case '\r':
				/* mac may read \r -- this should be ignored */
				if (*fmt == '\r') fmt++;  
                while (*fmt && *fmt != '\n' && isspace(*fmt))
                    ++fmt;
                break;
            default:
                xlerror("unknown format directive",cvstring((char *) (fmt-1)));
            }
        }
        else
            xlputc(stream,ch);
        
    /* return the value */
    if (val) val = getstroutput(val);
    xlpop();
    return val;
}

/* getstroutput - get the output stream string (internal) */
LOCAL LVAL getstroutput(LVAL stream)
{
    unsigned char *str;
    LVAL next,val;
    int len,ch;

    /* compute the length of the stream */
    for (len = 0, next = gethead(stream); next != NIL; next = cdr(next))
        ++len;

    /* create a new string */
    val = new_string(len + 1);
    
    /* copy the characters into the new string */
    str = getstring(val);
    while ((ch = xlgetc(stream)) != EOF)
        *str++ = ch;
    *str = '\0';

    /* return the string */
    return (val);
}


LVAL xlistdir(void)
{
    const char *path;
    LVAL result = NULL;
    LVAL *tail;
    /* get the path, converting unsigned char * to char * */
    path = (char *)getstring(xlgetfname());
    /* try to start listing */
    if (osdir_list_start(path)) {
        const char *filename;
        xlsave1(result);
        tail = &result;
        while ((filename = osdir_list_next())) {
            *tail = cons(NIL, NIL);
            rplaca(*tail, cvstring(filename));
            tail = &cdr(*tail);
        }
        osdir_list_finish();
        xlpop();
    }
    return result;
}


/* xbigendianp -- is this a big-endian machine? T or NIL */
LVAL xbigendianp() 
{
#ifdef XL_BIG_ENDIAN
    return s_true;
#else
    return NIL;
#endif
}


