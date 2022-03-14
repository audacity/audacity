/* xlread - xlisp expression input routine */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/
/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  eliminate some compiler warnings
 *              replaced system-specific code with generic calls (see path.c)
 */


#include "stdlib.h"
#include "string.h"
#include "switches.h"
#include "xlisp.h"
#ifdef WINDOWS
#include "winfun.h"
#endif
#ifdef MACINTOSH
#include "macstuff.h"
#endif

#ifdef DEBUG_INPUT
extern FILE *debug_input_fp;
#endif

/* symbol parser modes */
#define DONE	0
#define NORMAL	1
#define ESCAPE	2

/* external variables */
extern LVAL s_stdout,s_true,s_dot;
extern LVAL s_quote,s_function,s_bquote,s_comma,s_comat;
extern LVAL s_rtable,k_wspace,k_const,k_nmacro,k_tmacro;
extern LVAL k_sescape,k_mescape;
extern char buf[];

/* external routines */
extern FILE *osaopen(const char *name, const char *mode);
/* on the NeXT, atof is a macro in stdlib.h */
/* Is this a mistake? atof is declared in stdlib.h, but it is never a macro:
  #if !defined(atof) && !defined(_WIN32)
     extern double atof(const char *);
  #endif
*/
#ifndef __MWERKS__
#if !defined(ITYPE) && !defined(_WIN32) 
   extern ITYPE;
#endif
#endif

#define WSPACE "\t \f\r\n"
#define CONST1 "!$%&*+-./0123456789:<=>?@[]^_{}~"
#define CONST2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

/* forward declarations */
FORWARD LVAL callmacro(LVAL fptr, int ch);
FORWARD LOCAL LVAL psymbol(LVAL fptr);
FORWARD LOCAL LVAL punintern(LVAL fptr);
FORWARD LOCAL LVAL pnumber(LVAL fptr, int radix);
FORWARD LOCAL LVAL pquote(LVAL fptr, LVAL sym);
FORWARD LOCAL LVAL plist(LVAL fptr);
FORWARD LOCAL LVAL pvector(LVAL fptr);
FORWARD LOCAL void upcase(char *str);
FORWARD LOCAL int pname(LVAL fptr,int *pescflag);
FORWARD LOCAL void pcomment(LVAL fptr);
FORWARD LOCAL int checkeof(LVAL fptr);
FORWARD LOCAL int nextch(LVAL fptr);
FORWARD LOCAL void badeof(LVAL fptr);
FORWARD LOCAL int storech(char *buf, int i, int ch);

#ifdef WINDOWS
static char save_file_name[STRMAX+1]; /* keeps files opened by prompt */
static int sfn_valid = FALSE;
#endif

#ifdef DEBUG_INPUT
extern FILE *read_by_xlisp;
#endif


/* xlload - load a file of xlisp expressions */
int xlload(const char *fname, int vflag, int pflag)
{
    char fullname[STRMAX+1];
#ifdef WINDOWS
    char *ptr;
#endif
    LVAL fptr,expr;
    XLCONTEXT cntxt;
    FILE *fp;
    int sts;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fptr);
    xlsave(expr);

    /* space for copy + extension? */
    if (strlen(fname) > STRMAX - 4) {
	    expr = cvstring(fname);
		goto toolong;
	}
    strcpy(fullname,fname);
#ifdef WINDOWS
#ifdef WINGUI
    if (strcmp(fullname, "*") == 0) {
        if (sfn_valid) {
            strcpy(fullname, save_file_name);
        } else {
            strcpy(fullname, "*.*");
        }
    }
    if (strcmp(fullname, "*.*") == 0) {
        const char *name = getfilename(NULL, "lsp", "r", "Load file");
        if (name) {
            strcpy(fullname, name);
            strcpy(save_file_name, name);
            sfn_valid = TRUE;
        } else {
            xlpopn(2);
            return FALSE;
        }
    }
#endif
    /* replace "/" with "\" so that (current-path) will work */
    for (ptr = fullname; *ptr; ptr++) {
        if (*ptr == '/') *ptr = '\\';
    }
#endif

    /* allocate a file node */
    fptr = cvfile(NULL);

    /* open the file */
    fp = osaopen(fullname, "r");
    if (fp == NULL) {
        /* default the extension if there is room */
        if (needsextension(fullname)) {
            char fullname_plus[STRMAX+1];
            strcpy(fullname_plus, fullname);
            strcat(fullname_plus, ".lsp");
            fp = osaopen(fullname_plus, "r");
            if (fp) strcpy(fullname, fullname_plus);
        }
    }
    if (fp == NULL) {
        /* new cross-platform code by dmazzoni - new xlisp_path
           implementation is in path.c */
        const char *newname = find_in_xlisp_path(fullname);
        if (newname && newname[0]) {
            if (strlen(newname) > STRMAX) {
			    expr = cvstring(newname);
                goto toolong;
			}
            strcpy(fullname, newname);
            fp = osaopen(fullname, "r");
        }
    }
    if (fp == NULL) {
        /* the file STILL wasn't found */
#ifdef DEBUG_INPUT
        if (read_by_xlisp) {
		    fprintf(read_by_xlisp, ";;;;xlload: failed to open %s\n", fullname);
	    }
#endif
        xlpopn(2);
        return (FALSE);
    }

    setfile(fptr,fp);
    setvalue(s_loadingfiles, cons(fptr, getvalue(s_loadingfiles)));
    setvalue(s_loadingfiles, cons(cvstring(fullname), getvalue(s_loadingfiles)));

    /* print the information line */
    if (vflag)
        { snprintf(buf, STRMAX, "; loading \"%s\"\n", fullname); stdputstr(buf); }

#ifdef DEBUG_INPUT
	if (read_by_xlisp) {
		fprintf(read_by_xlisp, ";;;;xlload: begin loading %s\n", fullname);
	}
#endif

    /* read, evaluate and possibly print each expression in the file */
    xlbegin(&cntxt,CF_ERROR,s_true);
    if (_setjmp(cntxt.c_jmpbuf)) {
        sts = FALSE;
        #ifdef DEBUG_INPUT
            if (read_by_xlisp) {
		fprintf(read_by_xlisp, ";;;;xlload: catch longjump, back to %s\n", fullname);
            }
        #endif
    }
    else {
        #ifdef DEBUG_INPUT
            if (read_by_xlisp) {
		fprintf(read_by_xlisp, ";;;;xlload: about to read from %s (%x)\n", fullname, fptr);
            }
        #endif
        /* a nested load that fails will cause all loading files to be closed,
         * so check to make sure fptr is still valid each time through the loop */
        while (getfile(fptr) && xlread(fptr,&expr,FALSE)) {
            #ifdef DEBUG_INPUT
                if (debug_input_fp) {
                    int c = getc(debug_input_fp);
                    ungetc(c, debug_input_fp);
                }
            #endif
            
            expr = xleval(expr);
            
            #ifdef DEBUG_INPUT
                if (debug_input_fp) {
                    int c = getc(debug_input_fp);
                    ungetc(c, debug_input_fp);
                }
            #endif
            
            if (pflag)
                stdprint(expr);
                
            #ifdef DEBUG_INPUT
                if (debug_input_fp) {
                    int c = getc(debug_input_fp);
                    ungetc(c, debug_input_fp);
                }
            #endif
            #ifdef DEBUG_INPUT
                if (read_by_xlisp) {
                    fprintf(read_by_xlisp, ";;;;xlload: about to read from %s (%x)\n", fullname, fptr);
                }
            #endif
        }
        #ifdef DEBUG_INPUT
            if (read_by_xlisp) {
                fprintf(read_by_xlisp, ";;;;xlload: xlread returned false for %s (%x)\n", fullname, fptr);
            }
        #endif
        /* return success only if file did not disappear out from under us */
        sts = (getfile(fptr) != NULL);
    }
    xlend(&cntxt);

    /* close the file */
    if (getfile(fptr)) { /* test added by RBD, see close_loadingfiles() */
        osclose(getfile(fptr));
        setfile(fptr,NULL);
    }
    if (consp(getvalue(s_loadingfiles)) && 
        consp(cdr(getvalue(s_loadingfiles))) &&
        car(cdr(getvalue(s_loadingfiles))) == fptr) {
        setvalue(s_loadingfiles, cdr(cdr(getvalue(s_loadingfiles))));
    }

    /* restore the stack */
    xlpopn(2);

#ifdef DEBUG_INPUT
	if (read_by_xlisp) {
		fprintf(read_by_xlisp, ";;;;xlload: finished loading %s\n", fullname);
	}
#endif

    /* return status */
    return (sts);

toolong:
    xlcerror("ignore file", "file name too long", expr);
    xlpopn(2);
    return FALSE;
}

/* xlread - read an xlisp expression */
int xlread(LVAL fptr, LVAL *pval, int rflag)
{
    int sts;

    /* read an expression */
    while ((sts = readone(fptr,pval)) == FALSE) {
#ifdef DEBUG_INPUT
    if (debug_input_fp) {
        int c = getc(debug_input_fp);
        ungetc(c, debug_input_fp);
    }
#endif
    }

    /* return status */
    return (sts == EOF ? FALSE : TRUE);
}

/* readone - attempt to read a single expression */
int readone(LVAL fptr, LVAL *pval)
{
    LVAL val,type;
    int ch;

#ifdef DEBUG_INPUT
    if (debug_input_fp) {
        int c = getc(debug_input_fp);
        ungetc(c, debug_input_fp);
    }
#endif
    /* get a character and check for EOF */
    if ((ch = xlgetc(fptr)) == EOF)
        return (EOF);

    /* handle white space */
    if ((type = tentry(ch)) == k_wspace)
        return (FALSE);

    /* handle symbol constituents */
    else if (type == k_const) {
        xlungetc(fptr,ch);
        *pval = psymbol(fptr);
        return (TRUE);	    
    }

    /* handle single and multiple escapes */
    else if (type == k_sescape || type == k_mescape) {
        xlungetc(fptr,ch);
        *pval = psymbol(fptr);
        return (TRUE);
    }
    
    /* handle read macros */
    else if (consp(type)) {
        if ((val = callmacro(fptr,ch)) && consp(val)) {
            *pval = car(val);
            return (TRUE);
        }
        else
            return (FALSE);
    }

    /* handle illegal characters */
    else {
        xlerror("illegal character",cvfixnum((FIXTYPE)ch));
        /* this point will never be reached because xlerror() does a
           _longjmp(). The return is added to avoid false positive 
           error messages from static analyzers and compilers */
        return (FALSE);
    }
}

/* rmhash - read macro for '#' */
LVAL rmhash(void)
{
    LVAL fptr,mch,val;
    int escflag,ch;

    /* protect some pointers */
    xlsave1(val);

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* make the return value */
    val = consa(NIL);

    /* check the next character */
    switch (ch = xlgetc(fptr)) {
    case '\'':
                rplaca(val,pquote(fptr,s_function));
                break;
    case '(':
                rplaca(val,pvector(fptr));
                break;
    case 'b':
    case 'B':
                rplaca(val,pnumber(fptr,2));
                break;
    case 'o':
    case 'O':
                rplaca(val,pnumber(fptr,8));
                break;
    case 'x':
    case 'X':
                    rplaca(val,pnumber(fptr,16));
                break;
    case '\\':
                xlungetc(fptr,ch);
                pname(fptr,&escflag);
                ch = buf[0];
                if (strlen(buf) > 1) {
                    upcase((char *) buf);
                    if (strcmp(buf,"NEWLINE") == 0)
                        ch = '\n';
                    else if (strcmp(buf,"SPACE") == 0)
                        ch = ' ';
                    else if (strcmp(buf,"TAB") == 0)
                        ch = '\t';
                    else
                        xlerror("unknown character name",cvstring(buf));
                }
                rplaca(val,cvchar(ch));
                break;
    case ':':
                rplaca(val,punintern(fptr));
                break;
    case '|':
                    pcomment(fptr);
                val = NIL;
                break;
    default:
                xlerror("illegal character after #",cvfixnum((FIXTYPE)ch));
    }

    /* restore the stack */
    xlpop();

    /* return the value */
    return (val);
}

/* rmquote - read macro for '\'' */
LVAL rmquote(void)
{
    LVAL fptr,mch;

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_quote)));
}

/* rmdquote - read macro for '"' */
LVAL rmdquote(void)
{
    unsigned char buf[STRMAX+1],*p,*sptr;
    LVAL fptr,str,newstr,mch;
    int len,blen,ch,d2,d3;

    /* protect some pointers */
    xlsave1(str);

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* loop looking for a closing quote */
    len = blen = 0; p = buf;
    while ((ch = checkeof(fptr)) != '"') {

        /* handle escaped characters */
        switch (ch) {
        case '\\':
                switch (ch = checkeof(fptr)) {
                case 't':
                        ch = '\011';
                        break;
                case 'n':
                        ch = '\012';
                        break;
                case 'f':
                        ch = '\014';
                        break;
                case 'r':
                        ch = '\015';
                        break;
                default:
                        if (ch >= '0' && ch <= '7') {
                            d2 = checkeof(fptr);
                            d3 = checkeof(fptr);
                            if (d2 < '0' || d2 > '7'
                             || d3 < '0' || d3 > '7')
                                xlfail("invalid octal digit");
                            ch -= '0'; d2 -= '0'; d3 -= '0';
                            ch = (ch << 6) | (d2 << 3) | d3;
                        }
                        break;
                }
        }

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

    /* return the new string */
    return (consa(str));
}

/* rmbquote - read macro for '`' */
LVAL rmbquote(void)
{
    LVAL fptr,mch;

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_bquote)));
}

/* rmcomma - read macro for ',' */
LVAL rmcomma(void)
{
    LVAL fptr,mch,sym;
    int ch;

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* check the next character */
    if ((ch = xlgetc(fptr)) == '@')
        sym = s_comat;
    else {
        xlungetc(fptr,ch);
        sym = s_comma;
    }

    /* make the return value */
    return (consa(pquote(fptr,sym)));
}

/* rmlpar - read macro for '(' */
LVAL rmlpar(void)
{
    LVAL fptr,mch;

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* make the return value */
    return (consa(plist(fptr)));
}

/* 4035 is the "no return value" warning message */
/* rmrpar, pcomment, badeof, and upcase don't return anything */
/* #pragma warning(disable: 4035) */

/* rmrpar - read macro for ')' */
LVAL rmrpar(void)
{
    xlfail("misplaced right paren");
    return NULL; /* never used */
}

/* rmsemi - read macro for ';' */
LVAL rmsemi(void)
{
    LVAL fptr,mch;
    int ch;

    /* get the file and macro character */
    fptr = xlgetfile();
    mch = xlgachar();
    xllastarg();

    /* skip to end of line */
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n')
        ;

    /* return nil (nothing read) */
    return (NIL);
}

/* pcomment - parse a comment delimited by #| and |# */
LOCAL void pcomment(LVAL fptr)
{
    int lastch,ch,n;

    /* look for the matching delimiter (and handle nesting) */
    for (n = 1, lastch = -1; n > 0 && (ch = xlgetc(fptr)) != EOF; ) {
        if (lastch == '|' && ch == '#')
            { --n; ch = -1; }
        else if (lastch == '#' && ch == '|')
            { ++n; ch = -1; }
        lastch = ch;
    }
}

/* pnumber - parse a number */
LOCAL LVAL pnumber(LVAL fptr, int radix)
{
    int digit,ch;
    long num;
    
    for (num = 0L; (ch = xlgetc(fptr)) != EOF; ) {
        if (islower(ch)) ch = toupper(ch);
        if (!('0' <= ch && ch <= '9') && !('A' <= ch && ch <= 'F'))
            break;
        if ((digit = (ch <= '9' ? ch - '0' : ch - 'A' + 10)) >= radix)
            break;
        num = num * (long)radix + (long)digit;
    }
    xlungetc(fptr,ch);
    return (cvfixnum((FIXTYPE)num));
}

/* plist - parse a list */
LOCAL LVAL plist(LVAL fptr)
{
    LVAL val,expr,lastnptr,nptr;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(val);
    xlsave(expr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL; nextch(fptr) != ')'; )

        /* get the next expression */
        switch (readone(fptr,&expr)) {
        case EOF:
            badeof(fptr);
        case TRUE:

            /* check for a dotted tail */
            if (expr == s_dot) {
                /* make sure there's a node */
                if (lastnptr == NIL)
                    xlfail("invalid dotted pair");

                /* parse the expression after the dot */
                if (!xlread(fptr,&expr,TRUE))
                    badeof(fptr);
                rplacd(lastnptr,expr);

                /* make sure its followed by a close paren */
                if (nextch(fptr) != ')')
                    xlfail("invalid dotted pair");
            }

            /* otherwise, handle a normal list element */
            else {
                nptr = consa(expr);
                if (lastnptr == NIL)
                    val = nptr;
                else
                    rplacd(lastnptr,nptr);
                lastnptr = nptr;
            }
            break;
        }

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return successfully */
    return (val);
}

/* pvector - parse a vector */
LOCAL LVAL pvector(LVAL fptr)
{
    LVAL list,expr,val,lastnptr,nptr;
    int len,ch,i;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(list);
    xlsave(expr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL, len = 0; (ch = nextch(fptr)) != ')'; ) {

        /* check for end of file */
        if (ch == EOF)
            badeof(fptr);

        /* get the next expression */
        switch (readone(fptr,&expr)) {
        case EOF:
            badeof(fptr);
        case TRUE:
            nptr = consa(expr);
            if (lastnptr == NIL)
                list = nptr;
            else
                rplacd(lastnptr,nptr);
            lastnptr = nptr;
            len++;
            break;
        }
    }

    /* skip the closing paren */
    xlgetc(fptr);

    /* make a vector of the appropriate length */
    val = newvector(len);

    /* copy the list into the vector */
    for (i = 0; i < len; ++i, list = cdr(list))
        setelement(val,i,car(list));

    /* restore the stack */
    xlpopn(2);

    /* return successfully */
    return (val);
}

/* pquote - parse a quoted expression */
LOCAL LVAL pquote(LVAL fptr, LVAL sym)
{
    LVAL val,p;

    /* protect some pointers */
    xlsave1(val);

    /* allocate two nodes */
    val = consa(sym);
    rplacd(val,consa(NIL));

    /* initialize the second to point to the quoted expression */
    if (!xlread(fptr,&p,TRUE))
        badeof(fptr);
    rplaca(cdr(val),p);

    /* restore the stack */
    xlpop();

    /* return the quoted expression */
    return (val);
}

/* psymbol - parse a symbol name */
LOCAL LVAL psymbol(LVAL fptr)
{
    int escflag;
    LVAL val;
    pname(fptr,&escflag);
    return (escflag || !xlisnumber(buf,&val) ? xlenter(buf) : val);
}

/* punintern - parse an uninterned symbol */
LOCAL LVAL punintern(LVAL fptr)
{
    int escflag;
    pname(fptr,&escflag);
    return (xlmakesym(buf));
}

/* pname - parse a symbol/package name */
LOCAL int pname(LVAL fptr,int *pescflag)
{
    int mode,ch=0,i;
    LVAL type;

    /* initialize */
    *pescflag = FALSE;
    mode = NORMAL;
    i = 0;

    /* accumulate the symbol name */
    while (mode != DONE) {

        /* handle normal mode */
        while (mode == NORMAL)
            if ((ch = xlgetc(fptr)) == EOF)
                mode = DONE;
            else if ((type = tentry(ch)) == k_sescape) {
                i = storech(buf,i,checkeof(fptr));
                *pescflag = TRUE;
            }
            else if (type == k_mescape) {
                *pescflag = TRUE;
                mode = ESCAPE;
            }
            else if (type == k_const
                 ||  (consp(type) && car(type) == k_nmacro))
                i = storech(buf,i,islower(ch) ? toupper(ch) : ch);
            else
                mode = DONE;

        /* handle multiple escape mode */
        while (mode == ESCAPE)
            if ((ch = xlgetc(fptr)) == EOF)
                badeof(fptr);
            else if ((type = tentry(ch)) == k_sescape)
                i = storech(buf,i,checkeof(fptr));
            else if (type == k_mescape)
                mode = NORMAL;
            else
                i = storech(buf,i,ch);
    }
    buf[i] = 0;

    /* check for a zero length name */
    if (i == 0)
        xlerror("zero length name", s_unbound);

    /* unget the last character and return it */
    xlungetc(fptr,ch);
    return (ch);
}

/* storech - store a character in the print name buffer */
LOCAL int storech(char *buf, int i, int ch)
{
    if (i < STRMAX)
        buf[i++] = ch;
    return (i);
}

/* tentry - get a readtable entry */
LVAL tentry(int ch)
{
    LVAL rtable;
    rtable = getvalue(s_rtable);
    if (!vectorp(rtable) || ch < 0 || ch >= getsize(rtable))
        return (NIL);
    return (getelement(rtable,ch));
}

/* nextch - look at the next non-blank character */
LOCAL int nextch(LVAL fptr)
{
    int ch;

    /* return and save the next non-blank character */
    while ((ch = xlgetc(fptr)) != EOF && isspace(ch))
        ;
    xlungetc(fptr,ch);
    return (ch);
}

/* checkeof - get a character and check for end of file */
LOCAL int checkeof(LVAL fptr)
{
    int ch;

    if ((ch = xlgetc(fptr)) == EOF)
        badeof(fptr);
    return (ch);
}

/* badeof - unexpected eof */
LOCAL void badeof(LVAL fptr)
{
    xlgetc(fptr);
    xlfail("unexpected EOF");
}

/* xlisnumber - check if this string is a number */
int xlisnumber(char *str, LVAL *pval)
{
    int dl,dr;
    char *p;

    /* initialize */
    p = str; dl = dr = 0;

    /* check for a sign */
    if (*p == '+' || *p == '-')
        p++;

    /* check for a string of digits */
    while (isdigit(*p)) {
        p++;
        dl++;
    }
    /* check for a decimal point */
    if (*p == '.') {
        p++;
        while (isdigit(*p)) {
            p++;
            dr++;
        }
    }

    /* check for an exponent */
    if ((dl || dr) && *p == 'E') {
        p++;

        /* check for a sign */
        if (*p == '+' || *p == '-')
            p++;

        /* check for a string of digits */
        while (isdigit(*p)) {
            p++;
            dr++;
        }
    }

    /* make sure there was at least one digit and this is the end */
    if ((dl == 0 && dr == 0) || *p)
        return (FALSE);

    /* convert the string to an integer and return successfully */
    if (pval) {
        if (*str == '+') ++str;
        if (str[strlen(str)-1] == '.') str[strlen(str)-1] = 0;
        *pval = (dr ? cvflonum(atof(str)) : cvfixnum(ICNV(str)));
    }
    return (TRUE);
}

/* defmacro - define a read macro */
void defmacro(int ch, LVAL type, int offset)
{
    extern FUNDEF *funtab;
    LVAL subr;
    subr = cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset);
    setelement(getvalue(s_rtable),ch,cons(type,subr));
}

/* callmacro - call a read macro */
LVAL callmacro(LVAL fptr, int ch)
{
    LVAL *newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(cdr(getelement(getvalue(s_rtable),ch)));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(fptr);
    pusharg(cvchar(ch));
    xlfp = newfp;
    return (xlapply(2));
}

/* upcase - translate a string to upper case */
LOCAL void upcase(char *str)
{
    for (; *str != '\0'; ++str)
        if (islower(*str))
            *str = toupper(*str);
}

/* xlrinit - initialize the reader */
void xlrinit(void)
{
    LVAL rtable;
    char *p;
    int ch;

    /* create the read table */
    rtable = newvector(256);
    setvalue(s_rtable,rtable);

    /* initialize the readtable */
    for (p = WSPACE; (ch = *p++); )
        setelement(rtable,ch,k_wspace);
    for (p = CONST1; (ch = *p++); )
        setelement(rtable,ch,k_const);
    for (p = CONST2; (ch = *p++); )
        setelement(rtable,ch,k_const);

    /* setup the escape characters */
    setelement(rtable,'\\',k_sescape);
    setelement(rtable,'|', k_mescape);

    /* install the read macros */
    defmacro('#', k_nmacro,FT_RMHASH);
    defmacro('\'',k_tmacro,FT_RMQUOTE);
    defmacro('"', k_tmacro,FT_RMDQUOTE);
    defmacro('`', k_tmacro,FT_RMBQUOTE);
    defmacro(',', k_tmacro,FT_RMCOMMA);
    defmacro('(', k_tmacro,FT_RMLPAR);
    defmacro(')', k_tmacro,FT_RMRPAR);
    defmacro(';', k_tmacro,FT_RMSEMI);
}

