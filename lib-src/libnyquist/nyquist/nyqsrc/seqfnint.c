/* nyqsrc/seqfnint.c -- interface to  cmt/seqdecls.h, 
 * nyqsrc/seqext.h, cmt/seq.h, nyqsrc/seqinterf.h, 
 * cmt/seqmread.h, cmt/seqmwrite.h, cmt/seqread.h, 
 * cmt/seqwrite.h */

#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"

extern LVAL s_true;
#define cvboolean(i) ((i) ? s_true : NIL)
#define testarg2(e) (moreargs() ? (e) : (getflonum(xltoofew())))
#define xlgaanynum() (floatp(*xlargv) ? getflonum(nextarg()) : \
    (fixp(*xlargv) ? (double) getfixnum(nextarg()) : \
        getflonum(xlbadtype(*xlargv))))
#define getboolean(lval) ((lval) != NIL)

extern LVAL RSLT_sym;

#include "seqdecls.h"

#include "seqext.h"

#include "seq.h"

/* xlc_seq_reset -- interface to C routine seq_reset */
/**/
LVAL xlc_seq_reset(void)
{
    seq_type arg1 = getseq(xlgaseq());

    xllastarg();
    seq_reset(arg1);
    return NIL;
}


/* xlc_seq_insert_ctrl -- interface to C routine insert_ctrl */
/**/
LVAL xlc_seq_insert_ctrl(void)
{
    seq_type arg1 = getseq(xlgaseq());
    long arg2 = (long) getfixnum(xlgafixnum());
    long arg3 = (long) getfixnum(xlgafixnum());
    long arg4 = (long) getfixnum(xlgafixnum());
    long arg5 = (long) getfixnum(xlgafixnum());
    long arg6 = (long) getfixnum(xlgafixnum());

    xllastarg();
    insert_ctrl(arg1, arg2, arg3, arg4, arg5, arg6);
    return NIL;
}


/* xlc_seq_insert_ramp -- interface to C routine insert_ctrlramp */
/**/
LVAL xlc_seq_insert_ramp(void)
{
    seq_type arg1 = getseq(xlgaseq());
    long arg2 = (long) getfixnum(xlgafixnum());
    long arg3 = (long) getfixnum(xlgafixnum());
    long arg4 = (long) getfixnum(xlgafixnum());
    long arg5 = (long) getfixnum(xlgafixnum());
    long arg6 = (long) getfixnum(xlgafixnum());
    long arg7 = (long) getfixnum(xlgafixnum());
    long arg8 = (long) getfixnum(xlgafixnum());
    long arg9 = (long) getfixnum(xlgafixnum());

    xllastarg();
    insert_ctrlramp(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    return NIL;
}


/* xlc_seq_insert_macctrl -- interface to C routine insert_macctrl */
/**/
LVAL xlc_seq_insert_macctrl(void)
{
    seq_type arg1 = getseq(xlgaseq());
    long arg2 = (long) getfixnum(xlgafixnum());
    long arg3 = (long) getfixnum(xlgafixnum());
    long arg4 = (long) getfixnum(xlgafixnum());
    long arg5 = (long) getfixnum(xlgafixnum());
    long arg6 = (long) getfixnum(xlgafixnum());

    xllastarg();
    insert_macctrl(arg1, arg2, arg3, arg4, arg5, arg6);
    return NIL;
}


/* xlc_seq_insert_note -- interface to C routine insert_note */
/**/
LVAL xlc_seq_insert_note(void)
{
    seq_type arg1 = getseq(xlgaseq());
    long arg2 = (long) getfixnum(xlgafixnum());
    long arg3 = (long) getfixnum(xlgafixnum());
    long arg4 = (long) getfixnum(xlgafixnum());
    long arg5 = (long) getfixnum(xlgafixnum());
    long arg6 = (long) getfixnum(xlgafixnum());
    long arg7 = (long) getfixnum(xlgafixnum());

    xllastarg();
    insert_note(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    return NIL;
}


/* xlc_seq_copy -- interface to C routine seq_copy */
/**/
LVAL xlc_seq_copy(void)
{
    seq_type arg1 = getseq(xlgaseq());
    seq_type result;

    xllastarg();
    result = seq_copy(arg1);
    return cvseq(result);
}


/* xlc_seq_create -- interface to C routine seq_create */
/**/
LVAL xlc_seq_create(void)
{
    seq_type result;

    xllastarg();
    result = seq_create();
    return cvseq(result);
}


#include "seqinterf.h"

/* xlc_seq_next -- interface to C routine seq_next */
/**/
LVAL xlc_seq_next(void)
{
    seq_type arg1 = getseq(xlgaseq());
    boolean result;

    xllastarg();
    result = seq_next(arg1);
    return cvboolean(result);
}


/* xlc_seq_get -- interface to C routine seq_get */
/**/
LVAL xlc_seq_get(void)
{
    seq_type arg1 = getseq(xlgaseq());
    long arg2 = (long) 0;
    long arg3 = (long) 0;
    long arg4 = (long) 0;
    long arg5 = (long) 0;
    long arg6 = (long) 0;
    long arg7 = (long) 0;
    long arg8 = (long) 0;
    LVAL result;

    xllastarg();
    seq_get(arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8);
    {	LVAL *next = &getvalue(RSLT_sym);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg2);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg3);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg4);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg5);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg6);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg7);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg8);
    }
    result = getvalue(RSLT_sym);
    return result;
}


#include "seqmwrite.h"

/* xlc_seq_write_smf -- interface to C routine seq_xlwrite_smf */
/**/
LVAL xlc_seq_write_smf(void)
{
    seq_type arg1 = getseq(xlgaseq());
    LVAL arg2 = xlgetarg();

    xllastarg();
    seq_xlwrite_smf(arg1, arg2);
    return NIL;
}


#include "seqmread.h"

/* xlc_seq_read_smf -- interface to C routine seq_read_smf */
/**/
LVAL xlc_seq_read_smf(void)
{
    seq_type arg1 = getseq(xlgaseq());
    FILE * arg2 = getfile(xlgastream());

    xllastarg();
    seq_read_smf(arg1, arg2);
    return NIL;
}


#include "seqread.h"

/* xlc_seq_read -- interface to C routine seq_read */
/**/
LVAL xlc_seq_read(void)
{
    seq_type arg1 = getseq(xlgaseq());
    FILE * arg2 = getfile(xlgastream());

    xllastarg();
    seq_read(arg1, arg2);
    return NIL;
}


#include "seqwrite.h"

/* xlc_seq_write -- interface to C routine seq_write */
/**/
LVAL xlc_seq_write(void)
{
    seq_type arg1 = getseq(xlgaseq());
    FILE * arg2 = getfile(xlgastream());
    int arg3 = getboolean(xlgetarg());

    xllastarg();
    seq_write(arg1, arg2, arg3);
    return NIL;
}


