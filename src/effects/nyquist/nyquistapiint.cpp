/* nyquistapiint.cpp -- interface to  nyquistapi.h */

#ifndef mips
#include "stdlib.h"
#endif
#include "../../../lib-src/libnyquist/nyquist/xlisp/xlisp.h"

extern LVAL s_true;
#define cvboolean(i) ((i) ? s_true : NIL)
#define testarg2(e) (moreargs() ? (e) : (getflonum(xltoofew())))
#define xlgaanynum() (floatp(*xlargv) ? getflonum(nextarg()) : \
    (fixp(*xlargv) ? (double) getfixnum(nextarg()) : \
        getflonum(xlbadtype(*xlargv))))
#define getboolean(lval) ((lval) != NIL)

extern LVAL RSLT_sym;

#include "nyquistapi.h"

/* xlc_aud_get_track_info -- interface to C routine getTrackInfo */
/**/
LVAL xlc_aud_get_track_info(void)
{
    LVAL arg1 = xlgetarg();
    LVAL result;

    xllastarg();
    result = getTrackInfo(arg1);
    return (result);
}


/* xlc_aud_get_audio -- interface to C routine getAudio */
/**/
LVAL xlc_aud_get_audio(void)
{
    LVAL arg1 = xlgetarg();
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    LVAL result;

    xllastarg();
    result = getAudio(arg1, arg2, arg3);
    return (result);
}


/* xlc_aud_put_audio -- interface to C routine putAudio */
/**/
LVAL xlc_aud_put_audio(void)
{
    LVAL arg1 = xlgetarg();
    LVAL arg2 = xlgetarg();
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    LVAL result;

    xllastarg();
    result = putAudio(arg1, arg2, arg3, arg4);
    return (result);
}


/* xlc_aud_get_labels -- interface to C routine getLabels */
/**/
LVAL xlc_aud_get_labels(void)
{
    LVAL arg1 = xlgetarg();
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    LVAL result;

    xllastarg();
    result = getLabels(arg1, arg2, arg3);
    return (result);
}


/* xlc_aud_put_labels -- interface to C routine putLabels */
/**/
LVAL xlc_aud_put_labels(void)
{
    LVAL arg1 = xlgetarg();
    LVAL arg2 = xlgetarg();
    LVAL arg3 = xlgetarg();
    LVAL result;

    xllastarg();
    result = putLabels(arg1, arg2, arg3);
    return (result);
}


/* xlc_aud_get_notes -- interface to C routine getNotes */
/**/
LVAL xlc_aud_get_notes(void)
{
    LVAL arg1 = xlgetarg();
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    LVAL result;

    xllastarg();
    result = getNotes(arg1, arg2, arg3);
    return (result);
}


/* xlc_aud_put_notes -- interface to C routine putNotes */
/**/
LVAL xlc_aud_put_notes(void)
{
    LVAL arg1 = xlgetarg();
    LVAL arg2 = xlgetarg();
    LVAL arg3 = xlgetarg();
    LVAL result;

    xllastarg();
    result = putNotes(arg1, arg2, arg3);
    return (result);
}


/* xlc_aud_get_times -- interface to C routine getTimes */
/**/
LVAL xlc_aud_get_times(void)
{
    LVAL arg1 = xlgetarg();
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    LVAL result;

    xllastarg();
    result = getTimes(arg1, arg2, arg3);
    return (result);
}


/* xlc_aud_put_times -- interface to C routine putTimes */
/**/
LVAL xlc_aud_put_times(void)
{
    LVAL arg1 = xlgetarg();
    LVAL arg2 = xlgetarg();
    LVAL result;

    xllastarg();
    result = putTimes(arg1, arg2);
    return (result);
}


