/* c:\documents and settings\administrator\my documents\audacity\win\projects\libnyquist\/sndfnint.c -- interface to  
 * snd/snd.h, nyqsrc/sound.h, nyqsrc/downsample.h, 
 * nyqsrc/sndread.h, nyqsrc/sndseq.h, nyqsrc/add.h, 
 * nyqsrc/multiseq.h, nyqsrc/samples.h, nyqsrc/sndwrite.h, 
 * nyqsrc/sndmax.h, nyqsrc/compose.h, nyqsrc/inverse.h, 
 * nyqsrc/resamp.h, nyqsrc/resampv.h, nyqsrc/fft.h, 
 * nyqsrc/avg.h, tran/amosc.h, tran/clip.h, tran/const.h, 
 * tran/fmosc.h, tran/integrate.h, tran/log.h, 
 * tran/maxv.h, tran/osc.h, tran/prod.h, tran/buzz.h, 
 * tran/pwl.h, tran/recip.h, tran/upsample.h, 
 * tran/scale.h, tran/sine.h, tran/partial.h, 
 * tran/white.h, tran/tone.h, tran/tonev.h, 
 * tran/atonev.h, tran/atone.h, tran/reson.h, 
 * tran/areson.h, tran/resonvc.h, tran/resoncv.h, 
 * tran/aresonvc.h, tran/aresoncv.h, tran/resonvv.h, 
 * tran/aresonvv.h, tran/offset.h, tran/slope.h, 
 * tran/delay.h, tran/delaycv.h, tran/shape.h, 
 * tran/sampler.h, tran/exp.h, tran/siosc.h, 
 * tran/follow.h, tran/gate.h, tran/quantize.h, 
 * tran/ifft.h, tran/congen.h, tran/fromobject.h, 
 * tran/fromarraystream.h, tran/coterm.h, tran/convolve.h, 
 * tran/alpass.h, tran/oneshot.h, tran/chase.h, 
 * tran/tapv.h, tran/biquad.h, tran/pluck.h, tran/abs.h, 
 * tran/sqrt.h, tran/alpasscv.h, tran/alpassvc.h */

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


#include "snd/snd.h"

#include "nyqsrc/sound.h"

/* xlc_soundp -- interface to C routine soundp */
/**/
LVAL xlc_soundp(void)
{
    LVAL arg1 = xlgetarg();
    boolean result;

    xllastarg();
    result = soundp(arg1);
    return cvboolean(result);
}


/* xlc_hz_to_step -- interface to C routine hz_to_step */
/**/
LVAL xlc_hz_to_step(void)
{
    double arg1 = testarg2(xlgaanynum());
    double result;

    xllastarg();
    result = hz_to_step(arg1);
    return cvflonum(result);
}


/* xlc_snd_set_logical_stop -- interface to C routine set_logical_stop_time */
/**/
LVAL xlc_snd_set_logical_stop(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());

    xllastarg();
    set_logical_stop_time(arg1, arg2);
    return NIL;
}


/* xlc_log -- interface to C routine xlog */
/**/
LVAL xlc_log(void)
{
    double arg1 = getflonum(xlgaflonum());
    double result;

    xllastarg();
    result = xlog(arg1);
    return cvflonum(result);
}


/* xlc_snd_sref -- interface to C routine snd_sref */
/**/
LVAL xlc_snd_sref(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double result;

    xllastarg();
    result = snd_sref(arg1, arg2);
    return cvflonum(result);
}


/* xlc_sref_inverse -- interface to C routine snd_sref_inverse */
/**/
LVAL xlc_sref_inverse(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double result;

    xllastarg();
    result = snd_sref_inverse(arg1, arg2);
    return cvflonum(result);
}


/* xlc_snd_stop_time -- interface to C routine snd_stop_time */
/**/
LVAL xlc_snd_stop_time(void)
{
    sound_type arg1 = getsound(xlgasound());
    double result;

    xllastarg();
    result = snd_stop_time(arg1);
    return cvflonum(result);
}


/* xlc_snd_time -- interface to C routine snd_time */
/**/
LVAL xlc_snd_time(void)
{
    sound_type arg1 = getsound(xlgasound());
    double result;

    xllastarg();
    result = snd_time(arg1);
    return cvflonum(result);
}


/* xlc_snd_srate -- interface to C routine snd_srate */
/**/
LVAL xlc_snd_srate(void)
{
    sound_type arg1 = getsound(xlgasound());
    double result;

    xllastarg();
    result = snd_srate(arg1);
    return cvflonum(result);
}


/* xlc_snd_t0 -- interface to C routine snd_t0 */
/**/
LVAL xlc_snd_t0(void)
{
    sound_type arg1 = getsound(xlgasound());
    double result;

    xllastarg();
    result = snd_t0(arg1);
    return cvflonum(result);
}


/* xlc_snd_xform -- interface to C routine snd_xform */
/**/
LVAL xlc_snd_xform(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    double arg6 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_xform(arg1, arg2, arg3, arg4, arg5, arg6);
    return cvsound(result);
}


/* xlc_block_watch -- interface to C routine block_watch */
/**/
LVAL xlc_block_watch(void)
{
    long arg1 = getfixnum(xlgafixnum());

    xllastarg();
    block_watch(arg1);
    return NIL;
}


/* xlc_sound_nth_block -- interface to C routine sound_nth_block */
/**/
LVAL xlc_sound_nth_block(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    long result;

    xllastarg();
    result = sound_nth_block(arg1, arg2);
    return cvfixnum(result);
}


/* xlc_snd_copy -- interface to C routine sound_copy */
/**/
LVAL xlc_snd_copy(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = sound_copy(arg1);
    return cvsound(result);
}


/* xlc_snd_print -- interface to C routine sound_print */
/**/
LVAL xlc_snd_print(void)
{
    LVAL arg1 = xlgetarg();
    long arg2 = getfixnum(xlgafixnum());

    xllastarg();
    sound_print(arg1, arg2);
    return NIL;
}


/* xlc_snd_play -- interface to C routine sound_play */
/**/
LVAL xlc_snd_play(void)
{
    LVAL arg1 = xlgetarg();

    xllastarg();
    sound_play(arg1);
    return NIL;
}


/* xlc_stats -- interface to C routine stats */
/**/
LVAL xlc_stats(void)
{

    xllastarg();
    stats();
    return NIL;
}


/* xlc_snd_print_tree -- interface to C routine sound_print_tree */
/**/
LVAL xlc_snd_print_tree(void)
{
    sound_type arg1 = getsound(xlgasound());

    xllastarg();
    sound_print_tree(arg1);
    return NIL;
}


/* xlc_snd_scale -- interface to C routine sound_scale */
/**/
LVAL xlc_snd_scale(void)
{
    double arg1 = testarg2(xlgaanynum());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = sound_scale(arg1, arg2);
    return cvsound(result);
}


/* xlc_snd_zero -- interface to C routine sound_zero */
/**/
LVAL xlc_snd_zero(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = sound_zero(arg1, arg2);
    return cvsound(result);
}


/* xlc_step_to_hz -- interface to C routine step_to_hz */
/**/
LVAL xlc_step_to_hz(void)
{
    double arg1 = testarg2(xlgaanynum());
    double result;

    xllastarg();
    result = step_to_hz(arg1);
    return cvflonum(result);
}


#include "nyqsrc/downsample.h"

/* xlc_snd_down -- interface to C routine snd_down */
/**/
LVAL xlc_snd_down(void)
{
    double arg1 = testarg2(xlgaanynum());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_down(arg1, arg2);
    return cvsound(result);
}


#include "nyqsrc/sndread.h"

/* xlc_snd_read -- interface to C routine snd_make_read */
/**/
LVAL xlc_snd_read(void)
{
    unsigned char * arg1 = getstring(xlgastring());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    long arg4 = getfixnum(xlgafixnum());
    long arg5 = getfixnum(xlgafixnum());
    long arg6 = getfixnum(xlgafixnum());
    long arg7 = getfixnum(xlgafixnum());
    long arg8 = getfixnum(xlgafixnum());
    double arg9 = testarg2(xlgaanynum());
    double arg10 = testarg2(xlgaanynum());
    long arg11 = 0;
    long arg12 = 0;
    LVAL result;

    xllastarg();
    xlprot1(result);
    result = snd_make_read(arg1, arg2, arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &arg10, &arg11, &arg12);
    {	LVAL *next = &getvalue(RSLT_sym);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg4);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg5);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg6);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg7);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg8);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvflonum(arg9);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvflonum(arg10);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg11);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg12);
    }
    xlpop();
    return (result);
}


#include "nyqsrc/sndseq.h"

/* xlc_snd_seq -- interface to C routine snd_sndseq */
/**/
LVAL xlc_snd_seq(void)
{
    sound_type arg1 = getsound(xlgasound());
    LVAL arg2 = xlgetarg();
    sound_type result;

    xllastarg();
    result = snd_sndseq(arg1, arg2);
    return cvsound(result);
}


#include "nyqsrc/add.h"

/* xlc_snd_add -- interface to C routine snd_add */
/**/
LVAL xlc_snd_add(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_add(arg1, arg2);
    return cvsound(result);
}


#include "nyqsrc/multiseq.h"

/* xlc_snd_multiseq -- interface to C routine snd_make_multiseq */
/**/
LVAL xlc_snd_multiseq(void)
{
    LVAL arg1 = xlgetarg();
    LVAL arg2 = xlgetarg();
    LVAL result;

    xllastarg();
    result = snd_make_multiseq(arg1, arg2);
    return (result);
}


#include "nyqsrc/samples.h"

/* xlc_snd_from_array -- interface to C routine snd_from_array */
/**/
LVAL xlc_snd_from_array(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    LVAL arg3 = xlgetarg();
    sound_type result;

    xllastarg();
    result = snd_from_array(arg1, arg2, arg3);
    return cvsound(result);
}


/* xlc_snd_samples -- interface to C routine snd_samples */
/**/
LVAL xlc_snd_samples(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    LVAL result;

    xllastarg();
    result = snd_samples(arg1, arg2);
    return (result);
}


/* xlc_snd_length -- interface to C routine snd_length */
/**/
LVAL xlc_snd_length(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    long result;

    xllastarg();
    result = snd_length(arg1, arg2);
    return cvfixnum(result);
}


/* xlc_snd_maxsamp -- interface to C routine snd_maxsamp */
/**/
LVAL xlc_snd_maxsamp(void)
{
    sound_type arg1 = getsound(xlgasound());
    double result;

    xllastarg();
    result = snd_maxsamp(arg1);
    return cvflonum(result);
}


/* xlc_snd_fetch -- interface to C routine snd_fetch */
/**/
LVAL xlc_snd_fetch(void)
{
    sound_type arg1 = getsound(xlgasound());
    LVAL result;

    xllastarg();
    result = snd_fetch(arg1);
    return (result);
}


/* xlc_snd_fetch_array -- interface to C routine snd_fetch_array */
/**/
LVAL xlc_snd_fetch_array(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    long arg3 = getfixnum(xlgafixnum());
    LVAL result;

    xllastarg();
    result = snd_fetch_array(arg1, arg2, arg3);
    return (result);
}


#include "nyqsrc/sndwrite.h"

/* xlc_snd_save -- interface to C routine sound_save */
/**/
LVAL xlc_snd_save(void)
{
    LVAL arg1 = xlgetarg();
    long arg2 = getfixnum(xlgafixnum());
    unsigned char * arg3 = getstring(xlgastring());
    long arg4 = getfixnum(xlgafixnum());
    long arg5 = getfixnum(xlgafixnum());
    long arg6 = getfixnum(xlgafixnum());
    long arg7 = getfixnum(xlgafixnum());
    double arg8 = 0.0;
    long arg9 = 0;
    double arg10 = 0.0;
    LVAL arg11 = xlgetarg();
    double result;

    xllastarg();
    result = sound_save(arg1, arg2, arg3, arg4, arg5, arg6, arg7, &arg8, &arg9, &arg10, arg11);
    {	LVAL *next = &getvalue(RSLT_sym);
	*next = cons(NIL, NIL);
	car(*next) = cvflonum(arg8);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvfixnum(arg9);	next = &cdr(*next);
	*next = cons(NIL, NIL);
	car(*next) = cvflonum(arg10);
    }
    return cvflonum(result);
}


/* xlc_snd_overwrite -- interface to C routine sound_overwrite */
/**/
LVAL xlc_snd_overwrite(void)
{
    LVAL arg1 = xlgetarg();
    long arg2 = getfixnum(xlgafixnum());
    unsigned char * arg3 = getstring(xlgastring());
    long arg4 = getfixnum(xlgafixnum());
    long arg5 = getfixnum(xlgafixnum());
    long arg6 = getfixnum(xlgafixnum());
    long arg7 = getfixnum(xlgafixnum());
    long arg8 = getfixnum(xlgafixnum());
    double arg9 = testarg2(xlgaanynum());
    long arg10 = getfixnum(xlgafixnum());
    double arg11 = 0.0;
    double result;

    xllastarg();
    result = sound_overwrite(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, &arg11);
    {	LVAL *next = &getvalue(RSLT_sym);
	*next = cons(NIL, NIL);
	car(*next) = cvflonum(arg11);
    }
    return cvflonum(result);
}


#include "nyqsrc/sndmax.h"

/* xlc_snd_max -- interface to C routine sound_max */
/**/
LVAL xlc_snd_max(void)
{
    LVAL arg1 = xlgetarg();
    long arg2 = getfixnum(xlgafixnum());
    double result;

    xllastarg();
    result = sound_max(arg1, arg2);
    return cvflonum(result);
}


#include "nyqsrc/compose.h"

/* xlc_snd_compose -- interface to C routine snd_compose */
/**/
LVAL xlc_snd_compose(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_compose(arg1, arg2);
    return cvsound(result);
}


#include "nyqsrc/inverse.h"

/* xlc_snd_inverse -- interface to C routine snd_inverse */
/**/
LVAL xlc_snd_inverse(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_inverse(arg1, arg2, arg3);
    return cvsound(result);
}


#include "nyqsrc/resamp.h"

/* xlc_snd_resample -- interface to C routine snd_resample */
/**/
LVAL xlc_snd_resample(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_resample(arg1, arg2);
    return cvsound(result);
}


#include "nyqsrc/resampv.h"

/* xlc_snd_resamplev -- interface to C routine snd_resamplev */
/**/
LVAL xlc_snd_resamplev(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_resamplev(arg1, arg2, arg3);
    return cvsound(result);
}


#include "nyqsrc/fft.h"

/* xlc_snd_fft -- interface to C routine snd_fft */
/**/
LVAL xlc_snd_fft(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    long arg3 = getfixnum(xlgafixnum());
    LVAL arg4 = xlgetarg();
    LVAL result;

    xllastarg();
    result = snd_fft(arg1, arg2, arg3, arg4);
    return (result);
}


#include "nyqsrc/avg.h"

/* xlc_snd_avg -- interface to C routine snd_avg */
/**/
LVAL xlc_snd_avg(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    long arg3 = getfixnum(xlgafixnum());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_avg(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/amosc.h"

/* xlc_snd_amosc -- interface to C routine snd_amosc */
/**/
LVAL xlc_snd_amosc(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    sound_type arg6 = getsound(xlgasound());
    double arg7 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_amosc(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    return cvsound(result);
}


#include "tran/clip.h"

/* xlc_snd_clip -- interface to C routine snd_clip */
/**/
LVAL xlc_snd_clip(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_clip(arg1, arg2);
    return cvsound(result);
}


#include "tran/const.h"

/* xlc_snd_const -- interface to C routine snd_const */
/**/
LVAL xlc_snd_const(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_const(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/fmosc.h"

/* xlc_snd_fmosc -- interface to C routine snd_fmosc */
/**/
LVAL xlc_snd_fmosc(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    sound_type arg6 = getsound(xlgasound());
    double arg7 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_fmosc(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    return cvsound(result);
}


#include "tran/integrate.h"

/* xlc_snd_integrate -- interface to C routine snd_integrate */
/**/
LVAL xlc_snd_integrate(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_integrate(arg1);
    return cvsound(result);
}


#include "tran/log.h"

/* xlc_snd_log -- interface to C routine snd_log */
/**/
LVAL xlc_snd_log(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_log(arg1);
    return cvsound(result);
}


#include "tran/maxv.h"

/* xlc_snd_maxv -- interface to C routine snd_maxv */
/**/
LVAL xlc_snd_maxv(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_maxv(arg1, arg2);
    return cvsound(result);
}


#include "tran/osc.h"

/* xlc_snd_osc -- interface to C routine snd_osc */
/**/
LVAL xlc_snd_osc(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    double arg6 = testarg2(xlgaanynum());
    double arg7 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_osc(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    return cvsound(result);
}


#include "tran/prod.h"

/* xlc_snd_prod -- interface to C routine snd_prod */
/**/
LVAL xlc_snd_prod(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_prod(arg1, arg2);
    return cvsound(result);
}


#include "tran/buzz.h"

/* xlc_snd_buzz -- interface to C routine snd_buzz */
/**/
LVAL xlc_snd_buzz(void)
{
    long arg1 = getfixnum(xlgafixnum());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    sound_type arg5 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_buzz(arg1, arg2, arg3, arg4, arg5);
    return cvsound(result);
}


#include "tran/pwl.h"

/* xlc_snd_pwl -- interface to C routine snd_pwl */
/**/
LVAL xlc_snd_pwl(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    LVAL arg3 = xlgetarg();
    sound_type result;

    xllastarg();
    result = snd_pwl(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/recip.h"

/* xlc_snd_recip -- interface to C routine snd_recip */
/**/
LVAL xlc_snd_recip(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_recip(arg1);
    return cvsound(result);
}


#include "tran/upsample.h"

/* xlc_snd_up -- interface to C routine snd_up */
/**/
LVAL xlc_snd_up(void)
{
    double arg1 = testarg2(xlgaanynum());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_up(arg1, arg2);
    return cvsound(result);
}


#include "tran/scale.h"

/* xlc_snd_normalize -- interface to C routine snd_normalize */
/**/
LVAL xlc_snd_normalize(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_normalize(arg1);
    return cvsound(result);
}


#include "tran/sine.h"

/* xlc_snd_sine -- interface to C routine snd_sine */
/**/
LVAL xlc_snd_sine(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_sine(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/partial.h"

/* xlc_snd_partial -- interface to C routine snd_partial */
/**/
LVAL xlc_snd_partial(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_partial(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/white.h"

/* xlc_snd_white -- interface to C routine snd_white */
/**/
LVAL xlc_snd_white(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_white(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/tone.h"

/* xlc_snd_tone -- interface to C routine snd_tone */
/**/
LVAL xlc_snd_tone(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_tone(arg1, arg2);
    return cvsound(result);
}


#include "tran/tonev.h"

/* xlc_snd_tonev -- interface to C routine snd_tonev */
/**/
LVAL xlc_snd_tonev(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_tonev(arg1, arg2);
    return cvsound(result);
}


#include "tran/atonev.h"

/* xlc_snd_atonev -- interface to C routine snd_atonev */
/**/
LVAL xlc_snd_atonev(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_atonev(arg1, arg2);
    return cvsound(result);
}


#include "tran/atone.h"

/* xlc_snd_atone -- interface to C routine snd_atone */
/**/
LVAL xlc_snd_atone(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_atone(arg1, arg2);
    return cvsound(result);
}


#include "tran/reson.h"

/* xlc_snd_reson -- interface to C routine snd_reson */
/**/
LVAL xlc_snd_reson(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_reson(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/areson.h"

/* xlc_snd_areson -- interface to C routine snd_areson */
/**/
LVAL xlc_snd_areson(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_areson(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/resonvc.h"

/* xlc_snd_resonvc -- interface to C routine snd_resonvc */
/**/
LVAL xlc_snd_resonvc(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    double arg3 = testarg2(xlgaanynum());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_resonvc(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/resoncv.h"

/* xlc_snd_resoncv -- interface to C routine snd_resoncv */
/**/
LVAL xlc_snd_resoncv(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_resoncv(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/aresonvc.h"

/* xlc_snd_aresonvc -- interface to C routine snd_aresonvc */
/**/
LVAL xlc_snd_aresonvc(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    double arg3 = testarg2(xlgaanynum());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_aresonvc(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/aresoncv.h"

/* xlc_snd_aresoncv -- interface to C routine snd_aresoncv */
/**/
LVAL xlc_snd_aresoncv(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_aresoncv(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/resonvv.h"

/* xlc_snd_resonvv -- interface to C routine snd_resonvv */
/**/
LVAL xlc_snd_resonvv(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type arg3 = getsound(xlgasound());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_resonvv(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/aresonvv.h"

/* xlc_snd_aresonvv -- interface to C routine snd_aresonvv */
/**/
LVAL xlc_snd_aresonvv(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type arg3 = getsound(xlgasound());
    long arg4 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_aresonvv(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/offset.h"

/* xlc_snd_offset -- interface to C routine snd_offset */
/**/
LVAL xlc_snd_offset(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_offset(arg1, arg2);
    return cvsound(result);
}


#include "tran/slope.h"

/* xlc_snd_slope -- interface to C routine snd_slope */
/**/
LVAL xlc_snd_slope(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_slope(arg1);
    return cvsound(result);
}


#include "tran/delay.h"

/* xlc_snd_delay -- interface to C routine snd_delay */
/**/
LVAL xlc_snd_delay(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_delay(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/delaycv.h"

/* xlc_snd_delaycv -- interface to C routine snd_delaycv */
/**/
LVAL xlc_snd_delaycv(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_delaycv(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/shape.h"

/* xlc_snd_shape -- interface to C routine snd_shape */
/**/
LVAL xlc_snd_shape(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_shape(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/sampler.h"

/* xlc_snd_sampler -- interface to C routine snd_sampler */
/**/
LVAL xlc_snd_sampler(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    double arg6 = testarg2(xlgaanynum());
    sound_type arg7 = getsound(xlgasound());
    long arg8 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_sampler(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    return cvsound(result);
}


#include "tran/exp.h"

/* xlc_snd_exp -- interface to C routine snd_exp */
/**/
LVAL xlc_snd_exp(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_exp(arg1);
    return cvsound(result);
}


#include "tran/siosc.h"

/* xlc_snd_siosc -- interface to C routine snd_siosc */
/**/
LVAL xlc_snd_siosc(void)
{
    LVAL arg1 = xlgetarg();
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    sound_type arg5 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_siosc(arg1, arg2, arg3, arg4, arg5);
    return cvsound(result);
}


#include "tran/follow.h"

/* xlc_snd_follow -- interface to C routine snd_follow */
/**/
LVAL xlc_snd_follow(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    long arg5 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_follow(arg1, arg2, arg3, arg4, arg5);
    return cvsound(result);
}


#include "tran/gate.h"

/* xlc_snd_gate -- interface to C routine snd_gate */
/**/
LVAL xlc_snd_gate(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    double arg6 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_gate(arg1, arg2, arg3, arg4, arg5, arg6);
    return cvsound(result);
}


#include "tran/quantize.h"

/* xlc_snd_quantize -- interface to C routine snd_quantize */
/**/
LVAL xlc_snd_quantize(void)
{
    sound_type arg1 = getsound(xlgasound());
    long arg2 = getfixnum(xlgafixnum());
    sound_type result;

    xllastarg();
    result = snd_quantize(arg1, arg2);
    return cvsound(result);
}


#include "tran/ifft.h"

/* xlc_snd_ifft -- interface to C routine snd_ifft */
/**/
LVAL xlc_snd_ifft(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    LVAL arg3 = xlgetarg();
    long arg4 = getfixnum(xlgafixnum());
    LVAL arg5 = xlgetarg();
    sound_type result;

    xllastarg();
    result = snd_ifft(arg1, arg2, arg3, arg4, arg5);
    return cvsound(result);
}


#include "tran/congen.h"

/* xlc_snd_congen -- interface to C routine snd_congen */
/**/
LVAL xlc_snd_congen(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_congen(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/fromobject.h"

/* xlc_snd_fromobject -- interface to C routine snd_fromobject */
/**/
LVAL xlc_snd_fromobject(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    LVAL arg3 = xlgetarg();
    sound_type result;

    xllastarg();
    result = snd_fromobject(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/fromarraystream.h"

/* xlc_snd_fromarraystream -- interface to C routine snd_fromarraystream */
/**/
LVAL xlc_snd_fromarraystream(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    LVAL arg3 = xlgetarg();
    sound_type result;

    xllastarg();
    result = snd_fromarraystream(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/coterm.h"

/* xlc_snd_coterm -- interface to C routine snd_coterm */
/**/
LVAL xlc_snd_coterm(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_coterm(arg1, arg2);
    return cvsound(result);
}


#include "tran/convolve.h"

/* xlc_snd_convolve -- interface to C routine snd_convolve */
/**/
LVAL xlc_snd_convolve(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_convolve(arg1, arg2);
    return cvsound(result);
}


#include "tran/alpass.h"

/* xlc_snd_alpass -- interface to C routine snd_alpass */
/**/
LVAL xlc_snd_alpass(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_alpass(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/oneshot.h"

/* xlc_snd_oneshot -- interface to C routine snd_oneshot */
/**/
LVAL xlc_snd_oneshot(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_oneshot(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/chase.h"

/* xlc_snd_chase -- interface to C routine snd_chase */
/**/
LVAL xlc_snd_chase(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_chase(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/tapv.h"

/* xlc_snd_tapv -- interface to C routine snd_tapv */
/**/
LVAL xlc_snd_tapv(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    double arg4 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_tapv(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


#include "tran/biquad.h"

/* xlc_snd_biquad -- interface to C routine snd_biquad */
/**/
LVAL xlc_snd_biquad(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    double arg6 = testarg2(xlgaanynum());
    double arg7 = testarg2(xlgaanynum());
    double arg8 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_biquad(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    return cvsound(result);
}


#include "tran/pluck.h"

/* xlc_snd_pluck -- interface to C routine snd_pluck */
/**/
LVAL xlc_snd_pluck(void)
{
    double arg1 = testarg2(xlgaanynum());
    double arg2 = testarg2(xlgaanynum());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    double arg5 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_pluck(arg1, arg2, arg3, arg4, arg5);
    return cvsound(result);
}


#include "tran/abs.h"

/* xlc_snd_abs -- interface to C routine snd_abs */
/**/
LVAL xlc_snd_abs(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_abs(arg1);
    return cvsound(result);
}


#include "tran/sqrt.h"

/* xlc_snd_sqrt -- interface to C routine snd_sqrt */
/**/
LVAL xlc_snd_sqrt(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_sqrt(arg1);
    return cvsound(result);
}


#include "tran/alpasscv.h"

/* xlc_snd_alpasscv -- interface to C routine snd_alpasscv */
/**/
LVAL xlc_snd_alpasscv(void)
{
    sound_type arg1 = getsound(xlgasound());
    double arg2 = testarg2(xlgaanynum());
    sound_type arg3 = getsound(xlgasound());
    sound_type result;

    xllastarg();
    result = snd_alpasscv(arg1, arg2, arg3);
    return cvsound(result);
}


#include "tran/alpassvc.h"

/* xlc_snd_alpassvc -- interface to C routine snd_alpassvc */
/**/
LVAL xlc_snd_alpassvc(void)
{
    sound_type arg1 = getsound(xlgasound());
    sound_type arg2 = getsound(xlgasound());
    double arg3 = testarg2(xlgaanynum());
    double arg4 = testarg2(xlgaanynum());
    sound_type result;

    xllastarg();
    result = snd_alpassvc(arg1, arg2, arg3, arg4);
    return cvsound(result);
}


