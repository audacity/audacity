#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "nyq-osc-server.h"
#include "sliderdata.h"
#include "sndsliders.h"

LVAL xslider_read(void)
{
    LVAL arg = xlgafixnum();
    int index = (int) getfixnum(arg);
    xllastarg();
    if (index >= 0 && index < SLIDERS_MAX) {
        return cvflonum(slider_array[index]);
    }
    return NIL;
}

LVAL xosc_enable(void)
{
/* only need arg if OSC is defined, otherwise compiler complains */
#ifdef OSC
    LVAL arg = 
#endif
    xlgetarg();
    xllastarg();
#ifdef OSC
    if (nosc_enabled == !null(arg)) {
        return arg; /* no change */
    } else if (null(arg)) { /* nosc_enabled must be true */
        nosc_finish();
	return s_true;
    } else { /* nosc_enabled must be false */
        nosc_init();
	return NIL;
    }
#else
    return xlenter("DISABLED");
#endif    
}


void slider_free(snd_susp_type a_susp);


typedef struct slider_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    int index;
} slider_susp_node, *slider_susp_type;


void slider__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    slider_susp_type susp = (slider_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register sample_type c_reg;
    int limit = ((long) susp->susp.sr) / 50;
    falloc_sample_block(out, "slider__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    /* compute no more than 20ms to preserve some interactivity */
    if (limit < 1) limit = 1;
    if (limit > max_sample_block_len) limit = max_sample_block_len;

    while (cnt < limit) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = limit - cnt;

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo == 0) break;
        }

        n = togo;
        c_reg = slider_array[susp->index];
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            *out_ptr_reg++ = c_reg;
        } while (--n); /* inner loop */

        out_ptr += togo;
        cnt += togo;
    } /* outer loop */
    /* printf("slider %d cnt %d\n", susp->index, cnt); */
    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* slider__fetch */


void slider_free(snd_susp_type a_susp)
{
    slider_susp_type susp = (slider_susp_type) a_susp;
    ffree_generic(susp, sizeof(slider_susp_node), "slider_free");
}


void slider_print_tree(snd_susp_type susp, int n)
{
}


sound_type snd_make_slider(int index, time_type t0, rate_type sr, time_type d)
{
    register slider_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    if (index < 0 || index >= SLIDERS_MAX) {
        xlfail("slider index out of range");
    }
    falloc_generic(susp, slider_susp_node, "snd_make_slider");
    susp->susp.fetch = slider__fetch;
    susp->index = index;

    susp->terminate_cnt = ROUNDBIG((d) * sr);
    /* initialize susp state */
    susp->susp.free = slider_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = slider_print_tree;
    susp->susp.name = "slider";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_slider(int index, time_type t0, rate_type sr, time_type d)
{
    return snd_make_slider(index, t0, sr, d);
}
