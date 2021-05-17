#include "stdio.h"
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"

#include "falloc.h"
#include "cext.h"
#include "pluck.h"

void pluck_free(snd_susp_type a_susp);


typedef struct pluck_susp_struct {
    snd_susp_node susp;
    int64_t terminate_cnt;

    double stretch;
    double cons;
    double loss;
    long len;
    double x2;
    double x3;
    sample_type *shiftreg;
    sample_type *i1;
    sample_type *i2;
    sample_type *i3;
    sample_type *i4;
    sample_type *endptr;
} pluck_susp_node, *pluck_susp_type;


#define MAXLENGTH 20000

long pluck_parameters(double hz, double sr, double final, double dur,
                        double *stretch, double *cons, double *rho)
{
    double t = PI * (hz / sr);
    double y = fabs(cos(t));
    /* original m4c code used ratio of initial amp to final amp in dB
       and then converted to a ratio, e.g. you specify 60 and the 
       parameter Final is 1000.0. This is counterintuitive to me (RBD)
       because I would expect the value to be -60dB or 0.001. That is
       what I implemented, so to get this back into correspondence
       with the m4c algorithm, I take the NEGATIVE log to get lf, 
       whereas m4c takes the positive log:
     */
    double lf = -log(final);
    double tdecay = -lf / (hz * log(y));
    double st;
    long len;
    double x;

    if (hz <= sr / MAXLENGTH) {
        xlfail("pluck hz is too low");
    } else if (hz >= sr / 3) {
        xlfail("pluck hz is too high");
    }
    /*
     * if desired decay time is shorter than the natural decay time,
     * then introduce a loss factor.  Otherwise, stretch note out.
     */
    st = hz * dur;
    if (dur < tdecay) {
        *rho = exp(-lf / st) / y;
        *stretch = 0.5;
    } else {
        *rho = 1;
        *stretch = 0.5 + sqrt(0.25 - 
                              (1 - exp(2 * lf * (hz - sr) / (st * sr))) /
                              (2 - 2 * cos(2 * t)));
    }

    /* delay line length is */
    len = (int) ((sr / hz) - *stretch - 0.001);

    /* tuning constant is */
    x = (sr / hz) - len - *stretch;
    *cons = (1.0 - x) / (1.0 + x);

    if (len <= 1) {
        xlfail("internal error: pluck delay line length too short");
    }
    return len;
}

static unsigned int rnext = 1;
int krand()
{
    rnext = rnext * 1103515245 + 12345;
    return (rnext >> 16) & 0x7fff;
}

void pluck_initialize(sample_type *shiftreg, sample_type *array,
                      long len, double cons)
{
    sample_type suma = 0.0F;
    long k;
    sample_type avea;
    array[1] = 0;
    for (k = len; k > 0; k--, array--) {
        /* note: the m4c code has a bug. It claims to filter
           the initial values, but it really just sets the
           values to +1 or -1. The following does the same
           thing with much less code:
         */
        *array = (sample_type) ((krand() & 2) - 1);
        suma += *array; /* compute sum for the average */
    }
    avea = suma / len;
    /* zero the average */
    for (k = 0; k <= len + 1; k++) shiftreg[k] -= avea;
    shiftreg[len] = 0;
    shiftreg[len + 1] = 0;
}

void pluck__fetch(snd_susp_type a_susp, snd_list_type snd_list)
{
    pluck_susp_type susp = (pluck_susp_type) a_susp;
    int cnt = 0; /* how many samples computed */
    int togo;
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;

    register sample_block_values_type out_ptr_reg;

    register double stretch_reg;
    register double cons_reg;
    register double loss_reg;
    register double x2_reg;
    register double x3_reg;
    register sample_type * i1_reg;
    register sample_type * i2_reg;
    register sample_type * i3_reg;
    register sample_type * i4_reg;
    register sample_type * endptr_reg;
    falloc_sample_block(out, "pluck__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    while (cnt < max_sample_block_len) { /* outer loop */
        /* first compute how many samples to generate in inner loop: */
        /* don't overflow the output sample block: */
        togo = max_sample_block_len - cnt;

        /* don't run past terminate time */
        if (susp->terminate_cnt != UNKNOWN &&
            susp->terminate_cnt <= susp->susp.current + cnt + togo) {
            togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
            if (togo < 0) togo = 0;  /* avoids rounding errros */
            if (togo == 0) break;
        }

        n = togo;
        stretch_reg = susp->stretch;
        cons_reg = susp->cons;
        loss_reg = susp->loss;
        x2_reg = susp->x2;
        x3_reg = susp->x3;
        i1_reg = susp->i1;
        i2_reg = susp->i2;
        i3_reg = susp->i3;
        i4_reg = susp->i4;
        endptr_reg = susp->endptr;
        out_ptr_reg = out_ptr;
        if (n) do { /* the inner sample computation loop */
            sample_type sum = (sample_type)
                ((*i1_reg++ * x2_reg) + (*i2_reg++ * x3_reg) + 
                 (*i3_reg++ * stretch_reg) - (*i4_reg++ * cons_reg));
            /* wrap pointers around shift register if necessary */
            if (i1_reg == endptr_reg) i1_reg = susp->shiftreg;
            if (i2_reg == endptr_reg) i2_reg = susp->shiftreg;
            if (i3_reg == endptr_reg) i3_reg = susp->shiftreg;
            if (i4_reg == endptr_reg) i4_reg = susp->shiftreg;

            /* store new value in shift register */
            *i4_reg = (sample_type) (sum * loss_reg);

            /* deliver sample */
            *out_ptr_reg++ = sum;
        } while (--n); /* inner loop */

        susp->i1 = i1_reg;
        susp->i2 = i2_reg;
        susp->i3 = i3_reg;
        susp->i4 = i4_reg;
        out_ptr += togo;
        cnt += togo;
    } /* outer loop */

    /* test for termination */
    if (togo == 0 && cnt == 0) {
        snd_list_terminate(snd_list);
    } else {
        snd_list->block_len = cnt;
        susp->susp.current += cnt;
    }
} /* pluck__fetch */


void pluck_free(snd_susp_type a_susp)
{
    pluck_susp_type susp = (pluck_susp_type) a_susp;
    free(susp->shiftreg);
    ffree_generic(susp, sizeof(pluck_susp_node), "pluck_free");
}


void pluck_print_tree(snd_susp_type a_susp, int n)
{
}


sound_type snd_make_pluck(rate_type sr, double hz, time_type t0, time_type d, double final_amp)
{
    register pluck_susp_type susp;
    /* sr specified as input parameter */
    /* t0 specified as input parameter */
    sample_type scale_factor = 1.0F;
    falloc_generic(susp, pluck_susp_node, "snd_make_pluck");
    susp->stretch = 0;
    susp->cons = 0;
    susp->loss = 0;
    susp->len = pluck_parameters(hz, sr, final_amp, d,
                                &susp->stretch, &susp->cons, 
                                &susp->loss);
    susp->x2 = -susp->cons * (susp->stretch - 1);
    susp->x3 = susp->cons * susp->stretch - susp->stretch + 1;
    susp->shiftreg = (sample_type *) calloc (susp->len + 4, sizeof(sample_type));
    susp->i1 = susp->shiftreg + susp->len + 1;
    susp->i2 = susp->shiftreg + susp->len;
    susp->i3 = susp->shiftreg + susp->len - 1;
    susp->i4 = susp->shiftreg + susp->len - 2;
    susp->endptr = susp->shiftreg + susp->len + 2; 
                   pluck_initialize(susp->shiftreg, susp->i3, 
                                    susp->len, susp->cons);
    susp->susp.fetch = pluck__fetch;

    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((d) * sr));
    /* initialize susp state */
    susp->susp.free = pluck_free;
    susp->susp.sr = sr;
    susp->susp.t0 = t0;
    susp->susp.mark = NULL;
    susp->susp.print_tree = pluck_print_tree;
    susp->susp.name = "pluck";
    susp->susp.log_stop_cnt = UNKNOWN;
    susp->susp.current = 0;
    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);
}


sound_type snd_pluck(rate_type sr, double hz, time_type t0, time_type d, double final_amp)
{
    return snd_make_pluck(sr, hz, t0, d, final_amp);
}
