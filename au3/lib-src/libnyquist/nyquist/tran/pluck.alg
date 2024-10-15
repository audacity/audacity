;; PLUCK.ALG is based on the Pluck.t instrument from M4C
;;
; to assist with debugging, here are some calculated parameters
; dumped from a run of M4C:
;
; Setup Pluck at t=  6.000
; DUR= 1.000, pitch= 8.060, amp= 10000, DB_drop=  60.0, rfrac=0.00
; freq 369.995
; freq = 369.995361
; Final = 1000.000000, t= 0.052715, y = 0.000000, lF = 6.907755
; tdecay = 13.430565, sT = 369.995361, rho = 0.982869, stretch = 0.500000
; N = 59, x = 0.095342, cons = 0.825914
; x2 0.412957 x3 0.912957 stretch 0.5 cons 0.825914 SR 22050
; complete Pluck setup
;;

(PLUCK-ALG
(NAME "pluck")
(ARGUMENTS ("rate_type" "sr") ("double" "hz") ("time_type" "t0") 
           ("time_type" "d") ("double" "final_amp"))
(SUPPORT-FUNCTIONS "
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
        xlfail(\"pluck hz is too low\");
    } else if (hz >= sr / 3) {
        xlfail(\"pluck hz is too high\");
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
        xlfail(\"internal error: pluck delay line length too short\");
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
}")
(STATE ("double" "stretch" "0")
       ("double" "cons" "0")
       ("double" "loss" "0")
       ("long" "len" "pluck_parameters(hz, sr, final_amp, d,
                                &susp->stretch, &susp->cons, 
                                &susp->loss)")
       ("double" "x2" "-susp->cons * (susp->stretch - 1)")
       ("double" "x3" "susp->cons * susp->stretch - susp->stretch + 1")
       ("sample_type *" "shiftreg"
       ;; I think susp->len + 2 is the correct value, but I use +4 to be safe
                       "(sample_type *) calloc (susp->len + 4, sizeof(sample_type))")
       ("sample_type *" "i1" "susp->shiftreg + susp->len + 1")
       ("sample_type *" "i2" "susp->shiftreg + susp->len")
       ("sample_type *" "i3" "susp->shiftreg + susp->len - 1")
       ("sample_type *" "i4" "susp->shiftreg + susp->len - 2")
       ("sample_type *" "endptr" "susp->shiftreg + susp->len + 2; 
                   pluck_initialize(susp->shiftreg, susp->i3, 
                                    susp->len, susp->cons)"))
(CONSTANT "stretch" "cons" "loss" "len" "x2" "x3" "endptr")
(SAMPLE-RATE "sr")
(NOT-REGISTER shiftreg)
(TERMINATE (AFTER "d"))
(INNER-LOOP "sample_type sum = (sample_type)
                ((*i1++ * x2) + (*i2++ * x3) + 
                 (*i3++ * stretch) - (*i4++ * cons));
            /* wrap pointers around shift register if necessary */
            if (i1 == endptr) i1 = susp->shiftreg;
            if (i2 == endptr) i2 = susp->shiftreg;
            if (i3 == endptr) i3 = susp->shiftreg;
            if (i4 == endptr) i4 = susp->shiftreg;

            /* store new value in shift register */
            *i4 = (sample_type) (sum * loss);

            /* deliver sample */
            output = sum")
(FINALIZATION "    free(susp->shiftreg);\n")
)

