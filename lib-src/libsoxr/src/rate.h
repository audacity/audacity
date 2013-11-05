/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "filter.h"

#if defined SOXR_LIB
#include "internal.h"

typedef void (* fn_t)(void);
extern fn_t RDFT_CB[11];

#define rdft_forward_setup    (*(void * (*)(int))RDFT_CB[0])
#define rdft_backward_setup   (*(void * (*)(int))RDFT_CB[1])
#define rdft_delete_setup     (*(void (*)(void *))RDFT_CB[2])
#define rdft_forward          (*(void (*)(int, void *, sample_t *, sample_t *))RDFT_CB[3])
#define rdft_oforward         (*(void (*)(int, void *, sample_t *, sample_t *))RDFT_CB[4])
#define rdft_backward         (*(void (*)(int, void *, sample_t *, sample_t *))RDFT_CB[5])
#define rdft_obackward        (*(void (*)(int, void *, sample_t *, sample_t *))RDFT_CB[6])
#define rdft_convolve         (*(void (*)(int, void *, sample_t *, sample_t const *))RDFT_CB[7])
#define rdft_convolve_portion (*(void (*)(int, sample_t *, sample_t const *))RDFT_CB[8])
#define rdft_multiplier       (*(int (*)(void))RDFT_CB[9])
#define rdft_reorder_back     (*(void (*)(int, void *, sample_t *, sample_t *))RDFT_CB[10])

#endif

#if RATE_SIMD /* Align for SIMD: */
  #include "simd.h"
#if 0 /* Not using this yet. */
  #define RATE_SIMD_POLY 1
  #define num_coefs4 ((num_coefs + 3) & ~3)
  #define coefs4_check(i) ((i) < num_coefs)
#else
  #define RATE_SIMD_POLY 0
  #define num_coefs4 num_coefs
  #define coefs4_check(i) 1
#endif

  #define aligned_free    _soxr_simd_aligned_free
  #define aligned_malloc  _soxr_simd_aligned_malloc
  #define aligned_calloc  _soxr_simd_aligned_calloc
#if 0
  #define FIFO_REALLOC    aligned_realloc
  #define FIFO_MALLOC     aligned_malloc
  #define FIFO_FREE       aligned_free

  static void * aligned_realloc(void * q, size_t nb_bytes, size_t copy_bytes) {
    void * p = aligned_malloc(nb_bytes);
    if (p) memcpy(p, q, copy_bytes);
    aligned_free(q);
    return p;
  }
#endif
#else
  #define RATE_SIMD_POLY 0
  #define num_coefs4 num_coefs
  #define coefs4_check(i) 1

  #define aligned_free    free
  #define aligned_malloc  malloc
  #define aligned_calloc  calloc
#endif

#define  FIFO_SIZE_T int
#include "fifo.h"

typedef union { /* Int64 in parts */
  #if WORDS_BIGENDIAN
  struct {int32_t ms; uint32_t ls;} parts;
  #else
  struct {uint32_t ls; int32_t ms;} parts;
  #endif
  int64_t all;
} int64p_t;

typedef union { /* Uint64 in parts */
  #if WORDS_BIGENDIAN
  struct {uint32_t ms, ls;} parts;
  #else
  struct {uint32_t ls, ms;} parts;
  #endif
  uint64_t all;
} uint64p_t;

#define FLOAT_HI_PREC_CLOCK 0    /* Non-float hi-prec has ~96 bits. */
#define float_step_t long double /* __float128 is also a (slow) option */

#define coef(coef_p, interp_order, fir_len, phase_num, coef_interp_num, fir_coef_num) coef_p[(fir_len) * ((interp_order) + 1) * (phase_num) + ((interp_order) + 1) * (fir_coef_num) + (interp_order - coef_interp_num)]

#define raw_coef_t double

static sample_t * prepare_coefs(raw_coef_t const * coefs, int num_coefs,
    int num_phases, int interp_order, double multiplier)
{
  int i, j, length = num_coefs4 * num_phases;
  sample_t * result = malloc((size_t)(length * (interp_order + 1)) * sizeof(*result));
  double fm1 = coefs[0], f1 = 0, f2 = 0;

  for (i = num_coefs4 - 1; i >= 0; --i)
    for (j = num_phases - 1; j >= 0; --j) {
      double f0 = fm1, b = 0, c = 0, d = 0; /* = 0 to kill compiler warning */
      int pos = i * num_phases + j - 1;
      fm1 = coefs4_check(i) && pos > 0 ? coefs[pos - 1] * multiplier : 0;
      switch (interp_order) {
        case 1: b = f1 - f0; break;
        case 2: b = f1 - (.5 * (f2+f0) - f1) - f0; c = .5 * (f2+f0) - f1; break;
        case 3: c=.5*(f1+fm1)-f0;d=(1/6.)*(f2-f1+fm1-f0-4*c);b=f1-f0-d-c; break;
        default: if (interp_order) assert(0);
      }
      #define coef_coef(x) \
        coef(result, interp_order, num_coefs4, j, x, num_coefs4 - 1 - i)
      coef_coef(0) = (sample_t)f0;
      if (interp_order > 0) coef_coef(1) = (sample_t)b;
      if (interp_order > 1) coef_coef(2) = (sample_t)c;
      if (interp_order > 2) coef_coef(3) = (sample_t)d;
      #undef coef_coef
      f2 = f1, f1 = f0;
    }
  return result;
}

typedef struct {
  int        dft_length, num_taps, post_peak;
  void       * dft_forward_setup, * dft_backward_setup;
  sample_t   * coefs;
} dft_filter_t;

typedef struct { /* So generated filter coefs may be shared between channels */
  sample_t   * poly_fir_coefs;
  dft_filter_t dft_filter[2];
} rate_shared_t;

typedef enum {
  irrational_stage = 1,
  cubic_stage,
  dft_stage,
  half_stage,
  rational_stage
} stage_type_t;

struct stage;
typedef void (* stage_fn_t)(struct stage * input, fifo_t * output);
#define MULT32 (65536. * 65536.)

typedef union { /* Fixed point arithmetic */
  struct {uint64p_t ls; int64p_t ms;} fix;
  float_step_t flt;
} step_t;

typedef struct stage {
  /* Common to all stage types: */
  stage_type_t type;
  stage_fn_t fn;
  fifo_t     fifo;
  int        pre;       /* Number of past samples to store */
  int        pre_post;  /* pre + number of future samples to store */
  int        preload;   /* Number of zero samples to pre-load the fifo */
  double     out_in_ratio; /* For buffer management. */

  /* For a stage with variable (run-time generated) filter coefs: */
  rate_shared_t * shared;
  unsigned   dft_filter_num; /* Which, if any, of the 2 DFT filters to use */
  sample_t   * dft_scratch, * dft_out;

  /* For a stage with variable L/M: */
  step_t     at, step;
  bool       use_hi_prec_clock;
  int        L, remM;
  int        n, phase_bits, block_len;
  double     mult, phase0;
} stage_t;

#define stage_occupancy(s) max(0, fifo_occupancy(&(s)->fifo) - (s)->pre_post)
#define stage_read_p(s) ((sample_t *)fifo_read_ptr(&(s)->fifo) + (s)->pre)

static void cubic_stage_fn(stage_t * p, fifo_t * output_fifo)
{
  int i, num_in = stage_occupancy(p), max_num_out = 1 + (int)(num_in*p->out_in_ratio);
  sample_t const * input = stage_read_p(p);
  sample_t * output = fifo_reserve(output_fifo, max_num_out);

#define integer  fix.ms.parts.ms
#define fraction fix.ms.parts.ls
#define whole    fix.ms.all
  for (i = 0; p->at.integer < num_in; ++i, p->at.whole += p->step.whole) {
    sample_t const * s = input + p->at.integer;
    double x = p->at.fraction * (1 / MULT32);
    double b = .5*(s[1]+s[-1])-*s, a = (1/6.)*(s[2]-s[1]+s[-1]-*s-4*b);
    double c = s[1]-*s-a-b;
    output[i] = (sample_t)(p->mult * (((a*x + b)*x + c)*x + *s));
  }
  assert(max_num_out - i >= 0);
  fifo_trim_by(output_fifo, max_num_out - i);
  fifo_read(&p->fifo, p->at.integer, NULL);
  p->at.integer = 0;
}

#if RATE_SIMD
  #define dft_out p->dft_out
#else
  #define dft_out output
#endif

static void dft_stage_fn(stage_t * p, fifo_t * output_fifo)
{
  sample_t * output;
  int i, j, num_in = max(0, fifo_occupancy(&p->fifo));
  rate_shared_t const * s = p->shared;
  dft_filter_t const * f = &s->dft_filter[p->dft_filter_num];
  int const overlap = f->num_taps - 1;

  while (p->at.integer + p->L * num_in >= f->dft_length) {
    div_t divd = div(f->dft_length - overlap - p->at.integer + p->L - 1, p->L);
    sample_t const * input = fifo_read_ptr(&p->fifo);
    fifo_read(&p->fifo, divd.quot, NULL);
    num_in -= divd.quot;

    output = fifo_reserve(output_fifo, f->dft_length);

    if (lsx_is_power_of_2(p->L)) { /* F-domain */
      int portion = f->dft_length / p->L;
      memcpy(dft_out, input, (unsigned)portion * sizeof(*dft_out));
      rdft_oforward(portion, f->dft_forward_setup, dft_out, p->dft_scratch);
      for (i = portion + 2; i < (portion << 1); i += 2) /* Mirror image. */
        dft_out[i] = dft_out[(portion << 1) - i],
        dft_out[i+1] = -dft_out[(portion << 1) - i + 1];
      dft_out[portion] = dft_out[1];
      dft_out[portion + 1] = 0;
      dft_out[1] = dft_out[0];

      for (portion <<= 1; i < f->dft_length; i += portion, portion <<= 1) {
        memcpy(dft_out + i, dft_out, (size_t)portion * sizeof(*dft_out));
        dft_out[i + 1] = 0;
      }
      if (p->step.integer > 0)
        rdft_reorder_back(f->dft_length, f->dft_backward_setup, dft_out, p->dft_scratch);
    } else {
      if (p->L == 1)
        memcpy(dft_out, input, (size_t)f->dft_length * sizeof(*dft_out));
      else {
        memset(dft_out, 0, (size_t)f->dft_length * sizeof(*dft_out));
        for (j = 0, i = p->at.integer; i < f->dft_length; ++j, i += p->L)
          dft_out[i] = input[j];
        p->at.integer = p->L - 1 - divd.rem;
      }
      if (p->step.integer > 0)
        rdft_forward(f->dft_length, f->dft_forward_setup, dft_out, p->dft_scratch);
      else
        rdft_oforward(f->dft_length, f->dft_forward_setup, dft_out, p->dft_scratch);
    }

    if (p->step.integer > 0) {
      rdft_convolve(f->dft_length, f->dft_backward_setup, dft_out, f->coefs);
      rdft_backward(f->dft_length, f->dft_backward_setup, dft_out, p->dft_scratch);
#if RATE_SIMD
      if (p->step.integer == 1)
        memcpy(output, dft_out, (size_t)f->dft_length * sizeof(sample_t));
#endif
      if (p->step.integer != 1) {
        for (j = 0, i = p->remM; i < f->dft_length - overlap; ++j,
            i += p->step.integer)
          output[j] = dft_out[i];
        p->remM = i - (f->dft_length - overlap);
        fifo_trim_by(output_fifo, f->dft_length - j);
      }
      else fifo_trim_by(output_fifo, overlap);
    }
    else { /* F-domain */
      int m = -p->step.integer;
      rdft_convolve_portion(f->dft_length >> m, dft_out, f->coefs);
      rdft_obackward(f->dft_length >> m, f->dft_backward_setup, dft_out, p->dft_scratch);
#if RATE_SIMD
      memcpy(output, dft_out, (size_t)(f->dft_length >> m) * sizeof(sample_t));
#endif
      fifo_trim_by(output_fifo, (((1 << m) - 1) * f->dft_length + overlap) >>m);
    }
  }
}

#undef dft_out

/* Set to 4 x nearest power of 2 */
/* or half of that if danger of causing too many cache misses. */
static int set_dft_length(int num_taps, int min, int large)
{
  double d = log((double)num_taps) / log(2.);
  return 1 << range_limit((int)(d + 2.77), min, max((int)(d + 1.77), large));
}

static void dft_stage_init(
    unsigned instance, double Fp, double Fs, double Fn, double att,
    double phase, stage_t * p, int L, int M, double * multiplier,
    int min_dft_size, int large_dft_size)
{
  dft_filter_t * f = &p->shared->dft_filter[instance];
  int num_taps = 0, dft_length = f->dft_length, i;
  bool f_domain_m = abs(3-M) == 1 && Fs <= 1;

  if (!dft_length) {
    int k = phase == 50 && lsx_is_power_of_2(L) && Fn == L? L << 1 : 4;
    double * h = lsx_design_lpf(Fp, Fs, Fn, att, &num_taps, -k, -1.);

    if (phase != 50)
      lsx_fir_to_phase(&h, &num_taps, &f->post_peak, phase);
    else f->post_peak = num_taps / 2;

    dft_length = set_dft_length(num_taps, min_dft_size, large_dft_size);
    f->coefs = aligned_calloc((size_t)dft_length, sizeof(*f->coefs));
    for (i = 0; i < num_taps; ++i)
      f->coefs[(i + dft_length - num_taps + 1) & (dft_length - 1)]
        = (sample_t)(h[i] * ((1. / dft_length) * rdft_multiplier() * L * *multiplier));
    free(h);
  }

#if RATE_SIMD
  p->dft_out = aligned_malloc(sizeof(sample_t) * (size_t)dft_length);
#endif
#if 1 /* In fact, currently, only pffft needs this. */
  p->dft_scratch = aligned_malloc(2 * sizeof(sample_t) * (size_t)dft_length);
#endif

  if (!f->dft_length) {
    void * coef_setup = rdft_forward_setup(dft_length);
    int Lp = lsx_is_power_of_2(L)? L : 1;
    int Mp = f_domain_m? M : 1;
    f->dft_forward_setup = rdft_forward_setup(dft_length / Lp);
    f->dft_backward_setup = rdft_backward_setup(dft_length / Mp);
    if (Mp == 1)
      rdft_forward(dft_length, coef_setup, f->coefs, p->dft_scratch);
    else
      rdft_oforward(dft_length, coef_setup, f->coefs, p->dft_scratch);
    rdft_delete_setup(coef_setup);
    f->num_taps = num_taps;
    f->dft_length = dft_length;
    lsx_debug("fir_len=%i dft_length=%i Fp=%g Fs=%g Fn=%g att=%g %i/%i",
        num_taps, dft_length, Fp, Fs, Fn, att, L, M);
  }
  *multiplier = 1;
  p->out_in_ratio = (double)L / M;
  p->type = dft_stage;
  p->fn = dft_stage_fn;
  p->preload = f->post_peak / L;
  p->at.integer = f->post_peak % L;
  p->L = L;
  p->step.integer = f_domain_m? -M/2 : M;
  p->dft_filter_num = instance;
  p->block_len = f->dft_length - (f->num_taps - 1);
  p->phase0 = p->at.integer / p->L;
}

#include "filters.h"

typedef struct {
  double     factor;
  uint64_t   samples_in, samples_out;
  int        num_stages;
  stage_t    * stages;
} rate_t;

#define pre_stage       p->stages[shift]
#define arb_stage       p->stages[shift + have_pre_stage]
#define post_stage      p->stages[shift + have_pre_stage + have_arb_stage]
#define have_pre_stage  (preM  * preL  != 1)
#define have_arb_stage  (arbM  * arbL  != 1)
#define have_post_stage (postM * postL != 1)

#define TO_3dB(a)       ((1.6e-6*a-7.5e-4)*a+.646)
#define LOW_Q_BW0       (1385 / 2048.) /* 0.67625 rounded to be a FP exact. */

typedef enum {
  rolloff_none, rolloff_small /* <= 0.01 dB */, rolloff_medium /* <= 0.35 dB */
} rolloff_t;


static char const * rate_init(
  /* Private work areas (to be supplied by the client):                       */
  rate_t * p,                /* Per audio channel.                            */
  rate_shared_t * shared,    /* Between channels (undergoing same rate change)*/

  /* Public parameters:                                             Typically */
  double factor,             /* Input rate divided by output rate.            */
  double bits,               /* Required bit-accuracy (pass + stop)  16|20|28 */
  double phase,              /* Linear/minimum etc. filter phase.       50    */
  double passband_end,       /* 0dB pt. bandwidth to preserve; nyquist=1 0.913*/
  double stopband_begin,     /* Aliasing/imaging control; > passband_end  1   */
  rolloff_t rolloff,         /* Pass-band roll-off                    small   */
  bool maintain_3dB_pt,      /*                                        true   */
  double multiplier,         /* Linear gain to apply during conversion.   1   */

  /* Primarily for test/development purposes:                                 */
  bool use_hi_prec_clock,    /* Increase irrational ratio accuracy.   false   */
  int interpolator,          /* Force a particular coef interpolator.   -1    */
  size_t max_coefs_size,     /* k bytes of coefs to try to keep below.  400   */
  bool noSmallIntOpt,        /* Disable small integer optimisations.  false   */
  int log2_min_dft_size,
  int log2_large_dft_size)
{
  double att = (bits + 1) * linear_to_dB(2.), attArb = att;    /* pass + stop */
  double tbw0 = 1 - passband_end, Fs_a = stopband_begin;
  double arbM = factor, tbw_tighten = 1;
  int n = 0, i, preL = 1, preM = 1, shift = 0, arbL = 1, postL = 1, postM = 1;
  bool upsample = false, rational = false, iOpt = !noSmallIntOpt;
  int mode = rolloff > rolloff_small? factor > 1 || passband_end > LOW_Q_BW0:
    (int)ceil(2 + (bits - 17) / 4);
  stage_t * s;

  assert(factor > 0);
  assert(!bits || (15 <= bits && bits <= 33));
  assert(0 <= phase && phase <= 100);
  assert(.53 <= passband_end);
  assert(stopband_begin <= 1.2);
  assert(passband_end + .005 < stopband_begin);

  p->factor = factor;
  if (bits) while (!n++) {                               /* Determine stages: */
    int try, L, M, x, maxL = interpolator > 0? 1 : mode? 2048 :
      (int)ceil((double)max_coefs_size * 1000. / (U100_l * sizeof(sample_t)));
    double d, epsilon = 0, frac;
    upsample = arbM < 1;
    for (i = (int)(arbM * .5), shift = 0; i >>= 1; arbM *= .5, ++shift);
    preM = upsample || (arbM > 1.5 && arbM < 2);
    postM = 1 + (arbM > 1 && preM), arbM /= postM;
    preL = 1 + (!preM && arbM < 2) + (upsample && mode), arbM *= preL;
    if ((frac = arbM - (int)arbM))
      epsilon = fabs((uint32_t)(frac * MULT32 + .5) / (frac * MULT32) - 1);
    for (i = 1, rational = !frac; i <= maxL && !rational; ++i) {
      d = frac * i, try = (int)(d + .5);
      if ((rational = fabs(try / d - 1) <= epsilon)) {    /* No long doubles! */
        if (try == i)
          arbM = ceil(arbM), shift += arbM > 2, arbM /= 1 + (arbM > 2);
        else arbM = i * (int)arbM + try, arbL = i;
      }
    }
    L = preL * arbL, M = (int)(arbM * postM), x = (L|M)&1, L >>= !x, M >>= !x;
    if (iOpt && postL == 1 && (d = preL * arbL / arbM) > 4 && d != 5) {
      for (postL = 4, i = (int)(d / 16); (i >>= 1) && postL < 256; postL <<= 1);
      arbM = arbM * postL / arbL / preL, arbL = 1, n = 0;
    } else if (rational && (max(L, M) < 3 + 2 * iOpt || L * M < 6 * iOpt))
      preL = L, preM = M, arbM = arbL = postM = 1;
    if (!mode && (!rational || !n))
      ++mode, n = 0;
  }

  p->num_stages = shift + have_pre_stage + have_arb_stage + have_post_stage;
  if (!p->num_stages && multiplier != 1) {
    arbL = 0;
    ++p->num_stages;
  }
  p->stages = calloc((size_t)p->num_stages + 1, sizeof(*p->stages));
  for (i = 0; i < p->num_stages; ++i)
    p->stages[i].shared = shared;

  if ((n = p->num_stages) > 1) {                              /* Att. budget: */
    if (have_arb_stage)
      att += linear_to_dB(2.), attArb = att, --n;
    att += linear_to_dB((double)n);
  }

  for (n = 0; (size_t)n + 1 < array_length(half_firs) && att > half_firs[n].att; ++n);
  for (i = 0, s = p->stages; i < shift; ++i, ++s) {
    s->type = half_stage;
    s->fn = half_firs[n].fn;
    s->pre_post = 4 * half_firs[n].num_coefs;
    s->preload = s->pre = s->pre_post >> 1;
  }

  if (have_pre_stage) {
    if (maintain_3dB_pt && have_post_stage) {    /* Trans. bands overlapping. */
      double tbw3 = tbw0 * TO_3dB(att);                /* FFS: consider Fs_a. */
      double x = ((2.1429e-4 - 5.2083e-7 * att) * att - .015863) * att + 3.95;
      x = att * pow((tbw0 - tbw3) / (postM / (factor * postL) - 1 + tbw0), x);
      if (x > .035) {
        tbw_tighten = ((4.3074e-3 - 3.9121e-4 * x) * x - .040009) * x + 1.0014;
        lsx_debug("x=%g tbw_tighten=%g", x, tbw_tighten);
      }
    }
    dft_stage_init(0, 1 - tbw0 * tbw_tighten, Fs_a, preM? max(preL, preM) :
        arbM / arbL, att, phase, &pre_stage, preL, max(preM, 1), &multiplier,
        log2_min_dft_size, log2_large_dft_size);
  }

  if (!bits && have_arb_stage) {                /* Quick and dirty arb stage: */
    arb_stage.type = cubic_stage;
    arb_stage.fn = cubic_stage_fn;
    arb_stage.mult = multiplier, multiplier = 1;
    arb_stage.step.whole = (int64_t)(arbM * MULT32 + .5);
    arb_stage.pre_post = max(3, arb_stage.step.integer);
    arb_stage.preload = arb_stage.pre = 1;
    arb_stage.out_in_ratio = MULT32 / (double)arb_stage.step.whole;
  }
  else if (have_arb_stage) {                     /* Higher quality arb stage: */
    poly_fir_t const * f = &poly_firs[6*(upsample + !!preM) + mode - !upsample];
    int order, num_coefs = (int)f->interp[0].scalar, phase_bits, phases;
    size_t coefs_size;
    double x = .5, at, Fp, Fs, Fn, mult = upsample? 1 : arbL / arbM;
    poly_fir1_t const * f1;

    Fn = !upsample && preM? x = arbM / arbL : 1;
    Fp = !preM? mult : mode? .5 : 1;
    Fs = 2 - Fp;           /* Ignore Fs_a; it would have little benefit here. */
    Fp *= 1 - tbw0;
    if (rolloff > rolloff_small && mode)
      Fp = !preM? mult * .5 - .125 : mult * .05 + .1;
    else if (rolloff == rolloff_small)
      Fp = Fs - (Fs - .148 * x - Fp * .852) * (.00813 * bits + .973);

    i = (interpolator < 0? !rational : max(interpolator, !rational)) - 1;
    do {
      f1 = &f->interp[++i];
      assert(f1->fn);
      if (i)
        arbM /= arbL, arbL = 1, rational = false;
      phase_bits = (int)ceil(f1->scalar + log(mult)/log(2.));
      phases = !rational? (1 << phase_bits) : arbL;
      if (!f->interp[0].scalar) {
        int phases0 = max(phases, 19), n0 = 0;
        lsx_design_lpf(Fp, Fs, -Fn, attArb, &n0, phases0, f->beta);
        num_coefs = n0 / phases0 + 1, num_coefs += num_coefs & !preM;
      }
      if ((num_coefs & 1) && rational && (arbL & 1))
        phases <<= 1, arbL <<= 1, arbM *= 2;
      at = arbL * (arb_stage.phase0 = .5 * (num_coefs & 1));
      order = i + (i && mode > 4);
      coefs_size = (size_t)(num_coefs4 * phases * (order + 1)) * sizeof(sample_t);
    } while (interpolator < 0 && i < 2 && f->interp[i+1].fn &&
        coefs_size / 1000 > max_coefs_size);

    if (!arb_stage.shared->poly_fir_coefs) {
      int num_taps = num_coefs * phases - 1;
      raw_coef_t * coefs = lsx_design_lpf(
          Fp, Fs, Fn, attArb, &num_taps, phases, f->beta);
      arb_stage.shared->poly_fir_coefs = prepare_coefs(
          coefs, num_coefs, phases, order, multiplier);
      lsx_debug("fir_len=%i phases=%i coef_interp=%i size=%.3gk",
          num_coefs, phases, order, (double)coefs_size / 1000.);
      free(coefs);
    }
    multiplier = 1;
    arb_stage.type = rational? rational_stage : irrational_stage;
    arb_stage.fn = f1->fn;
    arb_stage.pre_post = num_coefs4 - 1;
    arb_stage.preload = ((num_coefs - 1) >> 1) + (num_coefs4 - num_coefs);
    arb_stage.n = num_coefs4;
    arb_stage.phase_bits = phase_bits;
    arb_stage.L = arbL;
    arb_stage.use_hi_prec_clock = mode > 1 && use_hi_prec_clock && !rational;
#if FLOAT_HI_PREC_CLOCK
    if (arb_stage.use_hi_prec_clock) {
      arb_stage.at.flt = at;
      arb_stage.step.flt = arbM;
      arb_stage.out_in_ratio = (double)(arbL / arb_stage.step.flt);
    } else
#endif
    {
      arb_stage.at.whole = (int64_t)(at * MULT32 + .5);
#if !FLOAT_HI_PREC_CLOCK
      if (arb_stage.use_hi_prec_clock) {
        arb_stage.at.fix.ls.parts.ms = 0x80000000ul;
        arbM *= MULT32;
        arb_stage.step.whole = (int64_t)arbM;
        arbM -= (double)arb_stage.step.whole;
        arbM *= MULT32 * MULT32;
        arb_stage.step.fix.ls.all = (uint64_t)arbM;
      } else
#endif
        arb_stage.step.whole = (int64_t)(arbM * MULT32 + .5);
      arb_stage.out_in_ratio = MULT32 * arbL / (double)arb_stage.step.whole;
    }
  }

  if (have_post_stage)
    dft_stage_init(1, 1 - (1 - (1 - tbw0) *
        (upsample? factor * postL / postM : 1)) * tbw_tighten, Fs_a,
        (double)max(postL, postM), att, phase, &post_stage, postL, postM,
        &multiplier, log2_min_dft_size, log2_large_dft_size);


  lsx_debug("%g: »%i⋅%i/%i⋅%i/%g⋅%i/%i",
      1/factor, shift, preL, preM, arbL, arbM, postL, postM);
  for (i = 0, s = p->stages; i < p->num_stages; ++i, ++s) {
    fifo_create(&s->fifo, (int)sizeof(sample_t));
    memset(fifo_reserve(&s->fifo, s->preload), 0, sizeof(sample_t) * (size_t)s->preload);
    lsx_debug("%5i|%-5i preload=%i remL=%i o/i=%g",
        s->pre, s->pre_post - s->pre, s->preload, s->at.integer, s->out_in_ratio);
  }
  fifo_create(&s->fifo, (int)sizeof(sample_t));
  return 0;
}

static void rate_process(rate_t * p)
{
  stage_t * stage = p->stages;
  int i;
  for (i = 0; i < p->num_stages; ++i, ++stage)
    stage->fn(stage, &(stage+1)->fifo);
}

static sample_t * rate_input(rate_t * p, sample_t const * samples, size_t n)
{
  p->samples_in += n;
  return fifo_write(&p->stages[0].fifo, (int)n, samples);
}

static sample_t const * rate_output(rate_t * p, sample_t * samples, size_t * n)
{
  fifo_t * fifo = &p->stages[p->num_stages].fifo;
  p->samples_out += *n = min(*n, (size_t)fifo_occupancy(fifo));
  return fifo_read(fifo, (int)*n, samples);
}

static void rate_flush(rate_t * p)
{
  fifo_t * fifo = &p->stages[p->num_stages].fifo;
#if defined _MSC_VER && _MSC_VER == 1200
  uint64_t samples_out = (uint64_t)(int64_t)((double)(int64_t)p->samples_in / p->factor + .5);
#else
  uint64_t samples_out = (uint64_t)((double)p->samples_in / p->factor + .5);
#endif
  size_t remaining = (size_t)(samples_out - p->samples_out);
  sample_t * buff = calloc(1024, sizeof(*buff));

  if (samples_out > p->samples_out) {
    while ((size_t)fifo_occupancy(fifo) < remaining) {
      rate_input(p, buff, 1024);
      rate_process(p);
    }
    fifo_trim_to(fifo, (int)remaining);
    p->samples_in = 0;
  }
  free(buff);
}

static void rate_close(rate_t * p)
{
  rate_shared_t * shared = p->stages[0].shared;
  int i;

  for (i = 0; i <= p->num_stages; ++i) {
    stage_t * s = &p->stages[i];
    aligned_free(s->dft_scratch);
    aligned_free(s->dft_out);
    fifo_delete(&s->fifo);
  }
  if (shared) {
    for (i = 0; i < 2; ++i) {
      dft_filter_t * f= &shared->dft_filter[i];
      aligned_free(f->coefs);
      rdft_delete_setup(f->dft_forward_setup);
      rdft_delete_setup(f->dft_backward_setup);
    }
    free(shared->poly_fir_coefs);
    memset(shared, 0, sizeof(*shared));
  }
  free(p->stages);
}

#if defined SOXR_LIB
static double rate_delay(rate_t * p)
{
#if defined _MSC_VER && _MSC_VER == 1200
  double samples_out = (double)(int64_t)p->samples_in / p->factor;
  return samples_out - (double)(int64_t)p->samples_out;
#else
  double samples_out = (double)p->samples_in / p->factor;
  return samples_out - (double)p->samples_out;
#endif
}

static void rate_sizes(size_t * shared, size_t * channel)
{
  *shared = sizeof(rate_shared_t);
  *channel = sizeof(rate_t);
}

#include "soxr.h"

static char const * rate_create(
    void * channel,
    void * shared,
    double io_ratio,
    soxr_quality_spec_t * q_spec,
    soxr_runtime_spec_t * r_spec,
    double scale)
{
  return rate_init(
      channel, shared,
      io_ratio,
      q_spec->precision,
      q_spec->phase_response,
      q_spec->passband_end,
      q_spec->stopband_begin,
      "\1\2\0"[q_spec->flags & 3],
      !!(q_spec->flags & SOXR_MAINTAIN_3DB_PT),
      scale,
      !!(q_spec->flags & SOXR_HI_PREC_CLOCK),
      (int)(r_spec->flags & 3) - 1,
      r_spec->coef_size_kbytes,
      !!(r_spec->flags & SOXR_NOSMALLINTOPT),
      (int)r_spec->log2_min_dft_size,
      (int)r_spec->log2_large_dft_size);
}

static char const * id(void)
{
  return RATE_ID;
}

fn_t RATE_CB[] = {
  (fn_t)rate_input,
  (fn_t)rate_process,
  (fn_t)rate_output,
  (fn_t)rate_flush,
  (fn_t)rate_close,
  (fn_t)rate_delay,
  (fn_t)rate_sizes,
  (fn_t)rate_create,
  (fn_t)0,
  (fn_t)id,
};
#endif
