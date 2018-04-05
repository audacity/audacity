/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details.
 *
 * Constant-rate resampling common code. */

#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "filter.h"

#if defined SOXR_LIB
  #include "internal.h"
  #define STATIC
#endif

#include "cr.h"

#define num_coefs4 ((core_flags&CORE_SIMD_POLY)? ((num_coefs+3)&~3) : num_coefs)

#define coef_coef(C,T,x) \
  C((T*)result, interp_order, num_coefs4, j, x, num_coefs4 - 1 - i)

#define STORE(C,T) { \
  if (interp_order > 2) coef_coef(C,T,3) = (T)d; \
  if (interp_order > 1) coef_coef(C,T,2) = (T)c; \
  if (interp_order > 0) coef_coef(C,T,1) = (T)b; \
  coef_coef(C,T,0) = (T)f0;}

static real * prepare_poly_fir_coefs(double const * coefs, int num_coefs,
    int num_phases, int interp_order, double multiplier,
    core_flags_t core_flags, alloc_t const * mem)
{
  int i, j, length = num_coefs4 * num_phases * (interp_order + 1);
  real * result = mem->calloc(1,(size_t)length << LOG2_SIZEOF_REAL(core_flags));
  double fm1 = coefs[0], f1 = 0, f2 = 0;

  for (i = num_coefs - 1; i >= 0; --i)
    for (j = num_phases - 1; j >= 0; --j) {
      double f0 = fm1, b = 0, c = 0, d = 0; /* = 0 to kill compiler warning */
      int pos = i * num_phases + j - 1;
      fm1 = pos > 0 ? coefs[pos - 1] * multiplier : 0;
      switch (interp_order) {
        case 1: b = f1 - f0; break;
        case 2: b = f1 - (.5 * (f2+f0) - f1) - f0; c = .5 * (f2+f0) - f1; break;
        case 3: c=.5*(f1+fm1)-f0;d=(1/6.)*(f2-f1+fm1-f0-4*c);b=f1-f0-d-c; break;
        default: assert(!interp_order);
      }
      switch (core_flags & 3) {
        case 0: if (WITH_CR32 ) STORE(coef , float ); break;
        case 1: if (WITH_CR64 ) STORE(coef , double); break;
        case 2: if (WITH_CR32S) STORE(coef4, float ); break;
        default:if (WITH_CR64S) STORE(coef4, double); break;
      }
      f2 = f1, f1 = f0;
    }
  return result;
}

#undef STORE
#undef coef_coef

#define IS_FLOAT32 (WITH_CR32 || WITH_CR32S) && \
    (!(WITH_CR64 || WITH_CR64S) || sizeof_real == sizeof(float))
#define WITH_FLOAT64 WITH_CR64 || WITH_CR64S

static void dft_stage_fn(stage_t * p, fifo_t * output_fifo)
{
  real * output, * dft_out;
  int i, j, num_in = max(0, fifo_occupancy(&p->fifo));
  rate_shared_t const * s = p->shared;
  dft_filter_t const * f = &s->dft_filter[p->dft_filter_num];
  int const overlap = f->num_taps - 1;

  if (p->at.integer + p->L * num_in >= f->dft_length) {
    fn_t const * const RDFT_CB = p->rdft_cb;
    size_t const sizeof_real = sizeof(char) << LOG2_SIZEOF_REAL(p->core_flags);
    div_t divd = div(f->dft_length - overlap - p->at.integer + p->L - 1, p->L);
    real const * input = fifo_read_ptr(&p->fifo);
    fifo_read(&p->fifo, divd.quot, NULL);
    num_in -= divd.quot;

    output = fifo_reserve(output_fifo, f->dft_length);
    dft_out = (p->core_flags & CORE_SIMD_DFT)? p->dft_out : output;

    if (lsx_is_power_of_2(p->L)) { /* F-domain */
      int portion = f->dft_length / p->L;
      memcpy(dft_out, input, (unsigned)portion * sizeof_real);
      rdft_oforward(portion, f->dft_forward_setup, dft_out, p->dft_scratch);
      if (IS_FLOAT32) {
#define dft_out ((float *)dft_out)
        for (i = portion + 2; i < (portion << 1); i += 2) /* Mirror image. */
          dft_out[i] = dft_out[(portion << 1) - i],
            dft_out[i+1] = -dft_out[(portion << 1) - i + 1];
        dft_out[portion] = dft_out[1];
        dft_out[portion + 1] = 0;
        dft_out[1] = dft_out[0];
#undef dft_out
      }
      else if (WITH_FLOAT64) {
#define dft_out ((double *)dft_out)
        for (i = portion + 2; i < (portion << 1); i += 2) /* Mirror image. */
          dft_out[i] = dft_out[(portion << 1) - i],
            dft_out[i+1] = -dft_out[(portion << 1) - i + 1];
        dft_out[portion] = dft_out[1];
        dft_out[portion + 1] = 0;
        dft_out[1] = dft_out[0];
#undef dft_out
      }

      for (portion <<= 1; i < f->dft_length; i += portion, portion <<= 1) {
        memcpy((char *)dft_out + (size_t)i * sizeof_real, dft_out, (size_t)portion * sizeof_real);
        if (IS_FLOAT32)
        #define dft_out ((float *)dft_out)
          dft_out[i + 1] = 0;
        #undef dft_out
        else if (WITH_FLOAT64)
        #define dft_out ((double *)dft_out)
          dft_out[i + 1] = 0;
        #undef dft_out
      }
      if (p->step.integer > 0)
        rdft_reorder_back(f->dft_length, f->dft_backward_setup, dft_out, p->dft_scratch);
    } else {
      if (p->L == 1)
        memcpy(dft_out, input, (size_t)f->dft_length * sizeof_real);
      else {
        memset(dft_out, 0, (size_t)f->dft_length * sizeof_real);
        if (IS_FLOAT32)
          for (j = 0, i = p->at.integer; i < f->dft_length; ++j, i += p->L)
            ((float *)dft_out)[i] = ((float *)input)[j];
        else if (WITH_FLOAT64)
          for (j = 0, i = p->at.integer; i < f->dft_length; ++j, i += p->L)
            ((double *)dft_out)[i] = ((double *)input)[j];
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
      if ((p->core_flags & CORE_SIMD_DFT) && p->step.integer == 1)
        memcpy(output, dft_out, (size_t)f->dft_length * sizeof_real);
      if (p->step.integer != 1) {
        if (IS_FLOAT32)
          for (j = 0, i = p->remM; i < f->dft_length - overlap; ++j,
              i += p->step.integer)
            ((float *)output)[j] = ((float *)dft_out)[i];
        else if (WITH_FLOAT64)
          for (j = 0, i = p->remM; i < f->dft_length - overlap; ++j,
              i += p->step.integer)
            ((double *)output)[j] = ((double *)dft_out)[i];
        p->remM = i - (f->dft_length - overlap);
        fifo_trim_by(output_fifo, f->dft_length - j);
      }
      else fifo_trim_by(output_fifo, overlap);
    }
    else { /* F-domain */
      int m = -p->step.integer;
      rdft_convolve_portion(f->dft_length >> m, dft_out, f->coefs);
      rdft_obackward(f->dft_length >> m, f->dft_backward_setup, dft_out, p->dft_scratch);
      if (p->core_flags & CORE_SIMD_DFT)
        memcpy(output, dft_out, (size_t)(f->dft_length >> m) * sizeof_real);
      fifo_trim_by(output_fifo, (((1 << m) - 1) * f->dft_length + overlap) >>m);
    }
    (void)RDFT_CB;
  }
  p->input_size = (f->dft_length - p->at.integer + p->L - 1) / p->L;
}

/* Set to 4 x nearest power of 2 or half of that */
/* if danger of causing too many cache misses. */
static int set_dft_length(int num_taps, int min, int large)
{
  double d = log((double)num_taps) / log(2.);
  return 1 << range_limit((int)(d + 2.77), min, max((int)(d + 1.77), large));
}

static void dft_stage_init(
    unsigned instance, double Fp, double Fs, double Fn, double att,
    double phase_response, stage_t * p, int L, int M, double * multiplier,
    unsigned min_dft_size, unsigned large_dft_size, core_flags_t core_flags,
    fn_t const * RDFT_CB)
{
  dft_filter_t * f = &p->shared->dft_filter[instance];
  int num_taps = 0, dft_length = f->dft_length, i, offset;
  bool f_domain_m = abs(3-M) == 1 && Fs <= 1;
  size_t const sizeof_real = sizeof(char) << LOG2_SIZEOF_REAL(core_flags);

  if (!dft_length) {
    int k = phase_response == 50 && lsx_is_power_of_2(L) && Fn == L? L << 1 : 4;
    double m, * h = lsx_design_lpf(Fp, Fs, Fn, att, &num_taps, -k, -1.);

    if (phase_response != 50)
      lsx_fir_to_phase(&h, &num_taps, &f->post_peak, phase_response);
    else f->post_peak = num_taps / 2;

    dft_length = set_dft_length(num_taps, (int)min_dft_size, (int)large_dft_size);
    f->coefs = rdft_calloc((size_t)dft_length, sizeof_real);
    offset = dft_length - num_taps + 1;
    m = (1. / dft_length) * rdft_multiplier() * L * *multiplier;
    if (IS_FLOAT32) for (i = 0; i < num_taps; ++i)
        ((float *)f->coefs)[(i + offset) & (dft_length - 1)] =(float)(h[i] * m);
    else if (WITH_FLOAT64) for (i = 0; i < num_taps; ++i)
        ((double *)f->coefs)[(i + offset) & (dft_length - 1)] = h[i] * m;
    free(h);
  }

  if (rdft_flags() & RDFT_IS_SIMD)
    p->dft_out = rdft_malloc(sizeof_real * (size_t)dft_length);
  if (rdft_flags() & RDFT_NEEDS_SCRATCH)
    p->dft_scratch = rdft_malloc(2 * sizeof_real * (size_t)dft_length);

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
  p->core_flags = core_flags;
  p->rdft_cb = RDFT_CB;
  p->fn = dft_stage_fn;
  p->preload = f->post_peak / L;
  p->at.integer = f->post_peak % L;
  p->L = L;
  p->step.integer = f_domain_m? -M/2 : M;
  p->dft_filter_num = instance;
  p->block_len = f->dft_length - (f->num_taps - 1);
  p->phase0 = p->at.integer / p->L;
  p->input_size = (f->dft_length - p->at.integer + p->L - 1) / p->L;
}

static struct half_fir_info const * find_half_fir(
    struct half_fir_info const * firs, size_t len, double att)
{
  size_t i;
  for (i = 0; i + 1 < len && att > firs[i].att; ++i);
  return &firs[i];
}

#define have_pre_stage  (preM  * preL  != 1)
#define have_arb_stage  (arbM  * arbL  != 1)
#define have_post_stage (postM * postL != 1)

#include "soxr.h"

STATIC char const * _soxr_init(
  rate_t * const p,             /* Per audio channel. */
  rate_shared_t * const shared, /* By channels undergoing same rate change. */
  double const io_ratio,        /* Input rate divided by output rate. */
  soxr_quality_spec_t const * const q_spec,
  soxr_runtime_spec_t const * const r_spec,
  double multiplier,            /* Linear gain to apply during conversion. */
  cr_core_t const * const core,
  core_flags_t const core_flags)
{
  size_t const sizeof_real = sizeof(char) << LOG2_SIZEOF_REAL(core_flags);
  double const tolerance = 1 + 1e-5;

  double       bits = q_spec->precision;
  rolloff_t const rolloff = (rolloff_t)(q_spec->flags & 3);
  int interpolator = (int)(r_spec->flags & 3) - 1;
  double const Fp0 = q_spec->passband_end, Fs0 = q_spec->stopband_begin;
  double const phase_response = q_spec->phase_response, tbw0 = Fs0-Fp0;

  bool const maintain_3dB_pt = !!(q_spec->flags & SOXR_MAINTAIN_3DB_PT);
  double tbw_tighten = 1, alpha;
  #define tighten(x) (Fs0-(Fs0-(x))*tbw_tighten)

  double arbM = io_ratio, Fn1, Fp1 = Fp0, Fs1 = Fs0, bits1 = min(bits,33);
  double att = (bits1 + 1) * linear_to_dB(2.), attArb = att; /* +1: pass+stop */
  int preL = 1, preM = 1, shr = 0, arbL = 1, postL = 1, postM = 1;
  bool upsample=false, rational=false, iOpt=!(r_spec->flags&SOXR_NOSMALLINTOPT);
  bool lq_bits= (q_spec->flags & SOXR_PROMOTE_TO_LQ)? bits <= 16 : bits == 16;
  bool lq_Fp0 = (q_spec->flags & SOXR_PROMOTE_TO_LQ)? Fp0<=lq_bw0 : Fp0==lq_bw0;
  int n = 0, i, mode = lq_bits && rolloff == rolloff_medium? io_ratio > 1 ||
    phase_response != 50 || !lq_Fp0 || Fs0 != 1 : ((int)ceil(bits1) - 6) / 4;
  struct half_fir_info const * half_fir_info;
  stage_t * s;

  if (io_ratio < 1 && Fs0 - 1 > 1 - Fp0 / tolerance)
    return "imaging greater than rolloff";
  if (.002 / tolerance > tbw0 || tbw0 > .5 * tolerance)
    return "transition bandwidth not in [0.2,50] % of nyquist";
  if (.5 / tolerance > Fp0 || Fs0 > 1.5 * tolerance)
    return "transition band not within [50,150] % of nyquist";
  if (bits!=0 && (15 > bits || bits > 33))
    return "precision not in [15,33] bits";
  if (io_ratio <= 0)
    return "resampling factor not positive";
  if (0 > phase_response || phase_response > 100)
    return "phase response not in [0=min-phase,100=max-phase] %";

  p->core = core;
  p->io_ratio = io_ratio;
  if (bits!=0) while (!n++) {                            /* Determine stages: */
    int try, L, M, x, maxL = interpolator > 0? 1 : mode? 2048 :
      (int)ceil(r_spec->coef_size_kbytes * 1000. / (U100_l * (int)sizeof_real));
    double d, epsilon = 0, frac;
    upsample = arbM < 1;
    for (i = (int)(.5 * arbM), shr = 0; i >>= 1; arbM *= .5, ++shr);
    preM = upsample || (arbM > 1.5 && arbM < 2);
    postM = 1 + (arbM > 1 && preM), arbM /= postM;
    preL = 1 + (!preM && arbM < 2) + (upsample && mode), arbM *= preL;
    if ((frac = arbM - (int)arbM)!=0)
      epsilon = fabs(floor(frac * MULT32 + .5) / (frac * MULT32) - 1);
    for (i = 1, rational = frac==0; i <= maxL && !rational; ++i) {
      d = frac * i, try = (int)(d + .5);
      if ((rational = fabs(try / d - 1) <= epsilon)) {    /* No long doubles! */
        if (try == i)
          arbM = ceil(arbM), shr += x = arbM > 3, arbM /= 1 + x;
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

  p->num_stages = shr + have_pre_stage + have_arb_stage + have_post_stage;
  if (!p->num_stages && multiplier != 1) {
    bits = arbL = 0;                         /* Use cubic_stage in this case. */
    ++p->num_stages;
  }
  p->stages = calloc((size_t)p->num_stages + 1, sizeof(*p->stages));
  if (!p->stages)
    return "out of memory";
  for (i = 0; i < p->num_stages; ++i) {
    p->stages[i].num = i;
    p->stages[i].shared = shared;
    p->stages[i].input_size = 8192;
  }
  p->stages[0].is_input = true;

  alpha = postM / (io_ratio * (postL << 0));

  if ((n = p->num_stages) > 1) {                              /* Att. budget: */
    if (have_arb_stage)
      att += linear_to_dB(2.), attArb = att, --n;
    att += linear_to_dB((double)n);
  }

  half_fir_info = find_half_fir(core->half_firs, core->half_firs_len, att);
  for (i = 0, s = p->stages; i < shr; ++i, ++s) {
    s->fn = half_fir_info->fn;
    s->coefs = half_fir_info->coefs;
    s->n = half_fir_info->num_coefs;
    s->pre_post = 4 * s->n;
    s->preload = s->pre = s->pre_post >> 1;
  }

  if (have_pre_stage) {
    if (maintain_3dB_pt && have_post_stage) {    /* Trans. bands overlapping. */
      double x = tbw0 * lsx_inv_f_resp(-3., att);
      x = -lsx_f_resp(x / (max(2 * alpha - Fs0, alpha) - Fp0), att);
      if (x > .035) {
        tbw_tighten = ((4.3074e-3 - 3.9121e-4 * x) * x - .040009) * x + 1.0014;
        lsx_debug("tbw_tighten=%g (%gdB)", tbw_tighten, x);
      }
    }
    Fn1 = preM? max(preL, preM) : arbM / arbL;
    dft_stage_init(0, tighten(Fp1), Fs1, Fn1, att, phase_response, s++, preL,
        max(preM, 1), &multiplier, r_spec->log2_min_dft_size,
        r_spec->log2_large_dft_size, core_flags, core->rdft_cb);
    Fp1 /= Fn1, Fs1 /= Fn1;
  }

  if (bits==0 && have_arb_stage) {                /* `Quick' cubic arb stage: */
    s->fn = core->cubic_stage_fn;
    s->mult = multiplier, multiplier = 1;
    s->step.whole = (int64_t)(arbM * MULT32 + .5);
    s->pre_post = max(3, s->step.integer);
    s->preload = s->pre = 1;
    s->out_in_ratio = MULT32 / (double)s->step.whole;
  }
  else if (have_arb_stage) {                     /* Higher quality arb stage: */
    static const float rolloffs[] = {-.01f, -.3f, 0, -.103f};
    poly_fir_t const * f = &core->poly_firs[6*(upsample+!!preM)+mode-!upsample];
    int order, num_coefs = (int)f->interp[0].scalar, phase_bits, phases;
    size_t coefs_size;
    double at, Fp = Fp1, Fs, Fn, mult = upsample? 1 : arbM / arbL;
    poly_fir1_t const * f1;

    if (!upsample && preM)
      Fn = 2 * mult, Fs = 3 + fabs(Fs1 - 1);
    else Fn = 1, Fs = 2 - (mode? Fp1 + (Fs1 - Fp1) * .7 : Fs1);

    if (mode)
      Fp = Fs - (Fs - Fp) / (1 - lsx_inv_f_resp(rolloffs[rolloff], attArb));

    i = (interpolator < 0? !rational : max(interpolator, !rational)) - 1;
    do {
      f1 = &f->interp[++i];
      assert(f1->fn);
      if (i)
        arbM /= arbL, arbL = 1, rational = false;
      phase_bits = (int)ceil(f1->scalar - log(mult)/log(2.));
      phases = !rational? (1 << phase_bits) : arbL;
      if (f->interp[0].scalar==0) {
        int phases0 = max(phases, 19), n0 = 0;
        lsx_design_lpf(Fp, Fs, -Fn, attArb, &n0, phases0, f->beta);
        num_coefs = n0 / phases0 + 1, num_coefs += num_coefs & !preM;
      }
      if ((num_coefs & 1) && rational && (arbL & 1))
        phases <<= 1, arbL <<= 1, arbM *= 2;
      at = arbL * (s->phase0 = .5 * (num_coefs & 1));
      order = i + (i && mode > 4);
      coefs_size = (size_t)(num_coefs4 * phases * (order+1)) * sizeof_real;
    } while (interpolator < 0 && i < 2 && f->interp[i+1].fn &&
        coefs_size / 1000 > r_spec->coef_size_kbytes);

    if (!s->shared->poly_fir_coefs) {
      int num_taps = num_coefs * phases - 1;
      double * coefs = lsx_design_lpf(
          Fp, Fs, Fn, attArb, &num_taps, phases, f->beta);
      s->shared->poly_fir_coefs = prepare_poly_fir_coefs(
          coefs, num_coefs, phases, order, multiplier, core_flags, &core->mem);
      lsx_debug("fir_len=%i phases=%i coef_interp=%i size=%.3gk",
          num_coefs, phases, order, (double)coefs_size / 1000.);
      free(coefs);
    }
    multiplier = 1;
    s->fn = f1->fn;
    s->pre_post = num_coefs4 - 1;
    s->preload = ((num_coefs - 1) >> 1) + (num_coefs4 - num_coefs);
    s->n = num_coefs4;
    s->phase_bits = phase_bits;
    s->L = arbL;
    s->use_hi_prec_clock =
      mode>1 && (q_spec->flags & SOXR_HI_PREC_CLOCK) && !rational;
#if WITH_FLOAT_STD_PREC_CLOCK
    if (order && !s->use_hi_prec_clock) {
      s->at.flt = at;
      s->step.flt = arbM;
      s->out_in_ratio = (double)(arbL / s->step.flt);
    } else
#endif
    {
      s->at.whole = (int64_t)(at * MULT32 + .5);
#if WITH_HI_PREC_CLOCK
      if (s->use_hi_prec_clock) {
        double M = arbM * MULT32;
        s->at.fix.ls.parts.ms = 0x80000000ul;
        s->step.whole = (int64_t)M;
        M -= (double)s->step.whole;
        M *= MULT32 * MULT32;
        s->step.fix.ls.all = (uint64_t)M;
      } else
#endif
        s->step.whole = (int64_t)(arbM * MULT32 + .5);
      s->out_in_ratio = MULT32 * arbL / (double)s->step.whole;
    }
    ++s;
  }

  if (have_post_stage)
    dft_stage_init(1, tighten(Fp0 / (upsample? alpha : 1)), upsample? max(2 -
        Fs0 / alpha, 1) : Fs0, (double)max(postL, postM), att, phase_response,
        s++, postL, postM, &multiplier, r_spec->log2_min_dft_size,
        r_spec->log2_large_dft_size, core_flags, core->rdft_cb);

  lsx_debug("%g: >>%i %i/%i %i/%g %i/%i (%x)", 1/io_ratio,
      shr, preL, preM, arbL, arbM, postL, postM, core_flags);

  for (i = 0, s = p->stages; i < p->num_stages; ++i, ++s) {
    fifo_create(&s->fifo, (int)sizeof_real);
    memset(fifo_reserve(&s->fifo, s->preload), 0,
        sizeof_real * (size_t)s->preload);
    lsx_debug_more("%5i|%-5i preload=%i remL=%i",
        s->pre, s->pre_post-s->pre, s->preload, s->at.integer);
  }
  fifo_create(&s->fifo, (int)sizeof_real);
  return 0;
}

static bool stage_process(stage_t * stage, bool flushing)
{
  fifo_t * fifo = &stage->fifo;
  bool done = false;
  int want;
  while (!done && (want = stage->input_size - fifo_occupancy(fifo)) > 0) {
    if (stage->is_input) {
      if (flushing)
        memset(fifo_reserve(fifo, want), 0, fifo->item_size * (size_t)want);
      else done = true;
    }
    else done = stage_process(stage - 1, flushing);
  }
  stage->fn(stage, &stage[1].fifo);
  return done && fifo_occupancy(fifo) < stage->input_size;
}

STATIC void _soxr_process(rate_t * p, size_t olen)
{
  int const n = p->flushing? min(-(int)p->samples_out, (int)olen) : (int)olen;
  stage_t * stage = &p->stages[p->num_stages];
  fifo_t * fifo = &stage->fifo;
  bool done = false;
  while (!done && fifo_occupancy(fifo) < (int)n)
    done = stage->is_input || stage_process(stage - 1, p->flushing);
}

STATIC real * _soxr_input(rate_t * p, real const * samples, size_t n)
{
  if (p->flushing)
    return 0;
  p->samples_in += (int64_t)n;
  return fifo_write(&p->stages[0].fifo, (int)n, samples);
}

STATIC real const * _soxr_output(rate_t * p, real * samples, size_t * n0)
{
  fifo_t * fifo = &p->stages[p->num_stages].fifo;
  int n = p->flushing? min(-(int)p->samples_out, (int)*n0) : (int)*n0;
  p->samples_out += n = min(n, fifo_occupancy(fifo));
  return fifo_read(fifo, (int)(*n0 = (size_t)n), samples);
}

STATIC void _soxr_flush(rate_t * p)
{
  if (p->flushing) return;
  p->samples_out -= (int64_t)((double)p->samples_in / p->io_ratio + .5);
  p->samples_in = 0;
  p->flushing = true;
}

STATIC void _soxr_close(rate_t * p)
{
  if (p->stages) {
    fn_t const * const RDFT_CB = p->core->rdft_cb;
    rate_shared_t * shared = p->stages[0].shared;
    int i;

    for (i = 0; i <= p->num_stages; ++i) {
      stage_t * s = &p->stages[i];
      rdft_free(s->dft_scratch);
      rdft_free(s->dft_out);
      fifo_delete(&s->fifo);
    }
    if (shared) {
      for (i = 0; i < 2; ++i) {
        dft_filter_t * f= &shared->dft_filter[i];
        rdft_free(f->coefs);
        rdft_delete_setup(f->dft_forward_setup);
        rdft_delete_setup(f->dft_backward_setup);
      }
      p->core->mem.free(shared->poly_fir_coefs);
      memset(shared, 0, sizeof(*shared));
    }
    free(p->stages);
    (void)RDFT_CB;
  }
}

#if defined SOXR_LIB
STATIC double _soxr_delay(rate_t * p)
{
  return (double)p->samples_in / p->io_ratio - (double)p->samples_out;
}

STATIC void _soxr_sizes(size_t * shared, size_t * channel)
{
  *shared = sizeof(rate_shared_t);
  *channel = sizeof(rate_t);
}
#endif
