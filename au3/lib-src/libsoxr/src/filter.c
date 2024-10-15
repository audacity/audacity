/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include "filter.h"

#include "math-wrap.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "fft4g.h"
#include "ccrw2.h"

#if 1 || WITH_CR64 || WITH_CR64S /* Always need this, for lsx_fir_to_phase. */
#define DFT_FLOAT double
#define DONE_WITH_FFT_CACHE done_with_fft_cache
#define FFT_CACHE_CCRW fft_cache_ccrw
#define FFT_LEN fft_len
#define LSX_CDFT lsx_cdft
#define LSX_CLEAR_FFT_CACHE lsx_clear_fft_cache
#define LSX_FFT_BR lsx_fft_br
#define LSX_FFT_SC lsx_fft_sc
#define LSX_INIT_FFT_CACHE lsx_init_fft_cache
#define LSX_RDFT lsx_rdft
#define LSX_SAFE_CDFT lsx_safe_cdft
#define LSX_SAFE_RDFT lsx_safe_rdft
#define UPDATE_FFT_CACHE update_fft_cache
#include "fft4g_cache.h"
#endif

#if (WITH_CR32 && !AVCODEC_FOUND) || (WITH_CR32S && !AVCODEC_FOUND && !WITH_PFFFT)
#define DFT_FLOAT float
#define DONE_WITH_FFT_CACHE done_with_fft_cache_f
#define FFT_CACHE_CCRW fft_cache_ccrw_f
#define FFT_LEN fft_len_f
#define LSX_CDFT lsx_cdft_f
#define LSX_CLEAR_FFT_CACHE lsx_clear_fft_cache_f
#define LSX_FFT_BR lsx_fft_br_f
#define LSX_FFT_SC lsx_fft_sc_f
#define LSX_INIT_FFT_CACHE lsx_init_fft_cache_f
#define LSX_RDFT lsx_rdft_f
#define LSX_SAFE_CDFT lsx_safe_cdft_f
#define LSX_SAFE_RDFT lsx_safe_rdft_f
#define UPDATE_FFT_CACHE update_fft_cache_f
#include "fft4g_cache.h"
#endif

#if WITH_CR64 || WITH_CR64S || !SOXR_LIB
#define DFT_FLOAT double
#define ORDERED_CONVOLVE lsx_ordered_convolve
#define ORDERED_PARTIAL_CONVOLVE lsx_ordered_partial_convolve
#include "rdft.h"
#endif

#if WITH_CR32
#define DFT_FLOAT float
#define ORDERED_CONVOLVE lsx_ordered_convolve_f
#define ORDERED_PARTIAL_CONVOLVE lsx_ordered_partial_convolve_f
#include "rdft.h"
#endif

double lsx_kaiser_beta(double att, double tr_bw)
{
  if (att >= 60) {
    static const double coefs[][4] = {
      {-6.784957e-10,1.02856e-05,0.1087556,-0.8988365+.001},
      {-6.897885e-10,1.027433e-05,0.10876,-0.8994658+.002},
      {-1.000683e-09,1.030092e-05,0.1087677,-0.9007898+.003},
      {-3.654474e-10,1.040631e-05,0.1087085,-0.8977766+.006},
      {8.106988e-09,6.983091e-06,0.1091387,-0.9172048+.015},
      {9.519571e-09,7.272678e-06,0.1090068,-0.9140768+.025},
      {-5.626821e-09,1.342186e-05,0.1083999,-0.9065452+.05},
      {-9.965946e-08,5.073548e-05,0.1040967,-0.7672778+.085},
      {1.604808e-07,-5.856462e-05,0.1185998,-1.34824+.1},
      {-1.511964e-07,6.363034e-05,0.1064627,-0.9876665+.18},
    };
    double realm = log(tr_bw/.0005)/log(2.);
    double const * c0 = coefs[range_limit(  (int)realm, 0, (int)array_length(coefs)-1)];
    double const * c1 = coefs[range_limit(1+(int)realm, 0, (int)array_length(coefs)-1)];
    double b0 = ((c0[0]*att + c0[1])*att + c0[2])*att + c0[3];
    double b1 = ((c1[0]*att + c1[1])*att + c1[2])*att + c1[3];
    return b0 + (b1 - b0) * (realm - (int)realm);
  }
  if (att > 50   ) return .1102 * (att - 8.7);
  if (att > 20.96) return .58417 * pow(att -20.96, .4) + .07886 * (att - 20.96);
  return 0;
}

double * lsx_make_lpf(
    int num_taps, double Fc, double beta, double rho, double scale)
{
  int i, m = num_taps - 1;
  double * h = malloc((size_t)num_taps * sizeof(*h));
  double mult = scale / lsx_bessel_I_0(beta), mult1 = 1 / (.5 * m + rho);
  assert(Fc >= 0 && Fc <= 1);
  lsx_debug("make_lpf(n=%i Fc=%.7g beta=%g rho=%g scale=%g)",
      num_taps, Fc, beta, rho, scale);

  if (h) for (i = 0; i <= m / 2; ++i) {
    double z = i - .5 * m, x = z * M_PI, y = z * mult1;
    h[i] = x!=0? sin(Fc * x) / x : Fc;
    h[i] *= lsx_bessel_I_0(beta * sqrt(1 - y * y)) * mult;
    if (m - i != i)
      h[m - i] = h[i];
  }
  return h;
}

void lsx_kaiser_params(double att, double Fc, double tr_bw, double * beta, int * num_taps)
{
  *beta = *beta < 0? lsx_kaiser_beta(att, tr_bw * .5 / Fc): *beta;
  att = att < 60? (att - 7.95) / (2.285 * M_PI * 2) :
    ((.0007528358-1.577737e-05**beta)**beta+.6248022)**beta+.06186902;
  *num_taps = !*num_taps? (int)ceil(att/tr_bw + 1) : *num_taps;
}

double * lsx_design_lpf(
    double Fp,      /* End of pass-band */
    double Fs,      /* Start of stop-band */
    double Fn,      /* Nyquist freq; e.g. 0.5, 1, PI */
    double att,     /* Stop-band attenuation in dB */
    int * num_taps, /* 0: value will be estimated */
    int k,          /* >0: number of phases; <0: num_taps = 1 (mod -k) */
    double beta)    /* <0: value will be estimated */
{
  int n = *num_taps, phases = max(k, 1), modulo = max(-k, 1);
  double tr_bw, Fc, rho = phases == 1? .5 : att < 120? .63 : .75;

  lsx_debug_more("./sinctest %-12.7g %-12.7g %g 0 %-5g %i %i 50 %g %g -4 >1",
      Fp, Fs, Fn, att, *num_taps, k, beta, rho);

  Fp /= fabs(Fn), Fs /= fabs(Fn);        /* Normalise to Fn = 1 */
  tr_bw = .5 * (Fs - Fp); /* Transition band-width: 6dB to stop points */
  tr_bw /= phases, Fs /= phases;
  tr_bw = min(tr_bw, .5 * Fs);
  Fc = Fs - tr_bw;
  assert(Fc - tr_bw >= 0);
  lsx_kaiser_params(att, Fc, tr_bw, &beta, num_taps);
  if (!n)
    *num_taps = phases > 1? *num_taps / phases * phases + phases - 1 :
      (*num_taps + modulo - 2) / modulo * modulo + 1;
  return Fn < 0? 0 : lsx_make_lpf(*num_taps, Fc, beta, rho, (double)phases);
}

static double safe_log(double x)
{
  assert(x >= 0);
  if (x!=0)
    return log(x);
  lsx_debug("log(0)");
  return -26;
}

void lsx_fir_to_phase(double * * h, int * len, int * post_len, double phase)
{
  double * pi_wraps, * work, phase1 = (phase > 50 ? 100 - phase : phase) / 50;
  int i, work_len, begin, end, imp_peak = 0, peak = 0;
  double imp_sum = 0, peak_imp_sum = 0;
  double prev_angle2 = 0, cum_2pi = 0, prev_angle1 = 0, cum_1pi = 0;

  for (i = *len, work_len = 2 * 2 * 8; i > 1; work_len <<= 1, i >>= 1);

  work = calloc((size_t)work_len + 2, sizeof(*work)); /* +2: (UN)PACK */
  pi_wraps = malloc((((size_t)work_len + 2) / 2) * sizeof(*pi_wraps));

  memcpy(work, *h, (size_t)*len * sizeof(*work));
  lsx_safe_rdft(work_len, 1, work); /* Cepstral: */
  LSX_UNPACK(work, work_len);

  for (i = 0; i <= work_len; i += 2) {
    double angle = atan2(work[i + 1], work[i]);
    double detect = 2 * M_PI;
    double delta = angle - prev_angle2;
    double adjust = detect * ((delta < -detect * .7) - (delta > detect * .7));
    prev_angle2 = angle;
    cum_2pi += adjust;
    angle += cum_2pi;
    detect = M_PI;
    delta = angle - prev_angle1;
    adjust = detect * ((delta < -detect * .7) - (delta > detect * .7));
    prev_angle1 = angle;
    cum_1pi += fabs(adjust); /* fabs for when 2pi and 1pi have combined */
    pi_wraps[i >> 1] = cum_1pi;

    work[i] = safe_log(sqrt(sqr(work[i]) + sqr(work[i + 1])));
    work[i + 1] = 0;
  }
  LSX_PACK(work, work_len);
  lsx_safe_rdft(work_len, -1, work);
  for (i = 0; i < work_len; ++i) work[i] *= 2. / work_len;

  for (i = 1; i < work_len / 2; ++i) { /* Window to reject acausal components */
    work[i] *= 2;
    work[i + work_len / 2] = 0;
  }
  lsx_safe_rdft(work_len, 1, work);

  for (i = 2; i < work_len; i += 2) /* Interpolate between linear & min phase */
    work[i + 1] = phase1 * i / work_len * pi_wraps[work_len >> 1] +
        (1 - phase1) * (work[i + 1] + pi_wraps[i >> 1]) - pi_wraps[i >> 1];

  work[0] = exp(work[0]), work[1] = exp(work[1]);
  for (i = 2; i < work_len; i += 2) {
    double x = exp(work[i]);
    work[i    ] = x * cos(work[i + 1]);
    work[i + 1] = x * sin(work[i + 1]);
  }

  lsx_safe_rdft(work_len, -1, work);
  for (i = 0; i < work_len; ++i) work[i] *= 2. / work_len;

  /* Find peak pos. */
  for (i = 0; i <= (int)(pi_wraps[work_len >> 1] / M_PI + .5); ++i) {
    imp_sum += work[i];
    if (fabs(imp_sum) > fabs(peak_imp_sum)) {
      peak_imp_sum = imp_sum;
      peak = i;
    }
    if (work[i] > work[imp_peak]) /* For debug check only */
      imp_peak = i;
  }
  while (peak && fabs(work[peak-1]) > fabs(work[peak]) && work[peak-1] * work[peak] > 0)
    --peak;

  if (phase1==0)
    begin = 0;
  else if (phase1 == 1)
    begin = peak - *len / 2;
  else {
    begin = (int)((.997 - (2 - phase1) * .22) * *len + .5);
    end   = (int)((.997 + (0 - phase1) * .22) * *len + .5);
    begin = peak - (begin & ~3);
    end   = peak + 1 + ((end + 3) & ~3);
    *len = end - begin;
    *h = realloc(*h, (size_t)*len * sizeof(**h));
  }
  for (i = 0; i < *len; ++i) (*h)[i] =
    work[(begin + (phase > 50 ? *len - 1 - i : i) + work_len) & (work_len - 1)];
  *post_len = phase > 50 ? peak - begin : begin + *len - (peak + 1);

  lsx_debug("nPI=%g peak-sum@%i=%g (val@%i=%g); len=%i post=%i (%g%%)",
      pi_wraps[work_len >> 1] / M_PI, peak, peak_imp_sum, imp_peak,
      work[imp_peak], *len, *post_len, 100 - 100. * *post_len / (*len - 1));
  free(pi_wraps), free(work);
}

#define F_x(F,expr) static double F(double x) {return expr;}
F_x(sinePhi, ((2.0517e-07*x-1.1303e-04)*x+.023154)*x+.55924 )
F_x(sinePsi, ((9.0667e-08*x-5.6114e-05)*x+.013658)*x+1.0977 )
F_x(sinePow, log(.5)/log(sin(x*.5)) )
#define dB_to_linear(x) exp((x) * (M_LN10 * 0.05))

double lsx_f_resp(double t, double a)
{
  double x;
  if (t > (a <= 160? .8 : .82)) {
    double a1 = a+15;
    double p = .00035*a+.375;
    double w = 1/(1-.597)*asin(pow((a1-10.6)/a1,1/p));
    double c = 1+asin(pow(1-a/a1,1/p))/w;
    return a1*(pow(sin((c-t)*w),p)-1);
  }
  if (t > .5)
    x = sinePsi(a), x = pow(sin((1-t) * x), sinePow(x));
  else
    x = sinePhi(a), x = 1 - pow(sin(t * x), sinePow(x));
  return linear_to_dB(x);
}

double lsx_inv_f_resp(double drop, double a)
{
  double x = sinePhi(a), s;
  drop = dB_to_linear(drop);
  s = drop > .5 ? 1 - drop : drop;
  x = asin(pow(s, 1/sinePow(x))) / x;
  return drop > .5? x : 1 -x;
}
