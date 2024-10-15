/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Resample using an interpolated poly-phase FIR with length LEN. */
/* Input must be followed by FIR_LENGTH-1 samples. */

#if COEF_INTERP != 1 && COEF_INTERP != 2 && COEF_INTERP != 3
  #error COEF_INTERP
#endif

#if SIMD_AVX || SIMD_SSE || SIMD_NEON
  #define N (FIR_LENGTH>>2)

  #if COEF_INTERP == 1
    #define _ sum=vMac(vMac(b,X,a),vLdu(in+j*4),sum), ++j;
  #elif COEF_INTERP == 2
    #define _ sum=vMac(vMac(vMac(c,X,b),X,a),vLdu(in+j*4),sum), ++j;
  #else
    #define _ sum=vMac(vMac(vMac(vMac(d,X,c),X,b),X,a),vLdu(in+j*4),sum), ++j;
  #endif

  #define a coefs[(COEF_INTERP+1)*(N*phase+j)+(COEF_INTERP-0)]
  #define b coefs[(COEF_INTERP+1)*(N*phase+j)+(COEF_INTERP-1)]
  #define c coefs[(COEF_INTERP+1)*(N*phase+j)+(COEF_INTERP-2)]
  #define d coefs[(COEF_INTERP+1)*(N*phase+j)+(COEF_INTERP-3)]

  #define BEGINNING v4_t X = vLds(x), sum = vZero(); \
      v4_t const * const __restrict coefs = (v4_t *)COEFS
  #define END vStorSum(output+i, sum)
  #define cc(n) case n: core(n); break
  #define CORE(n) switch (n) {cc(2); cc(3); cc(4); cc(5); cc(6); default: core(n);}
#else
  #define N FIR_LENGTH

  #if COEF_INTERP == 1
    #define _ sum += (b*x + a)*in[j], ++j;
  #elif COEF_INTERP == 2
    #define _ sum += ((c*x + b)*x + a)*in[j], ++j;
  #else
    #define _ sum += (((d*x + c)*x + b)*x + a)*in[j], ++j;
  #endif

  #define a (coef(COEFS, COEF_INTERP, N, phase, 0,j))
  #define b (coef(COEFS, COEF_INTERP, N, phase, 1,j))
  #define c (coef(COEFS, COEF_INTERP, N, phase, 2,j))
  #define d (coef(COEFS, COEF_INTERP, N, phase, 3,j))

  #define BEGINNING sample_t sum = 0
  #define END output[i] = sum
  #define CORE(n) core(n)
#endif



#define floatPrecCore(n) { \
  float_step_t at = p->at.flt; \
  for (i = 0; (int)at < num_in; ++i, at += p->step.flt) { \
    sample_t const * const __restrict in = input + (int)at; \
    float_step_t frac = at - (int)at; \
    int phase = (int)(frac * (1 << PHASE_BITS)); \
    sample_t x = (sample_t)(frac * (1 << PHASE_BITS) - phase); \
    int j = 0; \
    BEGINNING; CONVOLVE(n); END; \
  } \
  fifo_read(&p->fifo, (int)at, NULL); \
  p->at.flt = at - (int)at; } /* Could round to 1 in some cirmcumstances. */



#define highPrecCore(n) { \
  step_t at; at.fix = p->at.fix; \
  for (i = 0; at.integer < num_in; ++i, \
      at.fix.ls.all += p->step.fix.ls.all, \
      at.whole += p->step.whole + (at.fix.ls.all < p->step.fix.ls.all)) { \
    sample_t const * const __restrict in = input + at.integer; \
    uint32_t frac = at.fraction; \
    int phase = (int)(frac >> (32 - PHASE_BITS)); /* High-order bits */ \
    /* Low-order bits, scaled to [0,1): */ \
    sample_t x = (sample_t)((frac << PHASE_BITS) * (1 / MULT32)); \
    int j = 0; \
    BEGINNING; CONVOLVE(n); END; \
  } \
  fifo_read(&p->fifo, at.integer, NULL); \
  p->at.whole = at.fraction; \
  p->at.fix.ls = at.fix.ls; }



#define stdPrecCore(n) { \
  int64p_t at; at.all = p->at.whole; \
  for (i = 0; at.parts.ms < num_in; ++i, at.all += p->step.whole) { \
    sample_t const * const __restrict in = input + at.parts.ms; \
    uint32_t const frac = at.parts.ls; \
    int phase = (int)(frac >> (32 - PHASE_BITS)); /* high-order bits */ \
    /* Low-order bits, scaled to [0,1): */ \
    sample_t x = (sample_t)((frac << PHASE_BITS) * (1 / MULT32)); \
    int j = 0; \
    BEGINNING; CONVOLVE(n); END; \
  } \
  fifo_read(&p->fifo, at.parts.ms, NULL); \
  p->at.whole = at.parts.ls; }



#if WITH_FLOAT_STD_PREC_CLOCK
  #define SPCORE floatPrecCore
#else
  #define SPCORE stdPrecCore
#endif



#if WITH_HI_PREC_CLOCK
  #define core(n) if (p->use_hi_prec_clock) highPrecCore(n) else SPCORE(n)
#else
  #define core(n) SPCORE(n)
#endif



static void FUNCTION(stage_t * p, fifo_t * output_fifo)
{
  sample_t const * input = stage_read_p(p);
  int num_in = min(stage_occupancy(p), p->input_size);
  int i, max_num_out = 1 + (int)(num_in * p->out_in_ratio);
  sample_t * const __restrict output = fifo_reserve(output_fifo, max_num_out);

  CORE(N);
  assert(max_num_out - i >= 0);
  fifo_trim_by(output_fifo, max_num_out - i);
}



#undef _
#undef a
#undef b
#undef c
#undef d
#undef CORE
#undef cc
#undef core
#undef COEF_INTERP
#undef N
#undef BEGINNING
#undef END
#undef CONVOLVE
#undef FIR_LENGTH
#undef FUNCTION
#undef PHASE_BITS
