/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Resample using an interpolated poly-phase FIR with length LEN.*/
/* Input must be followed by LEN-1 samples. */

#define a (coef(p->shared->poly_fir_coefs, COEF_INTERP, FIR_LENGTH, phase, 0,j))
#define b (coef(p->shared->poly_fir_coefs, COEF_INTERP, FIR_LENGTH, phase, 1,j))
#define c (coef(p->shared->poly_fir_coefs, COEF_INTERP, FIR_LENGTH, phase, 2,j))
#define d (coef(p->shared->poly_fir_coefs, COEF_INTERP, FIR_LENGTH, phase, 3,j))
#if COEF_INTERP == 0
  #define _ sum += a *in[j], ++j;
#elif COEF_INTERP == 1
  #define _ sum += (b *x + a)*in[j], ++j;
#elif COEF_INTERP == 2
  #define _ sum += ((c *x + b)*x + a)*in[j], ++j;
#elif COEF_INTERP == 3
  #define _ sum += (((d*x + c)*x + b)*x + a)*in[j], ++j;
#else
  #error COEF_INTERP
#endif

static void FUNCTION(stage_t * p, fifo_t * output_fifo)
{
  sample_t const * input = stage_read_p(p);
  int i, num_in = stage_occupancy(p), max_num_out = 1 + (int)(num_in*p->out_in_ratio);
  sample_t * output = fifo_reserve(output_fifo, max_num_out);

#if defined HI_PREC_CLOCK
#if FLOAT_HI_PREC_CLOCK
  if (p->use_hi_prec_clock) {
    float_step_t at = p->at.flt;
    for (i = 0; (int)at < num_in; ++i, at += p->step.flt) {
      sample_t const * in = input + (int)at;
      float_step_t frac = at - (int)at;
      int phase = (int)(frac * (1 << PHASE_BITS));
#if COEF_INTERP > 0
      sample_t x = (sample_t)(frac * (1 << PHASE_BITS) - phase);
#endif
      sample_t sum = 0;
      int j = 0;
      CONVOLVE
      output[i] = sum;
    }
    fifo_read(&p->fifo, (int)at, NULL);
    p->at.flt = at - (int)at;
  } else
#else
  if (p->use_hi_prec_clock) {
    for (i = 0; p->at.integer < num_in; ++i,
        p->at.fix.ls.all += p->step.fix.ls.all,
        p->at.whole += p->step.whole + (p->at.fix.ls.all < p->step.fix.ls.all)) {
      sample_t const * in = input + p->at.integer;
      uint32_t frac = p->at.fraction;
      int phase = (int)(frac >> (32 - PHASE_BITS)); /* high-order bits */
#if COEF_INTERP > 0              /* low-order bits, scaled to [0,1) */
      sample_t x = (sample_t)((frac << PHASE_BITS) * (1 / MULT32));
#endif
      sample_t sum = 0;
      int j = 0;
      CONVOLVE
      output[i] = sum;
    }
    fifo_read(&p->fifo, p->at.integer, NULL);
    p->at.integer = 0;
  } else
#endif
#endif
  {
    for (i = 0; p->at.integer < num_in; ++i, p->at.whole += p->step.whole) {
      sample_t const * in = input + p->at.integer;
      uint32_t frac = p->at.fraction;
      int phase = (int)(frac >> (32 - PHASE_BITS)); /* high-order bits */
#if COEF_INTERP > 0              /* low-order bits, scaled to [0,1) */
      sample_t x = (sample_t)((frac << PHASE_BITS) * (1 / MULT32));
#endif
      sample_t sum = 0;
      int j = 0;
      CONVOLVE
      output[i] = sum;
    }
    fifo_read(&p->fifo, p->at.integer, NULL);
    p->at.integer = 0;
  }
  assert(max_num_out - i >= 0);
  fifo_trim_by(output_fifo, max_num_out - i);
}

#undef _
#undef a
#undef b
#undef c
#undef d
#undef COEF_INTERP
#undef CONVOLVE
#undef FIR_LENGTH
#undef FUNCTION
#undef PHASE_BITS
