/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Resample using a non-interpolated poly-phase FIR with length LEN. */
/* Input must be followed by FIR_LENGTH-1 samples. */

#if SIMD_AVX || SIMD_SSE || SIMD_NEON
  #define N (FIR_LENGTH>>2)
  #define BEGINNING v4_t sum = vZero(); \
      v4_t const * const __restrict coefs = (v4_t *)COEFS + N * rem;
  #define _ sum = vMac(vLdu(at+j*4), coefs[j], sum), ++j;
  #define END vStorSum(output+i, sum)
  #define cc(n) case n: core(n); break
  #define CORE(n) switch (n) {cc(2); cc(3); cc(4); cc(5); cc(6); default: core(n);}
#else
  #define N FIR_LENGTH
  #define BEGINNING sample_t sum = 0; \
      sample_t const * const __restrict coefs = (sample_t *)COEFS + N * rem;
  #define _ sum += coefs[j]*at[j], ++j;
  #define END output[i] = sum
  #define CORE(n) core(n)
#endif

#define core(n) \
  for (i = 0; at < num_in * p->L; ++i, at += step) { \
    int const div = at / p->L, rem = at % p->L; \
    sample_t const * const __restrict at = input + div; \
    int j = 0; BEGINNING; CONVOLVE(n); END;}

static void FUNCTION(stage_t * p, fifo_t * output_fifo)
{
  int num_in = min(stage_occupancy(p), p->input_size);
  if (num_in) {
    sample_t const * input = stage_read_p(p);
    int at = p->at.integer, step = p->step.integer;
    int i, num_out = (num_in * p->L - at + step - 1) / step;
    sample_t * __restrict output = fifo_reserve(output_fifo, num_out);

    CORE(N);
    assert(i == num_out);
    fifo_read(&p->fifo, at / p->L, NULL);
    p->at.integer = at % p->L;
  }
}

#undef _
#undef CORE
#undef cc
#undef core
#undef N
#undef BEGINNING
#undef MIDDLE
#undef END
#undef CONVOLVE
#undef FIR_LENGTH
#undef FUNCTION
