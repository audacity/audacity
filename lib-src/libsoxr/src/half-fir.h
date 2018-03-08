/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Decimate by 2 using a FIR with odd length (LEN). */
/* Input must be preceded and followed by LEN >> 1 samples. */

#define COEFS ((sample_t const *)p->coefs)

#if SIMD_SSE
  #define BEGINNING v4_t sum, q1, q2, t
  #define ____ \
    q1 = _mm_shuffle_ps(t=vLdu(input+2*j),vLdu(input+2*j+4),_MM_SHUFFLE(3,1,3,1)); \
    q2 = _mm_shuffle_ps(vLdu(input-2*j-4),vLdu(input-2*j-8),_MM_SHUFFLE(1,3,1,3)); \
    sum = vAdd(j? sum : vMul(vSet1(.5), t), vMul(vAdd(q1, q2), vLd(COEFS+j))); \
    j += 4;
  #define __ \
    q1 = _mm_shuffle_ps(vLdu(input+2*j), vLdu(input-2*j-4), _MM_SHUFFLE(1,3,3,1)); \
    q2 = _mm_loadl_pi(q2, (__m64*)(COEFS+j)), q2 = _mm_movelh_ps(q2, q2); \
    sum = vAdd(sum, vMul(q1, q2)); \
    j += 2;
  #define _ \
    q1 = _mm_add_ss(_mm_load_ss(input+2*j+1), _mm_load_ss(input-2*j-1)); \
    sum = _mm_add_ss(sum, _mm_mul_ss(q1, _mm_load_ss(COEFS+j))); \
    ++j;
  #define END vStorSum(output+i, sum)
/* #elif SIMD_AVX; No good solution found. */
/* #elif SIMD_NEON; No need: gcc -O3 does a good job by itself. */
#else
  #define BEGINNING sample_t sum = input[0] * .5f
  #define ____ __ __
  #define __ _ _
  #define _ sum += (input[-(2*j +1)] + input[(2*j +1)]) * COEFS[j], ++j;
  #define END output[i] = sum
#endif



static void FUNCTION_H(stage_t * p, fifo_t * output_fifo)
{
  sample_t const * __restrict input = stage_read_p(p);
  int num_in = min(stage_occupancy(p), p->input_size);
  int i, num_out = (num_in + 1) >> 1;
  sample_t * __restrict output = fifo_reserve(output_fifo, num_out);

  for (i = 0; i < num_out; ++i, input += 2) {
    int j = 0;
    BEGINNING; CONVOLVE; END;
  }
  fifo_read(&p->fifo, 2 * num_out, NULL);
}



#undef _
#undef __
#undef ____
#undef BEGINNING
#undef END
#undef COEFS
#undef CONVOLVE
#undef FUNCTION_H
