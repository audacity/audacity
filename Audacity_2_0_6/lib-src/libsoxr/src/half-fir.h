/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Down-sample by a factor of 2 using a FIR with odd length (LEN).*/
/* Input must be preceded and followed by LEN >> 1 samples. */

#define _ sum += (input[-(2*j +1)] + input[(2*j +1)]) * COEFS[j], ++j;
static void FUNCTION(stage_t * p, fifo_t * output_fifo)
{
  sample_t const * input = stage_read_p(p);
  int i, num_out = (stage_occupancy(p) + 1) / 2;
  sample_t * output = fifo_reserve(output_fifo, num_out);

  for (i = 0; i < num_out; ++i, input += 2) {
    int j = 0;
    sample_t sum = input[0] * .5f;
    CONVOLVE
    output[i] = sum;
  }
  fifo_read(&p->fifo, 2 * num_out, NULL);
}
#undef _
#undef COEFS
#undef CONVOLVE
#undef FUNCTION
