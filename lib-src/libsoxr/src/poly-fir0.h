/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Resample using a non-interpolated poly-phase FIR with length LEN.*/
/* Input must be followed by LEN-1 samples. */

#define _ sum += (coef(p->shared->poly_fir_coefs, 0, FIR_LENGTH, divided.rem, 0, j)) *at[j], ++j;

static void FUNCTION(stage_t * p, fifo_t * output_fifo)
{
  sample_t const * input = stage_read_p(p);
  int i, num_in = stage_occupancy(p), max_num_out = 1 + (int)(num_in*p->out_in_ratio);
  sample_t * output = fifo_reserve(output_fifo, max_num_out);
  div_t divided2;

  for (i = 0; p->at.integer < num_in * p->L; ++i, p->at.integer += p->step.integer) {
    div_t divided = div(p->at.integer, p->L);
    sample_t const * at = input + divided.quot;
    sample_t sum = 0;
    int j = 0;
    CONVOLVE
    output[i] = sum;
  }
  assert(max_num_out - i >= 0);
  fifo_trim_by(output_fifo, max_num_out - i);
  divided2 = div(p->at.integer, p->L);
  fifo_read(&p->fifo, divided2.quot, NULL);
  p->at.integer = divided2.rem;
}

#undef _
#undef CONVOLVE
#undef FIR_LENGTH
#undef FUNCTION
