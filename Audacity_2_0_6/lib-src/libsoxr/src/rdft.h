/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

void ORDERED_CONVOLVE(int n, void * not_used, DFT_FLOAT * a, const DFT_FLOAT * b)
{
  int i;
  a[0] *= b[0];
  a[1] *= b[1];
  for (i = 2; i < n; i += 2) {
    DFT_FLOAT tmp = a[i];
    a[i  ] = b[i  ] * tmp - b[i+1] * a[i+1];
    a[i+1] = b[i+1] * tmp + b[i  ] * a[i+1];
  }
  (void)not_used;
}

void ORDERED_PARTIAL_CONVOLVE(int n, DFT_FLOAT * a, const DFT_FLOAT * b)
{
  int i;
  a[0] *= b[0];
  for (i = 2; i < n; i += 2) {
    DFT_FLOAT tmp = a[i];
    a[i  ] = b[i  ] * tmp - b[i+1] * a[i+1];
    a[i+1] = b[i+1] * tmp + b[i  ] * a[i+1];
  }
  a[1] = b[i] * a[i] - b[i+1] * a[i+1];
}

#undef ORDERED_CONVOLVE
#undef ORDERED_PARTIAL_CONVOLVE
#undef DFT_FLOAT
