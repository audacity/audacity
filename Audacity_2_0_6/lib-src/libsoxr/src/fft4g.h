/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

void lsx_cdft(int, int, double *, int *, double *);
void lsx_rdft(int, int, double *, int *, double *);
void lsx_ddct(int, int, double *, int *, double *);
void lsx_ddst(int, int, double *, int *, double *);
void lsx_dfct(int, double *, double *, int *, double *);
void lsx_dfst(int, double *, double *, int *, double *);

void lsx_cdft_f(int, int, float *, int *, float *);
void lsx_rdft_f(int, int, float *, int *, float *);
void lsx_ddct_f(int, int, float *, int *, float *);
void lsx_ddst_f(int, int, float *, int *, float *);
void lsx_dfct_f(int, float *, float *, int *, float *);
void lsx_dfst_f(int, float *, float *, int *, float *);

#define dft_br_len(l) (2ul + (1ul << (int)(log(l / 2 + .5) / log(2.)) / 2))
#define dft_sc_len(l) ((unsigned long)l / 2)

/* Over-allocate h by 2 to use these macros */
#define LSX_PACK(h, n)   h[1] = h[n]
#define LSX_UNPACK(h, n) h[n] = h[1], h[n + 1] = h[1] = 0;
