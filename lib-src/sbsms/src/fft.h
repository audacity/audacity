#ifndef FFT_H
#define FFT_H

#include "sbsms.h"

namespace _sbsms_ {

typedef real t_fft[2];

typedef void (*fft_func)(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);

struct fftplan {
  fft_func *f;
  int *order;
  t_fft *reorder;
  int *N1;
  int *N2;
  int N;
  int n;
  int dir;
  real norm;
  t_fft **t;
};

t_fft *make_fft_buf(int N);
void free_fft_buf(t_fft *buf);
fftplan *planFFT(int N);
fftplan *planIFFT(int N);
void destroy_fftplan(fftplan *plan);
void FFT(fftplan *plan, t_fft *x);
void IFFT(fftplan *plan, t_fft *x);

void fft(fftplan *plan, t_fft *x, int r, int i);
void optimizeFactors(int *f);
int *getOrder(int n, int *N1, int *N2);
t_fft **calcTwiddles(int n, int *N1, int *N2, int dir);
fft_func *getFuncs(int *f);
int *getN1(int *f, int N);
int getNFactors(int *f);

void fft2(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);
void fft3(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);
void fft4(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);
void fft5(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);
void fft6(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);
void fft7(t_fft *x, int n1, int N1, int r, t_fft *t, int dir);

}

#endif

