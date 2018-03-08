/* SoX Resampler Library         Copyright (c) 2013 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Generate the filter coefficients for variable-rate resampling. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define PI 3.14159265358979323846            /* Since M_PI can't be relied on */

static void print(double * h, int m, double l, char const * name)
{                                                      /* Print out a filter: */
  int i, N = l? (int)(l*m)-(l>1) : m, R=(N+1)/2;
  int a = !l||l>1? 0:N-R, b = l>1? R:N;
  printf("static float const %s[] = {\n", name);
  if (l>1) printf(" 0.f,"); else if (!l) l=1;
  for (i=a; h && i<b; ++i, printf("% .9gf,%c",l*h[i-1],"\n "[(i-a)&3 && i<b]));
  puts("};\n");
  free(h);
}
                                                  /* Parks McClellan FIR LPF: */
#define even_adj(f) ((N&1)? 1 : cos(PI*.5*(f)))
#define W(f) (((f) < Fp+1e-9? weight : 1) * even_adj(f))      /* Weighting fn */
#define D(f) (((f) < Fp+1e-9) / even_adj(f))           /* Desired response fn */
#define F(i) ((i) <= end[0]? (i)*inc[0] : 1-(end[1]-(i))*inc[1])
#define EE(x,z) (_1 != x 1 && x E[i] > 0 && x E[i] >= x E[i z 1])
#define PEAK do {if (k<NP+1) peak[k]=i; ++k,_1=(E[i]>0)-(E[i]<0);} while (0)

typedef struct {double x, beta, gamma;} coef_t;

static double amp_response(coef_t * coef, int R, double f, int i)
{
  double n = 0, d = 0, x = cos(PI*f), t;
  for (; i < R; d += t = coef[i].beta / t, n += coef[i].gamma * t, ++i)
    if (fabs(t = x - coef[i].x) < 1e-9) return coef[i].gamma;
  return n/d;
}

static void fir(int m, double l, double Fp0, double Fs0,
    double weight0, int density, char const * name)
{
  double Fp=Fp0/l, Fs=Fs0/l, weight=1/weight0, inc[2], Ws=1-Fs;
  int N = (int)(l*m)-(l>1), R=(N+1)/2, NP=R+1, grid_size=1+density*R+1, pass=0;
  int n1 = Ws>=(2*R-1)*Fp? 1:(int)(R*Fp/(Fp+Ws)+.5), n2=NP-n1, _1, i, j, k;
  int    * peak = calloc(sizeof(*peak), (size_t)(NP+1)), * P=peak, end[2];
  coef_t * coef = calloc(sizeof(*coef), (size_t)(NP));
  float  * E    = calloc(sizeof(*E   ), (size_t)(grid_size));
  double d, n, e, f, mult, delta, sum, hi, lo, * A = (double*)E, *h=0;

  if (!P || !coef || !E) goto END;
  end[0] = n1 * density, end[1] = grid_size-1;     /* Create prototype peaks: */
  inc[0] = Fp/end[0],    inc[1] = n2==1? 0 : Ws / ((n2-1)*density);
  for (i=0; i<n1; P[n1-1-i] = end[0] - i*density,++i);
  for (i=0; i<n2; P[n1+i] = 1+end[0] + i*density,++i);

  do {                                               /* Coefs for amp. resp.: */
    for (i = 0; i<NP; coef[i].x = cos(PI*F(P[i])), ++i);
    for (_1=-1, n=d=i=0; i < NP; ++i) {
      for (mult = 1, j = 0; j < R; ++j) if (j != i) mult *= coef[i].x-coef[j].x;
      if (mult) coef[i].beta = 1/mult; else goto END;
      if (i != R) mult *= coef[i].x - coef[R].x;
      f = F(P[i]), n += D(f)/mult, d += (_1=-_1)/(W(f)*mult);
    }
    for (delta = n/d, _1 = -1, i = 0; i < R; ++i)
      f = F(P[i]), coef[i].gamma = D(f)-(_1=-_1)*delta/W(f);
    for (i = 0; i <= end[1]; ++i)            /* Amplitude response and error: */
      f = F(i), E[i] = (float)(W(f)*(D(f) - amp_response(coef, R, f, 0)));

    i = k = _1 = 0;                                        /* Find new peaks: */
    if (end[0]) if (EE(+,+) || EE(-,+)) PEAK;                       /* At F=0 */
    for (++i, j = 0; j < 2; ++j) {                              /* In band j: */
      for (; i < end[j]; ++i)
        if ((EE(+,-) && E[i]>E[i+1]) || (EE(-,-) && E[i]<E[i+1])) PEAK;
      if (!j) {PEAK; ++i; PEAK; ++i;}                           /* At Fp & Fs */
    }
    if (i==end[1]) if (EE(+,-) || EE(-,-)) PEAK;                    /* At F=1 */
    if ((unsigned)(k = k-NP) > 1) goto END;                  /* Too many/few? */
    P = peak + k * (fabs(E[peak[0]]) < fabs(E[peak[NP]]));         /* rm 1st? */

    for (lo = hi = fabs(E[P[0]]), i=1; i<NP; ++i)              /* Converged?: */
      e = fabs(E[P[i]]), lo = e<lo? e:lo, hi = e>hi? e:hi;
  } while ((hi-lo)/hi > .001 && ++pass < 20);
                      /* Create impulse response from final amp. resp. coefs: */
  if (!(h = malloc(sizeof(*h)*(size_t)N))) goto END;
  for (i = 0; i < R; f = 2.*i/N, A[i++] = amp_response(coef,R,f,0)*even_adj(f));
  for (i = 0; i < R; h[N-1-i] = h[i] = sum/N, ++i)
    for (sum=*A, j=1; j<R; sum += 2*cos(2*PI*(i-(N-1)/2.)/N*j)*A[j], ++j);
  END: free(coef), free(E), free(peak);
  print(h, m, l, name);
}
                                  /* Half-band IIR LPF (Mitra DSP 3/e, 13_9): */
static void iir(int N, double Fp, char const * name)
{
  double d=tan(PI*.5*Fp), r=d*d, t=sqrt(1-r*r), n=(1-sqrt(t))/(1+sqrt(t))*.5;
  double x=(n*n)*(n*n), Q=(((150*x+15)*x+2)*x+1)*n, q=pow(Q,.25), *h;
  int i=0, j, _1;
  if (!(h = malloc(sizeof(*h)*(size_t)N))) goto END;
  for (; i<N; t=n*q/d, t=t*t, t=sqrt((1-t*r)*(1-t/r))/(1+t), h[i++]=(1-t)/(1+t))
    for (_1=1, d=-.5, n=j=0, x=(i+1)*PI/(N+.5); j<7; ++j, _1=-_1)
      n += _1*pow(Q,j*(j+1))*sin(x*(j+.5)), d += _1*pow(Q,j*j)*cos(x*j);
  END: print(h, N, 0, name);
}

int main(int argc, char **argv)
{
  puts("/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net");
  puts(" * Licence for this file: LGPL v2.1                  See LICENCE for details. */\n");

  fir(241,  1, .45,  .5, 160, 32, "half_fir_coefs");
  fir( 24, .5, .25,  .5,   1, 31, "fast_half_fir_coefs");
  fir( 20, 12, .9 , 1.5, 160, 58, "coefs0_d");
  fir( 12,  6, .45, 1.5,  80, 29, "coefs0_u");
  iir( 15, .492, "iir_coefs");
  return 0*argc*!argv;
}
