#include "fft.h"
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdlib.h>
#include "utils.h"

namespace _sbsms_ {

#define REAL(tr,ti,r,i) (tr*r - ti*i)
#define IMAG(tr,ti,r,i) (tr*i + ti*r)

t_fft *make_fft_buf(int N) 
{
  return (t_fft*) malloc(N*sizeof(t_fft));
}

void free_fft_buf(t_fft *buf)
{
  free(buf);
}

void optimizeFactors(int *f)
{
  int n = 0;
  for(int k=0;;k++) {
    if(f[k]==0) break;
    n++;
  }
  int *g = (int*)calloc(n+1,sizeof(int));
  for(int k=0;k<n;k++) {
    g[k] = f[k];
  }

  int m = 0;
  for(int k=0;k<n;k++) {
    int f2 = g[k] * g[k+1];
    if(f2 == 6) {
      f[m++] = 6;
      k++;
    } else if(f2 == 4) {
      f[m++] = 4;
      k++;
    } else {
      f[m++] = g[k];
    }
  }
  f[m] = 0;
  free(g);
}

int *getOrder(int n, int *N1, int *N2)
{
  int N = N1[0]*N2[0];
  int *order = (int*)calloc(N,sizeof(int));
  for(int k=0;k<N;k++) {
    int kr = 0;
    int k0 = k;
    for(int i=0;i<n;i++) {
      int k2 = k0%N2[i];
      k0 /= N2[i];
      kr += N1[i]*k2;
    }
    order[k] = kr;
  }
    
  return order;
}

t_fft **calcTwiddles(int n, int *N1, int *N2, int *order, int sign)
{
  t_fft **t = (t_fft**)calloc(n,sizeof(t_fft*));

  for(int i=0;i<n;i++) {
    int Np = N1[i]*N2[i];
    t[i] = (t_fft*)calloc(Np,sizeof(t_fft));
    for(int k=0;k<Np;k++) {
      t[i][k][0] = cos((real)(-sign)*TWOPI*(real)k/(real)Np);
      t[i][k][1] = sin((real)(-sign)*TWOPI*(real)k/(real)Np);
    }
  }
  return t;
}

fft_func *getFuncs(int *f)
{
  int n = 0;
  for(int k=0;;k++) {
    if(f[k] == 0) break;
    n++;
  }

  fft_func *func = (fft_func*)calloc(n,sizeof(fft_func));
  for(int k=0;k<n;k++) {
    switch(f[k]) {
    case 2 : func[k] = fft2; break;
    case 3 : func[k] = fft3; break;
    case 4 : func[k] = fft4; break;
    case 5 : func[k] = fft5; break;
    case 6 : func[k] = fft6; break;
    case 7 : func[k] = fft7; break;
    }
  }
  return func;
}

int *getN1(int *f, int N)
{
  int n = 0;
  for(int k=0;;k++) {
    if(f[k] == 0) break;
    n++;
  }
  
  int *N1 = (int*)calloc(n,sizeof(int));
  for(int k=0;k<n;k++) {
    N /= f[k];
    N1[k] = N;
  }
  return N1;
}

int getNFactors(int *f)
{
  int n = 0;
  for(int k=0;;k++) {
    if(f[k] == 0) break;
    n++;
  }
  return n;
}

fftplan *planFFT(int N) 
{
  int *factors = factor(N);
  optimizeFactors(factors);

  fftplan *plan = new fftplan;
  plan->dir = 1;
  plan->n = getNFactors(factors);
  plan->N2 = factors;
  plan->N1 = getN1(factors,N);
  plan->order = getOrder(plan->n,plan->N1,plan->N2);
  plan->reorder = (t_fft*)malloc(N*sizeof(t_fft));
  plan->t = calcTwiddles(plan->n,plan->N1,plan->N2,plan->order,1);
  plan->f = getFuncs(factors);
  plan->N = N;
  plan->norm = 1.0;
  return plan;
}

fftplan *planIFFT(int N) 
{
  int *factors = factor(N);
  optimizeFactors(factors);
  fftplan *plan = new fftplan;
  plan->dir = -1;
  plan->n = getNFactors(factors);
  plan->N2 = factors;
  plan->N1 = getN1(factors,N);
  plan->order = getOrder(plan->n,plan->N1,plan->N2);
  plan->reorder = (t_fft*)malloc(N*sizeof(t_fft));
  plan->t = calcTwiddles(plan->n,plan->N1,plan->N2,plan->order,-1);
  plan->f = getFuncs(factors);
  plan->N = N;
  plan->norm = 1.0f/(real)N;
  return plan;
}

void destroy_fftplan(fftplan *plan)
{
  free(plan->reorder);
  free(plan->order);
  free(plan->f);
  for(int i=0;i<plan->n;i++)
    free(plan->t[i]);
  free(plan->t);
  free(plan->N1);
  free(plan->N2);
  free(plan);
}

void FFT(fftplan *plan, t_fft *x) 
{
  fft(plan,x,0,0);
  
  for(int k=0;k<plan->N;k++) {
    plan->reorder[k][0] = x[k][0];
    plan->reorder[k][1] = x[k][1];
  } 
  for(int k=0;k<plan->N;k++) {
    int kr = plan->order[k];
    if(k != kr) {
      x[k][0] = plan->reorder[kr][0];
      x[k][1] = plan->reorder[kr][1];
    }
  }

}

void IFFT(fftplan *plan, t_fft *x) 
{
  FFT(plan,x);
  /*
  real norm = plan->norm;
  for(int k=0;k<plan->N;k++) {
    x[k][0] *= norm;
    x[k][1] *= norm;
  }
  */
}

void fft(fftplan *plan, t_fft *x, int r, int i)
{
  int N1 = plan->N1[i];
  int N2 = plan->N2[i];
  for(int n1=0;n1<N1;n1++) {
    (plan->f[i])(x,n1,N1,r,plan->t[i],plan->dir);
  }
  if(N1==1) return;
  for(int k2=0;k2<N2;k2++) {
    fft(plan,x,r,i+1);
    r += N1;
  }
}

void fft2(t_fft *x, int n1, int N1, int r, t_fft *t, int dir)
{
  int i = r+n1;
  real *x0 = x[i]; i+= N1;
  real *x1 = x[i];

  real y0 = x0[0] - x1[0];
  real y1 = x0[1] - x1[1];

  x0[0] += x1[0];
  x0[1] += x1[1];

  real *t1 = t[n1];
  real t10 = t1[0];
  real t11 = t1[1];
  x1[0] = REAL(t10,t11,y0,y1);
  x1[1] = IMAG(t10,t11,y0,y1);
}

#define T300 0.5f
#define T301 0.86602540378444f
void fft3(t_fft *x, int n1, int N1, int r, t_fft *t, int dir)
{
  int i = r+n1;
  real *x0 = x[i]; i+=N1;
  real *x1 = x[i]; i+=N1;
  real *x2 = x[i];

  real z00 = x1[0] + x2[0];
  real z01 = x1[1] + x2[1];
  
  real z10 = x0[0] - T300*z00;
  real z11 = x0[1] - T300*z01;

  real z20;
  real z21;
  if(dir==1) {
    z20 = T301*(x2[0] - x1[0]);
    z21 = T301*(x2[1] - x1[1]);
  } else {
    z20 = T301*(x1[0] - x2[0]);
    z21 = T301*(x1[1] - x2[1]);
  }

  x0[0] = z00 + x0[0];
  x0[1] = z01 + x0[1];

  real z30 = z10 - z21;
  real z31 = z11 + z20;
  real *t1 = t[n1];
  real t10 = t1[0];
  real t11 = t1[1];
  x1[0] = REAL(t10,t11,z30,z31);
  x1[1] = IMAG(t10,t11,z30,z31);

  real z40 = z10 + z21;
  real z41 = z11 - z20;
  real *t2 = t[n1<<1];
  real t20 = t2[0];
  real t21 = t2[1];
  x2[0] = REAL(t20,t21,z40,z41);
  x2[1] = IMAG(t20,t21,z40,z41);
}

void fft4(t_fft *x, int n1, int N1, int r, t_fft *t, int dir)
{
  int i = r+n1;
  real *x0 = x[i]; i+=N1;
  real *x1 = x[i]; i+=N1;
  real *x2 = x[i]; i+=N1;
  real *x3 = x[i];

  real z20 = x0[0] - x2[0];
  real z21 = x0[1] - x2[1];
  real z00 = x0[0] + x2[0];
  real z01 = x0[1] + x2[1];
  
  real z10 = x1[0] + x3[0];
  real z11 = x1[1] + x3[1];
  
  x0[0] = z00 + z10;
  x0[1] = z01 + z11;
  
  real y20 = z00 - z10;
  real y21 = z01 - z11;
  real *t2 = t[n1<<1];
  real t20 = t2[0];
  real t21 = t2[1];
  x2[0] = REAL(t20,t21,y20,y21);
  x2[1] = IMAG(t20,t21,y20,y21);

  real z30;
  real z31;
  if(dir==1) {
    z30 = (x3[0] - x1[0]);
    z31 = (x3[1] - x1[1]);  
  } else {
    z30 = (x1[0] - x3[0]);
    z31 = (x1[1] - x3[1]);  
  }

  real y10 = z20 - z31;
  real y11 = z21 + z30;
  real *t1 = t[n1];
  real t10 = t1[0];
  real t11 = t1[1];
  x1[0] = REAL(t10,t11,y10,y11);
  x1[1] = IMAG(t10,t11,y10,y11);

  real y30 = z20 + z31;
  real y31 = z21 - z30;
  real *t3 = t[n1*3];
  real t30 = t3[0];
  real t31 = t3[1];
  x3[0] = REAL(t30,t31,y30,y31);
  x3[1] = IMAG(t30,t31,y30,y31);
}

#define T500 0.95105651629515f
#define T501 0.58778525229247f
#define T510 0.55901699437495f
#define T511 0.25f

void fft5(t_fft *x, int n1, int N1, int r, t_fft *t, int dir)
{
  int i = r+n1;
  real *x0 = x[i]; i+=N1;
  real *x1 = x[i]; i+=N1;
  real *x2 = x[i]; i+=N1;
  real *x3 = x[i]; i+=N1;
  real *x4 = x[i];

  real z00 = x1[0] + x4[0];
  real z01 = x1[1] + x4[1];

  real z10 = x2[0] + x3[0];
  real z11 = x2[1] + x3[1];

  real z20 = x1[0] - x4[0];
  real z21 = x1[1] - x4[1];

  real z30 = x2[0] - x3[0];
  real z31 = x2[1] - x3[1];

  real z40 = z00 + z10;
  real z41 = z01 + z11;
 
  real z50 = T510*(z00 - z10);
  real z51 = T510*(z01 - z11);

  real z60 = x0[0] - T511*z40;
  real z61 = x0[1] - T511*z41;

  real z70 = z50 + z60;
  real z71 = z51 + z61;

  real z80 = z60 - z50;
  real z81 = z61 - z51;
  
  real z90;
  real z91;
  if(dir==1) {
    z90 = -(T500*z20 + T501*z30);
    z91 = -(T500*z21 + T501*z31);
  } else {
    z90 = (T500*z20 + T501*z30);
    z91 = (T500*z21 + T501*z31);
  }
  
  real z100;
  real z101;
  if(dir==1) {
    z100 = (T500*z30 - T501*z20);
    z101 = (T500*z31 - T501*z21);
  } else {
    z100 = (T501*z20 - T500*z30);
    z101 = (T501*z21 - T500*z31);
  }

  x0[0] = x0[0] + z40;
  x0[1] = x0[1] + z41;

  real y10 = z70 - z91;
  real y11 = z71 + z90;
  real *t1 = t[n1];
  real t10 = t1[0];
  real t11 = t1[1];
  x1[0] = REAL(t10,t11,y10,y11);
  x1[1] = IMAG(t10,t11,y10,y11);

  real y20 = z80 - z101;
  real y21 = z81 + z100;
  real *t2 = t[n1<<1];
  real t20 = t2[0];
  real t21 = t2[1];
  x2[0] = REAL(t20,t21,y20,y21);
  x2[1] = IMAG(t20,t21,y20,y21);

  real y30 = z80 + z101;
  real y31 = z81 - z100;
  real *t3 = t[n1*3];
  real t30 = t3[0];
  real t31 = t3[1];
  x3[0] = REAL(t30,t31,y30,y31);
  x3[1] = IMAG(t30,t31,y30,y31);

  real y40 = z70 + z91;
  real y41 = z71 - z90;
  real *t4 = t[n1<<2];
  real t40 = t4[0];
  real t41 = t4[1];
  x4[0] = REAL(t40,t41,y40,y41);
  x4[1] = IMAG(t40,t41,y40,y41);
}

#define T600 0.86602540378444f
#define T601 0.5f
void fft6(t_fft *x, int n1, int N1, int r, t_fft *t, int dir)
{
  int i = r+n1;
  real *x0 = x[i]; i+=N1;
  real *x1 = x[i]; i+=N1;
  real *x2 = x[i]; i+=N1;
  real *x3 = x[i]; i+=N1;
  real *x4 = x[i]; i+=N1;
  real *x5 = x[i];

  real za00 = x2[0] + x4[0];
  real za01 = x2[1] + x4[1];
  
  real za10 = x0[0] - T601*za00;
  real za11 = x0[1] - T601*za01;

  real za20;
  real za21;
  if(dir==1) {
    za20 = T600*(x4[0] - x2[0]);
    za21 = T600*(x4[1] - x2[1]);
  } else {
    za20 = T600*(x2[0] - x4[0]);
    za21 = T600*(x2[1] - x4[1]);
  }

  real a00 = x0[0] + za00;
  real a01 = x0[1] + za01;

  real a10 = za10 - za21;
  real a11 = za11 + za20;

  real a20 = za10 + za21;
  real a21 = za11 - za20;

  real zb00 = x1[0] + x5[0];
  real zb01 = x1[1] + x5[1];
  
  real zb10 = x3[0] - T601*zb00;
  real zb11 = x3[1] - T601*zb01;

  real zb20;
  real zb21;
  if(dir==1) {
    zb20 = T600*(x1[0] - x5[0]);
    zb21 = T600*(x1[1] - x5[1]);
  } else {
    zb20 = T600*(x5[0] - x1[0]);
    zb21 = T600*(x5[1] - x1[1]);
  }

  real b00 = x3[0] + zb00;
  real b01 = x3[1] + zb01;

  real b10 = zb10 - zb21;
  real b11 = zb11 + zb20;

  real b20 = zb10 + zb21;
  real b21 = zb11 - zb20;

  x0[0] = a00 + b00;
  x0[1] = a01 + b01;

  real y10 = a10 - b10;
  real y11 = a11 - b11;
  real *t1 = t[n1];
  real t10 = t1[0];
  real t11 = t1[1];
  x1[0] = REAL(t10,t11,y10,y11);
  x1[1] = IMAG(t10,t11,y10,y11);

  real y20 = a20 + b20;
  real y21 = a21 + b21;
  real *t2 = t[n1<<1];
  real t20 = t2[0];
  real t21 = t2[1];
  x2[0] = REAL(t20,t21,y20,y21);
  x2[1] = IMAG(t20,t21,y20,y21);

  real y30 = a00 - b00;
  real y31 = a01 - b01;
  real *t3 = t[n1*3];
  real t30 = t3[0];
  real t31 = t3[1];
  x3[0] = REAL(t30,t31,y30,y31);
  x3[1] = IMAG(t30,t31,y30,y31);

  real y40 = a10 + b10;
  real y41 = a11 + b11;
  real *t4 = t[n1<<2];
  real t40 = t4[0];
  real t41 = t4[1];
  x4[0] = REAL(t40,t41,y40,y41);
  x4[1] = IMAG(t40,t41,y40,y41);

  real y50 = a20 - b20;
  real y51 = a21 - b21;
  real *t5 = t[n1*5];
  real t50 = t5[0];
  real t51 = t5[1];
  x5[0] = REAL(t50,t51,y50,y51);
  x5[1] = IMAG(t50,t51,y50,y51);
}

#define C71 -1.16666666666667f
#define C72 0.79015646852540f
#define C73 0.05585426728965f
#define C74 0.73430220123575f
#define C75 0.44095855184410f
#define C76 0.34087293062393f
#define C77 -0.53396936033773f
#define C78 0.87484229096166f

void fft7(t_fft *x, int n1, int N1, int r, t_fft *t, int dir)
{
  int i = r+n1;
  real *x0 = x[i]; i+=N1;
  real *x1 = x[i]; i+=N1;
  real *x2 = x[i]; i+=N1;
  real *x3 = x[i]; i+=N1;
  real *x4 = x[i]; i+=N1;
  real *x5 = x[i]; i+=N1;
  real *x6 = x[i];

  real u00 = x1[0] + x6[0];
  real u01 = x1[1] + x6[1];

  real u10 = x1[0] - x6[0];
  real u11 = x1[1] - x6[1];

  real u20 = x2[0] + x5[0];
  real u21 = x2[1] + x5[1];

  real u30 = x2[0] - x5[0];
  real u31 = x2[1] - x5[1];

  real u40 = x4[0] + x3[0];
  real u41 = x4[1] + x3[1];

  real u50 = x4[0] - x3[0];
  real u51 = x4[1] - x3[1];

  real u60 = u20 + u00;
  real u61 = u21 + u01;

  real u70 = u50 + u30;
  real u71 = u51 + u31;

  real b00 = x0[0] + u60 + u40;
  real b01 = x0[1] + u61 + u41;

  real b10 = C71*(u60 + u40);
  real b11 = C71*(u61 + u41);
  
  real b20 = C72*(u00 - u40);
  real b21 = C72*(u01 - u41);

  real b30 = C73*(u40 - u20);
  real b31 = C73*(u41 - u21);

  real b40 = C74*(u20 - u00);
  real b41 = C74*(u21 - u01);
  
  real b50;
  real b51;
  if(dir==1) {
    b50 = C75*(u70 + u10);
    b51 = C75*(u71 + u11);
  } else {
    b50 = -C75*(u70 + u10);
    b51 = -C75*(u71 + u11);
  }

  real b60;
  real b61;
  if(dir==1) {
    b60 = C76*(u10 - u50);
    b61 = C76*(u11 - u51);
  } else {
    b60 = C76*(u50 - u10);
    b61 = C76*(u51 - u11);
  }
  
  real b70;
  real b71;
  if(dir==1) {
    b70 = C77*(u50 - u30);
    b71 = C77*(u51 - u31);
  } else {
    b70 = C77*(u30 - u50);
    b71 = C77*(u31 - u51);
  }

  real b80;
  real b81;
  if(dir==1) {
    b80 = C78*(u30 - u10);
    b81 = C78*(u31 - u11);
  } else {
    b80 = C78*(u10 - u30);
    b81 = C78*(u11 - u31);
  }

  real T00 = b00 + b10;
  real T01 = b01 + b11;

  real T10 = b20 + b30;
  real T11 = b21 + b31;

  real T20 = b40 - b30;
  real T21 = b41 - b31;

  real T30 = -b20 - b40;
  real T31 = -b21 - b41;

  real T40 = b60 + b70;
  real T41 = b61 + b71;

  real T50 = b80 - b70;
  real T51 = b81 - b71;

  real T60 = -b80 - b60;
  real T61 = -b81 - b61;

  real T70 = T00 + T10;
  real T71 = T01 + T11;

  real T80 = T00 + T20;
  real T81 = T01 + T21;

  real T90 = T00 + T30;
  real T91 = T01 + T31;

  real T100 = T40 + b50;
  real T101 = T41 + b51;

  real T110 = T50 + b50;
  real T111 = T51 + b51;

  real T120 = T60 + b50;
  real T121 = T61 + b51;

  x0[0] = b00;
  x0[1] = b01;

  real y10 = T70 + T101;
  real y11 = T71 - T100;
  real *t1 = t[n1];
  real t10 = t1[0];
  real t11 = t1[1];
  x1[0] = REAL(t10,t11,y10,y11);
  x1[1] = IMAG(t10,t11,y10,y11);

  real y20 = T90 + T121;
  real y21 = T91 - T120;
  real *t2 = t[n1<<1];
  real t20 = t2[0];
  real t21 = t2[1];
  x2[0] = REAL(t20,t21,y20,y21);
  x2[1] = IMAG(t20,t21,y20,y21);

  real y30 = T80 - T111;
  real y31 = T81 + T110;
  real *t3 = t[n1*3];
  real t30 = t3[0];
  real t31 = t3[1];
  x3[0] = REAL(t30,t31,y30,y31);
  x3[1] = IMAG(t30,t31,y30,y31);

  real y40 = T80 + T111;
  real y41 = T81 - T110;
  real *t4 = t[n1<<2];
  real t40 = t4[0];
  real t41 = t4[1];
  x4[0] = REAL(t40,t41,y40,y41);
  x4[1] = IMAG(t40,t41,y40,y41);

  real y50 = T90 - T121;
  real y51 = T91 + T120;
  real *t5 = t[n1*5];
  real t50 = t5[0];
  real t51 = t5[1];
  x5[0] = REAL(t50,t51,y50,y51);
  x5[1] = IMAG(t50,t51,y50,y51);

  real y60 = T70 - T101;
  real y61 = T71 + T100;
  real *t6 = t[n1*6];
  real t60 = t6[0];
  real t61 = t6[1];
  x6[0] = REAL(t60,t61,y60,y61);
  x6[1] = IMAG(t60,t61,y60,y61);
}

}
