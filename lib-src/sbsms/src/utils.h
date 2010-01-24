#ifndef FFTUTILS_H
#define FFTUTILS_H

#include "real.h"
#include "sbsms.h"
#include "defs.h"
#include <math.h>
#include <limits.h>
#include <stdlib.h>

namespace _sbsms_ {

extern real COSFACTOR;
extern real *COSTABLE_;
extern real *COSTABLE;

void cosinit(int size);
void _evenodd2c(audio *eo, audio *even, audio *odd, int N);
void _c2evenodd(audio *eo, audio *even, audio *odd, int N);
inline real SIN(real x);
inline real COS(real x);
inline real square(real x);
void _c2magphase(audio *g, int N);
void _magphase2c(audio *g, int N);
inline real canon(real ph);
inline real norm(audio x);
inline real norm2(audio x);
inline real sign(real x);
inline real min(real a, real b);
inline long min(long a, long b);
inline real dBApprox(real x);
int *factor(int n);
void factor(int n, int *f, int m);

inline int ilog2(int x)
{
	int n = 0;
	while(x>1) {
		x>>=1;
		n++;
	}
	return n;
}

inline real dBApprox(real x) 
{
  real u = (x-1.0)/(x+1.0);
  real u2 = u*u;
  return 17.37177927613007*u*(1.0 + u2*(0.333333333333333 + u2*(0.2 + u2*0.14285714285714)));
}

inline real min(real a, real b)
{
  return ((a)<(b)?(a):(b));
}

inline real max(real a, real b)
{
  return ((a)>(b)?(a):(b));
}

inline long min(long a, long b)
{
  return ((a)<(b)?(a):(b));
}

inline long max(long a, long b)
{
  return ((a)>(b)?(a):(b));
}

inline int min(int a, int b)
{
  return ((a)<(b)?(a):(b));
}

inline int max(int a, int b)
{
  return ((a)>(b)?(a):(b));
}

inline real sign(real x)
{
  return x<0?-1:1;
}

inline real canon(real ph) 
{
  return ph - TWOPI*(real)round2int(ph*ONEOVERTWOPI);
}

inline real norm2(audio x)
{
  return square(x[0]) + square(x[1]);
}

inline real norm(audio x)
{
  return sqrt(norm2(x));
}

inline real SIN(real x)
{
  return sin(x);
}

inline real COS(real x)
{
  return COSTABLE[round2int(COSFACTOR*fabsf(x))];
}

inline real square(real x)
{ 
  return x*x;
}

}

#endif
