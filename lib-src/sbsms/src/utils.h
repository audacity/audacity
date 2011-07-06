#ifndef FFTUTILS_H
#define FFTUTILS_H

#include "real.h"
#include "sbsms.h"
#include <math.h>
#include <limits.h>
#include <stdlib.h>

namespace _sbsms_ {

#define ONEOVERTWOPI 0.15915494309189533576888376337251f
#define PI 3.1415926535897932384626433832795f
#define TWOPI 6.28318530717958647692528676655900576f

extern int COSSIZE;
extern real COSFACTOR;
extern real *COSTABLE;

void cosinit(int size);
void _evenodd2c(audio *eo, audio *even, audio *odd, int N);
void _c2evenodd(audio *eo, audio *even, audio *odd, int N);
inline real COS(real x);
inline real square(real x);
void _c2magphase(audio *g, int N);
void _magphase2c(audio *g, int N);
inline real canon(real ph);
inline real norm(audio x);
inline real norm2(audio x);
inline real sign(real x);
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
  real u = (x-1.0f)/(x+1.0f);
  real u2 = u*u;
  return 17.37177927613007f*u*(1.0f + u2*(0.333333333333333f + u2*(0.2f + u2*0.14285714285714f)));
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
