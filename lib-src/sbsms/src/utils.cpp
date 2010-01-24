#include <math.h>
#include "utils.h"
#include "real.h"
#include "defs.h"

namespace _sbsms_ {
real *COSTABLE_ = NULL;
real *COSTABLE = NULL;
real COSFACTOR;

int *factor(int n)
{
  int *f = (int*)calloc(ilog2(n)+1,sizeof(int));
  factor(n,f,0);
  return f;
}

void factor(int n, int *f, int m)
{
  for(int k=2;k<=n;k++) {
    if(n%k==0) {
      f[m++] = k;
      n /= k;
      break;
    }
  }
  if(n>1)
	  factor(n,f,m);
}

void _evenodd2c(audio *eo, audio *even, audio *odd, int N)
{
  for(int k=0;k<N;k++) {
    eo[k][0] = (even[k][0] - odd[k][1]);
    eo[k][1] = (even[k][1] + odd[k][0]);
  }
}

void _c2evenodd(audio *eo, audio *even, audio *odd, int N)
{
  int Nover2 = N/2;
  for(int k=0;k<=Nover2;k++) {
    int fk = (k==0)?0:N-k;

    real eok0 = eo[k][0];
    real eok1 = eo[k][1];
    real eofk0 = eo[fk][0];
    real eofk1 = eo[fk][1];
    
    even[k][0] = 0.5f*(eok0 + eofk0);
    even[k][1] = 0.5f*(eok1 - eofk1);
    
    odd[k][0] = 0.5f*(eok1 + eofk1);
    odd[k][1] = 0.5f*(-eok0 + eofk0);
  }
}

void cosinit(int size) {
  COSFACTOR = (real)(size-1)/PI;
  if(COSTABLE_) free(COSTABLE_);
  COSTABLE_ = (real*)malloc(size*sizeof(real));
  for(int k=0;k<size;k++)
    COSTABLE_[k] = cos((real)k/(real)(size-1)*PI);
  COSTABLE = COSTABLE_;
}

void _c2magphase(audio *g, int N)
{
  for(int k=0;k<N;k++) {
    real mag = norm(g[k]);
    real phase = atan2(g[k][1],g[k][0]);
    g[k][0] = mag;
    g[k][1] = phase;
  }
}

void _magphase2c(audio *g, int N)
{
 for(int k=0;k<N;k++) {
   real mag = g[k][0];
   real phi = g[k][1];
   real re = mag * COS(phi);
   real im = mag * SIN(phi);

   g[k][0] = re;
   g[k][1] = im;
  }
}

}
