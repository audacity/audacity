#include "cblas.h"
#include "cblas_f77.h"

extern double ddot_(const int *n, const double *dx, const int *incx, const double *dy, const int *incy);

double cblas_ddot( const int N, const double *X,
                   const int incX, const double *Y, const int incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else 
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   return ddot_( &F77_N, X, &F77_incX, Y, &F77_incY);
}   
