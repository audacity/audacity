#include "f2c.h"

#ifdef KR_headers
double floor();
integer i_nint(x) real *x;
#else
#undef abs
#include "math.h"
#ifdef __cplusplus
extern "C" {
#endif
integer i_nint(real *x)
#endif
{
return (integer)(*x >= 0 ? floor(*x + .5) : -floor(.5 - *x));
}
#ifdef __cplusplus
}
#endif
