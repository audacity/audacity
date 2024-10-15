/* stdefs.h */

#ifndef TRUE
#define TRUE  1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef PI
#define PI  (3.14159265358979323846)
#endif

#ifndef PI2
#define PI2 (6.28318530717958647693)
#endif

#define D2R (0.01745329348)          /* (2*pi)/360 */
#define R2D (57.29577951)            /* 360/(2*pi) */

#ifndef MAX
#define MAX(x,y) ((x)>(y) ?(x):(y))
#endif
#ifndef MIN
#define MIN(x,y) ((x)<(y) ?(x):(y))
#endif

#ifndef ABS
#define ABS(x)   ((x)<0   ?(-(x)):(x))
#endif

#ifndef SGN
#define SGN(x)   ((x)<0   ?(-1):((x)==0?(0):(1)))
#endif

typedef float mem_float;
typedef double fast_float;

/* I took out this typedef because the same thing
 * exists in cext.h which causes a conflict:  -RBD
typedef unsigned char boolean; */

#include "cext.h"

#define true 1
#define false 0


