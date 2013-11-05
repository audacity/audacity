/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Common includes etc. for the examples.  */

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
  /* Work-around for broken file-I/O on MS-Windows: */
  #include <io.h>
  #include <fcntl.h>
  #define USE_STD_STDIO _setmode(_fileno(stdout), _O_BINARY), \
                        _setmode(_fileno(stdin ), _O_BINARY);
  /* Sometimes missing, so ensure that it is defined: */
  #undef M_PI
  #define M_PI 3.14159265358979323846
#else
  #define USE_STD_STDIO
#endif

#undef int16_t
#define int16_t short

#undef int32_t
#if LONG_MAX > 2147483647L
  #define int32_t int
#elif LONG_MAX < 2147483647L
  #error this programme requires that 'long int' has at least 32-bits
#else
  #define int32_t long
#endif

#undef min
#undef max
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))

#define AL(a) (sizeof(a)/sizeof((a)[0]))  /* Array Length */
