/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_internal_included
#define soxr_internal_included

#include "std-types.h"



#undef min
#undef max
#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))



#define range_limit(x, lower, upper) (min(max(x, lower), upper))
#define linear_to_dB(x) (log10(x) * 20)
#define array_length(a) (sizeof(a)/sizeof(a[0]))
#if !defined AL
#define AL(a) array_length(a)
#endif
#define iAL(a) (int)AL(a)
#define sqr(a) ((a) * (a))



#if defined __GNUC__
  #define UNUSED __attribute__ ((unused))
#else
  #define UNUSED
#endif



#if !WITH_DEV_TRACE
  #ifdef __GNUC__
    void lsx_dummy(char const *, ...);
  #else
    static __inline void lsx_dummy(char const * x, ...) {}
  #endif
  #define lsx_debug if(0) lsx_dummy
  #define lsx_debug_more lsx_debug
#else
  extern int _soxr_trace_level;
  void _soxr_trace(char const * fmt, ...);
  #define lsx_debug      if (_soxr_trace_level > 0) _soxr_trace
  #define lsx_debug_more if (_soxr_trace_level > 1) _soxr_trace
#endif



/* soxr_quality_spec_t.flags: */

#define SOXR_ROLLOFF_LSR2Q     3u    /* Reserved for internal use. */
#define SOXR_ROLLOFF_MASK      3u    /* For masking these bits. */
#define SOXR_MAINTAIN_3DB_PT   4u    /* Reserved for internal use. */
#define SOXR_PROMOTE_TO_LQ    64u    /* Reserved for internal use. */



/* soxr_runtime_spec_t.flags: */

#define SOXR_STRICT_BUFFERING  4u    /* Reserved for future use. */
#define SOXR_NOSMALLINTOPT     8u    /* For test purposes only. */



/* soxr_quality_spec recipe: */

#define SOXR_PRECISIONQ         11   /* Quality specified by the precision parameter. */

#define SOXR_PHASE_MASK         0x30 /* For masking these bits. */



/* soxr_quality_spec flags: */

#define RESET_ON_CLEAR   (1u<<31)



#endif
