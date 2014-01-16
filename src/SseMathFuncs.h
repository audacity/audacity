/* SIMD (SSE1+MMX or SSE2) implementation of sin, cos, exp and log

Inspired by Intel Approximate Math library, and based on the
corresponding algorithms of the cephes math library

The default is to use the SSE1 version. If you define USE_SSE2 the
the SSE2 intrinsics will be used in place of the MMX intrinsics. Do
not expect any significant performance improvement with SSE2.
*/

/* Copyright (C) 2007  Julien Pommier

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software. If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.

(this is the zlib license)
*/
#ifndef SSE_MATHFUN
#define SSE_MATHFUN

#include <inttypes.h>
#include <xmmintrin.h>

/* yes I know, the top of this file is quite ugly */

#ifdef _MSC_VER /* visual c++ */
# define ALIGN16_BEG __declspec(align(16))
# define ALIGN16_END 
#else /* gcc or icc */
# define ALIGN16_BEG
# define ALIGN16_END __attribute__((aligned(16)))
#endif

/* __m128 is ugly to write */
//typedef __m128 _v4sfu;  // vector of 4 float (sse1)

#ifndef USE_SSE2 //sry this is all sse2 now
#define USE_SSE2
#endif

#ifdef USE_SSE2
# include <emmintrin.h>
#else
typedef __m64 v2si;   // vector of 2 int (mmx)
#endif

// !!! Andrew Hallendorff Warning changed call structure to make compatible with gcc

typedef ALIGN16_BEG union {
     float               m128_f32[4];
     int8_t              m128_i8[16];
     int16_t             m128_i16[8];
     int32_t             m128_i32[4];
     int64_t             m128_i64[2];
     uint8_t             m128_u8[16];
     uint16_t            m128_u16[8];
     uint32_t            m128_u32[4];
     uint64_t            m128_u64[2];
 } ALIGN16_END v4sfu;

__m128 log_ps(v4sfu *xPtr);
__m128 sin_ps(v4sfu *xPtr);
void sincos_ps(v4sfu *xptr, v4sfu *sptr, v4sfu *cptr);



#endif