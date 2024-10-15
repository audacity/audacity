// -*- mode: c++ -*-
#ifndef SSE_H
#define SSE_H

#include "config.h"

#if defined(ENABLE_SSE) && !defined(APPLE_PPC)

#include <xmmintrin.h>

namespace _sbsms_ {

typedef __m128 simd_vector;
#define LOAD(p) _mm_loadu_ps((float*)(p))
#define STORE(v,p) _mm_storeu_ps((float*)(p),v)
#define LOAD16(p) _mm_load_ps((float*)(p))
#define STORE16(v,p) _mm_store_ps((float*)(p),v)
#define LOADL(v,p) _mm_loadl_pi(v,(const __m64*)(p))
#define LOADH(v,p) _mm_loadh_pi(v,(const __m64*)(p))
#define STOREL(v,p) _mm_storel_pi((__m64 *)(p), v)
#define STOREH(v,p) _mm_storeh_pi((__m64 *)(p), v)
#define VADD(v1,v2) _mm_add_ps(v1,v2)
#define VSUB(v1,v2) _mm_sub_ps(v1,v2)
#define VMUL(v1,v2) _mm_mul_ps(v1,v2)
#define SHUFFLE(a,b,w,x,y,z) _mm_shuffle_ps(a,b,_MM_SHUFFLE(z,y,x,w))
#define SET(f) _mm_set1_ps(f)

}

#endif

#endif
