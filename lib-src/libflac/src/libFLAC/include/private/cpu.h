/* libFLAC - Free Lossless Audio Codec library
 * Copyright (C) 2001-2009  Josh Coalson
 * Copyright (C) 2011-2014  Xiph.Org Foundation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of the Xiph.org Foundation nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef FLAC__PRIVATE__CPU_H
#define FLAC__PRIVATE__CPU_H

#include "FLAC/ordinals.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined FLAC__HAS_X86INTRIN
/* SSE intrinsics support by ICC/MSVC/GCC */
#if defined __INTEL_COMPILER
  #define FLAC__SSE_TARGET(x)
  #define FLAC__SSE_SUPPORTED 1
  #define FLAC__SSE2_SUPPORTED 1
  #if (__INTEL_COMPILER >= 1000) /* Intel C++ Compiler 10.0 */
    #define FLAC__SSSE3_SUPPORTED 1
    #define FLAC__SSE4_1_SUPPORTED 1
  #endif
  #if (__INTEL_COMPILER >= 1110) /* Intel C++ Compiler 11.1 */
    #define FLAC__AVX_SUPPORTED 1
  #endif
  #if (__INTEL_COMPILER >= 1300) /* Intel C++ Compiler 13.0 */
    #define FLAC__AVX2_SUPPORTED 1
    #define FLAC__FMA_SUPPORTED 1
  #endif
#elif defined _MSC_VER
  #define FLAC__SSE_TARGET(x)
  #define FLAC__SSE_SUPPORTED 1
  #define FLAC__SSE2_SUPPORTED 1
  #if (_MSC_VER >= 1500) /* MS Visual Studio 2008 */
    #define FLAC__SSSE3_SUPPORTED 1
    #define FLAC__SSE4_1_SUPPORTED 1
  #endif
  #if (_MSC_FULL_VER >= 160040219) /* MS Visual Studio 2010 SP1 */
    #define FLAC__AVX_SUPPORTED 1
  #endif
  #if (_MSC_VER >= 1700) /* MS Visual Studio 2012 */
    #define FLAC__AVX2_SUPPORTED 1
    #define FLAC__FMA_SUPPORTED 1
  #endif
#elif defined __GNUC__
  #if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)) /* since GCC 4.9 -msse.. compiler options aren't necessary */
    #define FLAC__SSE_TARGET(x) __attribute__ ((__target__ (x)))
    #define FLAC__SSE_SUPPORTED 1
    #define FLAC__SSE2_SUPPORTED 1
    #define FLAC__SSSE3_SUPPORTED 1
    #define FLAC__SSE4_1_SUPPORTED 1
    #define FLAC__AVX_SUPPORTED 1
    #define FLAC__AVX2_SUPPORTED 1
    #define FLAC__FMA_SUPPORTED 1
  #else /* for GCC older than 4.9 */
    #define FLAC__SSE_TARGET(x)
    #ifdef __SSE__
      #define FLAC__SSE_SUPPORTED 1
    #endif
    #ifdef __SSE2__
      #define FLAC__SSE2_SUPPORTED 1
    #endif
    #ifdef __SSSE3__
      #define FLAC__SSSE3_SUPPORTED 1
    #endif
    #ifdef __SSE4_1__
      #define FLAC__SSE4_1_SUPPORTED 1
    #endif
    #ifdef __AVX__
      #define FLAC__AVX_SUPPORTED 1
    #endif
    #ifdef __AVX2__
      #define FLAC__AVX2_SUPPORTED 1
    #endif
    #ifdef __FMA__
      #define FLAC__FMA_SUPPORTED 1
    #endif
  #endif /* GCC version */
#endif /* compiler version */
#endif /* intrinsics support */

typedef enum {
	FLAC__CPUINFO_TYPE_IA32,
	FLAC__CPUINFO_TYPE_X86_64,
	FLAC__CPUINFO_TYPE_UNKNOWN
} FLAC__CPUInfo_Type;

#if defined FLAC__CPU_IA32
typedef struct {
	FLAC__bool cmov;
	FLAC__bool mmx;
	FLAC__bool sse;
	FLAC__bool sse2;

	FLAC__bool sse3;
	FLAC__bool ssse3;
	FLAC__bool sse41;
	FLAC__bool sse42;
	FLAC__bool avx;
	FLAC__bool avx2;
	FLAC__bool fma;
} FLAC__CPUInfo_IA32;
#elif defined FLAC__CPU_X86_64
typedef struct {
	FLAC__bool sse3;
	FLAC__bool ssse3;
	FLAC__bool sse41;
	FLAC__bool sse42;
	FLAC__bool avx;
	FLAC__bool avx2;
	FLAC__bool fma;
} FLAC__CPUInfo_x86;
#endif

typedef struct {
	FLAC__bool use_asm;
	FLAC__CPUInfo_Type type;
#if defined FLAC__CPU_IA32
	FLAC__CPUInfo_IA32 ia32;
#elif defined FLAC__CPU_X86_64
	FLAC__CPUInfo_x86 x86;
#endif
} FLAC__CPUInfo;

void FLAC__cpu_info(FLAC__CPUInfo *info);

#ifndef FLAC__NO_ASM
# if defined FLAC__CPU_IA32 && defined FLAC__HAS_NASM
FLAC__uint32 FLAC__cpu_have_cpuid_asm_ia32(void);
void         FLAC__cpu_info_asm_ia32(FLAC__uint32 *flags_edx, FLAC__uint32 *flags_ecx);
# endif
# if (defined FLAC__CPU_IA32 || defined FLAC__CPU_X86_64) && defined FLAC__HAS_X86INTRIN
FLAC__uint32 FLAC__cpu_have_cpuid_x86(void);
void         FLAC__cpu_info_x86(FLAC__uint32 level, FLAC__uint32 *eax, FLAC__uint32 *ebx, FLAC__uint32 *ecx, FLAC__uint32 *edx);
FLAC__uint32 FLAC__cpu_xgetbv_x86(void);
# endif
#endif

#endif
