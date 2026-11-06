/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

// MSVC has __has_builtin but not __builtin_readcyclecounter, so tracktion_CPU.h calls __rdtsc, which MSVC ARM64 lacks.
// Read the virtual counter instead, as tracktion does with other ARM compilers.

#include <intrin.h>

inline unsigned __int64 __rdtsc()
{
    return _ReadStatusReg(ARM64_CNTVCT);
}
