////////////////////////////////////////////////////////////////////////////////
///
/// Common type definitions for SoundTouch audio processing library.
///
/// Author        : Copyright (c) Olli Parviainen
/// Author e-mail : oparviai 'at' iki.fi
/// SoundTouch WWW: http://www.surina.net/soundtouch
///
////////////////////////////////////////////////////////////////////////////////
//
// Last changed  : $Date: 2006-09-18 07:39:15 $
// File revision : $Revision: 1.2 $
//
// $Id: STTypes.h,v 1.2 2006-09-18 07:39:15 richardash1981 Exp $
//
////////////////////////////////////////////////////////////////////////////////
//
// License :
//
//  SoundTouch audio processing library
//  Copyright (c) Olli Parviainen
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
////////////////////////////////////////////////////////////////////////////////

#ifndef STTypes_H
#define STTypes_H

typedef unsigned int    uint;
typedef unsigned long   ulong;

#ifdef __GNUC__
    // In GCC, include soundtouch_config.h made by config scritps
    #include "soundtouch_config.h"
#endif

#ifndef _WINDEF_
    // if these aren't defined already by Windows headers, define now

    typedef int BOOL;

    #define FALSE   0
    #define TRUE    1

#endif  // _WINDEF_


namespace soundtouch
{
/// Activate these undef's to overrule the possible sampletype 
/// setting inherited from some other header file:
//#undef INTEGER_SAMPLES
//#undef FLOAT_SAMPLES

#if !(INTEGER_SAMPLES || FLOAT_SAMPLES)
   
    /// Choose either 32bit floating point or 16bit integer sampletype
    /// by choosing one of the following defines, unless this selection 
    /// has already been done in some other file.
    ////
    /// Notes:
    /// - In Windows environment, choose the sample format with the
    ///   following defines.
    /// - In GNU environment, the floating point samples are used by 
    ///   default, but integer samples can be chosen by giving the 
    ///   following switch to the configure script:
    ///       ./configure --enable-integer-samples
    ///   However, if you still prefer to select the sample format here 
    ///   also in GNU environment, then please #undef the INTEGER_SAMPLE
    ///   and FLOAT_SAMPLE defines first as in comments above.
    //#define INTEGER_SAMPLES     1    //< 16bit integer samples
    #define FLOAT_SAMPLES       1    //< 32bit float samples
 
 #endif

    /// Define this to allow CPU-specific assembler optimizations. Notice that 
    /// having this enabled on non-x86 platforms doesn't matter; the compiler can 
    /// drop unsupported extensions on different platforms automatically. 
    /// However, if you're having difficulties getting the optimized routines 
    /// compiled with your compler (e.g. some gcc compiler versions may be picky), 
    /// you may wish to disable the optimizations to make the library compile.
    #define ALLOW_OPTIMIZATIONS     1


    // If defined, allows the SIMD-optimized routines to take minor shortcuts 
    // for improved performance. Undefine to require faithfully similar SIMD 
    // calculations as in normal C implementation.
    #define ALLOW_NONEXACT_SIMD_OPTIMIZATION    1


    #ifdef INTEGER_SAMPLES
        // 16bit integer sample type
        typedef short SAMPLETYPE;
        // data type for sample accumulation: Use 32bit integer to prevent overflows
        typedef long  LONG_SAMPLETYPE;

        #ifdef FLOAT_SAMPLES
            // check that only one sample type is defined
            #error "conflicting sample types defined"
        #endif // FLOAT_SAMPLES

        #ifdef ALLOW_OPTIMIZATIONS
            #if (WIN32 || __i386__ || __x86_64__)
                // Allow MMX optimizations
                #define ALLOW_MMX   1
            #endif
        #endif

    #else

        // floating point samples
        typedef float  SAMPLETYPE;
        // data type for sample accumulation: Use double to utilize full precision.
        typedef double LONG_SAMPLETYPE;

        #ifdef ALLOW_OPTIMIZATIONS
                // Allow 3DNow! and SSE optimizations
            #if WIN32
                #define ALLOW_3DNOW     1
            #endif

            #if (WIN32 || __i386__ || __x86_64__)
                #define ALLOW_SSE       1
            #endif
        #endif

    #endif  // INTEGER_SAMPLES
};

#endif
