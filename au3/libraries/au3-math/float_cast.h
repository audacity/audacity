/*
** Copyright (C) 2001 Erik de Castro Lopo <erikd AT mega-nerd DOT com>
**
** Permission to use, copy, modify, distribute, and sell this file for any
** purpose is hereby granted without fee, provided that the above copyright
** and this permission notice appear in all copies.  No representations are
** made about the suitability of this software for any purpose.  It is
** provided "as is" without express or implied warranty.
*/

/* Version 1.1 */

/*============================================================================
**	On Intel Pentium processors (especially PIII and probably P4), converting
**	from float to int is very slow. To meet the C specs, the code produced by
**	most C compilers targeting Pentium needs to change the FPU rounding mode
**	before the float to int conversion is performed.
**
**	Changing the FPU rounding mode causes the FPU pipeline to be flushed. It
**	is this flushing of the pipeline which is so slow.
**
**	Fortunately the ISO C99 specifications define the functions lrint, lrintf,
**	llrint and llrintf which fix this problem as a side effect.
**
**	On Unix-like systems, the configure process should have detected the
**	presence of these functions. If they weren't found we have to replace them
**	here with a standard C cast.
*/

/*
**	The C99 prototypes for lrint and lrintf are as follows:
**
**		long int lrintf (float x) ;
**		long int lrint  (double x) ;
*/

/*	The presence of the required functions are detected during the configure
**	process and the values HAVE_LRINT and HAVE_LRINTF are set accordingly in
**	the config.h file.
*/
#if (defined (WIN32) || defined (_WIN32)) && defined(_MSC_VER) && defined(_M_IX86)
// As of Visual Studio 2019 16.9, these functions have been made intrinsic and the build
// will fail. Unfortunately, the intrinsic versions run a LOT slower than the ones
// below, so force the compiler to use ours instead.
      #pragma function( lrint, lrintf )

// Including math.h allows us to use the inline assembler versions without
// producing errors in newer Visual Studio versions.
// Without the include, we get different linkage error messages.
// Without the inline assembler versions, these functions are VERY slow.
// I also see that the include was part of the original source for this file:
//    http://www.mega-nerd.com/FPcast/

   #include <math.h>

/*	Win32 doesn't seem to have these functions.
	**	Therefore implement inline versions of these functions here.
	*/
__inline long int
lrint(double flt)
{
    int intgr;

    _asm
    { fld flt
      fistp intgr
    };

    return intgr;
}

__inline long int
lrintf(float flt)
{
    int intgr;

    _asm
    { fld flt
      fistp intgr
    };

    return intgr;
}

__inline long long int
llrint(double flt)
{
    long long int intgr;

    _asm
    { fld flt
      fistp intgr
    };

    return intgr;
}

__inline long long int
llrintf(float flt)
{
    long long int intgr;

    _asm
    { fld flt
      fistp intgr
    };

    return intgr;
}

#elif (defined (WIN32) || defined (_WIN32)) && defined(_M_X64)

      #include <math.h>
      #include <immintrin.h>
      #include <emmintrin.h>

      #ifdef _MSC_VER
            #pragma function(lrint, lrintf)
      #endif

__inline
long int lrint(double flt)
{
    return _mm_cvtsd_si32(_mm_set_sd(flt));
}

__inline
long int lrintf(float flt)
{
    return _mm_cvtss_si32(_mm_set_ss(flt));
}

__inline
long long int llrint(double flt)
{
    return _mm_cvtsd_si64(_mm_set_sd(flt));
}

__inline
long long int llrintf(float flt)
{
    return _mm_cvtss_si64(_mm_set_ss(flt));
}

#elif (HAVE_LRINT && HAVE_LRINTF)

/*	These defines enable functionality introduced with the 1999 ISO C
	**	standard. They must be defined before the inclusion of math.h to
	**	engage them. If optimisation is enabled, these functions will be
	**	inlined. With optimisation switched off, you have to link in the
	**	maths library using -lm.
	*/

      #define     _ISOC9X_SOURCE 1
      #define  _ISOC99_SOURCE 1

      #define     __USE_ISOC9X   1
      #define     __USE_ISOC99   1

      #include    <math.h>
#else

/* dmazzoni: modified these to do a proper rounding, even though
    * it's slower.  Correctness and consistency is more important
    * than speed, especially since lrint/lrintf are certainly not
    * available everywhere.
    *
    * MM: Now uses internal math.h rint() function
    */

   #include <math.h>

   #define  lrint(dbl)        ((int)rint(dbl))
   #define  lrintf(flt)       ((int)rint(flt))

#endif
