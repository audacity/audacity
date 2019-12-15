#ifndef PA_DYNLINK_H
#define PA_DYNLINK_H
/*
 * $Id: pa_dynlink.h 1339 2008-02-15 07:50:33Z rossb $
 * Portable Audio I/O Library
 * Dynamic library helper
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 1999-2008 Ross Bencina, Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

/** @file
 @ingroup common_src

 @brief Dynamic library helper functions.
*/


#include "pa_debugprint.h"

#if defined(WIN32)
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

#if defined(WIN32)
typedef HANDLE paDynamicLib;
#define PADL_INLINE __inline
#else
typedef void * paDynamicLib;
#define PADL_INLINE inline
#endif

paDynamicLib PaDL_Load( char *name );
void PaDL_Unload( paDynamicLib lib );
void *PaDL_FindSymbol( paDynamicLib lib, char *name );
    
/* A little explanation of what's going on here.
 *
 * Only one source file should define PADL_DEFINE_POINTERS before including the header which
 * defines the functions.  This will cause the compiler to dump all of the function pointers
 * to a single object file and prevent duplicate symbol definitions during link.
 *
 * The PADL_FUNC_WITH_RETURN and PADL_FUNC_NO_RETURN macros do two things each:
 * 1)  Define or reference the variable that contains the actual function pointer
 * 2)  Define an inline function to pass control to the real function
 *
 * Since the macros redefine the real functions of the same name, the compiler will make
 * sure that the definitions are the same.  If not, it will complain.  For this to occur,
 * the functions MUST be defined in an extern "C" block otherwise the compiler just thinks the
 * functions are being overloaded.
 * 
 * The compiler should optimize away the inline function since it just passes control to the real
 * function and we should wind up with about the same function call we had before, only now it is
 * safer due to the validation.
 *
 * The PADL_FUNC_WITH_RETURN takes 4 arguments:
 * 1)  The return type           <---|
 * 2)  The function name             | Taken from the real funciton prototype
 * 3)  The function arguments    <---|
 * 4)  The argument list to pass to the real function
 *
 * The PADL_FUNC_NO_RETURN takes 3 arguments:
 * 1)  The function name         <---| Taken from the FFmpeg funciton prototype
 * 2)  The function arguments    <---|
 * 3)  The argument list to pass to the real function
 *
 * The PADL_FINDSYMBOL macro is responsible for retrieving the address of the real function
 * and storing that address in the function pointer variable.
 */
#if defined(PADL_DEFINE_POINTERS)
   #define FFX
#else
   #define FFX extern
#endif

#define PADL_FUNC_WITH_RETURN(r, n, a, p)                               \
   FFX r (*paDynFunc_ ## n ## _fp) a;                                   \
   PADL_INLINE r n a                                                    \
   {                                                                    \
      return paDynFunc_ ## n ## _fp p;                                  \
   }                                                                    \

#define PADL_FUNC_NO_RETURN(n, a, p)                                    \
   FFX void (*paDynFunc_ ## n ## _fp) a;                                \
   PADL_INLINE void n a                                                 \
   {                                                                    \
      paDynFunc_ ## n ## _fp p;                                         \
   }                                                                    \

#define PADL_FINDSYMBOL(l, f, e)                                        \
   *(void**)& paDynFunc_ ## f ## _fp = PaDL_FindSymbol(l, #f);          \
   if (!paDynFunc_ ## f ## _fp)                                         \
   {                                                                    \
      PA_DEBUG(("Could not locate address of %s\n", #f));               \
      e;                                                                \
   }

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* PA_DYNLINK_H */
