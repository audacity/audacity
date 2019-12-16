/* stdint.h -- missing from Microsoft compilers, so this is a
 *   version constructed solely for libsndfile. It does not 
 *   have a complete set of definitions.
 *
 * This file should be in a directory of include files used only
 * by Microsoft compilers
 *
 * Roger Dannenberg, Aug 2012
 */

#ifndef _MSC_VER
#error "This Microsoft-specific file was included by a non-Microsoft compiler"
#endif

#ifndef STDINT_H
#define STDINT_H

#ifdef __cplusplus
extern "C" {
#endif
#include <wchar.h>
#ifdef __cplusplus
}
#endif

// Define _W64 macros to mark types changing their size, like intptr_t.
#ifndef _W64
#if !defined(__midl) && (defined(_X86_) || defined(_M_IX86)) && _MSC_VER >= 1300
#define _W64 __w64
#else
#define _W64
#endif
#endif

#if (_MSC_VER < 1300)
//typedef signed char       int8_t;
typedef signed short      int16_t;
typedef signed int        int32_t;
//typedef unsigned char     uint8_t;
typedef unsigned short    uint16_t;
typedef unsigned int      uint32_t;
#else
//typedef signed __int8     int8_t;
typedef signed __int16    int16_t;
typedef signed __int32    int32_t;
//typedef unsigned __int8   uint8_t;
typedef unsigned __int16  uint16_t;
typedef unsigned __int32  uint32_t;
#endif
//typedef signed __int64       int64_t;
//typedef unsigned __int64     uint64_t;


#endif // STDINT_H

