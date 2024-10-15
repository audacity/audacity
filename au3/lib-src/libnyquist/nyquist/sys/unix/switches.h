/* see sys/switches.h.template */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  major reorganization of conditional compilation in Nyquist
 */

#define HAS_STDLIB_H 1
#define HAS_SYS_TYPES_H 1
#define HAS_SYS_STAT_H 1
#undef HAS_STAT_H
#undef HAS_MALLOC_H

#define HAS_GETTIMEOFDAY 1

// I think that READ_LINE prevents user from typing control characters to
// get info during lisp execution. This needs to be tested. Using READ_LINE
// is preventing any character echoing now, maybe due to new "improved"
// command line handling added recently. -RBD

// #define READ_LINE 1

/* this is defined in xlisp.h - RBD
#if i386
#define XL_LITTLE_ENDIAN 1
#elif __i386__
#define XL_LITTLE_ENDIAN 1
#else
#define XL_BIG_ENDIAN 1
#endif
*/

#undef USE_RANDOM
#define USE_RAND 1

/* define this to be printf, or define your own fn of the form
     void nyquist_printf(char *format, ...);
   (for a GUI)
*/
#define nyquist_printf printf

/* macOS, FreeBSD or OpenBSD */
#if (defined(__APPLE__) && defined(__GNUC__)) || defined(__FreeBSD__) || \
    defined(__OpenBSD__)
#define NEED_ULONG 1
#else
#include <sys/types.h>
#undef NEED_ULONG
#endif

#undef NEED_USHORT
#define NEED_BYTE 1

#define NEED_ROUND 1

#undef NEED_DEFINE_MALLOC

/* definitions for libsndfile */

/* Target processor clips on negative float to int conversion */
/* (true on i386 and PPC) */
#define CPU_CLIPS_NEGATIVE 1

/* Target processor clips on positive float to int conversion */
/* (true on i386 and PPC) */
#define CPU_CLIPS_POSITIVE 1

#ifdef __APPLE__
 #if defined (__LITTLE_ENDIAN__)
  /* Target processor is little endian. */
  #define CPU_IS_LITTLE_ENDIAN 1
  /* Target processor is big endian. */
  #define CPU_IS_BIG_ENDIAN 0
 #else
  /* Target processor is little endian. */
  #define CPU_IS_LITTLE_ENDIAN 0
  /* Target processor is big endian. */
  #define CPU_IS_BIG_ENDIAN 1
 #endif
#else
 #if defined(__linux__) || defined(__GLIBC__)
  #include <endian.h>
  #if __BYTE_ORDER == __LITTLE_ENDIAN
   /* Target processor is little endian. */
   #define CPU_IS_LITTLE_ENDIAN 1
   /* Target processor is big endian. */
   #define CPU_IS_BIG_ENDIAN 0
  #else
   /* Target processor is little endian. */
   #define CPU_IS_LITTLE_ENDIAN 0
   /* Target processor is big endian. */
   #define CPU_IS_BIG_ENDIAN 1
  #endif
 #else /* default is little endian */
   /* Target processor is little endian. */
   #define CPU_IS_LITTLE_ENDIAN 1
   /* Target processor is big endian. */
   #define CPU_IS_BIG_ENDIAN 0
 #endif
#endif

/* Set to 1 if S_IRGRP is defined */
#define HAVE_DECL_S_IRGRP 1

/* Set to 1 if the compiler supports the struct hack. */
#define HAVE_FLEXIBLE_ARRAY 1

/* Define to 1 if you have the `fsync' function. */
#define HAVE_FSYNC 1

/* Define to 1 if you have the `gmtime' function. */
#define HAVE_GMTIME 1

/* Define to 1 if you have the `gmtime_r' function. */
#define HAVE_GMTIME_R 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define if you have C99's lrint function. */
#define HAVE_LRINT 1

/* Define if you have C99's lrintf function. */
#define HAVE_LRINTF 1

/* Define to 1 if you have the `snprintf' function. */
#define HAVE_SNPRINTF 1

/* Define to 1 if the system has the type `ssize_t'. */
#define HAVE_SSIZE_T 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Set to 1 if compiling for MacOSX */
#ifdef __APPLE__
 #define OS_IS_MACOSX 1
#else
 #define OS_IS_MACOSX 0
#endif

/* Set to 1 if compiling for Win32 */
#define OS_IS_WIN32 0

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Set to 1 to use the native windows API */
#define USE_WINDOWS_API 0

#ifdef __GNUC__
 #define SIZEOF_LONG_LONG 8
#endif

/* The size of `int64_t', as computed by sizeof. */
#define SIZEOF_INT64_T 8

/* The size of long as computed by sizeof. */
#define SIZEOF_LONG 4

/* Set to long if unknown */
#define SIZEOF_SF_COUNT_T 8

/* explicitly choose a platform */
#define UNIX 1
#undef WINDOWS
#undef MICROSOFT
#undef DOS
#undef MACINTOSH

#define BUFFERED_SYNCHRONOUS_INPUT 1
#define SPACE_FOR_PLAY 10000
#define MAX_CHANNELS 16

/* this will enable code to read midi files, etc. */
#define CMTSTUFF 1

/* NYQUIST tells some CMT code that we're really in
 * XLISP and NYQUIST
 */
#ifndef NYQUIST
#define NYQUIST 1
#endif

/* If SAFE_NYQUIST is defined, Nyquist will prevent:
 * - writes anywhere except in the directory tree
 *     named after the -w flag (if any) on the command
 *     line
 * - play sounds or open the audio input or output
 * - any networking functions.
 */

/* If SECURE_NYQUIST is defined, Nyquist will
 * not read anywhere except in the directory forest
 * given by the initial search path (XLISPPATH) and
 * by a path list following -r on the command line
 * (if any).
 */

#include "swlogic.h"

