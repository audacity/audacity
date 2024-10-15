/* switches.h -- conditional compilation features for WIN32 systems */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  major reorganization of conditional compilation in Nyquist
 */

#ifdef SWITCHES
Error: switches.h included more than once.
#endif

#define __func__ __FUNCTION__

#define HAS_STDLIB_H 1
#define HAS_SYS_TYPES_H 1
#define HAS_SYS_STAT_H 1
#undef HAS_STAT_H
#undef HAS_MALLOC_H

/* define one of HAS_GETTIMEOFDAY, HAS_FTIME, */
#undef HAS_GETTIMEOFDAY
#undef HAS_FTIME

#undef READ_LINE

#undef USE_RANDOM
#define USE_RAND 1


/* since there are 2 versions Nyquist for windows: nyquist and nyqwin, 
   we use WINGUI to decide which to compile 
 */
#ifndef WINGUI
/* use C library printf as nyquist_printf() */
#define USE_PRINTF 1
#endif

/* define this to be printf, or define your own fn of the form
     void nyquist_printf(char *format, ...);
   (for a GUI)
*/
void nyquist_printf(char *format, ...);

#define NEED_ULONG 1
#define NEED_USHORT 1
#define NEED_BYTE 1

#define NEED_ROUND 1

#undef NEED_DEFINE_MALLOC

/* definitions for libsndfile */

/* Target processor clips on negative float to int conversion */
/* (true on i386) */
#define CPU_CLIPS_NEGATIVE 1

/* Target processor clips on positive float to int conversion */
/* (true on i386) */
#define CPU_CLIPS_POSITIVE 1

/* Target processor is little endian. */
#define CPU_IS_LITTLE_ENDIAN 1
/* Target processor is big endian. */
#define CPU_IS_BIG_ENDIAN 0

/* Set to 1 if S_IRGRP is defined */
#define HAVE_DECL_S_IRGRP 0

/* Set to 1 if the compiler supports the struct hack. */
#define HAVE_FLEXIBLE_ARRAY 1

/* Define to 1 if you have the `fsync' function. */
#define HAVE_FSYNC 1

/* Define to 1 if you have the `gmtime' function. */
#define HAVE_GMTIME 1

/* Define to 1 if you have the `gmtime_r' function. */
#undef HAVE_GMTIME_R

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 0

/* Define if you have C99's lrint function. */
#define HAVE_LRINT 0

/* Define if you have C99's lrintf function. */
#define HAVE_LRINTF 0

/* Define to 1 if you have the `snprintf' function. */
// #define snprintf _snprintf -- snprintf is a Standard Library function
#define HAVE_SNPRINTF 1

/* Define to 1 if the system has the type `ssize_t'. */
#define ssize_t SSIZE_T
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
#define HAVE_UNISTD_H 0

/* Define to 1 if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Set to 1 if compiling for MacOSX */
#define OS_IS_MACOSX 0

/* Set to 1 if compiling for Win32 */
#define OS_IS_WIN32 1

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Set to 1 to use the native windows API */
#define USE_WINDOWS_API 1

/* The size of `int64_t', as computed by sizeof. */
/* Omit this if int64_t is not a type */
#undef SIZEOF_INT64_T

/* The size of long as computed by sizeof. */
#define SIZEOF_LONG 4

/* Set to long if unknown */
#define SIZEOF_SF_COUNT_T 8

/* explicitly choose a platform */
#undef UNIX

#ifndef WINDOWS
#define WINDOWS 1
#endif

#ifndef WIN32
#define WIN32 1
#endif

#ifndef MICROSOFT
#define MICROSOFT 1
#endif

#ifndef DOS
#define DOS 1
#endif

#undef MACINTOSH

// Under Windows, we do not want synchronous input because then we do not
// get the break character (^G) while XLISP is busy. Actually, there is a
// way to send a message to the process, but the Java IDE cannot send
// Windows messages, so we have to check for console character input
// using the _kbhit function.
//#define BUFFERED_SYNCHRONOUS_INPUT 1
#define SPACE_FOR_PLAY 10000
#define MAX_CHANNELS 16

/* this will enable code to read midi files, etc. */
#define CMTSTUFF 1

/* NYQUIST tells some CMT code that we're really in
 * XLISP and NYQUIST
 */
#define NYQUIST 1

#include "swlogic.h"
