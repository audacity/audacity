/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */


#ifndef LAME_CONFIG_H
#define LAME_CONFIG_H


/* debug define */
/* #undef ABORTFP */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* alot of debug output */
/* #undef DEBUG */

/* allow to compute a more accurate replaygain value */
#define DECODE_ON_THE_FLY 1

/* double is faster than float on Alpha */
/* #undef FLOAT */

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* we link against libefence */
/* #undef HAVE_EFENCE */

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have the iconv() function and it works. */
#define HAVE_ICONV 1

/* add ieee754_float32_t type */
/* #undef HAVE_IEEE754_FLOAT32_T */
#ifndef HAVE_IEEE754_FLOAT32_T
	typedef float ieee754_float32_t;
#endif

/* add ieee754_float64_t type */
/* #undef HAVE_IEEE754_FLOAT64_T */
#ifndef HAVE_IEEE754_FLOAT64_T
	typedef double ieee754_float64_t;
#endif

/* system has 80 bit floats */
#define HAVE_IEEE854_FLOAT80 1

/* add ieee854_float80_t type */
/* #undef HAVE_IEEE854_FLOAT80_T */
#ifndef HAVE_IEEE854_FLOAT80_T
	typedef long double ieee854_float80_t;
#endif

/* add int16_t type */
#define HAVE_INT16_T 1
#ifndef HAVE_INT16_T
	typedef short int16_t;
#endif

/* add int32_t type */
#define HAVE_INT32_T 1
#ifndef HAVE_INT32_T
#define A_INT32_T int
	typedef A_INT32_T int32_t;
#endif

/* add int64_t type */
#define HAVE_INT64_T 1
#ifndef HAVE_INT64_T
#define A_INT64_T long
	typedef A_INT64_T int64_t;
#endif

/* add int8_t type */
#define HAVE_INT8_T 1
#ifndef HAVE_INT8_T
	typedef char int8_t;
#endif

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the <linux/soundcard.h> header file. */
/* #undef HAVE_LINUX_SOUNDCARD_H */

/* Define to 1 if the type `long double' works and has more range or precision
   than `double'. */
#define HAVE_LONG_DOUBLE 1

/* Define to 1 if the type `long double' works and has more range or precision
   than `double'. */
#define HAVE_LONG_DOUBLE_WIDER 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* build with mpglib support */
#define HAVE_MPGLIB 1

/* have nasm */
/* #undef HAVE_NASM */

/* Define to 1 if you have the <ncurses/termcap.h> header file. */
/* #undef HAVE_NCURSES_TERMCAP_H */

/* Define to 1 if you have the `socket' function. */
#define HAVE_SOCKET 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the <sys/soundcard.h> header file. */
/* #undef HAVE_SYS_SOUNDCARD_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* have termcap */
#define HAVE_TERMCAP 1

/* Define to 1 if you have the <termcap.h> header file. */
#define HAVE_TERMCAP_H 1

/* add uint16_t type */
#define HAVE_UINT16_T 1
#ifndef HAVE_UINT16_T
	typedef unsigned short uint16_t;
#endif

/* add uint32_t type */
#define HAVE_UINT32_T 1
#ifndef HAVE_UINT32_T
#define A_UINT32_T unsigned int
	typedef A_UINT32_T uint32_t;
#endif

/* add uint64_t type */
#define HAVE_UINT64_T 1
#ifndef HAVE_UINT64_T
#define A_UINT64_T unsigned long
	typedef A_UINT64_T uint64_t;
#endif

/* add uint8_t type */
#define HAVE_UINT8_T 1
#ifndef HAVE_UINT8_T
	typedef unsigned char uint8_t;
#endif

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define if SSE intrinsics work. */
/* #undef HAVE_XMMINTRIN_H */

/* Define as const if the declaration of iconv() needs const. */
#define ICONV_CONST 

/* requested by Frank, seems to be temporary needed for a smooth transition */
#define LAME_LIBRARY_BUILD 1

/* set to 1 if you have libsndfile */
/* #undef LIBSNDFILE */

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* use MMX version of choose_table */
/* #undef MMX_choose_table */

/* build without hooks for analyzer */
/* #undef NOANALYSIS */

/* Name of package */
#define PACKAGE "lame"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "lame-dev@lists.sf.net"

/* Define to the full name of this package. */
#define PACKAGE_NAME "lame"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "lame 3.100"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "lame"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "3.100"

/* The size of `double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of `float', as computed by sizeof. */
#define SIZEOF_FLOAT 4

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `long double', as computed by sizeof. */
/* #undef SIZEOF_LONG_DOUBLE */

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 8

/* The size of `unsigned long long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG 8

/* The size of `unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT 2

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* IEEE754 compatible machine */
#define TAKEHIRO_IEEE754_HACK 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* faster log implementation with less but enough precission */
#define USE_FAST_LOG 1

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Version number of package */
#define VERSION "3.100"

/* Define if using the dmalloc debugging malloc package */
/* #undef WITH_DMALLOC */

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Enable large inode numbers on Mac OS X 10.5.  */
#ifndef _DARWIN_USE_64_BIT_INODE
# define _DARWIN_USE_64_BIT_INODE 1
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* we're on DEC Alpha */
/* #undef __DECALPHA__ */

/* work around a glibc bug */
/* #undef __NO_MATH_INLINES */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

#endif /* LAME_CONFIG_H */
