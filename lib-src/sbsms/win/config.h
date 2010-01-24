/* Windows specific config.h */

/* Define to 1 if you have the `calloc' function. */
#define HAVE_CALLOC 1

/* Define to 1 if you have the `ceil' function. */
#define HAVE_CEIL 1

/* Define to 1 if you have the <dlfcn.h> header file. */
// #define HAVE_DLFCN_H 1

/* Define to 1 if you have the `floor' function. */
#define HAVE_FLOOR 1

/* Define to 1 if you have the `free' function. */
#define HAVE_FREE 1

/* Define to 1 if you have the <inttypes.h> header file. */
// #define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `m' library (-lm). */
// #define HAVE_LIBM 1

/* Define to 1 if you have the `lrint' function. */
// #define HAVE_LRINT 1

/* Define to 1 if you have the `lrintf' function. */
// #define HAVE_LRINTF 1

/* Define to 1 if you have the `malloc' function. */
#define HAVE_MALLOC 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `sqrt' function. */
#define HAVE_SQRT 1

/* Define to 1 if you have the <stdint.h> header file. */
// #define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Name of package */
#define PACKAGE "libsbsms"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "sbsms"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "sbsms 1.5.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "sbsms"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.5.0"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "1.5.0"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
#  define WORDS_BIGENDIAN 1
# endif
#endif
