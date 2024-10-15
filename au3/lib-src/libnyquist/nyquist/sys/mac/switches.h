/* switches.h for Macintosh */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  major reorganization of conditional compilation in Nyquist
 */


#define HAS_STDLIB_H 1
#undef HAS_SYS_TYPES_H
#undef HAS_SYS_STAT_H
#define HAS_STAT_H 1
#undef HAS_MALLOC_H

#define HAS_GETTIMEOFDAY 1

#undef READ_LINE

#define XL_BIG_ENDIAN 1
#undef XL_LITTLE_ENDIAN

#define USE_RAND 1
#undef USE_RANDOM

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

/* explicitly choose a platform */
#undef UNIX
#undef WINDOWS
#undef MICROSOFT
#undef DOS
#define MACINTOSH 1

#define BUFFERED_SYNCHRONOUS_INPUT 1
#define SPACE_FOR_PLAY 10000
#define MAX_CHANNELS 16

/* this will enable code to read midi files, etc. */
#define CMTSTUFF 1

/* NYQUIST tells some CMT code that we're really in
 * XLISP and NYQUIST
 */
#define NYQUIST 1

#include "swlogic.h"
