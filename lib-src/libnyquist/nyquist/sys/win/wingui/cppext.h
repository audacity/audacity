#pragma warning( disable: 4237 )		// eub: temporary kludge

// RBD -- this will define true, false, bool
#include "yvals.h"

//these preprocessor checks seem to get rid of compiler
//	error (redecl boolean)
// typedef int bool;
//#ifndef TRUE
#define TRUE 1
#define FALSE 0
//#endif

#define EOS '\000'

// now defined by yvals.h:
//#define true 1
//#define false 0

#define MALLOC ::malloc				// note -- these macros are not consistently used
#define FREE ::free

void *MEMGET(long n);
void *MEMFREE(void *data, long n);

#define STREQ(a, b)	(strcmp((a), (b)) == 0)

typedef unsigned long	uint32;
typedef long 			int32;
typedef unsigned short	uint16;
typedef short 			int16;
typedef unsigned char	uint8;

//istvan 082197
// RBD commented out the following, instead, we're including yvals.h,
//  a microsoft-dependent file
// #ifndef bool
// typedef unsigned char bool;
// #endif
