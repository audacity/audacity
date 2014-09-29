/**********************************************************************

  resample_defs.h

  Real-time library interface by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

**********************************************************************/

#ifndef __RESAMPLE_DEFS__
#define __RESAMPLE_DEFS__

#if !defined(WIN32) && !defined(__CYGWIN__)
#include "config.h"
#endif

#ifndef TRUE
#define TRUE  1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef PI
#define PI (3.14159265358979232846)
#endif

#ifndef PI2
#define PI2 (6.28318530717958465692)
#endif

#define D2R (0.01745329348)          /* (2*pi)/360 */
#define R2D (57.29577951)            /* 360/(2*pi) */

#ifndef MAX
#define MAX(x,y) ((x)>(y) ?(x):(y))
#endif
#ifndef MIN
#define MIN(x,y) ((x)<(y) ?(x):(y))
#endif

#ifndef ABS
#define ABS(x)   ((x)<0   ?(-(x)):(x))
#endif

#ifndef SGN
#define SGN(x)   ((x)<0   ?(-1):((x)==0?(0):(1)))
#endif

#if HAVE_INTTYPES_H
  #include <inttypes.h>
  typedef char           BOOL;
  typedef int32_t        WORD;
  typedef uint32_t       UWORD;
#else
  typedef char           BOOL;
  typedef int            WORD;
  typedef unsigned int   UWORD;
#endif

#ifdef DEBUG
#define INLINE
#else
#define INLINE inline
#endif

/* Accuracy */

#define Npc 4096

/* Function prototypes */

int lrsSrcUp(float X[], float Y[], double factor, double *Time,
             UWORD Nx, UWORD Nwing, float LpScl,
             float Imp[], float ImpD[], BOOL Interp);

int lrsSrcUD(float X[], float Y[], double factor, double *Time,
             UWORD Nx, UWORD Nwing, float LpScl,
             float Imp[], float ImpD[], BOOL Interp);

#endif
