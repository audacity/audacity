/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Concurrent Control with "Readers" and "Writers", P.J. Courtois et al, 1971 */

#if !defined soxr_ccrw2_included
#define soxr_ccrw2_included

#if defined SOXR_LIB
#include "internal.h"
#endif

#if defined _OPENMP

#include <omp.h>

typedef struct {
  int readcount, writecount; /* initial value = 0 */
  omp_lock_t mutex_1, mutex_2, mutex_3, w, r; /* initial value = 1 */
} ccrw2_t; /* Problem #2: `writers-preference' */

#define ccrw2_become_reader(p) do {\
  omp_set_lock(&p.mutex_3);\
    omp_set_lock(&p.r);\
      omp_set_lock(&p.mutex_1);\
        if (++p.readcount == 1) omp_set_lock(&p.w);\
      omp_unset_lock(&p.mutex_1);\
    omp_unset_lock(&p.r);\
  omp_unset_lock(&p.mutex_3);\
} while (0)
#define ccrw2_cease_reading(p) do {\
  omp_set_lock(&p.mutex_1);\
    if (!--p.readcount) omp_unset_lock(&p.w);\
  omp_unset_lock(&p.mutex_1);\
} while (0)
#define ccrw2_become_writer(p) do {\
  omp_set_lock(&p.mutex_2);\
    if (++p.writecount == 1) omp_set_lock(&p.r);\
  omp_unset_lock(&p.mutex_2);\
  omp_set_lock(&p.w);\
} while (0)
#define ccrw2_cease_writing(p) do {\
  omp_unset_lock(&p.w);\
  omp_set_lock(&p.mutex_2);\
    if (!--p.writecount) omp_unset_lock(&p.r);\
  omp_unset_lock(&p.mutex_2);\
} while (0)
#define ccrw2_init(p) do {\
  omp_init_lock(&p.mutex_1);\
  omp_init_lock(&p.mutex_2);\
  omp_init_lock(&p.mutex_3);\
  omp_init_lock(&p.w);\
  omp_init_lock(&p.r);\
} while (0)
#define ccrw2_clear(p) do {\
  omp_destroy_lock(&p.r);\
  omp_destroy_lock(&p.w);\
  omp_destroy_lock(&p.mutex_3);\
  omp_destroy_lock(&p.mutex_2);\
  omp_destroy_lock(&p.mutex_1);\
} while (0)

#else

typedef int ccrw2_t;
#define ccrw2_become_reader(x) (void)(x)
#define ccrw2_cease_reading(x) (void)(x)
#define ccrw2_become_writer(x) (void)(x)
#define ccrw2_cease_writing(x) (void)(x)
#define ccrw2_init(x) (void)(x)
#define ccrw2_clear(x) (void)(x)

#endif /* _OPENMP */

#endif
