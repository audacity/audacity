/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include "half_coefs.h"

#define FUNCTION h8
#define CONVOLVE _ _ _ _ _ _ _ _
#define h8_l 8
#define COEFS half_fir_coefs_8
#include "half-fir.h"

#define FUNCTION h9
#define CONVOLVE _ _ _ _ _ _ _ _ _
#define h9_l 9
#define COEFS half_fir_coefs_9
#include "half-fir.h"

#define FUNCTION h10
#define CONVOLVE _ _ _ _ _ _ _ _ _ _
#define h10_l 10
#define COEFS half_fir_coefs_10
#include "half-fir.h"

#define FUNCTION h11
#define CONVOLVE _ _ _ _ _ _ _ _ _ _ _
#define h11_l 11
#define COEFS half_fir_coefs_11
#include "half-fir.h"

#define FUNCTION h12
#define CONVOLVE _ _ _ _ _ _ _ _ _ _ _ _
#define h12_l 12
#define COEFS half_fir_coefs_12
#include "half-fir.h"

#define FUNCTION h13
#define CONVOLVE _ _ _ _ _ _ _ _ _ _ _ _ _
#define h13_l 13
#define COEFS half_fir_coefs_13
#include "half-fir.h"

static struct {int num_coefs; stage_fn_t fn; float att;} const half_firs[] = {
  { 8, h8 , 136.51f},
  { 9, h9 , 152.32f},
  {10, h10, 168.07f},
  {11, h11, 183.78f},
  {12, h12, 199.44f},
  {13, h13, 212.75f},
};

#define HI_PREC_CLOCK

#define VAR_LENGTH p->n
#define VAR_CONVOLVE while (j < FIR_LENGTH) _
#define VAR_POLY_PHASE_BITS p->phase_bits

#define FUNCTION vpoly0
#define FIR_LENGTH VAR_LENGTH
#define CONVOLVE VAR_CONVOLVE
#include "poly-fir0.h"

#define FUNCTION vpoly1
#define COEF_INTERP 1
#define PHASE_BITS VAR_POLY_PHASE_BITS
#define FIR_LENGTH VAR_LENGTH
#define CONVOLVE VAR_CONVOLVE
#include "poly-fir.h"

#define FUNCTION vpoly2
#define COEF_INTERP 2
#define PHASE_BITS VAR_POLY_PHASE_BITS
#define FIR_LENGTH VAR_LENGTH
#define CONVOLVE VAR_CONVOLVE
#include "poly-fir.h"

#define FUNCTION vpoly3
#define COEF_INTERP 3
#define PHASE_BITS VAR_POLY_PHASE_BITS
#define FIR_LENGTH VAR_LENGTH
#define CONVOLVE VAR_CONVOLVE
#include "poly-fir.h"

#undef HI_PREC_CLOCK

#define U100_l 42
#if RATE_SIMD_POLY
  #define U100_l_EXTRA _ _
  #define u100_l_EXTRA _
  #define U100_l_EXTRA_LENGTH 2
  #define u100_l_EXTRA_LENGTH 1
#else
  #define U100_l_EXTRA
  #define u100_l_EXTRA
  #define U100_l_EXTRA_LENGTH 0
  #define u100_l_EXTRA_LENGTH 0
#endif
#define poly_fir_convolve_U100 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ U100_l_EXTRA
#define FUNCTION U100_0
#define FIR_LENGTH (U100_l + U100_l_EXTRA_LENGTH)
#define CONVOLVE poly_fir_convolve_U100
#include "poly-fir0.h"

#define u100_l 11
#define poly_fir_convolve_u100 _ _ _ _ _ _ _ _ _ _ _ u100_l_EXTRA
#define FUNCTION u100_0
#define FIR_LENGTH (u100_l + u100_l_EXTRA_LENGTH)
#define CONVOLVE poly_fir_convolve_u100
#include "poly-fir0.h"

#define FUNCTION u100_1
#define COEF_INTERP 1
#define PHASE_BITS 8
#define FIR_LENGTH (u100_l + u100_l_EXTRA_LENGTH)
#define CONVOLVE poly_fir_convolve_u100
#include "poly-fir.h"
#define u100_1_b 8

#define FUNCTION u100_2
#define COEF_INTERP 2
#define PHASE_BITS 6
#define FIR_LENGTH (u100_l + u100_l_EXTRA_LENGTH)
#define CONVOLVE poly_fir_convolve_u100
#include "poly-fir.h"
#define u100_2_b 6

typedef struct {float scalar; stage_fn_t fn;} poly_fir1_t;
typedef struct {float beta; poly_fir1_t interp[3];} poly_fir_t;

static poly_fir_t const poly_firs[] = {
  {-1, {{0, vpoly0}, { 7.2f, vpoly1}, {5.0f, vpoly2}}},
  {-1, {{0, vpoly0}, { 9.4f, vpoly1}, {6.7f, vpoly2}}},
  {-1, {{0, vpoly0}, {12.4f, vpoly1}, {7.8f, vpoly2}}},
  {-1, {{0, vpoly0}, {13.6f, vpoly1}, {9.3f, vpoly2}}},
  {-1, {{0, vpoly0}, {10.5f, vpoly2}, {8.4f, vpoly3}}},
  {-1, {{0, vpoly0}, {11.85f,vpoly2}, {9.0f, vpoly3}}},

  {-1, {{0, vpoly0}, { 8.0f, vpoly1}, {5.3f, vpoly2}}},
  {-1, {{0, vpoly0}, { 8.6f, vpoly1}, {5.7f, vpoly2}}},
  {-1, {{0, vpoly0}, {10.6f, vpoly1}, {6.75f,vpoly2}}},
  {-1, {{0, vpoly0}, {12.6f, vpoly1}, {8.6f, vpoly2}}},
  {-1, {{0, vpoly0}, { 9.6f, vpoly2}, {7.6f, vpoly3}}},
  {-1, {{0, vpoly0}, {11.4f, vpoly2}, {8.65f,vpoly3}}},

  {10.62f, {{U100_l, U100_0}, {0, 0}, {0, 0}}},
  {11.28f, {{u100_l, u100_0}, {u100_1_b, u100_1}, {u100_2_b, u100_2}}},
  {-1, {{0, vpoly0}, {   9, vpoly1}, {  6, vpoly2}}},
  {-1, {{0, vpoly0}, {  11, vpoly1}, {  7, vpoly2}}},
  {-1, {{0, vpoly0}, {  13, vpoly1}, {  8, vpoly2}}},
  {-1, {{0, vpoly0}, {  10, vpoly2}, {  8, vpoly3}}},
  {-1, {{0, vpoly0}, {  12, vpoly2}, {  9, vpoly3}}},
};
