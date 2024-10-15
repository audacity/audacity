/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Variable-rate resampling. */

#include <assert.h>
#include "math-wrap.h"
#include <string.h>
#include <stdlib.h>
#include "internal.h"
#define FIFO_SIZE_T int
#define FIFO_MIN 0x8000
#include "fifo.h"
#include "vr-coefs.h"

#define FADE_LEN_BITS     9
#define PHASE_BITS_D      10
#define PHASE_BITS_U      9

#define PHASES0_D         12
#define POLY_FIR_LEN_D    20
#define PHASES0_U         6
#define POLY_FIR_LEN_U    12

#define MULT32            (65536. * 65536.)
#define PHASES_D          (1 << PHASE_BITS_D)
#define PHASES_U          (1 << PHASE_BITS_U)

#define CONVOLVE \
    _ _ _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _ _ _ \
    _ _ _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _ _ _ \
    _ _ _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _ _ _

#define HALF_FIR_LEN_2 (iAL(half_fir_coefs) - 1)
#define HALF_FIR_LEN_4 (HALF_FIR_LEN_2 / 2)

#define _ sum += (input[-i] + input[i]) * half_fir_coefs[i], ++i;
static float half_fir(float const * input)
{
  long i = 1;
  float sum = input[0] * half_fir_coefs[0];
  CONVOLVE CONVOLVE
  assert(i == HALF_FIR_LEN_2 + 1);
  return (float)sum;
}
#undef _

#define _ sum += (input[-i] + input[i]) * half_fir_coefs[2*i], ++i;
static float double_fir0(float const * input)
{
  int i = 1;
  float sum = input[0] * half_fir_coefs[0];
  CONVOLVE
  assert(i == HALF_FIR_LEN_4 + 1);
  return (float)(sum * 2);
}
#undef _

#define _ sum += (input[-i] + input[1+i]) * half_fir_coefs[2*i+1], ++i;
static float double_fir1(float const * input)
{
  int i = 0;
  float sum = 0;
  CONVOLVE
  assert(i == HALF_FIR_LEN_4 + 0);
  return (float)(sum * 2);
}
#undef _

static float fast_half_fir(float const * input)
{
  int i = 0;
  float sum = input[0] * .5f;
#define _ sum += (input[-(2*i+1)] + input[2*i+1]) * fast_half_fir_coefs[i], ++i;
  _ _ _ _ _ _
#undef _
  return (float)sum;
}

#define IIR_FILTER _ _ _ _ _ _ _
#define _ in1=(in1-p->y[i])*iir_coefs[i]+tmp1;tmp1=p->y[i],p->y[i]=in1;++i;\
          in0=(in0-p->y[i])*iir_coefs[i]+tmp0;tmp0=p->y[i],p->y[i]=in0;++i;

typedef struct {float x[2], y[AL(iir_coefs)];} half_iir_t;

static float half_iir1(half_iir_t * p, float in0, float in1)
{
  int i = 0;
  float tmp0, tmp1;
  tmp0 = p->x[0], p->x[0] = in0;
  tmp1 = p->x[1], p->x[1] = in1;
  IIR_FILTER
  p->y[i] = in1 = (in1 - p->y[i]) * iir_coefs[i] + tmp1;
  return in1 + in0;
}
#undef _

static void half_iir(half_iir_t * p, float * obuf, float const * ibuf, int olen)
{
  int i;
  for (i=0; i < olen; obuf[i] = (float)half_iir1(p, ibuf[i*2], ibuf[i*2+1]),++i);
}

static void half_phase(half_iir_t * p, float * buf, int len)
{
  float const small_normal = 1/MULT32/MULT32; /* To quash denormals on path 0.*/
  int i;
  for (i = 0; i < len; buf[i] = (float)half_iir1(p, buf[i], 0), ++i);
#define _ p->y[i] += small_normal, i += 2;
  i = 0, _ IIR_FILTER
#undef _
#define _ p->y[i] -= small_normal, i += 2;
  i = 0, _ IIR_FILTER
#undef _
}

#define coef(coef_p, interp_order, fir_len, phase_num, coef_interp_num, \
    fir_coef_num) coef_p[(fir_len) * ((interp_order) + 1) * (phase_num) + \
    ((interp_order) + 1) * (fir_coef_num) + (interp_order - coef_interp_num)]

#define COEF(h,l,i) ((i)<0||(i)>=(l)?0:(h)[(i)>(l)/2?(l)-(i):(i)])
static void prepare_coefs(float * coefs, int n, int phases0, int phases,
    float const * coefs0, double multiplier)
{
  double k[6];
  int length0 = n * phases0, length = n * phases, K0 = iAL(k)/2 - 1, i, j, pos;
  float * coefs1 = malloc(((size_t)length / 2  + 1) * sizeof(*coefs1));
  float * p = coefs1, f0, f1 = 0;

  for (j = 0; j < iAL(k); k[j] = COEF(coefs0, length0, j - K0), ++j);
  for (pos = i = 0; i < length0 / 2; ++i) {
    double b=(1/24.)*(k[0]+k[4]+6*k[2]-4*(k[1]+k[3])),d=.5*(k[1]+k[3])-k[2]-b;
    double a=(1/120.)*(k[5]-k[2]-9*(9*b+d)+2.5*(k[3]-k[1])-2*(k[4]-k[0]));
    double c=(1/12.)*(k[4]-k[0]-2*(k[3]-k[1])-60*a),e=.5*(k[3]-k[1])-a-c;
    for (; pos / phases == i; pos += phases0) {
      double x = (double)(pos % phases) / phases;
      *p++ = (float)(k[K0] + ((((a*x + b)*x + c)*x + d)*x + e)*x);
    }
    for (j = 0; j < iAL(k) - 1; k[j] = k[j + 1], ++j);
    k[j] = COEF(coefs0, length0, i + iAL(k) / 2 + 1);
  }
  if (!(length & 1))
    *p++ = (float)k[K0];
  assert(p - coefs1 == length / 2  + 1);

  for (i = 0; i < n; ++i) for (j = phases - 1; j >= 0; --j, f1 = f0) {
    pos = (n - 1 - i) * phases + j;
    f0 = COEF(coefs1, length, pos) * (float)multiplier;
    coef(coefs, 1, n, j, 0, i) = (float)f0;
    coef(coefs, 1, n, j, 1, i) = (float)(f1 - f0);
  }
  free(coefs1);
}

#define _ sum += (b *x + a)*input[i], ++i;
#define a (coef(poly_fir_coefs_d, 1, POLY_FIR_LEN_D, phase, 0,i))
#define b (coef(poly_fir_coefs_d, 1, POLY_FIR_LEN_D, phase, 1,i))
static float poly_fir_coefs_d[POLY_FIR_LEN_D * PHASES_D * 2];

static float poly_fir1_d(float const * input, uint32_t frac)
{
  int i = 0, phase = (int)(frac >> (32 - PHASE_BITS_D));
  float sum = 0, x = (float)(frac << PHASE_BITS_D) * (float)(1 / MULT32);
  _ _ _ _ _  _ _ _ _ _  _ _ _ _ _  _ _ _ _ _
  assert(i == POLY_FIR_LEN_D);
  return (float)sum;
}
#undef a
#undef b
#define a (coef(poly_fir_coefs_u, 1, POLY_FIR_LEN_U, phase, 0,i))
#define b (coef(poly_fir_coefs_u, 1, POLY_FIR_LEN_U, phase, 1,i))
static float poly_fir_coefs_u[POLY_FIR_LEN_U * PHASES_U * 2];

static float poly_fir1_u(float const * input, uint32_t frac)
{
  int i = 0, phase = (int)(frac >> (32 - PHASE_BITS_U));
  float sum = 0, x = (float)(frac << PHASE_BITS_U) * (float)(1 / MULT32);
  _ _ _ _ _  _ _ _ _ _  _ _
  assert(i == POLY_FIR_LEN_U);
  return (float)sum;
}
#undef a
#undef b
#undef _

#define ADD_TO(x,y)           x.all += y.all
#define SUBTRACT_FROM(x,y)    x.all -= y.all
#define FRAC(x)               x.part.frac
#define INT(x)                x.part.integer

typedef struct {
  union {
    int64_t all;
#if HAVE_BIGENDIAN
    struct {int32_t integer; uint32_t frac;} part;
#else
    struct {uint32_t frac; int32_t integer;} part;
#endif
  } at, step, step_step;
  float const * input;
  int len, stage_num;
  bool is_d; /* true: downsampling at x2 rate; false: upsampling at 1x rate. */
  double step_mult;
} stream_t;

static int poly_fir_d(stream_t * s, float * output, int olen)
{
  int i;
  float const * input = s->input - POLY_FIR_LEN_D / 2 + 1;
  for (i = 0; i < olen && INT(s->at) < s->len; ++i) {
    output[i] = poly_fir1_d(input + INT(s->at), FRAC(s->at));
    ADD_TO(s->at, s->step);
    if (!(INT(s->at) < s->len)) {
      SUBTRACT_FROM(s->at, s->step);
      break;
    }
    output[++i] = poly_fir1_d(input + INT(s->at), FRAC(s->at));
    ADD_TO(s->at, s->step);
    ADD_TO(s->step, s->step_step);
  }
  return i;
}

static int poly_fir_fade_d(
    stream_t * s, float const * vol, int step, float * output, int olen)
{
  int i;
  float const * input = s->input - POLY_FIR_LEN_D / 2 + 1;
  for (i = 0; i < olen && INT(s->at) < s->len; ++i, vol += step) {
    output[i] += *vol * poly_fir1_d(input + INT(s->at), FRAC(s->at));
    ADD_TO(s->at, s->step);
    if (!(INT(s->at) < s->len)) {
      SUBTRACT_FROM(s->at, s->step);
      break;
    }
    output[++i] += *(vol += step) * poly_fir1_d(input + INT(s->at),FRAC(s->at));
    ADD_TO(s->at, s->step);
    ADD_TO(s->step, s->step_step);
  }
  return i;
}

static int poly_fir_u(stream_t * s, float * output, int olen)
{
  int i;
  float const * input = s->input - POLY_FIR_LEN_U / 2 + 1;
  for (i = 0; i < olen && INT(s->at) < s->len; ++i) {
    output[i] = poly_fir1_u(input + INT(s->at), FRAC(s->at));
    ADD_TO(s->at, s->step);
    ADD_TO(s->step, s->step_step);
  }
  return i;
}

static int poly_fir_fade_u(
    stream_t * s, float const * vol, int step, float * output, int olen)
{
  int i;
  float const * input = s->input - POLY_FIR_LEN_U / 2 + 1;
  for (i = 0; i < olen && INT(s->at) < s->len; i += 2, vol += step) {
    output[i] += *vol * poly_fir1_u(input + INT(s->at), FRAC(s->at));
    ADD_TO(s->at, s->step);
    ADD_TO(s->step, s->step_step);
  }
  return i;
}

#define shiftr(x,by) ((by) < 0? (x) << (-(by)) : (x) >> (by))
#define shiftl(x,by) shiftr(x,-(by))
#define stage_occupancy(s) (fifo_occupancy(&(s)->fifo) - 4*HALF_FIR_LEN_2)
#define stage_read_p(s) ((float *)fifo_read_ptr(&(s)->fifo) + 2*HALF_FIR_LEN_2)
#define stage_preload(s) memset(fifo_reserve(&(s)->fifo, (s)->preload), \
    0, sizeof(float) * (size_t)(s)->preload);

typedef struct {
  fifo_t fifo;
  double step_mult;
  int is_fast, x_fade_len, preload;
} stage_t;

typedef struct {
  int num_stages0, num_stages, flushing;
  int fade_len, slew_len, xfade, stage_inc, switch_stage_num;
  double new_io_ratio, default_io_ratio;
  stage_t * stages;
  fifo_t output_fifo;
  half_iir_t halfer;
  stream_t current, fadeout; /* Current/fade-in, fadeout streams. */
} rate_t;

static float fade_coefs[(2 << FADE_LEN_BITS) + 1];

static void vr_init(rate_t * p, double default_io_ratio, int num_stages, double mult)
{
  int i;
  assert(num_stages >= 0);
  memset(p, 0, sizeof(*p));

  p->num_stages0 = num_stages;
  p->num_stages = num_stages = max(num_stages, 1);
  p->stages = (stage_t *)calloc((unsigned)num_stages + 1, sizeof(*p->stages)) + 1;
  for (i = -1; i < p->num_stages; ++i) {
    stage_t * s = &p->stages[i];
    fifo_create(&s->fifo, sizeof(float));
    s->step_mult = 2 * MULT32 / shiftl(2, i);
    s->preload = i < 0? 0 : i == 0? 2 * HALF_FIR_LEN_2 : 3 * HALF_FIR_LEN_2 / 2;
    stage_preload(s);
    s->is_fast = true;
    lsx_debug("%-3i preload=%i", i, s->preload);
  }
  fifo_create(&p->output_fifo, sizeof(float));
  p->default_io_ratio = default_io_ratio;
  if (fade_coefs[0]==0) {
    for (i = 0; i < iAL(fade_coefs); ++i)
      fade_coefs[i] = (float)(.5 * (1 + cos(M_PI * i / (AL(fade_coefs) - 1))));
    prepare_coefs(poly_fir_coefs_u, POLY_FIR_LEN_U, PHASES0_U, PHASES_U, coefs0_u, mult);
    prepare_coefs(poly_fir_coefs_d, POLY_FIR_LEN_D, PHASES0_D, PHASES_D, coefs0_d, mult *.5);
  }
  assert(fade_coefs[0]);
}

static void enter_new_stage(rate_t * p, int occupancy0)
{
  p->current.len = shiftr(occupancy0, p->current.stage_num);
  p->current.input = stage_read_p(&p->stages[p->current.stage_num]);

  p->current.step_mult = p->stages[p->current.stage_num].step_mult;
  p->current.is_d = p->current.stage_num >= 0;
  if (p->current.is_d)
    p->current.step_mult *= .5;
}

static void set_step(stream_t * p, double io_ratio)
{
  p->step.all = (int64_t)(io_ratio * p->step_mult + .5);
}

static bool set_step_step(stream_t * p, double io_ratio, int slew_len)
{
  int64_t dif;
  int difi;
  stream_t tmp = *p;
  set_step(&tmp, io_ratio);
  dif = tmp.step.all - p->step.all;
  dif = dif < 0? dif - (slew_len >> 1) : dif + (slew_len >> 1);
  difi = (int)dif;   /* Try to avoid int64_t div. */
  p->step_step.all = difi == dif? difi / slew_len : dif / slew_len;
  return p->step_step.all != 0;
}

static void vr_set_io_ratio(rate_t * p, double io_ratio, size_t slew_len)
{
  assert(io_ratio > 0);
  if (slew_len) {
    if (!set_step_step(&p->current, io_ratio, p->slew_len = (int)slew_len))
      p->slew_len = 0, p->new_io_ratio = 0, p->fadeout.step_step.all = 0;
    else {
      p->new_io_ratio = io_ratio;
      if (p->fade_len)
        set_step_step(&p->fadeout, io_ratio, p->slew_len);
    }
  }
  else {
    if (p->default_io_ratio!=0) { /* Then this is the first call to this fn. */
      int octave = (int)floor(log(io_ratio) / M_LN2);
      p->current.stage_num = octave < 0? -1 : min(octave, p->num_stages0-1);
      enter_new_stage(p, 0);
    }
    else if (p->fade_len)
      set_step(&p->fadeout, io_ratio);
    set_step(&p->current, io_ratio);
    if (p->default_io_ratio!=0) FRAC(p->current.at) = FRAC(p->current.step) >> 1;
    p->default_io_ratio = 0;
  }
}

static bool do_input_stage(rate_t * p, int stage_num, int sign, int min_stage_num)
{
  int i = 0;
  float * dest;
  stage_t * s = &p->stages[stage_num];
  stage_t * s1 = &p->stages[stage_num - sign];
  float const * src = (float *)fifo_read_ptr(&s1->fifo) + HALF_FIR_LEN_2;
  int len = shiftr(fifo_occupancy(&s1->fifo) - HALF_FIR_LEN_2 * 2, sign);
  int already_done = fifo_occupancy(&s->fifo) - s->preload;
  if ((len -= already_done) <= 0)
    return false;
  src += shiftl(already_done, sign);

  dest = fifo_reserve(&s->fifo, len);
  if (stage_num < 0) for (; i < len; ++src)
    dest[i++] = double_fir0(src), dest[i++] = double_fir1(src);
  else {
    bool should_be_fast = p->stage_inc;
    if (!s->x_fade_len && stage_num == p->switch_stage_num) {
      p->switch_stage_num = 0;
      if (s->is_fast != should_be_fast) {
        s->x_fade_len = 1 << FADE_LEN_BITS, s->is_fast = should_be_fast, ++p->xfade;
        lsx_debug("xfade level %i, inc?=%i", stage_num, p->stage_inc);
      }
    }
    if (s->x_fade_len) {
      float const * vol1 = fade_coefs + (s->x_fade_len << 1);
      float const * vol2 = fade_coefs + (((1 << FADE_LEN_BITS) - s->x_fade_len) << 1);
      int n = min(len, s->x_fade_len);
      /*lsx_debug("xfade level %i, inc?=%i len=%i n=%i", stage_num, p->stage_inc, s->x_fade_len, n);*/
      if (should_be_fast)
        for (; i < n; vol2 += 2, vol1 -= 2, src += 2)
          dest[i++] = *vol1 * fast_half_fir(src) + *vol2 * half_fir(src);
      else for (; i < n; vol2 += 2, vol1 -= 2, src += 2)
        dest[i++] = *vol2 * fast_half_fir(src) + *vol1 * half_fir(src);
      s->x_fade_len -= n;
      p->xfade -= !s->x_fade_len;
    }
    if (stage_num < min_stage_num)
      for (; i < len; dest[i++] = fast_half_fir(src), src += 2);
    else for (; i < len; dest[i++] = half_fir(src), src += 2);
  }
  if (p->flushing > 0)
    stage_preload(s);
  return true;
}

static int vr_process(rate_t * p, int olen0)
{
  assert(p->num_stages > 0);
  if (p->default_io_ratio!=0)
    vr_set_io_ratio(p, p->default_io_ratio, 0);
  {
    float * output = fifo_reserve(&p->output_fifo, olen0);
    int j, odone0 = 0, min_stage_num = p->current.stage_num;
    int occupancy0, max_stage_num = min_stage_num;
    if (p->fade_len) {
      min_stage_num = min(min_stage_num, p->fadeout.stage_num);
      max_stage_num = max(max_stage_num, p->fadeout.stage_num);
    }

    for (j = min(min_stage_num, 0); j <= max_stage_num; ++j)
      if (j && !do_input_stage(p, j, j < 0? -1 : 1, min_stage_num))
        break;
    if (p->flushing > 0)
      p->flushing = -1;

    occupancy0 = shiftl(max(0,stage_occupancy(&p->stages[max_stage_num])), max_stage_num);
    p->current.len = shiftr(occupancy0, p->current.stage_num);
    p->current.input = stage_read_p(&p->stages[p->current.stage_num]);
    if (p->fade_len) {
      p->fadeout.len = shiftr(occupancy0, p->fadeout.stage_num);
      p->fadeout.input = stage_read_p(&p->stages[p->fadeout.stage_num]);
    }

    while (odone0 < olen0) {
      int odone, odone2, olen = olen0 - odone0, stage_dif = 0, shift;
      float buf[64 << 1];

      olen = min(olen, (int)(AL(buf) >> 1));
      if (p->slew_len)
        olen = min(olen, p->slew_len);
      else if (p->new_io_ratio!=0) {
        set_step(&p->current, p->new_io_ratio);
        set_step(&p->fadeout, p->new_io_ratio);
        p->fadeout.step_step.all = p->current.step_step.all = 0;
        p->new_io_ratio = 0;
      }
      if (!p->flushing && !p->fade_len && !p->xfade) {
        if (p->current.is_d) {
          if (INT(p->current.step) && FRAC(p->current.step))
            stage_dif = 1, ++max_stage_num;
          else if (!INT(p->current.step) && FRAC(p->current.step) < (1u << 31))
            stage_dif = -1, --min_stage_num;
        } else if (INT(p->current.step) > 1 && FRAC(p->current.step))
          stage_dif = 1, ++max_stage_num;
      }
      if (stage_dif) {
        int n = p->current.stage_num + stage_dif;
        if (n >= p->num_stages)
          --max_stage_num;
        else {
          p->stage_inc = stage_dif > 0;
          p->fadeout = p->current;
          p->current.stage_num += stage_dif;
          if (!p->stage_inc)
          p->switch_stage_num = p->current.stage_num;
          if ((p->current.stage_num < 0 && stage_dif < 0) ||
              (p->current.stage_num > 0 && stage_dif > 0)) {
            stage_t * s = &p->stages[p->current.stage_num];
            fifo_clear(&s->fifo);
            stage_preload(s);
            s->is_fast = false;
            do_input_stage(p, p->current.stage_num, stage_dif, p->current.stage_num);
          }
          if (p->current.stage_num > 0 && stage_dif < 0) {
            int idone = INT(p->current.at);
            stage_t * s = &p->stages[p->current.stage_num];
            fifo_trim_to(&s->fifo, 2 * HALF_FIR_LEN_2 + idone + (POLY_FIR_LEN_D >> 1));
            do_input_stage(p, p->current.stage_num, 1, p->current.stage_num);
          }
          enter_new_stage(p, occupancy0);
          shift = -stage_dif;
#define lshift(x,by) (x)=(by)>0?(x)<<(by):(x)>>-(by)
          lshift(p->current.at.all, shift);
          shift += p->fadeout.is_d - p->current.is_d;
          lshift(p->current.step.all, shift);
          lshift(p->current.step_step.all, shift);
          p->fade_len = AL(fade_coefs) - 1;
          lsx_debug("switch from stage %i to %i, x2 from %i to %i", p->fadeout.stage_num, p->current.stage_num, p->fadeout.is_d, p->current.is_d);
        }
      }

      if (p->fade_len) {
        float const * vol1 = fade_coefs + p->fade_len;
        float const * vol2 = fade_coefs + (iAL(fade_coefs) - 1 - p->fade_len);
        int olen2 = (olen = min(olen, p->fade_len >> 1)) << 1;

        /* x2 is more fine-grained so may fail to produce a pair of samples
         * where x1 would not (the x1 second sample is a zero so is always
         * available).  So do x2 first, then feed odone to the second one. */
        memset(buf, 0, sizeof(*buf) * (size_t)olen2);
        if (p->current.is_d && p->fadeout.is_d) {
          odone  = poly_fir_fade_d(&p->current, vol1,-1, buf, olen2);
          odone2 = poly_fir_fade_d(&p->fadeout, vol2, 1, buf, odone);
        } else if (p->current.is_d) {
          odone  = poly_fir_fade_d(&p->current, vol1,-1, buf, olen2);
          odone2 = poly_fir_fade_u(&p->fadeout, vol2, 2, buf, odone);
        } else {
          assert(p->fadeout.is_d);
          odone  = poly_fir_fade_d(&p->fadeout, vol2, 1, buf, olen2);
          odone2 = poly_fir_fade_u(&p->current, vol1,-2, buf, odone);
        }
        assert(odone == odone2);
        (void)odone2;
        p->fade_len -= odone;
        if (!p->fade_len) {
          if (p->stage_inc)
            p->switch_stage_num = min_stage_num++;
          else
            --max_stage_num;
        }
        half_iir(&p->halfer, &output[odone0], buf, odone >>= 1);
      }
      else if (p->current.is_d) {
        odone = poly_fir_d(&p->current, buf, olen << 1) >> 1;
        half_iir(&p->halfer, &output[odone0], buf, odone);
      }
      else {
        odone = poly_fir_u(&p->current, &output[odone0], olen);
        if (p->num_stages0)
          half_phase(&p->halfer, &output[odone0], odone);
      }
      odone0 += odone;
      if (p->slew_len)
        p->slew_len -= odone;
      if (odone != olen)
        break; /* Need more input. */
    } {
      int from = max(0, max_stage_num), to = min(0, min_stage_num);
      int i, idone = shiftr(INT(p->current.at), from - p->current.stage_num);
      INT(p->current.at) -= shiftl(idone, from - p->current.stage_num);
      if (p->fade_len)
        INT(p->fadeout.at) -= shiftl(idone, from - p->fadeout.stage_num);
      for (i = from; i >= to; --i, idone <<= 1)
        fifo_read(&p->stages[i].fifo, idone, NULL);
    }
    fifo_trim_by(&p->output_fifo, olen0 - odone0);
    return odone0;
  }
}

static float * vr_input(rate_t * p, float const * input, size_t n)
{
  return fifo_write(&p->stages[0].fifo, (int)n, input);
}

static float const * vr_output(rate_t * p, float * output, size_t * n)
{
  fifo_t * fifo = &p->output_fifo;
  if (1 || !p->num_stages0)
    return fifo_read(fifo, (int)(*n = min(*n, (size_t)fifo_occupancy(fifo))), output);
  else { /* Ignore this complication for now. */
    int const IIR_DELAY = 2;
    float * ptr = fifo_read_ptr(fifo);
    int olen = min((int)*n, max(0, fifo_occupancy(fifo) - IIR_DELAY));
    *n = (size_t)olen;
    if (output)
      memcpy(output, ptr + IIR_DELAY, *n * sizeof(*output));
    fifo_read(fifo, olen, NULL);
    return ptr + IIR_DELAY;
  }
}

static void vr_flush(rate_t * p)
{
  if (!p->flushing) {
    stage_preload(&p->stages[0]);
    ++p->flushing;
  }
}

static void vr_close(rate_t * p)
{
  int i;

  fifo_delete(&p->output_fifo);
  for (i = -1; i < p->num_stages; ++i) {
    stage_t * s = &p->stages[i];
    fifo_delete(&s->fifo);
  }
  free(p->stages - 1);
}

static double vr_delay(rate_t * p)
{
  return 100; /* TODO */
  (void)p;
}

static void vr_sizes(size_t * shared, size_t * channel)
{
  *shared = 0;
  *channel = sizeof(rate_t);
}

static char const * vr_create(void * channel, void * shared,double max_io_ratio,
    void * q_spec, void * r_spec, double scale)
{
  double x = max_io_ratio;
  int n;
  for (n = 0; x > 1; x *= .5, ++n);
  vr_init(channel, max_io_ratio, n, scale);
  return 0;
  (void)shared, (void)q_spec, (void)r_spec;
}

static char const * vr_id(void)
{
  return "vr32";
}

typedef void (* fn_t)(void);
fn_t _soxr_vr32_cb[] = {
  (fn_t)vr_input,
  (fn_t)vr_process,
  (fn_t)vr_output,
  (fn_t)vr_flush,
  (fn_t)vr_close,
  (fn_t)vr_delay,
  (fn_t)vr_sizes,
  (fn_t)vr_create,
  (fn_t)vr_set_io_ratio,
  (fn_t)vr_id,
};
