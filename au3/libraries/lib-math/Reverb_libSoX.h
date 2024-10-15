/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverb_libSoX.h
  Stereo reverberation effect from libSoX,
  adapted for Audacity

  Copyright (c) 2007-2013 robs@users.sourceforge.net
  Licence: LGPL v2.1
  Filter configuration based on freeverb by Jezar Wakefield.

**********************************************************************/

#include <cassert>
#include <cstring>
#include <cstdlib>
#ifdef __WXMSW__
   #define M_LN10   2.30258509299404568402 /* log_e 10 */
#else
   #include <cmath>
#endif
#include <algorithm>
#include <wx/types.h>
using std::min;
using std::max;

#define array_length(a) (sizeof(a)/sizeof(a[0]))
#define dB_to_linear(x) exp((x) * M_LN10 * 0.05)
#define midi_to_freq(n) (440 * pow(2,((n)-69)/12.))
#define FIFO_SIZE_T size_t
#define FIFO_MIN 0x4000
#define fifo_read_ptr(f) fifo_read(f, (FIFO_SIZE_T)0, NULL)
#define lsx_zalloc(var, n) var = (float *)calloc(n, sizeof(*var))
#define filter_advance(p) if (--(p)->ptr < (p)->buffer) (p)->ptr += (p)->size
#define filter_delete(p) free((p)->buffer)

typedef struct {
   char * data;
   size_t allocation;   /* Number of bytes allocated for data. */
   size_t item_size;    /* Size of each item in data */
   size_t begin;        /* Offset of the first byte to read. */
   size_t end;          /* 1 + Offset of the last byte to read. */
} fifo_t;

static void fifo_clear(fifo_t * f)
{
   f->end = f->begin = 0;
}

static void fifo_clear_and_zero(fifo_t* f)
{
   f->end = f->begin = 0;
   memset(f->data, 0, f->allocation);
}


static void * fifo_reserve(fifo_t * f, FIFO_SIZE_T n)
{
   n *= f->item_size;

   if (f->begin == f->end)
      fifo_clear(f);

   while (1) {
      if (f->end + n <= f->allocation) {
         void *p = f->data + f->end;

         f->end += n;
         return p;
      }
      if (f->begin > FIFO_MIN) {
         memmove(f->data, f->data + f->begin, f->end - f->begin);
         f->end -= f->begin;
         f->begin = 0;
         continue;
      }
      f->allocation += n;
      f->data = (char *)realloc(f->data, f->allocation);
   }
}

static void * fifo_write(fifo_t * f, FIFO_SIZE_T n, void const * data)
{
   void * s = fifo_reserve(f, n);
   if (data)
      memcpy(s, data, n * f->item_size);
   return s;
}

static void * fifo_read(fifo_t * f, FIFO_SIZE_T n, void * data)
{
   char * ret = f->data + f->begin;
   n *= f->item_size;
   if (n > (FIFO_SIZE_T)(f->end - f->begin))
      return NULL;
   if (data)
      memcpy(data, ret, (size_t)n);
   f->begin += n;
   return ret;
}

static void fifo_delete(fifo_t * f)
{
   free(f->data);
}

static void fifo_create(fifo_t * f, FIFO_SIZE_T item_size)
{
   f->item_size = item_size;
   f->allocation = FIFO_MIN;
   f->data = (char *)malloc(f->allocation);
   fifo_clear(f);
}

typedef struct {
   size_t  size;
   float   * buffer, * ptr;
   float   store;
} filter_t;

static float comb_process(filter_t * p,  /* gcc -O2 will inline this */
      float const * input, float const * feedback, float const * hf_damping)
{
   float output = *p->ptr;
   p->store = output + (p->store - output) * *hf_damping;
   *p->ptr = *input + p->store * *feedback;
   filter_advance(p);
   return output;
}

static float allpass_process(filter_t * p,  /* gcc -O2 will inline this */
      float const * input)
{
   float output = *p->ptr;
   *p->ptr = *input + output * .5;
   filter_advance(p);
   return output - *input;
}

typedef struct {double b0, b1, a1, i1, o1;} one_pole_t;

static float one_pole_process(one_pole_t * p, float i0)
{
   float o0 = i0*p->b0 + p->i1*p->b1 - p->o1*p->a1;
   p->i1 = i0;
   return p->o1 = o0;
}

static const size_t /* Filter delay lengths in samples (44100Hz sample-rate) */
   comb_lengths[] = {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617},
   allpass_lengths[] = {225, 341, 441, 556}, stereo_adjust = 12;

typedef struct {
   filter_t comb   [array_length(comb_lengths)];
   filter_t allpass[array_length(allpass_lengths)];
   one_pole_t one_pole[2];
} filter_array_t;

static void one_pole_init(filter_array_t* p, double rate,
    double fc_highpass, double fc_lowpass)
{
   { /* EQ: highpass */
      one_pole_t* q = &p->one_pole[0];
      q->a1 = -exp(-2 * M_PI * fc_highpass / rate);
      q->b0 = (1 - q->a1) / 2, q->b1 = -q->b0;
   }
   { /* EQ: lowpass */
      one_pole_t* q = &p->one_pole[1];
      q->a1 = -exp(-2 * M_PI * fc_lowpass / rate);
      q->b0 = 1 + q->a1, q->b1 = 0;
   }
}


static void filter_array_allocate(filter_array_t* p, double rate,
   double scale, double offset)
{
   size_t i;
   double r = rate * (1 / 44100.); /* Compensate for actual sample-rate */

   for (i = 0; i < array_length(comb_lengths); ++i)
   {
      filter_t* pcomb = &p->comb[i];
      pcomb->size = (size_t)(scale * r * (comb_lengths[i] + stereo_adjust * offset) + .5);
      pcomb->ptr = lsx_zalloc(pcomb->buffer, pcomb->size);
   }
   for (i = 0; i < array_length(allpass_lengths); ++i)
   {
      filter_t* pallpass = &p->allpass[i];
      pallpass->size = (size_t)(r * (allpass_lengths[i] + stereo_adjust * offset) + .5);
      pallpass->ptr = lsx_zalloc(pallpass->buffer, pallpass->size);
   }
}

static void filter_t_resize(filter_t* p, size_t newSize);

static void filter_array_init(filter_array_t* p, double rate,
   double scale, double offset)
{
   size_t i;
   double r = rate * (1 / 44100.); /* Compensate for actual sample-rate */

   for (i = 0; i < array_length(comb_lengths); ++i, offset = -offset)
   {
      filter_t* pcomb = &p->comb[i];
      size_t newSize = (size_t)(scale * r * (comb_lengths[i] + stereo_adjust * offset) + .5);
      filter_t_resize(pcomb, newSize);
   }
   for (i = 0; i < array_length(allpass_lengths); ++i, offset = -offset)
   {
      filter_t* pallpass = &p->allpass[i];
      size_t newSize = (size_t)(r * (allpass_lengths[i] + stereo_adjust * offset) + .5);
      filter_t_resize(pallpass, newSize);
   }
}



static void filter_array_create(filter_array_t * p, double rate,
      double scale, double offset)
{
   size_t i;
   double r = rate * (1 / 44100.); /* Compensate for actual sample-rate */

   for (i = 0; i < array_length(comb_lengths); ++i, offset = -offset)
   {
      filter_t * pcomb = &p->comb[i];
      pcomb->size = (size_t)(scale * r * (comb_lengths[i] + stereo_adjust * offset) + .5);
      pcomb->ptr = lsx_zalloc(pcomb->buffer, pcomb->size);
   }
   for (i = 0; i < array_length(allpass_lengths); ++i, offset = -offset)
   {
      filter_t * pallpass = &p->allpass[i];
      pallpass->size = (size_t)(r * (allpass_lengths[i] + stereo_adjust * offset) + .5);
      pallpass->ptr = lsx_zalloc(pallpass->buffer, pallpass->size);
   }
}

static void filter_array_process(filter_array_t * p,
      size_t length, float const * input, float * output,
      float const * feedback, float const * hf_damping, float const * gain)
{
   while (length--) {
      float out = 0, in = *input++;

      size_t i = array_length(comb_lengths) - 1;
      do out += comb_process(p->comb + i, &in, feedback, hf_damping);
      while (i--);

      i = array_length(allpass_lengths) - 1;
      do out = allpass_process(p->allpass + i, &out);
      while (i--);

      out = one_pole_process(&p->one_pole[0], out);
      out = one_pole_process(&p->one_pole[1], out);
      *output++ = out * *gain;
   }
}

static void filter_array_delete(filter_array_t * p)
{
   size_t i;

   for (i = 0; i < array_length(allpass_lengths); ++i)
      filter_delete(&p->allpass[i]);
   for (i = 0; i < array_length(comb_lengths); ++i)
      filter_delete(&p->comb[i]);
}

typedef struct {
   float feedback;
   float hf_damping;
   float gain;
   fifo_t input_fifo;
   filter_array_t chan[2];
   float * out[2];
   bool initializedWithZeroDepth;
} reverb_t;


// Resizes a filter, resetting its contents
static void filter_t_resize_resetting(filter_t* p, size_t newSize)
{
   memset(p->buffer, 0, newSize * sizeof(float));
   p->ptr = p->buffer;
   p->store = 0;
   p->size = newSize;
}


// Resizes a filter, trying to keep as much of its history as possible.
static void filter_t_resize_preserving(filter_t* p, size_t newSize)
{
   // Imagine we have this filter_t as input:
   //
   //                  ptr goes from right to left and when falling off the left side, it wraps back to the right;
   //    <--ptr--      it reads the most distant sample in the past, and after reading it will write a new sample.
   //        |         Imagine someone just recorded "A B C D E", and now we are going to read "A", but a resize request comes.
   //        v
   //  | C B A E D |
   //

   // Depending on the new requested size and where ptr is, we can have these cases:
   //
   //        v
   //  | C B A 0 E D |  bigger size: right-shift what is right of ptr,
   //                   and fill the resulting gap with zeros.
   //
   //                   (with variant: ptr is already at the old right edge - then shift nothing, just append zeros)
   //        v
   //  | C B A D |      smaller size, and ptr is within it (but not at the edge): left-shift what is to the right of ptr,
   //                   discarding samples.
   //
   //        V
   //  | C B A |        smaller size, and ptr would be at the new right edge; do nothing.
   //
   //      v
   //  | B A |          smaller size, and ptr would be beyond the new right edge: left-shift what is ahead of ptr,
   //                   AND move ptr to size-1

   assert(newSize > 0);

   const int    sizeDiff = ((ssize_t)newSize - (ssize_t)p->size);
   const size_t ptrPos   = (p->ptr  - p->buffer);

   const size_t numSamplesBehindPtr = (p->size - 1) - ptrPos;

   if (sizeDiff > 0)
   {
      // case: bigger size

      if (numSamplesBehindPtr > 0)
      {
         // right-shift what is right of ptr
         memcpy(p->ptr + 1 + sizeDiff, p->ptr + 1, numSamplesBehindPtr * sizeof(float));
      }

      // fill the created gap with zeros
      memset(p->ptr + 1, 0, sizeDiff * sizeof(float));
   }
   else if (sizeDiff < 0)
   {
      // case: smaller size

      if (ptrPos < newSize-1)
      {
         size_t lenOfBlockToShift = newSize - 1 - ptrPos;
         float* ptrToBlockToShift = p->buffer + p->size - lenOfBlockToShift;
         memcpy(p->ptr + 1, ptrToBlockToShift, lenOfBlockToShift * sizeof(float));
      }
      else if (ptrPos == newSize - 1)
      {
         // sub-case: ptr is at the new edge - no shifting to do, and ptr can stay where it is
      }
      else
      {
         // sub-case: ptr would be beyond the new edge
         // left-shift what is ahead of ptr and make ptr point to the new edge
         memcpy(p->buffer, p->ptr - newSize + 1, newSize * sizeof(float));
         p->ptr = p->buffer + newSize - 1;
      }
   }

   p->size = newSize;
}


static void filter_t_resize(filter_t* p, size_t newSize)
{
   // Choose your resize method.
   //
   // When moving the Room Size slider:
   //
   // - using resize_resetting, you will not hear previous reverberations,
   //   but at the same time you will not hear sound artifacts
   //
   // - using resize_preserving, you will hear previous reverberations,
   //   but sometimes you might hear sound artifacts

#if 1
   filter_t_resize_resetting(p, newSize);
#else
   filter_t_resize_preserving(p, newSize);
#endif

}


static void reverb_allocate(reverb_t* p, double sample_rate_Hz, size_t buffer_size, float** out)
{
   memset(p, 0, sizeof(*p));

   // Input queue
   fifo_create(&p->input_fifo, sizeof(float));

   // Outputs
   out[0] = lsx_zalloc(p->out[0], buffer_size);
   out[1] = lsx_zalloc(p->out[1], buffer_size);

   // Allpass & Comb filters
   // pass the numbers that will cause the maximum possible allocation of the
   // buffer just once, even if it is in excess of what is required by the
   // settings passed in initialization after the allocation
   filter_array_allocate(p->chan + 0, sample_rate_Hz, 1.0, 0.0);
   filter_array_allocate(p->chan + 1, sample_rate_Hz, 1.0, 1.0);
}

// Some of the params can be set without having to re-init the input fifo
// or the comb/allpass filters.
static void reverb_set_simple_params
(
   reverb_t* p,
   double sample_rate_Hz,
   double wet_gain_dB,
   double reverberance,
   double hf_damping,
   double tone_low,       /* % */
   double tone_high       /* % */
)
{
   // Feedback, Damping, Gain
   double a = -1 / log(1 - /**/.3 /**/);           /* Set minimum feedback */
   double b = 100 / (log(1 - /**/.98/**/) * a + 1);  /* Set maximum feedback */

   p->feedback = 1 - exp((reverberance - b) / (a * b));
   p->hf_damping = hf_damping / 100 * .3 + .2;
   p->gain = dB_to_linear(wet_gain_dB) * .015;


   // LP-HP Filters
   double fc_highpass = midi_to_freq(72 - tone_low / 100 * 48);
   double fc_lowpass = midi_to_freq(72 + tone_high / 100 * 48);
   one_pole_init(&p->chan[0], sample_rate_Hz, fc_highpass, fc_lowpass);
   one_pole_init(&p->chan[1], sample_rate_Hz, fc_highpass, fc_lowpass);
}


static void reverb_init
(
   reverb_t* p,
   double sample_rate_Hz,
   double wet_gain_dB,
   double room_scale,     /* % */
   double reverberance,
   double hf_damping,
   double pre_delay_ms,
   double stereo_depth,
   double tone_low,       /* % */
   double tone_high       /* % */
)
{
   // Input queue
   fifo_clear_and_zero(&p->input_fifo);
   size_t delay = pre_delay_ms / 1000 * sample_rate_Hz + .5;
   memset(fifo_write(&p->input_fifo, delay, 0), 0, delay * sizeof(float));

   reverb_set_simple_params(p, sample_rate_Hz, wet_gain_dB, reverberance, hf_damping, tone_low, tone_high);

   // Allpass & Comb filters
   double scale = room_scale / 100 * .9 + .1;
   double depth = stereo_depth / 100;
   for (size_t i = 0; i <= ceil(depth); ++i)
   {
      filter_array_init(p->chan + i, sample_rate_Hz, scale, i * depth);
   }

   // Remember if stereo depth was set to 0, so we do not have to process twice
   p->initializedWithZeroDepth = (stereo_depth == 0.0);

}


static void reverb_create(reverb_t * p, double sample_rate_Hz,
      double wet_gain_dB,
      double room_scale,     /* % */
      double reverberance,   /* % */
      double hf_damping,     /* % */
      double pre_delay_ms,
      double stereo_depth,
      double tone_low,       /* % */
      double tone_high,      /* % */
      size_t buffer_size,
      float * * out)
{
   reverb_allocate(p, sample_rate_Hz, buffer_size, out);

   reverb_init(p, sample_rate_Hz, wet_gain_dB, room_scale, reverberance, hf_damping, pre_delay_ms, stereo_depth, tone_low, tone_high);
}

static void reverb_process(reverb_t * p, size_t length)
{
   filter_array_process(p->chan, length, (float *) fifo_read_ptr(&p->input_fifo), p->out[0], &p->feedback, &p->hf_damping, &p->gain);

   if (!p->initializedWithZeroDepth)
      filter_array_process(p->chan + 1, length, (float*)fifo_read_ptr(&p->input_fifo), p->out[1], &p->feedback, &p->hf_damping, &p->gain);

   fifo_read(&p->input_fifo, length, NULL);
}

static void reverb_delete(reverb_t * p)
{
   size_t i;
   for (i = 0; i < 2 && p->out[i]; ++i) {
      free(p->out[i]);
      filter_array_delete(p->chan + i);
   }
   fifo_delete(&p->input_fifo);
}

static void reverb_clear(reverb_t* p)
{
   for (size_t c = 0; c < 2; c++)
   {
      auto& chn = p->chan[c];

      chn.one_pole[0].i1 = 0.0;
      chn.one_pole[0].o1 = 0.0;

      chn.one_pole[1].i1 = 0.0;
      chn.one_pole[1].o1 = 0.0;

      for (size_t combIndex = 0; combIndex < array_length(comb_lengths); combIndex++)
      {
         auto& comb = chn.comb[combIndex];

         memset(comb.buffer, 0, comb.size * sizeof(float));

         comb.store = 0.0f;
      }

      for (size_t allpassIndex = 0; allpassIndex < array_length(allpass_lengths); allpassIndex++)
      {
         auto& allpass = chn.allpass[allpassIndex];

         memset(allpass.buffer, 0, allpass.size * sizeof(float));

         allpass.store = 0.0f;
      }
   } // loop on channels

   fifo_clear( &(p->input_fifo) );
}

