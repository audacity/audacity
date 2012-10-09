/* SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at
 * your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

/* Wrapper mostly compatible with `libsamplerate'.
 * (Libsoxr's native API can be found in soxr.h).  */

#if !defined SAMPLERATE_H
#define SAMPLERATE_H
#if defined __cplusplus
  extern "C" {
#endif

#if defined SOXR_DLL
  #if defined soxr_lsr_EXPORTS
    #define _ __declspec(dllexport)
  #else
    #define _ __declspec(dllimport)
  #endif
#else
  #define _
#endif

typedef float   SRC_SAMPLE;
#if !defined SOXR_LIB
enum SRC_SRCTYPE{SRC_SINC_BEST_QUALITY,SRC_SINC_MEDIUM_QUALITY,SRC_SINC_FASTEST,
                    SRC_ZERO_ORDER_HOLD, SRC_LINEAR};
typedef int     SRC_SRCTYPE;
typedef int     SRC_ERROR;
typedef long    (* src_callback_t)(void *, SRC_SAMPLE * *);
typedef struct  SRC_STATE SRC_STATE;
typedef struct  SRC_DATA {
  SRC_SAMPLE    * data_in, * data_out;
  long          input_frames, output_frames;
  long          input_frames_used, output_frames_gen;
  int           end_of_input;
  double        src_ratio;
} SRC_DATA;
#endif
_ SRC_STATE *   src_new(SRC_SRCTYPE, int num_channels, SRC_ERROR *);
_ SRC_ERROR     src_process  (SRC_STATE *, SRC_DATA *);
_ SRC_ERROR     src_set_ratio(SRC_STATE *, double);
_ SRC_ERROR     src_reset    (SRC_STATE *);
_ SRC_ERROR     src_error    (SRC_STATE *);
_ SRC_STATE *   src_delete   (SRC_STATE *);
_ SRC_STATE *   src_callback_new(
                    src_callback_t, SRC_SRCTYPE, int, SRC_ERROR *, void *);
_ long          src_callback_read(
                    SRC_STATE *, double src_ratio, long, SRC_SAMPLE *);
_ SRC_ERROR     src_simple(SRC_DATA *, SRC_SRCTYPE, int);
_ char const *  src_get_name(SRC_SRCTYPE);
_ char const *  src_get_description(SRC_SRCTYPE);
_ char const *  src_get_version(void);
_ char const *  src_strerror(SRC_ERROR);
_ int           src_is_valid_ratio(double);
_ void          src_short_to_float_array(short const *, float *, int);
_ void          src_float_to_short_array(float const *, short *, int);
_ void          src_int_to_float_array(int const *, float *, int);
_ void          src_float_to_int_array(float const *, int *, int);

#undef _
#if defined __cplusplus
  }
#endif
#endif
