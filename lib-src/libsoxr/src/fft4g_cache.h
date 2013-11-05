/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

static int * LSX_FFT_BR;
static DFT_FLOAT * LSX_FFT_SC;
static int FFT_LEN = -1;
static ccrw2_t FFT_CACHE_CCRW;

void LSX_INIT_FFT_CACHE(void)
{
  if (FFT_LEN >= 0)
    return;
  assert(LSX_FFT_BR == NULL);
  assert(LSX_FFT_SC == NULL);
  assert(FFT_LEN == -1);
  ccrw2_init(FFT_CACHE_CCRW);
  FFT_LEN = 0;
}

void LSX_CLEAR_FFT_CACHE(void)
{
  assert(FFT_LEN >= 0);
  ccrw2_clear(FFT_CACHE_CCRW);
  free(LSX_FFT_BR);
  free(LSX_FFT_SC);
  LSX_FFT_SC = NULL;
  LSX_FFT_BR = NULL;
  FFT_LEN = -1;
}

static bool UPDATE_FFT_CACHE(int len)
{
  LSX_INIT_FFT_CACHE();
  assert(lsx_is_power_of_2(len));
  assert(FFT_LEN >= 0);
  ccrw2_become_reader(FFT_CACHE_CCRW);
  if (len > FFT_LEN) {
    ccrw2_cease_reading(FFT_CACHE_CCRW);
    ccrw2_become_writer(FFT_CACHE_CCRW);
    if (len > FFT_LEN) {
      int old_n = FFT_LEN;
      FFT_LEN = len;
      LSX_FFT_BR = realloc(LSX_FFT_BR, dft_br_len(FFT_LEN) * sizeof(*LSX_FFT_BR));
      LSX_FFT_SC = realloc(LSX_FFT_SC, dft_sc_len(FFT_LEN) * sizeof(*LSX_FFT_SC));
      if (!old_n) {
        LSX_FFT_BR[0] = 0;
#if SOXR_LIB
        atexit(LSX_CLEAR_FFT_CACHE);
#endif
      }
      return true;
    }
    ccrw2_cease_writing(FFT_CACHE_CCRW);
    ccrw2_become_reader(FFT_CACHE_CCRW);
  }
  return false;
}

static void DONE_WITH_FFT_CACHE(bool is_writer)
{
  if (is_writer)
    ccrw2_cease_writing(FFT_CACHE_CCRW);
  else ccrw2_cease_reading(FFT_CACHE_CCRW);
}

void LSX_SAFE_RDFT(int len, int type, DFT_FLOAT * d)
{
  bool is_writer = UPDATE_FFT_CACHE(len);
  LSX_RDFT(len, type, d, LSX_FFT_BR, LSX_FFT_SC);
  DONE_WITH_FFT_CACHE(is_writer);
}

void LSX_SAFE_CDFT(int len, int type, DFT_FLOAT * d)
{
  bool is_writer = UPDATE_FFT_CACHE(len);
  LSX_CDFT(len, type, d, LSX_FFT_BR, LSX_FFT_SC);
  DONE_WITH_FFT_CACHE(is_writer);
}

#undef UPDATE_FFT_CACHE
#undef LSX_SAFE_RDFT
#undef LSX_SAFE_CDFT
#undef LSX_RDFT
#undef LSX_INIT_FFT_CACHE
#undef LSX_FFT_SC
#undef LSX_FFT_BR
#undef LSX_CLEAR_FFT_CACHE
#undef LSX_CDFT
#undef FFT_LEN
#undef FFT_CACHE_CCRW
#undef DONE_WITH_FFT_CACHE
#undef DFT_FLOAT
