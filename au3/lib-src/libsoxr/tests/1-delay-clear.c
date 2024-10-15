/* SoX Resampler Library      Copyright (c) 2007-15 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Test 1: exercises soxr_delay and soxr_clear */

#ifdef NDEBUG /* N.B. assert used with active statements so enable always. */
#undef NDEBUG /* Must undef above assert.h or other that might include it. */
#endif

#include <soxr.h>
#include "../examples/examples-common.h"

#define ranqd1(x) ((x) = 1664525 * (x) + 1013904223) /* int32_t x */
#define franqd1(x) (float)(ranqd1(x) * (1. / (65536. * 32768.))) /* [-1,1) */

#define irate 9600
#define orate 4410

int main(int argc, char const * arg[])
{
  soxr_error_t error;
  int32_t ran = 0;
  int j;

  soxr_t soxr = soxr_create(irate, orate, 1, &error, NULL, NULL, NULL);
  assert(!error);

  for (j=0; j<2; ++j) {
    float ibuf[irate], out[orate+2], obuf[orate+2], * ibuf1 = ibuf;
    size_t ilen = AL(ibuf)-1, olen = AL(obuf), i, odone = 0, odone0, odone1=0;
    soxr_quality_spec_t  q_spec = soxr_quality_spec(SOXR_HQ, 0);

    for (i=0; i<irate; ibuf[i++] = franqd1(ran));

    error = soxr_oneshot(irate, orate, 1, ibuf, ilen, NULL,
        out, AL(out), &odone0, NULL, &q_spec, NULL);
    assert(!error);
    assert(odone0==orate);

    for (i=0; ilen || odone1; ++i) {
      double out_samples = (double)orate / irate * (double)ilen;
      double delayed_samples = soxr_delay(soxr);
      unsigned max_out_samples = (unsigned)(out_samples + delayed_samples + .5);
      assert(delayed_samples >= 0);
      fprintf(stderr, "%5u %5u %5u\n",
          (unsigned)ilen, max_out_samples, (unsigned)odone);
      assert(max_out_samples+odone==odone0);
      error = soxr_process(soxr, ibuf1, ilen, NULL, obuf+odone, olen, &odone1);
      assert(!error);
      odone += odone1;
      ibuf1 = NULL, ilen = 0;
      olen = min(100, AL(obuf)-odone);
    }
    assert(odone==odone0);

    for (i=0; i<odone && out[i]==obuf[i]; ++i);
    assert(i==odone);

    soxr_clear(soxr);
  }
  soxr_delete(soxr);

  return 0 * argc * !arg;
}
