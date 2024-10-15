/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 5:  Variable-rate resampling.  A test signal (held in a buffer) is
 * resampled over a wide range of octaves.  Resampled data is sent to stdout as
 * raw, float32 samples.  Choices of 2 test-signals and of 2 ways of varying
 * the sample-rate are combined in a command-line option:
 *
 * Usage: ./5-variable-rate [0|1|2|3]
 */

#include <soxr.h>
#include "examples-common.h"

#define OCTAVES  5       /* Resampling range. ± */
#define OLEN     16      /* Output length in seconds. */
#define FS       44100   /* Output sampling rate in Hz. */

/* For output pos in [0,1], returns an ioratio in the 2^±OCTAVES range: */
static double ioratio(double pos, int fm)
{
  if (fm) /* fm: non-0 for a fast-changing ioratio, 0 for a slow sweep. */
    pos = .5 - cos(pos * 2 * M_PI) * .4 + sin(pos * OLEN * 20 * M_PI) * .05;
  return pow(2, 2 * OCTAVES * pos - OCTAVES);
}

int main(int argc, char *arg[])
{
  int opt = argc <= 1? 2 : (atoi(arg[1]) & 3), saw = opt & 1, fm = opt & 2;
  float ibuf[10 << OCTAVES], obuf[AL(ibuf)];
  int i, wl = 2 << OCTAVES;
  size_t ilen = AL(ibuf), need_input = 1, written;
  size_t odone, total_odone, total_olen = OLEN * FS;
  size_t olen1 = fm? 10 : AL(obuf); /* Small block-len if fast-changing ratio */
  soxr_error_t error;

  /* When creating a var-rate resampler, q_spec must be set as follows: */
  soxr_quality_spec_t q_spec = soxr_quality_spec(SOXR_HQ, SOXR_VR);

  /* The ratio of the given input rate and output rates must equate to the
   * maximum I/O ratio that will be used: */
  soxr_t soxr = soxr_create(1 << OCTAVES, 1, 1, &error, NULL, &q_spec, NULL);

  if (!error) {
    USE_STD_STDIO;

    /* Generate input signal, sine or saw, with wave-length = wl: */
    for (i = 0; i < (int)ilen; ++i)
      ibuf[i] = (float)(saw? (i%wl)/(wl-1.)-.5 : .9 * sin(2 * M_PI * i / wl));

    /* Set the initial resampling ratio (N.B. 3rd parameter = 0): */
    soxr_set_io_ratio(soxr, ioratio(0, fm), 0);

    /* Resample in blocks of size olen1: */
    for (total_odone = 0; !error && total_odone < total_olen;) {

      /* The last block might be shorter: */
      size_t block_len = min(olen1, total_olen - total_odone);

      /* Determine the position in [0,1] of the end of the current block: */
      double pos = (double)(total_odone + block_len) / (double)total_olen;

      /* Calculate an ioratio for this position and instruct the resampler to
       * move smoothly to the new value, over the course of outputting the next
       * 'block_len' samples (or give 0 for an instant change instead): */
      soxr_set_io_ratio(soxr, ioratio(pos, fm), block_len);

      /* Output the block of samples, supplying input samples as needed: */
      do {
        size_t len = need_input? ilen : 0;
        error = soxr_process(soxr, ibuf, len, NULL, obuf, block_len, &odone);
        written = fwrite(obuf, sizeof(float), odone, stdout);

        /* Update counters for the current block and for the total length: */
        block_len -= odone;
        total_odone += odone;

        /* If soxr_process did not provide the complete block, we must call it
         * again, supplying more input samples: */
        need_input = block_len != 0;

      } while (need_input && !error && written == odone);

      /* Now that the block for the current ioratio is complete, go back
       * round the main `for' loop in order to process the next block. */
    }
    soxr_delete(soxr);
  }
                                                              /* Diagnostics: */
  fprintf(stderr, "%-26s %s; I/O: %s\n", arg[0], soxr_strerror(error),
      ferror(stdin) || ferror(stdout)? strerror(errno) : "no error");
  return !!error;
}
