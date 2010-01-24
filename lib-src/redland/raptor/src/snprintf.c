/*
 * This file is in the Public Domain
 *
 * Based on code from Public Domain snprintf.c from mutt
 *   http://dev.mutt.org/hg/mutt/file/55cd4cb611d9/snprintf.c
 * Tue Aug 08 22:49:12 2006 +0000
 * 
 */

#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#include "raptor.h"
#include "raptor_internal.h"

#include <float.h>
#define __USE_ISOC99 1
#include <math.h>

#ifndef HAVE_ROUND
/* round (C99): round x to the nearest integer, away from zero */
#define round(x) (((x) < 0) ? (long)((x)-0.5) : (long)((x)+0.5))
#endif

#ifndef HAVE_TRUNC
/* trunc (C99): round x to the nearest integer, towards zero */
#define trunc(x) (((x) < 0) ? ceil((x)) : floor((x)))
#endif

/* Convert a double to xsd:decimal representation.
 * Returned is a pointer to the first character of the number
 * in buffer (don't free it).
 */
char*
raptor_format_float(char *buffer, size_t *currlen, size_t maxlen,
                    double fvalue, unsigned int min, unsigned int max,
                    int flags)
{
  /* DBL_EPSILON = 52 digits */
  #define FRAC_MAX_LEN 52

  double ufvalue;
  double intpart;
  double fracpart = 0;
  double frac;
  double frac_delta = 10;
  double mod_10;
  size_t exp_len;
  size_t frac_len = 0;
  size_t idx;

  if (max < min)
    max = min;
  
  /* index to the last char */
  idx = maxlen - 1;

  buffer[idx--] = '\0';
  
  ufvalue = fabs (fvalue);
  intpart = round(ufvalue);

  /* We "cheat" by converting the fractional part to integer by
   * multiplying by a factor of 10
   */


  frac = (ufvalue - intpart);
  
  for (exp_len=0; exp_len <= max; ++exp_len) {
    frac *= 10;

    mod_10 = trunc(fmod(trunc(frac), 10));
    
    if (fabs(frac_delta - (fracpart / pow(10, exp_len))) < (DBL_EPSILON * 2.0)) {
      break;
    }
    
    frac_delta = fracpart / pow(10, exp_len);

    /* Only "append" (numerically) if digit is not a zero */
    if (mod_10 > 0 && mod_10 < 10) {
        fracpart = round(frac);
        frac_len = exp_len;
    }
  }
  
  if (frac_len < min) {
    buffer[idx--] = '0';
  } else {
    /* Convert/write fractional part (right to left) */
    do {
      mod_10 = fmod(trunc(fracpart), 10);
      --frac_len;
      
      buffer[idx--] = "0123456789"[(unsigned)mod_10];
      fracpart /= 10;

    } while(fracpart > 1 && (frac_len + 1) > 0);
  }

  buffer[idx--] = '.';

  /* Convert/write integer part (right to left) */
  do {
    buffer[idx--] = "0123456789"[(int)fmod(intpart, 10)];
    intpart /= 10;
  } while(round(intpart));
  
  /* Write a sign, if requested */
  if(fvalue < 0)
    buffer[idx--] = '-';
  else if(flags)
    buffer[idx--] = '+';
  
  *currlen = maxlen - idx - 2;
  return buffer + idx + 1;
}
