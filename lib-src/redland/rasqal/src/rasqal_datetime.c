/*
 * rasqal_datetime.c - Rasqal XSD dateTime
 *
 * Copyright (C) 2007-2008, David Beckett http://www.dajobe.org/
 *
 * Contributions:
 *   Copyright (C) 2007, Lauri Aalto <laalto@iki.fi>
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */

#ifdef HAVE_CONFIG_H
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <stdarg.h>
#include <limits.h>

#include "rasqal.h"
#include "rasqal_internal.h"

/* Local definitions */
 
/**
 * rasqal_xsd_datetime:
 *
 * INTERNAL - XML schema dateTime datatype
 *
 * Signed types are required for normalization process where a value
 * can be negative temporarily.
 */
typedef struct {
  signed int year;
  /* the following fields are integer values not characters */
  unsigned char month;
  unsigned char day;
  signed char hour;
  signed char minute;
  signed char second;
  /* second_frac is a string of 1-3 length (+1 for NUL)
   * supports only up to milliseconds
   */
  char second_frac[3+1];
  /* have_tz is an integer flag: non-0 if 'Z'ulu timezone is present */
  char have_tz;
}  rasqal_xsd_datetime;


static int rasqal_xsd_datetime_parse_and_normalize(const unsigned char *datetime_string, rasqal_xsd_datetime *result);
static unsigned char *rasqal_xsd_datetime_to_string(const rasqal_xsd_datetime *dt);
static unsigned int days_per_month(int month, int year);


#ifndef ISNUM
#define ISNUM(c) ((c)>='0'&&(c)<='9')
#endif


/**
 * rasqal_xsd_datetime_normalize:
 * @datetime: date time
 *
 * INTERNAl - Normalize a date time into the allowed range
 *
 * Return value: zero on success, non zero on failure.
 */
static int
rasqal_xsd_datetime_normalize(rasqal_xsd_datetime *datetime)
{
  int t;
  
  /* second & second parts: no need to normalize as they are not
   * touched after range check
   */
  
  /* minute */
  if(datetime->minute < 0) {
    datetime->minute += 60;
    datetime->hour--;
  } else if(datetime->minute > 59) {
    datetime->minute -= 60;
    datetime->hour++;
  }
  
  /* hour */
  if(datetime->hour < 0) {
    datetime->hour += 24;
    datetime->day--;
  } else if(datetime->hour > 23) {
    datetime->hour -= 24;
    datetime->day++;
  }
  
  /* day */
  if(datetime->day < 1) {
    int y2;
    t = --datetime->month;
    /* going back beyond year boundary? */
    if(!t) {
      t = 12;
      y2 = datetime->year-1;
    } else
      y2 = datetime->year;
    datetime->day += days_per_month(t, y2);
  } else if(datetime->day > (t=days_per_month(datetime->month, datetime->year))) {
    datetime->day -= t;
    datetime->month++;
  }
  
  /* month & year */
  if(datetime->month < 1) {
    datetime->month += 12;
    datetime->year--;
    /* there is no year 0 - go backwards to year -1 */
    if(!datetime->year)
      datetime->year--;
  } else if(datetime->month > 12) {
    datetime->month -= 12;
    datetime->year++;
    /* there is no year 0 - go forwards to year 1 */
    if(!datetime->year)
      datetime->year++;
  }

  /* success */
  return 0;
}


/**
 * rasqal_xsd_datetime_parse_and_normalize:
 * @datetime_string: xsd:dateTime as lexical form string
 * @result: target struct for holding dateTime components
 *
 * INTERNAL - Parse a xsd:dateTime string to a normalized #rasqal_xsd_datetime struct.
 *
 * http://www.w3.org/TR/xmlschema-2/#dt-dateTime
 *
 * "The lexical space of dateTime consists of finite-length sequences of
 * characters of the form:
 * '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?,
 * where
 *
 * * '-'? yyyy is a four-or-more digit optionally negative-signed numeral that
 *   represents the year; if more than four digits, leading zeros are
 *   prohibited, and '0000' is prohibited (see the Note above (3.2.7); also
 *   note that a plus sign is not permitted);
 * * the remaining '-'s are separators between parts of the date portion;
 * * the first mm is a two-digit numeral that represents the month;
 * * dd is a two-digit numeral that represents the day;
 * * 'T' is a separator indicating that time-of-day follows;
 * * hh is a two-digit numeral that represents the hour; '24' is permitted if
 *   the minutes and seconds represented are zero, and the dateTime value so
 *   represented is the first instant of the following day (the hour property
 *   of a dateTime object in the value space cannot have a value greater
 *   than 23);
 * * ':' is a separator between parts of the time-of-day portion;
 * * the second mm is a two-digit numeral that represents the minute;
 * * ss is a two-integer-digit numeral that represents the whole seconds;
 * * '.' s+ (if present) represents the fractional seconds;
 * * zzzzzz (if present) represents the timezone"
 *
 * Return value: zero on success, non zero on failure.
 */
int
rasqal_xsd_datetime_parse_and_normalize(const unsigned char *datetime_string,
                                        rasqal_xsd_datetime *result)
{
  const char *p, *q; 
  char b[16];
  unsigned int l, t, t2, is_neg;
  unsigned long u;

  if(!datetime_string || !result)
    return -1;
  
  p=(const char *)datetime_string;
  is_neg=0;

  /* Parse year */
  
  /* negative years permitted */
  if(*p == '-') {
    is_neg=1;
    p++;
  }
  for(q=p; ISNUM(*p); p++)
    ;
  l=p-q;
  
  /* error if
     - less than 4 digits in year
     - more than 4 digits && leading zeros
     - '-' does not follow numbers
   */
  if(l < 4 || (l > 4 && *q=='0') || *p!='-')
    return -1;

  l=(l < sizeof(b)-1 ? l : sizeof(b)-1);
  strncpy(b, q, l);
  b[l]=0; /* ensure nul termination */
  u=strtoul(b, 0, 10);
  
  /* year "0000" not permitted
   * restrict to signed int range
   * >= instead of > to allow for +-1 year adjustment in normalization
   * (however, these +-INT_MAX years cannot be parsed back in if
   * converted to string)
   */
  if(!u || u >= INT_MAX)
    return -1;
    
  result->year=is_neg ? -(int)u : (int)u;

  /* parse month */
  
  for(q=++p; ISNUM(*p); p++)
    ;
  l=p-q;
  
  /* error if month is not 2 digits or '-' is not the separator */
  if(l != 2 || *p!='-')
    return -2;
  
  t=(*q++-'0')*10;
  t+=*q-'0';
  
  /* month must be 1..12 */
  if(t < 1 || t > 12)
    return -2;
  
  result->month=t;
  
  /* parse day */
  
  for(q=++p; ISNUM(*p); p++)
    ;
  l=p-q;
  
  /* error if day is not 2 digits or 'T' is not the separator */
  if(l != 2 || *p != 'T')
    return -3;
  
  t=(*q++-'0')*10;
  t+=*q-'0';
  
  /* day must be 1..days_per_month */
  if(t < 1 || t > days_per_month(result->month, result->year))
    return -3;
    
  result->day=t;
  
  /* parse hour */
  
  for(q=++p; ISNUM(*p); p++)
    ;
  l=p-q;
  
  /* error if hour is not 2 digits or ':' is not the separator */
  if(l != 2 || *p != ':')
    return -4;
   
  t=(*q++-'0')*10;
  t+=*q-'0';
 
  /* hour must be 0..24 - will handle special case 24 later
   * (no need to check for < 0)
   */
  if(t > 24)
    return -4;
    
  result->hour=t;
 
  /* parse minute */

  for(q=++p; ISNUM(*p); p++)
    ;
  l=p-q;
  
  /* error if minute is not 2 digits or ':' is not the separator */
  if(l != 2 || *p != ':')
    return -5;
   
  t=(*q++-'0')*10;
  t+=*q-'0';
 
  /* minute must be 0..59
   * (no need to check for < 0)
   */
  if(t > 59)
    return -5;

  result->minute=t;
  
  /* parse second whole part */
  
  for(q=++p; ISNUM(*p); p++)
    ;
  l=p-q;
  
  /* error if second is not 2 digits or separator is not 
   * '.' (second fraction)
   * 'Z' (utc)
   * '+' or '-' (timezone offset)
   * nul (end of string - second fraction and timezone are optional)
   */
  if(l != 2 || (*p && *p != '.' && *p != 'Z' && *p != '+' && *p != '-'))
    return -6;
    
  t=(*q++-'0')*10;
  t+=*q-'0';

  /* second must be 0..59
  * (no need to check for < 0)
  */
  if(t > 59)
    return -6;

  result->second=t;

  /* now that we have hour, minute and second, we can check
   * if hour == 24 -> only 24:00:00 permitted (normalized later)
   */
  if(result->hour==24 && (result->minute || result->second))
    return -7;
  
  /* parse fraction seconds if any */
  result->second_frac[0]=0;
  if(*p == '.') {
    for(q=++p; ISNUM(*p); p++)
      ;

    /* ignore trailing zeros */
    while(*--p == '0')
      ;
    p++;

    if(!(*q=='0' && q==p)) {
      /* allow ".0" */
      l=p-q;
      /* support only to milliseconds with truncation */
      if(l > sizeof(result->second_frac)-1)
        l=sizeof(result->second_frac)-1;

      if(l<1) /* need at least 1 num */
        return -8;

      for(t2=0; t2 < l; ++t2)
        result->second_frac[t2]=*q++;

      result->second_frac[l]=0;
    }

    /* skip ignored trailing zeros */
    while(*p == '0')
      p++;
  }
  
  /* parse & adjust timezone offset */
  /* result is normalized later */
  result->have_tz=0;
  if(*p) {
    if(*p == 'Z') {
      /* utc timezone - no need to adjust */
      p++;
      result->have_tz=1;
    } else if(*p=='+' || *p=='-') {
      /* work out timezone offsets */
      is_neg=*p == '-';
     
      /* timezone hours */
      for(q=++p; ISNUM(*p); p++)
        ;
      l=p-q;
      if(l != 2 || *p!=':')
        return -9;

      t2=(*q++ - '0')*10;
      t2+=*q - '0';
      if(t2 > 14)
        /* tz offset hours are restricted to 0..14
         * (no need to check for < 0)
         */
        return -9;
    
      /* negative tz offset adds to the result */
      result->hour+=is_neg ? t2 : -t2;

      /* timezone minutes */    
      for(q=++p; ISNUM(*p); p++)
        ;
      l=p-q;
      if(l!=2)
        return -10;

      t=(*q++ - '0')*10;
      t+=*q - '0';
      if(t > 59 || (t2 == 14 && t!=0)) {
        /* tz offset minutes are restricted to 0..59
         * (no need to check for < 0)
         * or 0 if hour offset is exactly +-14 
         */
        return -10;
      }
    
      /* negative tz offset adds to the result */
      result->minute += is_neg ? t : -t;
      result->have_tz=1;
    }
    
    /* failure if extra chars after the timezone part */
    if(*p)
      return -11;

  }

  return rasqal_xsd_datetime_normalize(result);
}


/**
 * rasqal_xsd_datetime_to_string:
 * @dt: datetime struct
 *
 * INTERNAL - Convert a #rasqal_xsd_datetime struct to a xsd:dateTime lexical form string.
 *
 * Caller should RASQAL_FREE() the returned string.
 *
 * Return value: lexical form string or NULL on failure.
 */
static unsigned char*
rasqal_xsd_datetime_to_string(const rasqal_xsd_datetime *dt)
{
  unsigned char *ret=0;
  int is_neg;
  int r=0;
  int i;
   
  if(!dt)
    return NULL;
    
  is_neg=dt->year<0;

  /* format twice: first with null buffer of zero size to get the
   * required buffer size second time to the allocated buffer
   */
  for(i=0; i < 2; i++) {
    r=snprintf((char*)ret, r, "%s%04d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d%s%s%s",
      is_neg ? "-" : "",
      is_neg ? -dt->year : dt->year,
      dt->month,
      dt->day,
      dt->hour,
      dt->minute,
      dt->second,
      *dt->second_frac ? "." : "",
      dt->second_frac,
      dt->have_tz ? "Z" : "");

    /* error? */
    if(r<0) {
      if(ret)
        RASQAL_FREE(cstring, ret);
      return NULL;
    }

    /* alloc return buffer on first pass */
    if(!i) {
      ret=(unsigned char *)RASQAL_MALLOC(cstring, ++r);
      if(!ret)
        return NULL;
    }
  }
  return ret;
}


/**
 * rasqal_xsd_datetime_string_to_canonical:
 * @datetime_string: xsd:dateTime as lexical form string
 *
 * Convert a XML Schema dateTime lexical form string to its canonical form.
 *
 * Caller should RASQAL_FREE() the returned string.
 *
 * Return value: canonical lexical form string or NULL on failure.
 *
 *
 * http://www.w3.org/TR/xmlschema-2/#dateTime-canonical-representation
 * 
 * "Except for trailing fractional zero digits in the seconds representation,
 * '24:00:00' time representations, and timezone (for timezoned values),
 * the mapping from literals to values is one-to-one.
 * Where there is more than one possible representation,
 * the canonical representation is as follows:
 *    * The 2-digit numeral representing the hour must not be '24';
 *    * The fractional second string, if present, must not end in '0';
 *    * for timezoned values, the timezone must be represented with 'Z'
 *      (All timezoned dateTime values are UTC.)."
 */
const unsigned char*
rasqal_xsd_datetime_string_to_canonical(const unsigned char* datetime_string)
{
  rasqal_xsd_datetime d; /* allocated on stack */

  /* parse_and_normalize makes the rasqal_xsd_datetime canonical... */
  if(rasqal_xsd_datetime_parse_and_normalize(datetime_string, &d))
    return NULL;
  /* ... so return a string representation of it */
  return rasqal_xsd_datetime_to_string(&d);
}




/**
 * days_per_month:
 * @month: month 1-12
 * @year: gregorian year
 *
 * INTERNAL - returns the number of days in given month and year.
 *
 * Return value: number of days or 0 on invalid arguments
 */
static unsigned int
days_per_month(int month, int year) {
  switch(month) {
    case 1:
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
      return 31;
  
    case 4:
    case 6:
    case 9:
    case 11:
      return 30;
  
    case 2:
      /* any of bottom 2 bits non-zero -> not 0 mod 4 -> not leap year */
      if(year & 3)
        return 28;

      /* 0 mod 400 and 0 mod 4 -> leap year */
      if(!(year % 400))
        return 29;

      /* 0 mod 100 and not 0 mod 400 and 0 mod 4 -> not leap year */
      if(!(year % 100))
        return 28;

      /* other 0 mod 4 years -> leap year */
      return 29;

    default:
       /* error */
      return 0;
  }
}


int
rasqal_xsd_datetime_check(const unsigned char* string)
{
  rasqal_xsd_datetime d;
  
  /* This should be correct according to 
   * http://www.w3.org/TR/xmlschema-2/#dateTime
   */
  return !rasqal_xsd_datetime_parse_and_normalize(string, &d);
}


#ifdef STANDALONE
#include <stdio.h>

int main(int argc, char *argv[]);

#define MYASSERT(c) \
  if(!(c)) { \
    fprintf(stderr, "%s: assertion failed at %s:%d: %s\n", program, __FILE__, __LINE__, #c); \
    exit(1); \
  }


static int test_datetime_parser_tostring(const char *in_str, const char *out_expected)
{
  unsigned char const *s;
  int r=1;
  s=rasqal_xsd_datetime_string_to_canonical((const unsigned char *)in_str);
  if(s) {
    r=strcmp((char*)s, out_expected);
    if(r)
      fprintf(stderr, "input \"%s\" converted to canonical \"%s\", expected \"%s\"\n", in_str, s, out_expected);
    RASQAL_FREE(cstring, (void*)s);
  } else
    fprintf(stderr, "input \"%s\" converted to canonical (null), expected \"%s\"\n", in_str, out_expected);
  return r;
}


int
main(int argc, char *argv[]) {
  char const *program=rasqal_basename(*argv);
  rasqal_xsd_datetime d;

  /* days_per_month */
  
  MYASSERT(!days_per_month(0,287));
  
  MYASSERT(days_per_month(1,467) == 31);

  MYASSERT(days_per_month(2,1900) == 28);  
  MYASSERT(days_per_month(2,1901) == 28);
  MYASSERT(days_per_month(2,2000) == 29);
  MYASSERT(days_per_month(2,2004) == 29);
  
  MYASSERT(days_per_month(3,1955) == 31);
  MYASSERT(days_per_month(4,3612) == 30);
  MYASSERT(days_per_month(5,467) == 31);
  MYASSERT(days_per_month(6,398) == 30);
  MYASSERT(days_per_month(7,1832) == 31);
  MYASSERT(days_per_month(8,8579248) == 31);
  MYASSERT(days_per_month(9,843) == 30);
  MYASSERT(days_per_month(10,84409) == 31);
  MYASSERT(days_per_month(11,398) == 30);
  MYASSERT(days_per_month(12,4853) == 31);
  MYASSERT(!days_per_month(13,45894));
  
  /* rasqal_xsd_datetime_parse_and_normalize,
     rasqal_xsd_datetime_to_string and
     rasqal_xsd_datetime_string_to_canonical */
  
  #define PARSE_AND_NORMALIZE(_s,_d) \
    rasqal_xsd_datetime_parse_and_normalize((const unsigned char*)_s, _d)
  
  /* generic */

  MYASSERT(!rasqal_xsd_datetime_to_string(0));

  MYASSERT(PARSE_AND_NORMALIZE(0,0));
  MYASSERT(PARSE_AND_NORMALIZE("uhgsufi",0));
  MYASSERT(PARSE_AND_NORMALIZE(0,&d));
  MYASSERT(PARSE_AND_NORMALIZE("fsdhufhdsuifhidu",&d));
  
  /* year */
  
  MYASSERT(PARSE_AND_NORMALIZE("123-12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("-123-12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("0000-12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("01234-12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("-01234-12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("1234a12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("-1234b12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("g162-12-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("5476574658746587465874-12-12T12:12:12Z",&d));
  
  MYASSERT(test_datetime_parser_tostring("1234-12-12T12:12:12Z", "1234-12-12T12:12:12Z")==0);
  MYASSERT(test_datetime_parser_tostring("-1234-12-12T12:12:12Z", "-1234-12-12T12:12:12Z")==0);
  MYASSERT(test_datetime_parser_tostring("1234567890-12-12T12:12:12Z", "1234567890-12-12T12:12:12Z")==0);
  MYASSERT(test_datetime_parser_tostring("-1234567890-12-12T12:12:12Z", "-1234567890-12-12T12:12:12Z")==0);
  
  /* month */
  
  MYASSERT(PARSE_AND_NORMALIZE("2004-v-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-00-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("PARSE_AND_NORMALIZE-011-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-13-12T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-12.12T12:12:12Z",&d));

  MYASSERT(test_datetime_parser_tostring("2004-01-01T12:12:12Z", "2004-01-01T12:12:12Z")==0);

  /* day */
  
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-ffT12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-00T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-007T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-32T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01t12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01- 1T12:12:12Z",&d));
  
  MYASSERT(PARSE_AND_NORMALIZE("2005-02-29T12:12:12Z",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2005-02-28T12:12:12Z",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-02-29T12:12:12Z",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2000-02-29T12:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("1900-02-29T12:12:12Z",&d));

  MYASSERT(test_datetime_parser_tostring("2012-04-12T12:12:12Z", "2012-04-12T12:12:12Z")==0);
  
  /* hour */

  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01Tew:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T-1:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T001:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T25:12:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T01.12:12Z",&d));
  
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T24:12:00Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T24:00:34Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T24:12:34Z",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T24:00:00Z",&d));
  
  MYASSERT(test_datetime_parser_tostring("2012-04-12T24:00:00", "2012-04-13T00:00:00")==0);
  
  /* minute */
  
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:ij:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:-1:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:042:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:69:12Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12.12Z",&d));
  
  /* second */

  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:ijZ",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:-1",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:054Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:69Z",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12z",&d));
  
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12",&d));
  
  /* fraction second */
  
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12.",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12.i",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.0",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.01",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.1",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.100",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.1000000000000000000000000000000000000000000",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.5798459847598743987549",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12.1d",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12.1Z",&d));

  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.01Z", "2006-05-18T18:36:03.01Z")==0);
  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.10Z", "2006-05-18T18:36:03.1Z")==0);
  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.010Z", "2006-05-18T18:36:03.01Z")==0);
  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.1234Z", "2006-05-18T18:36:03.123Z")==0);
  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.1234", "2006-05-18T18:36:03.123")==0);
  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.1239Z", "2006-05-18T18:36:03.123Z")==0);
  MYASSERT(test_datetime_parser_tostring("2006-05-18T18:36:03.1239", "2006-05-18T18:36:03.123")==0);

  /* timezones + normalization */

  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12-",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+00.00",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+aa:bb",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+15:00",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+14:01",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12+14:00",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12-14:01",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12-14:00",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+10:99",&d));
  MYASSERT(!PARSE_AND_NORMALIZE("2004-01-01T12:12:12+10:59",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+10:059",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+010:59",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+10:59a",&d));
  MYASSERT(PARSE_AND_NORMALIZE("2004-01-01T12:12:12+10:059",&d));

  MYASSERT(test_datetime_parser_tostring("2004-12-31T23:50:22-01:15", "2005-01-01T01:05:22Z")==0);
  MYASSERT(test_datetime_parser_tostring("2005-01-01T01:00:05+02:12", "2004-12-31T22:48:05Z")==0);
  MYASSERT(test_datetime_parser_tostring("0001-01-01T00:00:00+00:01", "-0001-12-31T23:59:00Z")==0);
  MYASSERT(test_datetime_parser_tostring("-0001-12-31T23:59:00-00:01", "0001-01-01T00:00:00Z")==0);
  MYASSERT(test_datetime_parser_tostring("2005-03-01T00:00:00+01:00", "2005-02-28T23:00:00Z")==0);
  MYASSERT(test_datetime_parser_tostring("2004-03-01T00:00:00+01:00", "2004-02-29T23:00:00Z")==0);
  MYASSERT(test_datetime_parser_tostring("2005-02-28T23:00:00-01:00", "2005-03-01T00:00:00Z")==0);
  MYASSERT(test_datetime_parser_tostring("2004-02-29T23:00:00-01:00", "2004-03-01T00:00:00Z")==0);

  return 0;
}

#endif
