/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * turtle_common.c - Raptor Turtle common code
 *
 * Copyright (C) 2003-2007, David Beckett http://www.dajobe.org/
 * Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
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
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


/**
 * raptor_stringbuffer_append_turtle_string:
 * @stringbuffer: String buffer to add to
 * @text: turtle string to decode
 * @len: length of string
 * @delim: terminating delimiter for string - only ', " or &gt; are allowed
 * @error_handler: error handling function
 * @error_data: error handler data
 *
 * Append to a stringbuffer a Turtle-escaped string.
 *
 * The passed in string is handled according to the Turtle string
 * escape rules giving a UTF-8 encoded output of the Unicode codepoints.
 *
 * The Turtle escapes are \n \r \t \\
 * \uXXXX \UXXXXXXXX where X is [A-F0-9]
 * 
 * Return value: non-0 on failure
 **/
int
raptor_stringbuffer_append_turtle_string(raptor_stringbuffer* stringbuffer,
                                         const unsigned char *text,
                                         size_t len, int delim,
                                         raptor_simple_message_handler error_handler, 
                                         void *error_data)
{
  size_t i;
  const unsigned char *s;
  unsigned char *d;
  unsigned char *string=(unsigned char *)RAPTOR_MALLOC(cstring, len+1);
  
  if(!string)
    return -1;

  for(s=text, d=string, i=0; i<len; s++, i++) {
    unsigned char c=*s;

    if(c == '\\' ) {
      s++; i++;
      c=*s;
      if(c == 'n')
        *d++= '\n';
      else if(c == 'r')
        *d++= '\r';
      else if(c == 't')
        *d++= '\t';
      else if(c == '\\' || c == delim)
        *d++=c;
      else if (c == 'u' || c == 'U') {
        size_t ulen=(c == 'u') ? 4 : 8;
        unsigned long unichar=0;
        int n;
        
        s++; i++;
        if(i+ulen > len) {
          error_handler(error_data,
                        "Turtle string error - \\%c over end of line", c);
          RAPTOR_FREE(cstring, string);
          return 1;
        }
        
        n=sscanf((const char*)s, ((ulen == 4) ? "%04lx" : "%08lx"), &unichar);
        if(n != 1) {
          error_handler(error_data,
                        "Turtle string error - illegal Uncode escape '%c%s...'",
                        c, s);
          RAPTOR_FREE(cstring, string);
          return 1;
        }

        s+= ulen-1;
        i+= ulen-1;
        
        if(unichar > 0x10ffff) {
          error_handler(error_data,
                        "Turtle string error - illegal Unicode character with code point #x%lX.", 
                        unichar);
          RAPTOR_FREE(cstring, string);
          return 1;
        }
          
        d+=raptor_unicode_char_to_utf8(unichar, d);

      } else {
        /* don't handle \x where x isn't one of: \t \n \r \\ (delim) */
        error_handler(error_data,
                      "Turtle string error - illegal escape \\%c (#x%02X) in \"%s\"", 
                      c, c, text);
      }
    } else
      *d++=c;
  }
  *d='\0';

  /* string gets owned by the stringbuffer after this */
  return raptor_stringbuffer_append_counted_string(stringbuffer, 
                                                   string, len, 0);

}



