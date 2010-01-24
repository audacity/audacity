/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_xs.c - Raptor XSD code
 *
 * Copyright (C) 2005-2006, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2005, University of Bristol, UK http://www.bristol.ac.uk/
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


raptor_identifier*
raptor_new_identifier_from_double(double d)
{
  unsigned char *string;
  raptor_uri *uri;

  string=(unsigned char*)RAPTOR_MALLOC(cstring, 32); /* FIXME */
  if((double)((int)d) == d)
    sprintf((char*)string, "%1g.0", d);
  else
    sprintf((char*)string, "%1g", d);
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#double");
  return raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, string, uri, NULL);
}

