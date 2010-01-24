/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_expat.c - Raptor expat functions
 *
 * Copyright (C) 2000-2006, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifdef RAPTOR_XML_EXPAT


void
raptor_expat_init(raptor_sax2* sax2, raptor_uri *base_uri)
{
  XML_Parser xp=XML_ParserCreate(NULL);

  /* create a new parser in the specified encoding */
  XML_SetUserData(xp, sax2);

  XML_SetBase(xp, (XML_Char*)raptor_uri_as_string(base_uri));

  /* XML_SetEncoding(xp, "..."); */

  XML_SetElementHandler(xp, 
                        (XML_StartElementHandler)raptor_sax2_start_element,
                        (XML_EndElementHandler)raptor_sax2_end_element);
  XML_SetCharacterDataHandler(xp, 
                              (XML_CharacterDataHandler)raptor_sax2_characters);

  XML_SetCommentHandler(xp, (XML_CommentHandler)raptor_sax2_comment);

  XML_SetUnparsedEntityDeclHandler(xp, 
                                   (XML_UnparsedEntityDeclHandler)raptor_sax2_unparsed_entity_decl);

  XML_SetExternalEntityRefHandler(xp, (XML_ExternalEntityRefHandler)raptor_sax2_external_entity_ref);

  sax2->xp=xp;
}


void
raptor_expat_update_document_locator(raptor_sax2* sax2,
                                     raptor_locator* locator)
{
  locator->line=XML_GetCurrentLineNumber(sax2->xp);
  locator->column=XML_GetCurrentColumnNumber(sax2->xp);
  locator->byte=XML_GetCurrentByteIndex(sax2->xp);
}

/* end if RAPTOR_XML_EXPAT */
#endif
