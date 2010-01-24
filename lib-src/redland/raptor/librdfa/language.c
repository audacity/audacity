/**
 * Copyright 2008 Digital Bazaar, Inc.
 *
 * This file is part of librdfa.
 * 
 * librdfa is Free Software, and can be licensed under any of the
 * following three licenses:
 * 
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any 
 *      newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE-* at the top of this software distribution for more
 * information regarding the details of each license.
 *
 * The language module is used to determine and set the current language.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "rdfa_utils.h"
#include "rdfa.h"

/**
 * Updates the language given the value of the xml:lang attribute.
 *
 * @param lang the new value of the lang attribute.
 */
void rdfa_update_language(rdfacontext* context, const char* lang)
{
   // the [current element] is parsed for any language information,
   // and [language] is set in the [current evaluation context];

   if(lang != NULL)
   {
      context->language = rdfa_replace_string(context->language, lang);
   }
}
