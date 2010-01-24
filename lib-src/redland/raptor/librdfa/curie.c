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
 * The CURIE module is used to resolve all forms of CURIEs that
 * XHTML+RDFa accepts.
 *
 * @author Manu Sporny
 */
#include "stdlib.h"
#include "string.h"
#include "stdio.h"
#include "rdfa_utils.h"
#include "rdfa.h"

// These are all of the @property reserved words in XHTML 1.1 that
// should generate triples.
#define XHTML_PROPERTY_RESERVED_WORDS_SIZE 6
static
   const char* g_property_reserved_words[XHTML_PROPERTY_RESERVED_WORDS_SIZE] =
{
   "description", "generator", "keywords", "reference", "robots", "title"
};

// These are all of the @rel/@rev reserved words in XHTML 1.1 that
// should generate triples.
#define XHTML_RELREV_RESERVED_WORDS_SIZE 23
static const char* g_relrev_reserved_words[XHTML_RELREV_RESERVED_WORDS_SIZE] =
{
   "alternate", "appendix", "bookmark", "chapter", "cite", "contents",
   "copyright", "glossary", "help", "icon", "index", "meta", "next", "p3pv1",
   "prev", "role",  "section",  "stylesheet", "subsection",  "start",
   "license", "up", "last"
};

// The base XHTML vocab URL is used to resolve URIs that are reserved
// words. Any reserved listed above is appended to the URL below to
// form a complete IRI.
#define XHTML_VOCAB_URI "http://www.w3.org/1999/xhtml/vocab#"
#define XHTML_VOCAB_URI_SIZE 35

/**
 * Gets the type of CURIE that is passed to it.
 *
 * @param uri the uri to check.
 *
 * @return either CURIE_TYPE_SAFE, CURIE_TYPE_URI or CURIE_TYPE_INVALID.
 */
curie_t get_curie_type(const char* uri)
{
   curie_t rval = CURIE_TYPE_INVALID;
   
   if(uri != NULL)
   {
      size_t uri_length = strlen(uri);

      if((uri[0] == '[') && (uri[uri_length - 1] == ']'))
      {
         // a safe curie starts with [ and ends with ]
         rval = CURIE_TYPE_SAFE;
      }
      else if(strstr(uri, ":") != NULL)
      {
         // at this point, it is unknown whether or not the CURIE is
         // an IRI or an unsafe CURIE
         rval = CURIE_TYPE_IRI_OR_UNSAFE;
      }
      else
      {
         // if none of the above match, then the CURIE is probably a
         // relative IRI
         rval = CURIE_TYPE_IRI_OR_UNSAFE;
      }
   }

   return rval;
}

char* rdfa_resolve_uri(rdfacontext* context, const char* uri)
{
   char* rval = NULL;
   size_t base_length = strlen(context->base);
   
   if(strlen(uri) < 1)
   {
      // if a blank URI is given, use the base context
      rval = rdfa_replace_string(rval, context->base);
   }
   else if(strstr(uri, ":") != NULL)
   {
      // if a IRI is given, don't concatenate
      rval = rdfa_replace_string(rval, uri);
   }
   else if(uri[0] == '#')
   {
      // if a fragment ID is given, concatenate it with the base URI
      rval = rdfa_join_string(context->base, uri);
   }
   else
   {
      if((char)context->base[base_length - 1] == '/')
      {
         // if the base URI already ends in /, concatenate
         rval = rdfa_join_string(context->base, uri);
      }
      else
      {
         // if we have a relative URI, chop off the name of the file
         // and replace it with the relative pathname
         char* end_index = rindex(context->base, '/');

         if(end_index != NULL)
         {
            char* tmpstr = NULL;
            char* end_index2;

            tmpstr = rdfa_replace_string(tmpstr, context->base);
            end_index2= rindex(tmpstr, '/');
            end_index2++;
            *end_index2 = '\0';

            rval = rdfa_join_string(tmpstr, uri);
            free(tmpstr);
         }
      }
   }

   return rval;
}

char* rdfa_resolve_curie(
   rdfacontext* context, const char* uri, curieparse_t mode)
{
   char* rval = NULL;
   curie_t ctype = get_curie_type(uri);

   if(ctype == CURIE_TYPE_INVALID)
   {
      rval = NULL;
   }
   else if((ctype == CURIE_TYPE_IRI_OR_UNSAFE) &&
           ((mode == CURIE_PARSE_HREF_SRC) ||
            (mode == CURIE_PARSE_ABOUT_RESOURCE)))
   {
      // If we are parsing something that can take either a CURIE or a
      // URI, and the type is either IRI or UNSAFE, assume that it is
      // an IRI
      rval = rdfa_resolve_uri(context, uri);
   }

   // if we are processing a safe CURIE OR
   // if we are parsing an unsafe CURIE that is an @type_of,
   // @datatype, @property, @rel, or @rev attribute, treat the curie
   // as not an IRI, but an unsafe CURIE
   if((ctype == CURIE_TYPE_SAFE) ||
         ((ctype == CURIE_TYPE_IRI_OR_UNSAFE) &&
          ((mode == CURIE_PARSE_INSTANCEOF_DATATYPE) ||
           (mode == CURIE_PARSE_PROPERTY) ||
           (mode == CURIE_PARSE_RELREV))))
   {
      char* working_copy = NULL;
      char* wcptr = NULL;
      char* prefix = NULL;
      char* curie_reference = NULL;
      const char* expanded_prefix = NULL;
      size_t expanded_prefix_length = 0;

      working_copy = (char*)malloc(strlen(uri) + 1);
      strcpy(working_copy, uri);//rdfa_replace_string(working_copy, uri);

      // if this is a safe CURIE, chop off the beginning and the end
      if(ctype == CURIE_TYPE_SAFE)
      {
         prefix = strtok_r(working_copy, "[:]", &wcptr);
         if(wcptr)
            curie_reference = strtok_r(NULL, "[:]", &wcptr);
      }
      else if(ctype == CURIE_TYPE_IRI_OR_UNSAFE)
      {
         prefix = strtok_r(working_copy, ":", &wcptr);
         if(wcptr)
            curie_reference = strtok_r(NULL, ":", &wcptr);
      }

      // fully resolve the prefix and get it's length

      // if a colon was found, but no prefix, use the context->base as
      // the prefix IRI
      if(uri[0] == ':')
      {
         expanded_prefix = "http://www.w3.org/1999/xhtml/vocab#";
         curie_reference = prefix;
         prefix = NULL;
      }
      else if((strlen(uri) > 2) && (uri[1] == ':'))
      {
         expanded_prefix = context->base;
         curie_reference = prefix;
         prefix = NULL;
      }
      else if(prefix != NULL)
      {
         if(strcmp(prefix, "_") == 0)
         {
            // if the prefix specifies this as a blank node, then we
            // use the blank node prefix
            expanded_prefix = "_";
         }
         //else if(strcmp(prefix, "rdf") == 0)
         //{
         //   expanded_prefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
         //}
         else
         {
            // if the prefix was defined, get it from the set of URI mappings.
#ifdef LIBRDFA_IN_RAPTOR
            raptor_namespace *nspace;
            raptor_uri* ns_uri;
            nspace = raptor_namespaces_find_namespace(&context->sax2->namespaces,
                                                      (const unsigned char*)prefix, strlen(prefix));
            if(nspace) {
               ns_uri = raptor_namespace_get_uri(nspace);
               if(ns_uri)
                  expanded_prefix = (const char*)raptor_uri_as_string(ns_uri);
            }
#else
            expanded_prefix =
               rdfa_get_mapping(context->uri_mappings, prefix);
#endif
         }
      }

      // get the length of the expanded prefix if it exists.
      if(expanded_prefix != NULL)
      {
         expanded_prefix_length = strlen(expanded_prefix);
      }
      
      // if the expanded prefix and the reference exist, generate the
      // full IRI.
      if((expanded_prefix != NULL) && (curie_reference != NULL))
      {
         if(strcmp(expanded_prefix, "_") == 0)
         {
            rval = rdfa_join_string("_:", curie_reference);
         }
         else
         {
            rval = rdfa_join_string(expanded_prefix, curie_reference);
         }
      }

      free(working_copy);
   }

   // if we're NULL at this point, the CURIE might be the special
   // unnamed bnode specified by _:
   if((rval == NULL) &&
      ((strcmp(uri, "[_:]") == 0) ||
       (strcmp(uri, "_:") == 0)))
   {
      if(context->underscore_colon_bnode_name == NULL)
      {
         context->underscore_colon_bnode_name = rdfa_create_bnode(context);
      }
      rval = rdfa_replace_string(rval, context->underscore_colon_bnode_name);
   }
   
   // even though a reference-only CURIE is valid, it does not
   // generate a triple in XHTML+RDFa. If we're NULL at this point,
   // the given value wasn't valid in XHTML+RDFa.
   
   return rval;
}

/**
 * Resolves a given uri depending on whether or not it is a fully
 * qualified IRI, a CURIE, or a short-form XHTML reserved word for
 * @rel or @rev as defined in the XHTML+RDFa Syntax Document.
 *
 * @param context the current processing context.
 * @param uri the URI part to process.
 *
 * @return the fully qualified IRI, or NULL if the conversion failed
 *         due to the given URI not being a short-form XHTML reserved
 *         word. The memory returned from this function MUST be freed.
 */
char* rdfa_resolve_relrev_curie(rdfacontext* context, const char* uri)
{
   char* rval = NULL;
   int i = 0;
   const char* resource = uri;

   // check to make sure the URI doesn't have an empty prefix
   if(uri[0] == ':')
   {
      resource++;
   }

   // search all of the XHTML @rel/@rev reserved words for a match
   // against the given URI
   for(i = 0; i < XHTML_RELREV_RESERVED_WORDS_SIZE; i++)
   {
      if(strcmp(g_relrev_reserved_words[i], resource) == 0)
      {
         // since the URI is a reserved word for @rel/@rev, generate
         // the full IRI and stop the loop.
         rval = rdfa_join_string(XHTML_VOCAB_URI, resource);         
         i = XHTML_RELREV_RESERVED_WORDS_SIZE;
      }
   }

   // if none of the XHTML @rel/@rev reserved words were found,
   // attempt to resolve the value as a standard CURIE
   if(rval == NULL)
   {
      rval = rdfa_resolve_curie(context, uri, CURIE_PARSE_RELREV);
   }
   
   return rval;
}

/**
 * Resolves a given uri depending on whether or not it is a fully
 * qualified IRI, a CURIE, or a short-form XHTML reserved word for
 * @property as defined in the XHTML+RDFa Syntax Document.
 *
 * @param context the current processing context.
 * @param uri the URI part to process.
 *
 * @return the fully qualified IRI, or NULL if the conversion failed
 *         due to the given URI not being a short-form XHTML reserved
 *         word. The memory returned from this function MUST be freed.
 */
char* rdfa_resolve_property_curie(rdfacontext* context, const char* uri)
{
   char* rval = NULL;
   int i = 0;
   const char* resource = uri;

   // check to make sure the URI doesn't have an empty prefix
   if(uri[0] == ':')
   {
      resource++;
   }
   
   // TODO: Is it clear that property has predefined values in the
   //       Syntax doc?
   // TODO: THIS IS A BUG AND SHOULD BE REMOVED - there are no
   //       predefined values for @property
   // search all of the XHTML @property reserved words for a match
   // against the given URI
   for(i = 0; i < XHTML_PROPERTY_RESERVED_WORDS_SIZE; i++)
   {
      if(strcmp(g_property_reserved_words[i], resource) == 0)
      {
         // since the URI is a reserved word, generate the full IRI
         // and stop the loop.
         rval = rdfa_join_string(XHTML_VOCAB_URI, resource);
         i = XHTML_PROPERTY_RESERVED_WORDS_SIZE;
      }
   }

   // if none of the XHTML @property reserved words were found,
   // attempt to resolve the value as a standard CURIE
   if(rval == NULL)
   {
      rval = rdfa_resolve_curie(context, uri, CURIE_PARSE_PROPERTY);
   }
   
   return rval;
}

rdfalist* rdfa_resolve_curie_list(
   rdfacontext* rdfa_context, const char* uris, curieparse_t mode)
{
   rdfalist* rval = rdfa_create_list(3);
   char* working_uris = NULL;
   char* uptr = NULL;
   char* ctoken = NULL;
   working_uris = rdfa_replace_string(working_uris, uris);

   // go through each item in the list of CURIEs and resolve each
   ctoken = strtok_r(working_uris, " ", &uptr);
   while(ctoken != NULL)
   {
      char* resolved_curie = NULL;

      if((mode == CURIE_PARSE_INSTANCEOF_DATATYPE) ||
         (mode == CURIE_PARSE_ABOUT_RESOURCE))
      {
         resolved_curie =
            rdfa_resolve_curie(rdfa_context, ctoken, mode);
      }
      else if(mode == CURIE_PARSE_RELREV)
      {
         resolved_curie =
            rdfa_resolve_relrev_curie(rdfa_context, ctoken);
      }
      else if(mode == CURIE_PARSE_PROPERTY)
      {
         resolved_curie =
            rdfa_resolve_property_curie(rdfa_context, ctoken);
      }

      // add the CURIE if it was a valid one
      if(resolved_curie != NULL)
      {
         rdfa_add_item(rval, resolved_curie, RDFALIST_FLAG_TEXT);
         free(resolved_curie);
      }
      
      ctoken = strtok_r(NULL, " ", &uptr);
   }
   
   free(working_uris);

   return rval;
}
