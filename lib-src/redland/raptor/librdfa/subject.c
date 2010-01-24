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
 * This file is used to process RDFa subjects.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "rdfa_utils.h"
#include "rdfa.h"

/**
 * Creates a new bnode given an RDFa context.
 *
 * @param context the RDFa context.
 *
 * @return a newly allocated string containing the bnode name. This
 *         string MUST be memory collected.
 */
char* rdfa_create_bnode(rdfacontext* context)
{
   char* rval = NULL;
   char buffer[64];
   
   // print and increment the bnode count
   sprintf(buffer, "_:bnode%i", (int)context->bnode_count++);
   rval = rdfa_replace_string(rval, buffer);

   return rval;
}

/**
 * Establishes a new subject for the given context given the
 * attributes on the current element. The given context's new_subject
 * value is updated if a new subject is found.
 *
 * @param context the RDFa context.
 * @param name the name of the current element that is being processed.
 * @param about the full IRI for about, or NULL if there isn't one.
 * @param src the full IRI for src, or NULL if there isn't one.
 * @param resource the full IRI for resource, or NULL if there isn't one.
 * @param href the full IRI for href, or NULL if there isn't one.
 * @param type_of The list of IRIs for type_of, or NULL if there was
 *                no type_of specified.
 */
void rdfa_establish_new_subject(
   rdfacontext* context, const char* name, const char* about, const char* src,
   const char* resource, const char* href, const rdfalist* type_of)
{
   // 4. If the [current element] contains no valid @rel or @rev
   // URI, obtained according to the section on CURIE and URI
   // Processing, then the next step is to establish a value for
   // [new subject]. Any of the attributes that can carry a
   // resource can set [new subject];

   if(about != NULL)
   {
      // * by using the URI from @about, if present, obtained according
      //   to the section on CURIE and URI Processing;
      context->new_subject =
         rdfa_replace_string(context->new_subject, about);
   }
   else if(src != NULL)
   {   
      // * otherwise, by using the URI from @src, if present, obtained
      //   according to the section on CURIE and URI Processing.
      context->new_subject =
         rdfa_replace_string(context->new_subject, src);
   }
   else if(resource != NULL)
   {   
      // * otherwise, by using the URI from @resource, if present,
      //   obtained according to the section on CURIE and URI
      //   Processing;
      context->new_subject =
         rdfa_replace_string(context->new_subject, resource);
   }
   else if(href != NULL)
   {
      // * otherwise, by using the URI from @href, if present, obtained
      //   according to the section on CURIE and URI Processing.
      context->new_subject =
         rdfa_replace_string(context->new_subject, href);
   }
   // * If no URI is provided by a resource attribute, then the first
   // match from the following rules will apply:
   else if((strcmp(name, "head") == 0) || (strcmp(name, "body") == 0))
   {
      // * if the element is the head or body element then act as if
      // there is an empty @about present, and process it according to
      // the rule for @about, above;
      context->new_subject =
         rdfa_replace_string(context->new_subject, context->base);
   }
   else if((type_of != NULL) && (type_of->num_items > 0))
   {
      // * if @type_of is present, obtained according to the
      // section on CURIE and URI Processing, then [new subject] is
      // set to be a newly created [bnode];
      context->new_subject = rdfa_create_bnode(context);
   }
   else if(context->parent_object != NULL)
   {
      // * otherwise, if [parent object] is present, [new subject] is
      // set to that and the [skip element] flag is set to 'true';
      context->new_subject =
         rdfa_replace_string(context->new_subject, context->parent_object);

      // TODO: The skip element flag will be set even if there is a
      // @property value, which is a bug, isn't it?
      //context->skip_element = 1;
   }
}

/**
 * Establishes a new subject for the given context when @rel or @rev
 * is present. The given context's new_subject and
 * current_object_resource values are updated if a new subject is found.
 *
 * @param context the RDFa context.
 * @param about the full IRI for about, or NULL if there isn't one.
 * @param src the full IRI for src, or NULL if there isn't one.
 * @param resource the full IRI for resource, or NULL if there isn't one.
 * @param href the full IRI for href, or NULL if there isn't one.
 * @param type_of the list of IRIs for type_of, or NULL if type_of
 *                wasn't specified on the current element.
 */
void rdfa_establish_new_subject_with_relrev(
   rdfacontext* context, const char* name, const char* about, const char* src,
   const char* resource, const char* href, const rdfalist* type_of)
{
   // 5. If the [current element] does contain a valid @rel or @rev
   // URI, obtained according to the section on CURIE and URI
   // Processing, then the next step is to establish both a value
   // for [new subject] and a value for [current object resource]:
   //
   // [new subject] is set to the URI obtained from the first match
   // from the following rules:
   
   if(about != NULL)
   {
      // * by using the URI from @about, if present, obtained
      // according to the section on CURIE and URI Processing;
      context->new_subject =
         rdfa_replace_string(context->new_subject, about);
   }
   else if(src != NULL)
   {
      // * otherwise, by using the URI from @src, if present, obtained
      // according to the section on CURIE and URI Processing.
      context->new_subject =
         rdfa_replace_string(context->new_subject, src);
   }
   // * If no URI is provided then the first match from the following
   // rules will apply:
   else if((strcmp(name, "head") == 0) || (strcmp(name, "body") == 0))
   {
      // * if the element is the head or body element then act as if
      // there is an empty @about present, and process it according to
      // the rule for @about, above;
      context->new_subject =
         rdfa_replace_string(context->new_subject, context->base);
   }
   else if((type_of != NULL) && (type_of->num_items > 0))
   {
      // * if @type_of is present, obtained according to the
      // section on CURIE and URI Processing, then [new subject] is
      // set to be a newly created [bnode];
      context->new_subject = rdfa_create_bnode(context);
   }
   else if(context->parent_object != NULL)
   {
      // * otherwise, if [parent object] is present, [new subject] is
      // set to that;
      context->new_subject =
         rdfa_replace_string(context->new_subject, context->parent_object);
   }

   // Then the [current object resource] is set to the URI obtained
   // from the first match from the following rules:
   if(resource != NULL)
   {
      // * by using the URI from @resource, if present, obtained
      //   according to the section on CURIE and URI Processing;
      context->current_object_resource =
         rdfa_replace_string(context->current_object_resource, resource);
   }
   else if(href != NULL)
   {
      // * otherwise, by using the URI from @href, if present,
      //   obtained according to the section on CURIE and URI Processing.
      context->current_object_resource =
         rdfa_replace_string(context->current_object_resource, href);
   }
   else
   {
      // * otherwise, null.
      context->current_object_resource = NULL;
   }

   // Note that final value of the [current object resource] will
   // either be null, or a full URI.
}
