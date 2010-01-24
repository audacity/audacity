/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_grddl.c - Raptor GRDDL (+microformats) Parser implementation
 *
 * Copyright (C) 2005-2008, David Beckett http://purl.org/net/dajobe/
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
 */

/*
 * Specifications:
 *   Gleaning Resource Descriptions from Dialects of Languages (GRDDL)
 *   W3C Proposed Recommendation 16 July 2007
 *   http://www.w3.org/TR/2007/PR-grddl-20070716/
 *   http://www.w3.org/TR/grddl/
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

#include <libxml/xpath.h>
/* for xmlXPathRegisterNs() */
#include <libxml/xpathInternals.h>
#include <libxml/xinclude.h>
#include <libxml/HTMLparser.h>

#include <libxslt/xslt.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <libxslt/security.h>


/*
 * libxslt API notes
 *
 * Inputs to an XSLT transformation process with libxslt are:
 *   1. A set of (key:value) parameters.
 *   2. An xsltStylesheetPtr for the XSLT sheet
 *     Which could be made from a file or an xmlDoc; and the xmlDoc.
 *     made from a file or memory buffer.
 *   3. An xmlDoc for the XML source
 *     Which could be made from a file or a memory buffer.
 *   
 */


static void raptor_grddl_filter_triples(void *user_data, const raptor_statement *statement);

static void raptor_libxslt_error_common(raptor_parser* rdf_parser, const char *msg, va_list args, const char *prefix) RAPTOR_PRINTF_FORMAT(2, 0);

static void raptor_grddl_xsltGenericError_handler(void *user_data, const char *msg, ...) RAPTOR_PRINTF_FORMAT(2, 0);


typedef struct
{
  /* transformation (XSLT) or profile URI */
  raptor_uri* uri;
  /* base URI in effect when the above was found */
  raptor_uri* base_uri;
} grddl_xml_context;
  

/*
 * XSLT parser object
 */
struct raptor_grddl_parser_context_s {
  raptor_parser* rdf_parser;
  
  xmlSAXHandler sax;

  /* HTML document ctxt */
  htmlParserCtxtPtr html_ctxt;
  /* XML document ctxt */
  xmlParserCtxtPtr xml_ctxt;

  /* Create xpath evaluation context */
  xmlXPathContextPtr xpathCtx; 

  /* parser for dealing with the result */
  raptor_parser* internal_parser;
  /* ... constructed with this name */
  const char* internal_parser_name;

  /* sax2 structure - only for recording error pointers */
  raptor_sax2* sax2;

  /* URI of root namespace of document */
  raptor_uri* root_ns_uri;

  /* List of transformation URIs for document */
  raptor_sequence* doc_transform_uris;

  /* Copy of the user data statement_handler overwritten to point to
   * raptor_grddl_filter_triples() 
   */
  void* saved_user_data;
  raptor_statement_handler saved_statement_handler;

  /* URI data-view:namespaceTransformation */
  raptor_uri* namespace_transformation_uri;

  /* URI data-view:profileTransformation */
  raptor_uri* profile_transformation_uri;

  /* List of namespace / <head profile> URIs */
  raptor_sequence* profile_uris;

  /* List of visited URIs */
  raptor_sequence* visited_uris;

  /* Depth of GRDDL parsers - 0 means that the lists above 
   * are owned by this parser: visited_uris
   * */
  int grddl_depth;

  /* Content-Type of top-level document */
  char* content_type;

  /* Check content type once */
  int content_type_check;

  /* stringbuffer to use to store retrieved document */
  raptor_stringbuffer* sb;

  /* non-0 to perform an additional RDF/XML parse on a retrieved document
   * because it has been identified as RDF/XML. */
  int process_this_as_rdfxml;

  /* non-0 to perform GRDL processing on document */
  int grddl_processing;

  /* non-0 to perform XML Include processing on document */
  int xinclude_processing;

  /* non-0 to perform HTML Base processing on document */
  int html_base_processing;

  /* non-0 to perform HTML <link> processing on document */
  int html_link_processing;
};


typedef struct raptor_grddl_parser_context_s raptor_grddl_parser_context;


static void
raptor_libxslt_error_common(raptor_parser* rdf_parser,
                            const char *msg, va_list args, 
                            const char *prefix)
{
  int prefix_length=strlen(prefix);
  int length;
  char *nmsg;

  length=prefix_length+strlen(msg)+1;
  nmsg=(char*)RAPTOR_MALLOC(cstring, length);
  if(nmsg) {
    strcpy(nmsg, prefix);
    strcpy(nmsg+prefix_length, msg);
    if(nmsg[length-1]=='\n')
      nmsg[length-1]='\0';
  }

  raptor_parser_error_varargs(rdf_parser, 
                              nmsg ? nmsg : msg,
                              args);
  if(nmsg)
    RAPTOR_FREE(cstring,nmsg);
}


static void
raptor_grddl_xsltGenericError_handler(void *user_data, const char *msg, ...)
{
  raptor_parser* rdf_parser=(raptor_parser*)user_data;
  va_list arguments;

  if(!msg || *msg == '\n')
    return;

  va_start(arguments, msg);
  raptor_libxslt_error_common(rdf_parser, msg, arguments, "libxslt error: ");
  va_end(arguments);
}


static grddl_xml_context*
raptor_new_xml_context(raptor_uri* uri, raptor_uri* base_uri)
{
  grddl_xml_context* xml_context;

  xml_context=(grddl_xml_context*)RAPTOR_MALLOC(xml_context, sizeof(grddl_xml_context));
  if(uri)
    uri=raptor_uri_copy(uri);
  if(base_uri)
    base_uri=raptor_uri_copy(base_uri);
  xml_context->uri=uri;
  xml_context->base_uri=base_uri;

  return xml_context;
}


static void
grddl_free_xml_context(void* userdata) 
{
  grddl_xml_context* xml_context=(grddl_xml_context*)userdata;
  
  if(xml_context->uri)
    raptor_free_uri(xml_context->uri);
  if(xml_context->base_uri)
    raptor_free_uri(xml_context->base_uri);
  RAPTOR_FREE(grddl_xml_context, xml_context);
}


static int
raptor_grddl_parse_init_common(raptor_parser* rdf_parser, const char *name)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  grddl_parser->rdf_parser=rdf_parser;
  
  /* sax2 structure - only for recording error pointers */
  grddl_parser->sax2=raptor_new_sax2(rdf_parser, &rdf_parser->error_handlers);

  /* The following error fields are normally initialised by
   * raptor_libxml_init() via raptor_sax2_parse_start() which is
   * not used here as we go to libxml calls direct.
   */
  raptor_libxml_init_sax_error_handlers(&grddl_parser->sax);

  /* Sequence of URIs of XSLT sheets to transform the document */
  grddl_parser->doc_transform_uris=raptor_new_sequence((raptor_sequence_free_handler*)grddl_free_xml_context, NULL);

  grddl_parser->grddl_processing=1;
  grddl_parser->xinclude_processing=1;
  grddl_parser->html_base_processing=0;
  grddl_parser->html_link_processing=1;

  return 0;
}


static int
raptor_grddl_parse_init(raptor_parser* rdf_parser, const char *name)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  raptor_grddl_parse_init_common(rdf_parser, name);

  /* Sequence of URIs from <head profile> */
  grddl_parser->profile_uris=raptor_new_sequence((raptor_sequence_free_handler*)grddl_free_xml_context, NULL);

  grddl_parser->namespace_transformation_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2003/g/data-view#namespaceTransformation");
  grddl_parser->profile_transformation_uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2003/g/data-view#profileTransformation");

  /* Sequence of URIs visited - may be overwritten if this is not
   * the depth 0 grddl parser
   */
  grddl_parser->visited_uris=raptor_new_sequence((raptor_sequence_free_handler*)raptor_free_uri, (raptor_sequence_print_handler*)raptor_sequence_print_uri);

  return 0;
}


static void
raptor_grddl_parse_terminate(raptor_parser *rdf_parser)
{
  raptor_grddl_parser_context *grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;
  if(grddl_parser->xml_ctxt) {
    if(grddl_parser->xml_ctxt->myDoc) {
      xmlFreeDoc(grddl_parser->xml_ctxt->myDoc);
      grddl_parser->xml_ctxt->myDoc=NULL;
    }
    xmlFreeParserCtxt(grddl_parser->xml_ctxt);
  }

  if(grddl_parser->html_ctxt) {
    if(grddl_parser->html_ctxt->myDoc) {
      xmlFreeDoc(grddl_parser->html_ctxt->myDoc);
      grddl_parser->html_ctxt->myDoc=NULL;
    }
    htmlFreeParserCtxt(grddl_parser->html_ctxt);
  }

  if(grddl_parser->xpathCtx)
    xmlXPathFreeContext(grddl_parser->xpathCtx);

  if(grddl_parser->internal_parser)
    raptor_free_parser(grddl_parser->internal_parser);

  if(grddl_parser->sax2)
    raptor_free_sax2(grddl_parser->sax2);

  if(grddl_parser->root_ns_uri)
    raptor_free_uri(grddl_parser->root_ns_uri);

  if(grddl_parser->doc_transform_uris)
    raptor_free_sequence(grddl_parser->doc_transform_uris);

  if(grddl_parser->profile_uris)
    raptor_free_sequence(grddl_parser->profile_uris);

  if(grddl_parser->namespace_transformation_uri)
    raptor_free_uri(grddl_parser->namespace_transformation_uri);

  if(grddl_parser->profile_transformation_uri)
    raptor_free_uri(grddl_parser->profile_transformation_uri);

  if(!grddl_parser->grddl_depth) {
    if(grddl_parser->visited_uris)
      raptor_free_sequence(grddl_parser->visited_uris);
  }

  if(grddl_parser->content_type)
    RAPTOR_FREE(cstring, grddl_parser->content_type);

  if(grddl_parser->sb)
    raptor_free_stringbuffer(grddl_parser->sb);
}


static void
raptor_grddl_parser_add_parent(raptor_parser *rdf_parser, 
                               raptor_grddl_parser_context* parent_grddl_parser)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  /* Do not set parent twice */
  if(grddl_parser->visited_uris == parent_grddl_parser->visited_uris)
    return;
  
  /* free any sequence here */
  if(grddl_parser->visited_uris)
    raptor_free_sequence(grddl_parser->visited_uris);

  /* share parent's list and do not free it here */
  grddl_parser->visited_uris= parent_grddl_parser->visited_uris;
  grddl_parser->grddl_depth= parent_grddl_parser->grddl_depth+1;

  grddl_parser->saved_user_data= parent_grddl_parser->rdf_parser;
  grddl_parser->saved_statement_handler= raptor_grddl_filter_triples;
}

    

static int
raptor_grddl_parse_start(raptor_parser *rdf_parser) 
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;
  raptor_locator *locator=&rdf_parser->locator;
  
  locator->line=1;
 
  grddl_parser->content_type_check=0;
  grddl_parser->process_this_as_rdfxml=0;

  return 0;
}


#define MATCH_IS_VALUE_LIST 1
#define MATCH_IS_PROFILE    2
#define MATCH_IS_HARDCODED  4
/* stop looking for other hardcoded matches */
#define MATCH_LAST          8
static struct {
  const xmlChar* xpath;
  int flags;
  const xmlChar* xslt_sheet_uri;
} match_table[]={
  /* XHTML document where the GRDDL profile is in
   * <link ref='transform' href='url'>  inside the html <head>
   * Value of @rel is a space-separated list of link types.
   */
  {
    (const xmlChar*)"/html:html/html:head[contains(@profile,\"http://www.w3.org/2003/g/data-view\")]/html:link[contains(@rel,\"transformation\")]/@href",
    0,
    NULL
  }
  ,
  /* XHTML document where the GRDDL profile is in
   * <a rel='transform' href='url'> inside the html <body>
   * Value of @rel is a space-separated list of link types.
   */
  {
    (const xmlChar*)"/html:html/html:head[contains(@profile,\"http://www.w3.org/2003/g/data-view\")]/../..//html:a[contains(@rel,\"transformation\")]/@href",
    0,
    NULL
  }
  ,
  /* XML document linking to transform via attribute dataview:transformation 
   * on the root element.
   * Example: http://www.w3.org/2004/01/rdxh/grddl-p3p-example
   **/
  {
    (const xmlChar*)"/*/@dataview:transformation",
    MATCH_IS_VALUE_LIST,
    NULL
  }
  ,
  /* hCalendar microformat http://microformats.org/wiki/hcalendar */
  {
    (const xmlChar*)"//*[contains(concat(' ', concat(normalize-space(@class),' ')),' vevent ')]",
    MATCH_IS_HARDCODED,
    (const xmlChar*)"http://www.w3.org/2002/12/cal/glean-hcal.xsl"
  }
  ,
  /* hReview microformat http://microformats.org/wiki/review */
  {
    (const xmlChar*)"//*[contains(concat(' ', concat(normalize-space(@class),' ')),' hreview ')]",
    MATCH_IS_HARDCODED | MATCH_LAST, /* stop here since hCard is inside hReview */
    (const xmlChar*)"http://www.w3.org/2001/sw/grddl-wg/doc29/hreview2rdfxml.xsl"
  }
  ,
  /* hCard microformat http://microformats.org/wiki/hcard */
  {
    (const xmlChar*)"//*[contains(concat(' ', concat(normalize-space(@class),' ')),' vcard ')]",
    MATCH_IS_HARDCODED,
    (const xmlChar*)"http://www.w3.org/2006/vcard/hcard2rdf.xsl"
  }
  ,
  { 
    NULL,
    0,
    0
  }
};


static const char* grddl_namespace_uris_ignore_list[]={
  "http://www.w3.org/1999/xhtml",
  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "http://www.w3.org/2001/XMLSchema",
  NULL
};


/* add URI to XSLT transformation URI list */
static void
raptor_grddl_add_transform_xml_context(raptor_grddl_parser_context* grddl_parser,
                                       grddl_xml_context* xml_context)
{
  int i;
  raptor_uri* uri=xml_context->uri;
  
#if RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("Found document transformation URI '%s'\n",
                raptor_uri_as_string(uri));
#endif

  for(i=0; i < raptor_sequence_size(grddl_parser->doc_transform_uris); i++) {
    grddl_xml_context* xc=(grddl_xml_context*)raptor_sequence_get_at(grddl_parser->doc_transform_uris, i);
    if(raptor_uri_equals(uri, xc->uri)) {
#if RAPTOR_DEBUG > 1
      RAPTOR_DEBUG2("Already seen XSLT URI '%s'\n", raptor_uri_as_string(uri));
#endif
      grddl_free_xml_context(xml_context);
      return;
    }
  }

  RAPTOR_DEBUG3("Adding new document transformation XSLT URI %s with base URI %s\n",
                (uri ? (const char*)raptor_uri_as_string(uri): "(NONE)"),
                (xml_context->base_uri ? (const char*)raptor_uri_as_string(xml_context->base_uri) : "(NONE)"));

  raptor_sequence_push(grddl_parser->doc_transform_uris, xml_context);
}


static void
raptor_grddl_filter_triples(void *user_data, const raptor_statement *statement)
{
  raptor_parser* rdf_parser=(raptor_parser*)user_data;
  raptor_grddl_parser_context* grddl_parser;
  int i;
  raptor_uri* predicate_uri;

  grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  /* Look for a triple <uri> <uri> <uri> */
  if(!statement->subject_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE ||
     !statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE ||
     !statement->object_type == RAPTOR_IDENTIFIER_TYPE_RESOURCE)
    return;

#if RAPTOR_DEBUG > 2
  RAPTOR_DEBUG2("Parser %p: Relaying statement: ", rdf_parser);
  raptor_print_statement(statement, stderr);
  fputc('\n', stderr);
#endif

#if RAPTOR_DEBUG > 1
  RAPTOR_DEBUG3("Parser %p: Checking against %d profile URIs\n", 
                rdf_parser, raptor_sequence_size(grddl_parser->profile_uris));
#endif
  
  /* Look for (i=0, root namespace URI)
   * <document-root-element-namespace-URI> data-view:namespaceTransformation ?tr
   * or (i>0, profile URIs)
   * <document-root-element-namespace-URI> data-view:profileTransformation ?tr
   * and then ?tr becomes a new document transformation URI
   */
  predicate_uri=grddl_parser->namespace_transformation_uri;
  for(i=0; i < raptor_sequence_size(grddl_parser->profile_uris); i++) {
    grddl_xml_context* xml_context=(grddl_xml_context*)raptor_sequence_get_at(grddl_parser->profile_uris, i);
    raptor_uri* profile_uri=xml_context->uri;
    grddl_xml_context* new_xml_context;
    
    if(i==1)
      predicate_uri=grddl_parser->profile_transformation_uri;
    
    if(!profile_uri)
      continue;
    
    if(raptor_uri_equals((raptor_uri*)statement->subject, profile_uri) &&
       raptor_uri_equals((raptor_uri*)statement->predicate, predicate_uri)) {
      raptor_uri* uri=(raptor_uri*)statement->object;
      
#if RAPTOR_DEBUG > 1
      RAPTOR_DEBUG4("Parser %p: Matches profile URI #%d '%s'\n",
                    rdf_parser, i, raptor_uri_as_string(profile_uri));
#endif
      
      new_xml_context=raptor_new_xml_context(uri, rdf_parser->base_uri);
      raptor_grddl_add_transform_xml_context(grddl_parser, new_xml_context);
    } else {
#if RAPTOR_DEBUG > 1
      RAPTOR_DEBUG4("Parser %p: Failed to match profile URI #%d '%s'\n",
                    rdf_parser, i, raptor_uri_as_string(profile_uri));
#endif
    }
    
  }

}


static int
raptor_grddl_ensure_internal_parser(raptor_parser* rdf_parser,
                                    const char* parser_name, int filter)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  if(!grddl_parser->internal_parser_name ||
     !strcmp(parser_name, "guess") ||
     strcmp(grddl_parser->internal_parser_name, parser_name)) {
    /* construct a new parser if none in use or not what is required */
    if(grddl_parser->internal_parser) {
      RAPTOR_DEBUG3("Parser %p: Freeing internal %s parser.\n",
                    rdf_parser, grddl_parser->internal_parser_name);
      
      raptor_free_parser(grddl_parser->internal_parser);
      grddl_parser->internal_parser=NULL;
      grddl_parser->internal_parser_name=NULL;
    }
    
    RAPTOR_DEBUG3("Parser %p: Allocating new internal %s parser.\n",
                  rdf_parser, parser_name);
    grddl_parser->internal_parser=raptor_new_parser(parser_name);
    if(!grddl_parser->internal_parser) {
      raptor_parser_error(rdf_parser, "Failed to create %s parser",
                          parser_name);
      return 1;
    } else {
      /* initialise the new parser with the outer state */
      grddl_parser->internal_parser_name=parser_name;
      raptor_parser_copy_user_state(grddl_parser->internal_parser,
                                    rdf_parser);
      grddl_parser->saved_user_data=rdf_parser->user_data;
      grddl_parser->saved_statement_handler=rdf_parser->statement_handler;
    }
  }

  /* Filter the triples for profile/namespace URIs */
  if(filter) {
    grddl_parser->internal_parser->user_data= rdf_parser;
    grddl_parser->internal_parser->statement_handler= raptor_grddl_filter_triples;
  } else {
    grddl_parser->internal_parser->user_data= grddl_parser->saved_user_data;
    grddl_parser->internal_parser->statement_handler= grddl_parser->saved_statement_handler;
  }

  return 0;
}


/* Run a GRDDL transform using a pre-parsed XSLT stylesheet already
 * formed into a libxml document (with URI)
 */
static int
raptor_grddl_run_grddl_transform_doc(raptor_parser* rdf_parser,
                                     grddl_xml_context* xml_context,
                                     xmlDocPtr xslt_doc,
                                     xmlDocPtr doc)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;
  int ret=0;
  xsltStylesheetPtr sheet=NULL;
  xmlDocPtr res=NULL;
  xmlChar *doc_txt=NULL;
  int doc_txt_len=0;
  const char* parser_name;
  const char* params[7];
  const unsigned char* base_uri_string;
  size_t base_uri_len;
  raptor_uri* xslt_uri;
  raptor_uri* base_uri;
  char *quoted_base_uri=NULL;

  xslt_uri=xml_context->uri;
  base_uri=xml_context->base_uri ? xml_context->base_uri : xml_context->uri;

  base_uri_string=raptor_uri_as_counted_string(base_uri, &base_uri_len);

  RAPTOR_DEBUG3("Running GRDDL transform with XSLT URI '%s' with doc base URI '%s'\n",
                raptor_uri_as_string(xslt_uri),
                base_uri_string);
  
  sheet = xsltParseStylesheetDoc(xslt_doc);
  if(!sheet) {
    raptor_parser_error(rdf_parser, "Failed to parse stylesheet in '%s'",
                        raptor_uri_as_string(xslt_uri));
    ret=1;
    goto cleanup_xslt;
  }

  xsltSetGenericErrorFunc(rdf_parser, raptor_grddl_xsltGenericError_handler);

#if 1
  /* FIXME:
   * Define 'base', 'Base' and 'url' params to allow some XSLT sheets to work:
   * base:
   *   http://www.w3.org/2000/07/uri43/uri.xsl
   * Base:
   *   http://www.w3.org/2000/08/w3c-synd/home2rss.xsl
   * url: (optional)
   *   http://www.w3.org/2001/sw/grddl-wg/td/RDFa2RDFXML.xsl
   */
  quoted_base_uri=(char*)RAPTOR_MALLOC(cstring, base_uri_len+3);
  quoted_base_uri[0]='\'';
  strncpy(quoted_base_uri+1, (const char*)base_uri_string, base_uri_len);
  quoted_base_uri[base_uri_len+1]='\'';
  quoted_base_uri[base_uri_len+2]='\0';
  
  params[0]="base";
  params[1]=(const char*)quoted_base_uri;
  params[2]="Base";
  params[3]=(const char*)quoted_base_uri;
  params[4]="url";
  params[5]=(const char*)quoted_base_uri;
  params[6]=NULL;

  res = xsltApplyStylesheet(sheet, doc, params);
#else
  res = xsltApplyStylesheet(sheet, doc, NULL); /* No params */
#endif
  if(!res) {
    raptor_parser_error(rdf_parser, "Failed to apply stylesheet in '%s'",
                        raptor_uri_as_string(xslt_uri));
    ret=1;
    goto cleanup_xslt;
  }

  if(res->type == XML_HTML_DOCUMENT_NODE) {
    if(sheet->method != NULL)
      xmlFree(sheet->method);
    sheet->method = (xmlChar*)xmlMalloc(5);
    strncpy((char*)sheet->method, "html", 5);
  }

  /* write the resulting XML to a string */
  xsltSaveResultToString(&doc_txt, &doc_txt_len, res, sheet);
  
  if(!doc_txt || !doc_txt_len) {
    /* FIXME: continue with an empty document? */
    raptor_parser_warning(rdf_parser, "XSLT returned an empty document");
    goto cleanup_xslt;
  }

  RAPTOR_DEBUG4("XSLT returned %d bytes document method %s media type %s\n",
                doc_txt_len,
                (sheet->method ? (const char*)sheet->method : "NULL"),
                (sheet->mediaType ? (const char*)sheet->mediaType : "NULL"));

  /* FIXME: Assumes mime types for XSLT <xsl:output method> */
  if(sheet->mediaType == NULL && sheet->method) {
    if(!(strcmp((const char*)sheet->method, "text"))) {
      sheet->mediaType = (xmlChar*)xmlMalloc(11);
      strncpy((char*)sheet->mediaType, "text/plain",11);
    } else if(!(strcmp((const char*)sheet->method, "xml"))) {
      sheet->mediaType = (xmlChar*)xmlMalloc(16);
      strncpy((char*)sheet->mediaType, "application/xml",16);
    } else if(!(strcmp((const char*)sheet->method, "html"))) {
      sheet->mediaType = (xmlChar*)xmlMalloc(10);
      /* FIXME: use xhtml mime type? */
      strncpy((char*)sheet->mediaType, "text/html",10);
    }
  }

  /* FIXME: Assume all that all media XML is RDF/XML and also that
   * with no information at all we have RDF/XML
   */
  if(!sheet->mediaType ||
     (sheet->mediaType &&
      !strcmp((const char*)sheet->mediaType, "application/xml"))) {
    if(sheet->mediaType)
      xmlFree(sheet->mediaType);
    sheet->mediaType = (xmlChar*)xmlMalloc(20);
    strncpy((char*)sheet->mediaType, "application/rdf+xml",20);
  }
  
  parser_name=raptor_guess_parser_name(NULL, (const char*)sheet->mediaType,
                                       doc_txt, doc_txt_len, NULL);
  if(!parser_name) {
    RAPTOR_DEBUG3("Parser %p: Guessed no parser from mime type '%s' and content - ending",
                  rdf_parser, sheet->mediaType);
    goto cleanup_xslt;
  }
  
  RAPTOR_DEBUG4("Parser %p: Guessed parser %s from mime type '%s' and content\n",
                rdf_parser, parser_name, sheet->mediaType);

  if(!strcmp((const char*)parser_name, "grddl")) {
    RAPTOR_DEBUG2("Parser %p: Ignoring guess to run grddl parser - ending",
                  rdf_parser);
    goto cleanup_xslt;
  }

  ret=raptor_grddl_ensure_internal_parser(rdf_parser, parser_name, 0);
  if(ret)
    goto cleanup_xslt;
  
  if(grddl_parser->internal_parser) {
    grddl_parser->internal_parser->default_generate_id_handler_base=
      raptor_parser_get_current_base_id(rdf_parser);

    /* generate the triples */
    raptor_start_parse(grddl_parser->internal_parser, base_uri);
    raptor_parse_chunk(grddl_parser->internal_parser, doc_txt, doc_txt_len, 1);

    rdf_parser->default_generate_id_handler_base=
      raptor_parser_get_current_base_id(grddl_parser->internal_parser);
  }
  
  cleanup_xslt:

  if(quoted_base_uri)
    RAPTOR_FREE(cstring, quoted_base_uri);
  
  if(doc_txt)    
    xmlFree(doc_txt);
  
  if(res)
    xmlFreeDoc(res);
  
  if(sheet)
    xsltFreeStylesheet(sheet);
  
  return ret;
}


typedef struct 
{
  raptor_parser* rdf_parser;
  xmlParserCtxtPtr xc;
  raptor_uri* base_uri;
} raptor_grddl_xml_parse_bytes_context;
  

static void
raptor_grddl_uri_xml_parse_bytes(raptor_www* www,
                                 void *userdata,
                                 const void *ptr, size_t size, size_t nmemb)
{
  raptor_grddl_xml_parse_bytes_context* xpbc=(raptor_grddl_xml_parse_bytes_context*)userdata;
  int len=size*nmemb;
  int rc=0;
  
  if(!xpbc->xc) {
    xmlParserCtxtPtr xc;
    
    xc = xmlCreatePushParserCtxt(NULL, NULL,
                                 (const char*)ptr, len,
                                 (const char*)raptor_uri_as_string(xpbc->base_uri));
    if(!xc)
      rc=1;
    else {
      int libxml_options = 0;

#ifdef RAPTOR_LIBXML_XML_PARSE_NONET
      if(xpbc->rdf_parser->features[RAPTOR_FEATURE_NO_NET])
        libxml_options |= XML_PARSE_NONET;
#endif
#ifdef HAVE_XMLCTXTUSEOPTIONS
      xmlCtxtUseOptions(xc, libxml_options);
#endif

      xc->replaceEntities = 1;
      xc->loadsubset = 1;
    }
    xpbc->xc=xc;
  } else
    rc=xmlParseChunk(xpbc->xc, (const char*)ptr, len, 0);

  if(rc)
    raptor_parser_error(xpbc->rdf_parser, "XML Parsing failed");
}


static void
raptor_grddl_discard_message(void *user_data, raptor_locator* locator,
                             const char *message)
{
  RAPTOR_DEBUG3("%s: Discarded error message: %s\n", 
                raptor_uri_as_string(locator->uri), message);
  return;
}


#define FETCH_IGNORE_ERRORS 1
#define FETCH_ACCEPT_XSLT   2

static int
raptor_grddl_fetch_uri(raptor_parser* rdf_parser, 
                       raptor_uri* uri,
                       raptor_www_write_bytes_handler write_bytes_handler,
                       void* write_bytes_user_data,
                       raptor_www_content_type_handler content_type_handler,
                       void* content_type_user_data,
                       int flags)
{
  raptor_www *www;
  const char *accept_h;
  int ret=0;
  int ignore_errors=(flags & FETCH_IGNORE_ERRORS);
  
  if(rdf_parser->features[RAPTOR_FEATURE_NO_NET]) {
    if(!raptor_uri_uri_string_is_file_uri(raptor_uri_as_string(uri)))
      return 1;
  }
  
  www=raptor_www_new();
  if(!www)
    return 1;
  
  raptor_www_set_user_agent(www, "grddl/0.1");
  
  if(flags & FETCH_ACCEPT_XSLT) {
    raptor_www_set_http_accept(www, "application/xml");
  } else {
    accept_h=raptor_parser_get_accept_header(rdf_parser);
    if(accept_h) {
      raptor_www_set_http_accept(www, accept_h);
      RAPTOR_FREE(cstring, accept_h);
    }
  }
  if(rdf_parser->uri_filter)
    raptor_www_set_uri_filter(www, rdf_parser->uri_filter,
                              rdf_parser->uri_filter_user_data);
  if(ignore_errors)
    raptor_www_set_error_handler(www, raptor_grddl_discard_message, NULL);
  else
    raptor_www_set_error_handler(www,
                                 rdf_parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].handler, 
                                 rdf_parser->error_handlers.handlers[RAPTOR_LOG_LEVEL_ERROR].user_data);
  raptor_www_set_write_bytes_handler(www, write_bytes_handler,
                                     write_bytes_user_data);
  raptor_www_set_content_type_handler(www, content_type_handler,
                                      content_type_user_data);

  if(rdf_parser->features[RAPTOR_FEATURE_WWW_TIMEOUT] > 0)
    raptor_www_set_connection_timeout(www, 
                                      rdf_parser->features[RAPTOR_FEATURE_WWW_TIMEOUT]);

  ret=raptor_www_fetch(www, uri);
  
  raptor_www_free(www);

  return ret;
}


/* Run a GRDDL transform using a XSLT stylesheet at a given URI */
static int
raptor_grddl_run_grddl_transform_uri(raptor_parser* rdf_parser,
                                     grddl_xml_context* xml_context, 
                                     xmlDocPtr doc)
{
  raptor_grddl_parser_context* grddl_parser;
  xmlParserCtxtPtr xslt_ctxt=NULL;
  raptor_grddl_xml_parse_bytes_context xpbc;
  int ret=0;
  raptor_uri* xslt_uri;
  raptor_uri* base_uri;
  raptor_uri* old_locator_uri;
  raptor_locator *locator=&rdf_parser->locator;

  xslt_uri=xml_context->uri;
  base_uri=xml_context->base_uri ? xml_context->base_uri : xml_context->uri;

  grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;
  
  RAPTOR_DEBUG3("Running GRDDL transform with XSLT URI %s and base URI %s\n",
                raptor_uri_as_string(xslt_uri),
                raptor_uri_as_string(base_uri));
  
  /* make an xsltStylesheetPtr via the raptor_grddl_uri_xml_parse_bytes 
   * callback as bytes are returned
   */
  xpbc.xc=NULL;
  xpbc.rdf_parser=rdf_parser;
  xpbc.base_uri=xslt_uri;

  old_locator_uri=locator->uri;
  locator->uri=xslt_uri;
  ret=raptor_grddl_fetch_uri(rdf_parser,
                             xslt_uri,
                             raptor_grddl_uri_xml_parse_bytes, &xpbc,
                             NULL, NULL,
                             FETCH_ACCEPT_XSLT);
  if(ret) {
    locator->uri=old_locator_uri;
    raptor_parser_warning(rdf_parser, "Fetching XSLT document URI '%s' failed",
                          raptor_uri_as_string(xslt_uri));
    ret=0;
  } else {
    xslt_ctxt=xpbc.xc;
    xmlParseChunk(xpbc.xc, NULL, 0, 1);
  
    ret=raptor_grddl_run_grddl_transform_doc(rdf_parser,
                                             xml_context,
                                             xslt_ctxt->myDoc,
                                             doc);
    locator->uri=old_locator_uri;
  }

  if(xslt_ctxt)
    xmlFreeParserCtxt(xslt_ctxt); 
  
  return ret;
}


static int
raptor_grddl_seen_uri(raptor_grddl_parser_context* grddl_parser,
                      raptor_uri* uri)
{
  int i;
  int seen=0;
  raptor_sequence* seq=grddl_parser->visited_uris;
  
  for(i=0; i < raptor_sequence_size(seq); i++) {
    raptor_uri* vuri=(raptor_uri*)raptor_sequence_get_at(seq, i);
    if(raptor_uri_equals(uri, vuri)) {
      seen=1;
      break;
    }
  }

  if(seen)
    RAPTOR_DEBUG2("Already seen URI '%s'\n", raptor_uri_as_string(uri));
  
  return seen;
}


static void
raptor_grddl_done_uri(raptor_grddl_parser_context* grddl_parser,
                      raptor_uri* uri)
{
  if(!grddl_parser->visited_uris)
    return;
  
  if(!raptor_grddl_seen_uri(grddl_parser, uri)) {
    raptor_sequence* seq=grddl_parser->visited_uris;
    raptor_sequence_push(seq, raptor_uri_copy(uri));
  }
}


static raptor_sequence*
raptor_grddl_run_xpath_match(raptor_parser* rdf_parser,
                             xmlDocPtr doc,
                             const xmlChar* xpathExpr,
                             int flags)
{
  raptor_grddl_parser_context* grddl_parser;
  /* Evaluate xpath expression */
  xmlXPathObjectPtr xpathObj=NULL;
  raptor_sequence* seq=NULL;
  xmlNodeSetPtr nodes;
  int i;
  
  grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  seq=raptor_new_sequence((raptor_sequence_free_handler*)grddl_free_xml_context, NULL);

  /* Evaluate xpath expression */
  xpathObj = xmlXPathEvalExpression(xpathExpr,
                                    grddl_parser->xpathCtx);
  if(!xpathObj) {
    raptor_parser_error(rdf_parser,
                        "Unable to evaluate XPath expression \"%s\"",
                        xpathExpr);
    raptor_free_sequence(seq); seq=NULL;
    goto cleanup_xpath_match;
  }

  nodes=xpathObj->nodesetval;
  if(!nodes || xmlXPathNodeSetIsEmpty(nodes)) {
#if RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("No match found with XPath expression \"%s\" over '%s'\n",
                  xpathExpr, raptor_uri_as_string(rdf_parser->base_uri));
#endif
    raptor_free_sequence(seq); seq=NULL;
    goto cleanup_xpath_match;
  }

#if RAPTOR_DEBUG > 1
  RAPTOR_DEBUG3("Found match with XPath expression \"%s\" over '%s'\n",
                xpathExpr, raptor_uri_as_string(rdf_parser->base_uri));
#endif
  
  for(i=0; i < xmlXPathNodeSetGetLength(nodes); i++) {
    xmlNodePtr node=nodes->nodeTab[i];
    const unsigned char* uri_string=NULL;
    xmlChar *base_uri_string;
    raptor_uri* base_uri=NULL;
    raptor_uri* uri=NULL;
    
    if(node->type != XML_ATTRIBUTE_NODE && 
       node->type != XML_ELEMENT_NODE) {
      raptor_parser_error(rdf_parser, "Got unexpected node type %d",
                          node->type);
      continue;
    }


    /* xmlNodeGetBase() returns base URI or NULL and must be freed
     * with xmlFree() 
     */
    if(grddl_parser->html_base_processing) {
      xmlElementType savedType=doc->type;
      doc->type=XML_HTML_DOCUMENT_NODE;
      base_uri_string=xmlNodeGetBase(doc, node);
      doc->type=savedType;
    } else
      base_uri_string=xmlNodeGetBase(doc, node);
    

    if(node->type == XML_ATTRIBUTE_NODE)
      uri_string=(const unsigned char*)node->children->content;
    else { /* XML_ELEMENT_NODE */
      if(node->ns)
        uri_string=(const unsigned char*)node->ns->href;
    }
    
    
    if(base_uri_string) {
      base_uri=raptor_new_uri(base_uri_string);
      xmlFree(base_uri_string);
#if RAPTOR_DEBUG > 1
      RAPTOR_DEBUG2("XML base URI of match is '%s'\n", raptor_uri_as_string(base_uri));
#endif
    } else if(rdf_parser->base_uri)
      base_uri=raptor_uri_copy(rdf_parser->base_uri);
    else
      base_uri=NULL;
    
    if(flags & MATCH_IS_VALUE_LIST) {
      char *start;
      char *end;
      char* buffer;
      size_t list_len=strlen((const char*)uri_string);
      
      buffer=(char*)RAPTOR_MALLOC(cstring, list_len+1);
      strncpy(buffer, (const char*)uri_string, list_len+1);
      
      for(start=end=buffer; end; start=end+1) {
        grddl_xml_context* xml_context;

        end=strchr(start, ' ');
        if(end)
          *end='\0';
        
        if(start == end)
          continue;
        
#if RAPTOR_DEBUG
        RAPTOR_DEBUG2("Got list match URI '%s'\n", start);
#endif
        
        uri=raptor_new_uri_relative_to_base(base_uri,
                                            (const unsigned char*)start);
        if(flags & MATCH_IS_PROFILE &&
           !strcmp((const char*)raptor_uri_as_string(uri),
                   "http://www.w3.org/2003/g/data-view'")) {
          raptor_free_uri(uri);
          continue;
        }

        xml_context=raptor_new_xml_context(uri, base_uri);
        raptor_sequence_push(seq, xml_context);
      }
      RAPTOR_FREE(cstring, buffer);
    } else if (flags & MATCH_IS_HARDCODED) {
#if RAPTOR_DEBUG
      RAPTOR_DEBUG2("Got hardcoded XSLT match for %s\n", xpathExpr);
#endif
      /* return at first match, that's enough */
      break;
    } else {
      grddl_xml_context* xml_context;
#if RAPTOR_DEBUG
      RAPTOR_DEBUG2("Got single match URI '%s'\n", uri_string);
#endif

      uri=raptor_new_uri_relative_to_base(base_uri, uri_string);
      xml_context=raptor_new_xml_context(uri, base_uri);
      raptor_sequence_push(seq, xml_context);
      raptor_free_uri(uri);
    }
    
    if(base_uri)
      raptor_free_uri(base_uri);
  }

  cleanup_xpath_match:
  if(xpathObj)
    xmlXPathFreeObject(xpathObj);
  
  return seq;
}


static void
raptor_grddl_check_recursive_content_type_handler(raptor_www* www,
                                                  void* userdata, 
                                                  const char* content_type)
{
  raptor_parser* rdf_parser=(raptor_parser*)userdata;
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;
  size_t len;

  if(!content_type)
    return;

  len=strlen(content_type)+1;
  if(grddl_parser->content_type)
    RAPTOR_FREE(cstring,grddl_parser->content_type);
  grddl_parser->content_type=(char*)RAPTOR_MALLOC(cstring, len+1);
  strncpy(grddl_parser->content_type, content_type, len+1);

  if(!strncmp(content_type, "application/rdf+xml", 19)) {
    grddl_parser->process_this_as_rdfxml=1;

    RAPTOR_DEBUG2("Parser %p: Found RDF/XML content type\n", rdf_parser);
    raptor_parser_save_content(rdf_parser, 1);
  }

  if(!strncmp(content_type, "text/html", 9) ||
     !strncmp(content_type, "application/html+xml", 20)) {
    RAPTOR_DEBUG3("Parser %p: Found HTML content type '%s'\n",
                  rdf_parser, content_type);
    grddl_parser->html_base_processing=1;
  }

}

#define RECURSIVE_FLAGS_IGNORE_ERRORS 1
#define RECURSIVE_FLAGS_FILTER        2

static int
raptor_grddl_run_recursive(raptor_parser* rdf_parser, raptor_uri* uri,
                           const char *parser_name, int flags)
{
  raptor_grddl_parser_context* grddl_parser;
  raptor_www_content_type_handler content_type_handler=NULL;
  int ret=0;
  const unsigned char* ibuffer=NULL;
  size_t ibuffer_len=0;
  raptor_parse_bytes_context rpbc;
  int ignore_errors=(flags & RECURSIVE_FLAGS_IGNORE_ERRORS) > 0;
  int filter=(flags & RECURSIVE_FLAGS_FILTER) > 0;
  int fetch_uri_flags=0;
  int is_grddl=!strcmp(parser_name, "grddl");
  
  grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  if(raptor_grddl_seen_uri(grddl_parser, uri))
    return 0;

  if(is_grddl)
    content_type_handler=raptor_grddl_check_recursive_content_type_handler;
  
  if(raptor_grddl_ensure_internal_parser(rdf_parser, parser_name, filter))
    return !ignore_errors;
  
  RAPTOR_DEBUG3("Running recursive %s operation on URI '%s'\n",
                parser_name, raptor_uri_as_string(uri));
  
  grddl_parser->internal_parser->default_generate_id_handler_base= 
    raptor_parser_get_current_base_id(rdf_parser);

  if(is_grddl)
    raptor_grddl_parser_add_parent(grddl_parser->internal_parser, grddl_parser);
  
  rpbc.rdf_parser=grddl_parser->internal_parser;
  rpbc.base_uri=NULL;
  rpbc.final_uri=NULL;
  rpbc.started=0;
  
  if(ignore_errors)
    fetch_uri_flags |=FETCH_IGNORE_ERRORS;
  
  if(raptor_grddl_fetch_uri(grddl_parser->internal_parser,
                            uri,
                            raptor_parse_uri_write_bytes, &rpbc,
                            content_type_handler, grddl_parser->internal_parser,
                            fetch_uri_flags)) {
    if(!ignore_errors)
      raptor_parser_warning(rdf_parser,
                            "Fetching GRDDL document URI '%s' failed\n",
                            raptor_uri_as_string(uri));
    ret=0;
    goto tidy;
  }

  if(ignore_errors) {
    raptor_error_handlers* eh=&grddl_parser->internal_parser->error_handlers;
    int i;
    /* NOTE not setting RAPTOR_LOG_LEVEL_NONE handler */
    for(i=1; i <= (int)eh->last_log_level; i++) {
      eh->handlers[i].handler=raptor_grddl_discard_message;
      eh->handlers[i].user_data=NULL;
    }
  }
  
  raptor_parse_chunk(grddl_parser->internal_parser, NULL, 0, 1);
  rdf_parser->default_generate_id_handler_base=
    raptor_parser_get_current_base_id(grddl_parser->internal_parser);

  /* If content was saved, process it as RDF/XML */
  ibuffer=raptor_parser_get_content(grddl_parser->internal_parser,
                                    &ibuffer_len);
  if(ibuffer && strcmp(parser_name, "rdfxml")) {
    RAPTOR_DEBUG2("Running additional RDF/XML parse on URI '%s' content\n",
                  raptor_uri_as_string(uri));
    
    if(raptor_grddl_ensure_internal_parser(rdf_parser, "rdfxml", 1))
      ret=1;
    else {
      grddl_parser->internal_parser->default_generate_id_handler_base=
        raptor_parser_get_current_base_id(rdf_parser);

      if(raptor_start_parse(grddl_parser->internal_parser, uri))
        ret=1;
      else {
        ret=raptor_parse_chunk(grddl_parser->internal_parser, ibuffer, 
                               ibuffer_len, 1);
        rdf_parser->default_generate_id_handler_base=
          raptor_parser_get_current_base_id(grddl_parser->internal_parser);
      }      
    }
    
    RAPTOR_FREE(cstring, ibuffer);
    raptor_parser_save_content(grddl_parser->internal_parser, 0);
  }

  if(rpbc.final_uri)
    raptor_free_uri(rpbc.final_uri);

  if(ignore_errors)
    ret=0;

 tidy:

  return ret;
}


static void
raptor_grddl_libxml_discard_error(void* user_data, const char *msg, ...)
{
  return;
}


static int
raptor_grddl_parse_chunk(raptor_parser* rdf_parser,
                         const unsigned char *s, size_t len,
                         int is_end)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;
  int i;
  int ret=0;
  const unsigned char* uri_string;
  raptor_uri* uri;
  /* XML document DOM */
  xmlDocPtr doc;
  int expri;
  const unsigned char* buffer=NULL;
  size_t buffer_len=0;
  int buffer_is_libxml=0;
  int loop;
  raptor_error_handlers eh;

  if(grddl_parser->content_type && !grddl_parser->content_type_check) {
    grddl_parser->content_type_check++;
    if(!strncmp(grddl_parser->content_type, "application/rdf+xml", 19)) {
      RAPTOR_DEBUG3("Parser %p: Found document with type '%s' is RDF/XML\n",
                    rdf_parser, grddl_parser->content_type);
      grddl_parser->process_this_as_rdfxml=1;
    }
    if(!strncmp(grddl_parser->content_type, "text/html", 9) ||
       !strncmp(grddl_parser->content_type, "application/html+xml", 20)) {
      RAPTOR_DEBUG3("Parser %p: Found document with type '%s' is HTML\n",
                    rdf_parser, grddl_parser->content_type);
      grddl_parser->html_base_processing=1;
    }
  }
  
  if(!grddl_parser->sb)
    grddl_parser->sb=raptor_new_stringbuffer();

  raptor_stringbuffer_append_counted_string(grddl_parser->sb, s, len, 1);
    
  if(!is_end)
    return 0;

  buffer_len=raptor_stringbuffer_length(grddl_parser->sb);
  buffer=(const unsigned char*)RAPTOR_MALLOC(cstring, buffer_len+1);
  if(buffer)
    raptor_stringbuffer_copy_to_string(grddl_parser->sb, 
                                       (unsigned char*)buffer, buffer_len);
  

  uri_string=raptor_uri_as_string(rdf_parser->base_uri);

  if(1) {
    raptor_error_handlers_init(&eh);
    eh.last_log_level=rdf_parser->error_handlers.last_log_level;

    /* Save error handlers and discard parsing errors
     * NOTE not setting RAPTOR_LOG_LEVEL_NONE handler
     */
    memcpy(&eh.handlers, &rdf_parser->error_handlers,
           sizeof(raptor_message_handler_closure) * (1+eh.last_log_level));
    for(i=1; i <= (int)eh.last_log_level; i++) {
      rdf_parser->error_handlers.handlers[i].handler=raptor_grddl_discard_message;
      rdf_parser->error_handlers.handlers[i].user_data=NULL;
    }
  }

  RAPTOR_DEBUG4("Parser %p: URI %s: processing %d bytes of content\n",
                rdf_parser, uri_string, (int)buffer_len);

  for(loop=0; loop<2; loop++) {
    int rc;

    if(loop == 0) { 
      int libxml_options = 0;

      RAPTOR_DEBUG2("Parser %p: Creating an XML parser\n", rdf_parser);

      /* try to create an XML parser context */
      grddl_parser->xml_ctxt = xmlCreatePushParserCtxt(NULL, NULL,
                                                       (const char*)buffer,
                                                       buffer_len,
                                                       (const char*)uri_string);
      if(!grddl_parser->xml_ctxt) {
        RAPTOR_DEBUG2("Parser %p: Creating an XML parser failed\n", rdf_parser);
        continue;
      }

#ifdef RAPTOR_LIBXML_XML_PARSE_NONET
      if(rdf_parser->features[RAPTOR_FEATURE_NO_NET])
        libxml_options |= XML_PARSE_NONET;
#endif
#ifdef HAVE_XMLCTXTUSEOPTIONS
      xmlCtxtUseOptions(grddl_parser->xml_ctxt, libxml_options);
#endif


      grddl_parser->xml_ctxt->vctxt.warning = raptor_grddl_libxml_discard_error;
      grddl_parser->xml_ctxt->vctxt.error = raptor_grddl_libxml_discard_error;
    
      grddl_parser->xml_ctxt->replaceEntities = 1;
      grddl_parser->xml_ctxt->loadsubset = 1;
    } else if (loop == 1) {
  
      /* try to create an HTML parser context */
      if(rdf_parser->features[RAPTOR_FEATURE_HTML_TAG_SOUP]) {
        xmlCharEncoding enc;
        int options;

        RAPTOR_DEBUG2("Parser %p: Creating an HTML parser\n", rdf_parser);

        enc = xmlDetectCharEncoding((const unsigned char*)buffer, buffer_len);
        grddl_parser->html_ctxt = htmlCreatePushParserCtxt(/*sax*/ NULL, 
                                                         /*user_data*/ NULL,
                                                         (const char *)buffer,
                                                         buffer_len,
                                                         (const char *)uri_string,
                                                         enc);
        if(!grddl_parser->html_ctxt) {
          RAPTOR_DEBUG2("Parser %p: Creating an HTML parser failed\n", rdf_parser);
          continue;
        }
  
        /* HTML parser */
        grddl_parser->html_ctxt->replaceEntities = 1;
        grddl_parser->html_ctxt->loadsubset = 1;
    
        grddl_parser->html_ctxt->vctxt.error = raptor_grddl_libxml_discard_error;
    
        /* HTML_PARSE_NOWARNING disables sax->warning, vxtxt.warning  */
        /* HTML_PARSE_NOERROR disables sax->error, vctxt.error */
        options = HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING;
#ifdef HTML_PARSE_RECOVER
        options |= HTML_PARSE_RECOVER;
#endif
#ifdef RAPTOR_LIBXML_HTML_PARSE_NONET
      if(rdf_parser->features[RAPTOR_FEATURE_NO_NET])
        options |= HTML_PARSE_NONET;
#endif

        htmlCtxtUseOptions(grddl_parser->html_ctxt, options);
 
      } else
        continue;
    } else
      continue;

    xmlSetStructuredErrorFunc(&rdf_parser->error_handlers, 
                              raptor_libxml_xmlStructuredErrorFunc);

    rc=0;

    if(grddl_parser->html_ctxt) {
      RAPTOR_DEBUG2("Parser %p: Parsing as HTML\n", rdf_parser);
      rc=htmlParseChunk(grddl_parser->html_ctxt, (const char*)s, 0, 1);
      RAPTOR_DEBUG3("Parser %p: Parsing as HTML %s\n", rdf_parser, (rc ? "failed" : "succeeded"));
      if(rc) {
        if(grddl_parser->html_ctxt->myDoc) {
          xmlFreeDoc(grddl_parser->html_ctxt->myDoc);
          grddl_parser->html_ctxt->myDoc=NULL;
        }
        htmlFreeParserCtxt(grddl_parser->html_ctxt);
        grddl_parser->html_ctxt=NULL;
      }
    } else {
      RAPTOR_DEBUG2("Parser %p: Parsing as XML\n", rdf_parser);
      rc=xmlParseChunk(grddl_parser->xml_ctxt, (const char*)s, 0, 1);
      RAPTOR_DEBUG3("Parser %p: Parsing as XML %s\n", rdf_parser, (rc ? "failed" : "succeeded"));
      if(rc) {
        if(grddl_parser->xml_ctxt->myDoc) {
          xmlFreeDoc(grddl_parser->xml_ctxt->myDoc);
          grddl_parser->xml_ctxt->myDoc=NULL;
        }
        xmlFreeParserCtxt(grddl_parser->xml_ctxt);
        grddl_parser->xml_ctxt=NULL;
      }
    }

    if(!rc)
      break;

  }
  
  if(1) {
    /* Restore error handlers */
    for(i=1; i<= (int)eh.last_log_level; i++)
      rdf_parser->error_handlers.handlers[i].handler=eh.handlers[i].handler;
      rdf_parser->error_handlers.handlers[i].user_data=eh.handlers[i].user_data;
  }

  if(!grddl_parser->html_ctxt && !grddl_parser->xml_ctxt) {
    raptor_parser_error(rdf_parser, "Failed to create HTML or XML parsers");
    ret=1;
    goto tidy;
  }
  
  raptor_grddl_done_uri(grddl_parser, rdf_parser->base_uri);

  if(grddl_parser->html_ctxt)
    doc=grddl_parser->html_ctxt->myDoc;
  else
    doc=grddl_parser->xml_ctxt->myDoc;
  if(!doc) {
    raptor_parser_error(rdf_parser, 
                        "Failed to create XML DOM for GRDDL document");
    ret=1;
    goto tidy;
  }

  if(!grddl_parser->grddl_processing)
    goto transform;
  

  if(grddl_parser->xinclude_processing) {
    RAPTOR_DEBUG3("Parser %p: Running XInclude processing on URI '%s'\n",
                  rdf_parser, raptor_uri_as_string(rdf_parser->base_uri));
    if(xmlXIncludeProcess(doc) < 0) {
      raptor_parser_error(rdf_parser, 
                          "XInclude processing failed for GRDDL document");
      ret=1;
      goto tidy;
    } else {
      int blen;
      
      /* write the result of XML Include to buffer */
      RAPTOR_FREE(cstring, buffer);
      xmlDocDumpFormatMemory(doc, (xmlChar**)&buffer, &blen,
                             1 /* indent the result */);
      buffer_len=blen;
      buffer_is_libxml=1;
      
      RAPTOR_DEBUG3("Parser %p: XML Include processing returned %d bytes document\n",
                    rdf_parser, (int)buffer_len);
    }
  }


  RAPTOR_DEBUG3("Parser %p: Running top-level GRDDL on URI '%s'\n",
                rdf_parser, raptor_uri_as_string(rdf_parser->base_uri));

  /* Work out if there is a root namespace URI */
  if(1) {
    xmlNodePtr xnp;
    xmlNsPtr rootNs = NULL;
    const unsigned char* ns_uri_string=NULL;

    xnp = xmlDocGetRootElement(doc);
    if(xnp) {
      rootNs = xnp->ns;
      if(rootNs)
        ns_uri_string = (const unsigned char*)(rootNs->href);
    }
    
    if(ns_uri_string) {
      int n;
      
      RAPTOR_DEBUG3("Parser %p: Root namespace URI is %s\n", 
                    rdf_parser, ns_uri_string);

      if(!strcmp((const char*)ns_uri_string,
                 (const char*)raptor_rdf_namespace_uri) &&
         !strcmp((const char*)xnp->name, "RDF")) {
        RAPTOR_DEBUG3("Parser %p: Root element of %s is rdf:RDF - process this as RDF/XML later\n",
                      rdf_parser, raptor_uri_as_string(rdf_parser->base_uri));
        grddl_parser->process_this_as_rdfxml=1;
      }

      for(n=0; grddl_namespace_uris_ignore_list[n]; n++) {
        if(!strcmp(grddl_namespace_uris_ignore_list[n],
                   (const char*)ns_uri_string)) {
          /* ignore this namespace */
          RAPTOR_DEBUG3("Parser %p: Ignoring GRDDL for namespace URI '%s'\n",
                        rdf_parser, ns_uri_string);
          ns_uri_string=NULL;
          break;
        }
      }
      if(ns_uri_string) {
        grddl_xml_context* xml_context;

        grddl_parser->root_ns_uri=raptor_new_uri_relative_to_base(rdf_parser->base_uri, 
                                                                  ns_uri_string);
        xml_context=raptor_new_xml_context(grddl_parser->root_ns_uri,
                                           rdf_parser->base_uri);
        raptor_sequence_push(grddl_parser->profile_uris, xml_context);

        RAPTOR_DEBUG3("Parser %p: Processing GRDDL namespace URI '%s'\n",
                      rdf_parser,
                      raptor_uri_as_string(grddl_parser->root_ns_uri));
        raptor_grddl_run_recursive(rdf_parser, grddl_parser->root_ns_uri, 
                                   "grddl",
                                   RECURSIVE_FLAGS_IGNORE_ERRORS |
                                   RECURSIVE_FLAGS_FILTER);
      }
      
    }
  }

  /* Always put something at the start of the list even if NULL 
   * so later it can be searched for in output triples
   */
  if(!grddl_parser->root_ns_uri) {
    grddl_xml_context* xml_context;
    xml_context=raptor_new_xml_context(NULL, NULL);
    raptor_sequence_push(grddl_parser->profile_uris, xml_context);
  }


  /* Create the XPath evaluation context */
  if(!grddl_parser->xpathCtx) {
    grddl_parser->xpathCtx = xmlXPathNewContext(doc);
    if(!grddl_parser->xpathCtx) {
      raptor_parser_error(rdf_parser,
                          "Failed to create XPath context for GRDDL document");
      ret=1;
      goto tidy;
    }
    
    xmlXPathRegisterNs(grddl_parser->xpathCtx,
                       (const xmlChar*)"html",
                       (const xmlChar*)"http://www.w3.org/1999/xhtml");
    xmlXPathRegisterNs(grddl_parser->xpathCtx,
                       (const xmlChar*)"dataview",
                       (const xmlChar*)"http://www.w3.org/2003/g/data-view#");
  }
  
  /* Try <head profile> URIs */
  if(1) {
    raptor_sequence* result;
    result=raptor_grddl_run_xpath_match(rdf_parser, doc, 
                                        (const xmlChar*)"/html:html/html:head/@profile",
                                        MATCH_IS_VALUE_LIST | MATCH_IS_PROFILE);
    if(result) {
      RAPTOR_DEBUG4("Parser %p: Found %d <head profile> URIs in URI '%s'\n",
                    rdf_parser, raptor_sequence_size(result),
                    raptor_uri_as_string(rdf_parser->base_uri));


      /* Store profile URIs, skipping NULLs or the GRDDL profile itself */
      while(raptor_sequence_size(result)) {
        grddl_xml_context* xml_context;

        xml_context=(grddl_xml_context*)raptor_sequence_unshift(result);
        if(!xml_context)
          continue;
        uri=xml_context->uri;
        if(!strcmp("http://www.w3.org/2003/g/data-view",
                   (const char*)raptor_uri_as_string(uri))) {
          RAPTOR_DEBUG3("Ignoring <head profile> of URI %s: URI %s\n",
                        raptor_uri_as_string(rdf_parser->base_uri),
                        raptor_uri_as_string(uri));
          grddl_free_xml_context(xml_context);
          continue;
        }
        raptor_sequence_push(grddl_parser->profile_uris, xml_context);
      }
      raptor_free_sequence(result);


      /* Recursive GRDDL through all the <head profile> URIs */
      for(i=1; i < raptor_sequence_size(grddl_parser->profile_uris); i++) {
        grddl_xml_context* xml_context=(grddl_xml_context*)raptor_sequence_get_at(grddl_parser->profile_uris, i);
        uri=xml_context->uri;
        if(!uri)
          continue;

        RAPTOR_DEBUG4("Processing <head profile> #%d of URI %s: URI %s\n",
                      i, raptor_uri_as_string(rdf_parser->base_uri),
                        raptor_uri_as_string(uri));
        ret=raptor_grddl_run_recursive(rdf_parser, uri, 
                                       "grddl",
                                       RECURSIVE_FLAGS_IGNORE_ERRORS|
                                       RECURSIVE_FLAGS_FILTER);
      }
    }

  } /* end head profile URIs */


  /* Try XHTML document with alternate forms
   * <link type="application/rdf+xml" href="URI" />
   * Value of @href is a URI
   */
  if(grddl_parser->html_link_processing &&
     rdf_parser->features[RAPTOR_FEATURE_HTML_LINK]) {
    raptor_sequence* result;
    result=raptor_grddl_run_xpath_match(rdf_parser, doc, 
                                        (const xmlChar*)"/html:html/html:head/html:link[@type=\"application/rdf+xml\"]/@href",
                                        0);
    if(result) {
      RAPTOR_DEBUG4("Parser %p: Found %d <link> URIs in URI '%s'\n",
                    rdf_parser, raptor_sequence_size(result),
                    raptor_uri_as_string(rdf_parser->base_uri));

      /* Recursively parse all the <link> URIs, skipping NULLs */
      i=0;
      while(raptor_sequence_size(result)) {
        grddl_xml_context* xml_context;

        xml_context=(grddl_xml_context*)raptor_sequence_unshift(result);
        if(!xml_context)
          continue;

        uri=xml_context->uri;
        if(uri) {
          RAPTOR_DEBUG4("Processing <link> #%d of URI %s: URI %s\n",
                        i, raptor_uri_as_string(rdf_parser->base_uri),
                        raptor_uri_as_string(uri));
          i++;
          ret=raptor_grddl_run_recursive(rdf_parser, uri, "guess",
                                         RECURSIVE_FLAGS_IGNORE_ERRORS);
        }
        grddl_free_xml_context(xml_context);
      }

      raptor_free_sequence(result);
    }
  }
  
  
  /* Try all XPaths */
  for(expri=0; match_table[expri].xpath; expri++) {
    raptor_sequence* result;
    int flags=match_table[expri].flags;

    if((flags & MATCH_IS_HARDCODED) && 
       !rdf_parser->features[RAPTOR_FEATURE_MICROFORMATS])
      continue;
    
    result=raptor_grddl_run_xpath_match(rdf_parser, doc, 
                                        match_table[expri].xpath, flags);
    if(result) {
      if(match_table[expri].xslt_sheet_uri) {
        grddl_xml_context* xml_context;

        /* Ignore what matched, use a hardcoded XSLT URI */
        uri_string=match_table[expri].xslt_sheet_uri;
        RAPTOR_DEBUG3("Parser %p: Using hard-coded XSLT URI '%s'\n",
                      rdf_parser, uri_string);

        raptor_free_sequence(result);
        result=raptor_new_sequence((raptor_sequence_free_handler*)grddl_free_xml_context, NULL);
        
        uri=raptor_new_uri_relative_to_base(rdf_parser->base_uri, uri_string);

        xml_context=raptor_new_xml_context(uri, rdf_parser->base_uri);
        raptor_sequence_push(result, xml_context);

        raptor_free_uri(uri);
      }
      
      while(raptor_sequence_size(result)) {
        grddl_xml_context* xml_context=(grddl_xml_context*)raptor_sequence_unshift(result);
        if(!xml_context)
          break;

        raptor_grddl_add_transform_xml_context(grddl_parser, xml_context);
      }
      raptor_free_sequence(result);

      if(flags & MATCH_LAST)
        break;
    }

    
    if(rdf_parser->failed)
      break;

  } /* end XPath expression loop */
  
  if(rdf_parser->failed) {
    ret=1;
    goto tidy;
  }


  /* Process this document's content buffer as RDF/XML */
  if(grddl_parser->process_this_as_rdfxml && buffer) {
    RAPTOR_DEBUG3("Parser %p: Running additional RDF/XML parse on root document URI '%s' content\n",
                  rdf_parser, raptor_uri_as_string(rdf_parser->base_uri));
    
    if(raptor_grddl_ensure_internal_parser(rdf_parser, "rdfxml", 0))
      ret=1;
    else {
      grddl_parser->internal_parser->default_generate_id_handler_base=
        raptor_parser_get_current_base_id(rdf_parser);

      if(raptor_start_parse(grddl_parser->internal_parser, 
                            rdf_parser->base_uri))
        ret=1;
      else {
        ret=raptor_parse_chunk(grddl_parser->internal_parser, buffer, 
                               buffer_len, 1);
        rdf_parser->default_generate_id_handler_base=
          raptor_parser_get_current_base_id(grddl_parser->internal_parser);
      }
    }
    
  }

  
  /* Apply all transformation URIs seen */
  transform:
  while(raptor_sequence_size(grddl_parser->doc_transform_uris)) {
    grddl_xml_context* xml_context=(grddl_xml_context*)raptor_sequence_unshift(grddl_parser->doc_transform_uris);
    ret=raptor_grddl_run_grddl_transform_uri(rdf_parser, xml_context, doc);
    grddl_free_xml_context(xml_context);
    if(ret)
      break;
  }


 tidy:
  if(buffer) {
    if(buffer_is_libxml)
      xmlFree((xmlChar*)buffer);
    else
      RAPTOR_FREE(cstring, buffer);
  }

  if(grddl_parser->sb) {
    raptor_free_stringbuffer(grddl_parser->sb);
    grddl_parser->sb=NULL;
  }

  if(grddl_parser->xml_ctxt) {
    if(grddl_parser->xml_ctxt->myDoc) {
      xmlFreeDoc(grddl_parser->xml_ctxt->myDoc);
      grddl_parser->xml_ctxt->myDoc=NULL;
    }
    xmlFreeParserCtxt(grddl_parser->xml_ctxt);
    grddl_parser->xml_ctxt=NULL;
  }
  if(grddl_parser->html_ctxt) {
    if(grddl_parser->html_ctxt->myDoc) {
      xmlFreeDoc(grddl_parser->html_ctxt->myDoc);
      grddl_parser->html_ctxt->myDoc=NULL;
    }
    xmlFreeParserCtxt(grddl_parser->html_ctxt);
    grddl_parser->html_ctxt=NULL;
  }

  if(grddl_parser->xpathCtx) {
    xmlXPathFreeContext(grddl_parser->xpathCtx);
    grddl_parser->xpathCtx=NULL;
  }

  return (ret != 0);
}


static int
raptor_grddl_parse_recognise_syntax(raptor_parser_factory* factory,
                                    const unsigned char *buffer, size_t len,
                                    const unsigned char *identifier,
                                    const unsigned char *suffix,
                                    const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "xhtml"))
      score=7;
    if(!strcmp((const char*)suffix, "html"))
      score=2;
  }
  
  if(identifier) {
    if(strstr((const char*)identifier, "xhtml"))
      score+=5;
  }
  
  return score;
}


static void
raptor_grddl_parse_content_type_handler(raptor_parser* rdf_parser, 
                                        const char* content_type)
{
  raptor_grddl_parser_context* grddl_parser=(raptor_grddl_parser_context*)rdf_parser->context;

  if(content_type) {
    size_t len=strlen(content_type)+1;
    if(grddl_parser->content_type)
      RAPTOR_FREE(cstring,grddl_parser->content_type);
    
    grddl_parser->content_type=(char*)RAPTOR_MALLOC(cstring, len+1);
    strncpy(grddl_parser->content_type, content_type, len+1);
  }
}



static int
raptor_grddl_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_grddl_parser_context);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_grddl_parse_init;
  factory->terminate = raptor_grddl_parse_terminate;
  factory->start     = raptor_grddl_parse_start;
  factory->chunk     = raptor_grddl_parse_chunk;
  factory->recognise_syntax = raptor_grddl_parse_recognise_syntax;
  factory->content_type_handler= raptor_grddl_parse_content_type_handler;

  rc+= raptor_parser_factory_add_mime_type(factory, "text/html", 2) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/xhtml+xml", 4) != 0;

  return rc;
}


static xsltSecurityPrefsPtr raptor_xslt_sec = NULL;

int
raptor_init_parser_grddl_common(void)
{
#ifdef HAVE_XSLTINIT
  xsltInit();
#endif

  raptor_xslt_sec = xsltNewSecurityPrefs();
  xsltSetDefaultSecurityPrefs(raptor_xslt_sec);


  /* no read from file (read from URI with scheme = file) */
  xsltSetSecurityPrefs(raptor_xslt_sec, XSLT_SECPREF_READ_FILE,
                       xsltSecurityForbid);
  
  /* no create/write to file */
  xsltSetSecurityPrefs(raptor_xslt_sec, XSLT_SECPREF_WRITE_FILE,
                       xsltSecurityForbid);
  
  /* no create directory */
  xsltSetSecurityPrefs(raptor_xslt_sec, XSLT_SECPREF_CREATE_DIRECTORY,
                       xsltSecurityForbid);
  
  /* yes read from URI with scheme != file (XSLT_SECPREF_READ_NETWORK) */
  
  /* no write to network (you can 'write' with GET params anyway) */
  xsltSetSecurityPrefs(raptor_xslt_sec, XSLT_SECPREF_WRITE_NETWORK,
                       xsltSecurityForbid);

  return 0;
}


int
raptor_init_parser_grddl(void)
{
  return !raptor_parser_register_factory("grddl", 
                                         "Gleaning Resource Descriptions from Dialects of Languages",
                                         &raptor_grddl_parser_register_factory);
}


void
raptor_terminate_parser_grddl_common(void)
{
  xsltCleanupGlobals();

  if(raptor_xslt_sec)
    xsltFreeSecurityPrefs(raptor_xslt_sec);
}

