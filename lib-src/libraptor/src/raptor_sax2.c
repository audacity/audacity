/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_sax2.c - Raptor SAX2 API
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


/* Define this for far too much output */
#undef RAPTOR_DEBUG_CDATA


int
raptor_sax2_init(void)
{
#ifdef RAPTOR_XML_LIBXML
  xmlInitParser();
#endif
  return 0;
}


void
raptor_sax2_finish(void)
{
#ifdef RAPTOR_XML_LIBXML
  xmlCleanupParser();
  xmlSetStructuredErrorFunc(NULL, NULL);
  xmlSetGenericErrorFunc(NULL, NULL);
#endif
}


/**
 * raptor_new_sax2:
 * @user_data: pointer context information to pass to handlers
 * @error_handlers: error handlers pointer
 *
 * Constructor - Create a new SAX2 with error handlers
 *
 * Return value: new #raptor_sax2 object or NULL on failure
 */
raptor_sax2*
raptor_new_sax2(void* user_data, raptor_error_handlers* error_handlers)
{
  raptor_sax2* sax2;
  sax2=(raptor_sax2*)RAPTOR_CALLOC(raptor_sax2, 1, sizeof(raptor_sax2));
  if(!sax2)
    return NULL;

#ifdef RAPTOR_XML_LIBXML
  sax2->magic=RAPTOR_LIBXML_MAGIC;
#endif

  sax2->user_data=user_data;

  sax2->locator=error_handlers->locator;
  
  sax2->error_handlers=error_handlers;

#ifdef RAPTOR_XML_LIBXML
  /* sets xmlGenericErrorContext and xmlStructuredError */
  xmlSetStructuredErrorFunc(&sax2->error_handlers, 
                            raptor_libxml_xmlStructuredErrorFunc);
  /* sets xmlGenericErrorContext and xmlGenericError */
  xmlSetGenericErrorFunc(&sax2->error_handlers, 
                         (xmlGenericErrorFunc)raptor_libxml_generic_error);
#endif
  
  return sax2;
}


/**
 * raptor_free_sax2:
 * @sax2: SAX2 object
 *
 * Destructor - destroy a SAX2 object
 */
void
raptor_free_sax2(raptor_sax2 *sax2) {
  raptor_xml_element *xml_element;

#ifdef RAPTOR_XML_EXPAT
  if(sax2->xp) {
    XML_ParserFree(sax2->xp);
    sax2->xp=NULL;
  }
#endif

#ifdef RAPTOR_XML_LIBXML
  if(sax2->xc) {
    raptor_libxml_free(sax2->xc);
    sax2->xc=NULL;
  }
#endif

  while( (xml_element=raptor_xml_element_pop(sax2)) )
    raptor_free_xml_element(xml_element);

  raptor_namespaces_clear(&sax2->namespaces);

  if(sax2->base_uri)
    raptor_free_uri(sax2->base_uri);

  RAPTOR_FREE(raptor_sax2, sax2);
}


/**
 * raptor_sax2_set_start_element_handler:
 * @sax2: SAX2 object
 * @handler: start element handler
 *
 * Set SAX2 start element handler.
 */
void
raptor_sax2_set_start_element_handler(raptor_sax2* sax2,
                                      raptor_sax2_start_element_handler handler)
{
  sax2->start_element_handler=handler;
}


/**
 * raptor_sax2_set_end_element_handler:
 * @sax2: SAX2 object
 * @handler: end element handler
 *
 * Set SAX2 end element handler.
 */
void
raptor_sax2_set_end_element_handler(raptor_sax2* sax2,
                                    raptor_sax2_end_element_handler handler)
{
  sax2->end_element_handler=handler;
}


/**
 * raptor_sax2_set_characters_handler:
 * @sax2: SAX2 object
 * @handler: characters handler
 *
 * Set SAX2 characters handler.
 */
void
raptor_sax2_set_characters_handler(raptor_sax2* sax2,
                                   raptor_sax2_characters_handler handler)
{
  sax2->characters_handler=handler;
}


/**
 * raptor_sax2_set_cdata_handler:
 * @sax2: SAX2 object
 * @handler: CDATA handler
 *
 * Set SAX2 CDATA handler.
 */
void
raptor_sax2_set_cdata_handler(raptor_sax2* sax2,
                              raptor_sax2_cdata_handler handler)
{
  sax2->cdata_handler=handler;
}


/**
 * raptor_sax2_set_comment_handler:
 * @sax2: SAX2 object
 * @handler: comment handler
 *
 * Set SAX2 XML comment handler.
 */
void
raptor_sax2_set_comment_handler(raptor_sax2* sax2,
                                raptor_sax2_comment_handler handler)
{
  sax2->comment_handler=handler;
}


/**
 * raptor_sax2_set_unparsed_entity_decl_handler:
 * @sax2: SAX2 object
 * @handler: unparsed entity declaration handler
 *
 * Set SAX2 XML unparsed entity declaration handler.
 */
void
raptor_sax2_set_unparsed_entity_decl_handler(raptor_sax2* sax2,
                                             raptor_sax2_unparsed_entity_decl_handler handler)
{
  sax2->unparsed_entity_decl_handler=handler;
}


/**
 * raptor_sax2_set_external_entity_ref_handler:
 * @sax2: SAX2 object
 * @handler: entity reference handler
 *
 * Set SAX2 XML entity reference handler.
 */
void
raptor_sax2_set_external_entity_ref_handler(raptor_sax2* sax2,
                                            raptor_sax2_external_entity_ref_handler handler)
{
  sax2->external_entity_ref_handler=handler;
}


/**
 * raptor_sax2_set_namespace_handler:
 * @sax2: #raptor_sax2 object
 * @handler: new namespace callback function
 *
 * Set the XML namespace handler function.
 *
 * When a prefix/namespace is seen in an XML parser, call the given
 * @handler with the prefix string and the #raptor_uri namespace URI.
 * Either can be NULL for the default prefix or default namespace.
 *
 * The handler function does not deal with duplicates so any
 * namespace may be declared multiple times when a namespace is seen
 * in different parts of a document.
 * 
 */
void
raptor_sax2_set_namespace_handler(raptor_sax2* sax2,
                                  raptor_namespace_handler handler)
{
  sax2->namespace_handler=handler;
}


raptor_xml_element*
raptor_xml_element_pop(raptor_sax2 *sax2) 
{
  raptor_xml_element *element=sax2->current_element;

  if(!element)
    return NULL;

  sax2->current_element=element->parent;
  if(sax2->root_element == element) /* just deleted root */
    sax2->root_element=NULL;

  return element;
}


void
raptor_xml_element_push(raptor_sax2 *sax2, raptor_xml_element* element) 
{
  element->parent=sax2->current_element;
  sax2->current_element=element;
  if(!sax2->root_element)
    sax2->root_element=element;
}


/**
 * raptor_xml_element_is_empty:
 * @xml_element: XML Element
 * 
 * Check if an XML Element is empty.
 * 
 * Return value: non-0 if the element is empty.
 */
int
raptor_xml_element_is_empty(raptor_xml_element* xml_element)
{
  return !xml_element->content_cdata_seen &&
         !xml_element->content_element_seen;
}


/**
 * raptor_sax2_inscope_xml_language:
 * @sax2: SAX2 object
 * 
 * Get the in-scope XML language
 * 
 * Return value: the XML language or NULL if none is in scope.
 */
const unsigned char*
raptor_sax2_inscope_xml_language(raptor_sax2 *sax2)
{
  raptor_xml_element* xml_element;
  
  for(xml_element=sax2->current_element;
      xml_element; 
      xml_element=xml_element->parent)
    if(xml_element->xml_language) {
      if(!*xml_element->xml_language)
        return NULL;
      return xml_element->xml_language;
    }
  
    
  return NULL;
}


/**
 * raptor_sax2_inscope_base_uri:
 * @sax2: SAX2 object
 * 
 * Get the in-scope base URI
 * 
 * Return value: the in-scope base URI shared object or NULL if none is in scope.
 */
raptor_uri*
raptor_sax2_inscope_base_uri(raptor_sax2 *sax2)
{
  raptor_xml_element *xml_element;
  
  for(xml_element=sax2->current_element; 
      xml_element; 
      xml_element=xml_element->parent)
    if(xml_element->base_uri)
      return xml_element->base_uri;
    
  return sax2->base_uri;
}


int
raptor_sax2_get_depth(raptor_sax2 *sax2)
{
  return sax2->depth;
}

void
raptor_sax2_inc_depth(raptor_sax2 *sax2)
{
  sax2->depth++;
}

void
raptor_sax2_dec_depth(raptor_sax2 *sax2)
{
  sax2->depth--;
}


static void raptor_sax2_simple_error(void* user_data, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);

/*
 * raptor_sax2_simple_error - Error from a sax2 - Internal
 *
 * Matches the raptor_simple_message_handler API but calls
 * the sax2 error_handler
 */
static void
raptor_sax2_simple_error(void* user_data, const char *message, ...)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  va_list arguments;

  va_start(arguments, message);

  if(sax2) {
    raptor_log_level level=RAPTOR_LOG_LEVEL_ERROR;
    raptor_message_handler_closure* cl;
    cl=&sax2->error_handlers->handlers[level];
    raptor_log_error_varargs(level, cl->handler, cl->user_data,
                             sax2->locator,
                             message, arguments);
  }
  
  va_end(arguments);
}



/**
 * raptor_sax2_parse_start:
 * @sax2: sax2 object
 * @base_uri: base URI
 *
 * Start an XML SAX2 parse.
 */
void
raptor_sax2_parse_start(raptor_sax2* sax2, raptor_uri *base_uri)
{
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  sax2->depth=0;
  sax2->root_element=NULL;
  sax2->current_element=NULL;

  if(sax2->base_uri)
    raptor_free_uri(sax2->base_uri);
  if(base_uri)
    sax2->base_uri=raptor_uri_copy(base_uri);
  else
    sax2->base_uri=NULL;

#ifdef RAPTOR_XML_EXPAT
  if(sax2->xp) {
    XML_ParserFree(sax2->xp);
    sax2->xp=NULL;
  }

  raptor_expat_init(sax2, base_uri);
#endif

#ifdef RAPTOR_XML_LIBXML
  raptor_libxml_init(sax2, base_uri);

  xmlSetStructuredErrorFunc(sax2->error_handlers, 
                            raptor_libxml_xmlStructuredErrorFunc);

#if LIBXML_VERSION < 20425
  sax2->first_read=1;
#endif

  if(sax2->xc) {
    raptor_libxml_free(sax2->xc);
    sax2->xc=NULL;
  }
#endif

  raptor_namespaces_clear(&sax2->namespaces);

  raptor_uri_get_handler(&uri_handler, &uri_context);
  raptor_namespaces_init(&sax2->namespaces,
                         uri_handler, uri_context,
                         (raptor_simple_message_handler)raptor_sax2_simple_error, sax2, 
                         1);

}


/**
 * raptor_sax2_parse_chunk:
 * @sax2: sax2 object
 * @buffer: input buffer
 * @len: input buffer lenght
 * @is_end: non-0 if end of data
 *
 * Parse a chunk of XML data generating SAX2 events
 *
 * Return value: non-0 on failure
 */
int
raptor_sax2_parse_chunk(raptor_sax2* sax2, const unsigned char *buffer,
                        size_t len, int is_end) 
{
#ifdef RAPTOR_XML_EXPAT
  XML_Parser xp=sax2->xp;
  int rc;
#endif
#ifdef RAPTOR_XML_LIBXML
  /* parser context */
  xmlParserCtxtPtr xc=sax2->xc;
  int rc;
#endif
  
#ifdef RAPTOR_XML_LIBXML
  if(!xc) {
    int libxml_options = 0;

    if(!len) {
      /* no data given at all - emit a similar message to expat */
      raptor_sax2_update_document_locator(sax2, sax2->locator);
      raptor_log_error_to_handlers(sax2->error_handlers,
                                   RAPTOR_LOG_LEVEL_ERROR, sax2->locator,
                                   "XML Parsing failed - no element found");
      return 1;
    }

    xc = xmlCreatePushParserCtxt(&sax2->sax, sax2, /* user data */
                                 (char*)buffer, len, 
                                 NULL);
    if(!xc)
      goto handle_error;

#ifdef RAPTOR_LIBXML_XML_PARSE_NONET
    if(sax2->feature_no_net)
      libxml_options |= XML_PARSE_NONET;
#endif
#ifdef HAVE_XMLCTXTUSEOPTIONS
    xmlCtxtUseOptions(xc, libxml_options);
#endif
    
    xc->userData = sax2; /* user data */
    xc->vctxt.userData = sax2; /* user data */
    xc->vctxt.error=(xmlValidityErrorFunc)raptor_libxml_validation_error;
    xc->vctxt.warning=(xmlValidityWarningFunc)raptor_libxml_validation_warning;
    xc->replaceEntities = 1;
    
    sax2->xc = xc;

    if(is_end)
      len=0;
    else
      return 0;
  }
#endif

  if(!len) {
#ifdef RAPTOR_XML_EXPAT
    rc=XML_Parse(xp, (char*)buffer, 0, 1);
    if(!rc) /* expat: 0 is failure */
      goto handle_error;
#endif
#ifdef RAPTOR_XML_LIBXML
    xmlParseChunk(xc, (char*)buffer, 0, 1);
#endif
    return 0;
  }


#ifdef RAPTOR_XML_EXPAT
  rc=XML_Parse(xp, (char*)buffer, len, is_end);
  if(!rc) /* expat: 0 is failure */
    goto handle_error;
  if(is_end)
    return 0;
#endif

#ifdef RAPTOR_XML_LIBXML

  /* This works around some libxml versions that fail to work
   * if the buffer size is larger than the entire file
   * and thus the entire parsing is done in one operation.
   *
   * The code below:
   *   2.4.19 (oldest tested) to 2.4.24 - required
   *   2.4.25                           - works with or without it
   *   2.4.26 or later                  - fails with this code
   */

#if LIBXML_VERSION < 20425
  if(sax2->first_read && is_end) {
    /* parse all but the last character */
    rc=xmlParseChunk(xc, (char*)buffer, len-1, 0);
    if(rc)
      goto handle_error;
    /* last character */
    rc=xmlParseChunk(xc, (char*)buffer + (len-1), 1, 0);
    if(rc)
      goto handle_error;
    /* end */
    xmlParseChunk(xc, (char*)buffer, 0, 1);
    return 0;
  }
#endif

#if LIBXML_VERSION < 20425
  sax2->first_read=0;
#endif
    
  rc=xmlParseChunk(xc, (char*)buffer, len, is_end);
  if(rc) /* libxml: non 0 is failure */
    goto handle_error;
  if(is_end)
    return 0;
#endif

  return 0;

#if defined(RAPTOR_XML_EXPAT) || defined(RAPTOR_XML_LIBXML)
  handle_error:
#endif

#ifdef RAPTOR_XML_EXPAT
#ifdef EXPAT_UTF8_BOM_CRASH
  if(sax2->tokens_count) {
#endif
    /* Work around a bug with the expat 1.95.1 shipped with RedHat 7.2
     * which dies here if the error is before <?xml?...
     * The expat 1.95.1 source release version works fine.
     */
    if(sax2->locator)
      raptor_sax2_update_document_locator(sax2, sax2->locator);
#ifdef EXPAT_UTF8_BOM_CRASH
  }
#endif
#endif /* EXPAT */

#if RAPTOR_XML_EXPAT
  if(1) {
    const char *error_prefix="XML Parsing failed - "; /* 21 chars */
    #define ERROR_PREFIX_LEN 21
    const char *error_message=XML_ErrorString(XML_GetErrorCode(xp));
    size_t error_length;
    char *error_buffer;

    error_length=strlen(error_message);
    error_buffer=(char*)RAPTOR_MALLOC(cstring, 
                                      ERROR_PREFIX_LEN + error_length+1);
    if(error_buffer) {
      strncpy(error_buffer, error_prefix, ERROR_PREFIX_LEN);
      strncpy(error_buffer+ERROR_PREFIX_LEN, error_message, error_length+1);

      raptor_log_error_to_handlers(sax2->error_handlers,
                                   RAPTOR_LOG_LEVEL_ERROR,
                                   sax2->locator, error_buffer);
      RAPTOR_FREE(cstring, error_buffer);
    } else
      raptor_log_error_to_handlers(sax2->error_handlers,
                                   RAPTOR_LOG_LEVEL_ERROR,
                                   sax2->locator, "XML Parsing failed");
  }
#endif

  return 1;
}


/**
 * raptor_sax2_set_feature:
 * @sax2: #raptor_sax2 SAX2 object
 * @feature: feature to set from enumerated #raptor_feature values
 * @value: integer feature value (0 or larger)
 *
 * Set various SAX2 features.
 * 
 * The allowed features are available via raptor_sax2_features_enumerate().
 *
 * Return value: non 0 on failure or if the feature is unknown
 */
int
raptor_sax2_set_feature(raptor_sax2 *sax2, raptor_feature feature, int value)
{
  if(value < 0)
    return -1;
  
  switch(feature) {
    case RAPTOR_FEATURE_NORMALIZE_LANGUAGE:
      sax2->feature_normalize_language=value;
      break;

    case RAPTOR_FEATURE_NO_NET:
      sax2->feature_no_net=value;
      break;

    case RAPTOR_FEATURE_SCANNING:
    case RAPTOR_FEATURE_ASSUME_IS_RDF:
    case RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES:
    case RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_ALLOW_BAGID:
    case RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST:
    case RAPTOR_FEATURE_NON_NFC_FATAL:
    case RAPTOR_FEATURE_WARN_OTHER_PARSETYPES:
    case RAPTOR_FEATURE_CHECK_RDF_ID:
    case RAPTOR_FEATURE_HTML_TAG_SOUP:
    case RAPTOR_FEATURE_MICROFORMATS:
    case RAPTOR_FEATURE_HTML_LINK:
    case RAPTOR_FEATURE_WWW_TIMEOUT:
    case RAPTOR_FEATURE_RELATIVE_URIS:
    case RAPTOR_FEATURE_START_URI:
    case RAPTOR_FEATURE_WRITER_AUTO_INDENT:
    case RAPTOR_FEATURE_WRITER_AUTO_EMPTY:
    case RAPTOR_FEATURE_WRITER_INDENT_WIDTH:
    case RAPTOR_FEATURE_WRITER_XML_VERSION:
    case RAPTOR_FEATURE_WRITER_XML_DECLARATION:

    /* DOT serializer features */
    case RAPTOR_FEATURE_RESOURCE_BORDER:
    case RAPTOR_FEATURE_LITERAL_BORDER:
    case RAPTOR_FEATURE_BNODE_BORDER:
    case RAPTOR_FEATURE_RESOURCE_FILL:
    case RAPTOR_FEATURE_LITERAL_FILL:
    case RAPTOR_FEATURE_BNODE_FILL:

    /* JSON serializer features */
    case RAPTOR_FEATURE_JSON_CALLBACK:
    case RAPTOR_FEATURE_JSON_EXTRA_DATA:
    
    /* Turtle serializer feature */
    case RAPTOR_FEATURE_WRITE_BASE_URI:

    /* WWW feature */
    case RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL:
    case RAPTOR_FEATURE_WWW_HTTP_USER_AGENT:
      
    default:
      return -1;
      break;
  }

  return 0;
}


void
raptor_sax2_update_document_locator(raptor_sax2* sax2, 
                                    raptor_locator* locator)
{
#ifdef RAPTOR_XML_EXPAT
  raptor_expat_update_document_locator(sax2, locator);
#endif
#ifdef RAPTOR_XML_LIBXML
  raptor_libxml_update_document_locator(sax2, locator);
#endif
}


/* start of an element */
void 
raptor_sax2_start_element(void* user_data, const unsigned char *name,
                          const unsigned char **atts)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  raptor_qname* el_name;
  unsigned char **xml_atts_copy=NULL;
  size_t xml_atts_size=0;
  int all_atts_count=0;
  int ns_attributes_count=0;
  raptor_qname** named_attrs=NULL;
  raptor_xml_element* xml_element=NULL;
  unsigned char *xml_language=NULL;
  raptor_uri *xml_base=NULL;

#ifdef RAPTOR_XML_EXPAT
#ifdef EXPAT_UTF8_BOM_CRASH
  sax2->tokens_count++;
#endif
#endif

#ifdef RAPTOR_XML_LIBXML
  if(atts) {
    int i;
    
    /* Do XML attribute value normalization */
    for (i = 0; atts[i]; i+=2) {
      unsigned char *value=(unsigned char*)atts[i+1];
      unsigned char *src = value;
      unsigned char *dst = xmlStrdup(value);

      if(!dst) {
        raptor_log_error_to_handlers(sax2->error_handlers, 
                                     RAPTOR_LOG_LEVEL_FATAL,
                                     sax2->locator, "Out of memory");
        return;
      }

      atts[i+1]=dst;

      while (*src == 0x20 || *src == 0x0d || *src == 0x0a || *src == 0x09) 
        src++;
      while (*src) {
        if (*src == 0x20 || *src == 0x0d || *src == 0x0a || *src == 0x09) {
          while (*src == 0x20 || *src == 0x0d || *src == 0x0a || *src == 0x09)
            src++;
          if (*src)
            *dst++ = 0x20;
        } else {
          *dst++ = *src++;
        }
      }
      *dst = '\0';
      xmlFree(value);
    }
  }
#endif

  raptor_sax2_inc_depth(sax2);

  if(atts) {
    int i;

    /* Save passed in XML attributes pointers so we can 
     * NULL the pointers when they get handled below (various atts[i]=NULL)
     */
    for (i = 0; atts[i]; i++) ;
    xml_atts_size=sizeof(unsigned char*) * i;
    if(xml_atts_size) {
      xml_atts_copy=(unsigned char**)RAPTOR_MALLOC(cstringpointer,xml_atts_size);
      if(!xml_atts_copy)
        goto fail;
      memcpy(xml_atts_copy, atts, xml_atts_size);
    }

    /* XML attributes processing:
     *   xmlns*   - XML namespaces (Namespaces in XML REC)
     *     Deleted and used to synthesise namespaces declarations
     *   xml:lang - XML language (XML REC)
     *     Deleted and optionally normalised to lowercase
     *   xml:base - XML Base (XML Base REC)
     *     Deleted and used to set the in-scope base URI for this XML element
     */
    for (i = 0; atts[i]; i+= 2) {
      all_atts_count++;

      if(strncmp((char*)atts[i], "xml", 3)) {
        /* count and skip non xml* attributes */
        ns_attributes_count++;
        continue;
      }

      /* synthesise the XML namespace events */
      if(!memcmp((const char*)atts[i], "xmlns", 5)) {
        const unsigned char *prefix=atts[i][5] ? &atts[i][6] : NULL;
        const unsigned char *namespace_name=atts[i+1];

        raptor_namespace* nspace;
        nspace=raptor_new_namespace(&sax2->namespaces,
                                    prefix, namespace_name,
                                    raptor_sax2_get_depth(sax2));

        if(nspace) {
          raptor_namespaces_start_namespace(&sax2->namespaces, nspace);

          if(sax2->namespace_handler)
            (*sax2->namespace_handler)(sax2->user_data, nspace);
        }
      } else if(!strcmp((char*)atts[i], "xml:lang")) {
        xml_language=(unsigned char*)RAPTOR_MALLOC(cstring, strlen((char*)atts[i+1])+1);
        if(!xml_language) {
          raptor_log_error_to_handlers(sax2->error_handlers, 
                                       RAPTOR_LOG_LEVEL_FATAL,
                                       sax2->locator, "Out of memory");
          goto fail;
        }

        /* optionally normalize language to lowercase */
        if(sax2->feature_normalize_language) {
          unsigned char *from=(unsigned char*)atts[i+1];
          unsigned char *to=xml_language;
          
          while(*from) {
            if(isupper(*from))
              *to++ =tolower(*from++);
            else
              *to++ =*from++;
          }
          *to='\0';
        } else
          strcpy((char*)xml_language, (char*)atts[i+1]);
      } else if(!strcmp((char*)atts[i], "xml:base")) {
        raptor_uri* base_uri;
        raptor_uri* xuri;
        base_uri=raptor_sax2_inscope_base_uri(sax2);
        xuri=raptor_new_uri_relative_to_base(base_uri, atts[i+1]);
        xml_base=raptor_new_uri_for_xmlbase(xuri);
        raptor_free_uri(xuri);
      }

      /* delete all xml attributes whether processed above or not */
      atts[i]=NULL; 
    }
  }


  /* Create new element structure */
  el_name=raptor_new_qname(&sax2->namespaces, name, NULL,
                           (raptor_simple_message_handler)raptor_sax2_simple_error, sax2);
  if(!el_name)
    goto fail;

  xml_element=raptor_new_xml_element(el_name, xml_language, xml_base);
  if(!xml_element) {
    raptor_free_qname(el_name);
    goto fail;
  }
  /* xml_language,xml_base now owned by xml_element */
  xml_language = NULL;
  xml_base = NULL; 

  /* Turn string attributes into namespaced-attributes */
  if(ns_attributes_count) {
    int i;
    int offset = 0;

    /* Allocate new array to hold namespaced-attributes */
    named_attrs=(raptor_qname**)RAPTOR_CALLOC(raptor_qname_array, 
                                              ns_attributes_count, 
                                              sizeof(raptor_qname*));
    if(!named_attrs) {
      raptor_log_error_to_handlers(sax2->error_handlers, 
                                   RAPTOR_LOG_LEVEL_FATAL,
                                   sax2->locator, "Out of memory");
      goto fail;
    }

    for (i = 0; i < all_atts_count; i++) {
      raptor_qname* attr;

      /* Skip previously processed attributes */
      if(!atts[i<<1])
        continue;

      /* namespace-name[i] stored in named_attrs[i] */
      attr=raptor_new_qname(&sax2->namespaces,
                            atts[i<<1], atts[(i<<1)+1],
                            (raptor_simple_message_handler)raptor_sax2_simple_error, sax2);
      if(!attr) { /* failed - tidy up and return */
        int j;

        for (j=0; j < i; j++)
          RAPTOR_FREE(raptor_qname, named_attrs[j]);
        RAPTOR_FREE(raptor_qname_array, named_attrs);
        goto fail;
      }

      named_attrs[offset++]=attr;
    }
  } /* end if ns_attributes_count */


  if(named_attrs)
    raptor_xml_element_set_attributes(xml_element,
                                      named_attrs, ns_attributes_count);

  raptor_xml_element_push(sax2, xml_element);

  if(sax2->start_element_handler)
    sax2->start_element_handler(sax2->user_data, xml_element);

  if(xml_atts_copy) {
    /* Restore passed in XML attributes, free the copy */
    memcpy((void*)atts, xml_atts_copy, xml_atts_size);
    RAPTOR_FREE(cstringpointer, xml_atts_copy);
  }

  return;

  fail:
  if(xml_atts_copy)
    RAPTOR_FREE(cstringpointer, xml_atts_copy);
  if(xml_base)
    raptor_free_uri(xml_base);
  if(xml_language)
    RAPTOR_FREE(cstring, xml_language);
  if(xml_element)
    raptor_free_xml_element(xml_element);
}


/* end of an element */
void
raptor_sax2_end_element(void* user_data, const unsigned char *name)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  raptor_xml_element* xml_element;

#ifdef RAPTOR_XML_EXPAT
#ifdef EXPAT_UTF8_BOM_CRASH
  sax2->tokens_count++;
#endif
#endif

  xml_element=sax2->current_element;
  if(xml_element) {
#ifdef RAPTOR_DEBUG_VERBOSE
    fprintf(stderr, "\nraptor_rdfxml_end_element_handler: End ns-element: ");
    raptor_qname_print(stderr, xml_element->name);
    fputc('\n', stderr);
#endif

    if(sax2->end_element_handler)
      sax2->end_element_handler(sax2->user_data, xml_element);
  }
  
  raptor_namespaces_end_for_depth(&sax2->namespaces, 
                                  raptor_sax2_get_depth(sax2));
  xml_element=raptor_xml_element_pop(sax2);
  if(xml_element)
    raptor_free_xml_element(xml_element);

  raptor_sax2_dec_depth(sax2);
}




/* characters */
void
raptor_sax2_characters(void* user_data, const unsigned char *s, int len)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  if(sax2->characters_handler)
    sax2->characters_handler(sax2->user_data, sax2->current_element, s, len);
}


/* like <![CDATA[...]> */
void
raptor_sax2_cdata(void* user_data, const unsigned char *s, int len)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
#ifdef RAPTOR_XML_EXPAT
#ifdef EXPAT_UTF8_BOM_CRASH
  sax2->tokens_count++;
#endif
#endif

  if(sax2->cdata_handler)
    sax2->cdata_handler(sax2->user_data, sax2->current_element, s, len);
}


/* comment */
void
raptor_sax2_comment(void* user_data, const unsigned char *s)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  if(sax2->comment_handler)
    sax2->comment_handler(sax2->user_data, sax2->current_element, s);
}


/* unparsed (NDATA) entity */
void
raptor_sax2_unparsed_entity_decl(void* user_data, 
                                 const unsigned char* entityName, 
                                 const unsigned char* base, 
                                 const unsigned char* systemId, 
                                 const unsigned char* publicId, 
                                 const unsigned char* notationName)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  if(sax2->unparsed_entity_decl_handler)
    sax2->unparsed_entity_decl_handler(sax2->user_data,
                                       entityName, base, systemId, 
                                       publicId, notationName);
}


/* external entity reference */
int
raptor_sax2_external_entity_ref(void* user_data, 
                                const unsigned char* context, 
                                const unsigned char* base, 
                                const unsigned char* systemId, 
                                const unsigned char* publicId)
{
  raptor_sax2* sax2=(raptor_sax2*)user_data;
  if(sax2->external_entity_ref_handler)
    return sax2->external_entity_ref_handler(sax2->user_data,
                                             context, base, systemId, publicId);

  raptor_sax2_simple_error((void*)sax2,
                           "Failed to handle external entity reference with base %s systemId %s publicId %s",
                           (base ?  (const char*)base : "(None)"), 
                           systemId,
                           (publicId ?  (const char*)publicId: "(None)"));
  
  /* Failed to handle external entity reference */
  return 0;
}
