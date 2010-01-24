/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_internal.h - Redland Parser Toolkit for RDF (Raptor) internals
 *
 * Copyright (C) 2002-2008, David Beckett http://purl.org/net/dajobe/
 * Copyright (C) 2002-2004, University of Bristol, UK http://www.bristol.ac.uk/
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



#ifndef RAPTOR_INTERNAL_H
#define RAPTOR_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef RAPTOR_INTERNAL

/* for the memory allocation functions */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#undef HAVE_STDLIB_H
#endif

#if defined(HAVE_DMALLOC_H) && defined(RAPTOR_MEMORY_DEBUG_DMALLOC)
#include <dmalloc.h>
#endif


/* Can be over-ridden or undefined in a config.h file or -Ddefine */
#ifndef RAPTOR_INLINE
#define RAPTOR_INLINE inline
#endif

#ifdef LIBRDF_DEBUG
#define RAPTOR_DEBUG 1
#endif

#if defined(RAPTOR_MEMORY_SIGN)
#define RAPTOR_SIGN_KEY 0x08A61080
void* raptor_sign_malloc(size_t size);
void* raptor_sign_calloc(size_t nmemb, size_t size);
void* raptor_sign_realloc(void *ptr, size_t size);
void raptor_sign_free(void *ptr);
  
#define RAPTOR_MALLOC(type, size)   raptor_sign_malloc(size)
#define RAPTOR_CALLOC(type, nmemb, size) raptor_sign_calloc(nmemb, size)
#define RAPTOR_REALLOC(type, ptr, size) raptor_sign_realloc(ptr, size)
#define RAPTOR_FREE(type, ptr)   raptor_sign_free(ptr)

#else
#define RAPTOR_MALLOC(type, size) malloc(size)
#define RAPTOR_CALLOC(type, nmemb, size) calloc(nmemb, size)
#define RAPTOR_REALLOC(type, ptr, size) realloc(ptr, size)
#define RAPTOR_FREE(type, ptr)   free((void*)ptr)

#endif

#ifdef RAPTOR_DEBUG
/* Debugging messages */
#define RAPTOR_DEBUG1(msg) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__); } while(0)
#define RAPTOR_DEBUG2(msg, arg1) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1);} while(0)
#define RAPTOR_DEBUG3(msg, arg1, arg2) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2);} while(0)
#define RAPTOR_DEBUG4(msg, arg1, arg2, arg3) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2, arg3);} while(0)
#define RAPTOR_DEBUG5(msg, arg1, arg2, arg3, arg4) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2, arg3, arg4);} while(0)
#define RAPTOR_DEBUG6(msg, arg1, arg2, arg3, arg4, arg5) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2, arg3, arg4, arg5);} while(0)

#if defined(HAVE_DMALLOC_H) && defined(RAPTOR_MEMORY_DEBUG_DMALLOC)
void* raptor_system_malloc(size_t size);
void raptor_system_free(void *ptr);
#define SYSTEM_MALLOC(size)   raptor_system_malloc(size)
#define SYSTEM_FREE(ptr)   raptor_system_free(ptr)
#else
#define SYSTEM_MALLOC(size)   malloc(size)
#define SYSTEM_FREE(ptr)   free(ptr)
#endif

#ifndef RAPTOR_ASSERT_DIE
#define RAPTOR_ASSERT_DIE abort();
#endif

#else
/* DEBUGGING TURNED OFF */

/* No debugging messages */
#define RAPTOR_DEBUG1(msg)
#define RAPTOR_DEBUG2(msg, arg1)
#define RAPTOR_DEBUG3(msg, arg1, arg2)
#define RAPTOR_DEBUG4(msg, arg1, arg2, arg3)
#define RAPTOR_DEBUG5(msg, arg1, arg2, arg3, arg4)
#define RAPTOR_DEBUG6(msg, arg1, arg2, arg3, arg4, arg5)

#define SYSTEM_MALLOC(size)   malloc(size)
#define SYSTEM_FREE(ptr)   free(ptr)

#ifndef RAPTOR_ASSERT_DIE
#define RAPTOR_ASSERT_DIE
#endif

#endif


#ifdef RAPTOR_DISABLE_ASSERT_MESSAGES
#define RAPTOR_ASSERT_REPORT(line)
#else
#define RAPTOR_ASSERT_REPORT(msg) fprintf(stderr, "%s:%d: (%s) assertion failed: " msg "\n", __FILE__, __LINE__, __func__);
#endif


#ifdef RAPTOR_DISABLE_ASSERT

#define RAPTOR_ASSERT(condition, msg) 
#define RAPTOR_ASSERT_RETURN(condition, msg, ret) 
#define RAPTOR_ASSERT_OBJECT_POINTER_RETURN(pointer, type)
#define RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(pointer, type, ret)

#else

#define RAPTOR_ASSERT(condition, msg) do { \
  if(condition) { \
    RAPTOR_ASSERT_REPORT(msg) \
    RAPTOR_ASSERT_DIE \
  } \
} while(0)

#define RAPTOR_ASSERT_RETURN(condition, msg, ret) do { \
  if(condition) { \
    RAPTOR_ASSERT_REPORT(msg) \
    RAPTOR_ASSERT_DIE \
    return ret; \
  } \
} while(0)

#define RAPTOR_ASSERT_OBJECT_POINTER_RETURN(pointer, type) do { \
  if(!pointer) { \
    RAPTOR_ASSERT_REPORT("object pointer of type " #type " is NULL.") \
    RAPTOR_ASSERT_DIE \
    return; \
  } \
} while(0)

#define RAPTOR_ASSERT_OBJECT_POINTER_RETURN_VALUE(pointer, type, ret) do { \
  if(!pointer) { \
    RAPTOR_ASSERT_REPORT("object pointer of type " #type " is NULL.") \
    RAPTOR_ASSERT_DIE \
    return ret; \
  } \
} while(0)

#endif


/* Fatal errors - always happen */
#define RAPTOR_FATAL1(msg) do {fprintf(stderr, "%s:%d:%s: fatal error: " msg, __FILE__, __LINE__ , __func__); abort();} while(0)
#define RAPTOR_FATAL2(msg,arg) do {fprintf(stderr, "%s:%d:%s: fatal error: " msg, __FILE__, __LINE__ , __func__, arg); abort();} while(0)
#define RAPTOR_FATAL3(msg,arg1,arg2) do {fprintf(stderr, "%s:%d:%s: fatal error: " msg, __FILE__, __LINE__ , __func__, arg1, arg2); abort();} while(0)

#define MAX_ASCII_INT_SIZE 13
  
/* XML parser includes */
#ifdef RAPTOR_XML_EXPAT
#ifdef HAVE_EXPAT_H
#include <expat.h>
#endif
#ifdef HAVE_XMLPARSE_H
#include <xmlparse.h>
#endif
#endif


#ifdef RAPTOR_XML_LIBXML

#include <libxml/parser.h>


/* libxml-only prototypes */


/* raptor_libxml.c exports */
extern void raptor_libxml_init(raptor_sax2* sax2, raptor_uri *base_uri);
extern void raptor_libxml_init_sax_error_handlers(xmlSAXHandler *sax);
extern void raptor_libxml_generic_error(void* user_data, const char *msg, ...) RAPTOR_PRINTF_FORMAT(2, 3);

extern void raptor_libxml_validation_error(void *context, const char *msg, ...) RAPTOR_PRINTF_FORMAT(2, 3);
extern void raptor_libxml_validation_warning(void *context, const char *msg, ...) RAPTOR_PRINTF_FORMAT(2, 3);
void raptor_libxml_free(xmlParserCtxtPtr xc);
void raptor_libxml_xmlStructuredErrorFunc(void *user_data, xmlErrorPtr err);

/* raptor_parse.c - exported to libxml part */
extern void raptor_libxml_update_document_locator(raptor_sax2* sax2, raptor_locator* locator);

/* end of libxml-only */
#endif


/* expat-only prototypes */

#ifdef RAPTOR_XML_EXPAT
/* raptor_expat.c exports */
extern void raptor_expat_init(raptor_sax2* sax2, raptor_uri *base_uri);
extern void raptor_expat_update_document_locator(raptor_sax2* sax2, raptor_locator *locator);

/* raptor_parse.c */
void raptor_xml_unparsed_entity_decl_handler(void *user_data, const unsigned char* entityName, const unsigned char* base, const unsigned char* systemId, const unsigned char* publicId, const unsigned char* notationName);
int raptor_xml_external_entity_ref_handler(void *user_data, const unsigned char* context, const unsigned char* base, const unsigned char* systemId, const unsigned char* publicId);

/* end of expat-only */
#endif


typedef struct raptor_parser_factory_s raptor_parser_factory;
typedef struct raptor_serializer_factory_s raptor_serializer_factory;
typedef struct raptor_id_set_s raptor_id_set;
typedef struct raptor_uri_detail_s raptor_uri_detail;

/* raptor_avltree.c */
/* AVL tree */
typedef struct raptor_avltree_s raptor_avltree;


/* Raptor Namespace Stack node */
struct raptor_namespace_stack_s {
  raptor_namespace* top;
  const raptor_uri_handler *uri_handler;
  void *uri_context;
  raptor_simple_message_handler error_handler;
  void *error_data;

  raptor_uri *rdf_ms_uri;
  raptor_uri *rdf_schema_uri;
};


/* Forms:
 * 1) prefix=NULL uri=<URI>      - default namespace defined
 * 2) prefix=NULL, uri=NULL      - no default namespace
 * 3) prefix=<prefix>, uri=<URI> - regular pair defined <prefix>:<URI>
 */
struct raptor_namespace_s {
  /* next down the stack, NULL at bottom */
  struct raptor_namespace_s* next;

  raptor_namespace_stack *nstack;

  /* NULL means is the default namespace */
  const unsigned char *prefix;
  /* needed to safely compare prefixed-names */
  int prefix_length;
  /* URI of namespace or NULL for default */
  raptor_uri *uri;
  /* parsing depth that this ns was added.  It will
   * be deleted when the parser leaves this depth 
   */
  int depth;
  /* Non 0 if is xml: prefixed name */
  int is_xml;
  /* Non 0 if is RDF M&S Namespace */
  int is_rdf_ms;
  /* Non 0 if is RDF Schema Namespace */
  int is_rdf_schema;
};


#ifdef RAPTOR_XML_LIBXML
#define RAPTOR_LIBXML_MAGIC 0x8AF108
#endif


/*
 * Raptor parser object
 */
struct raptor_parser_s {
#ifdef RAPTOR_XML_LIBXML
  int magic;
#endif

  /* can be filled with error location information */
  raptor_locator locator;

  /* non 0 if parser had fatal error and cannot continue */
  int failed;

  /* generated ID counter */
  int genid;

  /* base URI of RDF/XML */
  raptor_uri *base_uri;

  /* static statement for use in passing to user code */
  raptor_statement statement;

  /* Features */
  int features[RAPTOR_FEATURE_LAST+1];

  /* stuff for our user */
  void *user_data;

  raptor_error_handlers error_handlers;

  void* unused1; /* UNUSED - re-use struct slot later needed */

  /* parser callbacks */
  raptor_statement_handler statement_handler;

  raptor_graph_handler graph_handler;

  void *generate_id_handler_user_data;
  raptor_generate_id_handler generate_id_handler;

  int default_generate_id_handler_base;
  char *default_generate_id_handler_prefix;
  size_t default_generate_id_handler_prefix_length;

  void* uri_filter_user_data;
  raptor_uri_filter_func uri_filter;

  /* parser specific stuff */
  void *context;

  struct raptor_parser_factory_s* factory;

  /* namespace callback */
  raptor_namespace_handler namespace_handler;

  void* namespace_handler_user_data;

  raptor_stringbuffer* sb;

  /* raptor_www pointer stored here to allow cleanup on error */
  raptor_www* www;

  /* internal data for lexers */
  void* lexer_user_data;

  /* FEATURE:
   * HTTP Cache-Control: header value to send (default NULL)
   * RAPTOR_FEATURE_WWW_HTTP_CACHE_CONTROL
   */
  const char* cache_control;

  /* FEATURE:
   * HTTP User-Agent: header value to send (default NULL)
   * RAPTOR_FEATURE_WWW_HTTP_USER_AGENT
   */
  const char* user_agent;
};


/** A list of (MIME Type, Q) values */
struct raptor_type_q_s {
  const char* mime_type;
  size_t mime_type_len;
  int q; /* 0-10 standing for 0.0-1.0 */
};
typedef struct raptor_type_q_s raptor_type_q;


/** A Parser Factory for a syntax */
struct raptor_parser_factory_s {
  struct raptor_parser_factory_s* next;

  /* syntax name */
  const char* name;
  /* alternate syntax name; not mentioned in enumerations */
  const char* alias;

  /* syntax readable label */
  const char* label;

  /* syntax MIME type (or NULL) */
  raptor_sequence* mime_types;

  /* syntax URI (or NULL) */
  const unsigned char* uri_string;
  
  /* the rest of this structure is populated by the
     parser-specific register function */
  size_t context_length;
  
  /* create a new parser */
  int (*init)(raptor_parser* parser, const char *name);
  
  /* destroy a parser */
  void (*terminate)(raptor_parser* parser);
  
  /* start a parse */
  int (*start)(raptor_parser* parser);
  
  /* parse a chunk of memory */
  int (*chunk)(raptor_parser* parser, const unsigned char *buffer, size_t len, int is_end);

  /* finish the parser factory */
  void (*finish_factory)(raptor_parser_factory* factory);

  /* score recognition of the syntax by a block of characters, the
   *  content identifier or it's suffix or a mime type
   *  (different from the factory-registered one)
   */
  int (*recognise_syntax)(raptor_parser_factory* factory, const unsigned char *buffer, size_t len, const unsigned char *identifier, const unsigned char *suffix, const char *mime_type);

  /* get the Content-Type value of a URI request */
  void (*content_type_handler)(raptor_parser* rdf_parser, const char* content_type);

  /* get the Accept header of a URI request (OPTIONAL) */
  const char* (*accept_header)(raptor_parser* rdf_parser);

  /* non-0 if this parser needs a base URI */
  int need_base_uri;

  /* get the current generated ID base (OPTIONAL) */
  int (*get_current_base_id)(raptor_parser* rdf_parser);
};


/*
 * Raptor serializer object
 */
struct raptor_serializer_s {
  /* can be filled with error location information */
  raptor_locator locator;
  
  /* FEATURE:
   * non 0 to write base URI to document header (@base)
   */
  int feature_write_base_uri;

  /* FEATURE:
   * non 0 to write relative URIs wherever possible
   */
  int feature_relative_uris;

  /* FEATURE:
   * non NULL to start serializing from this URI
   */
  raptor_uri* feature_start_uri;

  /* FEATURES:
   * non NULL to override default border color
   */
  unsigned char *feature_resource_border;
  unsigned char *feature_literal_border;
  unsigned char *feature_bnode_border;

  /* FEATURES:
   * non NULL to fill with value
   */
  unsigned char *feature_resource_fill;
  unsigned char *feature_literal_fill;
  unsigned char *feature_bnode_fill;

  void *error_user_data;
  void *warning_user_data;

  raptor_message_handler error_handler;
  raptor_message_handler warning_handler;

  /* non 0 if serializer had fatal error and cannot continue */
  int failed;

  /* base URI of RDF/XML */
  raptor_uri *base_uri;

  /* serializer specific stuff */
  void *context;

  /* destination stream for the serialization */
  raptor_iostream *iostream;
  
  struct raptor_serializer_factory_s* factory;

  /* XML 1.0 (10) or XML 1.1 (11) */
  int xml_version;

  /* FEATURE:
   * non 0 to write XML 1.0 or 1.1 declaration (default 1)
   */
  int feature_write_xml_declaration;

  /* FEATURE:
   * JSON serializer callback function name
   */
  unsigned char *feature_json_callback;

  /* FEATURE:
   * JSON serializer extra data
   */
  unsigned char *feature_json_extra_data;
};


/** A Serializer Factory for a syntax */
struct raptor_serializer_factory_s {
  struct raptor_serializer_factory_s* next;

  /* syntax name */
  const char* name;
  /* alternate syntax name; not mentioned in enumerations */
  const char* alias;

  /* syntax readable label */
  const char* label;
  /* syntax MIME type (or NULL) */
  const char* mime_type;
  /* syntax URI (or NULL) */
  const unsigned char* uri_string;
  
  /* the rest of this structure is populated by the
     serializer-specific register function */
  size_t context_length;
  
  /* create a new serializer */
  int (*init)(raptor_serializer* serializer, const char *name);
  
  /* destroy a serializer */
  void (*terminate)(raptor_serializer* serializer);
  
  /* add a namespace */
  int (*declare_namespace)(raptor_serializer* serializer, raptor_uri *uri, const unsigned char *prefix);
  
  /* start a serialization */
  int (*serialize_start)(raptor_serializer* serializer);
  
  /* serialize a statement */
  int (*serialize_statement)(raptor_serializer* serializer, const raptor_statement *statment);

  /* end a serialization */
  int (*serialize_end)(raptor_serializer* serializer);
  
  /* finish the serializer factory */
  void (*finish_factory)(raptor_serializer_factory* factory);

  /* add a namespace using an existing namespace */
  int (*declare_namespace_from_namespace)(raptor_serializer* serializer, raptor_namespace *nspace);
};


/* for raptor_parse_uri_write_bytes() when used as a handler for
 * raptor_www_set_write_bytes_handler()
 */
typedef struct 
{
  raptor_parser* rdf_parser;
  raptor_uri* base_uri;
  raptor_uri* final_uri;
  int started;
} raptor_parse_bytes_context;


/* raptor_serialize.c */
int raptor_serializer_register_factory(const char *name, const char *label, const char *mime_type, const char *alias, const unsigned char *uri_string, int (*factory) (raptor_serializer_factory*));


/* raptor_general.c */

raptor_parser_factory* raptor_parser_register_factory(const char *name, const char *label, int (*factory) (raptor_parser_factory*));
int raptor_parser_factory_add_alias(raptor_parser_factory* factory, const char *alias);
int raptor_parser_factory_add_mime_type(raptor_parser_factory* factory, const char* mime_type, int q);
int raptor_parser_factory_add_uri(raptor_parser_factory* factory, const unsigned char *uri_string);

unsigned char* raptor_parser_internal_generate_id(raptor_parser *rdf_parser, raptor_genid_type type, unsigned char *user_bnodeid);

#ifdef RAPTOR_DEBUG
void raptor_stats_print(raptor_parser *rdf_parser, FILE *stream);
#endif
const char* raptor_basename(const char *name);
raptor_statement* raptor_statement_copy(const raptor_statement *statement);
void raptor_free_statement(raptor_statement *statement);

/* raptor_parse.c */
raptor_parser_factory* raptor_get_parser_factory(const char *name);  
void raptor_delete_parser_factories(void);
void raptor_free_type_q(raptor_type_q* type_q);
const char* raptor_parser_get_accept_header_all(void);
int raptor_parse_uri_no_net_filter(void *user_data, raptor_uri* uri);
void raptor_parse_uri_write_bytes(raptor_www* www, void *userdata, const void *ptr, size_t size, size_t nmemb);

/* raptor_general.c */
extern void raptor_parser_fatal_error(raptor_parser* parser, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
extern void raptor_parser_error(raptor_parser* parser, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
extern void raptor_parser_simple_error(void* parser, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
extern void raptor_parser_error_message_handler(void *user_data, raptor_locator* locator, const char *message);
extern void raptor_parser_warning(raptor_parser* parser, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
extern void raptor_parser_fatal_error_varargs(raptor_parser* parser, const char *message, va_list arguments) RAPTOR_PRINTF_FORMAT(2, 0);
extern void raptor_parser_fatal_error_message_handler(void *user_data, raptor_locator* locator, const char *message);
extern void raptor_parser_error_varargs(raptor_parser* parser, const char *message, va_list arguments) RAPTOR_PRINTF_FORMAT(2, 0);
extern void raptor_parser_warning_varargs(raptor_parser* parser, const char *message, va_list arguments)  RAPTOR_PRINTF_FORMAT(2, 0);
void raptor_parser_warning_message_handler(void *user_data, raptor_locator* locator, const char *message);

/* logging */
#define RAPTOR_ERROR_HANDLER_MAGIC 0xD00DB1FF

void raptor_log_error_to_handlers(raptor_error_handlers* error_handlers, raptor_log_level level, raptor_locator* locator, const char* message);
void raptor_log_error_varargs(raptor_log_level level, raptor_message_handler handler, void* handler_data, raptor_locator* locator, const char* message, va_list arguments) RAPTOR_PRINTF_FORMAT(5, 0);
void raptor_log_error(raptor_log_level level, raptor_message_handler handler, void* handler_data, raptor_locator* locator, const char* message);


/* raptor_parse.c */

typedef struct raptor_rdfxml_parser_s raptor_rdfxml_parser;

/* Prototypes for common expat/libxml parsing event-handling functions */
extern void raptor_xml_start_element_handler(void *user_data, const unsigned char *name, const unsigned char **atts);
extern void raptor_xml_end_element_handler(void *user_data, const unsigned char *name);
/* s is not 0 terminated. */
extern void raptor_xml_characters_handler(void *user_data, const unsigned char *s, int len);
extern void raptor_xml_cdata_handler(void *user_data, const unsigned char *s, int len);
void raptor_xml_comment_handler(void *user_data, const unsigned char *s);

#if RAPTOR_DEBUG > 1
void raptor_rdfxml_parser_stats_print(raptor_rdfxml_parser* rdf_xml_parser, FILE *stream);
#endif

void raptor_parser_copy_user_state(raptor_parser *to_parser, raptor_parser *from_parser);

/* raptor_feature.c */
int raptor_features_enumerate_common(const raptor_feature feature, const char **name, raptor_uri **uri, const char **label, int flags);

/* raptor_general.c */
extern int raptor_valid_xml_ID(raptor_parser *rdf_parser, const unsigned char *string);
int raptor_check_ordinal(const unsigned char *name);

/* raptor_identifier.c */
#ifdef RAPTOR_DEBUG
void raptor_identifier_print(FILE *stream, raptor_identifier* identifier);
#endif
  
/* raptor_locator.c */


#ifdef HAVE_STRCASECMP
#define raptor_strcasecmp strcasecmp
#define raptor_strncasecmp strncasecmp
#else
#ifdef HAVE_STRICMP
#define raptor_strcasecmp stricmp
#define raptor_strncasecmp strnicmp
#endif
#endif


/* raptor_nfc.c */
int raptor_nfc_check (const unsigned char* string, size_t len, int *error);


/* raptor_namespace.c */

#ifdef RAPTOR_DEBUG
void raptor_namespace_print(FILE *stream, raptor_namespace* ns);
#endif

void raptor_parser_start_namespace(raptor_parser* rdf_parser, raptor_namespace* nspace);


/* 
 * Raptor XML-namespace qualified name (qname), for elements or attributes 
 *
 * namespace is only defined when the XML name has a namespace and
 * only then is uri also given.
 */
struct raptor_qname_s {
  /* Name - always present */
  const unsigned char *local_name;
  int local_name_length;
  /* Namespace or NULL if not in a namespace */
  const raptor_namespace *nspace;
  /* URI of namespace+local_name or NULL if not defined */
  raptor_uri *uri;
  /* optional value - used when name is an attribute */
  const unsigned char *value;
  unsigned int value_length;
};



/* raptor_qname.c */
#ifdef RAPTOR_DEBUG
void raptor_qname_print(FILE *stream, raptor_qname* name);
#endif


/* raptor_uri.c */
int raptor_uri_init(void);
raptor_uri* raptor_new_uri_from_rdf_ordinal(int ordinal);

/* parsers */
int raptor_init_parser_rdfxml(void);
int raptor_init_parser_ntriples(void);
int raptor_init_parser_turtle(void);
int raptor_init_parser_trig(void);
int raptor_init_parser_n3(void);
int raptor_init_parser_grddl_common(void);
int raptor_init_parser_grddl(void);
int raptor_init_parser_guess(void);
int raptor_init_parser_rss(void);

void raptor_terminate_parser_grddl_common(void);

/* raptor_parse.c */
int raptor_parsers_init(void);
void raptor_parsers_finish(void);

void raptor_parser_save_content(raptor_parser* rdf_parser, int save);
const unsigned char* raptor_parser_get_content(raptor_parser* rdf_parser, size_t* length_p);
void raptor_parser_set_graph_name(raptor_parser* parser, raptor_uri* uri);
int raptor_parser_get_current_base_id(raptor_parser* parser);

/* raptor_rss.c */
int raptor_init_serializer_rss10(void);
int raptor_init_serializer_atom(void);

extern const unsigned char * const raptor_atom_namespace_uri;

/* raptor_rfc2396.c */
raptor_uri_detail* raptor_new_uri_detail(const unsigned char *uri_string);
void raptor_free_uri_detail(raptor_uri_detail* uri_detail);
unsigned char* raptor_uri_detail_to_string(raptor_uri_detail *ud, size_t* len_p);

/* serializers */
int raptor_init_serializer_rdfxml(void);
int raptor_init_serializer_ntriples(void);
int raptor_init_serializer_dot(void);
int raptor_init_serializer_simple(void);
void raptor_delete_serializer_factories(void);

/* raptor_serializer.c */
int raptor_serializers_init(void);
void raptor_serializers_finish(void);

void raptor_serializer_error(raptor_serializer* serializer, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
void raptor_serializer_simple_error(void* serializer, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
void raptor_serializer_error_varargs(raptor_serializer* serializer, const char *message,  va_list arguments) RAPTOR_PRINTF_FORMAT(2, 0);
void raptor_serializer_warning(raptor_serializer* serializer, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);
void raptor_serializer_warning_varargs(raptor_serializer* serializer, const char *message, va_list arguments) RAPTOR_PRINTF_FORMAT(2, 0);

/* raptor_serialize_rdfxmla.c */  
int raptor_init_serializer_rdfxmla(void);

/* raptor_serialize_turtle.c */  
int raptor_init_serializer_turtle(void);

/* raptor_serialize_json.c */  
int raptor_init_serializer_json(void);

/* raptor_utf8.c */
int raptor_unicode_is_namestartchar(raptor_unichar c);
int raptor_unicode_is_namechar(raptor_unichar c);
int raptor_utf8_is_nfc(const unsigned char *input, size_t length);

/* raptor_www*.c */
#ifdef RAPTOR_WWW_LIBXML
#include <libxml/parser.h>
#include <libxml/xmlerror.h>
#include <libxml/nanohttp.h>
#endif

#ifdef RAPTOR_WWW_LIBCURL
#include <curl/curl.h>
#include <curl/types.h>
#include <curl/easy.h>
#endif

/* Size of buffer used in various raptor_www places for I/O  */
#ifndef RAPTOR_WWW_BUFFER_SIZE
#define RAPTOR_WWW_BUFFER_SIZE 4096
#endif

/* WWW library state */
struct  raptor_www_s {
  char *type;
  int free_type;
  int total_bytes;
  int failed;
  int status_code;

  raptor_uri *uri;

#ifdef RAPTOR_WWW_LIBCURL
  CURL* curl_handle;
  char error_buffer[CURL_ERROR_SIZE];
  int curl_init_here;
  int checked_status;
#endif

#ifdef RAPTOR_WWW_LIBXML
  void *ctxt;
  char buffer[RAPTOR_WWW_BUFFER_SIZE];
  int is_end;
  void *old_xmlGenericErrorContext;
#endif

#ifdef RAPTOR_WWW_LIBFETCH
  char buffer[RAPTOR_WWW_BUFFER_SIZE];
#endif

  char *user_agent;

  /* proxy URL string or NULL for none */
  char *proxy;
  
  void *write_bytes_userdata;
  raptor_www_write_bytes_handler write_bytes;
  void *content_type_userdata;
  raptor_www_content_type_handler content_type;

  void* uri_filter_user_data;
  raptor_uri_filter_func uri_filter;

  /* can be filled with error location information */
  raptor_locator locator;

  char *http_accept;

  FILE* handle;

  raptor_error_handlers error_handlers;

  int connection_timeout;

  /* The URI returned after any redirections */
  raptor_uri* final_uri;

  void *final_uri_userdata;
  raptor_www_final_uri_handler final_uri_handler;

  char* cache_control;
};



/* internal */
void raptor_www_libxml_init(raptor_www *www);
void raptor_www_libxml_free(raptor_www *www);
int raptor_www_libxml_fetch(raptor_www *www);

void raptor_www_error(raptor_www *www, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);

void raptor_www_curl_init(raptor_www *www);
void raptor_www_curl_free(raptor_www *www);
int raptor_www_curl_fetch(raptor_www *www);

void raptor_www_libfetch_init(raptor_www *www);
void raptor_www_libfetch_free(raptor_www *www);
int raptor_www_libfetch_fetch(raptor_www *www);

/* raptor_set.c */
raptor_id_set* raptor_new_id_set(void);
void raptor_free_id_set(raptor_id_set* set);
int raptor_id_set_add(raptor_id_set* set, raptor_uri* base_uri, const unsigned char *item, size_t item_len);
#if RAPTOR_DEBUG > 1
void raptor_id_set_stats_print(raptor_id_set* set, FILE *stream);
#endif

/* raptor_sax2.c */
/*
 * SAX2 elements/attributes on stack 
 */
struct raptor_xml_element_s {
  /* NULL at bottom of stack */
  struct raptor_xml_element_s *parent;
  raptor_qname *name;
  raptor_qname **attributes;
  unsigned int attribute_count;

  /* value of xml:lang attribute on this element or NULL */
  const unsigned char *xml_language;

  /* URI of xml:base attribute value on this element or NULL */
  raptor_uri *base_uri;

  /* CDATA content of element and checks for mixed content */
  raptor_stringbuffer* content_cdata_sb;
  unsigned int content_cdata_length;
  /* how many cdata blocks seen */
  unsigned int content_cdata_seen;
  /* how many contained elements seen */
  unsigned int content_element_seen;

  raptor_sequence *declared_nspaces;

  void* user_data;
};


struct raptor_sax2_s {
#ifdef RAPTOR_XML_LIBXML
  int magic;
#endif
  void* user_data;
  
#ifdef RAPTOR_XML_EXPAT
  XML_Parser xp;
#ifdef EXPAT_UTF8_BOM_CRASH
  int tokens_count; /* used to see if trying to get location info is safe */
#endif
#endif
#ifdef RAPTOR_XML_LIBXML
  /* structure holding sax event handlers */
  xmlSAXHandler sax;
  /* parser context */
  xmlParserCtxtPtr xc;
  /* pointer to SAX document locator */
  xmlSAXLocatorPtr loc;

#if LIBXML_VERSION < 20425
  /* flag for some libxml eversions*/
  int first_read;
#endif

#endif  

  /* element depth */
  int depth;

  /* stack of elements - elements add after current_element */
  raptor_xml_element *root_element;
  raptor_xml_element *current_element;

  /* start of an element */
  raptor_sax2_start_element_handler start_element_handler;
  /* end of an element */
  raptor_sax2_end_element_handler end_element_handler;
  /* characters */
  raptor_sax2_characters_handler characters_handler;
  /* like <![CDATA[...]> */
  raptor_sax2_cdata_handler cdata_handler;
  /* comment */
  raptor_sax2_comment_handler comment_handler;
  /* unparsed (NDATA) entity */
  raptor_sax2_unparsed_entity_decl_handler unparsed_entity_decl_handler;
  /* external entity reference */
  raptor_sax2_external_entity_ref_handler external_entity_ref_handler;

  raptor_locator *locator;

  raptor_error_handlers* error_handlers;

  /* New XML namespace callback */
  raptor_namespace_handler  namespace_handler;

  /* FEATURE: 
   * non 0 if require normalizing xml:lang attribute values to lowercase.
   */
  int feature_normalize_language;

  /* FEATURE: 
   * non 0 if network access is denied
   */
  int feature_no_net;

  /* stack of namespaces, most recently added at top */
  raptor_namespace_stack namespaces;

  /* base URI for resolving relative URIs or xml:base URIs */
  raptor_uri* base_uri;
};

int raptor_sax2_init(void);
void raptor_sax2_finish(void);


raptor_xml_element* raptor_xml_element_pop(raptor_sax2* sax2);
void raptor_xml_element_push(raptor_sax2* sax2, raptor_xml_element* element);
int raptor_sax2_get_depth(raptor_sax2* sax2);
void raptor_sax2_inc_depth(raptor_sax2* sax2);
void raptor_sax2_dec_depth(raptor_sax2* sax2);
void raptor_sax2_update_document_locator(raptor_sax2* sax2, raptor_locator* locator);
int raptor_sax2_set_feature(raptor_sax2* sax2, raptor_feature feature, int value);
  
#ifdef RAPTOR_DEBUG
void raptor_print_xml_element(raptor_xml_element *element, FILE* stream);
#endif

void raptor_sax2_start_element(void* user_data, const unsigned char *name, const unsigned char **atts);
void raptor_sax2_end_element(void* user_data, const unsigned char *name);
void raptor_sax2_characters(void* user_data, const unsigned char *s, int len);
void raptor_sax2_cdata(void* user_data, const unsigned char *s, int len);
void raptor_sax2_comment(void* user_data, const unsigned char *s);
void raptor_sax2_unparsed_entity_decl(void* user_data, const unsigned char* entityName, const unsigned char* base, const unsigned char* systemId, const unsigned char* publicId, const unsigned char* notationName);
int raptor_sax2_external_entity_ref(void* user_data, const unsigned char* context, const unsigned char* base, const unsigned char* systemId, const unsigned char* publicId);


/* turtle_parser.y and turtle_lexer.l */
typedef struct raptor_turtle_parser_s raptor_turtle_parser;

/* n3_parser.y and n3_lexer.l */
typedef struct raptor_n3_parser_s raptor_n3_parser;

typedef struct {
  raptor_identifier *subject;
  raptor_identifier *predicate;
  raptor_identifier *object;
} raptor_triple;


/* raptor_rfc2396.c */
struct raptor_uri_detail_s
{
  size_t uri_len;
  /* buffer is the same size as the original uri_len */
  unsigned char *buffer;

  /* URI Components.  These all point into buffer */
  unsigned char *scheme;
  unsigned char *authority;
  unsigned char *path;
  unsigned char *query;
  unsigned char *fragment;

  /* Lengths of the URI Components  */
  size_t scheme_len;
  size_t authority_len;
  size_t path_len;
  size_t query_len;
  size_t fragment_len;

  /* Flags */
  int is_hierarchical;
};


/* for time_t */
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

/* parsedate.c */
#ifdef HAVE_INN_PARSEDATE
#include <libinn.h>
#define RAPTOR_PARSEDATE_FUNCTION parsedate
#else
#ifdef HAVE_RAPTOR_PARSE_DATE
time_t raptor_parse_date(const char *p, time_t *now);
#define RAPTOR_PARSEDATE_FUNCTION raptor_parse_date
#else
#ifdef HAVE_CURL_CURL_H
#include <curl/curl.h>
#define RAPTOR_PARSEDATE_FUNCTION curl_getdate
#endif
#endif
#endif

/* turtle_common.c */
int raptor_stringbuffer_append_turtle_string(raptor_stringbuffer* stringbuffer, const unsigned char *text, size_t len, int delim, raptor_simple_message_handler error_handler, void *error_data);


/* raptor_xsd.c */
raptor_identifier* raptor_new_identifier_from_double(double d);


/* raptor_abbrev.c */

typedef struct {
  int ref_count;         /* count of references to this node */
  int count_as_subject;  /* count of this blank/resource node as subject */
  int count_as_object;   /* count of this blank/resource node as object */
  
  raptor_identifier_type type;  /* node type */
  union {

    struct {
      raptor_uri *uri;
    } resource;

    struct {
      unsigned char *string;
      raptor_uri *datatype;
      unsigned char *language;
    } literal;

    struct {
      int ordinal;
    } ordinal;

    struct {
      unsigned char *string;
    } blank;
    
  } value;
} raptor_abbrev_node;


typedef struct {
  raptor_abbrev_node* node;      /* node representing the subject of
                                  * this resource */
  raptor_abbrev_node* node_type; /* the rdf:type of this resource */
  raptor_avltree *properties;   /* list of properties
                                  * (predicate/object pair) of this
                                  * subject */
  raptor_sequence *list_items;   /* list of container elements if
                                  * is rdf container */
} raptor_abbrev_subject;


raptor_abbrev_node* raptor_new_abbrev_node(raptor_identifier_type node_type, const void *node_data, raptor_uri *datatype, const unsigned char *language);
void raptor_free_abbrev_node(raptor_abbrev_node* node);
int raptor_abbrev_node_cmp(raptor_abbrev_node* node1, raptor_abbrev_node* node2);
int raptor_abbrev_node_equals(raptor_abbrev_node* node1, raptor_abbrev_node* node2);
int raptor_abbrev_node_matches(raptor_abbrev_node* node, raptor_identifier_type node_type, const void *node_data, raptor_uri *datatype, const unsigned char *language);
raptor_abbrev_node* raptor_abbrev_node_lookup(raptor_avltree* nodes, raptor_identifier_type node_type, const void *node_value, raptor_uri *datatype, const unsigned char *language);

raptor_abbrev_subject* raptor_new_abbrev_subject(raptor_abbrev_node* node);
void raptor_free_abbrev_subject(raptor_abbrev_subject* subject);
int raptor_abbrev_subject_add_property(raptor_abbrev_subject* subject, raptor_abbrev_node* predicate, raptor_abbrev_node* object);
int raptor_abbrev_subject_add_list_element(raptor_abbrev_subject* subject, int ordinal, raptor_abbrev_node* object);
raptor_abbrev_subject* raptor_abbrev_subject_find(raptor_sequence *sequence, raptor_identifier_type node_type, const void *node_data, int *idx);
raptor_abbrev_subject* raptor_abbrev_subject_lookup(raptor_avltree* nodes, raptor_sequence* subjects, raptor_sequence* blanks, raptor_identifier_type node_type, const void *node_data);

unsigned char *raptor_unique_id(unsigned char *base);

raptor_qname* raptor_new_qname_from_resource(raptor_sequence* namespaces, raptor_namespace_stack* nstack, int* namespace_count, raptor_abbrev_node* node);


/**
 * raptor_turtle_writer:
 *
 * Raptor Turtle Writer class
 */
typedef struct raptor_turtle_writer_s raptor_turtle_writer;

/* Turtle Writer Class (raptor_turtle_writer) */
raptor_turtle_writer* raptor_new_turtle_writer(raptor_uri* base_uri, int write_base_uri, raptor_namespace_stack *nstack, const raptor_uri_handler *uri_handler, void *uri_context, raptor_iostream* iostr, raptor_simple_message_handler error_handler, void *error_data);
void raptor_free_turtle_writer(raptor_turtle_writer* turtle_writer);
void raptor_turtle_writer_raw(raptor_turtle_writer* turtle_writer, const unsigned char *s);
void raptor_turtle_writer_raw_counted(raptor_turtle_writer* turtle_writer, const unsigned char *s, unsigned int len);
void raptor_turtle_writer_namespace_prefix(raptor_turtle_writer* turtle_writer, raptor_namespace* ns);
void raptor_turtle_writer_base(raptor_turtle_writer* turtle_writer, raptor_uri* base_uri);
void raptor_turtle_writer_increase_indent(raptor_turtle_writer *turtle_writer);
void raptor_turtle_writer_decrease_indent(raptor_turtle_writer *turtle_writer);
void raptor_turtle_writer_newline(raptor_turtle_writer *turtle_writer);
void raptor_turtle_writer_reference(raptor_turtle_writer* turtle_writer, raptor_uri* uri);
int raptor_turtle_writer_literal(raptor_turtle_writer* turtle_writer, raptor_namespace_stack *nstack, const unsigned char *s, const unsigned char* lang, raptor_uri* datatype);
void raptor_turtle_writer_qname(raptor_turtle_writer* turtle_writer, raptor_qname* qname);
int raptor_turtle_writer_quoted_counted_string(raptor_turtle_writer* turtle_writer, const unsigned char *s, size_t length);
void raptor_turtle_writer_comment(raptor_turtle_writer* turtle_writer, const unsigned char *s);
int raptor_turtle_writer_features_enumerate(const raptor_feature feature, const char **name,  raptor_uri **uri, const char **label);
int raptor_turtle_writer_set_feature(raptor_turtle_writer *turtle_writer, raptor_feature feature, int value);
int raptor_turtle_writer_set_feature_string(raptor_turtle_writer *turtle_writer, raptor_feature feature, const unsigned char *value);
int raptor_turtle_writer_get_feature(raptor_turtle_writer *turtle_writer, raptor_feature feature);
const unsigned char *raptor_turtle_writer_get_feature_string(raptor_turtle_writer *turtle_writer, raptor_feature feature);


/**
 * raptor_json_writer:
 *
 * Raptor JSON Writer class
 */
typedef struct raptor_json_writer_s raptor_json_writer;

/* raptor_json_writer.c */
raptor_json_writer* raptor_new_json_writer(raptor_uri* base_uri, const raptor_uri_handler *uri_handler, void *uri_context, raptor_iostream* iostr, raptor_simple_message_handler error_handler, void *error_data);
void raptor_free_json_writer(raptor_json_writer* json_writer);

int raptor_json_writer_newline(raptor_json_writer* json_writer);
int raptor_json_writer_key_value(raptor_json_writer* json_writer, const char* key, size_t key_len, const char* value, size_t value_len);
int raptor_json_writer_start_block(raptor_json_writer* json_writer, char c);
int raptor_json_writer_end_block(raptor_json_writer* json_writer, char c);
int raptor_json_writer_literal_object(raptor_json_writer* json_writer, unsigned char* s, unsigned char* lang, raptor_uri* datatype, const char* key, const char* type_key);
int raptor_json_writer_blank_object(raptor_json_writer* json_writer, const char* blank);
int raptor_json_writer_uri_object(raptor_json_writer* json_writer, raptor_uri* uri);
int raptor_json_writer_key_uri_value(raptor_json_writer* json_writer, const char* key, size_t key_len, raptor_uri* uri);


/* snprintf.c */
char* raptor_format_float(char *buffer, size_t *currlen, size_t maxlen, double fvalue, unsigned int min, unsigned int max, int flags);

/* user functions */
typedef int (*raptor_data_compare_function)(const void* data1, const void* data2);
typedef void (*raptor_data_free_function)(void* data);
typedef int (*raptor_avltree_visit_function)(int depth, void* data, void *user_data);
typedef void (*raptor_data_print_function)(FILE* handle, const void* data);

/* constructor / destructor */
raptor_avltree* raptor_new_avltree(raptor_data_compare_function compare_fn, raptor_data_free_function free_fn, unsigned int flags);
void raptor_free_avltree(raptor_avltree* tree);

/* methods */
int raptor_avltree_add(raptor_avltree* tree, void* p_user);
void* raptor_avltree_remove(raptor_avltree* tree, void* p_data);
int raptor_avltree_delete(raptor_avltree* tree, void* p_user);
void* raptor_avltree_search(raptor_avltree* tree, const void* p_user);
int raptor_avltree_visit(raptor_avltree* tree, raptor_avltree_visit_function visit_fn, void* user_data);
int raptor_avltree_size(raptor_avltree* tree);
void raptor_avltree_set_print_handler(raptor_avltree* tree, raptor_data_print_function print_fn);
void raptor_avltree_print(raptor_avltree* tree, FILE* stream);

#ifdef RAPTOR_DEBUG
int raptor_avltree_dump(raptor_avltree* tree, FILE* stream);
void raptor_avltree_check(raptor_avltree* tree);
#endif
int raptor_avltree_cursor_first(raptor_avltree* tree);
int raptor_avltree_cursor_last(raptor_avltree* tree);
int raptor_avltree_cursor_prev(raptor_avltree* tree);
int raptor_avltree_cursor_next(raptor_avltree* tree);
void* raptor_avltree_cursor_get(raptor_avltree* tree);


/* end of RAPTOR_INTERNAL */
#endif


#ifdef __cplusplus
}
#endif

#endif
