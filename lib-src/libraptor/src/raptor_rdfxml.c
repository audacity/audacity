/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_rdfxml.c - Raptor RDF/XML Parser
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


/* Define these for far too much output */
#undef RAPTOR_DEBUG_VERBOSE
#undef RAPTOR_DEBUG_CDATA


/* Raptor structures */

typedef enum {
  /* Catch uninitialised state */
  RAPTOR_STATE_INVALID = 0,

  /* Skipping current tree of elements - used to recover finding
   * illegal content, when parsling permissively.
   */
  RAPTOR_STATE_SKIPPING,

  /* Not in RDF grammar yet - searching for a start element.
   *
   * This can be <rdf:RDF> (goto NODE_ELEMENT_LIST) but since it is optional,
   * the start element can also be one of  
   *   http://www.w3.org/TR/rdf-syntax-grammar/#nodeElementURIs
   * 
   * If RDF content is assumed, go straight to OBJ
   */
  RAPTOR_STATE_UNKNOWN,

  /* A list of node elements
   *   http://www.w3.org/TR/rdf-syntax-grammar/#nodeElementList 
   */
  RAPTOR_STATE_NODE_ELEMENT_LIST,

  /* Found an <rdf:Description> */
  RAPTOR_STATE_DESCRIPTION,

  /* Found a property element
   *   http://www.w3.org/TR/rdf-syntax-grammar/#propertyElt
   */
  RAPTOR_STATE_PROPERTYELT,

  /* A property element that is an ordinal - rdf:li, rdf:_n
   */
  RAPTOR_STATE_MEMBER_PROPERTYELT,

  /* Found a node element
   *   http://www.w3.org/TR/rdf-syntax-grammar/#nodeElement
   */
  RAPTOR_STATE_NODE_ELEMENT,

  /* A property element with rdf:parseType="Literal"
   *   http://www.w3.org/TR/rdf-syntax-grammar/#parseTypeLiteralPropertyElt
   */
  RAPTOR_STATE_PARSETYPE_LITERAL,

  /* A property element with rdf:parseType="Resource"
   *   http://www.w3.org/TR/rdf-syntax-grammar/#parseTypeResourcePropertyElt
   */
  RAPTOR_STATE_PARSETYPE_RESOURCE,

  /* A property element with rdf:parseType="Collection"
   *  http://www.w3.org/TR/rdf-syntax-grammar/#parseTypeCollectionPropertyElt
   *
   * (This also handles daml:Collection)
   */
  RAPTOR_STATE_PARSETYPE_COLLECTION,

  /* A property element with a rdf:parseType attribute and a value
   * not "Literal" or "Resource"
   *   http://www.w3.org/TR/rdf-syntax-grammar/#parseTypeOtherPropertyElt
   */
  RAPTOR_STATE_PARSETYPE_OTHER,

  RAPTOR_STATE_PARSETYPE_LAST = RAPTOR_STATE_PARSETYPE_OTHER


} raptor_state;


static const char * const raptor_state_names[RAPTOR_STATE_PARSETYPE_LAST+2]={
  "INVALID",
  "SKIPPING",
  "UNKNOWN",
  "nodeElementList",
  "propertyElt",
  "Description",
  "propertyElt",
  "memberPropertyElt",
  "nodeElement",
  "parseTypeLiteral",
  "parseTypeResource",
  "parseTypeCollection",
  "parseTypeOther"
};


static const char * raptor_rdfxml_state_as_string(raptor_state state) 
{
  if(state<1 || state > RAPTOR_STATE_PARSETYPE_LAST)
    state=(raptor_state)0;
  return raptor_state_names[(int)state];
}


/*
 * RDF/XML syntax terms, properties and classes. 
 * Must match names in rdf_syntax_terms_info below.
 */
typedef enum {
  RDF_ATTR_RDF             = 0,
  RDF_ATTR_Description     = 1,
  RDF_ATTR_li              = 2,
  RDF_ATTR_about           = 3, /* value of rdf:about attribute */
  RDF_ATTR_aboutEach       = 4, /* " rdf:aboutEach */
  RDF_ATTR_aboutEachPrefix = 5, /* " rdf:aboutEachPrefix */
  RDF_ATTR_ID              = 6, /* " rdf:ID */
  RDF_ATTR_bagID           = 7, /* " rdf:bagID */
  RDF_ATTR_resource        = 8, /* " rdf:resource */
  RDF_ATTR_parseType       = 9, /* " rdf:parseType */
  RDF_ATTR_nodeID          = 10, /* " rdf:nodeID */
  RDF_ATTR_datatype        = 11, /* " rdf:datatype */
  /* rdf:Property-s */
  RDF_ATTR_type            = 12, /* " rdf:type -- a property in RDF Model */
  RDF_ATTR_value           = 13, /* " rdf:value -- a property in RDF model */
  RDF_ATTR_subject         = 14, /* " rdf:subject -- a property in RDF model */
  RDF_ATTR_predicate       = 15, /* " rdf:predicate -- a property in RDF model */
  RDF_ATTR_object          = 16, /* " rdf:object -- a property in RDF model */
  RDF_ATTR_first           = 17, /* " rdf:first -- a property in RDF model */
  RDF_ATTR_rest            = 18, /* " rdf:rest -- a property in RDF model */
  /* rdfs:Class-s */
  RDF_ATTR_Seq             = 19, /* " rdf:Seq -- a class in RDF Model */
  RDF_ATTR_Bag             = 20, /* " rdf:Bag -- a class in RDF model */
  RDF_ATTR_Alt             = 21, /* " rdf:Alt -- a class in RDF model */
  RDF_ATTR_Statement       = 22, /* " rdf:Statement -- a class in RDF model */
  RDF_ATTR_Property        = 23, /* " rdf:Property -- a class in RDF model */
  RDF_ATTR_List            = 24, /* " rdf:List -- a class in RDF model */
  RDF_ATTR_XMLLiteral      = 25, /* " rdf:XMLLiteral - a cless in RDF graph */
  /* rdfs:Resource-s */
  RDF_ATTR_nil             = 26, /* " rdf:nil -- a resource in RDF graph */

  RDF_ATTR_LAST            = RDF_ATTR_nil
} rdf_attr;


/*
 * http://www.w3.org/TR/rdf-syntax-grammar/#section-grammar-summary
 *
 * coreSyntaxTerms := rdf:RDF | rdf:ID | rdf:about | rdf:bagID | 
                      rdf:parseType | rdf:resource | rdf:nodeID | rdf:datatype
 * syntaxTerms     := coreSyntaxTerms | rdf:Description | rdf:li
 * oldTerms        := rdf:aboutEach | rdf:aboutEachPrefix | rdf:bagID
 *
 * nodeElementURIs       := anyURI - ( coreSyntaxTerms | rdf:li | oldTerms )
 * propertyElementURIs   := anyURI - ( coreSyntaxTerms | rdf:Description | oldTerms )
 * propertyAttributeURIs := anyURI - ( coreSyntaxTerms | rdf:Description | rdf:li | oldTerms )
 *
 * So, forbidden terms in the RDF namespace are:
 * nodeElements 
 *   RDF | ID | about | bagID | parseType | resource | nodeID | datatype |
 *   li | aboutEach | aboutEachPrefix | bagID
 *
 * propertyElements
 *   RDF | ID | about | bagID | parseType | resource | nodeID | datatype |
 *   Description | aboutEach | aboutEachPrefix | bagID
 *
 * propertyAttributes
 *   RDF | ID | about | bagID | parseType | resource | nodeID | datatype |
 *   Description | li | aboutEach | aboutEachPrefix | bagID
 *
 * Information about rdf attributes:
 *   raptor_identifier_type type
 *     Set when the attribute is a property rather than just syntax
 *     NOTE: raptor_rdfxml_process_property_attributes() expects only 
 *      RAPTOR_IDENTIFIER_TYPE_NONE,
 *       RAPTOR_IDENTIFIER_TYPE_LITERAL or RAPTOR_IDENTIFIER_TYPE_RESOURCE
 *   allowed_unprefixed_on_attribute
 *     If allowed for legacy reasons to be unprefixed as an attribute.
 *
 */

static const struct { 
  const char * const name;            /* term name */
  int forbidden_as_nodeElement;
  int forbidden_as_propertyElement;
  int forbidden_as_propertyAttribute;
  const raptor_identifier_type type;  /* statement value */
  int allowed_unprefixed_on_attribute;
} rdf_syntax_terms_info[]={
  /* syntax only */
  { "RDF",             1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  { "Description",     0, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  { "li",              1, 0, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  { "about",           1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 1 },
  { "aboutEach",       1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  { "aboutEachPrefix", 1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  { "ID",              1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 1 },
  { "bagID",           1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 1 },
  { "resource",        1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 1 },
  { "parseType",       1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 1 },
  { "nodeID",          1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  { "datatype",        1, 1, 1, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 },
  /* rdf:Property-s */
  { "type",            0, 0, 0, RAPTOR_IDENTIFIER_TYPE_RESOURCE, 1 },
  { "value",           0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "subject",         0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "predicate",       0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "object",          0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "first",           0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "rest",            0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  /* rdfs:Class-s */
  { "Seq",             0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "Bag",             0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "Alt",             0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "Statement",       0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "Property",        0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "List",            0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { "XMLLiteral",      0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  /* rdfs:Resource-s */
  { "nil",             0, 0, 0, RAPTOR_IDENTIFIER_TYPE_LITERAL , 0 },
  { NULL ,             0, 0, 0, RAPTOR_IDENTIFIER_TYPE_UNKNOWN , 0 }
};


static int
raptor_rdfxml_forbidden_nodeElement_name(const char *name) 
{
  int i;

  if(*name == '_')
    return 0;
  
  for(i=0; rdf_syntax_terms_info[i].name; i++)
    if(!strcmp(rdf_syntax_terms_info[i].name, name))
      return rdf_syntax_terms_info[i].forbidden_as_nodeElement;

  return -1;
}


static int
raptor_rdfxml_forbidden_propertyElement_name(const char *name) 
{
  int i;

  if(*name == '_')
    return 0;
  
  for(i=0; rdf_syntax_terms_info[i].name; i++)
    if(!strcmp(rdf_syntax_terms_info[i].name, (const char*)name))
      return rdf_syntax_terms_info[i].forbidden_as_propertyElement;

  return -1;
}


static int
raptor_rdfxml_forbidden_propertyAttribute_name(const char *name) 
{
  int i;

  if(*name == '_')
    return 0;
  
  for(i=0; rdf_syntax_terms_info[i].name; i++)
    if(!strcmp(rdf_syntax_terms_info[i].name, (const char*)name))
      return rdf_syntax_terms_info[i].forbidden_as_propertyAttribute;

  return -1;
}


typedef enum {
  /* undetermined yet - whitespace is stored */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_UNKNOWN,

  /* literal content - no elements, cdata allowed, whitespace significant 
   * <propElement> blah </propElement>
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL,

  /* parseType literal content (WF XML) - all content preserved
   * <propElement rdf:parseType="Literal"><em>blah</em></propElement> 
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL,

  /* top-level nodes - 0+ elements expected, no cdata, whitespace ignored,
   * any non-whitespace cdata is error
   * only used for <rdf:RDF> or implict <rdf:RDF>
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_NODES,

  /* properties - 0+ elements expected, no cdata, whitespace ignored,
   * any non-whitespace cdata is error
   * <nodeElement><prop1>blah</prop1> <prop2>blah</prop2> </nodeElement>
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES,

  /* property content - all content preserved
   * any content type changes when first non-whitespace found
   * <propElement>...
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTY_CONTENT,

  /* resource URI given - no element, no cdata, whitespace ignored,
   * any non-whitespace cdata is error 
   * <propElement rdf:resource="uri"/>
   * <propElement rdf:resource="uri"></propElement>
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE,

  /* skipping content - all content is preserved 
   * Used when skipping content for unknown parseType-s,
   * error recovery, some other reason
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PRESERVED,

  /* parseType Collection - all content preserved
   * Parsing of this determined by RDF/XML (Revised) closed collection rules
   * <propElement rdf:parseType="Collection">...</propElement>
   */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION,

  /* Like above but handles "daml:collection" */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION,

  /* dummy for use in strings below */
  RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LAST

} raptor_rdfxml_element_content_type;


static const struct {
  const char * const name;
  const int whitespace_significant;
  /* non-blank cdata */
  const int cdata_allowed;
  /* XML element content */
  const int element_allowed;
  /* Do RDF-specific processing? (property attributes, rdf: attributes, ...) */
  const int rdf_processing;
} rdf_content_type_info[RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LAST]={
  {"Unknown",         1, 1, 1, 0 },
  {"Literal",         1, 1, 0, 0 },
  {"XML Literal",     1, 1, 1, 0 },
  {"Nodes",           0, 0, 1, 1 },
  {"Properties",      0, 1, 1, 1 },
  {"Property Content",1, 1, 1, 1 },
  {"Resource",        0, 0, 0, 0 },
  {"Preserved",       1, 1, 1, 0 },
  {"Collection",      1, 1, 1, 1 },
  {"DAML Collection", 1, 1, 1, 1 },
};



static const char *
raptor_rdfxml_element_content_type_as_string(raptor_rdfxml_element_content_type type) 
{
  if(type > RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LAST)
    return "INVALID";
  return rdf_content_type_info[type].name;
}





/*
 * Raptor Element/attributes on stack 
 */
struct raptor_rdfxml_element_s {
  raptor_xml_element *xml_element;

  /* NULL at bottom of stack */
  struct raptor_rdfxml_element_s *parent;

  /* attributes declared in M&S */
  const unsigned char * rdf_attr[RDF_ATTR_LAST+1];
  /* how many of above seen */
  int rdf_attr_count;

  /* state that this production matches */
  raptor_state state;

  /* how to handle the content inside this XML element */
  raptor_rdfxml_element_content_type content_type;


  /* starting state for children of this element */
  raptor_state child_state;

  /* starting content type for children of this element */
  raptor_rdfxml_element_content_type child_content_type;


  /* STATIC Reified statement identifier */
  raptor_identifier reified;

  /* STATIC Bag identifier */
  raptor_identifier bag;
  int last_bag_ordinal; /* starts at 0, so first predicate is rdf:_1 */

  /* STATIC Subject identifier (URI/anon ID), type, source
   *
   * When the XML element represents a node, this is the identifier 
   */
  raptor_identifier subject;
  
  /* STATIC Predicate URI, source is either
   * RAPTOR_URI_SOURCE_ELEMENT or RAPTOR_URI_SOURCE_ATTRIBUTE
   *
   * When the XML element represents a node or predicate,
   * this is the identifier of the predicate 
   */
  raptor_identifier predicate;

  /* STATIC Object identifier (URI/anon ID), type, source
   *
   * When this XML element generates a statement that needs an object,
   * possibly from a child element, this is the identifier of the object 
   */
  raptor_identifier object;

  /* URI of datatype of literal */
  raptor_uri *object_literal_datatype;

  /* last ordinal used, so initialising to 0 works, emitting rdf:_1 first */
  int last_ordinal;

  /* If this element's parseType is a Collection 
   * this identifies the anon node of current tail of the collection(list). 
   */
  const unsigned char *tail_id;

  /* RDF/XML specific checks */

  /* all cdata so far is whitespace */
  unsigned int content_cdata_all_whitespace;
};

typedef struct raptor_rdfxml_element_s raptor_rdfxml_element;


#define RAPTOR_RDFXML_N_CONCEPTS 22

/*
 * Raptor parser object
 */
struct raptor_rdfxml_parser_s {
  raptor_sax2 *sax2;
  
  /* stack of elements - elements add after current_element */
  raptor_rdfxml_element *root_element;
  raptor_rdfxml_element *current_element;

  raptor_uri* concepts[RAPTOR_RDFXML_N_CONCEPTS];

  /* set of seen rdf:ID / rdf:bagID values (with in-scope base URI) */
  raptor_id_set* id_set;

  void *xml_content;
  size_t xml_content_length;
  raptor_iostream* iostream;

  /* writer for building parseType="Literal" content */
  raptor_xml_writer* xml_writer;
};




/* static variables */

#define RAPTOR_RDF_type_URI(rdf_xml_parser)      rdf_xml_parser->concepts[0]
#define RAPTOR_RDF_value_URI(rdf_xml_parser)     rdf_xml_parser->concepts[1]
#define RAPTOR_RDF_subject_URI(rdf_xml_parser)   rdf_xml_parser->concepts[2]
#define RAPTOR_RDF_predicate_URI(rdf_xml_parser) rdf_xml_parser->concepts[3]
#define RAPTOR_RDF_object_URI(rdf_xml_parser)    rdf_xml_parser->concepts[4]
#define RAPTOR_RDF_Statement_URI(rdf_xml_parser) rdf_xml_parser->concepts[5]

#define RAPTOR_RDF_Seq_URI(rdf_xml_parser) rdf_xml_parser->concepts[6]
#define RAPTOR_RDF_Bag_URI(rdf_xml_parser) rdf_xml_parser->concepts[7]
#define RAPTOR_RDF_Alt_URI(rdf_xml_parser) rdf_xml_parser->concepts[8]

#define RAPTOR_RDF_List_URI(rdf_xml_parser)  rdf_xml_parser->concepts[9]
#define RAPTOR_RDF_first_URI(rdf_xml_parser) rdf_xml_parser->concepts[10]
#define RAPTOR_RDF_rest_URI(rdf_xml_parser)  rdf_xml_parser->concepts[11]
#define RAPTOR_RDF_nil_URI(rdf_xml_parser)   rdf_xml_parser->concepts[12]

#define RAPTOR_DAML_NS_URI(rdf_xml_parser)   rdf_xml_parser->concepts[13]

#define RAPTOR_DAML_List_URI(rdf_xml_parser)  rdf_xml_parser->concepts[14]
#define RAPTOR_DAML_first_URI(rdf_xml_parser) rdf_xml_parser->concepts[15]
#define RAPTOR_DAML_rest_URI(rdf_xml_parser)  rdf_xml_parser->concepts[16]
#define RAPTOR_DAML_nil_URI(rdf_xml_parser)   rdf_xml_parser->concepts[17]

#define RAPTOR_RDF_RDF_URI(rdf_xml_parser)         rdf_xml_parser->concepts[18]
#define RAPTOR_RDF_Description_URI(rdf_xml_parser) rdf_xml_parser->concepts[19]
#define RAPTOR_RDF_li_URI(rdf_xml_parser)          rdf_xml_parser->concepts[20]

#define RAPTOR_RDF_XMLLiteral_URI(rdf_xml_parser)  rdf_xml_parser->concepts[21]

/* RAPTOR_RDFXML_N_CONCEPTS defines size of array */


/* prototypes for element functions */
static raptor_rdfxml_element* raptor_rdfxml_element_pop(raptor_rdfxml_parser *rdf_parser);
static void raptor_rdfxml_element_push(raptor_rdfxml_parser *rdf_parser, raptor_rdfxml_element* element);

static int raptor_rdfxml_record_ID(raptor_parser *rdf_parser, raptor_rdfxml_element *element, const unsigned char *id);

/* prototypes for grammar functions */
static void raptor_rdfxml_start_element_grammar(raptor_parser *parser, raptor_rdfxml_element *element);
static void raptor_rdfxml_end_element_grammar(raptor_parser *parser, raptor_rdfxml_element *element);
static void raptor_rdfxml_cdata_grammar(raptor_parser *parser, const unsigned char *s, int len, int is_cdata);


/* prototype for statement related functions */
static void raptor_rdfxml_generate_statement(raptor_parser *rdf_parser, raptor_uri *subject_uri, const unsigned char *subject_id, const raptor_identifier_type subject_type, const raptor_uri_source subject_uri_source, raptor_uri *predicate_uri, const unsigned char *predicate_id, const raptor_identifier_type predicate_type, const raptor_uri_source predicate_uri_source, int predicate_ordinal, raptor_uri *object_uri, const unsigned char *object_id, const raptor_identifier_type object_type, const raptor_uri_source object_uri_source, raptor_uri *literal_datatype, raptor_identifier *reified, raptor_rdfxml_element *bag_element);



/* Prototypes for parsing data functions */
static int raptor_rdfxml_parse_init(raptor_parser* rdf_parser, const char *name);
static void raptor_rdfxml_parse_terminate(raptor_parser *rdf_parser);
static int raptor_rdfxml_parse_start(raptor_parser* rdf_parser);
static int raptor_rdfxml_parse_chunk(raptor_parser* rdf_parser, const unsigned char *buffer, size_t len, int is_end);
static void raptor_rdfxml_update_document_locator(raptor_parser *rdf_parser);

static raptor_uri* raptor_rdfxml_inscope_base_uri(raptor_parser *rdf_parser);


static raptor_rdfxml_element*
raptor_rdfxml_element_pop(raptor_rdfxml_parser *rdf_xml_parser) 
{
  raptor_rdfxml_element *element=rdf_xml_parser->current_element;

  if(!element)
    return NULL;

  rdf_xml_parser->current_element=element->parent;
  if(rdf_xml_parser->root_element == element) /* just deleted root */
    rdf_xml_parser->root_element=NULL;

  return element;
}


static void
raptor_rdfxml_element_push(raptor_rdfxml_parser *rdf_xml_parser, raptor_rdfxml_element* element) 
{
  element->parent=rdf_xml_parser->current_element;
  rdf_xml_parser->current_element=element;
  if(!rdf_xml_parser->root_element)
    rdf_xml_parser->root_element=element;
}


static void
raptor_free_rdfxml_element(raptor_rdfxml_element *element)
{
  int i;
  
  /* Free special RDF M&S attributes */
  for(i=0; i<= RDF_ATTR_LAST; i++) 
    if(element->rdf_attr[i])
      RAPTOR_FREE(cstring, (void*)element->rdf_attr[i]);

  raptor_free_identifier(&element->subject);
  raptor_free_identifier(&element->predicate);
  raptor_free_identifier(&element->object);
  raptor_free_identifier(&element->bag);
  raptor_free_identifier(&element->reified);

  if(element->tail_id)
    RAPTOR_FREE(cstring, (char*)element->tail_id);
  if(element->object_literal_datatype)
    raptor_free_uri(element->object_literal_datatype);

  RAPTOR_FREE(raptor_rdfxml_element, element);
}


static void
raptor_rdfxml_sax2_new_namespace_handler(void *user_data,
                                         raptor_namespace* nspace)
{
  raptor_parser* rdf_parser;
  const unsigned char* namespace_name;
  size_t namespace_name_len;
  raptor_uri* uri=raptor_namespace_get_uri(nspace);

  rdf_parser=(raptor_parser*)user_data;
  raptor_parser_start_namespace(rdf_parser, nspace);

  if(!uri)
    return;
  
  namespace_name=raptor_uri_as_counted_string(uri, &namespace_name_len);
  
  if(namespace_name_len == raptor_rdf_namespace_uri_len-1 && 
     !strncmp((const char*)namespace_name, 
              (const char*)raptor_rdf_namespace_uri, 
              namespace_name_len)) {
    const unsigned char *prefix=raptor_namespace_get_prefix(nspace);
    raptor_parser_warning(rdf_parser, "Declaring a namespace with prefix %s to URI %s - one letter short of the RDF namespace URI and probably a mistake.", prefix, namespace_name);
  } 

  if(namespace_name_len > raptor_rdf_namespace_uri_len && 
     !strncmp((const char*)namespace_name,
              (const char*)raptor_rdf_namespace_uri,
              raptor_rdf_namespace_uri_len)) {
    raptor_parser_error(rdf_parser, "Declaring a namespace URI %s to which the RDF namespace URI is a prefix is forbidden.", namespace_name);
  }
}



static void
raptor_rdfxml_start_element_handler(void *user_data,
                                    raptor_xml_element* xml_element)
{
  raptor_parser* rdf_parser;
  raptor_rdfxml_parser* rdf_xml_parser;
  raptor_rdfxml_element* element;
  int ns_attributes_count=0;
  raptor_qname** named_attrs=NULL;
  int i;
  int count_bumped=0;
  
  rdf_parser=(raptor_parser*)user_data;
  rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  
  if(rdf_parser->failed)
    return;

  raptor_rdfxml_update_document_locator(rdf_parser);

  /* Create new element structure */
  element=(raptor_rdfxml_element*)RAPTOR_CALLOC(raptor_rdfxml_element, 1, 
                                         sizeof(raptor_rdfxml_element));
  if(!element) {
    raptor_parser_fatal_error(rdf_parser, "Out of memory");
    rdf_parser->failed=1;
    return;
  } 
  element->xml_element=xml_element;


  raptor_rdfxml_element_push(rdf_xml_parser, element);

  named_attrs=raptor_xml_element_get_attributes(xml_element);
  ns_attributes_count=raptor_xml_element_get_attributes_count(xml_element);

  /* RDF-specific processing of attributes */
  if(ns_attributes_count) {
    raptor_qname** new_named_attrs;
    int offset = 0;
    raptor_rdfxml_element* parent_element;

    parent_element=element->parent;

    /* Allocate new array to move namespaced-attributes to if
     * rdf processing is performed
     */
    new_named_attrs=(raptor_qname**)RAPTOR_CALLOC(raptor_qname_array, 
                                                  ns_attributes_count, 
                                                  sizeof(raptor_qname*));
    if(!new_named_attrs) {
      raptor_parser_fatal_error(rdf_parser, "Out of memory");
      rdf_parser->failed=1;
      return;
    }

    for (i = 0; i < ns_attributes_count; i++) {
      raptor_qname* attr=named_attrs[i];

      /* If:
       *  1 We are handling RDF content and RDF processing is allowed on
       *    this element
       * OR
       *  2 We are not handling RDF content and 
       *    this element is at the top level (top level Desc. / typedNode)
       *    i.e. we have no parent
       * then handle the RDF attributes
       */
      if((parent_element &&
          rdf_content_type_info[parent_element->child_content_type].rdf_processing) ||
         !parent_element) {

        /* Save pointers to some RDF M&S attributes */
        
        /* If RDF namespace-prefixed attributes */
        if(attr->nspace && attr->nspace->is_rdf_ms) {
          const unsigned char *attr_name=attr->local_name;
          int j;

          for(j=0; j<= RDF_ATTR_LAST; j++)
            if(!strcmp((const char*)attr_name, rdf_syntax_terms_info[j].name)) {
              element->rdf_attr[j]=attr->value;
              element->rdf_attr_count++;
              /* Delete it if it was stored elsewhere */
#if RAPTOR_DEBUG_VERBOSE
              RAPTOR_DEBUG3("Found RDF namespace attribute '%s' URI %s\n", (char*)attr_name, attr->value);
#endif
              /* make sure value isn't deleted from qname structure */
              attr->value=NULL;
              raptor_free_qname(attr);
              attr=NULL;
              break;
            }
        } /* end if RDF namespaced-prefixed attributes */

        if(!attr)
          continue;

        /* If non namespace-prefixed RDF attributes found on an element */
        if(rdf_parser->features[RAPTOR_FEATURE_ALLOW_NON_NS_ATTRIBUTES] &&
           !attr->nspace) {
          const unsigned char *attr_name=attr->local_name;
          int j;

          for(j=0; j<= RDF_ATTR_LAST; j++)
            if(!strcmp((const char*)attr_name, rdf_syntax_terms_info[j].name)) {
              element->rdf_attr[j]=attr->value;
              element->rdf_attr_count++;
              if(!rdf_syntax_terms_info[j].allowed_unprefixed_on_attribute)
                raptor_parser_warning(rdf_parser, "Using rdf attribute '%s' without the RDF namespace has been deprecated.", attr_name);
              /* Delete it if it was stored elsewhere */
              /* make sure value isn't deleted from qname structure */
              attr->value=NULL;
              raptor_free_qname(attr);
              attr=NULL;
              break;
            }
        } /* end if non-namespace prefixed RDF attributes */

        if(!attr)
          continue;

      } /* end if leave literal XML alone */

      if(attr)
        new_named_attrs[offset++]=attr;
    }

    /* new attribute count is set from attributes that haven't been skipped */
    ns_attributes_count=offset;
    if(!ns_attributes_count) {
      /* all attributes were deleted so delete the new array */
      RAPTOR_FREE(raptor_qname_array, new_named_attrs);
      new_named_attrs=NULL;
    }

    RAPTOR_FREE(raptor_qname_array, named_attrs);
    named_attrs=new_named_attrs;
    raptor_xml_element_set_attributes(xml_element, 
                                      named_attrs, ns_attributes_count);
  } /* end if ns_attributes_count */


  /* start from unknown; if we have a parent, it may set this */
  element->state=RAPTOR_STATE_UNKNOWN;
  element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_UNKNOWN;

  if(element->parent && 
     element->parent->child_content_type != RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_UNKNOWN) {
    element->content_type=element->parent->child_content_type;
      
    if(element->parent->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE &&
       element->content_type != RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION &&
       element->content_type != RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) {
      /* If parent has an rdf:resource, this element should not be here */
      raptor_parser_error(rdf_parser, "property element '%s' has multiple object node elements, skipping.", 
                            raptor_xml_element_get_name(element->parent->xml_element)->local_name);
      element->state=RAPTOR_STATE_SKIPPING;
      element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PRESERVED;

    } else {
      if(!element->parent->child_state) {
        raptor_parser_fatal_error(rdf_parser, "raptor_rdfxml_start_element_handler: no parent element child_state set");
        return;
      }

      element->state=element->parent->child_state;
      element->parent->xml_element->content_element_seen++;
      count_bumped++;
    
      /* leave literal XML alone */
      if (!rdf_content_type_info[element->content_type].cdata_allowed) {
        if(element->parent->xml_element->content_element_seen &&
           element->parent->xml_element->content_cdata_seen) {
          /* Uh oh - mixed content, the parent element has cdata too */
          raptor_parser_warning(rdf_parser, "element '%s' has mixed content.", 
                                raptor_xml_element_get_name(element->parent->xml_element)->local_name);
        }
        
        /* If there is some existing all-whitespace content cdata
         * before this node element, delete it
         */
        if(element->parent->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES &&
           element->parent->xml_element->content_element_seen &&
           element->parent->content_cdata_all_whitespace &&
           element->parent->xml_element->content_cdata_length) {
          
          element->parent->content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
          
          raptor_free_stringbuffer(element->parent->xml_element->content_cdata_sb);
          element->parent->xml_element->content_cdata_sb=NULL;
          element->parent->xml_element->content_cdata_length=0;
        }
        
      } /* end if leave literal XML alone */
      
    } /* end if parent has no rdf:resource */

  } /* end if element->parent */


#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Using content type %s\n", rdf_content_type_info[element->content_type].name);

  fprintf(stderr, "raptor_rdfxml_start_element_handler: Start ns-element: ");
  raptor_print_xml_element(xml_element, stderr);
#endif

  
  /* Check for non namespaced stuff when not in a parseType literal, other */
  if (rdf_content_type_info[element->content_type].rdf_processing) {

    /* The element */
    /* If has no namespace or the namespace has no name (xmlns="") */
    if(!raptor_xml_element_get_name(xml_element)->nspace ||
       (raptor_xml_element_get_name(xml_element)->nspace &&
        !raptor_namespace_get_uri(raptor_xml_element_get_name(xml_element)->nspace))) {
      raptor_parser_error(rdf_parser, "Using an element '%s' without a namespace is forbidden.", 
                          raptor_xml_element_get_name(element->parent->xml_element)->local_name);
      element->state=RAPTOR_STATE_SKIPPING;
      /* Remove count above so that parent thinks this is empty */
      if(count_bumped)
        element->parent->xml_element->content_element_seen--;
      element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PRESERVED;
    }


    /* Check for any remaining non-namespaced attributes */
    if (named_attrs) {
      for(i=0; i < ns_attributes_count; i++) {
        raptor_qname *attr=named_attrs[i];
        /* Check if any attributes are non-namespaced */
        if(!attr->nspace ||
           (attr->nspace && !raptor_namespace_get_uri(attr->nspace))) {
          raptor_parser_error(rdf_parser, "Using an attribute '%s' without a namespace is forbidden.", attr->local_name);
          raptor_free_qname(attr);
          named_attrs[i]=NULL;
        }
      }
    }
  }
  

  if (element->rdf_attr[RDF_ATTR_aboutEach] || 
      element->rdf_attr[RDF_ATTR_aboutEachPrefix]) {
    raptor_parser_warning(rdf_parser, "element '%s' has aboutEach / aboutEachPrefix, skipping.", 
                          raptor_xml_element_get_name(xml_element)->local_name);
    element->state=RAPTOR_STATE_SKIPPING;
    /* Remove count above so that parent thinks this is empty */
    if(count_bumped)
      element->parent->xml_element->content_element_seen--;
    element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PRESERVED;
  }
  
  /* Right, now ready to enter the grammar */
  raptor_rdfxml_start_element_grammar(rdf_parser, element);

  return;
}


static void
raptor_rdfxml_end_element_handler(void *user_data, 
                                  raptor_xml_element* xml_element)
{
  raptor_parser* rdf_parser;
  raptor_rdfxml_parser* rdf_xml_parser;
  raptor_rdfxml_element* element;

  rdf_parser=(raptor_parser*)user_data;
  rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;

  if(!rdf_parser->failed) {
    raptor_rdfxml_update_document_locator(rdf_parser);

    raptor_rdfxml_end_element_grammar(rdf_parser, rdf_xml_parser->current_element);
  }
  
  element=raptor_rdfxml_element_pop(rdf_xml_parser);
  if(element) {
    if(element->parent) {
      /* Do not change this; PROPERTYELT will turn into MEMBER if necessary
       * See the switch case for MEMBER / PROPERTYELT where the test is done.
       *
       * PARSETYPE_RESOURCE should never be propogated up since it
       * will turn the next child (node) element into a property
       */
      if(element->state != RAPTOR_STATE_MEMBER_PROPERTYELT &&
         element->state != RAPTOR_STATE_PARSETYPE_RESOURCE)
        element->parent->child_state=element->state;
    }
  
    raptor_free_rdfxml_element(element);
  }
}


/* cdata (and ignorable whitespace for libxml). 
 * s is not 0 terminated for expat, is for libxml - grrrr.
 */
static void
raptor_rdfxml_characters_handler(void *user_data, 
                                 raptor_xml_element* xml_element,
                                 const unsigned char *s, int len)
{
  raptor_parser* rdf_parser=(raptor_parser*)user_data;

  raptor_rdfxml_cdata_grammar(rdf_parser, s, len, 0);
}


/* cdata (and ignorable whitespace for libxml). 
 * s is not 0 terminated for expat, is for libxml - grrrr.
 */
static void
raptor_rdfxml_cdata_handler(void *user_data, raptor_xml_element* xml_element,
                            const unsigned char *s, int len)
{
  raptor_parser* rdf_parser=(raptor_parser*)user_data;

  raptor_rdfxml_cdata_grammar(rdf_parser, s, len, 1);
}


/* comment handler
 * s is 0 terminated
 */
static void
raptor_rdfxml_comment_handler(void *user_data, raptor_xml_element* xml_element,
                              const unsigned char *s)
{
  raptor_parser* rdf_parser=(raptor_parser*)user_data;
  raptor_rdfxml_parser* rdf_xml_parser;
  raptor_rdfxml_element* element;

  if(rdf_parser->failed || !xml_element)
    return;

  rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  element=rdf_xml_parser->current_element;

  if(element) {
    if(element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL)
      raptor_xml_writer_comment(rdf_xml_parser->xml_writer, s);
  }
  

#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("XML Comment '%s'\n", s);
#endif
}



static int
raptor_rdfxml_parse_init(raptor_parser* rdf_parser, const char *name)
{
  raptor_rdfxml_parser* rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  raptor_sax2* sax2;

  /* Allocate sax2 object */
  sax2=raptor_new_sax2(rdf_parser, &rdf_parser->error_handlers);
  rdf_xml_parser->sax2=sax2;
  if(!sax2)
    return 1;

  /* Initialize sax2 element handlers */
  raptor_sax2_set_start_element_handler(sax2, raptor_rdfxml_start_element_handler);
  raptor_sax2_set_end_element_handler(sax2, raptor_rdfxml_end_element_handler);
  raptor_sax2_set_characters_handler(sax2, raptor_rdfxml_characters_handler);
  raptor_sax2_set_cdata_handler(sax2, raptor_rdfxml_cdata_handler);
  raptor_sax2_set_comment_handler(sax2, raptor_rdfxml_comment_handler);
  raptor_sax2_set_namespace_handler(sax2, raptor_rdfxml_sax2_new_namespace_handler);

  /* Allocate uris */  
  RAPTOR_RDF_type_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("type");
  RAPTOR_RDF_value_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("value");
  RAPTOR_RDF_subject_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("subject");
  RAPTOR_RDF_predicate_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("predicate");
  RAPTOR_RDF_object_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("object");
  RAPTOR_RDF_Statement_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("Statement");

  RAPTOR_RDF_Seq_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("Seq");
  RAPTOR_RDF_Bag_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("Bag");
  RAPTOR_RDF_Alt_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("Alt");

  RAPTOR_RDF_List_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("List");
  RAPTOR_RDF_first_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("first");
  RAPTOR_RDF_rest_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("rest");
  RAPTOR_RDF_nil_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("nil");

  RAPTOR_DAML_NS_URI(rdf_xml_parser)=raptor_new_uri((const unsigned char*)"http://www.daml.org/2001/03/daml+oil#");

  RAPTOR_DAML_List_URI(rdf_xml_parser)=raptor_new_uri_from_uri_local_name(RAPTOR_DAML_NS_URI(rdf_xml_parser), (const unsigned char *)"List");
  RAPTOR_DAML_first_URI(rdf_xml_parser)=raptor_new_uri_from_uri_local_name(RAPTOR_DAML_NS_URI(rdf_xml_parser) ,(const unsigned char *)"first");
  RAPTOR_DAML_rest_URI(rdf_xml_parser)=raptor_new_uri_from_uri_local_name(RAPTOR_DAML_NS_URI(rdf_xml_parser), (const unsigned char *)"rest");
  RAPTOR_DAML_nil_URI(rdf_xml_parser)=raptor_new_uri_from_uri_local_name(RAPTOR_DAML_NS_URI(rdf_xml_parser), (const unsigned char *)"nil");

  RAPTOR_RDF_RDF_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("RDF");
  RAPTOR_RDF_Description_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("Description");
  RAPTOR_RDF_li_URI(rdf_xml_parser)=raptor_new_uri_for_rdf_concept("li");

  RAPTOR_RDF_XMLLiteral_URI(rdf_xml_parser)=raptor_new_uri(raptor_xml_literal_datatype_uri_string);

  /* Check for uri allocation failures */
  if(!RAPTOR_RDF_type_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_value_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_subject_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_predicate_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_object_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_Statement_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_Seq_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_Bag_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_Alt_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_List_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_first_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_rest_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_nil_URI(rdf_xml_parser) ||
     !RAPTOR_DAML_NS_URI(rdf_xml_parser) ||
     !RAPTOR_DAML_List_URI(rdf_xml_parser) ||
     !RAPTOR_DAML_first_URI(rdf_xml_parser) ||
     !RAPTOR_DAML_rest_URI(rdf_xml_parser) ||
     !RAPTOR_DAML_nil_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_RDF_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_Description_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_li_URI(rdf_xml_parser) ||
     !RAPTOR_RDF_XMLLiteral_URI(rdf_xml_parser))
    return 1;

  /* Create id set object */
  rdf_xml_parser->id_set=raptor_new_id_set();
  if(!rdf_xml_parser->id_set)
    return 1;

  /* Everything succeeded */
  return 0;
}


static int
raptor_rdfxml_parse_start(raptor_parser* rdf_parser)
{
  raptor_uri *uri=rdf_parser->base_uri;
  raptor_rdfxml_parser* rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;

  /* base URI required for RDF/XML */
  if(!uri)
    return 1;

  /* Optionally normalize language to lowercase
   * http://www.w3.org/TR/rdf-concepts/#dfn-language-identifier
   */
  raptor_sax2_set_feature(rdf_xml_parser->sax2,
                          RAPTOR_FEATURE_NORMALIZE_LANGUAGE, 
                          rdf_parser->features[RAPTOR_FEATURE_NORMALIZE_LANGUAGE]);

  /* Optionally forbid network requests in the XML parser */
  raptor_sax2_set_feature(rdf_xml_parser->sax2, 
                          RAPTOR_FEATURE_NO_NET,
                          rdf_parser->features[RAPTOR_FEATURE_NO_NET]);
  
  raptor_sax2_parse_start(rdf_xml_parser->sax2, uri);

  return 0;
}


static void
raptor_rdfxml_parse_terminate(raptor_parser *rdf_parser) 
{
  raptor_rdfxml_parser* rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  raptor_rdfxml_element* element;
  int i;

  if(rdf_xml_parser->sax2) {
    raptor_free_sax2(rdf_xml_parser->sax2);
    rdf_xml_parser->sax2=NULL;
  }
  
  while( (element=raptor_rdfxml_element_pop(rdf_xml_parser)) )
    raptor_free_rdfxml_element(element);


  for(i=0; i< RAPTOR_RDFXML_N_CONCEPTS; i++) {
    raptor_uri* concept_uri=rdf_xml_parser->concepts[i];
    if(concept_uri) {
      raptor_free_uri(concept_uri);
      rdf_xml_parser->concepts[i]=NULL;
    }
  }
  
  if(rdf_xml_parser->id_set) {
    raptor_free_id_set(rdf_xml_parser->id_set);
    rdf_xml_parser->id_set=NULL;
  }

}


static int
raptor_rdfxml_parse_recognise_syntax(raptor_parser_factory* factory, 
                                     const unsigned char *buffer, size_t len,
                                     const unsigned char *identifier, 
                                     const unsigned char *suffix, 
                                     const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "rdf") || 
       !strcmp((const char*)suffix, "rdfs") ||
       !strcmp((const char*)suffix, "foaf") ||
       !strcmp((const char*)suffix, "doap") ||
       !strcmp((const char*)suffix, "owl") ||
       !strcmp((const char*)suffix, "daml"))
      score=9;
    if(!strcmp((const char*)suffix, "rss"))
      score=3;
  }
  
  if(identifier) {
    if(strstr((const char*)identifier, "rss1"))
      score+=5;
    else if(!suffix && strstr((const char*)identifier, "rss"))
      score+=3;
    else if(!suffix && strstr((const char*)identifier, "rdf"))
      score+=2;
    else if(!suffix && strstr((const char*)identifier, "RDF"))
      score+=2;
  }
  
  if(mime_type) {
    if(strstr((const char*)mime_type, "html"))
      score-= 4;
    else if(!strcmp((const char*)mime_type, "text/rdf"))
      score+= 7;
    else if(!strcmp((const char*)mime_type, "application/xml"))
      score+= 5;
  }

  if(buffer && len) {
    /* Check it's an XML namespace declared and not N3 or Turtle which
     * mention the namespace URI but not in this form.
     */
#define  HAS_RDF_XMLNS1 (strstr((const char*)buffer, "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#") != NULL)
#define  HAS_RDF_XMLNS2 (strstr((const char*)buffer, "xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#") != NULL)
#define  HAS_RDF_XMLNS3 (strstr((const char*)buffer, "xmlns=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#") != NULL)
#define  HAS_RDF_XMLNS4 (strstr((const char*)buffer, "xmlns='http://www.w3.org/1999/02/22-rdf-syntax-ns#") != NULL)
#define  HAS_RDF_ENTITY1 (strstr((const char*)buffer, "<!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>") != NULL)
#define  HAS_RDF_ENTITY2 (strstr((const char*)buffer, "<!ENTITY rdf \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">") != NULL)
#define  HAS_RDF_ENTITY3 (strstr((const char*)buffer, "xmlns:rdf=\"&rdf;\"") != NULL)
#define  HAS_RDF_ENTITY4 (strstr((const char*)buffer, "xmlns:rdf='&rdf;'") != NULL)
#define  HAS_HTML_NS (strstr((const char*)buffer, "http://www.w3.org/1999/xhtml") != NULL)
#define  HAS_HTML_ROOT (strstr((const char*)buffer, "<html") != NULL)

    if(!HAS_HTML_NS && !HAS_HTML_ROOT &&
       (HAS_RDF_XMLNS1 || HAS_RDF_XMLNS2 || HAS_RDF_XMLNS3 || HAS_RDF_XMLNS4 ||
        HAS_RDF_ENTITY1 || HAS_RDF_ENTITY2 || HAS_RDF_ENTITY3 || HAS_RDF_ENTITY4)
      ) {
      int has_rdf_RDF=(strstr((const char*)buffer, "<rdf:RDF") != NULL);
      int has_rdf_Description=(strstr((const char*)buffer, "rdf:Description") != NULL);
      int has_rdf_about=(strstr((const char*)buffer, "rdf:about") != NULL);

      score+= 7;
      if(has_rdf_RDF)
        score++;
      if(has_rdf_Description)
        score++;
      if(has_rdf_about)
        score++;
    }
  }
  
  return score;
}



static int
raptor_rdfxml_parse_chunk(raptor_parser* rdf_parser, const unsigned char *buffer,
                       size_t len, int is_end) 
{
  raptor_rdfxml_parser* rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  if(rdf_parser->failed)
    return 1;

  return raptor_sax2_parse_chunk(rdf_xml_parser->sax2, buffer, len, is_end);
}


static void
raptor_rdfxml_generate_statement(raptor_parser *rdf_parser, 
                          raptor_uri *subject_uri,
                          const unsigned char *subject_id,
                          const raptor_identifier_type subject_type,
                          const raptor_uri_source subject_uri_source,
                          raptor_uri *predicate_uri,
                          const unsigned char *predicate_id,
                          raptor_identifier_type predicate_type,
                          const raptor_uri_source predicate_uri_source,
                          int predicate_ordinal,
                          raptor_uri *object_uri,
                          const unsigned char *object_id,
                          const raptor_identifier_type object_type,
                          const raptor_uri_source object_uri_source,
                          raptor_uri *literal_datatype,
                          raptor_identifier *reified,
                          raptor_rdfxml_element* bag_element)
{
  raptor_statement *statement=&rdf_parser->statement;
  const unsigned char *language=NULL;
  static const char empty_literal[1]="";
  raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  char *reified_id=NULL;
  raptor_uri* uri1=NULL;
  raptor_uri* uri2=NULL;
  
  if(rdf_parser->failed)
    return;

  if((object_type == RAPTOR_IDENTIFIER_TYPE_LITERAL ||
      object_type == RAPTOR_IDENTIFIER_TYPE_XML_LITERAL) &&
     !literal_datatype) {
    language=raptor_sax2_inscope_xml_language(rdf_xml_parser->sax2);
    if(!object_uri)
      object_uri=(raptor_uri*)empty_literal;
  }
  
  statement->subject=subject_uri ? (void*)subject_uri : (void*)subject_id;
  statement->subject_type=subject_type;

  statement->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  if(predicate_type == RAPTOR_IDENTIFIER_TYPE_ORDINAL) {
    /* new URI object */
    uri1=raptor_new_uri_from_rdf_ordinal(predicate_ordinal);
    predicate_uri=uri1;
    predicate_id=NULL;
  }
  statement->predicate=predicate_uri;
  
  statement->object=object_uri ? (void*)object_uri : (void*)object_id;
  statement->object_type=object_type;

  statement->object_literal_language=language;
  statement->object_literal_datatype=literal_datatype;


#ifdef RAPTOR_DEBUG_VERBOSE
  fprintf(stderr, "raptor_rdfxml_generate_statement: Generating statement: ");
  raptor_print_statement(statement, stderr);
  fputc('\n', stderr);

  if(!(subject_uri||subject_id))
    RAPTOR_FATAL1("Statement has no subject\n");
  
  if(!(predicate_uri||predicate_id))
    RAPTOR_FATAL1("Statement has no predicate\n");
  
  if(!(object_uri||object_id))
    RAPTOR_FATAL1("Statement has no object\n");
  
#endif

  if(!rdf_parser->statement_handler)
    goto generate_tidy;

  /* Generate the statement; or is it fact? */
  (*rdf_parser->statement_handler)(rdf_parser->user_data, statement);


  /* the bagID mess */
  if(rdf_parser->features[RAPTOR_FEATURE_ALLOW_BAGID] &&
     bag_element && (bag_element->bag.uri || bag_element->bag.id)) {
    raptor_identifier* bag=&bag_element->bag;
    
    statement->subject=bag->uri ? (void*)bag->uri : (void*)bag->id;
    statement->subject_type=bag->type;

    bag_element->last_bag_ordinal++;

    /* new URI object */
    uri2=raptor_new_uri_from_rdf_ordinal(bag_element->last_bag_ordinal);
    statement->predicate=uri2;

    if(reified && (reified->uri || reified->id)) {
      statement->object=reified->uri ? (void*)reified->uri : (void*)reified->id;
      statement->object_type=reified->type;
    } else {
      /* reified may be NULL so do not use it */
      reified_id=(char*)raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
      statement->object=reified_id;
      statement->object_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
    }
    
    (*rdf_parser->statement_handler)(rdf_parser->user_data, statement);
    
  } else if(!reified || (!reified->uri && !reified->id))
    goto generate_tidy;

  /* generate reified statements */
  statement->subject_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  statement->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  statement->object_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;

  statement->object_literal_language=NULL;

  if(reified_id) {
    /* reified may be NULL so do not use it */
    statement->subject=reified_id;
    statement->subject_type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
  } else {
    statement->subject=reified->uri ? (void*)reified->uri : (void*)reified->id;
    statement->subject_type=reified->type;
  }
  
  statement->predicate=RAPTOR_RDF_type_URI(rdf_xml_parser);
  statement->object=RAPTOR_RDF_Statement_URI(rdf_xml_parser);
  (*rdf_parser->statement_handler)(rdf_parser->user_data, statement);

  statement->predicate=RAPTOR_RDF_subject_URI(rdf_xml_parser);
  statement->object=subject_uri ? (void*)subject_uri : (void*)subject_id;
  statement->object_type=subject_type;
  (*rdf_parser->statement_handler)(rdf_parser->user_data, statement);

  statement->predicate=RAPTOR_RDF_predicate_URI(rdf_xml_parser);
  statement->object=predicate_uri ? (void*)predicate_uri : (void*)predicate_id;
  statement->object_type=predicate_type;
  (*rdf_parser->statement_handler)(rdf_parser->user_data, statement);

  statement->predicate=RAPTOR_RDF_object_URI(rdf_xml_parser);
  statement->object=object_uri ? (void*)object_uri : (void*)object_id;
  statement->object_type=object_type;
  statement->object_literal_language=language;

  (*rdf_parser->statement_handler)(rdf_parser->user_data, statement);

 generate_tidy:
  /* Tidy up things allocated here */
  if(reified_id)
    RAPTOR_FREE(cstring, reified_id);
  if(uri1)
    raptor_free_uri(uri1);
  if(uri2)
    raptor_free_uri(uri2);
}



/**
 * raptor_rdfxml_element_has_property_attributes:
 * @element: element with the property attributes 
 *
 * Return true if the element has at least one property attribute.
 * 
 **/
static int
raptor_rdfxml_element_has_property_attributes(raptor_rdfxml_element *element) 
{
  int i;
  
  if(element->xml_element->attribute_count >0)
    return 1;

  /* look for rdf: properties */
  for(i=0; i<= RDF_ATTR_LAST; i++) {
    if(element->rdf_attr[i] &&
       rdf_syntax_terms_info[i].type != RAPTOR_IDENTIFIER_TYPE_UNKNOWN)
      return 1;
  }
  return 0;
}


/**
 * raptor_rdfxml_process_property_attributes:
 * @rdf_parser: Raptor parser object
 * @attributes_element: element with the property attributes 
 * @resource_element: element that defines the resource URI 
 *                    subject_uri, subject_uri_source etc.
 * @property_node_identifier: Use this identifier for the resource URI
 *   and count any ordinals for it locally
 *
 * Process the property attributes for an element for a given resource.
 * 
 **/
static void 
raptor_rdfxml_process_property_attributes(raptor_parser *rdf_parser, 
                                          raptor_rdfxml_element *attributes_element,
                                          raptor_rdfxml_element *resource_element,
                                          raptor_identifier *property_node_identifier)
{
  unsigned int i;
  raptor_identifier *resource_identifier;

  resource_identifier=property_node_identifier ? property_node_identifier : &resource_element->subject;
  

  /* Process attributes as propAttr* = * (propName="string")*
   */
  for(i=0; i < attributes_element->xml_element->attribute_count; i++) {
    raptor_qname* attr=attributes_element->xml_element->attributes[i];
    const unsigned char *name;
    const unsigned char *value;
    int handled=0;

    if(!attr)
      continue;
    
    name=attr->local_name;
    value = attr->value;

    if(!attr->nspace) {
      raptor_rdfxml_update_document_locator(rdf_parser);
      raptor_parser_error(rdf_parser, "Using property attribute '%s' without a namespace is forbidden.", name);
      continue;
    }


    if(!raptor_utf8_is_nfc(value, strlen((const char*)value))) {
      const char *message="Property attribute '%s' has a string not in Unicode Normal Form C: %s";
      raptor_rdfxml_update_document_locator(rdf_parser);
      if(rdf_parser->features[RAPTOR_FEATURE_NON_NFC_FATAL])
        raptor_parser_error(rdf_parser, message, name, value);
      else
        raptor_parser_warning(rdf_parser, message, name, value);
      continue;
    }
    

    /* Generate the property statement using one of these properties:
     * 1) rdf:_n
     * 2) the URI from the rdf:* attribute where allowed
     * 3) otherwise forbidden (including rdf:li)
     */
    if(attr->nspace->is_rdf_ms) {
      /* is rdf: namespace */
      int ordinal=0;
        
      if(*name == '_') {
        /* recognise rdf:_ */
        name++;
        ordinal=raptor_check_ordinal(name);
        if(ordinal < 1) {
          raptor_rdfxml_update_document_locator(rdf_parser);
          raptor_parser_error(rdf_parser, "Illegal ordinal value %d in property attribute '%s' seen on containing element '%s'.", ordinal, attr->local_name, name);
          ordinal=1;
        }
      } else {
        raptor_rdfxml_update_document_locator(rdf_parser);
        if(raptor_rdfxml_forbidden_propertyAttribute_name((const char*)name) > 0)
          raptor_parser_error(rdf_parser, "RDF term %s is forbidden as a property attribute.", name);
        else
          raptor_parser_warning(rdf_parser, "Unknown RDF namespace property attribute '%s'.", 
                                name);
      }

      if(ordinal >= 1) {
        /* Generate an ordinal property when there are no problems */
        raptor_rdfxml_generate_statement(rdf_parser, 
                                  resource_identifier->uri,
                                  resource_identifier->id,
                                  resource_identifier->type,
                                  resource_identifier->uri_source,
                                  
                                  NULL,
                                  NULL,
                                  RAPTOR_IDENTIFIER_TYPE_ORDINAL,
                                  RAPTOR_URI_SOURCE_NOT_URI,
                                  ordinal,
                                  
                                  (raptor_uri*)value,
                                  NULL,
                                  RAPTOR_IDENTIFIER_TYPE_LITERAL,
                                  RAPTOR_URI_SOURCE_NOT_URI,
                                  NULL,
                                  
                                  NULL, /* Property attributes are never reified*/
                                  resource_element);
        handled=1;
      }
      
    } /* end is RDF namespace property */


    if(!handled)
      /* else not rdf: namespace or unknown in rdf: namespace so
       * generate a statement with a literal object
       */
      raptor_rdfxml_generate_statement(rdf_parser, 
                                resource_identifier->uri,
                                resource_identifier->id,
                                resource_identifier->type,
                                resource_identifier->uri_source,
                                
                                attr->uri,
                                NULL,
                                RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                RAPTOR_URI_SOURCE_ATTRIBUTE,
                                0,
                                
                                (raptor_uri*)value,
                                NULL,
                                RAPTOR_IDENTIFIER_TYPE_LITERAL,
                                RAPTOR_URI_SOURCE_NOT_URI,
                                NULL,
                                
                                NULL, /* Property attributes are never reified*/
                                resource_element);

  } /* end for ... attributes */


  /* Handle rdf property attributes
   * (only rdf:type and rdf:value at present) 
   */
  for(i=0; i<= RDF_ATTR_LAST; i++) {
    const unsigned char *value=attributes_element->rdf_attr[i];
    int object_is_literal=(rdf_syntax_terms_info[i].type == RAPTOR_IDENTIFIER_TYPE_LITERAL);
    raptor_uri *property_uri, *object_uri;
    raptor_identifier_type object_type;
    
    if(!value)
      continue;

    if(rdf_syntax_terms_info[i].type == RAPTOR_IDENTIFIER_TYPE_UNKNOWN) {
      const char *name=rdf_syntax_terms_info[i].name;
      if(raptor_rdfxml_forbidden_propertyAttribute_name(name)) {
        raptor_rdfxml_update_document_locator(rdf_parser);
        raptor_parser_error(rdf_parser, "RDF term %s is forbidden as a property attribute.", name);
        continue;
      }
    }

    if(object_is_literal && !raptor_utf8_is_nfc(value, strlen((const char*)value))) {
      const char *message="Property attribute '%s' has a string not in Unicode Normal Form C: %s";
      raptor_rdfxml_update_document_locator(rdf_parser);
      if(rdf_parser->features[RAPTOR_FEATURE_NON_NFC_FATAL])
        raptor_parser_error(rdf_parser, message, rdf_syntax_terms_info[i].name, value);
      else
        raptor_parser_warning(rdf_parser, message, rdf_syntax_terms_info[i].name, value);
      continue;
    }

    property_uri=raptor_new_uri_for_rdf_concept(rdf_syntax_terms_info[i].name);
    
    object_uri=object_is_literal ? (raptor_uri*)value : raptor_new_uri_relative_to_base(raptor_rdfxml_inscope_base_uri(rdf_parser), value);
    object_type=object_is_literal ? RAPTOR_IDENTIFIER_TYPE_LITERAL : RAPTOR_IDENTIFIER_TYPE_RESOURCE;
    
    raptor_rdfxml_generate_statement(rdf_parser, 
                              resource_identifier->uri,
                              resource_identifier->id,
                              resource_identifier->type,
                              resource_identifier->uri_source,
                              
                              property_uri,
                              NULL,
                              RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                              RAPTOR_URI_SOURCE_ATTRIBUTE,
                              0,
                              
                              object_uri,
                              NULL,
                              object_type,
                              RAPTOR_URI_SOURCE_NOT_URI,
                              NULL,

                              NULL, /* Property attributes are never reified*/
                              resource_element);
    if(!object_is_literal)
      raptor_free_uri(object_uri);

    raptor_free_uri(property_uri);
    
  } /* end for rdf:property values */

}


static void
raptor_rdfxml_start_element_grammar(raptor_parser *rdf_parser,
                                    raptor_rdfxml_element *element) 
{
  int finished;
  raptor_state state;
  raptor_xml_element* xml_element=element->xml_element;
  const unsigned char *el_name=raptor_xml_element_get_name(xml_element)->local_name;
  int element_in_rdf_ns=(raptor_xml_element_get_name(xml_element)->nspace && 
                         raptor_xml_element_get_name(xml_element)->nspace->is_rdf_ms);
  raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  int rc=0;
  raptor_uri* base_uri;
  
  state=element->state;
#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Starting in state %s\n", raptor_rdfxml_state_as_string(state));
#endif

  base_uri=raptor_rdfxml_inscope_base_uri(rdf_parser);

  finished= 0;
  while(!finished) {
    switch(state) {
      case RAPTOR_STATE_SKIPPING:
        element->child_state=state;
        element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PRESERVED;
        finished=1;
        break;
        
      case RAPTOR_STATE_UNKNOWN:
        /* found <rdf:RDF> ? */

        if(element_in_rdf_ns) {
          if(raptor_uri_equals(raptor_xml_element_get_name(xml_element)->uri, RAPTOR_RDF_RDF_URI(rdf_xml_parser))) {
            element->child_state=RAPTOR_STATE_NODE_ELEMENT_LIST;
            element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_NODES;
            /* Yes - need more content before can continue,
             * so wait for another element
             */
            finished=1;
            break;
          }
          if(raptor_uri_equals(raptor_xml_element_get_name(xml_element)->uri, RAPTOR_RDF_Description_URI(rdf_xml_parser))) {
	    state=RAPTOR_STATE_DESCRIPTION;
	    element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES;
            /* Yes - found something so move immediately to description */
            break;
          }

          if(element_in_rdf_ns && (rc=raptor_rdfxml_forbidden_nodeElement_name((const char*)el_name))) {
            if(rc > 0) {
              raptor_parser_error(rdf_parser, "rdf:%s is forbidden as a node element.", el_name);
              state=RAPTOR_STATE_SKIPPING;
              element->child_state=RAPTOR_STATE_SKIPPING;
              finished=1;
              break;
            } else
              raptor_parser_warning(rdf_parser, "rdf:%s is an unknown RDF namespaced element.", el_name);
          }
        }

        /* If scanning for element, can continue */
        if(rdf_parser->features[RAPTOR_FEATURE_SCANNING]) {
          finished=1;
          break;
        }

        /* Otherwise the choice of the next state can be made
         * from the current element by the OBJ state
         */
        state=RAPTOR_STATE_NODE_ELEMENT_LIST;
        element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_NODES;
        break;


      case RAPTOR_STATE_NODE_ELEMENT_LIST:
        /* Handling
         *   http://www.w3.org/TR/rdf-syntax-grammar/#nodeElementList 
         *
         * Everything goes to nodeElement
         */

        state=RAPTOR_STATE_NODE_ELEMENT;

        element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES;

        break;



      case RAPTOR_STATE_DESCRIPTION:
      case RAPTOR_STATE_NODE_ELEMENT:
      case RAPTOR_STATE_PARSETYPE_RESOURCE:
      case RAPTOR_STATE_PARSETYPE_COLLECTION:
        /* Handling <rdf:Description> or other node element
         *   http://www.w3.org/TR/rdf-syntax-grammar/#nodeElement
         *
         * or a property element acting as a node element for
         * rdf:parseType="Resource"
         *   http://www.w3.org/TR/rdf-syntax-grammar/#parseTypeResourcePropertyElt
         * or rdf:parseType="Collection" (and daml:Collection)
         *   http://www.w3.org/TR/rdf-syntax-grammar/#parseTypeCollectionPropertyElt
         *
         * Only create a bag if bagID given
         */

        if(!raptor_xml_element_get_name(xml_element)->uri) {
          /* We cannot handle this */
          raptor_parser_warning(rdf_parser, "Using node element '%s' without a namespace is forbidden.", 
                                raptor_xml_element_get_name(xml_element)->local_name);
          raptor_rdfxml_update_document_locator(rdf_parser);
          element->state=RAPTOR_STATE_SKIPPING;
          element->child_state=RAPTOR_STATE_SKIPPING;
          finished=1;
          break;
        }

        if(element_in_rdf_ns &&
           (rc = raptor_rdfxml_forbidden_nodeElement_name((const char*)el_name))) {
          if(rc > 0) {
            raptor_parser_error(rdf_parser, "rdf:%s is forbidden as a node element.", el_name);
            state=RAPTOR_STATE_SKIPPING;
            element->state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          } else
            raptor_parser_warning(rdf_parser, "rdf:%s is an unknown RDF namespaced element.", el_name);
        }

        if(element->content_type !=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION &&
           element->content_type !=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION && 
           element->parent && 
           (element->parent->state == RAPTOR_STATE_PROPERTYELT ||
            element->parent->state == RAPTOR_STATE_MEMBER_PROPERTYELT) &&
           element->parent->xml_element->content_element_seen > 1) {
          raptor_rdfxml_update_document_locator(rdf_parser);
          raptor_parser_error(rdf_parser, "The enclosing property already has an object");
          state=RAPTOR_STATE_SKIPPING;
          element->child_state=RAPTOR_STATE_SKIPPING;
          finished=1;
          break;
        }

        if(state == RAPTOR_STATE_NODE_ELEMENT || 
           state == RAPTOR_STATE_DESCRIPTION || 
           state == RAPTOR_STATE_PARSETYPE_COLLECTION) {
          if(element_in_rdf_ns &&
             raptor_uri_equals(raptor_xml_element_get_name(xml_element)->uri, RAPTOR_RDF_Description_URI(rdf_xml_parser)))
            state=RAPTOR_STATE_DESCRIPTION;
          else
            state=RAPTOR_STATE_NODE_ELEMENT;
        }
        

        if((element->rdf_attr[RDF_ATTR_ID]!=NULL) +
           (element->rdf_attr[RDF_ATTR_about]!=NULL) +
           (element->rdf_attr[RDF_ATTR_nodeID]!=NULL)>1) {
          raptor_rdfxml_update_document_locator(rdf_parser);
          raptor_parser_error(rdf_parser, "Multiple attributes of rdf:ID, rdf:about and rdf:nodeID on element '%s' - only one allowed.", el_name);
        }

        if(element->rdf_attr[RDF_ATTR_ID]) {
          element->subject.id=element->rdf_attr[RDF_ATTR_ID];
          element->rdf_attr[RDF_ATTR_ID]=NULL;
          element->subject.uri=raptor_new_uri_from_id(base_uri, element->subject.id);
          if(!element->subject.uri)
            goto oom;
          element->subject.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          element->subject.uri_source=RAPTOR_URI_SOURCE_ID;
          if(!raptor_valid_xml_ID(rdf_parser, element->subject.id)) {
            raptor_parser_error(rdf_parser, "Illegal rdf:ID value '%s'", element->subject.id);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
          if(raptor_rdfxml_record_ID(rdf_parser, element, element->subject.id)) {
            raptor_parser_error(rdf_parser, "Duplicated rdf:ID value '%s'", element->subject.id);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
        } else if (element->rdf_attr[RDF_ATTR_about]) {
          element->subject.uri=raptor_new_uri_relative_to_base(base_uri, (const unsigned char*)element->rdf_attr[RDF_ATTR_about]);
          RAPTOR_FREE(cstring, (void*)element->rdf_attr[RDF_ATTR_about]);
          element->rdf_attr[RDF_ATTR_about]=NULL;
          if(!element->subject.uri)
            goto oom;
          element->subject.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          element->subject.uri_source=RAPTOR_URI_SOURCE_URI;
        } else if (element->rdf_attr[RDF_ATTR_nodeID]) {
          element->subject.id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, (unsigned char*)element->rdf_attr[RDF_ATTR_nodeID]);
          element->rdf_attr[RDF_ATTR_nodeID]=NULL;
          element->subject.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
          element->subject.uri_source=RAPTOR_URI_SOURCE_BLANK_ID;
          if(!raptor_valid_xml_ID(rdf_parser, element->subject.id)) {
            raptor_parser_error(rdf_parser, "Illegal rdf:nodeID value '%s'", element->subject.id);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
        } else if (element->parent && 
                   element->parent->child_content_type != RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION &&
                   element->parent->child_content_type != RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION &&
                   (element->parent->object.uri || element->parent->object.id)) {
          /* copy from parent (property element), it has a URI for us */
          raptor_copy_identifier(&element->subject, &element->parent->object);
        } else {
          element->subject.id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
          element->subject.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
          element->subject.uri_source=RAPTOR_URI_SOURCE_GENERATED;
        }


        if(element->rdf_attr[RDF_ATTR_bagID]) {
          if(rdf_parser->features[RAPTOR_FEATURE_ALLOW_BAGID]) {
            element->bag.id=element->rdf_attr[RDF_ATTR_bagID];
            element->rdf_attr[RDF_ATTR_bagID]=NULL;
            element->bag.uri=raptor_new_uri_from_id(base_uri, element->bag.id);
            if(!element->bag.uri)
              goto oom;
            element->bag.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
            element->bag.uri_source=RAPTOR_URI_SOURCE_GENERATED;
            
            if(!raptor_valid_xml_ID(rdf_parser, element->bag.id)) {
              raptor_parser_error(rdf_parser, "Illegal rdf:bagID value '%s'", element->bag.id);
              state=RAPTOR_STATE_SKIPPING;
              element->child_state=RAPTOR_STATE_SKIPPING;
              finished=1;
              break;
            }
            if(raptor_rdfxml_record_ID(rdf_parser, element, element->bag.id)) {
              raptor_parser_error(rdf_parser, "Duplicated rdf:bagID value '%s'", element->bag.id);
              state=RAPTOR_STATE_SKIPPING;
              element->child_state=RAPTOR_STATE_SKIPPING;
              finished=1;
              break;
            }

            raptor_parser_warning(rdf_parser, "rdf:bagID is deprecated.");

            raptor_rdfxml_generate_statement(rdf_parser, 
                                      element->bag.uri,
                                      element->bag.id,
                                      element->bag.type,
                                      element->bag.uri_source,
                                      
                                      RAPTOR_RDF_type_URI(rdf_xml_parser),
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                      RAPTOR_URI_SOURCE_URI,
                                      0,
                                      
                                      RAPTOR_RDF_Bag_URI(rdf_xml_parser),
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                      RAPTOR_URI_SOURCE_NOT_URI,
                                      NULL,
                                      
                                      NULL,
                                      NULL);
          } else {
            /* bagID forbidden */
            raptor_parser_error(rdf_parser, "rdf:bagID is forbidden.");
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
        }


        if(element->parent) {

          /* In a rdf:parseType="Collection" the resources are appended
           * to the list at the genid element->parent->tail_id
           */
          if (element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION ||
              element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) {
            const unsigned char * idList = raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
            
            /* <idList> rdf:type rdf:List */
            raptor_uri *collection_uri=(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) ? RAPTOR_DAML_List_URI(rdf_xml_parser) : RAPTOR_RDF_List_URI(rdf_xml_parser);

            if((element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) ||
               rdf_parser->features[RAPTOR_FEATURE_ALLOW_RDF_TYPE_RDF_LIST])
              raptor_rdfxml_generate_statement(rdf_parser, 
                                        NULL,
                                        idList,
                                        RAPTOR_IDENTIFIER_TYPE_ANONYMOUS,
                                        RAPTOR_URI_SOURCE_ID,
                                        
                                        RAPTOR_RDF_type_URI(rdf_xml_parser),
                                        NULL,
                                        RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                        RAPTOR_URI_SOURCE_URI,
                                        0,
                                        
                                        collection_uri,
                                        NULL,
                                        RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                        RAPTOR_URI_SOURCE_URI,
                                        NULL,
                                        
                                        NULL,
                                        element);

            collection_uri=(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) ? RAPTOR_DAML_first_URI(rdf_xml_parser) : RAPTOR_RDF_first_URI(rdf_xml_parser);

            /* <idList> rdf:first <element->uri> */
            raptor_rdfxml_generate_statement(rdf_parser, 
                                      NULL,
                                      idList,
                                      RAPTOR_IDENTIFIER_TYPE_ANONYMOUS,
                                      RAPTOR_URI_SOURCE_ID,

                                      collection_uri,
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                      RAPTOR_URI_SOURCE_URI,
                                      0,

                                      element->subject.uri,
                                      element->subject.id,
                                      element->subject.type,
                                      element->subject.uri_source,
                                      NULL,

                                      NULL,
                                      NULL);
            
            /* If there is no rdf:parseType="Collection" */
            if (!element->parent->tail_id) {
              int len;
              unsigned char *new_id;
              
              /* Free any existing object URI still around
               * I suspect this can never happen.
               */
              if(element->parent->object.uri) {
                abort();
                raptor_free_uri(element->parent->object.uri);
              }

              len=strlen((char*)idList);
              new_id=(unsigned char*)RAPTOR_MALLOC(cstring, len+1);
              if(!len) {
                if(new_id)
                  RAPTOR_FREE(cstring, new_id);
                return;
              }
              strncpy((char*)new_id, (char*)idList, len+1);

              element->parent->object.id=new_id;
              element->parent->object.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
              element->parent->object.uri_source=RAPTOR_URI_SOURCE_ID;
            } else {
              collection_uri=(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) ? RAPTOR_DAML_rest_URI(rdf_xml_parser) : RAPTOR_RDF_rest_URI(rdf_xml_parser);
              /* _:tail_id rdf:rest _:listRest */
              raptor_rdfxml_generate_statement(rdf_parser, 
                                        NULL,
                                        element->parent->tail_id,
                                        RAPTOR_IDENTIFIER_TYPE_ANONYMOUS,
                                        RAPTOR_URI_SOURCE_ID,

                                        collection_uri,
                                        NULL,
                                        RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                        RAPTOR_URI_SOURCE_URI,
                                        0,

                                        NULL,
                                        idList,
                                        RAPTOR_IDENTIFIER_TYPE_ANONYMOUS,
                                        RAPTOR_URI_SOURCE_ID,
                                        NULL,

                                        NULL,
                                        NULL);
            }

            /* update new tail */
            if(element->parent->tail_id)
              RAPTOR_FREE(cstring, (char*)element->parent->tail_id);

            element->parent->tail_id=idList;
            
          } else if(element->parent->state != RAPTOR_STATE_UNKNOWN &&
                    element->state != RAPTOR_STATE_PARSETYPE_RESOURCE) {
            /* If there is a parent element (property) containing this
             * element (node) and it has no object, set it from this subject
             */
            
            if(element->parent->object.uri) {
              raptor_rdfxml_update_document_locator(rdf_parser);
              raptor_parser_error(rdf_parser, "Tried to set multiple objects of a statement");
            } else {
              /* Store URI of this node in our parent as the property object */
              raptor_copy_identifier(&element->parent->object, &element->subject);
              element->parent->content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
            }

          }
        }
        

        /* If this is a node element, generate the rdf:type statement
         * from this node
         */
        if(state == RAPTOR_STATE_NODE_ELEMENT)
          raptor_rdfxml_generate_statement(rdf_parser, 
                                    element->subject.uri,
                                    element->subject.id,
                                    element->subject.type,
                                    element->subject.uri_source,

                                    RAPTOR_RDF_type_URI(rdf_xml_parser),
                                    NULL,
                                    RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                    RAPTOR_URI_SOURCE_URI,
                                    0,

                                    raptor_xml_element_get_name(xml_element)->uri,
                                    NULL,
                                    RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                    element->object.uri_source,
                                    NULL,

                                    &element->reified,
                                    element);

        raptor_rdfxml_process_property_attributes(rdf_parser, element, element, 0);

        /* for both productions now need some more content or
         * property elements before can do any more work.
         */

        element->child_state=RAPTOR_STATE_PROPERTYELT;
        element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES;
        finished=1;
        break;


      case RAPTOR_STATE_PARSETYPE_OTHER:
        /* FALLTHROUGH */

      case RAPTOR_STATE_PARSETYPE_LITERAL:
        raptor_xml_writer_start_element(rdf_xml_parser->xml_writer, xml_element);
        element->child_state = RAPTOR_STATE_PARSETYPE_LITERAL;
        element->child_content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL;
        
        finished=1;
        break;

        /* Handle all the detail of the various options of property element
         *   http://www.w3.org/TR/rdf-syntax-grammar/#propertyElt
         *
         * All the attributes must be scanned here to see what additional
         * property element work is needed.  No triples are generated
         * until the end of this element, until it is clear if the
         * element was empty.
         */
      case RAPTOR_STATE_MEMBER_PROPERTYELT:
      case RAPTOR_STATE_PROPERTYELT:

        if(!raptor_xml_element_get_name(xml_element)->uri) {
          raptor_parser_error(rdf_parser, "Using property element '%s' without a namespace is forbidden.", 
                              raptor_xml_element_get_name(element->parent->xml_element)->local_name);
          raptor_rdfxml_update_document_locator(rdf_parser);
          element->state=RAPTOR_STATE_SKIPPING;
          element->child_state=RAPTOR_STATE_SKIPPING;
          finished=1;
          break;
        }

        /* Handling rdf:li as a property, noting special processing */ 
        if(element_in_rdf_ns && 
           raptor_uri_equals(raptor_xml_element_get_name(xml_element)->uri, RAPTOR_RDF_li_URI(rdf_xml_parser))) {
          state=RAPTOR_STATE_MEMBER_PROPERTYELT;
        }


        if(element_in_rdf_ns && 
           (rc = raptor_rdfxml_forbidden_propertyElement_name((const char*)el_name))) {
          if(rc > 0) {
            raptor_parser_error(rdf_parser, "rdf:%s is forbidden as a property element.", el_name);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          } else
            raptor_parser_warning(rdf_parser, "rdf:%s is an unknown RDF namespaced element.", el_name);
        }
          

        /* rdf:ID on a property element - reify a statement. 
         * Allowed on all property element forms
         */
        if(element->rdf_attr[RDF_ATTR_ID]) {
          element->reified.id=element->rdf_attr[RDF_ATTR_ID];
          element->rdf_attr[RDF_ATTR_ID]=NULL;
          element->reified.uri=raptor_new_uri_from_id(base_uri, element->reified.id);
          if(!element->reified.uri)
            goto oom;
          element->reified.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
          element->reified.uri_source=RAPTOR_URI_SOURCE_GENERATED;

          if(!raptor_valid_xml_ID(rdf_parser, element->reified.id)) {
            raptor_parser_error(rdf_parser, "Illegal rdf:ID value '%s'", element->reified.id);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
          if(raptor_rdfxml_record_ID(rdf_parser, element, element->reified.id)) {
            raptor_parser_error(rdf_parser, "Duplicated rdf:ID value '%s'", element->reified.id);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
        }
        
        /* rdf:datatype on a property element.  
         * Only allowed for
         *   http://www.w3.org/TR/rdf-syntax-grammar/#literalPropertyElt 
         */
        if (element->rdf_attr[RDF_ATTR_datatype]) {
          element->object_literal_datatype=raptor_new_uri_relative_to_base(base_uri, (const unsigned char*)element->rdf_attr[RDF_ATTR_datatype]);
          RAPTOR_FREE(cstring, (void*)element->rdf_attr[RDF_ATTR_datatype]); 
          element->rdf_attr[RDF_ATTR_datatype]=NULL; 
          if(!element->object_literal_datatype)
            goto oom;
        }

        if(element->rdf_attr[RDF_ATTR_bagID]) {

          if(rdf_parser->features[RAPTOR_FEATURE_ALLOW_BAGID]) {

            if(element->rdf_attr[RDF_ATTR_resource] ||
               element->rdf_attr[RDF_ATTR_parseType]) {
              
              raptor_parser_error(rdf_parser, "rdf:bagID is forbidden on property element '%s' with an rdf:resource or rdf:parseType attribute.", el_name);
              /* prevent this being used later either */
              element->rdf_attr[RDF_ATTR_bagID]=NULL;
            } else {
              element->bag.id=element->rdf_attr[RDF_ATTR_bagID];
              element->rdf_attr[RDF_ATTR_bagID]=NULL;
              element->bag.uri=raptor_new_uri_from_id(base_uri, element->bag.id);
              if(!element->bag.uri)
                goto oom;
              element->bag.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
              element->bag.uri_source=RAPTOR_URI_SOURCE_GENERATED;
              
              if(!raptor_valid_xml_ID(rdf_parser, element->bag.id)) {
                raptor_parser_error(rdf_parser, "Illegal rdf:bagID value '%s'", element->bag.id);
                state=RAPTOR_STATE_SKIPPING;
                element->child_state=RAPTOR_STATE_SKIPPING;
                finished=1;
                break;
              }
              if(raptor_rdfxml_record_ID(rdf_parser, element, element->bag.id)) {
                raptor_parser_error(rdf_parser, "Duplicated rdf:bagID value '%s'", element->bag.id);
                state=RAPTOR_STATE_SKIPPING;
                element->child_state=RAPTOR_STATE_SKIPPING;
                finished=1;
                break;
              }

              raptor_parser_warning(rdf_parser, "rdf:bagID is deprecated.");
            }
          } else {
            /* bagID forbidden */
            raptor_parser_error(rdf_parser, "rdf:bagID is forbidden.");
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }
        } /* if rdf:bagID on property element */
        

        element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTY_CONTENT;

        if (element->rdf_attr[RDF_ATTR_parseType]) {
          const unsigned char *parse_type=element->rdf_attr[RDF_ATTR_parseType];
          int i;
          int is_parseType_Literal=0;

          if(raptor_rdfxml_element_has_property_attributes(element)) {
            raptor_parser_error(rdf_parser, "Property attributes cannot be used with rdf:parseType='%s'", parse_type);
            state=RAPTOR_STATE_SKIPPING;
            element->child_state=RAPTOR_STATE_SKIPPING;
            finished=1;
            break;
          }

          /* Check for bad combinations of things with parseType */
          for(i=0; i<= RDF_ATTR_LAST; i++)
            if(element->rdf_attr[i] && i != RDF_ATTR_parseType) {
              raptor_parser_error(rdf_parser, "Attribute '%s' cannot be used with rdf:parseType='%s'", rdf_syntax_terms_info[i].name, parse_type);
              state=RAPTOR_STATE_SKIPPING;
              element->child_state=RAPTOR_STATE_SKIPPING;
              finished=1;
              break;
            }


          if(!strcmp((char*)parse_type, "Literal"))
            is_parseType_Literal=1;
          else if (!strcmp((char*)parse_type, "Resource")) {
            state=RAPTOR_STATE_PARSETYPE_RESOURCE;
            element->child_state=RAPTOR_STATE_PROPERTYELT;
            element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES;

            /* create a node for the subject of the contained properties */
            element->subject.id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
            element->subject.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
            element->subject.uri_source=RAPTOR_URI_SOURCE_GENERATED;
          } else if(!strcmp((char*)parse_type, "Collection")) {
            /* An rdf:parseType="Collection" appears as a single node */
            element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
            element->child_state=RAPTOR_STATE_PARSETYPE_COLLECTION;
            element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION;
          } else {
            if(rdf_parser->features[RAPTOR_FEATURE_ALLOW_OTHER_PARSETYPES] &&
               !raptor_strcasecmp((char*)parse_type, "daml:collection")) {
                /* A DAML collection appears as a single node */
                element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
                element->child_state=RAPTOR_STATE_PARSETYPE_COLLECTION;
                element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION;
            } else {
              if(rdf_parser->features[RAPTOR_FEATURE_WARN_OTHER_PARSETYPES]) {
                raptor_parser_warning(rdf_parser, "Unknown rdf:parseType value '%s' taken as 'Literal'", parse_type);
              }
              is_parseType_Literal=1;
            }
            
          }
          
          if(is_parseType_Literal) {
            /* rdf:parseType="Literal" - explicitly or default
             * if the parseType value is not recognised
             */
            const raptor_uri_handler *uri_handler;
            void *uri_context;
            
            raptor_uri_get_handler(&uri_handler, &uri_context);
            rdf_xml_parser->xml_content=NULL;
            rdf_xml_parser->xml_content_length=0;
            rdf_xml_parser->iostream=raptor_new_iostream_to_string(&rdf_xml_parser->xml_content, &rdf_xml_parser->xml_content_length, raptor_alloc_memory);
            if(!rdf_xml_parser->iostream)
              goto oom;
            rdf_xml_parser->xml_writer=raptor_new_xml_writer(NULL,
                                                             uri_handler, uri_context,
                                                             rdf_xml_parser->iostream,
                                                             (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser,
                                                             1);
            if(!rdf_xml_parser->xml_writer)
              goto oom;
            
            raptor_xml_writer_set_feature(rdf_xml_parser->xml_writer, 
                                          RAPTOR_FEATURE_WRITER_XML_DECLARATION, 0);

            element->child_state=RAPTOR_STATE_PARSETYPE_LITERAL;
            element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL;
            element->child_content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL;
          }
        } else {

          /* Can only be the empty property element case
           *   http://www.w3.org/TR/rdf-syntax-grammar/#emptyPropertyElt
           */

          /* The presence of the rdf:resource or rdf:nodeID
           * attributes is checked at element close time
           */

          /*
           * Assign reified URI here so we don't reify property attributes
           * using this id
           */
          if(element->reified.id && !element->reified.uri) {
            element->reified.uri=raptor_new_uri_from_id(base_uri, element->reified.id);
            if(!element->reified.uri)
              goto oom;
            element->reified.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
            element->reified.uri_source=RAPTOR_URI_SOURCE_GENERATED;
          }

          if(element->rdf_attr[RDF_ATTR_resource] ||
             element->rdf_attr[RDF_ATTR_nodeID]) {
            /* Done - wait for end of this element to end in order to 
             * check the element was empty as expected */
            element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
          } else {
            /* Otherwise process content in obj (value) state */
            element->child_state=RAPTOR_STATE_NODE_ELEMENT_LIST;
            element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTY_CONTENT;
          }
        }

        finished=1;

        break;


      case RAPTOR_STATE_INVALID:
      default:
        raptor_parser_fatal_error(rdf_parser, "raptor_rdfxml_start_element_grammar: Unexpected parser state %d - %s", state, raptor_rdfxml_state_as_string(state));
        finished=1;

    } /* end switch */

    if(state != element->state) {
      element->state=state;
#ifdef RAPTOR_DEBUG_VERBOSE
      RAPTOR_DEBUG3("Moved to state %d - %s\n", state, raptor_rdfxml_state_as_string(state));
#endif
    }

  } /* end while */

#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Ending in state %s\n", raptor_rdfxml_state_as_string(state));
#endif

  return;

  oom:
  raptor_parser_fatal_error(rdf_parser, "Out of memory, skipping");
  element->state=RAPTOR_STATE_SKIPPING;
}


static void
raptor_rdfxml_end_element_grammar(raptor_parser *rdf_parser,
                                  raptor_rdfxml_element *element) 
{
  raptor_state state;
  int finished;
  raptor_xml_element* xml_element=element->xml_element;
  const unsigned char *el_name=raptor_xml_element_get_name(xml_element)->local_name;
  int element_in_rdf_ns=(raptor_xml_element_get_name(xml_element)->nspace && 
                         raptor_xml_element_get_name(xml_element)->nspace->is_rdf_ms);
  raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;


  state=element->state;
#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Starting in state %s\n", raptor_rdfxml_state_as_string(state));
#endif

  finished= 0;
  while(!finished) {
    switch(state) {
      case RAPTOR_STATE_SKIPPING:
        finished=1;
        break;

      case RAPTOR_STATE_UNKNOWN:
        finished=1;
        break;

      case RAPTOR_STATE_NODE_ELEMENT_LIST:
        if(element_in_rdf_ns && 
           raptor_uri_equals(raptor_xml_element_get_name(xml_element)->uri, RAPTOR_RDF_RDF_URI(rdf_xml_parser))) {
          /* end of RDF - boo hoo */
          state=RAPTOR_STATE_UNKNOWN;
          finished=1;
          break;
        }
        /* When scanning, another element ending is outside the RDF
         * world so this can happen without further work
         */
        if(rdf_parser->features[RAPTOR_FEATURE_SCANNING]) {
          state=RAPTOR_STATE_UNKNOWN;
          finished=1;
          break;
        }
        /* otherwise found some junk after RDF content in an RDF-only 
         * document (probably never get here since this would be
         * a mismatched XML tag and cause an error earlier)
         */
        raptor_rdfxml_update_document_locator(rdf_parser);
        raptor_parser_warning(rdf_parser, "Element '%s' ended, expected end of RDF element", el_name);
        state=RAPTOR_STATE_UNKNOWN;
        finished=1;
        break;


      case RAPTOR_STATE_DESCRIPTION:
      case RAPTOR_STATE_NODE_ELEMENT:
      case RAPTOR_STATE_PARSETYPE_RESOURCE:

        /* If there is a parent element containing this element and
         * the parent isn't a description, has an identifier,
         * create the statement between this node using parent property
         * (Need to check for identifier so that top-level typed nodes
         * don't get connect to <rdf:RDF> parent element)
         */
        if(state == RAPTOR_STATE_NODE_ELEMENT && 
           element->parent &&
           (element->parent->subject.uri || element->parent->subject.id))
          raptor_rdfxml_generate_statement(rdf_parser, 
                                    element->parent->subject.uri,
                                    element->parent->subject.id,
                                    element->parent->subject.type,
                                    element->parent->subject.uri_source,

                                    raptor_xml_element_get_name(element->parent->xml_element)->uri,
                                    NULL,
                                    RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                    RAPTOR_URI_SOURCE_ELEMENT,
                                    0,

                                    element->subject.uri,
                                    element->subject.id,
                                    element->subject.type,
                                    element->subject.uri_source,
                                    NULL,

                                    NULL,
                                    element);
        else if(state == RAPTOR_STATE_PARSETYPE_RESOURCE && 
                element->parent &&
                (element->parent->subject.uri || element->parent->subject.id)) {
          /* Handle rdf:li as the rdf:parseType="resource" property */
          if(element_in_rdf_ns && 
             raptor_uri_equals(raptor_xml_element_get_name(xml_element)->uri, RAPTOR_RDF_li_URI(rdf_xml_parser))) {
            element->parent->last_ordinal++;
            raptor_rdfxml_generate_statement(rdf_parser, 
                                      element->parent->subject.uri,
                                      element->parent->subject.id,
                                      element->parent->subject.type,
                                      element->parent->subject.uri_source,
                                      
                                      NULL,
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_ORDINAL,
                                      RAPTOR_URI_SOURCE_NOT_URI,
                                      element->parent->last_ordinal,
                                      
                                      element->subject.uri,
                                      element->subject.id,
                                      element->subject.type,
                                      element->subject.uri_source,
                                      NULL,

                                      &element->reified,
                                      element->parent);
          } else {
            raptor_rdfxml_generate_statement(rdf_parser, 
                                      element->parent->subject.uri,
                                      element->parent->subject.id,
                                      element->parent->subject.type,
                                      element->parent->subject.uri_source,
                                      
                                      raptor_xml_element_get_name(xml_element)->uri,
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                      RAPTOR_URI_SOURCE_ELEMENT,
                                      0,
                                      
                                      element->subject.uri,
                                      element->subject.id,
                                      element->subject.type,
                                      element->subject.uri_source,
                                      NULL,

                                      &element->reified,
                                      element->parent);
          }
        }
        finished=1;
        break;

      case RAPTOR_STATE_PARSETYPE_COLLECTION:

        finished=1;
        break;

      case RAPTOR_STATE_PARSETYPE_OTHER:
        /* FALLTHROUGH */

      case RAPTOR_STATE_PARSETYPE_LITERAL:
        element->parent->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL;

        raptor_xml_writer_end_element(rdf_xml_parser->xml_writer, xml_element);

        finished=1;
        break;


      case RAPTOR_STATE_PROPERTYELT:
      case RAPTOR_STATE_MEMBER_PROPERTYELT:
        /* A property element
         *   http://www.w3.org/TR/rdf-syntax-grammar/#propertyElt
         *
         * Literal content part is handled here.
         * The element content is handled in the internal states
         * Empty content is checked here.
         */

        if(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTY_CONTENT) {
          if(xml_element->content_cdata_seen) 
            element->content_type= RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL;
          else if (xml_element->content_element_seen) 
            element->content_type= RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES;
          else { /* Empty Literal */
            element->object.type= RAPTOR_IDENTIFIER_TYPE_LITERAL;
            element->content_type= RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL;
          }
          
        }


        /* Handle terminating a rdf:parseType="Collection" list */
        if(element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION ||
           element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) {
          raptor_uri* nil_uri=(element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) ? RAPTOR_DAML_nil_URI(rdf_xml_parser) : RAPTOR_RDF_nil_URI(rdf_xml_parser);
          if (!element->tail_id) {
            /* If No List: set object of statement to rdf:nil */
            element->object.uri= raptor_uri_copy(nil_uri);
            element->object.id= NULL;
            element->object.type= RAPTOR_IDENTIFIER_TYPE_RESOURCE;
            element->object.uri_source= RAPTOR_URI_SOURCE_URI;
          } else {
            raptor_uri* rest_uri=(element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION) ? RAPTOR_DAML_rest_URI(rdf_xml_parser) : RAPTOR_RDF_rest_URI(rdf_xml_parser);
            /* terminate the list */
            raptor_rdfxml_generate_statement(rdf_parser, 
                                      NULL,
                                      element->tail_id,
                                      RAPTOR_IDENTIFIER_TYPE_ANONYMOUS,
                                      RAPTOR_URI_SOURCE_ID,
                                      
                                      rest_uri,
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                      RAPTOR_URI_SOURCE_URI,
                                      0,
                                      
                                      nil_uri,
                                      NULL,
                                      RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                      RAPTOR_URI_SOURCE_URI,
                                      NULL,

                                      NULL,
                                      NULL);
          }

        } /* end rdf:parseType="Collection" termination */
        

#ifdef RAPTOR_DEBUG_VERBOSE
        RAPTOR_DEBUG3("Content type %s (%d)\n", raptor_rdfxml_element_content_type_as_string(element->content_type), element->content_type);
#endif

        switch(element->content_type) {
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE:

            if(raptor_rdfxml_element_has_property_attributes(element) &&
               element->child_state == RAPTOR_STATE_DESCRIPTION) {
              raptor_parser_error(rdf_parser, "Property element '%s' has both property attributes and a node element content", el_name);
              state=RAPTOR_STATE_SKIPPING;
              element->child_state=RAPTOR_STATE_SKIPPING;
              finished=1;
              break;
            }

            if(element->object.type == RAPTOR_IDENTIFIER_TYPE_UNKNOWN) {
              if(element->rdf_attr[RDF_ATTR_resource]) {
                element->object.uri=raptor_new_uri_relative_to_base(raptor_rdfxml_inscope_base_uri(rdf_parser),
                                                    (const unsigned char*)element->rdf_attr[RDF_ATTR_resource]);
                RAPTOR_FREE(cstring, (void*)element->rdf_attr[RDF_ATTR_resource]);
                element->rdf_attr[RDF_ATTR_resource]=NULL;
                if(!element->object.uri)
                  goto oom;
                element->object.type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
                element->object.uri_source=RAPTOR_URI_SOURCE_URI;
                element->content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
              } else if(element->rdf_attr[RDF_ATTR_nodeID]) {
                element->object.id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, (unsigned char*)element->rdf_attr[RDF_ATTR_nodeID]);
                element->rdf_attr[RDF_ATTR_nodeID]=NULL;
                element->object.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
                element->object.uri_source=RAPTOR_URI_SOURCE_BLANK_ID;
                element->content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
                if(!raptor_valid_xml_ID(rdf_parser, element->object.id)) {
                  raptor_parser_error(rdf_parser, "Illegal rdf:nodeID value '%s'", element->object.id);
                  state=RAPTOR_STATE_SKIPPING;
                  element->child_state=RAPTOR_STATE_SKIPPING;
                  finished=1;
                  break;
                }
              } else {
                element->object.id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
                element->object.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
                element->object.uri_source=RAPTOR_URI_SOURCE_GENERATED;
                element->content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
              }

              raptor_rdfxml_process_property_attributes(rdf_parser, element, 
                                                        element->parent, 
                                                        &element->object);

            }

            /* We know object is a resource, so delete any unsignficant
             * whitespace so that FALLTHROUGH code below finds the object.
             */
            if(xml_element->content_cdata_length) {
              raptor_free_stringbuffer(xml_element->content_cdata_sb);
              xml_element->content_cdata_sb=NULL;
              xml_element->content_cdata_length=0;
            }

            /* FALLTHROUGH */
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL:

            if(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL) {

              if(rdf_parser->features[RAPTOR_FEATURE_ALLOW_BAGID]) {
                /* Only an empty literal can have a rdf:bagID */
                if(element->bag.uri || element->bag.id) {
                  if(xml_element->content_cdata_length > 0) {
                    raptor_parser_error(rdf_parser, "rdf:bagID is forbidden on a literal property element '%s'.", el_name);
                    /* prevent this being used later either */
                    element->rdf_attr[RDF_ATTR_bagID]=NULL;
                  } else
                    raptor_rdfxml_generate_statement(rdf_parser, 
                                              element->bag.uri,
                                              element->bag.id,
                                              element->bag.type,
                                              element->bag.uri_source,
                                              
                                              RAPTOR_RDF_type_URI(rdf_xml_parser),
                                              NULL,
                                              RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                              RAPTOR_URI_SOURCE_URI,
                                              0,
                                              
                                              RAPTOR_RDF_Bag_URI(rdf_xml_parser),
                                              NULL,
                                              RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                              RAPTOR_URI_SOURCE_NOT_URI,
                                              NULL,
                                              
                                              NULL,
                                              NULL);
                }
              } /* if rdf:bagID */

              /* If there is empty literal content with properties
               * generate a node to hang properties off 
               */
              if(raptor_rdfxml_element_has_property_attributes(element) &&
                 xml_element->content_cdata_length > 0) {
                raptor_parser_error(rdf_parser, "Literal property element '%s' has property attributes", el_name);
                state=RAPTOR_STATE_SKIPPING;
                element->child_state=RAPTOR_STATE_SKIPPING;
                finished=1;
                break;
              }

              if(element->object.type == RAPTOR_IDENTIFIER_TYPE_LITERAL &&
                 raptor_rdfxml_element_has_property_attributes(element) &&
                 !element->object.uri) {
                element->object.id=raptor_parser_internal_generate_id(rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
                element->object.type=RAPTOR_IDENTIFIER_TYPE_ANONYMOUS;
                element->object.uri_source=RAPTOR_URI_SOURCE_GENERATED;
                element->content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_RESOURCE;
              }
              
              raptor_rdfxml_process_property_attributes(rdf_parser, element, 
                                                        element,
                                                        &element->object);
            }
            

            /* just be friendly to older compilers and don't declare
             * variables in the middle of a block
             */
            if(1) {
              raptor_uri *predicate_uri=NULL;
              raptor_identifier_type predicate_type;
              int predicate_ordinal=0;
              raptor_uri *object_uri;
              raptor_identifier_type object_type;
              raptor_uri *literal_datatype=NULL;
              const unsigned char* empty_literal=(const unsigned char*)"";

              if(state == RAPTOR_STATE_MEMBER_PROPERTYELT) {
                element->parent->last_ordinal++;
                predicate_ordinal=element->parent->last_ordinal;
                predicate_type=RAPTOR_IDENTIFIER_TYPE_ORDINAL;

              } else {
                predicate_uri=raptor_xml_element_get_name(xml_element)->uri;
                predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
              }


              if(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL) {
                unsigned char* literal;

                object_type=RAPTOR_IDENTIFIER_TYPE_LITERAL;
                literal=raptor_stringbuffer_as_string(xml_element->content_cdata_sb);
                literal_datatype=element->object_literal_datatype;

                if(!literal_datatype && literal &&
                   !raptor_utf8_is_nfc(literal, xml_element->content_cdata_length)) {
                  const char *message="Property element '%s' has a string not in Unicode Normal Form C: %s";
                  raptor_rdfxml_update_document_locator(rdf_parser);
                  if(rdf_parser->features[RAPTOR_FEATURE_NON_NFC_FATAL])
                    raptor_parser_error(rdf_parser, message, el_name, literal);
                  else
                    raptor_parser_warning(rdf_parser, message, el_name, literal);
                }

                if(!literal)
                  /* empty literal */
                  literal=(unsigned char*)empty_literal;

                object_uri=(raptor_uri*)literal;
              } else { 
                object_type=element->object.type;
                object_uri=element->object.uri;
              }

              raptor_rdfxml_generate_statement(rdf_parser, 
                                        element->parent->subject.uri,
                                        element->parent->subject.id,
                                        element->parent->subject.type,
                                        RAPTOR_URI_SOURCE_ELEMENT,

                                        predicate_uri,
                                        NULL,
                                        predicate_type,
                                        RAPTOR_URI_SOURCE_NOT_URI,
                                        predicate_ordinal,

                                        object_uri,
                                        element->object.id,
                                        object_type,
                                        element->object.uri_source,
                                        literal_datatype,

                                        &element->reified,
                                        element->parent);
              
            }
            
            break;

        case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PRESERVED:
        case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL:
            {
              unsigned char *buffer;
              unsigned int length;
              
              if(rdf_xml_parser->xml_writer) {
                raptor_free_iostream(rdf_xml_parser->iostream);
                rdf_xml_parser->iostream=NULL;
                
                buffer=(unsigned char*)rdf_xml_parser->xml_content;
                length=rdf_xml_parser->xml_content_length;
              } else {
                buffer=raptor_stringbuffer_as_string(xml_element->content_cdata_sb);
                length=xml_element->content_cdata_length;
              }

              if(!raptor_utf8_is_nfc(buffer, length)) {
                const char *message="Property element '%s' has XML literal content not in Unicode Normal Form C: %s";
                raptor_rdfxml_update_document_locator(rdf_parser);
                if(rdf_parser->features[RAPTOR_FEATURE_NON_NFC_FATAL])
                  raptor_parser_error(rdf_parser, message, el_name, buffer);
                else
                  raptor_parser_warning(rdf_parser, message, el_name, buffer);
              }
              

              if(state == RAPTOR_STATE_MEMBER_PROPERTYELT) {
                element->parent->last_ordinal++;
                raptor_rdfxml_generate_statement(rdf_parser, 
                                          element->parent->subject.uri,
                                          element->parent->subject.id,
                                          element->parent->subject.type,
                                          element->parent->subject.uri_source,
                                          
                                          NULL,
                                          NULL,
                                          RAPTOR_IDENTIFIER_TYPE_ORDINAL,
                                          RAPTOR_URI_SOURCE_NOT_URI,
                                          element->parent->last_ordinal,
                                          
                                          (raptor_uri*)buffer,
                                          NULL,
                                          RAPTOR_IDENTIFIER_TYPE_LITERAL,
                                          RAPTOR_URI_SOURCE_NOT_URI,
                                          RAPTOR_RDF_XMLLiteral_URI(rdf_xml_parser),
                                          
                                          &element->reified,
                                          element->parent);
              } else {
                raptor_rdfxml_generate_statement(rdf_parser, 
                                          element->parent->subject.uri,
                                          element->parent->subject.id,
                                          element->parent->subject.type,
                                          element->parent->subject.uri_source,
                                          
                                          raptor_xml_element_get_name(xml_element)->uri,
                                          NULL,
                                          RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                                          RAPTOR_URI_SOURCE_ELEMENT,
                                          0,
                                          
                                          (raptor_uri*)buffer,
                                          NULL,
                                          RAPTOR_IDENTIFIER_TYPE_LITERAL,
                                          RAPTOR_URI_SOURCE_NOT_URI,
                                          RAPTOR_RDF_XMLLiteral_URI(rdf_xml_parser),
                                          
                                          &element->reified,
                                          element->parent);
              }
              
              /* Finish the xml writer iostream for parseType="Literal" */
              if(rdf_xml_parser->xml_writer) {
                raptor_free_xml_writer(rdf_xml_parser->xml_writer);
                RAPTOR_FREE(cstring, rdf_xml_parser->xml_content);
                rdf_xml_parser->xml_content=NULL;
                rdf_xml_parser->xml_content_length=0;
              }
            }
            
          break;

          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_COLLECTION:
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_DAML_COLLECTION:
            abort();
            
            break;

          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_NODES:
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES:
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTY_CONTENT:
            
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_UNKNOWN:
          case RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LAST:
          default:
            raptor_parser_fatal_error(rdf_parser, "raptor_rdfxml_end_element_grammar: state RAPTOR_STATE_PROPERTYELT - unexpected content type %s (%d)", raptor_rdfxml_element_content_type_as_string(element->content_type), element->content_type);
            abort();
        } /* end switch */

      finished=1;
      break;

      case RAPTOR_STATE_INVALID:
      default:
        raptor_parser_fatal_error(rdf_parser, "raptor_rdfxml_end_element_grammar: Unexpected parser state %d - %s", state, raptor_rdfxml_state_as_string(state));
        finished=1;

    } /* end switch */

    if(state != element->state) {
      element->state=state;
#ifdef RAPTOR_DEBUG_VERBOSE
      RAPTOR_DEBUG3("Moved to state %d - %s\n", state, raptor_rdfxml_state_as_string(state));
#endif
    }

  } /* end while */

#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Ending in state %s\n", raptor_rdfxml_state_as_string(state));
#endif

  return;

  oom:
  raptor_parser_fatal_error(rdf_parser, "Out of memory, skipping");
  element->state=RAPTOR_STATE_SKIPPING;
}



static void
raptor_rdfxml_cdata_grammar(raptor_parser *rdf_parser,
                            const unsigned char *s, int len,
                            int is_cdata)
{
  raptor_rdfxml_parser* rdf_xml_parser;
  raptor_rdfxml_element* element;
  raptor_xml_element* xml_element;
  raptor_state state;
  int all_whitespace=1;
  int i;

  rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;

  if(rdf_parser->failed)
    return;

#ifdef RAPTOR_DEBUG_CDATA
  RAPTOR_DEBUG2("Adding characters (is_cdata=%d): '", is_cdata);
  (void)fwrite(s, 1, len, stderr);
  fprintf(stderr, "' (%d bytes)\n", len);
#endif

  for(i=0; i<len; i++)
    if(!isspace(s[i])) {
      all_whitespace=0;
      break;
    }

  element=rdf_xml_parser->current_element;

  /* this file is very broke - probably not XML, whatever */
  if(!element)
    return;

  xml_element=element->xml_element;
  
  raptor_rdfxml_update_document_locator(rdf_parser);

  /* cdata never changes the parser state 
   * and the containing element state always determines what to do.
   * Use the child_state first if there is one, since that applies
   */
  state=element->child_state;
#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Working in state %s\n", raptor_rdfxml_state_as_string(state));
#endif


#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG3("Content type %s (%d)\n", raptor_rdfxml_element_content_type_as_string(element->content_type), element->content_type);
#endif
  


  if(state == RAPTOR_STATE_SKIPPING)
    return;

  if(state == RAPTOR_STATE_UNKNOWN) {
    /* Ignore all cdata if still looking for RDF */
    if(rdf_parser->features[RAPTOR_FEATURE_SCANNING])
      return;

    /* Ignore all whitespace cdata before first element */
    if(all_whitespace)
      return;
    
    /* This probably will never happen since that would make the
     * XML not be well-formed
     */
    raptor_parser_warning(rdf_parser, "Character data before RDF element.");
  }


  if(element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTIES) {
    /* If found non-whitespace content, move to literal content */
    if(!all_whitespace)
      element->child_content_type = RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL; 
  }


  if(!rdf_content_type_info[element->child_content_type].whitespace_significant) {

    /* Whitespace is ignored except for literal or preserved content types */
    if(all_whitespace) {
#ifdef RAPTOR_DEBUG_CDATA
      RAPTOR_DEBUG2("Ignoring whitespace cdata inside element '%s'\n", raptor_xml_element_get_name(element->parent->xml_element)->local_name);
#endif
      return;
    }

    if(xml_element->content_cdata_seen && xml_element->content_element_seen) {
      /* Uh oh - mixed content, this element has elements too */
      raptor_parser_warning(rdf_parser, "element '%s' has mixed content.", 
                            raptor_xml_element_get_name(element->parent->xml_element)->local_name);
    }
  }


  if(element->content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_PROPERTY_CONTENT) {
    element->content_type=RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_LITERAL;
#ifdef RAPTOR_DEBUG_VERBOSE
    RAPTOR_DEBUG3("Content type changed to %s (%d)\n", raptor_rdfxml_element_content_type_as_string(element->content_type), element->content_type);
#endif
  }

  if(element->child_content_type == RAPTOR_RDFXML_ELEMENT_CONTENT_TYPE_XML_LITERAL)
    raptor_xml_writer_cdata_counted(rdf_xml_parser->xml_writer, s, len);
  else {
    raptor_stringbuffer_append_counted_string(xml_element->content_cdata_sb,
                                              s, len, 1);
    element->content_cdata_all_whitespace &= all_whitespace;
    
    /* adjust stored length */
    xml_element->content_cdata_length += len;
  }


#ifdef RAPTOR_DEBUG_CDATA
  RAPTOR_DEBUG3("Content cdata now: %d bytes\n", xml_element->content_cdata_length);
#endif
#ifdef RAPTOR_DEBUG_VERBOSE
  RAPTOR_DEBUG2("Ending in state %s\n", raptor_rdfxml_state_as_string(state));
#endif
}



/**
 * raptor_rdfxml_inscope_base_uri:
 * @rdf_parser: Raptor parser object
 *
 * Return the in-scope base URI.
 * 
 * Looks for the innermost xml:base on an element or document URI
 * 
 * Return value: The URI string value or NULL on failure.
 **/
static raptor_uri*
raptor_rdfxml_inscope_base_uri(raptor_parser *rdf_parser)
{
  raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  raptor_uri* base_uri;

  base_uri=raptor_sax2_inscope_base_uri(rdf_xml_parser->sax2);
  if(!base_uri)
    base_uri=rdf_parser->base_uri;

  return base_uri;
}


/**
 * raptor_rdfxml_record_ID:
 * @rdf_parser: Raptor parser object
 * @element: Current element
 * @id: ID string
 *
 * Record an rdf:ID / rdf:bagID value (with xml base) and check it hasn't been seen already.
 * 
 * Record and check the ID values, if they have been seen already.
 * per in-scope-base URI.
 * 
 * Return value: non-zero if already seen, or failure
 **/
static int
raptor_rdfxml_record_ID(raptor_parser *rdf_parser,
                        raptor_rdfxml_element *element,
                        const unsigned char *id)
{
  raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  raptor_uri* base_uri=raptor_rdfxml_inscope_base_uri(rdf_parser);
  size_t id_len=strlen((const char*)id);
  int rc;
  
  if(!rdf_parser->features[RAPTOR_FEATURE_CHECK_RDF_ID])
    return 0;

  rc=raptor_id_set_add(rdf_xml_parser->id_set, base_uri, id, id_len);

  return (rc != 0);
}



static void
raptor_rdfxml_update_document_locator(raptor_parser *rdf_parser)
{
  raptor_rdfxml_parser *rdf_xml_parser=(raptor_rdfxml_parser*)rdf_parser->context;
  raptor_sax2_update_document_locator(rdf_xml_parser->sax2,
                                      &rdf_parser->locator);
}



static void
raptor_rdfxml_parse_finish_factory(raptor_parser_factory* factory)
{
}


static int
raptor_rdfxml_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_rdfxml_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_rdfxml_parse_init;
  factory->terminate = raptor_rdfxml_parse_terminate;
  factory->start     = raptor_rdfxml_parse_start;
  factory->chunk     = raptor_rdfxml_parse_chunk;
  factory->finish_factory = raptor_rdfxml_parse_finish_factory;
  factory->recognise_syntax = raptor_rdfxml_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_alias(factory, "raptor") != 0;

  rc+= raptor_parser_factory_add_uri(factory,
                                     (const unsigned char*)"http://www.w3.org/TR/rdf-syntax-grammar") != 0;

  rc+= raptor_parser_factory_add_mime_type(factory, "application/rdf+xml", 10) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "text/rdf", 6) != 0;

  return rc;
}


int
raptor_init_parser_rdfxml(void)
{
  return !raptor_parser_register_factory("rdfxml", "RDF/XML",
                                         &raptor_rdfxml_parser_register_factory);
}


#if RAPTOR_DEBUG > 1
void
raptor_rdfxml_parser_stats_print(raptor_rdfxml_parser* rdf_xml_parser, 
                                 FILE *stream)
{
  fputs("rdf:ID set ", stream);
  raptor_id_set_stats_print(rdf_xml_parser->id_set, stream);
}
#endif
