/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal.h - Rasqal RDF Query library interfaces and definition
 *
 * Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
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
 */



#ifndef RASQAL_H
#define RASQAL_H


#ifdef __cplusplus
extern "C" {
#endif


/**
 * RASQAL_API:
 *
 * Macro for wrapping API function call declarations.
 *
 */
#ifndef RASQAL_API
#  ifdef WIN32
#    ifdef __GNUC__
#      undef _declspec
#      define _declspec(x) __declspec(x)
#    endif
#    ifdef RASQAL_STATIC
#      define RASQAL_API
#    else
#      ifdef RASQAL_INTERNAL
#        define RASQAL_API _declspec(dllexport)
#      else
#        define RASQAL_API _declspec(dllimport)
#      endif
#    endif
#  else
#    define RASQAL_API
#  endif
#endif

/* Use gcc 3.1+ feature to allow marking of deprecated API calls.
 * This gives a warning during compiling.
 */
#if ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 ) || __GNUC__ > 3
#ifdef __APPLE_CC__
/* OSX gcc cpp-precomp is broken */
#define RASQAL_DEPRECATED
#else
#define RASQAL_DEPRECATED __attribute__((deprecated))
#endif
#else
#define RASQAL_DEPRECATED
#endif


#ifndef LIBRDF_OBJC_FRAMEWORK
#include <raptor.h>
#else
#include <Redland/raptor.h>
#endif

/* Public statics */
RASQAL_API
extern const char * const rasqal_short_copyright_string;
RASQAL_API
extern const char * const rasqal_copyright_string;
RASQAL_API
extern const char * const rasqal_version_string;
RASQAL_API
extern const unsigned int rasqal_version_major;
RASQAL_API
extern const unsigned int rasqal_version_minor;
RASQAL_API
extern const unsigned int rasqal_version_release;
RASQAL_API
extern const unsigned int rasqal_version_decimal;
RASQAL_API
extern const char * const rasqal_license_string;
RASQAL_API
extern const char * const rasqal_home_url_string;


/* Public structures */

#ifndef RASQAL_WORLD_DECLARED
#define RASQAL_WORLD_DECLARED 1
/**
 * rasqal_world:
 *
 * Rasqal world class.
 */
typedef struct rasqal_world_s rasqal_world;
#endif

/**
 * rasqal_query:
 *
 * Rasqal query class.
 */
typedef struct rasqal_query_s rasqal_query;

/**
 * rasqal_query_results:
 *
 * Rasqal query results class.
 */
typedef struct rasqal_query_results_s rasqal_query_results;


#ifndef RASQAL_QUERY_RESULTS_FORMATTER_DECLARED
#define RASQAL_QUERY_RESULTS_FORMATTER_DECLARED 1
/**
 * rasqal_query_results_formatter:
 *
 * Rasqal query results formatter class.
 */
typedef struct rasqal_query_results_formatter_s rasqal_query_results_formatter;
#endif


typedef struct rasqal_literal_s rasqal_literal;

/**
 * rasqal_graph_pattern:
 *
 * Rasqal graph pattern class.
 */
typedef struct rasqal_graph_pattern_s rasqal_graph_pattern;

/**
 * rasqal_feature:
 * @RASQAL_FEATURE_NO_NET: Deny network requests.
 * @RASQAL_FEATURE_LAST: Internal.
 *
 * Query features.
 *
 * None currently defined.
 */
typedef enum {
  RASQAL_FEATURE_NO_NET,
  RASQAL_FEATURE_LAST = RASQAL_FEATURE_NO_NET
} rasqal_feature;


/**
 * rasqal_prefix:
 * @prefix: short prefix string
 * @uri: URI associated with the prefix.
 * @declared: Internal flag.
 * @depth: Internal flag.
 *
 * Namespace (prefix, uri) pair.
 *
 * Includes internal flags used for marking when prefixes are
 * declared and at what XML element depth when used in XML formats.
 */
typedef struct {
  const unsigned char *prefix;
  raptor_uri* uri;
  int declared;
  int depth;
} rasqal_prefix;


/**
 * rasqal_variable_type:
 * @RASQAL_VARIABLE_TYPE_NORMAL: The regular variable type.
 * @RASQAL_VARIABLE_TYPE_ANONYMOUS: Anonymous variable type.
 * @RASQAL_VARIABLE_TYPE_UNKNOWN: Internal.
 *
 * Rasqal variable types.
 *
 * ANONYMOUS can be used in queries but cannot be returned in a
 * result.
 */
typedef enum {
  RASQAL_VARIABLE_TYPE_UNKNOWN   = 0,
  RASQAL_VARIABLE_TYPE_NORMAL    = 1,
  RASQAL_VARIABLE_TYPE_ANONYMOUS = 2
} rasqal_variable_type;


/* forward reference */
struct rasqal_expression_s;

/**
 * rasqal_variable:
 * @name: Variable name.
 * @value: Variable value or NULL if unbound.
 * @offset: Internal.
 * @type: Variable type.
 * @expression: Expression when the variable is a computed SELECT expression 
 *
 * Binding between a variable name and a value.
 *
 * Includes internal field @offset for recording the offset into the
 * (internal) rasqal_query variables array.
 */
typedef struct {
  const unsigned char *name;
  rasqal_literal* value;
  int offset;
  rasqal_variable_type type;
  struct rasqal_expression_s* expression;
} rasqal_variable;


/**
 * rasqal_data_graph_flags:
 * @RASQAL_DATA_GRAPH_NONE: Internal.
 * @RASQAL_DATA_GRAPH_NAMED: Graphs with a source and name.
 * @RASQAL_DATA_GRAPH_BACKGROUND: Graphs with a source only.
 *
 * Flags for the type of #rasqal_data_graph.
 *
 * These are used by rasqal_query_add_data_graph(). See #rasqal_data_graph.
 */
typedef enum {
  RASQAL_DATA_GRAPH_NONE  = 0,
  RASQAL_DATA_GRAPH_NAMED = 1,
  RASQAL_DATA_GRAPH_BACKGROUND = 2,
} rasqal_data_graph_flags;


/**
 * rasqal_data_graph:
 * @uri: source URI
 * @name_uri: name of graph for %RASQAL_DATA_NAMED
 * @flags: %RASQAL_DATA_GRAPH_NAMED or %RASQAL_DATA_GRAPH_BACKGROUND
 *
 * A source of RDF data for querying. 
 *
 * The #uri is the original source (base URI) of the content.  It may
 * also have an additional name @name_uri as long as @flags is
 * %RASQAL_DATA_NAMED
 */
typedef struct {
  raptor_uri* uri;
  raptor_uri* name_uri;
  int flags;
} rasqal_data_graph;


/**
 * rasqal_literal_type:
 * @RASQAL_LITERAL_BLANK: RDF blank node literal (SPARQL r:bNode)
 * @RASQAL_LITERAL_URI: RDF URI Literal (SPARQL r:URI)
 * @RASQAL_LITERAL_STRING: RDF Literal / xsd:string (SPARQL r:Literal)
 * @RASQAL_LITERAL_BOOLEAN: Boolean literal xsd:boolean.
 * @RASQAL_LITERAL_INTEGER: Integer literal xsd:integer.
 * @RASQAL_LITERAL_DOUBLE: Double floating point literal xsd:double.
 * @RASQAL_LITERAL_FLOAT: Floating point literal xsd:float.
 * @RASQAL_LITERAL_DECIMAL: Decimal integer xsd:decimal.
 * @RASQAL_LITERAL_DATETIME: Date/Time literal xsd:dateTime.
 * @RASQAL_LITERAL_PATTERN: Pattern literal for a regex.
 * @RASQAL_LITERAL_QNAME: XML Qname literal.
 * @RASQAL_LITERAL_VARIABLE: Variable literal.
 * @RASQAL_LITERAL_UNKNOWN: Internal.
 * @RASQAL_LITERAL_FIRST_XSD: Internal.
 * @RASQAL_LITERAL_LAST_XSD: Internal.
 * @RASQAL_LITERAL_LAST: Internal.
 *
 * Types of literal.
 *
 * The order in the enumeration is significant as it encodes
 * the SPARQL term ordering conditions:
 *
 *   Blank Nodes << IRIs << RDF literals << typed literals
 *
 * which coresponds to in enum values
 *
 *   BLANK << URI << STRING << 
 *     (BOOLEAN | INTEGER | DOUBLE | FLOAT | DECIMAL | DATETIME)
 *
 *     (RASQAL_LITERAL_FIRST_XSD ... RASQAL_LITERAL_LAST_XSD)
 *
 * Not used (internal): PATTERN, QNAME, VARIABLE
 *
 * See rasqal_literal_compare() when used with flags
 * %RASQAL_COMPARE_XQUERY
 */
typedef enum {
  /* internal */
  RASQAL_LITERAL_UNKNOWN,
  RASQAL_LITERAL_BLANK,
  RASQAL_LITERAL_URI,
  RASQAL_LITERAL_STRING,
  RASQAL_LITERAL_BOOLEAN,
  RASQAL_LITERAL_INTEGER,
  RASQAL_LITERAL_DOUBLE,
  RASQAL_LITERAL_FLOAT,
  RASQAL_LITERAL_DECIMAL,
  RASQAL_LITERAL_DATETIME,
  /* internal */
  RASQAL_LITERAL_FIRST_XSD = RASQAL_LITERAL_STRING,
  /* internal */
  RASQAL_LITERAL_LAST_XSD = RASQAL_LITERAL_DATETIME,
  RASQAL_LITERAL_PATTERN,
  RASQAL_LITERAL_QNAME,
  RASQAL_LITERAL_VARIABLE,
  /* internal */
  RASQAL_LITERAL_LAST= RASQAL_LITERAL_VARIABLE
} rasqal_literal_type;


/**
 * rasqal_xsd_decimal:
 *
 * Rasqal XSD Decimal class.
 */
typedef struct rasqal_xsd_decimal_s rasqal_xsd_decimal;

/**
 * rasqal_literal:
 * @usage: Usage count.
 * @type: Type of literal.
 * @string: String form of literal for literal types UTF-8 string, pattern, qname, blank, double, float, decimal, datetime.
 * @string_len: Length of @string.
 * @value: Alternate value content.
 * @language: Language for string literal type.
 * @datatype: Datatype for string literal type.
 * @flags: Flags for literal types
 *
 * Rasqal literal class.
 *
 */
struct rasqal_literal_s {
  int usage;
  rasqal_literal_type type;
  /* UTF-8 string, pattern, qname, blank, double, float, decimal, datetime */
  const unsigned char *string;
  unsigned int string_len;
  
  union {
    /* integer and boolean types */
    int integer;
    /* double and float */
    double floating;
    /* uri (can be temporarily NULL if a qname, see flags below) */
    raptor_uri* uri;
    /* variable */
    rasqal_variable* variable;
    /* decimal */
    rasqal_xsd_decimal* decimal;
  } value;

  /* for string */
  const char *language;
  raptor_uri *datatype;

  /* various flags for literal types:
   *  pattern  regex flags
   *  string   datatype of qname
   *  uri      qname of URI not yet expanded (temporary)
   */
  const unsigned char *flags;

  /* parent XSD type if any or RASQAL_LITERAL_UNKNOWN */
  rasqal_literal_type parent_type;

  /* world object */
  rasqal_world *world;
};


/**
 * rasqal_op:
 * @RASQAL_EXPR_AND: Expression for AND(A, B)
 * @RASQAL_EXPR_OR: Expression for OR(A, B)
 * @RASQAL_EXPR_EQ: Expression for A equals B
 * @RASQAL_EXPR_NEQ: Expression for A not equals B.
 * @RASQAL_EXPR_LT: Expression for A less than B.
 * @RASQAL_EXPR_GT: Expression for A greather than B.
 * @RASQAL_EXPR_LE: Expression for A less than or equal to B.
 * @RASQAL_EXPR_GE: Expression for A greater than or equal to B.
 * @RASQAL_EXPR_UMINUS: Expression for -A.
 * @RASQAL_EXPR_PLUS: Expression for +A.
 * @RASQAL_EXPR_MINUS: Expression for A-B.
 * @RASQAL_EXPR_STAR: Expression for A*B.
 * @RASQAL_EXPR_SLASH: Expression for A/B.
 * @RASQAL_EXPR_REM: Expression for A/B remainder.
 * @RASQAL_EXPR_STR_EQ: Expression for A string equals B.
 * @RASQAL_EXPR_STR_NEQ: Expression for A string not-equals B.
 * @RASQAL_EXPR_STR_MATCH: Expression for string A matches literal regex B with flags.
 * @RASQAL_EXPR_STR_NMATCH: Expression for string A not-matches literal regex B with flags.
 * @RASQAL_EXPR_REGEX: Expression for string A matches expression regex B with flags.
 * @RASQAL_EXPR_TILDE: Expression for binary not A.
 * @RASQAL_EXPR_BANG: Expression for logical not A.
 * @RASQAL_EXPR_LITERAL: Expression for a #rasqal_literal.
 * @RASQAL_EXPR_FUNCTION: Expression for a function A with arguments (B...).
 * @RASQAL_EXPR_BOUND: Expression for SPARQL ISBOUND(A).
 * @RASQAL_EXPR_STR: Expression for SPARQL STR(A).
 * @RASQAL_EXPR_LANG: Expression for SPARQL LANG(A).
 * @RASQAL_EXPR_LANGMATCHES: Expression for SPARQL LANGMATCHES(A, B).
 * @RASQAL_EXPR_DATATYPE: Expression for SPARQL DATATYPE(A).
 * @RASQAL_EXPR_ISURI: Expression for SPARQL ISURI(A).
 * @RASQAL_EXPR_ISBLANK: Expression for SPARQL ISBLANK(A).
 * @RASQAL_EXPR_ISLITERAL: Expression for SPARQL ISLITERAL(A).
 * @RASQAL_EXPR_CAST: Expression for cast literal A to type B.
 * @RASQAL_EXPR_ORDER_COND_ASC: Expression for SPARQL order condition ascending.
 * @RASQAL_EXPR_ORDER_COND_DESC: Expression for SPARQL order condition descending.
 * @RASQAL_EXPR_GROUP_COND_ASC: Expression for LAQRS group condition ascending.
 * @RASQAL_EXPR_GROUP_COND_DESC: Expression for LAQRS group condition descending.
 * @RASQAL_EXPR_COUNT: Expression for LAQRS select COUNT()
 * @RASQAL_EXPR_VARSTAR: Expression for LAQRS select Variable *
 * @RASQAL_EXPR_SAMETERM: Expression for SPARQL sameTerm
 * @RASQAL_EXPR_UNKNOWN: Internal
 * @RASQAL_EXPR_LAST: Internal
 *
 * Rasqal expression operators.  A mixture of unary, binary and
 * tertiary operators (string matches).  Also includes casting and
 * two ordering operators from ORDER BY in SPARQL.
 */
typedef enum {
  /* internal */
  RASQAL_EXPR_UNKNOWN,
  RASQAL_EXPR_AND,
  RASQAL_EXPR_OR,
  RASQAL_EXPR_EQ,
  RASQAL_EXPR_NEQ,
  RASQAL_EXPR_LT,
  RASQAL_EXPR_GT,
  RASQAL_EXPR_LE,
  RASQAL_EXPR_GE,
  RASQAL_EXPR_UMINUS,
  RASQAL_EXPR_PLUS,
  RASQAL_EXPR_MINUS,
  RASQAL_EXPR_STAR,
  RASQAL_EXPR_SLASH,
  RASQAL_EXPR_REM,
  RASQAL_EXPR_STR_EQ,
  RASQAL_EXPR_STR_NEQ,
  RASQAL_EXPR_STR_MATCH,
  RASQAL_EXPR_STR_NMATCH,
  RASQAL_EXPR_TILDE,
  RASQAL_EXPR_BANG,
  RASQAL_EXPR_LITERAL,
  RASQAL_EXPR_FUNCTION,
  RASQAL_EXPR_BOUND,
  RASQAL_EXPR_STR,
  RASQAL_EXPR_LANG,
  RASQAL_EXPR_DATATYPE,
  RASQAL_EXPR_ISURI,
  RASQAL_EXPR_ISBLANK,
  RASQAL_EXPR_ISLITERAL,
  RASQAL_EXPR_CAST,
  RASQAL_EXPR_ORDER_COND_ASC,
  RASQAL_EXPR_ORDER_COND_DESC,
  RASQAL_EXPR_LANGMATCHES,
  RASQAL_EXPR_REGEX,
  RASQAL_EXPR_GROUP_COND_ASC,
  RASQAL_EXPR_GROUP_COND_DESC,
  RASQAL_EXPR_COUNT,
  RASQAL_EXPR_VARSTAR,
  RASQAL_EXPR_SAMETERM,
  /* internal */
  RASQAL_EXPR_LAST= RASQAL_EXPR_SAMETERM
} rasqal_op;


/**
 * rasqal_expression:
 *
 * expression (arg1), unary op (arg1), binary op (arg1,arg2),
 * literal or variable 
 */
struct rasqal_expression_s {
  int usage; /* reference count - 1 for itself */

  rasqal_op op;
  struct rasqal_expression_s* arg1;
  struct rasqal_expression_s* arg2;
  struct rasqal_expression_s* arg3; /* optional 3rd arg for EXPR_REGEX */
  rasqal_literal* literal;
  unsigned char *value; /* UTF-8 value */

  /* for extension function qname(args...) and cast-to-uri */
  raptor_uri* name;
  raptor_sequence* args;
};
typedef struct rasqal_expression_s rasqal_expression;


/**
 * rasqal_triple_flags:
 * @RASQAL_TRIPLE_FLAGS_EXACT: Not used.
 * @RASQAL_TRIPLE_FLAGS_OPTIONAL: Not used.
 * @RASQAL_TRIPLE_FLAGS_LAST: Internal.
 *
 * Flags for triple patterns.
 */
typedef enum {

  /* Not used - was only used internally in the execution engine */
  RASQAL_TRIPLE_FLAGS_EXACT=1,

  /* Not used - this is now a property of a graph pattern */
  RASQAL_TRIPLE_FLAGS_OPTIONAL=2,

  RASQAL_TRIPLE_FLAGS_LAST=RASQAL_TRIPLE_FLAGS_OPTIONAL
} rasqal_triple_flags;


/**
 * rasqal_triple:
 * @subject: Triple subject.
 * @predicate: Triple predicate.
 * @object: Triple object.
 * @origin: Triple origin.
 * @flags: Or of enum #rasqal_triple_flags bits.
 *
 * A triple pattern or RDF triple.
 *
 * This is used as a triple pattern in queries and
 * an RDF triple when generating RDF triples such as with SPARQL CONSTRUCT.
 */
typedef struct {
  rasqal_literal* subject;
  rasqal_literal* predicate;
  rasqal_literal* object;
  rasqal_literal* origin;
  unsigned int flags;
} rasqal_triple;


/**
 * rasqal_pattern_flags:
 * @RASQAL_PATTERN_FLAGS_OPTIONAL: True when the graph pattern is an optional match.
 * @RASQAL_PATTERN_FLAGS_LAST: Internal
 *
 * Flags for #rasqal_graph_pattern.
 */
typedef enum {
  RASQAL_PATTERN_FLAGS_OPTIONAL=1,

  RASQAL_PATTERN_FLAGS_LAST=RASQAL_PATTERN_FLAGS_OPTIONAL
} rasqal_pattern_flags;


typedef unsigned char* (*rasqal_generate_bnodeid_handler)(rasqal_query* query, void *user_data, unsigned char *user_bnodeid);


/**
 * rasqal_query_verb:
 * @RASQAL_QUERY_VERB_SELECT: RDQL/SPARQL query select verb. 
 * @RASQAL_QUERY_VERB_CONSTRUCT: SPARQL query construct verb.
 * @RASQAL_QUERY_VERB_DESCRIBE: SPARQL query describe verb.
 * @RASQAL_QUERY_VERB_ASK: SPARQL query ask verb.
 * @RASQAL_QUERY_VERB_DELETE: LAQRS query delete verb.
 * @RASQAL_QUERY_VERB_INSERT: LAQRS query insert verb.
 * @RASQAL_QUERY_VERB_UNKNOWN: Internal
 * @RASQAL_QUERY_VERB_LAST: Internal
 *
 * Query main operation verbs describing the major type of query
 * being performed.
 */
typedef enum {
  /* internal */
  RASQAL_QUERY_VERB_UNKNOWN   = 0,
  RASQAL_QUERY_VERB_SELECT    = 1,
  RASQAL_QUERY_VERB_CONSTRUCT = 2,
  RASQAL_QUERY_VERB_DESCRIBE  = 3,
  RASQAL_QUERY_VERB_ASK       = 4,
  RASQAL_QUERY_VERB_DELETE    = 5,
  RASQAL_QUERY_VERB_INSERT    = 6,

  /* internal */
  RASQAL_QUERY_VERB_LAST=RASQAL_QUERY_VERB_INSERT
} rasqal_query_verb;


/**
 * rasqal_graph_pattern_operator:
 * @RASQAL_GRAPH_PATTERN_OPERATOR_BASIC: Just triple patterns and constraints.
 * @RASQAL_GRAPH_PATTERN_OPERATOR_OPTIONAL: Set of graph patterns (ANDed) and constraints.
 * @RASQAL_GRAPH_PATTERN_OPERATOR_UNION: Set of graph patterns (UNIONed) and constraints.
 * @RASQAL_GRAPH_PATTERN_OPERATOR_GROUP: Set of graph patterns (ANDed) and constraints.
 * @RASQAL_GRAPH_PATTERN_OPERATOR_GRAPH: A graph term + a graph pattern and constraints.
 * @RASQAL_GRAPH_PATTERN_OPERATOR_UNKNOWN: Internal.
 * @RASQAL_GRAPH_PATTERN_OPERATOR_LAST: Internal.
 *
 * Graph pattern operators
 */
typedef enum {
  RASQAL_GRAPH_PATTERN_OPERATOR_UNKNOWN   = 0,
  RASQAL_GRAPH_PATTERN_OPERATOR_BASIC     = 1,
  RASQAL_GRAPH_PATTERN_OPERATOR_OPTIONAL  = 2,
  RASQAL_GRAPH_PATTERN_OPERATOR_UNION     = 3,
  RASQAL_GRAPH_PATTERN_OPERATOR_GROUP     = 4,
  RASQAL_GRAPH_PATTERN_OPERATOR_GRAPH     = 5,

  RASQAL_GRAPH_PATTERN_OPERATOR_LAST=RASQAL_GRAPH_PATTERN_OPERATOR_GRAPH
} rasqal_graph_pattern_operator;


/**
 * rasqal_graph_pattern_visit_fn:
 * @query: #rasqal_query containing the graph pattern
 * @gp: current graph_pattern
 * @user_data: user data passed in
 *
 * User function to visit an graph_pattern and operate on it with
 * rasqal_graph_pattern_visit() or rasqal_query_graph_pattern_visit()
 *
 * Return value: 0 to truncate the visit
 */
typedef int (*rasqal_graph_pattern_visit_fn)(rasqal_query* query, rasqal_graph_pattern* gp, void *user_data);


/* RASQAL API */

/* Public functions */

RASQAL_API
rasqal_world *rasqal_new_world(void);
RASQAL_API
void rasqal_free_world(rasqal_world* world);

/* Features */
RASQAL_API
int rasqal_features_enumerate(const rasqal_feature feature, const char **name, raptor_uri **uri, const char **label);
RASQAL_API
unsigned int rasqal_get_feature_count(void);
RASQAL_API
rasqal_feature rasqal_feature_from_uri(raptor_uri *uri);
RASQAL_API
int rasqal_feature_value_type(const rasqal_feature feature);

RASQAL_API
int rasqal_languages_enumerate(rasqal_world* world, unsigned int counter, const char **name, const char **label, const unsigned char **uri_string);

RASQAL_API
int rasqal_language_name_check(rasqal_world* world, const char *name);


/* Query class */

/* Create */
RASQAL_API
rasqal_query* rasqal_new_query(rasqal_world* world, const char *name, const unsigned char *uri);

/* Destroy */
RASQAL_API
void rasqal_free_query(rasqal_query* query);

/* Methods */
RASQAL_API
const char* rasqal_query_get_name(rasqal_query* query);
RASQAL_API
const char* rasqal_query_get_label(rasqal_query* query);
RASQAL_API
void rasqal_query_set_fatal_error_handler(rasqal_query* query, void *user_data, raptor_message_handler handler);
RASQAL_API
void rasqal_query_set_error_handler(rasqal_query* query, void *user_data, raptor_message_handler handler);
RASQAL_API
void rasqal_query_set_warning_handler(rasqal_query* query, void *user_data, raptor_message_handler handler);
RASQAL_API
int rasqal_query_set_feature(rasqal_query* query, rasqal_feature feature, int value);
RASQAL_API
int rasqal_query_set_feature_string(rasqal_query *query, rasqal_feature feature, const unsigned char *value);
RASQAL_API
int rasqal_query_get_feature(rasqal_query *query, rasqal_feature feature);
RASQAL_API
const unsigned char* rasqal_query_get_feature_string(rasqal_query *query, rasqal_feature feature);
RASQAL_API
void rasqal_query_set_default_generate_bnodeid_parameters(rasqal_query* rdf_query, char *prefix, int base);
RASQAL_API
void rasqal_query_set_generate_bnodeid_handler(rasqal_query* query, void *user_data, rasqal_generate_bnodeid_handler handler);

RASQAL_API
rasqal_query_verb rasqal_query_get_verb(rasqal_query* query);
RASQAL_API
int rasqal_query_get_wildcard(rasqal_query* query);
RASQAL_API
int rasqal_query_get_distinct(rasqal_query* query);
RASQAL_API
void rasqal_query_set_distinct(rasqal_query* query, int distinct_mode);
RASQAL_API
int rasqal_query_get_explain(rasqal_query* query);
RASQAL_API
void rasqal_query_set_explain(rasqal_query* query, int is_explain);
RASQAL_API
int rasqal_query_get_limit(rasqal_query* query);
RASQAL_API
void rasqal_query_set_limit(rasqal_query* query, int limit);
RASQAL_API
int rasqal_query_get_offset(rasqal_query* query);
RASQAL_API
void rasqal_query_set_offset(rasqal_query* query, int offset);

RASQAL_API
int rasqal_query_add_data_graph(rasqal_query* query, raptor_uri* uri, raptor_uri* name_uri, int flags);
RASQAL_API
raptor_sequence* rasqal_query_get_data_graph_sequence(rasqal_query* query);
RASQAL_API
rasqal_data_graph* rasqal_query_get_data_graph(rasqal_query* query, int idx);

RASQAL_API
int rasqal_query_add_variable(rasqal_query* query, rasqal_variable* var);
RASQAL_API
raptor_sequence* rasqal_query_get_bound_variable_sequence(rasqal_query* query);
RASQAL_API
raptor_sequence* rasqal_query_get_anonymous_variable_sequence(rasqal_query* query);
RASQAL_API
raptor_sequence* rasqal_query_get_all_variable_sequence(rasqal_query* query);
RASQAL_API
rasqal_variable* rasqal_query_get_variable(rasqal_query* query, int idx);
RASQAL_API
int rasqal_query_has_variable(rasqal_query* query, const unsigned char *name);
RASQAL_API
int rasqal_query_set_variable(rasqal_query* query, const unsigned char *name, rasqal_literal* value);
RASQAL_API
raptor_sequence* rasqal_query_get_triple_sequence(rasqal_query* query);
RASQAL_API
rasqal_triple* rasqal_query_get_triple(rasqal_query* query, int idx);
RASQAL_API
int rasqal_query_add_prefix(rasqal_query* query, rasqal_prefix* prefix);
RASQAL_API
raptor_sequence* rasqal_query_get_prefix_sequence(rasqal_query* query);
RASQAL_API
rasqal_prefix* rasqal_query_get_prefix(rasqal_query* query, int idx);
RASQAL_API
raptor_sequence* rasqal_query_get_order_conditions_sequence(rasqal_query* query);
RASQAL_API
rasqal_expression* rasqal_query_get_order_condition(rasqal_query* query, int idx);
RASQAL_API
raptor_sequence* rasqal_query_get_group_conditions_sequence(rasqal_query* query);
RASQAL_API
rasqal_expression* rasqal_query_get_group_condition(rasqal_query* query, int idx);
RASQAL_API
raptor_sequence* rasqal_query_get_construct_triples_sequence(rasqal_query* query);
RASQAL_API
rasqal_triple* rasqal_query_get_construct_triple(rasqal_query* query, int idx);
RASQAL_API
void rasqal_query_graph_pattern_visit(rasqal_query* query, rasqal_graph_pattern_visit_fn visit_fn, void* data);
RASQAL_API
int rasqal_query_write(raptor_iostream* iostr, rasqal_query* query, raptor_uri* format_uri, raptor_uri* base_uri);

/* graph patterns */
RASQAL_API
rasqal_graph_pattern* rasqal_query_get_query_graph_pattern(rasqal_query* query);
RASQAL_API
raptor_sequence* rasqal_query_get_graph_pattern_sequence(rasqal_query* query);
RASQAL_API
rasqal_graph_pattern* rasqal_query_get_graph_pattern(rasqal_query* query, int idx);
RASQAL_API
int rasqal_graph_pattern_add_sub_graph_pattern(rasqal_graph_pattern* graph_pattern, rasqal_graph_pattern* sub_graph_pattern);
RASQAL_API
rasqal_triple* rasqal_graph_pattern_get_triple(rasqal_graph_pattern* graph_pattern, int idx);
RASQAL_API
raptor_sequence* rasqal_graph_pattern_get_sub_graph_pattern_sequence(rasqal_graph_pattern* graph_pattern);
RASQAL_API
rasqal_graph_pattern* rasqal_graph_pattern_get_sub_graph_pattern(rasqal_graph_pattern* graph_pattern, int idx);
RASQAL_API
rasqal_graph_pattern_operator rasqal_graph_pattern_get_operator(rasqal_graph_pattern* graph_pattern);
RASQAL_API
const char* rasqal_graph_pattern_operator_as_string(rasqal_graph_pattern_operator op);
RASQAL_API
void rasqal_graph_pattern_print(rasqal_graph_pattern* gp, FILE* fh);
RASQAL_API
int rasqal_graph_pattern_add_constraint(rasqal_graph_pattern* gp, rasqal_expression* expr);
RASQAL_API
raptor_sequence* rasqal_graph_pattern_get_constraint_sequence(rasqal_graph_pattern* gp);
RASQAL_API
rasqal_expression* rasqal_graph_pattern_get_constraint(rasqal_graph_pattern* gp, int idx);
RASQAL_API
int rasqal_graph_pattern_visit(rasqal_query* query, rasqal_graph_pattern *gp, rasqal_graph_pattern_visit_fn fn, void* user_data);
RASQAL_API
int rasqal_graph_pattern_get_index(rasqal_graph_pattern* gp);

/* Utility methods */
RASQAL_API
const char* rasqal_query_verb_as_string(rasqal_query_verb verb);
RASQAL_API
void rasqal_query_print(rasqal_query* query, FILE* fh);

/* Query */
RASQAL_API
int rasqal_query_prepare(rasqal_query* query, const unsigned char *query_string, raptor_uri *base_uri);
RASQAL_API
rasqal_query_results* rasqal_query_execute(rasqal_query* query);

RASQAL_API
void* rasqal_query_get_user_data(rasqal_query* query);
RASQAL_API
void rasqal_query_set_user_data(rasqal_query* query, void *user_data);

/* query results */
RASQAL_API
void rasqal_free_query_results(rasqal_query_results *query_results);

/* Bindings result format */
RASQAL_API
int rasqal_query_results_is_bindings(rasqal_query_results *query_results);
RASQAL_API
int rasqal_query_results_get_count(rasqal_query_results *query_results);
RASQAL_API
int rasqal_query_results_next(rasqal_query_results *query_results);
RASQAL_API
int rasqal_query_results_finished(rasqal_query_results *query_results);
RASQAL_API
int rasqal_query_results_get_bindings(rasqal_query_results *query_results, const unsigned char ***names, rasqal_literal ***values);
RASQAL_API
rasqal_literal* rasqal_query_results_get_binding_value(rasqal_query_results *query_results, int offset);
RASQAL_API
const unsigned char* rasqal_query_results_get_binding_name(rasqal_query_results *query_results, int offset);
RASQAL_API
rasqal_literal* rasqal_query_results_get_binding_value_by_name(rasqal_query_results *query_results, const unsigned char *name);
RASQAL_API
int rasqal_query_results_get_bindings_count(rasqal_query_results *query_results);

/* Boolean result format */
RASQAL_API
int rasqal_query_results_is_boolean(rasqal_query_results *query_results);
RASQAL_API
int rasqal_query_results_get_boolean(rasqal_query_results *query_results);

/* Graph result format */
RASQAL_API
int rasqal_query_results_is_graph(rasqal_query_results *query_results);
RASQAL_API
raptor_statement* rasqal_query_results_get_triple(rasqal_query_results *query_results);
RASQAL_API
int rasqal_query_results_next_triple(rasqal_query_results *query_results);

/* Syntax result format */
RASQAL_API
int rasqal_query_results_is_syntax(rasqal_query_results* query_results);

RASQAL_API
int rasqal_query_results_write(raptor_iostream *iostr, rasqal_query_results *results, raptor_uri *format_uri, raptor_uri *base_uri);
RASQAL_API
int rasqal_query_results_read(raptor_iostream *iostr, rasqal_query_results *results, raptor_uri *format_uri, raptor_uri *base_uri);


/**
 * RASQAL_QUERY_RESULTS_FORMAT_FLAG_READER:
 *
 * Flag for rasqal_query_results_formats_enumerate() to get query results formats that can be read.
 */
#define RASQAL_QUERY_RESULTS_FORMAT_FLAG_READER 1

/**
 * RASQAL_QUERY_RESULTS_FORMAT_FLAG_WRITER:
 *
 * Flag for rasqal_query_results_formats_enumerate() to get query results formats that can be written.
 */
#define RASQAL_QUERY_RESULTS_FORMAT_FLAG_WRITER 2

RASQAL_API
int rasqal_query_results_formats_enumerate(rasqal_world* world, unsigned int counter, const char **name, const char **label, const unsigned char **uri_string, const char **mime_type, int* flags);
RASQAL_API
int rasqal_query_results_formats_check(rasqal_world* world, const char *name, raptor_uri* uri, const char *mime_type);
RASQAL_API
rasqal_query_results_formatter* rasqal_new_query_results_formatter(rasqal_world* world, const char *name, raptor_uri* format_uri);
RASQAL_API
rasqal_query_results_formatter* rasqal_new_query_results_formatter_by_mime_type(rasqal_world* world, const char *mime_type);
RASQAL_API
void rasqal_free_query_results_formatter(rasqal_query_results_formatter* formatter);
RASQAL_API
int rasqal_query_results_formatter_write(raptor_iostream *iostr, rasqal_query_results_formatter* formatter, rasqal_query_results* results, raptor_uri *base_uri);
RASQAL_API
int rasqal_query_results_formatter_read(rasqal_world* world, raptor_iostream *iostr, rasqal_query_results_formatter* formatter, rasqal_query_results* results, raptor_uri *base_uri);
RASQAL_API
const char* rasqal_query_results_formatter_get_mime_type(rasqal_query_results_formatter *formatter);

RASQAL_API
int rasqal_query_iostream_write_escaped_counted_string(rasqal_query* query, raptor_iostream* iostr, const unsigned char* string, size_t len);
RASQAL_API
unsigned char* rasqal_query_escape_counted_string(rasqal_query* query, const unsigned char *string, size_t len, size_t* output_len_p);


/* Data graph class */
RASQAL_API
rasqal_data_graph* rasqal_new_data_graph(raptor_uri* uri, raptor_uri* name_uri, int flags);
RASQAL_API
void rasqal_free_data_graph(rasqal_data_graph* dg);
RASQAL_API
void rasqal_data_graph_print(rasqal_data_graph* dg, FILE* fh);


/**
 * rasqal_compare_flags:
 * @RASQAL_COMPARE_NOCASE: String comparisons are case independent.
 * @RASQAL_COMPARE_XQUERY: XQuery comparsion rules apply.
 * @RASQAL_COMPARE_RDF:    RDF Term comparsion rules apply.
 * @RASQAL_COMPARE_URI:    Allow comparison of URIs
 *
 * Flags for rasqal_expression_evaluate() or rasqal_literal_compare().
 */
typedef enum {
  RASQAL_COMPARE_NOCASE = 1,
  RASQAL_COMPARE_XQUERY = 2,
  RASQAL_COMPARE_RDF    = 4,
  RASQAL_COMPARE_URI    = 8
} rasqal_compare_flags;


/* Expression class */
RASQAL_API
rasqal_expression* rasqal_new_0op_expression(rasqal_op op);
RASQAL_API
rasqal_expression* rasqal_new_1op_expression(rasqal_op op, rasqal_expression* arg);
RASQAL_API
rasqal_expression* rasqal_new_2op_expression(rasqal_op op, rasqal_expression* arg1, rasqal_expression* arg2);
RASQAL_API
rasqal_expression* rasqal_new_3op_expression(rasqal_op op, rasqal_expression* arg1,  rasqal_expression* arg2, rasqal_expression* arg3);
RASQAL_API
rasqal_expression* rasqal_new_string_op_expression(rasqal_op op, rasqal_expression* arg1, rasqal_literal* literal);
RASQAL_API
rasqal_expression* rasqal_new_literal_expression(rasqal_literal* literal);
RASQAL_API
rasqal_expression* rasqal_new_function_expression(raptor_uri* name, raptor_sequence* args);
RASQAL_API
rasqal_expression* rasqal_new_cast_expression(raptor_uri* name, rasqal_expression *value);
RASQAL_API
rasqal_expression* rasqal_new_expression_from_expression(rasqal_expression* e);

RASQAL_API
void rasqal_free_expression(rasqal_expression* e);
RASQAL_API
void rasqal_expression_print_op(rasqal_expression* e, FILE* fh);
RASQAL_API
void rasqal_expression_print(rasqal_expression* e, FILE* fh);
RASQAL_API
rasqal_literal* rasqal_expression_evaluate(rasqal_query* query, rasqal_expression* e, int flags);

/**
 * rasqal_expression_visit_fn:
 * @user_data: user data passed in with rasqal_expression_visit()
 * @e: current expression
 *
 * User function to visit an expression and operate on it with
 * rasqal_expression_visit()
 *
 * Return value: 0 to truncate the visit
 */
typedef int (*rasqal_expression_visit_fn)(void *user_data, rasqal_expression *e);
RASQAL_API
int rasqal_expression_visit(rasqal_expression* e, rasqal_expression_visit_fn fn, void *user_data);


/* Literal class */
RASQAL_API
rasqal_literal* rasqal_new_integer_literal(rasqal_world* world, rasqal_literal_type type, int integer);
RASQAL_API
rasqal_literal* rasqal_new_typed_literal(rasqal_world* world, rasqal_literal_type type, const unsigned char* string);
RASQAL_API
rasqal_literal* rasqal_new_double_literal(rasqal_world* world, double d);
RASQAL_API
rasqal_literal* rasqal_new_float_literal(rasqal_world* world, float f);
RASQAL_API
rasqal_literal* rasqal_new_uri_literal(rasqal_world* world, raptor_uri* uri);
RASQAL_API
rasqal_literal* rasqal_new_pattern_literal(rasqal_world* world, const unsigned char *pattern, const char *flags);
RASQAL_API
rasqal_literal* rasqal_new_string_literal(rasqal_world* world, const unsigned char *string, const char *language, raptor_uri *datatype, const unsigned char *datatype_qname);
RASQAL_API
rasqal_literal* rasqal_new_simple_literal(rasqal_world* world, rasqal_literal_type type, const unsigned char *string);
RASQAL_API
rasqal_literal* rasqal_new_boolean_literal(rasqal_world* world, int value);
RASQAL_API
rasqal_literal* rasqal_new_variable_literal(rasqal_world* world, rasqal_variable *variable);
RASQAL_API
rasqal_literal* rasqal_new_decimal_literal(rasqal_world* world, const unsigned char *string);
RASQAL_API
rasqal_literal* rasqal_new_decimal_literal_from_decimal(rasqal_world* world, const unsigned char *string, rasqal_xsd_decimal* decimal);

RASQAL_API
rasqal_literal* rasqal_new_literal_from_literal(rasqal_literal* l);
RASQAL_API
void rasqal_free_literal(rasqal_literal* l);
RASQAL_API
void rasqal_literal_print(rasqal_literal* l, FILE* fh);
RASQAL_API
void rasqal_literal_print_type(rasqal_literal* l, FILE* fh);
RASQAL_API
rasqal_variable* rasqal_literal_as_variable(rasqal_literal* l);
RASQAL_API
const unsigned char* rasqal_literal_as_string(rasqal_literal* l);
RASQAL_API
const unsigned char* rasqal_literal_as_string_flags(rasqal_literal* l, int flags, int *error);
RASQAL_API
rasqal_literal* rasqal_literal_as_node(rasqal_literal* l);
RASQAL_API
raptor_uri* rasqal_literal_datatype(rasqal_literal* l);
RASQAL_API
rasqal_literal* rasqal_literal_value(rasqal_literal* l);

RASQAL_API
int rasqal_literal_compare(rasqal_literal* l1, rasqal_literal* l2, int flags, int *error);
RASQAL_API
int rasqal_literal_equals(rasqal_literal* l1, rasqal_literal* l2);

RASQAL_API
rasqal_prefix* rasqal_new_prefix(const unsigned char* prefix, raptor_uri* uri);
RASQAL_API
void rasqal_free_prefix(rasqal_prefix* p);
RASQAL_API
void rasqal_prefix_print(rasqal_prefix* p, FILE* fh);

/* Triple class */
RASQAL_API
rasqal_triple* rasqal_new_triple(rasqal_literal* subject, rasqal_literal* predicate, rasqal_literal* object);
RASQAL_API
rasqal_triple* rasqal_new_triple_from_triple(rasqal_triple* t);
RASQAL_API
void rasqal_free_triple(rasqal_triple* t);
RASQAL_API
void rasqal_triple_print(rasqal_triple* t, FILE* fh);
RASQAL_API
void rasqal_triple_set_origin(rasqal_triple* t, rasqal_literal *l);
RASQAL_API
rasqal_literal* rasqal_triple_get_origin(rasqal_triple* t);

/* Variable class */
RASQAL_API
rasqal_variable* rasqal_new_variable_typed(rasqal_query* rq, rasqal_variable_type type, unsigned char *name, rasqal_literal *value);
RASQAL_API
rasqal_variable* rasqal_new_variable(rasqal_query* rq, unsigned char *name, rasqal_literal *value);
RASQAL_API
rasqal_variable* rasqal_new_variable_from_variable(rasqal_variable* v);
RASQAL_API
void rasqal_free_variable(rasqal_variable* v);
RASQAL_API
void rasqal_variable_print(rasqal_variable* v, FILE* fh);
RASQAL_API
void rasqal_variable_set_value(rasqal_variable* v, rasqal_literal* l);

/* memory functions */
RASQAL_API
void rasqal_free_memory(void *ptr);
RASQAL_API
void* rasqal_alloc_memory(size_t size);
RASQAL_API
void* rasqal_calloc_memory(size_t nmemb, size_t size);


/* decimal functions */
RASQAL_API
rasqal_xsd_decimal* rasqal_new_xsd_decimal(void);
RASQAL_API
void rasqal_free_xsd_decimal(rasqal_xsd_decimal* dec);
RASQAL_API
int rasqal_xsd_decimal_set_string(rasqal_xsd_decimal* dec, const char* string);
RASQAL_API
double rasqal_xsd_decimal_get_double(rasqal_xsd_decimal* dec);
RASQAL_API
char* rasqal_xsd_decimal_as_string(rasqal_xsd_decimal* dec);
RASQAL_API
char* rasqal_xsd_decimal_as_counted_string(rasqal_xsd_decimal* dec, size_t* len_p);
RASQAL_API
int rasqal_xsd_decimal_set_long(rasqal_xsd_decimal* dec, long l);
RASQAL_API
int rasqal_xsd_decimal_set_double(rasqal_xsd_decimal* dec, double d);
RASQAL_API
int rasqal_xsd_decimal_print(rasqal_xsd_decimal* dec, FILE* stream);
RASQAL_API
int rasqal_xsd_decimal_add(rasqal_xsd_decimal* result, rasqal_xsd_decimal* a, rasqal_xsd_decimal* b);
RASQAL_API
int rasqal_xsd_decimal_subtract(rasqal_xsd_decimal* result, rasqal_xsd_decimal* a, rasqal_xsd_decimal* b);
RASQAL_API
int rasqal_xsd_decimal_multiply(rasqal_xsd_decimal* result, rasqal_xsd_decimal* a, rasqal_xsd_decimal* b);
RASQAL_API
int rasqal_xsd_decimal_divide(rasqal_xsd_decimal* result, rasqal_xsd_decimal* a, rasqal_xsd_decimal* b);
RASQAL_API
int rasqal_xsd_decimal_negate(rasqal_xsd_decimal* result, rasqal_xsd_decimal* a);
RASQAL_API
int rasqal_xsd_decimal_compare(rasqal_xsd_decimal* a, rasqal_xsd_decimal* b);
RASQAL_API
int rasqal_xsd_decimal_equals(rasqal_xsd_decimal* a, rasqal_xsd_decimal* b);
RASQAL_API
int rasqal_xsd_decimal_is_zero(rasqal_xsd_decimal* d);

/* rasqal_engine.c */

/**
 * rasqal_triple_parts:
 * @RASQAL_TRIPLE_SUBJECT: Subject present in a triple.
 * @RASQAL_TRIPLE_PREDICATE: Predicate present in a triple.
 * @RASQAL_TRIPLE_OBJECT: Object present in a triple.
 * @RASQAL_TRIPLE_ORIGIN: Origin/graph present in a triple.
 * @RASQAL_TRIPLE_GRAPH:  Alias for RASQAL_TRIPLE_ORIGIN
 * @RASQAL_TRIPLE_SPO: Subject, Predicate and Object present in a triple.
 * @RASQAL_TRIPLE_SPOG: Subject, Predicate, Object, Graph present in a triple.
 *
 * Flags for parts of a triple.
 */
typedef enum {
  RASQAL_TRIPLE_SUBJECT  = 1,
  RASQAL_TRIPLE_PREDICATE= 2,
  RASQAL_TRIPLE_OBJECT   = 4,
  RASQAL_TRIPLE_ORIGIN   = 8,
  RASQAL_TRIPLE_GRAPH    = RASQAL_TRIPLE_ORIGIN,
  RASQAL_TRIPLE_SPO      = RASQAL_TRIPLE_SUBJECT | RASQAL_TRIPLE_PREDICATE | RASQAL_TRIPLE_OBJECT,
  RASQAL_TRIPLE_SPOG     = RASQAL_TRIPLE_SPO | RASQAL_TRIPLE_GRAPH
} rasqal_triple_parts;



/**
 * rasqal_triples_match:
 * @user_data: User data pointer for factory methods.
 * @bind_match: The [4]array (s,p,o,origin) bindings against the current triple match only touching triple parts given. Returns parts that were bound or 0 on failure.
 * @next_match: Move to next match.
 * @is_end: Check for end of triple match - return non-0 if is end.
 * @finish: Finish triples match and destroy any allocated memory.
 * @world: rasqal_world object
 * 
 * Triples match structure as initialised by #rasqal_triples_source
 * method init_triples_match.
 */
struct rasqal_triples_match_s {
  void *user_data;

  rasqal_triple_parts (*bind_match)(struct rasqal_triples_match_s* rtm, void *user_data, rasqal_variable *bindings[4], rasqal_triple_parts parts);

  void (*next_match)(struct rasqal_triples_match_s* rtm, void *user_data);

  int (*is_end)(struct rasqal_triples_match_s* rtm, void *user_data);

  void (*finish)(struct rasqal_triples_match_s* rtm, void *user_data);

  rasqal_world *world;
};
typedef struct rasqal_triples_match_s rasqal_triples_match;

/**
 * rasqal_triple_meta:
 * @bindings: Variable bindings for this triple+origin to set.
 * @triples_match: The matcher that is setting these bindings.
 * @context: Context data used by the matcher.
 * @parts: Parts of the triple to match/bindings to set.
 * @is_exact: non-0 if all parts of the triple are given
 * @executed: non-0 if the triple pattern has been fully executed
 *
 * Triple matching metadata for one triple pattern.
 */
typedef struct {
  /* triple (subject, predicate, object) and origin */
  rasqal_variable* bindings[4];

  rasqal_triples_match *triples_match;

  void *context;

  rasqal_triple_parts parts;

  /* non-0 if the associated triple pattern contains no variables */
  int is_exact;

  /* non-0 if the triple pattern has been fully executed */
  int executed;
} rasqal_triple_meta;


/**
 * rasqal_triples_source:
 * @query: Source for this query.
 * @user_data: Context user data passed into the factory methods.
 * @init_triples_match: Factory method to initalise a new #rasqal_triples_match.
 * @triple_present: Factory method to return presence or absence of a complete triple.
 * @free_triples_source: Factory method to deallocate resources.
 *
 * Triples source as initialised by a #rasqal_triples_source_factory.
 */
struct rasqal_triples_source_s {
  rasqal_query* query;

  void *user_data;

  int (*init_triples_match)(rasqal_triples_match* rtm, struct rasqal_triples_source_s* rts, void *user_data, rasqal_triple_meta *m, rasqal_triple *t);

  int (*triple_present)(struct rasqal_triples_source_s* rts, void *user_data, rasqal_triple *t);

  void (*free_triples_source)(void *user_data);
};
typedef struct rasqal_triples_source_s rasqal_triples_source;


/**
 * rasqal_triples_source_factory:
 * @user_data: User data for triples_source_factory.
 * @user_data_size: Size Of @user_data for new_triples_source.
 * @new_triples_source: Create a new triples source - returns non-zero on failure &lt; 0 is a 'no rdf data error', &gt; 0 is an unspecified error..
 *
 * A factory that initialises #rasqal_triples_source structures
 * to returning matches to a triple pattern.
 */
typedef struct {
  void *user_data;
  size_t user_data_size;

  int (*new_triples_source)(rasqal_query* query, void *factory_user_data, void *user_data, rasqal_triples_source* rts);
} rasqal_triples_source_factory;
  

/* set the triples_source_factory */
RASQAL_API
void rasqal_set_triples_source_factory(rasqal_world* world, void (*register_fn)(rasqal_triples_source_factory *factory), void* user_data);



/* The info below is solely for gtk-doc - ignore it */

/**
 * RASQAL_QUERY_RESULTS_FORMATTER_DECLARED:
 *
 * Internal
 */

/**
 * RASQAL_WORLD_DECLARED:
 *
 * Internal
 */

/**
 * rasqal_expression_s:
 * @usage: Internal
 * @op: Internal
 * @arg1: Internal
 * @arg2: Internal
 * @arg3: Internal
 * @literal: Internal
 * @value: Internal
 * @name: Internal
 * @args: Internal
 *
 * Internal - see #rasqal_expression.
 *
 */

/**
 * bind_match:
 * @rtm: triples match context
 * @user_data: user data
 * @bindings: variable binding for parts of triple (s, p, o, g)
 * @parts: parts of triple to match
 *
 * Internal - see #rasqal_triples_match
 *
 * Return value: match parts
*/

/**
 * next_match:
 * @rtm: triples match context
 * @user_data: user data
 * 
 * Internal - see #rasqal_triples_match
 */

/**
 * is_end:
 * @rtm: triples match context
 * @user_data: user data
 *
 * Internal - see #rasqal_triples_match
 *
 * Return value: non-0 if end of match
 */

/**
 * finish:
 * @rtm: triples match context
 * @user_data: user data
 *
 * Internal - see #rasqal_triples_match
 */

/**
 * init_triples_match:
 * @rtm: triples match context
 * @rts: triples match source
 * @user_data: user data
 * @m: triple meta
 * @t: triple
 *
 * Internal - see #rasqal_triples_source
 *
 * Return value: non-0 on failure
 */

/**
 * triple_present:
 * @rts: triples match source
 * @user_data: user data
 * @t: triple to test for presence
 *
 * Internal - see #rasqal_triples_source
 *
 * Return value: non-0 on failure
 */

/**
 * free_triples_source:
 * @user_data: user data
 *
 * Internal - see #rasqal_triples_source
 */


#ifdef __cplusplus
}
#endif

#endif
