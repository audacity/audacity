/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_internal.h - Rasqal RDF Query library internals
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



#ifndef RASQAL_INTERNAL_H
#define RASQAL_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef RASQAL_INTERNAL

/* for the memory allocation functions */
#if defined(HAVE_DMALLOC_H) && defined(RASQAL_MEMORY_DEBUG_DMALLOC)
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#undef HAVE_STDLIB_H
#endif
#include <dmalloc.h>
#endif

#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 4)
#define RASQAL_PRINTF_FORMAT(string_index, first_to_check_index) \
  __attribute__((__format__(__printf__, string_index, first_to_check_index)))
#else
#define RASQAL_PRINTF_FORMAT(string_index, first_to_check_index)
#endif

/* Can be over-ridden or undefined in a config.h file or -Ddefine */
#ifndef RASQAL_INLINE
#define RASQAL_INLINE inline
#endif

#ifdef LIBRDF_DEBUG
#define RASQAL_DEBUG 1
#endif

#if defined(RASQAL_MEMORY_SIGN)
#define RASQAL_SIGN_KEY 0x08A59A10
void* rasqal_sign_malloc(size_t size);
void* rasqal_sign_calloc(size_t nmemb, size_t size);
void* rasqal_sign_realloc(void *ptr, size_t size);
void rasqal_sign_free(void *ptr);
  
#define RASQAL_MALLOC(type, size)   rasqal_sign_malloc(size)
#define RASQAL_CALLOC(type, nmemb, size) rasqal_sign_calloc(nmemb, size)
#define RASQAL_REALLOC(type, ptr, size) rasqal_sign_realloc(ptr, size)
#define RASQAL_FREE(type, ptr)   rasqal_sign_free(ptr)

#else
#define RASQAL_MALLOC(type, size) malloc(size)
#define RASQAL_CALLOC(type, size, count) calloc(size, count)
#define RASQAL_FREE(type, ptr)   free((void*)ptr)

#endif

#ifdef RASQAL_DEBUG
/* Debugging messages */
#define RASQAL_DEBUG1(msg) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__); } while(0)
#define RASQAL_DEBUG2(msg, arg1) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1);} while(0)
#define RASQAL_DEBUG3(msg, arg1, arg2) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2);} while(0)
#define RASQAL_DEBUG4(msg, arg1, arg2, arg3) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2, arg3);} while(0)
#define RASQAL_DEBUG5(msg, arg1, arg2, arg3, arg4) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2, arg3, arg4);} while(0)
#define RASQAL_DEBUG6(msg, arg1, arg2, arg3, arg4, arg5) do {fprintf(stderr, "%s:%d:%s: " msg, __FILE__, __LINE__, __func__, arg1, arg2, arg3, arg4, arg5);} while(0)

#if defined(HAVE_DMALLOC_H) && defined(RASQAL_MEMORY_DEBUG_DMALLOC)
void* rasqal_system_malloc(size_t size);
void rasqal_system_free(void *ptr);
#define SYSTEM_MALLOC(size)   rasqal_system_malloc(size)
#define SYSTEM_FREE(ptr)   rasqal_system_free(ptr)
#else
#define SYSTEM_MALLOC(size)   malloc(size)
#define SYSTEM_FREE(ptr)   free(ptr)
#endif

#ifndef RASQAL_ASSERT_DIE
#define RASQAL_ASSERT_DIE abort();
#endif

#else
/* DEBUGGING TURNED OFF */

/* No debugging messages */
#define RASQAL_DEBUG1(msg)
#define RASQAL_DEBUG2(msg, arg1)
#define RASQAL_DEBUG3(msg, arg1, arg2)
#define RASQAL_DEBUG4(msg, arg1, arg2, arg3)
#define RASQAL_DEBUG5(msg, arg1, arg2, arg3, arg4)
#define RASQAL_DEBUG6(msg, arg1, arg2, arg3, arg4, arg5)

#define SYSTEM_MALLOC(size)   malloc(size)
#define SYSTEM_FREE(ptr)   free(ptr)

#ifndef RASQAL_ASSERT_DIE
#define RASQAL_ASSERT_DIE
#endif

#endif


#ifdef RASQAL_DISABLE_ASSERT_MESSAGES
#define RASQAL_ASSERT_REPORT(line)
#else
#define RASQAL_ASSERT_REPORT(msg) fprintf(stderr, "%s:%d: (%s) assertion failed: " msg "\n", __FILE__, __LINE__, __func__);
#endif


#ifdef RASQAL_DISABLE_ASSERT

#define RASQAL_ASSERT(condition, msg) 
#define RASQAL_ASSERT_RETURN(condition, msg, ret) 
#define RASQAL_ASSERT_OBJECT_POINTER_RETURN(pointer, type) do { \
  if(!pointer) \
    return; \
} while(0)
#define RASQAL_ASSERT_OBJECT_POINTER_RETURN_VALUE(pointer, type, ret)

#else

#define RASQAL_ASSERT(condition, msg) do { \
  if(condition) { \
    RASQAL_ASSERT_REPORT(msg) \
    RASQAL_ASSERT_DIE \
  } \
} while(0)

#define RASQAL_ASSERT_RETURN(condition, msg, ret) do { \
  if(condition) { \
    RASQAL_ASSERT_REPORT(msg) \
    RASQAL_ASSERT_DIE \
    return ret; \
  } \
} while(0)

#define RASQAL_ASSERT_OBJECT_POINTER_RETURN(pointer, type) do { \
  if(!pointer) { \
    RASQAL_ASSERT_REPORT("object pointer of type " #type " is NULL.") \
    RASQAL_ASSERT_DIE \
    return; \
  } \
} while(0)

#define RASQAL_ASSERT_OBJECT_POINTER_RETURN_VALUE(pointer, type, ret) do { \
  if(!pointer) { \
    RASQAL_ASSERT_REPORT("object pointer of type " #type " is NULL.") \
    RASQAL_ASSERT_DIE \
    return ret; \
  } \
} while(0)

#endif


/* Fatal errors - always happen */
#define RASQAL_FATAL1(msg) do {fprintf(stderr, "%s:%d:%s: fatal error: " msg, __FILE__, __LINE__ , __func__); abort();} while(0)
#define RASQAL_FATAL2(msg,arg) do {fprintf(stderr, "%s:%d:%s: fatal error: " msg, __FILE__, __LINE__ , __func__, arg); abort();} while(0)
#define RASQAL_FATAL3(msg,arg1,arg2) do {fprintf(stderr, "%s:%d:%s: fatal error: " msg, __FILE__, __LINE__ , __func__, arg1, arg2); abort();} while(0)

#ifndef NO_STATIC_DATA
#define RASQAL_DEPRECATED_MESSAGE(msg) do {static int warning_given=0; if(!warning_given++) fprintf(stderr, "Function %s is deprecated - " msg,  __func__); } while(0)
#define RASQAL_DEPRECATED_WARNING(rq, msg) do {static int warning_given=0; if(!warning_given++) rasqal_query_warning(rq, msg); } while(0)
#else
#define RASQAL_DEPRECATED_MESSAGE(msg) do { fprintf(stderr, "Function %s is deprecated - " msg,  __func__); } while(0)
#define RASQAL_DEPRECATED_WARNING(rq, msg) do { rasqal_query_warning(rq, msg); } while(0)
#endif


typedef struct rasqal_query_engine_factory_s rasqal_query_engine_factory;


/*
 * Pattern graph for executing
 */
struct rasqal_graph_pattern_s {
  rasqal_query* query;

  /* operator for this graph pattern's contents */
  rasqal_graph_pattern_operator op;
  
  raptor_sequence* triples;          /* ... rasqal_triple*         */
  raptor_sequence* graph_patterns;   /* ... rasqal_graph_pattern*  */

  int start_column;
  int end_column;

  /* Max optional graph pattern allowed so far to stop backtracking
   * going over old graph patterns
   */
  int max_optional_graph_pattern;

  raptor_sequence *constraints; /* ... rasqal_expression*          */
  /* the expression version of the sequence of constraints above - this is
   * where the constraints are freed
   */
  rasqal_expression* constraints_expression;

  /* index of the graph pattern in the query (0.. query->graph_pattern_count-1) */
  int gp_index;
};

rasqal_graph_pattern* rasqal_new_basic_graph_pattern(rasqal_query* query, raptor_sequence* triples, int start_column, int end_column);
rasqal_graph_pattern* rasqal_new_graph_pattern_from_sequence(rasqal_query* query, raptor_sequence* graph_patterns, rasqal_graph_pattern_operator op);
void rasqal_free_graph_pattern(rasqal_graph_pattern* gp);
void rasqal_graph_pattern_adjust(rasqal_graph_pattern* gp, int offset);
void rasqal_graph_pattern_set_origin(rasqal_graph_pattern* graph_pattern, rasqal_literal* origin);
int rasqal_reset_triple_meta(rasqal_triple_meta* m);


/*
 * A query in some query language
 */
struct rasqal_query_s {
  rasqal_world* world; /* world object */

  int usage; /* reference count - 1 for itself, plus for query_results */
  
  unsigned char* query_string;
  int query_string_length; /* length including NULs */

  raptor_namespace_stack* namespaces;

  /* query graph pattern, containing the sequence of graph_patterns below */
  rasqal_graph_pattern* query_graph_pattern;
  
  /* the query verb - in SPARQL terms: SELECT, CONSTRUCT, DESCRIBE or ASK */
  rasqal_query_verb verb;
  
  /* sequences of ... */
  raptor_sequence* selects;     /* ... rasqal_variable* names only */
  raptor_sequence* data_graphs; /* ... rasqal_data_graph*          */
  raptor_sequence* triples;     /* ... rasqal_triple*              */
  raptor_sequence* prefixes;    /* ... rasqal_prefix*              */
  raptor_sequence* constructs;  /* ... rasqal_triple*       SPARQL */
  raptor_sequence* optional_triples; /* ... rasqal_triple*  SPARQL */
  raptor_sequence* describes;   /* ... rasqal_literal* (var or URIs) SPARQL */

  /* DISTINCT mode:
   * 0 if not given
   * 1 if DISTINCT: ensure solutions are unique
   * 2 if SPARQL REDUCED: permit elimination of some non-unique solutions 
   * otherwise undefined
   */
  int distinct;

  /* result limit LIMIT (>=0) or <0 if not given */
  int limit;

  /* result offset OFFSET (>=0) or <0 if not given */
  int offset;

  /* non-0 if '*' was seen after a verb (the appropriate list such as selects or constructs will be NULL) */
  int wildcard;

  int prepared;
  
  /* variable name/value table built from all distinct variables seen
   * in selects, triples, constraints and anonymous (no name, cannot
   * be selected or refered to).  An array of size variables_count
   *
   * The first select_variables_count of this array are from the
   * selects.  The selected variables only are typically returned to
   * the user.
   *
   * Anonymous variables appear at the end of the 'variables' array but
   * are taken from the anon_variables_sequence.
   */
  rasqal_variable** variables;
  int variables_count;
  int select_variables_count;

  /* array of size variables_count
   * pointing to triple column where variable[i] is declared
   */
  int* variables_declared_in;

  /* holds one copy of all the variables - this is where they are freed */
  raptor_sequence* variables_sequence;

  /* holds one copy of all anonymous variables - this is where they are freed */
  raptor_sequence* anon_variables_sequence;

  int anon_variables_count;

  /* array of variable names to bind or NULL if no variables wanted
   * (size (select_variables_count OR construct_variables_count)+1, last NULL)
   * indexes into the names in variables_sequence above.
   */
  const unsigned char** variable_names;
  /* can be filled with error location information */
  raptor_locator locator;

  /* base URI of this query for resolving relative URIs in queries */
  raptor_uri* base_uri;

  /* non 0 if query had fatal error in parsing and cannot be executed */
  int failed;

  /* stuff for our user */
  void* user_data;

  int default_generate_bnodeid_handler_base;
  char *default_generate_bnodeid_handler_prefix;
  size_t default_generate_bnodeid_handler_prefix_length;

  void *generate_bnodeid_handler_user_data;
  rasqal_generate_bnodeid_handler generate_bnodeid_handler;


  /* query engine specific stuff */
  void* context;

  struct rasqal_query_engine_factory_s* factory;

  rasqal_triples_source_factory* triples_source_factory;

  /* (linked list of) query results made from this query */
  rasqal_query_results* results;

  /* incrementing counter for declaring prefixes in order of appearance */
  int prefix_depth;

  /* sequence of constraints - internal for RDQL parsing, not returned */
  raptor_sequence* constraints_sequence;
  
  /* sequence of order condition expressions */
  raptor_sequence* order_conditions_sequence;

  /* sequence of group by condition expressions */
  raptor_sequence* group_conditions_sequence;

  /* INTERNAL rasqal_literal_compare / rasqal_expression_evaluate flags */
  int compare_flags;

  /* Number of graph patterns in this query */
  int graph_pattern_count;
  
  /* Graph pattern shared pointers by gp index (after prepare) */
  raptor_sequence* graph_patterns_sequence;

  /* Features */
  int features[RASQAL_FEATURE_LAST+1];

  /* Name of requested query results syntax.  If present, this
   * is the name used for constructing a rasqal_query_formatter
   * from the results.
   */
  const char* query_results_formatter_name;

  /* EXPLAIN was given */
  int explain;

  /* generated counter - increments at every generation */
  int genid_counter;

  /* INTERNAL lexer internal data */
  void* lexer_user_data;

  /* INTERNAL for now: non-0 to store results otherwise lazy eval results */
  int store_results;
};


/*
 * A query engine factory for a query language
 */
struct rasqal_query_engine_factory_s {
  struct rasqal_query_engine_factory_s* next;

  /* query language name */
  const char* name;

  /* query language readable label */
  const char* label;

  /* query language alternate name */
  const char* alias;

  /* query language MIME type (or NULL) */
  const char* mime_type;

  /* query language URI (or NULL) */
  const unsigned char* uri_string;
  
  /* the rest of this structure is populated by the
     query-engine-specific register function */
  size_t context_length;
  
  /* create a new query */
  int (*init)(rasqal_query* rq, const char *name);
  
  /* destroy a query */
  void (*terminate)(rasqal_query* rq);
  
  /* prepare a query */
  int (*prepare)(rasqal_query* rq);
  
  /* finish the query engine factory */
  void (*finish_factory)(rasqal_query_engine_factory* factory);

  /* Write a string to an iostream in escaped form suitable for the query */
  int (*iostream_write_escaped_counted_string)(rasqal_query* rq, raptor_iostream* iostr, const unsigned char* string, size_t len);
};


typedef struct rasqal_rowsource_s rasqal_rowsource;

/*
 * A row of values from a query result, usually generated by a rowsource
 */
typedef struct {
  /* reference count */
  int usage;

  /* Rowsource this row is associated with (or NULL if none) */
  rasqal_rowsource* rowsource;

  /* current row number in the sequence of rows*/
  int offset;

  /* values for each variable in the query sequence of values */
  int size;
  rasqal_literal** values;

  /* literal values for ORDER BY expressions evaluated for this row */
  /* number of expressions (can be 0) */
  int order_size;
  rasqal_literal** order_values;
} rasqal_query_result_row;


typedef struct rasqal_map_s rasqal_map;

/* The execution data here is a sequence of
 * rasqal_graph_pattern_data execution data of size
 * query->graph_pattern_count with each rasqal_graph_pattern_data
 */
typedef struct {
  raptor_sequence* seq;

  /* offset into stored results sequence */
  int offset;

  /* for ordering results during execution */
  rasqal_map* map;
} rasqal_engine_execution_data;


/**
 * rasqal_query_results_type:
 * @RASQAL_QUERY_RESULTS_BINDINGS: variable binding
 * @RASQAL_QUERY_RESULTS_BOOLEAN: a single boolean
 * @RASQAL_QUERY_RESULTS_GRAPH: an RDF graph
 * @RASQAL_QUERY_RESULTS_SYNTAX: a syntax
 *
 * Query result type.
 */

typedef enum {
  RASQAL_QUERY_RESULTS_BINDINGS,
  RASQAL_QUERY_RESULTS_BOOLEAN,
  RASQAL_QUERY_RESULTS_GRAPH,
  RASQAL_QUERY_RESULTS_SYNTAX
} rasqal_query_results_type;


/*
 * A query result for some query
 */
struct rasqal_query_results_s {
  /* Type of query result */
  rasqal_query_results_type type;
  
  /* stopping? */
  int abort;

  /* non-0 if got all results */
  int finished;

  /* non-0 if query has been executed */
  int executed;

  /* non 0 if query had fatal error and cannot be executed */
  int failed;

  /* query that this was executed over */
  rasqal_query* query;

  /* how many results already found */
  int result_count;

  /* execution data - depends on execution engine */
  void* execution_data;

  /* pointer to function that tidies the query_results execution data above */
  void (*free_execution_data)(rasqal_query* query, struct rasqal_query_results_s* query_results, void *execution_data);
  
  /* next query result */
  rasqal_query_results *next;

  /* current row of results */
  rasqal_query_result_row* row;

  /* boolean ASK result >0 true, 0 false or -1 uninitialised */
  int ask_result;

  /* New variables bound from during the current 'next result' run */
  int new_bindings_count;

  rasqal_triples_source* triples_source;

  /* current triple in the sequence of triples 'constructs' or -1 */
  int current_triple_result;

  /* constructed triple result (SHARED) */
  raptor_statement result_triple;

  /* INTERNAL triple used to store literals for subject, predicate, object
   * never returned
   */
  rasqal_triple* triple;
  
  /* INTERNAL sequence of results for ordering */
  raptor_sequence* results_sequence;

  /* size of result row fields row->results, row->values, variables, variable_names, variables_sequence */
  int size;

  /* size of result row ordering fields row->order_values */
  int order_size;

  /* array of variable names */
  const unsigned char** variable_names;

  /* sequence of variables */
  raptor_sequence* variables_sequence;

  /* variable name/value table of length 'size' */
  rasqal_variable** variables;

  /* Source of rows that are filling this query result*/
  rasqal_rowsource* rowsource;
};
    

/* rasqal_rowsource.c */

/**
 * rasqal_rowsource_init_func:
 * @context: stream context data
 *
 * Handler function for #rasqal_rowsource initialising.
 *
 * Return value: non-0 on failure.
 */
typedef int (*rasqal_rowsource_init_func) (rasqal_rowsource* rowsource, void *user_data);

/**
 * rasqal_rowsource_finish_func:
 * @user_data: user data
 *
 * Handler function for #rasqal_rowsource terminating.
 *
 * Return value: non-0 on failure
 */
typedef int (*rasqal_rowsource_finish_func) (rasqal_rowsource* rowsource, void *user_data);

/**
 * rasqal_rowsource_ensure_variables_func
 * @rowsource: #rasqal_rowsource
 * @user_data: user data
 *
 * Handler function for ensuring rowsource variables fields are initialised
 *
 * Return value: non-0 on failure
 */
typedef int (*rasqal_rowsource_ensure_variables_func) (rasqal_rowsource* rowsource, void *user_data);

/**
 * rasqal_rowsource_read_row_func
 * @user_data: user data
 *
 * Handler function for returning the next result row
 *
 * Return value: a query result row or NULL if exhausted
 */
typedef rasqal_query_result_row* (*rasqal_rowsource_read_row_func) (rasqal_rowsource* rowsource, void *user_data);


/**
 * rasqal_rowsource_read_all_rows_func
 * @user_data: user data
 *
 * Handler function for returning all rows as a sequence
 *
 * Return value: a query result row or NULL if exhausted
 */
typedef raptor_sequence* (*rasqal_rowsource_read_all_rows_func) (rasqal_rowsource* rowsource, void *user_data);


/**
 * rasqal_rowsource_get_query_func
 * @user_data: user data
 *
 * Handler function for returning a query associated with a rowsource
 *
 * Return value: a query or NULL if none is associated with it
 */
typedef rasqal_query* (*rasqal_rowsource_get_query_func) (rasqal_rowsource* rowsource, void *user_data);


/**
 * rasqal_rowsource_handler:
 * @version: API version - 1
 * @init:  initialisation handler - optional, called at most once (V1)
 * @finish: finishing handler - optional, called at most once (V1)
 * @ensure_variables: update variables handler- optional, called at most once (V1)
 * @read_row: read row handler - required (V1)
 *
 * ROW implementation handler structure.
 * 
 */
typedef struct {
  int version;
  /* API V1 methods */
  rasqal_rowsource_init_func             init;
  rasqal_rowsource_finish_func           finish;
  rasqal_rowsource_ensure_variables_func ensure_variables;
  rasqal_rowsource_read_row_func         read_row;
  rasqal_rowsource_read_all_rows_func    read_all_rows;
  rasqal_rowsource_get_query_func        get_query;
} rasqal_rowsource_handler;


/**
 * rasqal_rowsource:
 *
 * Rasqal Row Source class
 */
struct rasqal_rowsource_s
{
  int flags;
  
  void *user_data;
  const rasqal_rowsource_handler* handler;

  /* non-0 if rowsource has been exhausted */
  int finished;

  /* count of number of rows returned */
  int count;

  /* array of rasqal_variable* */
  raptor_sequence* variables_sequence;

  /* row width */
  int size;

  /* order row width */
  int order_size;
};


#define RASQAL_ROWSOURCE_FLAGS_ORDERING 1

rasqal_rowsource* rasqal_new_rowsource_from_handler(void* user_data, const rasqal_rowsource_handler *handler, int flags);
void rasqal_free_rowsource(rasqal_rowsource *rowsource);

int rasqal_rowsource_update_variables(rasqal_rowsource *rowsource, rasqal_query_results* results);
rasqal_query_result_row* rasqal_rowsource_read_row(rasqal_rowsource *rowsource);
int rasqal_rowsource_get_rows_count(rasqal_rowsource *rowsource);
raptor_sequence* rasqal_rowsource_read_all_rows(rasqal_rowsource *rowsource);
rasqal_query* rasqal_rowsource_get_query(rasqal_rowsource *rowsource);
void rasqal_rowsource_add_variable(rasqal_rowsource *rowsource, rasqal_variable* v);
void rasqal_rowsource_get_sizes(rasqal_rowsource *rowsource, int* size_p, int* order_size_p);
rasqal_variable* rasqal_rowsource_get_variable_by_offset(rasqal_rowsource *rowsource, int offset);
int rasqal_rowsource_get_variable_offset_by_name(rasqal_rowsource *rowsource, const char* name);

typedef int (*rasqal_query_results_formatter_func)(raptor_iostream *iostr, rasqal_query_results* results, raptor_uri *base_uri);

typedef rasqal_rowsource* (*rasqal_query_results_get_rowsource_func)(rasqal_world*, raptor_iostream *iostr, raptor_uri *base_uri);


typedef struct {
  /* query results format name */
  const char* name;

  /* query results format name */
  const char* label;

  /* query results format URI (or NULL) */
  const unsigned char* uri_string;

  /* format writer: READ from results, WRITE syntax (using base URI) to iostr */
  rasqal_query_results_formatter_func writer;

  /* format reader: READ syntax from iostr using base URI, WRITE to results */
  rasqal_query_results_formatter_func reader;

  /* format get rowsource: get a rowsource that will return a sequence of rows from an iostram */
  rasqal_query_results_get_rowsource_func get_rowsource;

  /* MIME Type of the format */
  const char* mime_type;
} rasqal_query_results_format_factory;


/*
 * A query results formatter for some query_results
 */
struct rasqal_query_results_formatter_s {
  rasqal_query_results_format_factory* factory;

  const char *mime_type;
};

/* rasqal_datetime.c */
int rasqal_xsd_datetime_check(const unsigned char* string);


/* rasqal_general.c */
char* rasqal_vsnprintf(const char* message, va_list arguments);

int rasqal_query_engine_register_factory(rasqal_world*, const char* name, const char* label, const char* alias, const unsigned char* uri_string, void (*factory) (rasqal_query_engine_factory*));
rasqal_query_engine_factory* rasqal_get_query_engine_factory (rasqal_world*, const char* name, const unsigned char* uri);
void rasqal_log_error_simple(rasqal_world* world, raptor_log_level level, raptor_locator* locator, const char* message, ...) RASQAL_PRINTF_FORMAT(4, 5);
void rasqal_log_error_varargs(rasqal_world* world, raptor_log_level level, raptor_locator* locator, const char* message, va_list arguments) RASQAL_PRINTF_FORMAT(4, 0);
void rasqal_log_error(raptor_log_level level, raptor_message_handler handler, void* handler_data, raptor_locator* locator, const char* message);
void rasqal_query_simple_error(void* query, const char *message, ...) RASQAL_PRINTF_FORMAT(2, 3);

const char* rasqal_basename(const char* name);


/* rasqal_graph_pattern.c */
unsigned char* rasqal_escaped_name_to_utf8_string(const unsigned char* src, size_t len, size_t* dest_lenp, raptor_simple_message_handler error_handler, void* error_data);

unsigned char* rasqal_query_generate_bnodeid(rasqal_query* rdf_query, unsigned char *user_bnodeid);

/* rdql_parser.y */
int rasqal_init_query_engine_rdql(rasqal_world*);

/* sparql_parser.y */
int rasqal_init_query_engine_sparql(rasqal_world*);
int rasqal_init_query_engine_laqrs(rasqal_world*);

/* rasqal_engine.c */
int rasqal_engine_sequence_has_qname(raptor_sequence* seq);
int rasqal_engine_expand_triple_qnames(rasqal_query* rq);
int rasqal_engine_query_constraints_has_qname(rasqal_query* gp);
int rasqal_engine_graph_pattern_constraints_has_qname(rasqal_graph_pattern* gp);
int rasqal_engine_expand_query_constraints_qnames(rasqal_query* rq);
int rasqal_engine_expand_graph_pattern_constraints_qnames(rasqal_query* rq, rasqal_graph_pattern* gp);
int rasqal_engine_build_constraints_expression(rasqal_graph_pattern* gp);
int rasqal_engine_assign_variables(rasqal_query* rq);

int rasqal_engine_prepare(rasqal_query* query);
int rasqal_engine_execute_init(rasqal_query_results* query_results);
int rasqal_engine_execute_finish(rasqal_query_results* query_results);
int rasqal_engine_join_graph_patterns(rasqal_graph_pattern *dest_gp, rasqal_graph_pattern *src_gp);
int rasqal_engine_check_limit_offset(rasqal_query_results* query_results);
int rasqal_engine_merge_triples(rasqal_query* query, rasqal_graph_pattern* gp, void* data);
int rasqal_engine_merge_graph_patterns(rasqal_query* query, rasqal_graph_pattern* gp, void* data);
int rasqal_engine_expression_fold(rasqal_query* rq, rasqal_expression* e);
int rasqal_engine_graph_pattern_fold_expressions(rasqal_query* rq, rasqal_graph_pattern* gp);
int rasqal_engine_query_fold_expressions(rasqal_query* rq);
int rasqal_engine_remove_empty_group_graph_patterns(rasqal_query* query, rasqal_graph_pattern* gp, void* data);
  
rasqal_triples_source* rasqal_new_triples_source(rasqal_query_results* query_results);
void rasqal_free_triples_source(rasqal_triples_source* rts);
int rasqal_triples_source_next_source(rasqal_triples_source* rts);

int rasqal_engine_get_next_result(rasqal_query_results* query_results);
void rasqal_engine_assign_binding_values(rasqal_query* query);
int rasqal_engine_move_constraints(rasqal_graph_pattern* dest_gp, rasqal_graph_pattern* src_gp);
int rasqal_engine_execute_run(rasqal_query_results* query_results);
void rasqal_engine_free_query_result_row(rasqal_query_result_row* row);
rasqal_literal** rasqal_engine_get_result_values(rasqal_query_results* query_results);
rasqal_literal* rasqal_engine_get_result_value(rasqal_query_results* query_results, int offset);
int rasqal_engine_execute_next(rasqal_query_results* query_results);

/* rasqal_expr.c */
rasqal_literal* rasqal_new_string_literal_node(rasqal_world*, const unsigned char *string, const char *language, raptor_uri *datatype);
int rasqal_literal_as_boolean(rasqal_literal* literal, int* error);
int rasqal_literal_as_integer(rasqal_literal* l, int* error);
double rasqal_literal_as_floating(rasqal_literal* l, int* error);
raptor_uri* rasqal_literal_as_uri(rasqal_literal* l);
int rasqal_literal_string_to_native(rasqal_literal *l, raptor_simple_message_handler error_handler, void *error_data, int flags);
int rasqal_literal_has_qname(rasqal_literal* l);
int rasqal_literal_expand_qname(void* user_data, rasqal_literal* l);
int rasqal_literal_is_constant(rasqal_literal* l);
int rasqal_expression_has_qname(void* user_data, rasqal_expression* e);
int rasqal_expression_expand_qname(void* user_data, rasqal_expression* e);
int rasqal_literal_ebv(rasqal_literal* l);
int rasqal_expression_is_constant(rasqal_expression* e);
void rasqal_expression_clear(rasqal_expression* e);
void rasqal_expression_convert_to_literal(rasqal_expression* e, rasqal_literal* l);
int rasqal_expression_mentions_variable(rasqal_expression* e, rasqal_variable* v);

/* strcasecmp.c */
#ifdef HAVE_STRCASECMP
#  define rasqal_strcasecmp strcasecmp
#  define rasqal_strncasecmp strncasecmp
#else
#  ifdef HAVE_STRICMP
#    define rasqal_strcasecmp stricmp
#    define rasqal_strncasecmp strnicmp
#   else
int rasqal_strcasecmp(const char* s1, const char* s2);
int rasqal_strncasecmp(const char* s1, const char* s2, size_t n);
#  endif
#endif

/* rasqal_raptor.c */
int rasqal_raptor_init(rasqal_world*);

#ifdef RAPTOR_TRIPLES_SOURCE_REDLAND
/* rasqal_redland.c */
int rasqal_redland_init(rasqal_world*);
void rasqal_redland_finish(void);
#endif  


/* rasqal_general.c */
int rasqal_uri_init(rasqal_world*);
void rasqal_uri_finish(rasqal_world*);

/* rasqal_literal.c */
typedef struct {
  raptor_sequence *triples;
  rasqal_literal *value;
} rasqal_formula;

rasqal_formula* rasqal_new_formula(void);
void rasqal_free_formula(rasqal_formula* formula);
void rasqal_formula_print(rasqal_formula* formula, FILE *stream);
rasqal_formula* rasqal_formula_join(rasqal_formula* first_formula, rasqal_formula* second_formula);
rasqal_graph_pattern* rasqal_engine_new_basic_graph_pattern_from_formula(rasqal_query* query, rasqal_formula* formula);
rasqal_graph_pattern* rasqal_engine_group_2_graph_patterns(rasqal_query* query, rasqal_graph_pattern* first_gp, rasqal_graph_pattern* second_gp);

/* The following should be public eventually in rasqal.h or raptor.h or ...? */

typedef int (rasqal_compare_fn)(const void *a, const void *b);
typedef void (rasqal_kv_free_fn)(const void *key, const void *value);


#define RASQAL_XSD_BOOLEAN_TRUE (const unsigned char*)"true"
#define RASQAL_XSD_BOOLEAN_FALSE (const unsigned char*)"false"

rasqal_literal* rasqal_literal_cast(rasqal_literal* l, raptor_uri* datatype, int flags,  int* error_p);
rasqal_literal* rasqal_new_numeric_literal(rasqal_world*, rasqal_literal_type type, double d);
int rasqal_literal_is_numeric(rasqal_literal* literal);
rasqal_literal* rasqal_literal_add(rasqal_literal* l1, rasqal_literal* l2, int *error);
rasqal_literal* rasqal_literal_subtract(rasqal_literal* l1, rasqal_literal* l2, int *error);
rasqal_literal* rasqal_literal_multiply(rasqal_literal* l1, rasqal_literal* l2, int *error);
rasqal_literal* rasqal_literal_divide(rasqal_literal* l1, rasqal_literal* l2, int *error);
rasqal_literal* rasqal_literal_negate(rasqal_literal* l, int *error_p);
int rasqal_literal_equals_flags(rasqal_literal* l1, rasqal_literal* l2, int flags, int* error);
rasqal_literal_type rasqal_literal_get_rdf_term_type(rasqal_literal* l);



/* rasqal_map.c */
typedef void (*rasqal_map_visit_fn)(void *key, void *value, void *user_data);

rasqal_map* rasqal_new_map(rasqal_compare_fn* compare_fn, rasqal_kv_free_fn* free_fn, raptor_sequence_print_handler* print_key_fn, raptor_sequence_print_handler* print_value_fn, int flags);
void rasqal_free_map(rasqal_map *map);
int rasqal_map_add_kv(rasqal_map* map, void* key, void *value);
void rasqal_map_visit(rasqal_map* map, rasqal_map_visit_fn fn, void *user_data);
void rasqal_map_print(rasqal_map* map, FILE* fh);

/* rasqal_query.c */
void rasqal_query_remove_query_result(rasqal_query* query, rasqal_query_results* query_results);
int rasqal_query_declare_prefix(rasqal_query* rq, rasqal_prefix* prefix);
int rasqal_query_declare_prefixes(rasqal_query* rq);
unsigned char* rasqal_query_get_genid(rasqal_query* query, const unsigned char* base, int counter);
void rasqal_query_set_base_uri(rasqal_query* rq, raptor_uri* base_uri);
#ifdef RASQAL_DEBUG
void rasqal_query_set_store_results(rasqal_query* query, int store_results);
#endif

/* rasqal_query_results.c */
int rasqal_init_query_results(void);
void rasqal_finish_query_results(void);

/* rasqal_result_formats.c */
int rasqal_query_results_format_register_factory(rasqal_world*, const char *name, const char *label, const unsigned char* uri_string, rasqal_query_results_formatter_func writer, rasqal_query_results_formatter_func reader, rasqal_query_results_get_rowsource_func get_rowsource, const char *mime_type);
int rasqal_init_result_formats(rasqal_world*);
void rasqal_finish_result_formats(rasqal_world*);

/* rasqal_result_format_sparql_xml.c */
int rasqal_init_result_format_sparql_xml(rasqal_world*);


rasqal_query_results* rasqal_new_query_results(rasqal_query* query);
void rasqal_query_results_reset(rasqal_query_results* query_results);
rasqal_query_result_row* rasqal_new_query_result_row(rasqal_rowsource* rowsource);
rasqal_query_result_row* rasqal_new_query_result_row_from_query_result_row(rasqal_query_result_row* row);
void rasqal_free_query_result_row(rasqal_query_result_row* row);
int rasqal_query_results_set_variables(rasqal_query_results* query_results, raptor_sequence* variables_sequence, int size, int order_size);
void rasqal_query_result_row_print(rasqal_query_result_row* row, FILE* fh);
void rasqal_query_results_set_order_conditions(rasqal_query_results* query_results, int order_size);
void rasqal_query_results_add_row(rasqal_query_results* query_results, rasqal_query_result_row* row);
void rasqal_query_result_row_set_value_at(rasqal_query_result_row* row, int offset, rasqal_literal* value);


/* rasqal_xsd_datatypes.c */
int rasqal_xsd_init(rasqal_world*);
void rasqal_xsd_finish(rasqal_world*);
rasqal_literal_type rasqal_xsd_datatype_uri_to_type(rasqal_world*, raptor_uri* uri);
raptor_uri* rasqal_xsd_datatype_type_to_uri(rasqal_world*, rasqal_literal_type type);
int rasqal_xsd_datatype_check(rasqal_literal_type native_type, const unsigned char* string, int flags);
const char* rasqal_xsd_datatype_label(rasqal_literal_type native_type);
int rasqal_xsd_is_datatype_uri(rasqal_world*, raptor_uri* uri);
const unsigned char* rasqal_xsd_datetime_string_to_canonical(const unsigned char* datetime_string);
rasqal_literal_type rasqal_xsd_datatype_uri_parent_type(rasqal_world* world, raptor_uri* uri);
int rasqal_xsd_datatype_is_numeric(rasqal_literal_type type);
unsigned char* rasqal_xsd_format_double(double d, size_t *len_p);


/* rasqal_world structure */
struct rasqal_world_s {
  /* error handlers structure */
  raptor_error_handlers error_handlers;

  /* linked list of query engines */
  rasqal_query_engine_factory *query_engines;

  /* registered query results formats */
  raptor_sequence *query_results_formats;

  /* rasqal_uri rdf uris */
  raptor_uri *rdf_namespace_uri;
  raptor_uri *rdf_first_uri;
  raptor_uri *rdf_rest_uri;
  raptor_uri *rdf_nil_uri;

  /* triples source factory */
  rasqal_triples_source_factory triples_source_factory;

  /* rasqal_xsd_datatypes */
  raptor_uri *xsd_namespace_uri;
  raptor_uri **xsd_datatype_uris;
};


/* end of RASQAL_INTERNAL */
#endif


#ifdef __cplusplus
}
#endif

#endif
