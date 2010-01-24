/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rasqal_expr.c - Rasqal general expression support
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
 * 
 */

#ifdef HAVE_CONFIG_H
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <stdarg.h>

#ifdef RASQAL_REGEX_PCRE
#include <pcre.h>
#endif

#ifdef RASQAL_REGEX_POSIX
#include <sys/types.h>
#include <regex.h>
#endif

#include "rasqal.h"
#include "rasqal_internal.h"


#ifndef STANDALONE


/**
 * rasqal_new_data_graph:
 * @uri: source URI
 * @name_uri: name of graph (or NULL)
 * @flags: %RASQAL_DATA_GRAPH_NAMED or %RASQAL_DATA_GRAPH_BACKGROUND
 * 
 * Constructor - create a new #rasqal_data_graph.
 * 
 * The name_uri is only used when the flags are %RASQAL_DATA_GRAPH_NAMED.
 * 
 * Return value: a new #rasqal_data_graph or NULL on failure.
 **/
rasqal_data_graph*
rasqal_new_data_graph(raptor_uri* uri, raptor_uri* name_uri, int flags)
{
  rasqal_data_graph* dg=(rasqal_data_graph*)RASQAL_CALLOC(rasqal_data_graph, 1,
                                                      sizeof(rasqal_data_graph));
  if(dg) {  
    dg->uri=raptor_uri_copy(uri);
    if(name_uri)
      dg->name_uri=raptor_uri_copy(name_uri);
    dg->flags=flags;
  }

  return dg;
}


/**
 * rasqal_free_data_graph:
 * @dg: #rasqal_data_graph object
 * 
 * Destructor - destroy a #rasqal_data_graph object.
 *
 **/
void
rasqal_free_data_graph(rasqal_data_graph* dg)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(dg, rasqal_data_graph);
  
  if(dg->uri)
    raptor_free_uri(dg->uri);
  if(dg->name_uri)
    raptor_free_uri(dg->name_uri);
  RASQAL_FREE(rasqal_data_graph, dg);
}


/**
 * rasqal_data_graph_print:
 * @dg: #rasqal_data_graph object
 * @fh: the #FILE* handle to print to
 *
 * Print a Rasqal data graph in a debug format.
 * 
 * The print debug format may change in any release.
 **/
void
rasqal_data_graph_print(rasqal_data_graph* dg, FILE* fh)
{
  if(dg->name_uri)
    fprintf(fh, "data graph(%s named as %s flags %d)", 
            raptor_uri_as_string(dg->uri),
            raptor_uri_as_string(dg->name_uri),
            dg->flags);
  else
    fprintf(fh, "data graph(%s, flags %d)", 
            raptor_uri_as_string(dg->uri), dg->flags);
}



/**
 * rasqal_new_variable_typed:
 * @rq: #rasqal_query to associate the variable with
 * @type: variable type defined by enumeration rasqal_variable_type
 * @name: variable name
 * @value: variable #rasqal_literal value (or NULL)
 *
 * Constructor - Create a new typed Rasqal variable.
 * 
 * The variable must be associated with a query, since variable
 * names are only significant with a single query.
 * 
 * The @name and @value become owned by the rasqal_variable structure
 *
 * Return value: a new #rasqal_variable or NULL on failure.
 **/
rasqal_variable*
rasqal_new_variable_typed(rasqal_query* rq,
                          rasqal_variable_type type, 
                          unsigned char *name, rasqal_literal *value)
{
  int i;
  rasqal_variable* v;
  raptor_sequence* seq=NULL;
  int* count_p=NULL;

  if(rq) {
    switch(type) {
      case RASQAL_VARIABLE_TYPE_ANONYMOUS:
        seq=rq->anon_variables_sequence;
        count_p=&rq->anon_variables_count;
        break;
      case RASQAL_VARIABLE_TYPE_NORMAL:
        seq=rq->variables_sequence;
        count_p=&rq->variables_count;
        break;

      case RASQAL_VARIABLE_TYPE_UNKNOWN:
      default:
        RASQAL_DEBUG2("Unknown variable type %d", type);
        return NULL;
    }
  
    for(i=0; i< raptor_sequence_size(seq); i++) {
      v=(rasqal_variable*)raptor_sequence_get_at(seq, i);
      if(!strcmp((const char*)v->name, (const char*)name)) {
        /* name already present, do not need a copy */
        RASQAL_FREE(cstring, name);
        return v;
      }
    }
  }
  
  v=(rasqal_variable*)RASQAL_CALLOC(rasqal_variable, 1, sizeof(rasqal_variable));
  if(v) {
    v->type= type;
    v->name= name;
    v->value= value;
    if(count_p)
      v->offset= (*count_p);

    if(seq && raptor_sequence_push(seq, v))
      return NULL;

    /* Increment count only after sequence push succeeded */
    if(count_p)
      (*count_p)++;
  } else {
    RASQAL_FREE(cstring, name);
    if(value)
      rasqal_free_literal(value);
  }
  
  return v;
}


/**
 * rasqal_new_variable:
 * @rq: #rasqal_query to associate the variable with
 * @name: variable name
 * @value: variable #rasqal_literal value (or NULL)
 *
 * Constructor - Create a new Rasqal normal variable.
 * 
 * The variable must be associated with a query, since variable
 * names are only significant with a single query.
 *
 * This creates a regular variable that can be returned of type
 * RASQAL_VARIABLE_TYPE_NORMAL.  Use rasqal_new_variable_typed
 * to create other variables.
 * 
 * The @name and @value become owned by the rasqal_variable structure
 *
 * Return value: a new #rasqal_variable or NULL on failure.
 **/
rasqal_variable*
rasqal_new_variable(rasqal_query* rq,
                    unsigned char *name, rasqal_literal *value) 
{
  return rasqal_new_variable_typed(rq, RASQAL_VARIABLE_TYPE_NORMAL, name, value);
}


/**
 * rasqal_new_variable_from_variable:
 * @v: #rasqal_variable to copy
 *
 * Copy Constructor - Create a new Rasqal variable from an existing one
 *
 * This does a deep copy of all variable fields
 *
 * Return value: a new #rasqal_variable or NULL on failure.
 **/
rasqal_variable*
rasqal_new_variable_from_variable(rasqal_variable* v)
{
  rasqal_variable* new_v;
  size_t name_len;
  unsigned char *new_name;

  new_v=(rasqal_variable*)RASQAL_CALLOC(rasqal_variable, 1, sizeof(rasqal_variable));
  if(!new_v)
    return NULL;
  
  name_len=strlen((const char*)v->name);
  new_name=(unsigned char*)RASQAL_MALLOC(cstring, name_len+1);
  if(!new_name) {
    RASQAL_FREE(rasqal_variable, new_v);
    return NULL;
  }
  memcpy(new_name, v->name, name_len+1);
  
  new_v->name= new_name;
  new_v->value= rasqal_new_literal_from_literal(v->value);
  new_v->offset= v->offset;
  new_v->type= v->type;
  new_v->expression= rasqal_new_expression_from_expression(v->expression);

  return new_v;
}

/**
 * rasqal_free_variable:
 * @v: #rasqal_variable object
 *
 * Destructor - Destroy a Rasqal variable object.
 *
 **/
void
rasqal_free_variable(rasqal_variable* v)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(v, rasqal_variable);
  
  if(v->name)
    RASQAL_FREE(cstring, (void*)v->name);
  if(v->value)
    rasqal_free_literal(v->value);
  if(v->expression)
    rasqal_free_expression(v->expression);
  RASQAL_FREE(rasqal_variable, v);
}


/**
 * rasqal_variable_print:
 * @v: the #rasqal_variable object
 * @fh: the #FILE* handle to print to
 *
 * Print a Rasqal variable in a debug format.
 * 
 * The print debug format may change in any release.
 * 
 **/
void
rasqal_variable_print(rasqal_variable* v, FILE* fh)
{
  if(v->type == RASQAL_VARIABLE_TYPE_ANONYMOUS)
    fprintf(fh, "anon-variable(%s", v->name);
  else
    fprintf(fh, "variable(%s", v->name);
  if(v->expression) {
    fputc('=', fh);
    rasqal_expression_print(v->expression, fh);
  }
  if(v->value) {
    fputc('=', fh);
    rasqal_literal_print(v->value, fh);
  }
  fputc(')', fh);
}


/**
 * rasqal_variable_set_value:
 * @v: the #rasqal_variable object
 * @l: the #rasqal_literal value to set (or NULL)
 *
 * Set the value of a Rasqal variable.
 * 
 * The variable value is an input parameter and is copied in, not shared.
 * If the variable value is NULL, any existing value is deleted.
 * 
 **/
void
rasqal_variable_set_value(rasqal_variable* v, rasqal_literal* l)
{
  if(v->value)
    rasqal_free_literal(v->value);
  v->value=l;
#ifdef RASQAL_DEBUG
  if(!v->name)
    RASQAL_FATAL1("variable has no name");
  RASQAL_DEBUG2("setting variable %s to value ", v->name);
  if(v->value)
    rasqal_literal_print(v->value, stderr);
  else
    fputs("(NULL)", stderr);
  fputc('\n', stderr);
#endif
}


/**
 * rasqal_new_prefix:
 * @prefix: Short prefix string to stand for URI or NULL.
 * @uri: Name #raptor_uri.
 * 
 * Constructor - create a new #rasqal_prefix.
 * Takes ownership of prefix and uri.
 * 
 * Return value: a new #rasqal_prefix or NULL on failure.
 **/
rasqal_prefix*
rasqal_new_prefix(const unsigned char *prefix, raptor_uri* uri) 
{
  rasqal_prefix* p=(rasqal_prefix*)RASQAL_CALLOC(rasqal_prefix, 1,
                                                 sizeof(rasqal_prefix));

  if(p) {  
    p->prefix=prefix;
    p->uri=uri;
  } else {
    RASQAL_FREE(cstring, prefix);
    raptor_free_uri(uri);
  }

  return p;
}


/**
 * rasqal_free_prefix:
 * @p: #rasqal_prefix object.
 * 
 * Destructor - destroy a #rasqal_prefix object.
 **/
void
rasqal_free_prefix(rasqal_prefix* p)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(p, rasqal_prefix);
  
  if(p->prefix)
    RASQAL_FREE(cstring, (void*)p->prefix);
  if(p->uri)
    raptor_free_uri(p->uri);
  RASQAL_FREE(rasqal_prefix, p);
}


/**
 * rasqal_prefix_print:
 * @p: #rasqal_prefix object.
 * @fh: The #FILE* handle to print to.
 *
 * Print a Rasqal prefix in a debug format.
 * 
 * The print debug format may change in any release.
 **/
void
rasqal_prefix_print(rasqal_prefix* p, FILE* fh)
{
  fprintf(fh, "prefix(%s as %s)", (p->prefix ? (const char*)p->prefix : "(default)"), raptor_uri_as_string(p->uri));
}



/**
 * rasqal_new_triple:
 * @subject: Triple subject.
 * @predicate: Triple predicate.
 * @object: Triple object.
 * 
 * Constructor - create a new #rasqal_triple triple or triple pattern.
 * Takes ownership of the literals passed in.
 * 
 * The triple origin can be set with rasqal_triple_set_origin().
 *
 * Return value: a new #rasqal_triple or NULL on failure.
 **/
rasqal_triple*
rasqal_new_triple(rasqal_literal* subject, rasqal_literal* predicate, rasqal_literal* object)
{
  rasqal_triple* t=(rasqal_triple*)RASQAL_CALLOC(rasqal_triple, 1, sizeof(rasqal_triple));

  if(t) {
    t->subject=subject;
    t->predicate=predicate;
    t->object=object;
  } else {
    if(subject)
      rasqal_free_literal(subject);
    if(predicate)
      rasqal_free_literal(predicate);
    if(object)
      rasqal_free_literal(object);
  }

  return t;
}


/**
 * rasqal_new_triple_from_triple:
 * @t: Triple to copy.
 * 
 * Copy constructor - create a new #rasqal_triple from an existing one.
 * 
 * Return value: a new #rasqal_triple or NULL on failure.
 **/
rasqal_triple*
rasqal_new_triple_from_triple(rasqal_triple* t)
{
  rasqal_triple* newt=(rasqal_triple*)RASQAL_CALLOC(rasqal_triple, 1, sizeof(rasqal_triple));

  if(newt) {
    newt->subject=rasqal_new_literal_from_literal(t->subject);
    newt->predicate=rasqal_new_literal_from_literal(t->predicate);
    newt->object=rasqal_new_literal_from_literal(t->object);
  }

  return newt;
}


/**
 * rasqal_free_triple:
 * @t: #rasqal_triple object.
 * 
 * Destructor - destroy a #rasqal_triple object.
 **/
void
rasqal_free_triple(rasqal_triple* t)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(t, rasqal_triple);
  
  if(t->subject)
    rasqal_free_literal(t->subject);
  if(t->predicate)
    rasqal_free_literal(t->predicate);
  if(t->object)
    rasqal_free_literal(t->object);
  if(t->origin)
    rasqal_free_literal(t->origin);
  RASQAL_FREE(rasqal_triple, t);
}


/**
 * rasqal_triple_print:
 * @t: #rasqal_triple object.
 * @fh: The #FILE* handle to print to.
 * 
 * Print a Rasqal triple in a debug format.
 * 
 * The print debug format may change in any release.
 **/
void
rasqal_triple_print(rasqal_triple* t, FILE* fh)
{
  fputs("triple(", fh);
  rasqal_literal_print(t->subject, fh);
  fputs(", ", fh);
  rasqal_literal_print(t->predicate, fh);
  fputs(", ", fh);
  rasqal_literal_print(t->object, fh);
  fputc(')', fh);
  if(t->origin) {
    fputs(" with origin(", fh);
    rasqal_literal_print(t->origin, fh);
    fputc(')', fh);
  }
}


/**
 * rasqal_triple_set_origin:
 * @t: The triple object. 
 * @l: The #rasqal_literal object to set as origin.
 * 
 * Set the origin field of a #rasqal_triple.
 **/
void
rasqal_triple_set_origin(rasqal_triple* t, rasqal_literal* l)
{
  t->origin=l;
}


/**
 * rasqal_triple_get_origin:
 * @t: The triple object. 
 * 
 * Get the origin field of a #rasqal_triple.
 * 
 * Return value: The triple origin or NULL.
 **/
rasqal_literal*
rasqal_triple_get_origin(rasqal_triple* t)
{
  return t->origin;
}


/**
 * rasqal_new_0op_expression:
 * @op: Expression operator
 * 
 * Constructor - create a new 0-operand (constant) expression.
 *
 * The operators are:
 * @RASQAL_EXPR_VARSTAR
 *
 * The only operator here is the '*' in COUNT(*) as used by LAQRS.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_0op_expression(rasqal_op op)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=op;
  }
  return e;
}


/**
 * rasqal_new_1op_expression:
 * @op: Expression operator
 * @arg: Operand 1 
 * 
 * Constructor - create a new 1-operand expression.
 * Takes ownership of the operand expression.
 *
 * The operators are:
 * @RASQAL_EXPR_TILDE @RASQAL_EXPR_BANG @RASQAL_EXPR_UMINUS
 * @RASQAL_EXPR_BOUND @RASQAL_EXPR_STR @RASQAL_EXPR_LANG
 * @RASQAL_EXPR_LANGMATCHES
 * @RASQAL_EXPR_DATATYPE @RASQAL_EXPR_ISURI @RASQAL_EXPR_ISBLANK
 * @RASQAL_EXPR_ISLITERAL @RASQAL_EXPR_ORDER_COND_ASC
 * @RASQAL_EXPR_ORDER_COND_DESC @RASQAL_EXPR_GROUP_COND_ASC
 * @RASQAL_EXPR_GROUP_COND_DESC @RASQAL_EXPR_COUNT
 *
 * @RASQAL_EXPR_BANG and @RASQAL_EXPR_UMINUS are used by RDQL and
 * SPARQL.  @RASQAL_EXPR_TILDE by RDQL only.  The rest by SPARQL
 * only.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_1op_expression(rasqal_op op, rasqal_expression* arg)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=op;
    e->arg1=arg;
  } else {
    rasqal_free_expression(arg);
  }
  return e;
}


/**
 * rasqal_new_2op_expression:
 * @op: Expression operator
 * @arg1: Operand 1 
 * @arg2: Operand 2
 * 
 * Constructor - create a new 2-operand expression.
 * Takes ownership of the operand expressions.
 * 
 * The operators are:
 * @RASQAL_EXPR_AND @RASQAL_EXPR_OR @RASQAL_EXPR_EQ
 * @RASQAL_EXPR_NEQ @RASQAL_EXPR_LT @RASQAL_EXPR_GT @RASQAL_EXPR_LE
 * @RASQAL_EXPR_GE @RASQAL_EXPR_PLUS @RASQAL_EXPR_MINUS
 * @RASQAL_EXPR_STAR @RASQAL_EXPR_SLASH @RASQAL_EXPR_REM
 * @RASQAL_EXPR_STR_EQ @RASQAL_EXPR_STR_NEQ
 *
 * @RASQAL_EXPR_REM @RASQAL_EXPR_STR_EQ and @RASQAL_EXPR_STR_NEQ are
 * not used by SPARQL. @RASQAL_EXPR_REM is used by RDQL.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_2op_expression(rasqal_op op,
                          rasqal_expression* arg1, 
                          rasqal_expression* arg2)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=op;
    e->arg1=arg1;
    e->arg2=arg2;
  } else {
    rasqal_free_expression(arg1);
    rasqal_free_expression(arg2);
  }
  return e;
}


/**
 * rasqal_new_3op_expression:
 * @op: Expression operator
 * @arg1: Operand 1 
 * @arg2: Operand 2
 * @arg3: Operand 3 (may be NULL)
 * 
 * Constructor - create a new 3-operand expression.
 * Takes ownership of the operands.
 * 
 * The only operator is:
 * @RASQAL_EXPR_REGEX
 *
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_3op_expression(rasqal_op op,
                          rasqal_expression* arg1, 
                          rasqal_expression* arg2,
                          rasqal_expression* arg3)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=op;
    e->arg1=arg1;
    e->arg2=arg2;
    e->arg3=arg3;
  } else {
    rasqal_free_expression(arg1);
    rasqal_free_expression(arg2);
    if(arg3)
      rasqal_free_expression(arg3);
  }
  return e;
}


/**
 * rasqal_new_string_op_expression:
 * @op: Expression operator
 * @arg1: Operand 1 
 * @literal: Literal operand 2
 * 
 * Constructor - create a new expression with one expression and one string operand.
 * Takes ownership of the operands.
 *
 * The operators are:
 * @RASQAL_EXPR_STR_MATCH (RDQL, SPARQL) and
 * @RASQAL_EXPR_STR_NMATCH (RDQL)
 *
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_string_op_expression(rasqal_op op,
                                rasqal_expression* arg1,
                                rasqal_literal* literal)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=op;
    e->arg1=arg1;
    e->literal=literal;
  } else {
    rasqal_free_expression(arg1);
    rasqal_free_literal(literal);
  }
  return e;
}


/**
 * rasqal_new_literal_expression:
 * @literal: Literal operand 1
 * 
 * Constructor - create a new expression for a #rasqal_literal
 * Takes ownership of the operand literal.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_literal_expression(rasqal_literal *literal)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {  
    e->usage=1;
    e->op=RASQAL_EXPR_LITERAL;
    e->literal=literal;
  } else {
    rasqal_free_literal(literal);
  }
  return e;
}


/**
 * rasqal_new_function_expression:
 * @name: function name
 * @args: sequence of #rasqal_expression function arguments
 * 
 * Constructor - create a new expression for a function with expression arguments.
 * Takes ownership of the function uri and arguments.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_function_expression(raptor_uri* name,
                               raptor_sequence* args)
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=RASQAL_EXPR_FUNCTION;
    e->name=name;
    e->args=args;
  } else {
    raptor_free_uri(name);
    raptor_free_sequence(args);
  }
  return e;
}


/**
 * rasqal_new_cast_expression:
 * @name: cast datatype URI
 * @value: expression value to cast to @datatype type
 * 
 * Constructor - create a new expression for casting and expression to a datatype.
 * Takes ownership of the datatype uri and expression value.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_cast_expression(raptor_uri* name, rasqal_expression *value) 
{
  rasqal_expression* e=(rasqal_expression*)RASQAL_CALLOC(rasqal_expression, 1, sizeof(rasqal_expression));
  if(e) {
    e->usage=1;
    e->op=RASQAL_EXPR_CAST;
    e->name=name;
    e->arg1=value;
  } else {
    raptor_free_uri(name);
    rasqal_free_expression(value);
  }
  return e;
}


/**
 * rasqal_expression_clear:
 * @e: expression
 * 
 * Empty an expression of contained content.
 *
 * Intended to be used to deallocate resources from a statically
 * declared #rasqal_expression such as on a stack.
 **/
void
rasqal_expression_clear(rasqal_expression* e)
{
  switch(e->op) {
    case RASQAL_EXPR_AND:
    case RASQAL_EXPR_OR:
    case RASQAL_EXPR_EQ:
    case RASQAL_EXPR_NEQ:
    case RASQAL_EXPR_LT:
    case RASQAL_EXPR_GT:
    case RASQAL_EXPR_LE:
    case RASQAL_EXPR_GE:
    case RASQAL_EXPR_PLUS:
    case RASQAL_EXPR_MINUS:
    case RASQAL_EXPR_STAR:
    case RASQAL_EXPR_SLASH:
    case RASQAL_EXPR_REM:
    case RASQAL_EXPR_STR_EQ:
    case RASQAL_EXPR_STR_NEQ:
    case RASQAL_EXPR_LANGMATCHES:
    case RASQAL_EXPR_SAMETERM:
      rasqal_free_expression(e->arg1);
      rasqal_free_expression(e->arg2);
      break;
    case RASQAL_EXPR_REGEX:
      rasqal_free_expression(e->arg1);
      rasqal_free_expression(e->arg2);
      if(e->arg3)
        rasqal_free_expression(e->arg3);
      break;
    case RASQAL_EXPR_TILDE:
    case RASQAL_EXPR_BANG:
    case RASQAL_EXPR_UMINUS:
    case RASQAL_EXPR_BOUND:
    case RASQAL_EXPR_STR:
    case RASQAL_EXPR_LANG:
    case RASQAL_EXPR_DATATYPE:
    case RASQAL_EXPR_ISURI:
    case RASQAL_EXPR_ISBLANK:
    case RASQAL_EXPR_ISLITERAL:
    case RASQAL_EXPR_ORDER_COND_ASC:
    case RASQAL_EXPR_ORDER_COND_DESC:
    case RASQAL_EXPR_GROUP_COND_ASC:
    case RASQAL_EXPR_GROUP_COND_DESC:
    case RASQAL_EXPR_COUNT:
      rasqal_free_expression(e->arg1);
      break;
    case RASQAL_EXPR_STR_MATCH:
    case RASQAL_EXPR_STR_NMATCH:
      rasqal_free_expression(e->arg1);
      /* FALLTHROUGH */
    case RASQAL_EXPR_LITERAL:
      rasqal_free_literal(e->literal);
      break;
    case RASQAL_EXPR_FUNCTION:
      raptor_free_uri(e->name);
      raptor_free_sequence(e->args);
      break;
    case RASQAL_EXPR_CAST:
      raptor_free_uri(e->name);
      rasqal_free_expression(e->arg1);
      break;

    case RASQAL_EXPR_VARSTAR:
      /* constants */
      break;
      
    case RASQAL_EXPR_UNKNOWN:
    default:
      RASQAL_FATAL2("Unknown operation %d", e->op);
  }
}


/**
 * rasqal_new_expression_from_expression:
 * @e: #rasqal_expression object to copy
 *
 * Copy Constructor - create a new #rasqal_expression object from an existing rasqal_expression object.
 * 
 * Return value: a new #rasqal_expression object or NULL on failure
 **/
rasqal_expression*
rasqal_new_expression_from_expression(rasqal_expression* e)
{
  if(!e)
    return NULL;
  
  e->usage++;
  return e;
}


/**
 * rasqal_free_expression:
 * @e: #rasqal_expression object
 * 
 * Destructor - destroy a #rasqal_expression object.
 *
 **/
void
rasqal_free_expression(rasqal_expression* e)
{
  RASQAL_ASSERT_OBJECT_POINTER_RETURN(e, rasqal_expression);
  
  if(--e->usage)
    return;

  rasqal_expression_clear(e);
  RASQAL_FREE(rasqal_expression, e);
}


/**
 * rasqal_expression_visit:
 * @e:  #rasqal_expression to visit
 * @fn: visit function
 * @user_data: user data to pass to visit function
 * 
 * Visit a user function over a #rasqal_expression
 * 
 * If the user function @fn returns 0, the visit is truncated.
 *
 * Return value: 0 if the visit was truncated.
 **/
int
rasqal_expression_visit(rasqal_expression* e, 
                        rasqal_expression_visit_fn fn,
                        void *user_data)
{
  int i;
  int result=0;

  /* This ordering allows fn to potentially edit 'e' in-place */
  result=fn(user_data, e);
  if(result)
    return result;

  switch(e->op) {
    case RASQAL_EXPR_AND:
    case RASQAL_EXPR_OR:
    case RASQAL_EXPR_EQ:
    case RASQAL_EXPR_NEQ:
    case RASQAL_EXPR_LT:
    case RASQAL_EXPR_GT:
    case RASQAL_EXPR_LE:
    case RASQAL_EXPR_GE:
    case RASQAL_EXPR_PLUS:
    case RASQAL_EXPR_MINUS:
    case RASQAL_EXPR_STAR:
    case RASQAL_EXPR_SLASH:
    case RASQAL_EXPR_REM:
    case RASQAL_EXPR_STR_EQ:
    case RASQAL_EXPR_STR_NEQ:
    case RASQAL_EXPR_LANGMATCHES:
    case RASQAL_EXPR_SAMETERM:
      return rasqal_expression_visit(e->arg1, fn, user_data) ||
             rasqal_expression_visit(e->arg2, fn, user_data);
      break;
    case RASQAL_EXPR_REGEX:
      return rasqal_expression_visit(e->arg1, fn, user_data) ||
             rasqal_expression_visit(e->arg2, fn, user_data) ||
             (e->arg3 && rasqal_expression_visit(e->arg3, fn, user_data));
      break;
    case RASQAL_EXPR_TILDE:
    case RASQAL_EXPR_BANG:
    case RASQAL_EXPR_UMINUS:
    case RASQAL_EXPR_BOUND:
    case RASQAL_EXPR_STR:
    case RASQAL_EXPR_LANG:
    case RASQAL_EXPR_DATATYPE:
    case RASQAL_EXPR_ISURI:
    case RASQAL_EXPR_ISBLANK:
    case RASQAL_EXPR_ISLITERAL:
    case RASQAL_EXPR_CAST:
    case RASQAL_EXPR_ORDER_COND_ASC:
    case RASQAL_EXPR_ORDER_COND_DESC:
    case RASQAL_EXPR_GROUP_COND_ASC:
    case RASQAL_EXPR_GROUP_COND_DESC:
    case RASQAL_EXPR_COUNT:
      return rasqal_expression_visit(e->arg1, fn, user_data);
      break;
    case RASQAL_EXPR_STR_MATCH:
    case RASQAL_EXPR_STR_NMATCH:
      return fn(user_data, e->arg1);
      break;
    case RASQAL_EXPR_LITERAL:
      return 0;
    case RASQAL_EXPR_FUNCTION:
      for(i=0; i<raptor_sequence_size(e->args); i++) {
        rasqal_expression* e2=(rasqal_expression*)raptor_sequence_get_at(e->args, i);
        if(!rasqal_expression_visit(e2, fn, user_data)) {
          result=0;
          break;
        }
      }
      return result;
      break;

    case RASQAL_EXPR_VARSTAR:
      /* constants */
      return 0;
      break;
      
    case RASQAL_EXPR_UNKNOWN:
    default:
      RASQAL_FATAL2("Unknown operation %d", e->op);
      return -1; /* keep some compilers happy */
  }
}


/* 
 * rasqal_language_matches:
 * @lang_tag: language tag such as "en" or "en-US" or "ab-cd-ef"
 * @lang_range: language range such as "*" (SPARQL) or "en" or "ab-cd"
 *
 * INTERNAL - Match a language tag against a language range
 *
 * Returns true if @lang_range matches @lang_tag per
 *   Matching of Language Tags [RFC4647] section 2.1
 * RFC4647 defines a case-insensitive, hierarchical matching
 * algorithm which operates on ISO-defined subtags for language and
 * country codes, and user defined subtags.
 *
 * (Note: RFC3066 section 2.5 matching is identical to
 * RFC4647 section 3.3.1 Basic Filtering )
 * 
 * In SPARQL, a language-range of "*" matches any non-empty @lang_tag string.
 * See http://www.w3.org/TR/2007/WD-rdf-sparql-query-20070326/#func-langMatches
 *
 * Return value: non-0 if true
 */
static int
rasqal_language_matches(const unsigned char* lang_tag,
                        const unsigned char* lang_range) 
{
  int b= 0;

  if(!(lang_tag && lang_range && *lang_tag && *lang_range)) {
    /* One of the arguments is NULL or the empty string */
    return 0;
  }

  /* Now have two non-empty arguments */

  /* Simple range string "*" matches anything excluding NULL/empty
   * lang_tag (checked above)
   */
  if(lang_range[0] == '*') {
    if(!lang_range[1])
      b = 1;
    return b;
  }
  
  while (1) {
    char tag_c   = tolower(*lang_tag++);
    char range_c = tolower(*lang_range++);
    if ((!tag_c && !range_c) || (!range_c && tag_c == '-')) {
      /* EITHER
       *   The end of both strings (thus everything previous matched
       *   such as e.g. tag "fr-CA" matching range "fr-ca")
       * OR
       *   The end of the range and end of the tag prefix (e.g. tag
       *   "en-US" matching range "en")
       * means a match
       */
      b = 1;
      break;
    } 
    if (range_c != tag_c) {
      /* If a difference was found - including one of the
       * strings being shorter than the other, it means no match
       * (b is set to 0 above)
       */
      break;
    }
  }

  return b;
}


/* 
 * rasqal_expression_evaluate_strmatch:
 * @query: #rasqal_query this expression belongs to
 * @e: The expression to evaluate.
 * @flags: Compare flags
 *
 * INTERNAL - Evaluate RASQAL_EXPR_STR_MATCH, RASQAL_EXPR_STR_NMATCH and
 * RASQAL_EXPR_REGEX expressions.
 *
 * Return value: A #rasqal_literal value or NULL on failure.
 */
static rasqal_literal*
rasqal_expression_evaluate_strmatch(rasqal_query *query, rasqal_expression* e,
                                    int flags)
{
  int b=0;
  int flag_i=0; /* flags contains i */
  const unsigned char *p;
  const unsigned char *match_string;
  const unsigned char *pattern;
  const unsigned char *regex_flags;
  rasqal_literal *l1, *l2, *l3;
  int error=0;
  int rc=0;
#ifdef RASQAL_REGEX_PCRE
  pcre* re;
  int options=0;
  const char *re_error=NULL;
  int erroffset=0;
#endif
#ifdef RASQAL_REGEX_POSIX
  regex_t reg;
  int options=REG_EXTENDED | REG_NOSUB;
#endif
    
  l1=rasqal_expression_evaluate(query, e->arg1, flags);
  if(!l1)
    goto failed;

  match_string=rasqal_literal_as_string_flags(l1, flags, &error);
  if(error || !match_string) {
    rasqal_free_literal(l1);
    goto failed;
  }
    
  l3=NULL;
  regex_flags=NULL;
  if(e->op == RASQAL_EXPR_REGEX) {
    l2=rasqal_expression_evaluate(query, e->arg2, flags);
    if(!l2) {
      rasqal_free_literal(l1);
      goto failed;
    }

    if(e->arg3) {
      l3=rasqal_expression_evaluate(query, e->arg3, flags);
      if(!l3) {
        rasqal_free_literal(l1);
        rasqal_free_literal(l2);
        goto failed;
      }
      regex_flags=l3->string;
    }
      
  } else {
    l2=e->literal;
    regex_flags=l2->flags;
  }
  pattern=l2->string;
    
  for(p=regex_flags; p && *p; p++)
    if(*p == 'i')
      flag_i++;
      
#ifdef RASQAL_REGEX_PCRE
  if(flag_i)
    options |= PCRE_CASELESS;
    
  re=pcre_compile((const char*)pattern, options, 
                  &re_error, &erroffset, NULL);
  if(!re) {
    query->failed=1;
    rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_ERROR,
                            &query->locator,
                            "Regex compile of '%s' failed - %s", pattern, re_error);
    rc= -1;
  } else {
    rc=pcre_exec(re, 
                 NULL, /* no study */
                 (const char*)match_string, strlen((const char*)match_string),
                 0 /* startoffset */,
                 0 /* options */,
                 NULL, 0 /* ovector, ovecsize - no matches wanted */
                 );
    if(rc >= 0)
      b=1;
    else if(rc != PCRE_ERROR_NOMATCH) {
      query->failed=1;
      rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_ERROR,
                              &query->locator,
                              "Regex match failed - returned code %d", rc);
      rc= -1;
    } else
      rc=0;
  }
  pcre_free(re);
  
#endif
    
#ifdef RASQAL_REGEX_POSIX
  if(flag_i)
    options |=REG_ICASE;
    
  rc=regcomp(&reg, (const char*)pattern, options);
  if(rc) {
    query->failed=1;
    rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_ERROR,
                            &query->locator,
                            "Regex compile of '%s' failed", pattern);
    rc= -1;
  } else {
    rc=regexec(&reg, (const char*)match_string, 
               0, NULL, /* nmatch, regmatch_t pmatch[] - no matches wanted */
               0 /* eflags */
               );
    if(!rc)
      b=1;
    else if (rc != REG_NOMATCH) {
      query->failed=1;
      rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_ERROR,
                              &query->locator,
                              "Regex match failed - returned code %d", rc);
      rc= -1;
    } else
      rc= 0;
  }
  regfree(&reg);
#endif

#ifdef RASQAL_REGEX_NONE
  rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                          &query->locator,
                          "Regex support missing, cannot compare '%s' to '%s'", match_string, pattern);
  b=1;
  rc= -1;
#endif

  RASQAL_DEBUG5("regex match returned %s for '%s' against '%s' (flags=%s)\n", b ? "true" : "false", match_string, pattern, l2->flags ? (char*)l2->flags : "");
  
  if(e->op == RASQAL_EXPR_STR_NMATCH)
    b=1-b;

  rasqal_free_literal(l1);
  if(e->op == RASQAL_EXPR_REGEX) {
    rasqal_free_literal(l2);
    if(l3)
      rasqal_free_literal(l3);
  }
    
  if(rc<0)
    goto failed;
    
  return rasqal_new_boolean_literal(query->world, b);

  failed:
  return NULL;
}

/**
 * rasqal_expression_evaluate:
 * @query: #rasqal_query this expression belongs to
 * @e: The expression to evaluate.
 * @flags: Flags for rasqal_literal_compare() and RASQAL_COMPARE_NOCASE for string matches.
 * 
 * Evaluate a #rasqal_expression tree to give a #rasqal_literal result
 * or error.
 * 
 * Return value: a #rasqal_literal value or NULL on failure.
 **/
rasqal_literal*
rasqal_expression_evaluate(rasqal_query *query, rasqal_expression* e,
                           int flags)
{
  rasqal_literal* result=NULL;

  rasqal_literal *l1;
  rasqal_literal *l2;
  const unsigned char *s;

  /* pack vars from different switch cases in unions to save some stack space */
  union {
    struct { int e1; int e2; } errs;
    struct { int dummy_do_not_mask_e; int free_literal; } flags;
    int e;
  } errs;
  union {
    struct { int b1; int b2; } bools;
    int b;
    int i;
    raptor_uri *dt_uri;
    const unsigned char *s;
    unsigned char *new_s;
    rasqal_variable *v;
  } vars;

  errs.e=0;

#ifdef RASQAL_DEBUG
  RASQAL_DEBUG2("evaluating expression %p: ", e);
  rasqal_expression_print(e, stderr);
  fprintf(stderr, "\n");
#endif
  
  switch(e->op) {
    case RASQAL_EXPR_AND:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1) {
        errs.errs.e1=1;
        vars.bools.b1=0;
      } else {
        errs.errs.e1=0;
        vars.bools.b1=rasqal_literal_as_boolean(l1, &errs.errs.e1);
        rasqal_free_literal(l1);
      }

      l1=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l1) {
        errs.errs.e2=1;
        vars.bools.b2=0;
      } else {
        errs.errs.e2=0;
        vars.bools.b2=rasqal_literal_as_boolean(l1, &errs.errs.e2);
        rasqal_free_literal(l1);
      }

      /* See http://www.w3.org/TR/2005/WD-rdf-sparql-query-20051123/#truthTable */
      if(!errs.errs.e1 && !errs.errs.e2) {
        /* No type error, answer is A && B */
        vars.b = vars.bools.b1 && vars.bools.b2; /* don't need b1,b2 anymore */
      } else {
        if((!vars.bools.b1 && errs.errs.e2) || (errs.errs.e1 && vars.bools.b2))
          /* F && E => F.   E && F => F. */
          vars.b=0;
        else
          /* Otherwise E */
          goto failed;
      }
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;
      
    case RASQAL_EXPR_OR:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1) {
        errs.errs.e1=1;
        vars.bools.b1=0;
      } else {
        errs.errs.e1=0;
        vars.bools.b1=rasqal_literal_as_boolean(l1, &errs.errs.e1);
        rasqal_free_literal(l1);
      }

      l1=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l1) {
        errs.errs.e2=1;
        vars.bools.b2=0;
      } else {
        errs.errs.e2=0;
        vars.bools.b2=rasqal_literal_as_boolean(l1, &errs.errs.e2);
        rasqal_free_literal(l1);
      }

      /* See http://www.w3.org/TR/2005/WD-rdf-sparql-query-20051123/#truthTable */
      if(!errs.errs.e1 && !errs.errs.e2) {
        /* No type error, answer is A || B */
        vars.b = vars.bools.b1 || vars.bools.b2; /* don't need b1,b2 anymore */
      } else {
        if((vars.bools.b1 && errs.errs.e2) || (errs.errs.e1 && vars.bools.b2))
          /* T || E => T.   E || T => T */
          vars.b=1;
        else
          /* Otherwise E */
          goto failed;
      }
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_EQ:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_equals_flags(l1, l2, flags, &errs.e) != 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_NEQ:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_equals_flags(l1, l2, flags, &errs.e) == 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_LT:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_compare(l1, l2, flags, &errs.e) < 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_GT:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_compare(l1, l2, flags, &errs.e) > 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_LE:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_compare(l1, l2, flags, &errs.e) <= 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;        

    case RASQAL_EXPR_GE:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_compare(l1, l2, flags, &errs.e) >= 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_UMINUS:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      result=rasqal_literal_negate(l1, &errs.e);
      rasqal_free_literal(l1);
      if(errs.e)
        goto failed;
      break;

    case RASQAL_EXPR_BOUND:
      /* Do not use rasqal_expression_evaluate() here since
       * we need to check the argument is a variable, and
       * that function will flatten such thing to literals
       * as early as possible. See (FLATTEN_LITERAL) below
       */
      if(!e->arg1 || e->arg1->op != RASQAL_EXPR_LITERAL)
        goto failed;

      l1=e->arg1->literal;
      if(!l1 || l1->type != RASQAL_LITERAL_VARIABLE)
        goto failed;

      vars.v=rasqal_literal_as_variable(l1);
      if(!vars.v)
        goto failed;

      result=rasqal_new_boolean_literal(query->world, (vars.v->value != NULL));
      break;

    case RASQAL_EXPR_STR:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      /* Note: flags removes RASQAL_COMPARE_XQUERY as this is the
       * explicit stringify operation
       */
      s=rasqal_literal_as_string_flags(l1, (flags & ~RASQAL_COMPARE_XQUERY),
                                       &errs.e);
      if(!s || errs.e) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.new_s=(unsigned char *)RASQAL_MALLOC(cstring, strlen((const char*)s)+1);
      if(!vars.new_s) {
        rasqal_free_literal(l1);
        goto failed;
      }
      strcpy((char*)vars.new_s, (const char*)s);

      result=rasqal_new_string_literal(query->world, vars.new_s, NULL, NULL, NULL);
      rasqal_free_literal(l1);

      break;
      
    case RASQAL_EXPR_LANG:
      errs.flags.free_literal=1;
      
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      vars.v=rasqal_literal_as_variable(l1);
      if(vars.v) {
        rasqal_free_literal(l1);
        l1=vars.v->value; /* don't need vars.v after this */
        errs.flags.free_literal=0;
        if(!l1)
          goto failed;
      }

      if(rasqal_literal_get_rdf_term_type(l1) != RASQAL_LITERAL_STRING) {
        if(errs.flags.free_literal)
          rasqal_free_literal(l1);
        goto failed;
      }

      if(l1->language) {
        vars.new_s=(unsigned char*)RASQAL_MALLOC(cstring,
                                                 strlen(l1->language)+1);
        if(!vars.new_s) {
          if(errs.flags.free_literal)
            rasqal_free_literal(l1);
          goto failed;
        }
        strcpy((char*)vars.new_s, l1->language);
      } else  {
        vars.new_s=(unsigned char*)RASQAL_MALLOC(cstring, 1);
        if(!vars.new_s) {
          if(errs.flags.free_literal)
            rasqal_free_literal(l1);
          goto failed;
        }
        *vars.new_s='\0';
      }
      result=rasqal_new_string_literal(query->world, vars.new_s, NULL, NULL, NULL);
      
      if(errs.flags.free_literal)
        rasqal_free_literal(l1);

      break;

    case RASQAL_EXPR_LANGMATCHES:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      s=rasqal_literal_as_string_flags(l1, flags, &errs.e);
      vars.s=rasqal_literal_as_string_flags(l2, flags, &errs.e);

      if(errs.e)
        vars.b=0;
      else
        vars.b=rasqal_language_matches(s, vars.s); /* don't need s anymore */
      
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_DATATYPE:
      errs.flags.free_literal=1;
      vars.dt_uri=NULL;
      
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      vars.v=rasqal_literal_as_variable(l1);
      if(vars.v) {
        rasqal_free_literal(l1);
        l1=vars.v->value; /* don't need vars.v after this */
        errs.flags.free_literal=0;
        if(!l1)
          goto failed;
      }

      if(rasqal_literal_get_rdf_term_type(l1) != RASQAL_LITERAL_STRING) {
        if(errs.flags.free_literal)
          rasqal_free_literal(l1);
        goto failed;
      }

      if(l1->language) {
        if(errs.flags.free_literal)
          rasqal_free_literal(l1);
        goto failed;
      }

      /* The datatype of a plain literal is xsd:string */
      vars.dt_uri=l1->datatype;
      if(!vars.dt_uri && l1->type == RASQAL_LITERAL_STRING)
        vars.dt_uri=rasqal_xsd_datatype_type_to_uri(l1->world, l1->type);

      if(!vars.dt_uri) {
        if(errs.flags.free_literal)
          rasqal_free_literal(l1);
        goto failed;
      }
      
      result=rasqal_new_uri_literal(query->world, raptor_uri_copy(vars.dt_uri));

      if(errs.flags.free_literal)
        rasqal_free_literal(l1);

      break;

    case RASQAL_EXPR_ISURI:
      errs.flags.free_literal=1;
      
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;
      
      vars.v=rasqal_literal_as_variable(l1);
      if(vars.v) {
        rasqal_free_literal(l1);
        l1=vars.v->value; /* don't need vars.v after this */
        errs.flags.free_literal=0;
        if(!l1)
          goto failed;
      }

      vars.b=(l1->type == RASQAL_LITERAL_URI);
      
      if(errs.flags.free_literal)
        rasqal_free_literal(l1);

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_ISBLANK:
      errs.flags.free_literal=1;
      
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;
      
      vars.v=rasqal_literal_as_variable(l1);
      if(vars.v) {
        rasqal_free_literal(l1);
        l1=vars.v->value; /* don't need vars.v after this */
        errs.flags.free_literal=0;
        if(!l1)
          goto failed;
      }

      vars.b=(l1->type == RASQAL_LITERAL_BLANK);

      if(errs.flags.free_literal)
        rasqal_free_literal(l1);

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_ISLITERAL:
      errs.flags.free_literal=1;
      
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;
      
      vars.v=rasqal_literal_as_variable(l1);
      if(vars.v) {
        rasqal_free_literal(l1);
        l1=vars.v->value; /* don't need vars.v after this */
        errs.flags.free_literal=0;
        if(!l1)
          goto failed;
      }

      vars.b=(rasqal_literal_get_rdf_term_type(l1) == RASQAL_LITERAL_STRING);

      if(errs.flags.free_literal)
        rasqal_free_literal(l1);

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;
      
    case RASQAL_EXPR_PLUS:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      result=rasqal_literal_add(l1, l2, &errs.e);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      
      break;
        
    case RASQAL_EXPR_MINUS:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      result=rasqal_literal_subtract(l1, l2, &errs.e);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      
      break;
      
    case RASQAL_EXPR_STAR:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      result=rasqal_literal_multiply(l1, l2, &errs.e);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      
      break;
      
    case RASQAL_EXPR_SLASH:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      result=rasqal_literal_divide(l1, l2, &errs.e);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;
      
      break;
      
    case RASQAL_EXPR_REM:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.i=rasqal_literal_as_integer(l2, &errs.errs.e2);
      /* error if divisor is zero */
      if(!vars.i)
        errs.errs.e2=1;
      else
        vars.i=rasqal_literal_as_integer(l1, &errs.errs.e1) % vars.i;

      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.errs.e1 || errs.errs.e2)
        goto failed;

      result=rasqal_new_integer_literal(query->world, RASQAL_LITERAL_INTEGER, vars.i);
      break;
      
    case RASQAL_EXPR_STR_EQ:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_compare(l1, l2, flags | RASQAL_COMPARE_NOCASE,
                                     &errs.e) == 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;
      
    case RASQAL_EXPR_STR_NEQ:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=(rasqal_literal_compare(l1, l2, flags | RASQAL_COMPARE_NOCASE, 
                                     &errs.e) != 0);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_TILDE:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      vars.i= ~ rasqal_literal_as_integer(l1, &errs.e);
      rasqal_free_literal(l1);
      if(errs.e)
        goto failed;

      result=rasqal_new_integer_literal(query->world, RASQAL_LITERAL_INTEGER, vars.i);
      break;

    case RASQAL_EXPR_BANG:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      vars.b= ! rasqal_literal_as_boolean(l1, &errs.e);
      rasqal_free_literal(l1);
      if(errs.e)
        goto failed;

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;

    case RASQAL_EXPR_STR_MATCH:
    case RASQAL_EXPR_STR_NMATCH:
    case RASQAL_EXPR_REGEX:
      result=rasqal_expression_evaluate_strmatch(query, e, flags);
      break;

    case RASQAL_EXPR_LITERAL:
      /* flatten any literal to a value as soon as possible - this
       * removes variables from expressions the first time they are seen.
       * (FLATTEN_LITERAL)
       */
      result=rasqal_new_literal_from_literal(rasqal_literal_value(e->literal));
      break;

    case RASQAL_EXPR_FUNCTION:
      rasqal_log_error_simple(query->world, RAPTOR_LOG_LEVEL_WARNING,
                              &query->locator,
                              "No function expressions support at present.  Returning false.");
      result=rasqal_new_boolean_literal(query->world, 0);
      break;
      
    case RASQAL_EXPR_CAST:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;

      result=rasqal_literal_cast(l1, e->name, flags, &errs.e);

      rasqal_free_literal(l1);
      if(errs.e)
        goto failed;

      break;

    case RASQAL_EXPR_ORDER_COND_ASC:
    case RASQAL_EXPR_ORDER_COND_DESC:
    case RASQAL_EXPR_GROUP_COND_ASC:
    case RASQAL_EXPR_GROUP_COND_DESC:
    case RASQAL_EXPR_COUNT:
      result=rasqal_expression_evaluate(query, e->arg1, flags);
      break;

    case RASQAL_EXPR_VARSTAR:
      /* constants */
      break;
      
    case RASQAL_EXPR_SAMETERM:
      l1=rasqal_expression_evaluate(query, e->arg1, flags);
      if(!l1)
        goto failed;
      
      l2=rasqal_expression_evaluate(query, e->arg2, flags);
      if(!l2) {
        rasqal_free_literal(l1);
        goto failed;
      }

      vars.b=rasqal_literal_equals_flags(l1, l2, RASQAL_COMPARE_RDF, &errs.e);
      rasqal_free_literal(l1);
      rasqal_free_literal(l2);
      if(errs.e)
        goto failed;

      result=rasqal_new_boolean_literal(query->world, vars.b);
      break;
      
    case RASQAL_EXPR_UNKNOWN:
    default:
      RASQAL_FATAL2("Unknown operation %d", e->op);
  }

  got_result:

#ifdef RASQAL_DEBUG
  RASQAL_DEBUG2("result of %p: ", e);
  rasqal_expression_print(e, stderr);
  fputs( ": ", stderr);
  if(result)
    rasqal_literal_print(result, stderr);
  else
    fputs("(NULL)",stderr);
  fputc('\n', stderr);
#endif
  
  return result;

  failed:

  if(result) {
    rasqal_free_literal(result);
    result=NULL;
  }
  goto got_result;
}


static const char* const rasqal_op_labels[RASQAL_EXPR_LAST+1]={
  "UNKNOWN",
  "and",
  "or",
  "eq",
  "neq",
  "lt",
  "gt",
  "le",
  "ge",
  "uminus",
  "plus",
  "minus",
  "star",
  "slash",
  "rem",
  "str_eq",
  "str_ne",
  "str_match",
  "str_nmatch",
  "tilde",
  "bang",
  "literal",
  "function",
  "bound",
  "str",
  "lang",
  "datatype",
  "isUri",
  "isBlank",
  "isLiteral",
  "cast",
  "order asc",
  "order desc",
  "langMatches",
  "regex",
  "group asc",
  "group desc",
  "count",
  "varstar",
  "sameTerm"
};


/**
 * rasqal_expression_print_op:
 * @e: the #rasqal_expression object
 * @fh: the #FILE* handle to print to
 * 
 * Print a rasqal expression operator in a debug format.
 *
 * The print debug format may change in any release.
 **/
void
rasqal_expression_print_op(rasqal_expression* e, FILE* fh)
{
  rasqal_op op=e->op;
  if(op > RASQAL_EXPR_LAST)
    op=RASQAL_EXPR_UNKNOWN;
  fputs(rasqal_op_labels[(int)op], fh);
}


/**
 * rasqal_expression_print:
 * @e: #rasqal_expression object.
 * @fh: The #FILE* handle to print to.
 * 
 * Print a Rasqal expression in a debug format.
 * 
 * The print debug format may change in any release.
 **/
void
rasqal_expression_print(rasqal_expression* e, FILE* fh)
{
  fputs("expr(", fh);
  switch(e->op) {
    case RASQAL_EXPR_AND:
    case RASQAL_EXPR_OR:
    case RASQAL_EXPR_EQ:
    case RASQAL_EXPR_NEQ:
    case RASQAL_EXPR_LT:
    case RASQAL_EXPR_GT:
    case RASQAL_EXPR_LE:
    case RASQAL_EXPR_GE:
    case RASQAL_EXPR_PLUS:
    case RASQAL_EXPR_MINUS:
    case RASQAL_EXPR_STAR:
    case RASQAL_EXPR_SLASH:
    case RASQAL_EXPR_REM:
    case RASQAL_EXPR_STR_EQ:
    case RASQAL_EXPR_STR_NEQ:
    case RASQAL_EXPR_LANGMATCHES:
    case RASQAL_EXPR_REGEX:
    case RASQAL_EXPR_SAMETERM:
      fputs("op ", fh);
      rasqal_expression_print_op(e, fh);
      fputc('(', fh);
      rasqal_expression_print(e->arg1, fh);
      fputs(", ", fh);
      rasqal_expression_print(e->arg2, fh);
      /* There is only one 3-op expression and it's handled here */
      if(e->op == RASQAL_EXPR_REGEX && e->arg3) {
        fputs(", ", fh);
        rasqal_expression_print(e->arg3, fh);
      }
      fputc(')', fh);
      break;
    case RASQAL_EXPR_STR_MATCH:
    case RASQAL_EXPR_STR_NMATCH:
      fputs("op ", fh);
      rasqal_expression_print_op(e, fh);
      fputc('(', fh);
      rasqal_expression_print(e->arg1, fh);
      fputs(", ", fh);
      rasqal_literal_print(e->literal, fh);
      fputc(')', fh);
      break;
    case RASQAL_EXPR_TILDE:
    case RASQAL_EXPR_BANG:
    case RASQAL_EXPR_UMINUS:
    case RASQAL_EXPR_BOUND:
    case RASQAL_EXPR_STR:
    case RASQAL_EXPR_LANG:
    case RASQAL_EXPR_DATATYPE:
    case RASQAL_EXPR_ISURI:
    case RASQAL_EXPR_ISBLANK:
    case RASQAL_EXPR_ISLITERAL:
    case RASQAL_EXPR_ORDER_COND_ASC:
    case RASQAL_EXPR_ORDER_COND_DESC:
    case RASQAL_EXPR_GROUP_COND_ASC:
    case RASQAL_EXPR_GROUP_COND_DESC:
    case RASQAL_EXPR_COUNT:
      fputs("op ", fh);
      rasqal_expression_print_op(e, fh);
      fputc('(', fh);
      rasqal_expression_print(e->arg1, fh);
      fputc(')', fh);
      break;

    case RASQAL_EXPR_LITERAL:
      rasqal_literal_print(e->literal, fh);
      break;

    case RASQAL_EXPR_FUNCTION:
      fputs("function(uri=", fh);
      raptor_uri_print(e->name, fh);
      fputs(", args=", fh);
      raptor_sequence_print(e->args, fh);
      fputc(')', fh);
      break;

    case RASQAL_EXPR_CAST:
      fputs("cast(type=", fh);
      raptor_uri_print(e->name, fh);
      fputs(", value=", fh);
      rasqal_expression_print(e->arg1, fh);
      fputc(')', fh);
      break;

    case RASQAL_EXPR_VARSTAR:
      fputs("varstar", fh);
      break;
      
    case RASQAL_EXPR_UNKNOWN:
    default:
      RASQAL_FATAL2("Unknown operation %d", e->op);
  }
  fputc(')', fh);
}


/* for use with rasqal_expression_visit and user_data=rasqal_query */
int
rasqal_expression_has_qname(void *user_data, rasqal_expression *e)
{
  if(e->op == RASQAL_EXPR_LITERAL)
    return rasqal_literal_has_qname(e->literal);

  return 0;
}


/* for use with rasqal_expression_visit and user_data=rasqal_query */
int
rasqal_expression_expand_qname(void *user_data, rasqal_expression *e)
{
  if(e->op == RASQAL_EXPR_LITERAL)
    return rasqal_literal_expand_qname(user_data, e->literal);

  return 0;
}


int
rasqal_expression_is_constant(rasqal_expression* e) 
{
  int i;
  int result=0;
  
  switch(e->op) {
    case RASQAL_EXPR_AND:
    case RASQAL_EXPR_OR:
    case RASQAL_EXPR_EQ:
    case RASQAL_EXPR_NEQ:
    case RASQAL_EXPR_LT:
    case RASQAL_EXPR_GT:
    case RASQAL_EXPR_LE:
    case RASQAL_EXPR_GE:
    case RASQAL_EXPR_PLUS:
    case RASQAL_EXPR_MINUS:
    case RASQAL_EXPR_STAR:
    case RASQAL_EXPR_SLASH:
    case RASQAL_EXPR_REM:
    case RASQAL_EXPR_STR_EQ:
    case RASQAL_EXPR_STR_NEQ:
    case RASQAL_EXPR_LANGMATCHES:
    case RASQAL_EXPR_SAMETERM:
      result=rasqal_expression_is_constant(e->arg1) &&
             rasqal_expression_is_constant(e->arg2);
      break;
    case RASQAL_EXPR_REGEX:
      result=rasqal_expression_is_constant(e->arg1) &&
             rasqal_expression_is_constant(e->arg2) &&
             (e->arg3 && rasqal_expression_is_constant(e->arg3));
      break;
    case RASQAL_EXPR_STR_MATCH:
    case RASQAL_EXPR_STR_NMATCH:
      result=rasqal_expression_is_constant(e->arg1) &&
             rasqal_literal_is_constant(e->literal);
      break;
    case RASQAL_EXPR_TILDE:
    case RASQAL_EXPR_BANG:
    case RASQAL_EXPR_UMINUS:
    case RASQAL_EXPR_BOUND:
    case RASQAL_EXPR_STR:
    case RASQAL_EXPR_LANG:
    case RASQAL_EXPR_DATATYPE:
    case RASQAL_EXPR_ISURI:
    case RASQAL_EXPR_ISBLANK:
    case RASQAL_EXPR_ISLITERAL:
    case RASQAL_EXPR_ORDER_COND_ASC:
    case RASQAL_EXPR_ORDER_COND_DESC:
    case RASQAL_EXPR_GROUP_COND_ASC:
    case RASQAL_EXPR_GROUP_COND_DESC:
    case RASQAL_EXPR_COUNT:
      result=rasqal_expression_is_constant(e->arg1);
      break;

    case RASQAL_EXPR_LITERAL:
      result=rasqal_literal_is_constant(e->literal);
      break;

    case RASQAL_EXPR_FUNCTION:
      result=1;
      for(i=0; i<raptor_sequence_size(e->args); i++) {
        rasqal_expression* e2=(rasqal_expression*)raptor_sequence_get_at(e->args, i);
        if(!rasqal_expression_is_constant(e2)) {
          result=0;
          break;
        }
      }
      break;

    case RASQAL_EXPR_CAST:
      result=rasqal_expression_is_constant(e->arg1);
      break;

    case RASQAL_EXPR_VARSTAR:
      result=0;
      break;
      
    case RASQAL_EXPR_UNKNOWN:
    default:
      RASQAL_FATAL2("Unknown operation %d", e->op);
  }
  
  return result;
}


void
rasqal_expression_convert_to_literal(rasqal_expression* e, rasqal_literal* l)
{
  int usage=e->usage;

  /* update expression 'e' in place */
  rasqal_expression_clear(e);

  memset(e, 0, sizeof(rasqal_expression));
  e->usage=usage;
  e->op=RASQAL_EXPR_LITERAL;
  e->literal=l;
}

  


/* for use with rasqal_expression_visit and user_data=rasqal_query */
static int
rasqal_expression_has_variable(void *user_data, rasqal_expression *e)
{
  rasqal_variable* v;
  const unsigned char* name=((rasqal_variable*)user_data)->name;

  if(e->op != RASQAL_EXPR_LITERAL)
    return 0;
  
  v=rasqal_literal_as_variable(e->literal);
  if(!v)
    return 0;
  
  if(!strcmp((const char*)v->name, (const char*)name))
    return 1;

  return 0;
}


int
rasqal_expression_mentions_variable(rasqal_expression* e, rasqal_variable* v)
{
  return rasqal_expression_visit(e, rasqal_expression_has_variable, v);
}


#endif /* not STANDALONE */




#ifdef STANDALONE
#include <stdio.h>

int main(int argc, char *argv[]);


#define assert_match(function, result, string) do { if(strcmp(result, string)) { fprintf(stderr, #function " failed - returned %s, expected %s\n", result, string); exit(1); } } while(0)


int
main(int argc, char *argv[]) 
{
  const char *program=rasqal_basename(argv[0]);
  rasqal_literal *lit1, *lit2;
  rasqal_expression *expr1, *expr2;
  rasqal_expression* expr;
  rasqal_literal* result;
  int error=0;
  rasqal_world *world;

  raptor_init();

  /* FIXME: hack to make a world object to initialise */
  world=(rasqal_world*)RASQAL_CALLOC(rasqal_world, sizeof(rasqal_world), 1);
  rasqal_uri_init(world);

  rasqal_xsd_init(world);
  
  lit1=rasqal_new_integer_literal(world, RASQAL_LITERAL_INTEGER, 1);
  expr1=rasqal_new_literal_expression(lit1);
  lit2=rasqal_new_integer_literal(world, RASQAL_LITERAL_INTEGER, 1);
  expr2=rasqal_new_literal_expression(lit2);
  expr=rasqal_new_2op_expression(RASQAL_EXPR_PLUS, expr1, expr2);

  fprintf(stderr, "%s: expression: ", program);
  rasqal_expression_print(expr, stderr);
  fputc('\n', stderr);

  result=rasqal_expression_evaluate(NULL, expr, 0);

  if(result) {
    int bresult;
    
    fprintf(stderr, "%s: expression result: \n", program);
    rasqal_literal_print(result, stderr);
    fputc('\n', stderr);
    bresult=rasqal_literal_as_boolean(result, &error);
    if(error) {
      fprintf(stderr, "%s: boolean expression FAILED\n", program);
    } else
      fprintf(stderr, "%s: boolean expression result: %d\n", program, bresult);


  } else
    fprintf(stderr, "%s: expression evaluation FAILED with error\n", program);

  rasqal_free_expression(expr);

  if(result)
    rasqal_free_literal(result);

  rasqal_xsd_finish(world);

  rasqal_uri_finish(world);
  
  RASQAL_FREE(rasqal_world, world);

  raptor_finish();

  return error;
}
#endif
