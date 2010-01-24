/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * turtle_lexer.l - Raptor Turtle lexer - making tokens for turtle grammar generator
 *
 * Copyright (C) 2003-2006, David Beckett http://purl.org/net/dajobe/
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
 * Turtle is defined in http://www.dajobe.org/2004/01/turtle/
 *
 * To generate the C files from this source, rather than use the
 * shipped turtle_lexer.c/.h needs a patched version of flex 2.5.31 such
 * as the one available in Debian GNU/Linux.   Details below
 * near the %option descriptions.
 *
 */


/* recognise 8-bits */
%option 8bit
%option warn nodefault

/* all symbols prefixed by this */
%option prefix="turtle_lexer_"

/* This is not needed, flex is invoked -oturtle_lexer.c */
/* %option outfile="turtle_lexer.c" */

/* Emit a C header file for prototypes
 * Only available in flex 2.5.13 or newer.
 * It was renamed to header-file in flex 2.5.19
 */
%option header-file="turtle_lexer.h"

/* Do not emit #include <unistd.h>
 * Only available in flex 2.5.7 or newer.
 * Broken in flex 2.5.31 without patches.
 */
%option nounistd

/* Never interactive */
/*  No isatty() check */
%option never-interactive

/* Batch scanner */
%option batch

/* Never use yyunput */
%option nounput

/* Supply our own alloc/realloc/free functions */
%option noyyalloc noyyrealloc noyyfree

/* Re-entrant scanner */
%option reentrant


  /* definitions */

%{

/* NOTE: These headers are NOT included here but are inserted by
 * fix-flex since otherwise it appears far too late in the generated C
 */

/*
#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif
*/

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

#include <raptor.h>
#include <raptor_internal.h>

#include <turtle_parser.h>

#include <turtle_common.h>


/* Prototypes */ 
static unsigned char *turtle_copy_token(unsigned char *text, size_t len);
static unsigned char *turtle_copy_string_token(raptor_parser* rdf_parser, unsigned char *text, size_t len, int delim);
void turtle_lexer_syntax_error(void* ctx, const char *message, ...) RAPTOR_PRINTF_FORMAT(2, 3);

#ifdef RAPTOR_DEBUG
const char * turtle_token_print(int token, YYSTYPE *lval);
#endif

int turtle_lexer_lex (YYSTYPE *turtle_parser_lval, yyscan_t yyscanner);
#define YY_DECL int turtle_lexer_lex (YYSTYPE *turtle_parser_lval, yyscan_t yyscanner)

#ifdef __cplusplus
#define INPUT_FN yyinput
#else
#define INPUT_FN input
#endif


/* Missing turtle_lexer.c/h prototypes */
int turtle_lexer_get_column(yyscan_t yyscanner);
void turtle_lexer_set_column(int  column_no , yyscan_t yyscanner);

static void turtle_lexer_cleanup(yyscan_t yyscanner);

/* fatal error handler declaration */
#define YY_FATAL_ERROR(msg) turtle_lexer_fatal_error(msg, yyscanner)
static void turtle_lexer_fatal_error(yyconst char *msg, yyscan_t yyscanner);

/* Out-of-memory reporting macro.
 * Do not report as fatal error - we don't want an abort(),
 * instead return EOF with yyterminate() and let the parser clean up.
 */
#define TURTLE_LEXER_OOM() do { \
  turtle_syntax_error(rdf_parser, turtle_lexer_oom_text); \
  yyterminate(); \
} while(0)

static const char turtle_lexer_oom_text[]="turtle_lexer: Out of memory";

%}

/* from SPARQL */
LANGUAGETOKEN [A-Za-z][-A-Z_a-z0-9]*
NCCHAR1 [A-Za-z\\\x80-\xff]
NCCHAR {NCCHAR1}|"-"|"_"|[0-9]
NCNAME_PREFIX {NCCHAR1}(({NCCHAR}|".")*{NCCHAR})?
NCNAME ("_"|{NCCHAR1})(({NCCHAR}|".")*{NCCHAR})?
QNAME {NCNAME_PREFIX}?":"{NCNAME}?
BNAME "_:"{NCNAME}

/* similar to SPARQL but no need for <= check here */
QUOTEDURI \<[^\>]*\>

DECIMAL [0-9]+"."[0-9]*|"."[0-9]+
DOUBLE [0-9]+"."[0-9]*{EXPONENT}|"."([0-9])+{EXPONENT}|([0-9])+{EXPONENT}
EXPONENT [eE][+-]?[0-9]+


%x PREF LITERAL


%%
  /* rules */

  raptor_parser *rdf_parser=(raptor_parser*)yyextra;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

\r\n|\r|\n   { turtle_parser->lineno++; }
 
[\ \t\v]+   { /* empty */ }


"a" { return A; }

"."       { return DOT; } 
","       { return COMMA; } 
";"       { return SEMICOLON; }
"["       { return LEFT_SQUARE; }
"]"       { return RIGHT_SQUARE; }
"@prefix" { BEGIN(PREF); return PREFIX; }
"@base"   { return BASE; }
"@"       { return AT; }
"^^"      { return HAT; }
"("       { return LEFT_ROUND; }
")"       { return RIGHT_ROUND; }
"{"       { return LEFT_CURLY; }
"}"       { return RIGHT_CURLY; }
":-"      { return COLONMINUS; }
"true"    { return TRUE_TOKEN; }
"false"   { return FALSE_TOKEN; }


\"([^"\\\n\r]|\\[^\n\r])*\"   { turtle_parser_lval->string=turtle_copy_string_token(rdf_parser, (unsigned char*)yytext+1, yyleng-2, '"'); /* ' */
                                if(!turtle_parser_lval->string)
                                  TURTLE_LEXER_OOM();
                                return STRING_LITERAL; }

\"\"\"				{ BEGIN(LITERAL); 
                                  turtle_parser->sb=raptor_new_stringbuffer();
                                  if(!turtle_parser->sb)
                                    TURTLE_LEXER_OOM();
                          }

<LITERAL>\"\"\"			{
		  size_t len;
     
		  BEGIN(INITIAL);
                  len=raptor_stringbuffer_length(turtle_parser->sb);
                  turtle_parser_lval->string=(unsigned char *)RAPTOR_MALLOC(cstring, len+1);
                  if(!turtle_parser_lval->string)
                    TURTLE_LEXER_OOM();
                  raptor_stringbuffer_copy_to_string(turtle_parser->sb, (unsigned char*)turtle_parser_lval->string, len);
                  turtle_parser_lval->string[len]='\0';

                  raptor_free_stringbuffer(turtle_parser->sb);
                  turtle_parser->sb=NULL;
                  return STRING_LITERAL; }

<LITERAL>\"|(\\[^uU]|\\u....|\\U........|[^\"]|\n)*	{
		  if (*yytext == EOF) {
                    BEGIN(INITIAL);
                    turtle_syntax_error(rdf_parser, "End of file in middle of literal");
                    raptor_free_stringbuffer(turtle_parser->sb);
                    turtle_parser->sb=NULL;
                    return EOF;
                  }

                  if(raptor_stringbuffer_append_turtle_string(turtle_parser->sb, (unsigned char*)yytext, yyleng, '"', (raptor_simple_message_handler)turtle_lexer_syntax_error, rdf_parser)) { /* " */
                    BEGIN(INITIAL);
                    raptor_free_stringbuffer(turtle_parser->sb);
                    turtle_parser->sb=NULL;
                    TURTLE_LEXER_OOM();
                  }
                  
   }

{BNAME}	{ turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext+2, yyleng-2);
          if(!turtle_parser_lval->string)
            TURTLE_LEXER_OOM();
                          return BLANK_LITERAL; }

{QNAME}	{ turtle_parser_lval->uri=turtle_qname_to_uri(rdf_parser, (unsigned char*)yytext, yyleng);
          if(!turtle_parser_lval->uri)
            TURTLE_LEXER_OOM();
                          return QNAME_LITERAL; }

[-+]?{DECIMAL}	{ turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext, yyleng);
                  if(!turtle_parser_lval->string)
                    TURTLE_LEXER_OOM();
                        return DECIMAL_LITERAL;
}

[-+]?{DOUBLE} { turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext, yyleng);
                  if(!turtle_parser_lval->string)
                    TURTLE_LEXER_OOM();
                        return FLOATING_LITERAL;
}

[-+]?[0-9]+        { turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext, yyleng);
                  if(!turtle_parser_lval->string)
                    TURTLE_LEXER_OOM();
                          return INTEGER_LITERAL; }

<PREF>[\ \t\v]+ { /* eat up leading whitespace */ }
<PREF>{NCNAME_PREFIX}":"	{ turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext, yyleng);
                            if(!turtle_parser_lval->string)
                              TURTLE_LEXER_OOM();
                          BEGIN(INITIAL);
                          return IDENTIFIER; }
<PREF>":"	{ BEGIN(INITIAL);
		  turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext, 0);
      if(!turtle_parser_lval->string)
        TURTLE_LEXER_OOM();
                  return IDENTIFIER; }

<PREF>(.|\n)	{ BEGIN(INITIAL);
		  if (*yytext == EOF)
                    return EOF;

                  turtle_syntax_error(rdf_parser, "syntax error at '%c'", *yytext);
                  yyterminate();  }


{QUOTEDURI}   { if(yyleng == 2) 
                  turtle_parser_lval->uri=raptor_uri_copy(rdf_parser->base_uri);
                else {
                  raptor_stringbuffer* sb;
                  unsigned char* uri_string;

                  yytext[yyleng-1]='\0';
                  sb=raptor_new_stringbuffer();
                  if(!sb)
                    TURTLE_LEXER_OOM();
                  if(raptor_stringbuffer_append_turtle_string(sb, (unsigned char*)yytext+1, yyleng-1, '>', (raptor_simple_message_handler)turtle_lexer_syntax_error, rdf_parser)) {
                    raptor_free_stringbuffer(sb);
                    TURTLE_LEXER_OOM();
                  }
                  uri_string=raptor_stringbuffer_as_string(sb);
                  turtle_parser_lval->uri=raptor_new_uri_relative_to_base(rdf_parser->base_uri, uri_string);
                  if(!turtle_parser_lval->uri) {
                    raptor_free_stringbuffer(sb);
                    TURTLE_LEXER_OOM();
                  }
                  raptor_free_stringbuffer(sb);
                }
                return URI_LITERAL; }

{LANGUAGETOKEN}	{ turtle_parser_lval->string=turtle_copy_token((unsigned char*)yytext, yyleng);
                  if(!turtle_parser_lval->string)
                    TURTLE_LEXER_OOM();
                          return IDENTIFIER; }

\#[^\r\n]*(\r\n|\r|\n)	{ /* # comment */
		turtle_parser->lineno++;
                }

\#[^\r\n]*	{ /* # comment on the last line with no terminating newline */
                }

.         	{ if (*yytext == EOF)
                    return EOF;

                  turtle_syntax_error(rdf_parser, "syntax error at '%c'", *yytext);
                  yyterminate();
		}

%%
  /* user code */

int
yywrap (yyscan_t yyscanner) {
  return 1;
}


static unsigned char *
turtle_copy_token(unsigned char *text, size_t len)
{
  unsigned char *s;
  if(!len)
    len=strlen((const char*)text);
  s=(unsigned char *)RAPTOR_MALLOC(cstring, len+1);
  if(s) {
    strncpy((char*)s, (const char*)text, len);
    s[len] = '\0';
  }
  return s;
}


static unsigned char *
turtle_copy_string_token(raptor_parser* rdf_parser, 
                         unsigned char *string, size_t len, int delim)
{
  raptor_stringbuffer* sb=NULL;
  int rc;
  
  if(len) {
    sb=raptor_new_stringbuffer();
    if(!sb)
      return NULL;
    
    rc=raptor_stringbuffer_append_turtle_string(sb, string, len, delim,
                                                (raptor_simple_message_handler)turtle_lexer_syntax_error,
                                                rdf_parser);
    if(rc) {
      raptor_free_stringbuffer(sb);
      return NULL;
    }

    len=raptor_stringbuffer_length(sb);
  }
  
  string=(unsigned char *)RAPTOR_MALLOC(cstring, len+1);
  if(string) {
    if(sb) 
      raptor_stringbuffer_copy_to_string(sb, string, len+1);
    string[len]='\0';
  }

  if(sb)
    raptor_free_stringbuffer(sb);
  
  return string;
}


void
turtle_lexer_syntax_error(void* ctx, const char *message, ...)
{
  raptor_parser* rdf_parser=(raptor_parser *)ctx;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  va_list arguments;
  
  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  va_start(arguments, message);
  raptor_parser_error_varargs(((raptor_parser*)rdf_parser), message, arguments);

  va_end(arguments);
}


/*
 * turtle_lexer_fatal_error:
 * @msg:
 * @yyscanner:
 *
 * INTERNAL - replacement for the generated error handler.
 */
static void turtle_lexer_fatal_error(yyconst char *msg, yyscan_t yyscanner)
{
  raptor_parser *rdf_parser=NULL;

  if(yyscanner)
    rdf_parser=(raptor_parser *)turtle_lexer_get_extra(yyscanner);

  if(rdf_parser)
    /* avoid "format not a string literal and no format arguments" warning with %s */
    raptor_parser_fatal_error(rdf_parser, "%s", msg); 
  else {
    fputs(msg, stderr);
    fputc('\n', stderr);
  }

  abort();
}


/* Define LEXER_ALLOC_TRACKING to enable allocated memory tracking
 * - fixes lexer memory leak when ensure_buffer_stack fails
 */

#ifdef LEXER_ALLOC_TRACKING
typedef struct {
  /* Number of void* slots allocated */
  int lexer_allocs_size;
  /* Allocted void* slots follow in memory after this header */
} lexer_alloc_tracker_header;

/* Initial alloc tracker slot array size - 2 seems to be enough for almost all cases */
static const int initial_lexer_allocs_size=2;
#endif

/*
 * turtle_lexer_cleanup:
 * @yyscanner:
 *
 * INTERNAL - Clean up unfreed lexer allocs if LEXER_ALLOC_TRACKING is enabled.
 */
static void turtle_lexer_cleanup(yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  raptor_parser *rdf_parser;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;

  if(!yyscanner)
    return;

  rdf_parser=(raptor_parser *)turtle_lexer_get_extra(yyscanner);
  if(!rdf_parser)
    return;

  tracker=(lexer_alloc_tracker_header *)rdf_parser->lexer_user_data;
  if(!tracker)
    return;
  lexer_allocs=(void**)&tracker[1];

  for(i=0; i<tracker->lexer_allocs_size; ++i) {
    if(lexer_allocs[i])
      free(lexer_allocs[i]);
    lexer_allocs[i]=NULL;
  }
  free(rdf_parser->lexer_user_data);
  rdf_parser->lexer_user_data=NULL;
#endif
}


/*
 * turtle_lexer_alloc:
 * @size
 * @yyscanner
 *
 * INTERNAL - alloc replacement.
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void *turtle_lexer_alloc(yy_size_t size, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  raptor_parser *rdf_parser;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;
  void *ptr;

  /* yyscanner not initialized -> probably initializing yyscanner itself
   * -> just malloc without tracking
   */
  if(!yyscanner)
    return malloc(size);

  rdf_parser=(raptor_parser *)turtle_lexer_get_extra(yyscanner);
  if(!rdf_parser)
    YY_FATAL_ERROR("lexer_alloc: yyscanner extra not initialized");

  /* try to allocate tracker if it does not exist */
  tracker=(lexer_alloc_tracker_header *)rdf_parser->lexer_user_data;
  if(!tracker) {
    /* allocate tracker header + array of void* slots */
    tracker=calloc(1, sizeof(lexer_alloc_tracker_header)+initial_lexer_allocs_size*sizeof(void*));
    if(!tracker)
      YY_FATAL_ERROR("lexer_alloc: cannot allocate tracker");
    tracker->lexer_allocs_size=initial_lexer_allocs_size;
    rdf_parser->lexer_user_data=(void *)tracker;
  }
  lexer_allocs=(void**)&tracker[1];

  /* allocate memory */
  ptr=malloc(size);
  
  /* find a free slot for ptr */
  for(i=0; i<tracker->lexer_allocs_size; ++i) {
    if(!lexer_allocs[i]) {
      lexer_allocs[i]=ptr;
      break;
    }
  }

  /* no free slots -> grow tracker slot array */
  if(i>=tracker->lexer_allocs_size) {
    int j;
    void **dest;
    tracker=calloc(1, sizeof(lexer_alloc_tracker_header)+i*2*sizeof(void*));
    if(!tracker) {
      if(ptr)
        free(ptr);
      YY_FATAL_ERROR("lexer_alloc: cannot grow tracker");
    }
    tracker->lexer_allocs_size=i*2;
    
    /* copy data from old tracker */
    dest=(void**)&tracker[1];
    for(j=0; j<i; ++j) {
      dest[j]=lexer_allocs[j];
    }
    
    /* set new item to first free slot */
    dest[j]=ptr;

    /* free old tracker and replace with new one */
    free(rdf_parser->lexer_user_data);
    rdf_parser->lexer_user_data=tracker;
  }

  return ptr;
#else
  return malloc(size);
#endif
}


/*
 * turtle_lexer_realloc:
 *
 * INTERNAL - realloc replacement
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void *turtle_lexer_realloc(void *ptr, yy_size_t size, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  raptor_parser *rdf_parser;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;
  void *newptr;

  if(!yyscanner)
    YY_FATAL_ERROR("lexer_realloc: yyscanner not initialized");

  rdf_parser=(raptor_parser *)turtle_lexer_get_extra(yyscanner);
  if(!rdf_parser)
    YY_FATAL_ERROR("lexer_realloc: yyscanner extra not initialized");

  tracker=(lexer_alloc_tracker_header *)rdf_parser->lexer_user_data;
  if(!tracker)
    YY_FATAL_ERROR("lexer_realloc: no alloc tracker");
  lexer_allocs=(void**)&tracker[1];

  /* find the old slot for ptr */
  for(i=0; i<tracker->lexer_allocs_size; ++i) {
    if(lexer_allocs[i]==ptr)
      break;
  }

  /* no old slot -> error */  
  if(i>=tracker->lexer_allocs_size)
    YY_FATAL_ERROR("lexer_realloc: cell not in tracker");

  /* realloc */
  newptr=realloc((char*)ptr, size);

  /* replace entry in tracker */
  lexer_allocs[i]=newptr;

  return newptr;
#else
  return realloc((char*)ptr, size);
#endif
}


/*
 * turtle_lexer_free:
 *
 * INTERNAL - free replacement.
 * Checks for NULL pointer to be freed unlike the default lexer free function.
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void turtle_lexer_free(void *ptr, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  raptor_parser *rdf_parser;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;

  /* do not free NULL */
  if(!ptr)
    return;

  /* free ptr even if we would encounter an error */
  free(ptr);

  /* yyscanner is allocated with turtle_lexer_alloc() but it's never stored in the tracker
   * - we need yyscanner to access the tracker */
  if(!yyscanner || ptr==yyscanner)
    return;

  rdf_parser=(raptor_parser *)turtle_lexer_get_extra(yyscanner);
  if(!rdf_parser)
    return;

  tracker=(lexer_alloc_tracker_header *)rdf_parser->lexer_user_data;
  if(!tracker)
    return;
  lexer_allocs=(void**)&tracker[1];

  /* find the slot for ptr */
  for(i=0; i<tracker->lexer_allocs_size; ++i) {
    if(lexer_allocs[i]==ptr)
      break;
  }

  /* no slot -> error */  
  if(i>=tracker->lexer_allocs_size)
    YY_FATAL_ERROR("lexer_free: cell not in tracker");

  /* remove entry from tracker */
  lexer_allocs[i]=NULL;
#else
  if(ptr)
    free(ptr);
#endif
}


#ifdef RAPTOR_DEBUG

const char *
turtle_token_print(int token, YYSTYPE *lval)
{
  static char buffer[2048];

  if(!token)
    return "<<EOF>>";
  
  switch(token) {
    case PREFIX:
      return "PREFIX";

    case BASE:
      return "BASE";

    case A:
      return "A";

    case DOT:
      return "DOT";

    case COMMA:
      return "COMMA";

    case SEMICOLON:
      return "SEMICOLON";

    case LEFT_SQUARE:
      return "LEFT_SQUARE";

    case RIGHT_SQUARE:
      return "RIGHT_SQUARE";

    case AT:
      return "AT";

    case HAT:
      return "HAT";

    case STRING_LITERAL:
      sprintf(buffer, "STRING_LITERAL(%s)", lval->string);
      return buffer;

    case URI_LITERAL:
      sprintf(buffer, "URI_LITERAL(%s)", 
              (lval->uri ? (char*)raptor_uri_as_string(lval->uri) : ""));
      return buffer;

    case BLANK_LITERAL:
      sprintf(buffer, "BLANK_LITERAL(%s)", lval->string);
      return buffer;

    case QNAME_LITERAL:
      sprintf(buffer, "QNAME_LITERAL(%s)", 
              (lval->uri ? (char*)raptor_uri_as_string(lval->uri) : ""));
      return buffer;

    case INTEGER_LITERAL:
      sprintf(buffer, "INTEGER_LITERAL(%d)", lval->integer);
      return buffer;

    case FLOATING_LITERAL:
      sprintf(buffer, "FLOATING_LITERAL(%s)", lval->string);
      return buffer;

    case IDENTIFIER:
      sprintf(buffer, "IDENTIFIER(%s)", 
              (lval->string ? (char*)lval->string : ""));
      return buffer;

    case DECIMAL_LITERAL:
      sprintf(buffer, "DECIMAL_LITERAL(%s)", lval->string);
      return buffer;

    case ERROR_TOKEN:
      return "ERROR";

   default:
     fprintf(stderr, "turtle_token_print: UNKNOWN token %d - add a new case\n", token);
     abort();
  }
}
#endif



#ifdef STANDALONE

static void
turtle_token_free(int token, YYSTYPE *lval)
{
  if(!token)
    return;

  switch(token) {
    case STRING_LITERAL:
    case BLANK_LITERAL:
    case IDENTIFIER:
      if(lval->string)
        RAPTOR_FREE(cstring, lval->string);
      break;

    case URI_LITERAL:
    case QNAME_LITERAL:
      if(lval->uri)
        raptor_free_uri(lval->uri);
      break;
    default:
      break;
  }
}


int
main(int argc, char *argv[]) 
{
  raptor_parser rdf_parser;
  raptor_turtle_parser turtle_parser;
  yyscan_t scanner;
  int token=EOF;
  FILE *fh;
  YYSTYPE lval;
  const unsigned char *uri_string;
  char *filename=NULL;
  
  raptor_init();
  
  if(argc > 1) {
    filename=argv[1];
    fh=fopen(filename, "r");
    if(!fh) {
      fprintf(stderr, "%s: Cannot open file %s - %s\n", argv[0], filename,
              strerror(errno));
      exit(1);
    }
  } else {
    filename="<stdin>";
    fh=stdin;
  }

  memset(&rdf_parser, 0, sizeof(raptor_parser));
  memset(&turtle_parser, 0, sizeof(raptor_turtle_parser));

  yylex_init(&turtle_parser.scanner);
  scanner=turtle_parser.scanner;
  turtle_lexer_set_in(fh, scanner);
  turtle_lexer_set_extra(&rdf_parser, scanner);

  /* Initialise enough of the parser and locator to get error messages */
  rdf_parser.context=&turtle_parser;
  turtle_parser.lineno=1;
  rdf_parser.locator.file=filename;
  rdf_parser.locator.column= -1;

  uri_string=raptor_uri_filename_to_uri_string(filename);
  rdf_parser.base_uri=raptor_new_uri(uri_string);
  RAPTOR_FREE(cstring, (void*)uri_string);
  
  while(1) {
    memset(&lval, 0, sizeof(YYSTYPE));
    if(turtle_lexer_get_text(scanner) != NULL)
      printf("yyinput '%s'\n", turtle_lexer_get_text(scanner));
    token=yylex(&lval, scanner);
#ifdef RAPTOR_DEBUG
    printf("token %s\n", turtle_token_print(token, &lval));
#else
    printf("token %d\n", token);
#endif
    turtle_token_free(token, &lval);
    if(!token || token == EOF || token == ERROR_TOKEN)
      break;
  }

  yylex_destroy(scanner);

  raptor_free_uri(rdf_parser.base_uri);

  raptor_finish();


  if(token == ERROR_TOKEN)
    return 1;
 
  return 0;
}
#endif
