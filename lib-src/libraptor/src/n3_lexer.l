/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * n3_lexer.l - Raptor Notation 3 lexer - making tokens for grammar generator
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
 * This is an incomplete Notation 3 parser.
 *
 * To generate the C files from this source, rather than use the
 * shipped n3_lexer.c/.h needs a patched version of flex 2.5.31 such
 * as the one available in Debian GNU/Linux.   Details below
 * near the %option descriptions.
 *
 */


/* recognise 8-bits */
%option 8bit
%option warn nodefault

/* all symbols prefixed by this */
%option prefix="n3_lexer_"

/* This is not needed, flex is invoked -on3_lexer.c */
/* %option outfile="n3_lexer.c" */

/* Emit a C header file for prototypes
 * Only available in flex 2.5.13 or newer.
 * It was renamed to header-file in flex 2.5.19
 */
%option header-file="n3_lexer.h"

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

#include <n3_parser.h>

#include <n3_common.h>


/* Prototypes */ 
static unsigned char *n3_copy_token(unsigned char *text, size_t len);
static unsigned char *n3_copy_string_token(raptor_parser* rdf_parser, unsigned char *text, size_t len, int delim);
void n3_lexer_syntax_error(void* rdf_parser, const char *msg, ...) RAPTOR_PRINTF_FORMAT(2, 3);

#ifdef RAPTOR_DEBUG
const char * n3_token_print(int token, YYSTYPE *lval);
#endif

int n3_lexer_lex (YYSTYPE *n3_parser_lval, yyscan_t yyscanner);
#define YY_DECL int n3_lexer_lex (YYSTYPE *n3_parser_lval, yyscan_t yyscanner)

#ifdef __cplusplus
#define INPUT_FN yyinput
#else
#define INPUT_FN input
#endif


/* Missing n3_lexer.c/h prototypes */
int n3_lexer_get_column(yyscan_t yyscanner);
void n3_lexer_set_column(int  column_no , yyscan_t yyscanner);

static void n3_lexer_cleanup(yyscan_t yyscanner);

/* fatal error handler declaration */
#define YY_FATAL_ERROR(msg) n3_lexer_fatal_error(msg, yyscanner)
static void n3_lexer_fatal_error(yyconst char *msg, yyscan_t yyscanner);

/* Out-of-memory reporting macro.
 * Do not report as fatal error - we don't want an abort(),
 * instead return EOF with yyterminate() and let the parser clean up.
 */
#define N3_LEXER_OOM() do { \
  n3_syntax_error(rdf_parser, n3_lexer_oom_text); \
  yyterminate(); \
} while(0)

static const char n3_lexer_oom_text[]="n3_lexer: Out of memory";

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
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;

\r\n|\r|\n   { n3_parser->lineno++; }
 
[\ \t\v]+   { /* empty */ }

"a" { return A; }

"."       { return DOT; } 
","       { return COMMA; } 
";"       { return SEMICOLON; }
"["       { return LEFT_SQUARE; }
"]"       { return RIGHT_SQUARE; }
"@prefix" { BEGIN(PREF); return PREFIX; }
"@"       { return AT; }
"^^"      { return HAT; }
"("       { return LEFT_ROUND; }
")"       { return RIGHT_ROUND; }


'([^'\\\n\r]|\\[^\n\r])*'    { n3_parser_lval->string=n3_copy_string_token(rdf_parser, (unsigned char*)yytext+1, yyleng-2, '\"'); /* ' */
                               if(!n3_parser_lval->string)
                                 N3_LEXER_OOM();
                               return STRING_LITERAL; }

\"([^"\\\n\r]|\\[^\n\r])*\"   { n3_parser_lval->string=n3_copy_string_token(rdf_parser, (unsigned char*)yytext+1, yyleng-2, '"'); /* ' */
                                if(!n3_parser_lval->string)
                                  N3_LEXER_OOM();
                                return STRING_LITERAL; }

\"\"\"				{ BEGIN(LITERAL); 
                                  n3_parser->sb=raptor_new_stringbuffer();
                                  if(!n3_parser->sb)
                                    N3_LEXER_OOM();
                          }

<LITERAL>\"\"\"			{
		  size_t len;
     
		  BEGIN(INITIAL);
                  len=raptor_stringbuffer_length(n3_parser->sb);
                  n3_parser_lval->string=(unsigned char *)RAPTOR_MALLOC(cstring, len+1);
                  if(!n3_parser_lval->string)
                    N3_LEXER_OOM();
                  raptor_stringbuffer_copy_to_string(n3_parser->sb, (unsigned char*)n3_parser_lval->string, len);
                  n3_parser_lval->string[len]='\0';

                  raptor_free_stringbuffer(n3_parser->sb);
                  n3_parser->sb=NULL;
                  return STRING_LITERAL; }

<LITERAL>\"|(\\[^uU]|\\u....|\\U........|[^\"]|\n)*	{
		  if (*yytext == EOF) {
                    BEGIN(INITIAL);
                    n3_syntax_error(rdf_parser, "End of file in middle of literal");
                    raptor_free_stringbuffer(n3_parser->sb);
                    n3_parser->sb=NULL;
                    return EOF;
                  }

                  if(raptor_stringbuffer_append_turtle_string(n3_parser->sb, (unsigned char*)yytext, yyleng, '"', (raptor_simple_message_handler)n3_lexer_syntax_error, rdf_parser)) { /* " */
                    BEGIN(INITIAL);
                    raptor_free_stringbuffer(n3_parser->sb);
                    n3_parser->sb=NULL;
                    N3_LEXER_OOM();
                  }
                  
   }

{BNAME}	{ n3_parser_lval->string=n3_copy_token((unsigned char*)yytext+2, yyleng-2);
          if(!n3_parser_lval->string)
            N3_LEXER_OOM();
                          return BLANK_LITERAL; }

{QNAME}	{ n3_parser_lval->uri=n3_qname_to_uri(rdf_parser, (unsigned char*)yytext, yyleng);
          if(!n3_parser_lval->uri)
            N3_LEXER_OOM();
                          return QNAME_LITERAL; }

[-+]?{DECIMAL}	{ n3_parser_lval->string=n3_copy_token((unsigned char*)yytext, yyleng);
                  if(!n3_parser_lval->string)
                    N3_LEXER_OOM();
                        return DECIMAL_LITERAL;
}

[-+]?{DOUBLE} {
                        double d;
                        int n;
                        
                        n=sscanf((const char*)yytext, "%lf", &d);
                        if(n != 1) {
                          n3_syntax_error(rdf_parser, "N3 syntax error - Illegal floating point constant %s", yytext);
                          yyterminate();
                        }
                        n3_parser_lval->floating=d;
                        return FLOATING_LITERAL;
}

[-+]?[0-9]+        { n3_parser_lval->integer=atoi(yytext);
                          return INTEGER_LITERAL; }

<PREF>[\ \t\v]+ { /* eat up leading whitespace */ }
<PREF>{NCNAME_PREFIX}":"	{ n3_parser_lval->string=n3_copy_token((unsigned char*)yytext, yyleng);
                            if(!n3_parser_lval->string)
                              N3_LEXER_OOM();
                          BEGIN(INITIAL);
                          return IDENTIFIER; }
<PREF>":"	{ BEGIN(INITIAL);
		  n3_parser_lval->string=n3_copy_token((unsigned char*)yytext, 0);
      if(!n3_parser_lval->string)
        N3_LEXER_OOM();
                  return IDENTIFIER; }

<PREF>(.|\n)	{ BEGIN(INITIAL);
		  if (*yytext == EOF)
                    return EOF;

                  n3_syntax_error(rdf_parser, "syntax error at '%c' - @prefix name must end in :", *yytext);
                  yyterminate();  }


{QUOTEDURI}   { if(yyleng == 2) 
                n3_parser_lval->uri=raptor_uri_copy(rdf_parser->base_uri);
              else {
                yytext[yyleng-1]='\0';
                n3_parser_lval->uri=raptor_new_uri_relative_to_base(rdf_parser->base_uri, (const unsigned char*)yytext+1);
                if(!n3_parser_lval->uri)
                  N3_LEXER_OOM();
              }
              return URI_LITERAL; }

{LANGUAGETOKEN}	{ n3_parser_lval->string=n3_copy_token((unsigned char*)yytext, yyleng);
                  if(!n3_parser_lval->string)
                    N3_LEXER_OOM();
                          return IDENTIFIER; }

\#[^\r\n]*(\r\n|\r|\n)	{ /* # comment */
		n3_parser->lineno++;
                }

\#[^\r\n]*	{ /* # comment on the last line with no terminating newline */
                }

.         	{ if (*yytext == EOF)
                    return EOF;

                  n3_syntax_error(rdf_parser, "syntax error at '%c'", *yytext);
                  yyterminate();
		}

%%
  /* user code */

int
yywrap (yyscan_t yyscanner) {
  return 1;
}


static unsigned char *
n3_copy_token(unsigned char *text, size_t len)
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
n3_copy_string_token(raptor_parser* rdf_parser, 
                     unsigned char *string, size_t len, int delim)
{
  raptor_stringbuffer* sb=NULL;
  int rc;
  
  if(len) {
    sb=raptor_new_stringbuffer();
    if(!sb)
      return NULL;
    
    rc=raptor_stringbuffer_append_turtle_string(sb, string, len, delim,
                                                (raptor_simple_message_handler)n3_lexer_syntax_error,
                                                rdf_parser);
    if(rc) {
      raptor_free_stringbuffer(sb);
      return NULL;
    }

    len=raptor_stringbuffer_length(sb);
  }
  
  string=(unsigned char *)RAPTOR_MALLOC(cstring, len+1);
  if(string) {
    if(sb) {
      raptor_stringbuffer_copy_to_string(sb, string, len+1);
    }
    string[len]='\0';
  }

  if(sb)
    raptor_free_stringbuffer(sb);

  return string;
}



void
n3_lexer_syntax_error(void* ctx, const char *message, ...)
{
  raptor_parser* rdf_parser=(raptor_parser *)ctx;
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;
  va_list arguments;
  
  rdf_parser->locator.line=n3_parser->lineno;
#ifdef RAPTOR_N3_USE_ERROR_COLUMNS
  rdf_parser->locator.column=n3_lexer_get_column(yyscanner);
#endif

  va_start(arguments, message);
  raptor_parser_error_varargs(((raptor_parser*)rdf_parser), message, arguments);

  va_end(arguments);
}


/*
 * n3_lexer_fatal_error:
 * @msg:
 * @yyscanner:
 *
 * INTERNAL - replacement for the generated error handler.
 */
static void n3_lexer_fatal_error(yyconst char *msg, yyscan_t yyscanner)
{
  raptor_parser *rdf_parser=NULL;

  if(yyscanner)
    rdf_parser=(raptor_parser *)n3_lexer_get_extra(yyscanner);

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
 * n3_lexer_cleanup:
 * @yyscanner:
 *
 * INTERNAL - Clean up unfreed lexer allocs if LEXER_ALLOC_TRACKING is enabled.
 */
static void n3_lexer_cleanup(yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  raptor_parser *rdf_parser;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;

  if(!yyscanner)
    return;

  rdf_parser=(raptor_parser *)n3_lexer_get_extra(yyscanner);
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
 * n3_lexer_alloc:
 * @size
 * @yyscanner
 *
 * INTERNAL - alloc replacement.
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void *n3_lexer_alloc(yy_size_t size, yyscan_t yyscanner)
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

  rdf_parser=(raptor_parser *)n3_lexer_get_extra(yyscanner);
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
 * n3_lexer_realloc:
 *
 * INTERNAL - realloc replacement
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void *n3_lexer_realloc(void *ptr, yy_size_t size, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  raptor_parser *rdf_parser;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;
  void *newptr;

  if(!yyscanner)
    YY_FATAL_ERROR("lexer_realloc: yyscanner not initialized");

  rdf_parser=(raptor_parser *)n3_lexer_get_extra(yyscanner);
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
 * n3_lexer_free:
 *
 * INTERNAL - free replacement.
 * Checks for NULL pointer to be freed unlike the default lexer free function.
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void n3_lexer_free(void *ptr, yyscan_t yyscanner)
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

  /* yyscanner is allocated with n3_lexer_alloc() but it's never stored in the tracker
   * - we need yyscanner to access the tracker */
  if(!yyscanner || ptr==yyscanner)
    return;

  rdf_parser=(raptor_parser *)n3_lexer_get_extra(yyscanner);
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
n3_token_print(int token, YYSTYPE *lval)
{
  static char buffer[2048];

  if(!token)
    return "<<EOF>>";
  
  switch(token) {
    case PREFIX:
      return "PREFIX";

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
      sprintf(buffer, "FLOATING_LITERAL(%1g)", lval->floating);
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
     fprintf(stderr, "n3_token_print: UNKNOWN token %d - add a new case\n", token);
     abort();
  }
}
#endif



#ifdef STANDALONE

static void
n3_token_free(int token, YYSTYPE *lval)
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
  raptor_n3_parser n3_parser;
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
  memset(&n3_parser, 0, sizeof(raptor_n3_parser));

  yylex_init(&n3_parser.scanner);
  scanner=n3_parser.scanner;
  n3_lexer_set_in(fh, scanner);
  n3_lexer_set_extra(&rdf_parser, scanner);

  /* Initialise enough of the parser and locator to get error messages */
  rdf_parser.context=&n3_parser;
  n3_parser.lineno=1;
  rdf_parser.locator.file=filename;
  rdf_parser.locator.column= -1;

  uri_string=raptor_uri_filename_to_uri_string(filename);
  rdf_parser.base_uri=raptor_new_uri(uri_string);
  RAPTOR_FREE(cstring, (void*)uri_string);
  
  while(1) {
    memset(&lval, 0, sizeof(YYSTYPE));
    if(n3_lexer_get_text(scanner) != NULL)
      printf("yyinput '%s'\n", n3_lexer_get_text(scanner));
    token=yylex(&lval, scanner);
#ifdef RAPTOR_DEBUG
    printf("token %s\n", n3_token_print(token, &lval));
#else
    printf("token %d\n", token);
#endif
    n3_token_free(token, &lval);
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
