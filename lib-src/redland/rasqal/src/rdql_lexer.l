/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdql_lexer.l - Rasqal RDQL lexer - making tokens for rdql grammar generator
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
 * To generate the C files from this source, rather than use the
 * shipped rdql_lexer.c/.h needs a patched version of flex 2.5.31 such
 * as the one available in Debian GNU/Linux.   Details below
 * near the %option descriptions.
 *
 */


/* recognise 8-bits */
%option 8bit
%option warn nodefault

/* all symbols prefixed by this */
%option prefix="rdql_lexer_"

/* This is not needed, flex is invoked -ordql_lexer.c */
%option outfile="rdql_lexer.c"

/* Emit a C header file for prototypes
 * Only available in flex 2.5.13 or newer.
 * It was renamed to header-file in flex 2.5.19
 */
%option header-file="rdql_lexer.h"

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


%x PATTERN ID

  /* definitions */

%{

/* NOTE: These headers are NOT included here. They are inserted by fix-flex
 * since otherwise it appears far too late in the generated C
 */

/*
#ifdef HAVE_CONFIG_H
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif
*/

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#ifdef HAVE_SETJMP_H
#include <setjmp.h>
#endif

#include <rasqal.h>
#include <rasqal_internal.h>

#include <rdql_parser.h>

#include <rdql_common.h>



static int rdql_skip_c_comment(rasqal_query *rq);

static unsigned char* rdql_copy_name(rasqal_query *rq, const unsigned char *text, size_t len);
static unsigned char* rdql_copy_qname(rasqal_query *rq, const unsigned char *text, size_t len);
static int rdql_copy_regex_token(rasqal_query *rq, YYSTYPE* lval, unsigned char delim);
static int rdql_copy_string_token(rasqal_query *rq, YYSTYPE* lval, const unsigned char *text, size_t len, int delim);

#ifdef RASQAL_DEBUG
const char * rdql_token_print(int token, YYSTYPE *lval);
#endif

int rdql_lexer_lex (YYSTYPE *rdql_parser_lval, yyscan_t yyscanner);
#define YY_DECL int rdql_lexer_lex (YYSTYPE *rdql_parser_lval, yyscan_t yyscanner)

#ifdef __cplusplus
#define INPUT_FN yyinput
#else
#define INPUT_FN input
#endif

/* Remove the re-fill function since it should never be called */
#define YY_INPUT(buf,result,max_size) { return YY_NULL; }


/* Missing rdql_lexer.c/h prototypes */
int rdql_lexer_get_column(yyscan_t yyscanner);
void rdql_lexer_set_column(int  column_no , yyscan_t yyscanner);

static void rdql_lexer_cleanup(yyscan_t yyscanner);

#ifdef HAVE_SETJMP
static jmp_buf rdql_lexer_fatal_error_longjmp_env;

/* fatal error handler declaration */
#define YY_FATAL_ERROR(msg) do {		\
    rdql_lexer_fatal_error(msg, yyscanner);   \
    longjmp(rdql_lexer_fatal_error_longjmp_env, 1);        \
} while(0)
#else
#define YY_FATAL_ERROR(msg) do {		\
    rdql_lexer_fatal_error(msg, yyscanner);   \
    abort();                                    \
} while(0)
#endif
 
static void rdql_lexer_fatal_error(yyconst char *msg, yyscan_t yyscanner);

%}

LANGUAGETOKEN [A-Za-z][-A-Z_a-z0-9]*
/* See
 * http://www.w3.org/TR/xml11/#NT-NameStartChar and 
 * http://www.w3.org/TR/xml11/#NT-NameChar 
 */
PREFIX [A-Za-z\\\x80-\xff][-A-Z_a-z\\\x80-\xff0-9]*
NAME [A-Za-z_\\\x80-\xff][-A-Z_a-z\\\x80-\xff0-9]*
QNAME {PREFIX}:{NAME}*
/* The initial char conditions are to ensure this doesn't grab < or <= */
QUOTEDURI \<[A-Za-z][^>]+\>

%%
  /* rules */

%{

  int c;
  rasqal_query *rq=(rasqal_query*)yyextra;
  rasqal_rdql_query_engine *rqe=(rasqal_rdql_query_engine*)rq->context;

#ifdef HAVE_SETJMP
  if(setjmp(rdql_lexer_fatal_error_longjmp_env))
    return 1;
#endif
  
%}

"//"[^\r\n]*(\r\n|\r|\n)	{ /* C++ comment */
        rqe->lineno++;
}

"/*"	{ int lines=rdql_skip_c_comment(rq);
          if(lines < 0)
            yyterminate();
          rqe->lineno += lines;
        }

\r\n|\r|\n     { rqe->lineno++; }

[\ \t\v]+   { /* eat up other whitespace */
	;
}

"SELECT"|"select" { return SELECT; }
"SOURCE"|"source" { return SOURCE; }
"FROM"|"from"     { return FROM; } 
"WHERE"|"where"   { return WHERE; } 
"AND"|"and"       { return AND; } 
"USING"|"using"   { return USING; }
"FOR"|"for"       { return FOR; }

","      { return ','; } 
"("      { return '('; } 
")"      { return ')'; } 
"?"  { BEGIN(ID); return '?'; }

"||"         { return SC_OR; }
"&&"         { return SC_AND; }

"EQ"|"eq"  { return STR_EQ; }
"NE"|"NE"  { return STR_NE; }

"=~"|"~~"  { BEGIN(PATTERN); return STR_MATCH; }
"!~"       { BEGIN(PATTERN); return STR_NMATCH; }

<PATTERN>[ \t\v]+   {
	;
}

<PATTERN>\r\n|\r|\n     { rqe->lineno++; }


<PATTERN>. { /* first non whitespace */
             if(!rdql_copy_regex_token(rq, rdql_parser_lval, *yytext)) {
               BEGIN(INITIAL);
               return PATTERN_LITERAL;
             }
             BEGIN(INITIAL);
	     yyterminate();
         };


"=="            { return EQ; }
"!="            { return NEQ; }
"<"/[^A-Za-z=]  { return LT; }
">"             { return GT; }
"<="         { return LE; }
">="         { return GE; }

"+"         { return '+'; }
"-"         { return '-'; }
"*"         { return '*'; }
"/"         { return '/'; }
"%"         { return '%'; }
"~"         { return '~'; }
"!"         { return '!'; }

[0-9]+["lL"]?   { c=yytext[yyleng-1];
                  if (c== 'l' || c == 'L')
                    yytext[yyleng-1]='\0';
                  rdql_parser_lval->literal=rasqal_new_integer_literal(rq->world, RASQAL_LITERAL_INTEGER, atoi(yytext));
 		  return INTEGER_LITERAL; 
}

0[xX][0-9a-fA-F]+   { int i;
                      int n;

                      if(yytext[yyleng+1] == 'x') 
                        n=sscanf(yytext+2, "%x", &i);
                      else
                        n=sscanf(yytext+2, "%X", &i);
                      if(n != 1) {
                        rdql_syntax_error(rq, "RDQL syntax error - Illegal hex constant %c%c%c",
                                          yytext[0], yytext[1], yytext[2]);
                        yyterminate();
                      }
                      rdql_parser_lval->literal=rasqal_new_integer_literal(rq->world, RASQAL_LITERAL_INTEGER, i);
                      return INTEGER_LITERAL; 
}

[0-9]+"."[0-9]*[eE][+-]?[0-9]+[fFdD]?|"."[0-9]+[eE][+-]?[0-9]+[fFdD]?|[0-9]+[eE][+-]?[0-9]+[fFdD]?|[0-9]+[eE][+-]?[0-9]+[fFdD]?|[0-9]+"."[0-9]*  { 
                        rdql_parser_lval->literal=rasqal_new_typed_literal(rq->world, RASQAL_LITERAL_DOUBLE, (const unsigned char*)yytext);
                        if(!rdql_parser_lval->literal)
                          yyterminate();
                        return FLOATING_POINT_LITERAL;
}

'([^'\\\n\r]|\\[^\n\r])*'(@{LANGUAGETOKEN})?(^^({QUOTEDURI}|{QNAME}))?    { /*' */
                        if(!rdql_copy_string_token(rq, rdql_parser_lval,
                          (const unsigned char*)yytext+1, yyleng-1, '\''))
                          return STRING_LITERAL;
		        yyterminate();
}

\"([^"\\\n\r]|\\[^\n\r])*\"(@{LANGUAGETOKEN})?(^^({QUOTEDURI}|{QNAME}))?   { /* " */
                        if(!rdql_copy_string_token(rq, rdql_parser_lval,
                          (const unsigned char*)yytext+1, yyleng-1, '"'))
                          return STRING_LITERAL; 
		        yyterminate();
}

"true"|"false"	{ rdql_parser_lval->literal=rasqal_new_boolean_literal(rq->world, *yytext== 't');
                  return BOOLEAN_LITERAL; }

"null"	{ rdql_parser_lval->literal=NULL;
          return NULL_LITERAL; }

<*>{NAME}	{ rdql_parser_lval->name=rdql_copy_name(rq, (const unsigned char*)yytext, yyleng);
		  if(!rdql_parser_lval->name)
                    yyterminate();
                          BEGIN(INITIAL);
                          return IDENTIFIER; }
<ID>(.|\n)	{	BEGIN(INITIAL);
		rdql_syntax_error(rq, "RDQL syntax error - missing variable name after ?");
                yyterminate();
		}

{QNAME}	{ rdql_parser_lval->name=rdql_copy_qname(rq, (const unsigned char*)yytext, yyleng);
	  if(!rdql_parser_lval->name)
            yyterminate();
                          return QNAME_LITERAL; }

\<{QNAME}\>	{ rdql_parser_lval->name=rdql_copy_qname(rq, (const unsigned char*)yytext+1, yyleng-2);
		  if(!rdql_parser_lval->name)
	            yyterminate();
                  rdql_syntax_warning(rq, "Obsolete RDQL <qname> syntax found in \"%s\"", (const unsigned char*)yytext);
                  return QNAME_LITERAL; }

{QUOTEDURI}   { if(yyleng == 2) 
                  rdql_parser_lval->uri=raptor_uri_copy(rq->base_uri);
                else {
                  yytext[yyleng-1]='\0';
                  rdql_parser_lval->uri=raptor_new_uri_relative_to_base(rq->base_uri, (const unsigned char*)yytext+1);
                 }
                 return URI_LITERAL; }

\#              { while((c=INPUT_FN(yyscanner)) != '\n' && c)
                    ;
                }

.         	{ if (!*yytext)
                    return EOF;

                  rdql_syntax_error(rq, "RDQL syntax error at '%s'", yytext);
                  yyterminate();
		}

%%
  /* user code */

int
yywrap (yyscan_t yyscanner) {
  return 1;
}


static unsigned char *
rdql_copy_name(rasqal_query *rq, const unsigned char *text, size_t len) {
  size_t dest_len=0;
  unsigned char *s;

  s=rasqal_escaped_name_to_utf8_string((unsigned char*)text, len,
                                       &dest_len,
                                       (raptor_simple_message_handler)rdql_syntax_error, rq);
  if(!s)
    return s;

  if(!raptor_xml_name_check(s, dest_len, 11))
    rdql_syntax_warning(rq, "Invalid RDQL name \"%s\"", s);

  return s;
}


static unsigned char *
rdql_copy_qname(rasqal_query *rq, const unsigned char *text, size_t len) {
  unsigned char *p;
  size_t dest_len=0;
  unsigned char *s;
  
  s=rasqal_escaped_name_to_utf8_string((unsigned char*)text, len,
                                       &dest_len,
                                       (raptor_simple_message_handler)rdql_syntax_error, rq);
  if(!s)
    return s;

  p=(unsigned char*)strchr((const char*)s, ':');
  if(!raptor_xml_name_check(s, p-s, 11))
    rdql_syntax_warning(rq, "Invalid RDQL name \"%s\"", s);
  if(!raptor_xml_name_check(p+1, dest_len-((p+1)-s), 11))
    rdql_syntax_warning(rq, "Invalid RDQL name \"%s\"", p+1);

  return s;
}


static int
rdql_copy_regex_token(rasqal_query* rq, YYSTYPE* lval, unsigned char delim) {
  rasqal_rdql_query_engine *rqe=(rasqal_rdql_query_engine*)rq->context;
  yyscan_t yyscanner=rqe->scanner;
  unsigned int ind=0;
  size_t buffer_len=0;
  unsigned char *buffer=NULL;
  size_t flags_len=0;
  unsigned char *flags=NULL;
  int c;

  if(delim == 'm') {
    /* Handle pattern literal m/foo/ */
    c=INPUT_FN(yyscanner);
    if(c == EOF) {
      rdql_syntax_error(rq, "RDQL syntax error - EOF in regex");
      return 1;
    }
    delim=(unsigned char)c;
  }

  while((c=INPUT_FN(yyscanner)) && c != EOF && c != delim) {
    /* May add 2 chars - \' */
    if(ind+2 > buffer_len) {
      unsigned char *new_buffer;
      size_t new_buffer_len=buffer_len <<1;

      if(new_buffer_len<10)
        new_buffer_len=10;
      new_buffer=(unsigned char *)RASQAL_CALLOC(cstring, 1, new_buffer_len+1);
      if(buffer_len) {
        strncpy((char*)new_buffer, (const char*)buffer, buffer_len);
        RASQAL_FREE(cstring, buffer);
      }
      buffer=new_buffer;
      buffer_len=new_buffer_len;
    }
    buffer[ind++]=c;
    if(c == '\\') {
      c=INPUT_FN(yyscanner);
      buffer[ind++]=c;
    }
  }

  if(!buffer) {
    buffer_len=0;
    buffer=(unsigned char *)RASQAL_CALLOC(cstring, 1, buffer_len+1);
  }
  buffer[ind]='\0';

  if(c == EOF) {
    rdql_syntax_error(rq, "RDQL syntax error - EOF in regex");
    return 1;
  }

  /* flags */
  ind=0;
  while((c=INPUT_FN(yyscanner)) && c != EOF && isalpha(c)) {
    if(ind+1 > flags_len) {
      unsigned char *new_flags;
      size_t new_flags_len=flags_len + 5;

      if(new_flags_len<5)
        new_flags_len=5;
      new_flags=(unsigned char *)RASQAL_CALLOC(cstring, 1, new_flags_len+1);
      if(flags_len) {
        strncpy((char*)new_flags, (const char*)flags, flags_len);
        RASQAL_FREE(cstring, flags);
      }
      flags=new_flags;
      flags_len=new_flags_len;
    }
    flags[ind++]=c;
  }
  if(flags)
    flags[ind]='\0';

  lval->literal=rasqal_new_pattern_literal(rq->world, buffer, (const char*)flags);
  return 0;
}


static int
rdql_copy_string_token(rasqal_query* rq, YYSTYPE* lval,
                       const unsigned char *text, size_t len, int delim) {
  unsigned int i;
  const unsigned char *s;
  unsigned char *d;
  unsigned char *string=(unsigned char *)RASQAL_MALLOC(cstring, len+1);
  char *language=NULL;
  unsigned char *dt=NULL;
  raptor_uri *dt_uri=NULL;
  unsigned char *dt_qname=NULL;

  for(s=text, d=string, i=0; i<len; s++, i++) {
    unsigned char c=*s;

    if(c == '\\' ) {
      s++; i++;
      c=*s;
      if(c == 'n')
        *d++= '\n';
      else if(c == 'r')
        *d++= '\r';
      else if(c == 't')
        *d++= '\t';
      else if(c == '\\' || c == delim)
        *d++=c;
      else if (c == 'u' || c == 'U') {
        int ulen=(c == 'u') ? 4 : 8;
        unsigned long unichar=0;
        int n;
        
        s++; i++;
        if(i+ulen > len) {
          printf("\\%c over end of line", c);
          RASQAL_FREE(cstring, string);
          return 1;
        }
        
        n=sscanf((const char*)s, ((ulen == 4) ? "%04lx" : "%08lx"), &unichar);
        if(n != 1) {
          rdql_syntax_error(rq, "RDQL syntax error - Illegal Unicode escape '%c%s...'", c, s);
          RASQAL_FREE(cstring, string);
          return 1;
        }

        s+= ulen-1;
        i+= ulen-1;
        
        if(unichar > 0x10ffff) {
          rdql_syntax_error(rq, "RDQL syntax error - Illegal Unicode character with code point #x%lX.", unichar);
          RASQAL_FREE(cstring, string);
          return 1;
        }
          
        d+=raptor_unicode_char_to_utf8(unichar, d);
      } else {
        /* Ignore \x where x isn't the one of: \n \r \t \\ (delim) \u \U */
        rdql_syntax_warning(rq, "Unknown RDQL string escape \\%c in \"%s\"", c, text);
        *d++=c;
      }
    } else if(c== delim) {
      *d++='\0';

      /* skip delim */
      s++; i++;

      c=*s++; i++;
      if(c=='@') {
        language=(char*)d;
        while(i<=len) {
          c=*s++; i++;
          if(!isalpha(c) && !isdigit(c) && c != '-')
            break;
          *d++=c;
        }
        *d++='\0';
      }
      if(c=='^') {
        /* skip second char of ^^ */
        s++; i++;

        dt=d;
        while(i++<=len)
          *d++=*s++;
        /* *d='\0' below */
      } else if (language)
        *d='\0';
      
      break;
   } else
    *d++=c;
  } /* end of for */

  *d='\0';

  if(language) {
    char *new_language=(char *)RASQAL_MALLOC(cstring, strlen((const char*)language)+1);
    strcpy(new_language, language);
    language=new_language;
  }
  
  if(dt) {
    /* dt can be a URI or qname */
    if(*dt == '<') {
      dt[strlen((const char*)dt)-1]='\0';
      dt_uri=raptor_new_uri(dt+1);
    } else {
      size_t dt_qname_len=strlen((const char*)dt);
      
      if(!raptor_xml_name_check(dt, dt_qname_len, 11))
        rdql_syntax_warning(rq, "Invalid RDQL name \"%s\"", dt);
      
      /* the qname is expanded later */
      dt_qname=(unsigned char *)RASQAL_MALLOC(cstring, dt_qname_len+1);
      strcpy((char*)dt_qname, (const char*)dt);
    }
  }

#if RASQAL_DEBUG >3
  fprintf(stderr, "string='%s', language='%s'\n", 
          string, (language ? language : ""));
  fprintf(stderr, "dt uri='%s',qname='%s'\n",
          (dt_uri ? (const char*)raptor_uri_as_string(dt_uri) : ""),
          (dt_qname ? (const char*)dt_qname : ""));
#endif

  lval->literal=rasqal_new_string_literal(rq->world, string, language, dt_uri, dt_qname);

  return 0;
}


static int
rdql_skip_c_comment(rasqal_query *rq) {
  rasqal_rdql_query_engine *rqe=(rasqal_rdql_query_engine*)rq->context;
  yyscan_t yyscanner=rqe->scanner;
  int lines=0;
  int c;
  
  while(1) {
    while ((c=INPUT_FN(yyscanner)) != '*' && c!= EOF) {
      if(c == '\r' || c == '\n')
        lines++;
    }
    if( c == '*') {
      while ((c=INPUT_FN(yyscanner)) == '*') {
        if(c == '\r' || c == '\n')
          lines++;
      }

      if(c == '/')
        break;
    }
    if (c == EOF) {
      rdql_syntax_error(rq, "RDQL syntax error - EOF in comment");
      lines= -1;
      break;
    }
  }
  return lines;
}


/*
 * rdql_lexer_fatal_error:
 *
 * INTERNAL - replacement for the generated error handler.
 * Uses rasqal_query_fatal_error() when possible.
 */
static void rdql_lexer_fatal_error(yyconst char *msg, yyscan_t yyscanner)
{
  rasqal_query *rq=NULL;

  if(yyscanner)
    rq=(rasqal_query *)rdql_lexer_get_extra(yyscanner);

  if(rq) {
    /* avoid "format not a string literal and no format arguments" warning with %s */
    rq->failed=1;
    rasqal_log_error_simple(rq->world, RAPTOR_LOG_LEVEL_FATAL,
                            &rq->locator, "%s", msg); 
  } else
    (void)fprintf(stderr, "%s\n", msg);

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
 * rdql_lexer_cleanup:
 * @yyscanner:
 *
 * INTERNAL - Clean up unfreed lexer allocs if LEXER_ALLOC_TRACKING is enabled.
 */
static void rdql_lexer_cleanup(yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  rasqal_query *rq;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;

  if(!yyscanner)
    return;

  rq=(rasqal_query *)rdql_lexer_get_extra(yyscanner);
  if(!rq)
    return;

  tracker=(lexer_alloc_tracker_header *)rq->lexer_user_data;
  if(!tracker)
    return;
  lexer_allocs=(void**)&tracker[1];

  for(i=0; i<tracker->lexer_allocs_size; ++i) {
    if(lexer_allocs[i])
      free(lexer_allocs[i]);
    lexer_allocs[i]=NULL;
  }
  free(rq->lexer_user_data);
  rq->lexer_user_data=NULL;
#endif
}


/*
 * rdql_lexer_alloc:
 * @size
 * @yyscanner
 *
 * INTERNAL - alloc replacement.
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void *rdql_lexer_alloc(yy_size_t size, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  rasqal_query *rq;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;
  void *ptr;

  /* yyscanner not initialized -> probably initializing yyscanner itself
   * -> just malloc without tracking
   */
  if(!yyscanner)
    return malloc(size);

  rq=(rasqal_query *)rdql_lexer_get_extra(yyscanner);
  if(!rq)
    YY_FATAL_ERROR("lexer_alloc: yyscanner extra not initialized");

  /* try to allocate tracker if it does not exist */
  tracker=(lexer_alloc_tracker_header *)rq->lexer_user_data;
  if(!tracker) {
    /* allocate tracker header + array of void* slots */
    tracker=calloc(1, sizeof(lexer_alloc_tracker_header)+initial_lexer_allocs_size*sizeof(void*));
    if(!tracker)
      YY_FATAL_ERROR("lexer_alloc: cannot allocate tracker");
    tracker->lexer_allocs_size=initial_lexer_allocs_size;
    rq->lexer_user_data=(void *)tracker;
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
    free(rq->lexer_user_data);
    rq->lexer_user_data=tracker;
  }

  return ptr;
#else
  return malloc(size);
#endif
}


/*
 * rdql_lexer_realloc:
 *
 * INTERNAL - realloc replacement
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void *rdql_lexer_realloc(void *ptr, yy_size_t size, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  rasqal_query *rq;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;
  void *newptr;

  if(!yyscanner)
    YY_FATAL_ERROR("lexer_realloc: yyscanner not initialized");

  rq=(rasqal_query *)rdql_lexer_get_extra(yyscanner);
  if(!rq)
    YY_FATAL_ERROR("lexer_realloc: yyscanner extra not initialized");

  tracker=(lexer_alloc_tracker_header *)rq->lexer_user_data;
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
 * rdql_lexer_free:
 *
 * INTERNAL - free replacement.
 * Checks for NULL pointer to be freed unlike the default lexer free function.
 * Tracks allocated cells if LEXER_ALLOC_TRACKING is enabled.
 */
void rdql_lexer_free(void *ptr, yyscan_t yyscanner)
{
#ifdef LEXER_ALLOC_TRACKING
  rasqal_query *rq;
  lexer_alloc_tracker_header *tracker;
  void **lexer_allocs;
  int i;

  /* do not free NULL */
  if(!ptr)
    return;

  /* free ptr even if we would encounter an error */
  free(ptr);

  /* yyscanner is allocated with rdql_lexer_alloc() but it's never stored in the tracker
   * - we need yyscanner to access the tracker */
  if(!yyscanner || ptr==yyscanner)
    return;

  rq=(rasqal_query *)rdql_lexer_get_extra(yyscanner);
  if(!rq)
    return;

  tracker=(lexer_alloc_tracker_header *)rq->lexer_user_data;
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


#ifdef RASQAL_DEBUG

const char *
rdql_token_print(int token, YYSTYPE *lval)
{
  static char buffer[2048];

  if(!token)
    return "<<EOF>>";
  
  switch(token) {
    case SELECT:
      return "SELECT";

    case SOURCE:
      return "SOURCE";

    case FROM:
      return "FROM";

    case WHERE:
      return "WHERE";

    case AND:
      return "AND";

    case FOR:
      return "FOR";

    case ',':
      return ",";

    case '(':
      return "(";

    case ')':
      return "(";

    case '?':
      return "?";

    case USING:
      return "USING";

    case SC_AND:
      return "SC_AND";

    case SC_OR:
      return "SC_OR";

    case STR_NMATCH:
      return "STR_NMATCH";

    case STR_MATCH:
      return "STR_MATCH";

    case STR_NE:
      return "STR_NE";

    case STR_EQ:
      return "STR_EQ";

    case GE:
      return "GE";

    case LE:
      return "LE";

    case GT:
      return "GT";

    case LT:
      return "LT";

    case NEQ:
      return "NEQ";

    case EQ:
      return "EQ";

    case '%':
      return "%";

    case '/':
      return "/";

    case '*':
      return "*";

    case '-':
      return "-";

    case '+':
      return "+";

    case '!':
      return "!";

    case '~':
      return "~";

    case INTEGER_LITERAL:
      sprintf(buffer, "INTEGER_LITERAL(%d)", lval->literal->value.integer);
      return buffer;

    case FLOATING_POINT_LITERAL:
      sprintf(buffer, "FLOATING_POINT_LITERAL(%g)", lval->floating);
      return buffer;

    case STRING_LITERAL:
      if(lval->literal->language) {
        if(lval->literal->datatype)
          sprintf(buffer, "STRING_LITERAL(\"%s\"@%s^^%s)",
                  lval->literal->string, lval->literal->language,
                  raptor_uri_as_string(lval->literal->datatype));
        else
          sprintf(buffer, "STRING_LITERAL(\"%s\"@%s)",
                  lval->literal->string, lval->literal->language);
      } else {
        if(lval->literal->datatype)
          sprintf(buffer, "STRING_LITERAL(\"%s\"^^%s)", 
                  lval->literal->string,
                  raptor_uri_as_string(lval->literal->datatype));
        else
          sprintf(buffer, "STRING_LITERAL(\"%s\")", lval->literal->string);
      }
      return buffer;

    case PATTERN_LITERAL:
      sprintf(buffer, "PATTERN_LITERAL(%s,%s)", lval->literal->string,
              (lval->literal->flags ? (char*)lval->literal->flags : "-"));
      return buffer;

    case BOOLEAN_LITERAL:
      return (lval->literal->value.integer ? "BOOLEAN_LITERAL(true)" : "BOOLEAN_LITERAL(false)");

    case NULL_LITERAL:
      return "NULL_LITERAL";

    case URI_LITERAL:
      sprintf(buffer, "URI_LITERAL(%s)", raptor_uri_as_string(lval->uri));
      return buffer;

    case QNAME_LITERAL:
      sprintf(buffer, "QNAME_LITERAL(%s)", lval->name);
      return buffer;

    case IDENTIFIER:
      sprintf(buffer, "IDENTIFIER(%s)", lval->name);
      return buffer;

   default:
     RASQAL_DEBUG2("UNKNOWN token %d - add a new case\n", token);
     abort();
  }
}
#endif



#ifdef STANDALONE
static void
rdql_token_free(int token, YYSTYPE *lval)
{
  if(!token)
    return;
  
  switch(token) {
    case STRING_LITERAL:
    case PATTERN_LITERAL:
      rasqal_free_literal(lval->literal);
      break;
    case URI_LITERAL:
      raptor_free_uri(lval->uri);
      break;
    case IDENTIFIER:
      RASQAL_FREE(cstring, lval->name);
      break;
    case QNAME_LITERAL:
      if(lval->name)
        RASQAL_FREE(cstring, lval->name);
      break;
    default:
      break;
  }
}


#define FILE_READ_BUF_SIZE 2048

int
main(int argc, char *argv[]) 
{
  const char *program=rasqal_basename(argv[0]);
  char *query_string=NULL;
  rasqal_query rq;
  rasqal_rdql_query_engine rdql;
  yyscan_t scanner;
  int token=EOF;
  YYSTYPE lval;
  const unsigned char *uri_string;
  const char *filename=NULL;
  char *buf=NULL;
  size_t len;
  void *buffer;
  rasqal_world *world;

  world=rasqal_new_world();

  if(argc > 1) {
    FILE *fh;
    query_string=(char*)RASQAL_CALLOC(cstring, FILE_READ_BUF_SIZE, 1);
    filename=argv[1];
    fh=fopen(filename, "r");
    if(fh) {
      fread(query_string, FILE_READ_BUF_SIZE, 1, fh);
      fclose(fh);
    } else {
      fprintf(stderr, "%s: Cannot open file %s - %s\n", program, filename,
              strerror(errno));
      exit(1);
    }
  } else {
    filename="<stdin>";
    query_string=(char*)RASQAL_CALLOC(cstring, FILE_READ_BUF_SIZE, 1);
    fread(query_string, FILE_READ_BUF_SIZE, 1, stdin);
  }

  memset(&rq, 0, sizeof(rasqal_query));
  rq.world=world;
  memset(&rdql, 0, sizeof(rasqal_rdql_query_engine));

  yylex_init(&rdql.scanner);
  scanner=rdql.scanner;

#if 0
  /* set
   *   %option debug 
   * above first before enabling this
   */
  rdql_lexer_set_debug(1, scanner);
#endif
  
  len= strlen((const char*)query_string);
  buf= (char *)RASQAL_MALLOC(cstring, len+3);
  strncpy(buf, query_string, len);
  buf[len]= ' ';
  buf[len+1]= buf[len+2]='\0'; /* YY_END_OF_BUFFER_CHAR; */
  buffer= rdql_lexer__scan_buffer(buf, len+3, scanner);

  rdql_lexer_set_extra(&rq, scanner);

  /* Initialise enough of the rasqal_query and locator to get error messages */
  rq.context=&rdql;
  rdql.lineno=1;
  rq.locator.file=filename;
  rq.locator.column= -1;

  uri_string=raptor_uri_filename_to_uri_string(filename);
  rq.base_uri=raptor_new_uri(uri_string);
  raptor_free_memory((void*)uri_string);

  while(1) {
    memset(&lval, 0, sizeof(YYSTYPE));
    if(rdql_lexer_get_text(scanner) != NULL)
      printf("yyinput '%s'\n", rdql_lexer_get_text(scanner));
    token=yylex(&lval, scanner);
#ifdef RASQAL_DEBUG
    printf("token %s\n", rdql_token_print(token, &lval));
#else
    printf("token %d\n", token);
#endif
    rdql_token_free(token, &lval);
    if(!token || token == EOF)
      break;
  }

  if(buf)
    RASQAL_FREE(cstring, buf);
  
  yylex_destroy(scanner);

  raptor_free_uri(rq.base_uri);

  RASQAL_FREE(cstring, query_string);

  rasqal_free_world(world);

  if(rq.failed)
    return 1;
 
  return 0;
}
#endif
