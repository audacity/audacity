/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     SELECT = 258,
     FROM = 259,
     WHERE = 260,
     OPTIONAL = 261,
     DESCRIBE = 262,
     CONSTRUCT = 263,
     ASK = 264,
     DISTINCT = 265,
     REDUCED = 266,
     LIMIT = 267,
     UNION = 268,
     PREFIX = 269,
     BASE = 270,
     BOUND = 271,
     GRAPH = 272,
     NAMED = 273,
     FILTER = 274,
     OFFSET = 275,
     ORDER = 276,
     BY = 277,
     REGEX = 278,
     ASC = 279,
     DESC = 280,
     LANGMATCHES = 281,
     A = 282,
     STR = 283,
     LANG = 284,
     DATATYPE = 285,
     ISURI = 286,
     ISBLANK = 287,
     ISLITERAL = 288,
     SAMETERM = 289,
     EXPLAIN = 290,
     GROUP = 291,
     COUNT = 292,
     AS = 293,
     DELETE = 294,
     INSERT = 295,
     SC_OR = 296,
     SC_AND = 297,
     EQ = 298,
     NEQ = 299,
     LT = 300,
     GT = 301,
     LE = 302,
     GE = 303,
     STRING_LITERAL = 304,
     DOUBLE_LITERAL = 305,
     DOUBLE_POSITIVE_LITERAL = 306,
     DOUBLE_NEGATIVE_LITERAL = 307,
     INTEGER_LITERAL = 308,
     INTEGER_POSITIVE_LITERAL = 309,
     INTEGER_NEGATIVE_LITERAL = 310,
     DECIMAL_LITERAL = 311,
     DECIMAL_POSITIVE_LITERAL = 312,
     DECIMAL_NEGATIVE_LITERAL = 313,
     BOOLEAN_LITERAL = 314,
     URI_LITERAL = 315,
     URI_LITERAL_BRACE = 316,
     QNAME_LITERAL = 317,
     QNAME_LITERAL_BRACE = 318,
     BLANK_LITERAL = 319,
     IDENTIFIER = 320
   };
#endif
/* Tokens.  */
#define SELECT 258
#define FROM 259
#define WHERE 260
#define OPTIONAL 261
#define DESCRIBE 262
#define CONSTRUCT 263
#define ASK 264
#define DISTINCT 265
#define REDUCED 266
#define LIMIT 267
#define UNION 268
#define PREFIX 269
#define BASE 270
#define BOUND 271
#define GRAPH 272
#define NAMED 273
#define FILTER 274
#define OFFSET 275
#define ORDER 276
#define BY 277
#define REGEX 278
#define ASC 279
#define DESC 280
#define LANGMATCHES 281
#define A 282
#define STR 283
#define LANG 284
#define DATATYPE 285
#define ISURI 286
#define ISBLANK 287
#define ISLITERAL 288
#define SAMETERM 289
#define EXPLAIN 290
#define GROUP 291
#define COUNT 292
#define AS 293
#define DELETE 294
#define INSERT 295
#define SC_OR 296
#define SC_AND 297
#define EQ 298
#define NEQ 299
#define LT 300
#define GT 301
#define LE 302
#define GE 303
#define STRING_LITERAL 304
#define DOUBLE_LITERAL 305
#define DOUBLE_POSITIVE_LITERAL 306
#define DOUBLE_NEGATIVE_LITERAL 307
#define INTEGER_LITERAL 308
#define INTEGER_POSITIVE_LITERAL 309
#define INTEGER_NEGATIVE_LITERAL 310
#define DECIMAL_LITERAL 311
#define DECIMAL_POSITIVE_LITERAL 312
#define DECIMAL_NEGATIVE_LITERAL 313
#define BOOLEAN_LITERAL 314
#define URI_LITERAL 315
#define URI_LITERAL_BRACE 316
#define QNAME_LITERAL 317
#define QNAME_LITERAL_BRACE 318
#define BLANK_LITERAL 319
#define IDENTIFIER 320




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 116 "./sparql_parser.y"
{
  raptor_sequence *seq;
  rasqal_variable *variable;
  rasqal_literal *literal;
  rasqal_triple *triple;
  rasqal_expression *expr;
  rasqal_graph_pattern *graph_pattern;
  double floating;
  raptor_uri *uri;
  unsigned char *name;
  rasqal_formula *formula;
}
/* Line 1489 of yacc.c.  */
#line 192 "sparql_parser.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



