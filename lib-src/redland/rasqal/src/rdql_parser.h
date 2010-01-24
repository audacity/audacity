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
     SOURCE = 259,
     FROM = 260,
     WHERE = 261,
     AND = 262,
     FOR = 263,
     USING = 264,
     SC_OR = 266,
     SC_AND = 268,
     STR_EQ = 270,
     STR_NE = 272,
     STR_MATCH = 274,
     STR_NMATCH = 276,
     EQ = 278,
     NEQ = 280,
     LT = 282,
     GT = 284,
     LE = 286,
     GE = 288,
     FLOATING_POINT_LITERAL = 289,
     STRING_LITERAL = 290,
     INTEGER_LITERAL = 291,
     PATTERN_LITERAL = 292,
     BOOLEAN_LITERAL = 293,
     NULL_LITERAL = 294,
     URI_LITERAL = 295,
     QNAME_LITERAL = 296,
     IDENTIFIER = 297
   };
#endif
/* Tokens.  */
#define SELECT 258
#define SOURCE 259
#define FROM 260
#define WHERE 261
#define AND 262
#define FOR 263
#define USING 264
#define SC_OR 266
#define SC_AND 268
#define STR_EQ 270
#define STR_NE 272
#define STR_MATCH 274
#define STR_NMATCH 276
#define EQ 278
#define NEQ 280
#define LT 282
#define GT 284
#define LE 286
#define GE 288
#define FLOATING_POINT_LITERAL 289
#define STRING_LITERAL 290
#define INTEGER_LITERAL 291
#define PATTERN_LITERAL 292
#define BOOLEAN_LITERAL 293
#define NULL_LITERAL 294
#define URI_LITERAL 295
#define QNAME_LITERAL 296
#define IDENTIFIER 297




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 100 "./rdql_parser.y"
{
  raptor_sequence *seq;
  rasqal_variable *variable;
  rasqal_literal *literal;
  rasqal_triple *triple;
  rasqal_expression *expr;
  double floating;
  raptor_uri *uri;
  unsigned char *name;
}
/* Line 1489 of yacc.c.  */
#line 120 "rdql_parser.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



