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
     A = 258,
     AT = 259,
     HAT = 260,
     DOT = 261,
     COMMA = 262,
     SEMICOLON = 263,
     LEFT_SQUARE = 264,
     RIGHT_SQUARE = 265,
     LEFT_ROUND = 266,
     RIGHT_ROUND = 267,
     LEFT_CURLY = 268,
     RIGHT_CURLY = 269,
     COLONMINUS = 270,
     TRUE_TOKEN = 271,
     FALSE_TOKEN = 272,
     STRING_LITERAL = 273,
     URI_LITERAL = 274,
     BLANK_LITERAL = 275,
     QNAME_LITERAL = 276,
     PREFIX = 277,
     BASE = 278,
     IDENTIFIER = 279,
     INTEGER_LITERAL = 280,
     FLOATING_LITERAL = 281,
     DECIMAL_LITERAL = 282,
     ERROR_TOKEN = 283
   };
#endif
/* Tokens.  */
#define A 258
#define AT 259
#define HAT 260
#define DOT 261
#define COMMA 262
#define SEMICOLON 263
#define LEFT_SQUARE 264
#define RIGHT_SQUARE 265
#define LEFT_ROUND 266
#define RIGHT_ROUND 267
#define LEFT_CURLY 268
#define RIGHT_CURLY 269
#define COLONMINUS 270
#define TRUE_TOKEN 271
#define FALSE_TOKEN 272
#define STRING_LITERAL 273
#define URI_LITERAL 274
#define BLANK_LITERAL 275
#define QNAME_LITERAL 276
#define PREFIX 277
#define BASE 278
#define IDENTIFIER 279
#define INTEGER_LITERAL 280
#define FLOATING_LITERAL 281
#define DECIMAL_LITERAL 282
#define ERROR_TOKEN 283




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 122 "./turtle_parser.y"
{
  unsigned char *string;
  raptor_identifier *identifier;
  raptor_sequence *sequence;
  raptor_uri *uri;
  int integer; /* 0+ for a xsd:integer datatyped RDF literal */
}
/* Line 1489 of yacc.c.  */
#line 113 "turtle_parser.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



