/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse sparql_parser_parse
#define yylex   sparql_parser_lex
#define yyerror sparql_parser_error
#define yylval  sparql_parser_lval
#define yychar  sparql_parser_char
#define yydebug sparql_parser_debug
#define yynerrs sparql_parser_nerrs


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




/* Copy the first part of user declarations.  */
#line 32 "./sparql_parser.y"

#ifdef HAVE_CONFIG_H
#include <rasqal_config.h>
#endif

#ifdef WIN32
#include <win32_rasqal_config.h>
#endif

#include <stdio.h>
#include <stdarg.h>

#include <rasqal.h>
#include <rasqal_internal.h>

#include <sparql_parser.h>

#define YY_DECL int sparql_lexer_lex (YYSTYPE *sparql_parser_lval, yyscan_t yyscanner)
#define YY_NO_UNISTD_H 1
#include <sparql_lexer.h>

#include <sparql_common.h>

/*
#undef RASQAL_DEBUG
#define RASQAL_DEBUG 2
*/

#define DEBUG_FH stdout

/* Make verbose error messages for syntax errors */
#define YYERROR_VERBOSE 1

/* Fail with an debug error message if RASQAL_DEBUG > 1 */
#if RASQAL_DEBUG > 1
#define YYERROR_MSG(msg) do { fputs("** YYERROR ", DEBUG_FH); fputs(msg, DEBUG_FH); fputc('\n', DEBUG_FH); YYERROR; } while(0)
#else
#define YYERROR_MSG(ignore) YYERROR
#endif

/* Slow down the grammar operation and watch it work */
#if RASQAL_DEBUG > 2
#define YYDEBUG 1
#endif

/* the lexer does not seem to track this */
#undef RASQAL_SPARQL_USE_ERROR_COLUMNS

/* Missing sparql_lexer.c/h prototypes */
int sparql_lexer_get_column(yyscan_t yyscanner);
/* Not used here */
/* void sparql_lexer_set_column(int  column_no , yyscan_t yyscanner);*/


/* What the lexer wants */
extern int sparql_lexer_lex (YYSTYPE *sparql_parser_lval, yyscan_t scanner);
#define YYLEX_PARAM ((rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context))->scanner

/* Pure parser argument (a void*) */
#define YYPARSE_PARAM rq

/* Make the yyerror below use the rdf_parser */
#undef yyerror
#define yyerror(message) sparql_query_error((rasqal_query*)rq, message)

/* Make lex/yacc interface as small as possible */
#undef yylex
#define yylex sparql_lexer_lex


static int sparql_parse(rasqal_query* rq);
static void sparql_query_error(rasqal_query* rq, const char *message);
static void sparql_query_error_full(rasqal_query *rq, const char *message, ...) RASQAL_PRINTF_FORMAT(2, 3);



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

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
/* Line 187 of yacc.c.  */
#line 323 "sparql_parser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 336 "sparql_parser.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   756

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  82
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  86
/* YYNRULES -- Number of rules.  */
#define YYNRULES  196
/* YYNRULES -- Number of states.  */
#define YYNSTATES  330

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   320

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    81,     2,     2,    49,     2,     2,     2,
      42,    43,    60,    58,    41,    59,    79,    61,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    80,
       2,     2,     2,    48,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    44,     2,    45,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    46,     2,    47,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    50,    51,    52,    53,
      54,    55,    56,    57,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     7,     9,    10,    12,    14,    16,    18,
      20,    22,    25,    28,    29,    34,    35,    42,    49,    55,
      57,    59,    62,    66,    68,    70,    74,    76,    80,    84,
      86,    91,    96,   102,   108,   111,   115,   117,   123,   127,
     131,   135,   139,   143,   144,   146,   149,   151,   154,   156,
     157,   161,   165,   166,   169,   172,   174,   176,   177,   181,
     182,   185,   187,   190,   193,   195,   197,   199,   201,   204,
     207,   211,   213,   214,   219,   221,   226,   228,   231,   232,
     235,   238,   239,   241,   243,   245,   248,   252,   256,   258,
     262,   264,   267,   269,   271,   273,   277,   281,   283,   284,
     288,   292,   294,   295,   298,   301,   305,   308,   309,   311,
     312,   315,   318,   319,   321,   323,   325,   327,   329,   333,
     337,   340,   342,   344,   346,   348,   350,   352,   354,   357,
     360,   362,   364,   366,   368,   370,   372,   375,   377,   381,
     383,   387,   389,   393,   397,   401,   405,   409,   413,   415,
     419,   423,   426,   429,   431,   435,   439,   441,   444,   447,
     450,   452,   454,   456,   458,   460,   462,   466,   471,   476,
     483,   488,   493,   500,   505,   510,   515,   517,   524,   533,
     535,   537,   539,   541,   543,   545,   547,   549,   551,   553,
     555,   557,   559,   561,   563,   565,   567
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      83,     0,    -1,    86,    84,    85,    -1,    35,    -1,    -1,
      89,    -1,    98,    -1,    96,    -1,    99,    -1,   100,    -1,
     101,    -1,    87,    88,    -1,    15,    73,    -1,    -1,    88,
      14,    78,    73,    -1,    -1,     3,    10,    90,   102,   106,
     107,    -1,     3,    11,    90,   102,   106,   107,    -1,     3,
      90,   102,   106,   107,    -1,    91,    -1,    60,    -1,    91,
      92,    -1,    91,    41,    92,    -1,    92,    -1,   147,    -1,
      93,    38,   148,    -1,    94,    -1,    42,    94,    43,    -1,
      42,   150,    43,    -1,    95,    -1,    37,    42,   150,    43,
      -1,    37,    42,    60,    43,    -1,     7,    97,   102,   106,
     107,    -1,     7,    60,   102,   106,   107,    -1,    97,   146,
      -1,    97,    41,   146,    -1,   146,    -1,     8,   130,   102,
     106,   107,    -1,     9,   102,   106,    -1,    39,   102,   106,
      -1,    40,   102,   106,    -1,   102,     4,   103,    -1,   102,
       4,   104,    -1,    -1,   105,    -1,    18,   105,    -1,   166,
      -1,     5,   115,    -1,   115,    -1,    -1,   108,   110,   109,
      -1,    36,    22,   111,    -1,    -1,   113,   114,    -1,   114,
     113,    -1,   113,    -1,   114,    -1,    -1,    21,    22,   111,
      -1,    -1,   111,   112,    -1,   112,    -1,    24,   158,    -1,
      25,   158,    -1,   128,    -1,   147,    -1,   158,    -1,   159,
      -1,    12,    66,    -1,    20,    66,    -1,    46,   117,    47,
      -1,    79,    -1,    -1,   118,   121,   116,   117,    -1,   118,
      -1,   119,   126,   116,   118,    -1,   119,    -1,   132,   120,
      -1,    -1,   120,   132,    -1,   120,    79,    -1,    -1,   122,
      -1,   124,    -1,   123,    -1,     6,   115,    -1,    17,   146,
     115,    -1,   115,    13,   125,    -1,   115,    -1,   125,    13,
     115,    -1,   115,    -1,    19,   127,    -1,   158,    -1,   159,
      -1,   128,    -1,   161,   129,    43,    -1,   129,    41,   150,
      -1,   150,    -1,    -1,    46,   131,    47,    -1,   132,    79,
     131,    -1,   132,    -1,    -1,   145,   133,    -1,   140,   135,
      -1,   139,   136,   134,    -1,    80,   135,    -1,    -1,   133,
      -1,    -1,   138,   137,    -1,    41,   136,    -1,    -1,   144,
      -1,   146,    -1,    27,    -1,   142,    -1,   141,    -1,    44,
     133,    45,    -1,    42,   143,    43,    -1,   143,   144,    -1,
     144,    -1,   145,    -1,   140,    -1,   147,    -1,   149,    -1,
     147,    -1,   166,    -1,    48,   148,    -1,    49,   148,    -1,
      78,    -1,   166,    -1,    62,    -1,   162,    -1,    72,    -1,
     167,    -1,    42,    43,    -1,   151,    -1,   151,    50,   152,
      -1,   152,    -1,   152,    51,   153,    -1,   153,    -1,   154,
      52,   154,    -1,   154,    53,   154,    -1,   154,    54,   154,
      -1,   154,    55,   154,    -1,   154,    56,   154,    -1,   154,
      57,   154,    -1,   154,    -1,   155,    58,   154,    -1,   155,
      59,   154,    -1,   155,   164,    -1,   155,   165,    -1,   155,
      -1,   156,    60,   155,    -1,   156,    61,   155,    -1,   156,
      -1,    81,   157,    -1,    58,   157,    -1,    59,   157,    -1,
     157,    -1,   158,    -1,   159,    -1,   128,    -1,   149,    -1,
     147,    -1,    42,   150,    43,    -1,    28,    42,   150,    43,
      -1,    29,    42,   150,    43,    -1,    26,    42,   150,    41,
     150,    43,    -1,    30,    42,   150,    43,    -1,    16,    42,
     147,    43,    -1,    34,    42,   150,    41,   150,    43,    -1,
      31,    42,   150,    43,    -1,    32,    42,   150,    43,    -1,
      33,    42,   150,    43,    -1,   160,    -1,    23,    42,   150,
      41,   150,    43,    -1,    23,    42,   150,    41,   150,    41,
     150,    43,    -1,    74,    -1,    76,    -1,   163,    -1,   164,
      -1,   165,    -1,    66,    -1,    69,    -1,    63,    -1,    67,
      -1,    70,    -1,    64,    -1,    68,    -1,    71,    -1,    65,
      -1,    73,    -1,    75,    -1,    77,    -1,    44,    45,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   295,   295,   301,   311,   318,   323,   328,   333,   337,
     341,   349,   357,   362,   369,   397,   404,   410,   416,   427,
     431,   442,   451,   460,   478,   482,   503,   507,   511,   518,
     525,   538,   558,   563,   572,   581,   590,   605,   614,   623,
     635,   647,   648,   649,   654,   670,   686,   694,   698,   702,
     707,   712,   736,   741,   742,   743,   744,   746,   752,   760,
     765,   775,   794,   800,   806,   813,   829,   836,   847,   863,
     878,   886,   887,   892,   939,   980,  1038,  1087,  1116,  1126,
    1153,  1158,  1165,  1169,  1173,  1181,  1201,  1226,  1241,  1248,
    1258,  1282,  1290,  1294,  1298,  1306,  1337,  1347,  1363,  1370,
    1378,  1407,  1419,  1426,  1469,  1516,  1611,  1616,  1623,  1628,
    1635,  1712,  1717,  1724,  1732,  1742,  1769,  1773,  1781,  1847,
    1967,  2005,  2041,  2045,  2053,  2065,  2078,  2084,  2092,  2096,
    2103,  2114,  2118,  2122,  2126,  2130,  2134,  2143,  2151,  2157,
    2165,  2172,  2181,  2187,  2193,  2199,  2205,  2211,  2217,  2226,
    2232,  2238,  2249,  2260,  2267,  2273,  2279,  2287,  2293,  2297,
    2303,  2317,  2321,  2325,  2334,  2340,  2353,  2361,  2367,  2373,
    2379,  2385,  2399,  2405,  2411,  2417,  2423,  2431,  2437,  2450,
    2456,  2475,  2479,  2483,  2490,  2494,  2498,  2506,  2510,  2514,
    2522,  2526,  2530,  2542,  2548,  2567,  2572
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SELECT", "FROM", "WHERE", "OPTIONAL",
  "DESCRIBE", "CONSTRUCT", "ASK", "DISTINCT", "REDUCED", "LIMIT", "UNION",
  "PREFIX", "BASE", "BOUND", "GRAPH", "NAMED", "FILTER", "OFFSET", "ORDER",
  "BY", "REGEX", "ASC", "DESC", "LANGMATCHES", "\"a\"", "\"str\"",
  "\"lang\"", "\"datatype\"", "\"isUri\"", "\"isBlank\"", "\"isLiteral\"",
  "\"sameTerm\"", "EXPLAIN", "GROUP", "COUNT", "AS", "DELETE", "INSERT",
  "','", "'('", "')'", "'['", "']'", "'{'", "'}'", "'?'", "'$'", "SC_OR",
  "SC_AND", "EQ", "NEQ", "LT", "GT", "LE", "GE", "'+'", "'-'", "'*'",
  "'/'", "\"string literal\"", "\"double literal\"",
  "\"double positive literal\"", "\"double negative literal\"",
  "\"integer literal\"", "\"integer positive literal\"",
  "\"integer negative literal\"", "\"decimal literal\"",
  "\"decimal positive literal\"", "\"decimal negative literal\"",
  "\"boolean literal\"", "\"URI literal\"", "\"URI literal (\"",
  "\"QName literal\"", "\"QName literal (\"", "\"blank node literal\"",
  "\"identifier\"", "'.'", "';'", "'!'", "$accept", "Query", "ExplainOpt",
  "ReportFormat", "Prologue", "BaseDeclOpt", "PrefixDeclListOpt",
  "SelectQuery", "SelectExpressionList", "SelectExpressionListTail",
  "SelectTerm", "SelectExpression", "AggregateExpression",
  "CountAggregateExpression", "DescribeQuery", "VarOrIRIrefList",
  "ConstructQuery", "AskQuery", "DeleteQuery", "InsertQuery",
  "DatasetClauseListOpt", "DefaultGraphClause", "NamedGraphClause",
  "SourceSelector", "WhereClauseOpt", "SolutionModifier", "GroupClauseOpt",
  "LimitOffsetClausesOpt", "OrderClauseOpt", "OrderConditionList",
  "OrderCondition", "LimitClause", "OffsetClause", "GroupGraphPattern",
  "DotOptional", "GraphPattern", "FilteredBasicGraphPattern",
  "TriplesBlockOpt", "TriplesSameSubjectDotListOpt",
  "GraphPatternNotTriples", "OptionalGraphPattern", "GraphGraphPattern",
  "GroupOrUnionGraphPattern", "GroupOrUnionGraphPatternList", "Filter",
  "Constraint", "FunctionCall", "ArgList", "ConstructTemplate",
  "ConstructTriplesOpt", "TriplesSameSubject", "PropertyListNotEmpty",
  "PropertyListTailOpt", "PropertyList", "ObjectList", "ObjectTail",
  "Object", "Verb", "TriplesNode", "BlankNodePropertyList", "Collection",
  "GraphNodeListNotEmpty", "GraphNode", "VarOrTerm", "VarOrIRIref", "Var",
  "VarName", "GraphTerm", "Expression", "ConditionalOrExpression",
  "ConditionalAndExpression", "RelationalExpression", "AdditiveExpression",
  "MultiplicativeExpression", "UnaryExpression", "PrimaryExpression",
  "BrackettedExpression", "BuiltInCall", "RegexExpression", "IRIrefBrace",
  "NumericLiteral", "NumericLiteralUnsigned", "NumericLiteralPositive",
  "NumericLiteralNegative", "IRIref", "BlankNode", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,    44,    40,    41,    91,    93,   123,   125,    63,    36,
     296,   297,   298,   299,   300,   301,   302,   303,    43,    45,
      42,    47,   304,   305,   306,   307,   308,   309,   310,   311,
     312,   313,   314,   315,   316,   317,   318,   319,   320,    46,
      59,    33
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    82,    83,    84,    84,    85,    85,    85,    85,    85,
      85,    86,    87,    87,    88,    88,    89,    89,    89,    90,
      90,    91,    91,    91,    92,    92,    93,    93,    93,    94,
      95,    95,    96,    96,    97,    97,    97,    98,    99,   100,
     101,   102,   102,   102,   103,   104,   105,   106,   106,   106,
     107,   108,   108,   109,   109,   109,   109,   109,   110,   110,
     111,   111,   112,   112,   112,   112,   112,   112,   113,   114,
     115,   116,   116,   117,   117,   118,   118,   119,   119,   120,
     120,   120,   121,   121,   121,   122,   123,   124,   124,   125,
     125,   126,   127,   127,   127,   128,   129,   129,   129,   130,
     131,   131,   131,   132,   132,   133,   134,   134,   135,   135,
     136,   137,   137,   138,   139,   139,   140,   140,   141,   142,
     143,   143,   144,   144,   145,   145,   146,   146,   147,   147,
     148,   149,   149,   149,   149,   149,   149,   150,   151,   151,
     152,   152,   153,   153,   153,   153,   153,   153,   153,   154,
     154,   154,   154,   154,   155,   155,   155,   156,   156,   156,
     156,   157,   157,   157,   157,   157,   158,   159,   159,   159,
     159,   159,   159,   159,   159,   159,   159,   160,   160,   161,
     161,   162,   162,   162,   163,   163,   163,   164,   164,   164,
     165,   165,   165,   166,   166,   167,   167
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     1,     0,     1,     1,     1,     1,     1,
       1,     2,     2,     0,     4,     0,     6,     6,     5,     1,
       1,     2,     3,     1,     1,     3,     1,     3,     3,     1,
       4,     4,     5,     5,     2,     3,     1,     5,     3,     3,
       3,     3,     3,     0,     1,     2,     1,     2,     1,     0,
       3,     3,     0,     2,     2,     1,     1,     0,     3,     0,
       2,     1,     2,     2,     1,     1,     1,     1,     2,     2,
       3,     1,     0,     4,     1,     4,     1,     2,     0,     2,
       2,     0,     1,     1,     1,     2,     3,     3,     1,     3,
       1,     2,     1,     1,     1,     3,     3,     1,     0,     3,
       3,     1,     0,     2,     2,     3,     2,     0,     1,     0,
       2,     2,     0,     1,     1,     1,     1,     1,     3,     3,
       2,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     2,     1,     3,     1,
       3,     1,     3,     3,     3,     3,     3,     3,     1,     3,
       3,     2,     2,     1,     3,     3,     1,     2,     2,     2,
       1,     1,     1,     1,     1,     1,     3,     4,     4,     6,
       4,     4,     6,     4,     4,     4,     1,     6,     8,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      13,     0,     0,     4,    15,    12,     1,     3,     0,    11,
       0,     0,     0,    43,    43,    43,     2,     5,     7,     6,
       8,     9,    10,     0,     0,     0,     0,     0,     0,     0,
      20,    43,    19,    23,     0,    26,    29,    24,    43,   193,
     194,    43,    36,   126,   127,   102,    43,    49,    49,    49,
       0,    43,    43,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,   186,
     189,   192,   184,   187,   190,   185,   188,   191,   134,   179,
     180,   195,     0,     0,   163,   165,   164,     0,   137,   139,
     141,   148,   153,   156,   160,   161,   162,   176,    98,   133,
     181,   182,   183,   131,   135,   130,   128,   129,    49,     0,
      21,     0,    49,     0,    49,    34,     0,     0,     0,   101,
     109,   117,   116,     0,   124,   125,    49,     0,     0,    78,
      38,    48,    39,    40,    14,    49,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   136,
       0,   196,   158,   159,   157,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   151,   152,     0,
       0,     0,    97,    52,    22,    25,    52,    35,    52,   123,
       0,   121,   122,   115,     0,     0,   114,    99,   102,   108,
     104,   103,    52,     0,    41,    42,    44,    46,    47,     0,
      74,    76,    81,    52,    52,    31,    30,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   166,   138,   140,
     142,   143,   144,   145,   146,   147,   149,   150,   154,   155,
       0,    95,     0,    18,    59,    33,    32,   119,   120,   118,
     107,   112,   113,   100,    37,    45,    70,     0,     0,    88,
      72,    82,    84,    83,     0,    72,    77,    16,    17,   171,
       0,     0,   167,   168,   170,   173,   174,   175,     0,    96,
       0,     0,    57,   109,   105,     0,   110,    85,     0,     0,
      71,    78,     0,    91,    94,    92,    93,    78,    80,    79,
       0,     0,     0,     0,     0,    51,    61,    64,    65,    66,
      67,     0,     0,     0,    50,    55,    56,   106,   111,    86,
      90,    87,    73,    75,     0,   177,   169,   172,    62,    63,
      60,    58,    68,    69,    53,    54,     0,     0,    89,   178
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     8,    16,     3,     4,     9,    17,    31,    32,
      33,    34,    35,    36,    18,    41,    19,    20,    21,    22,
      47,   194,   195,   196,   130,   233,   234,   304,   272,   295,
     296,   305,   306,   131,   281,   199,   200,   201,   256,   250,
     251,   252,   253,   311,   255,   283,    84,   171,    46,   118,
     202,   189,   274,   190,   240,   276,   241,   185,   120,   121,
     122,   180,   242,   123,   186,    85,   106,    86,   150,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -275
static const yytype_int16 yypact[] =
{
      62,   -56,    90,    60,  -275,  -275,  -275,  -275,    93,    84,
      33,   148,    59,  -275,  -275,  -275,  -275,  -275,  -275,  -275,
    -275,  -275,  -275,    31,   208,   208,    75,   270,    56,    56,
    -275,  -275,   188,  -275,    98,  -275,  -275,  -275,  -275,  -275,
    -275,   153,  -275,  -275,  -275,   679,  -275,    14,    14,    14,
      65,  -275,  -275,   332,   103,   110,   112,   115,   118,   132,
     134,   135,   137,   150,   394,   155,   518,   518,  -275,  -275,
    -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,
    -275,  -275,   518,   160,  -275,  -275,  -275,   161,   162,   147,
    -275,   315,   -13,   -19,  -275,  -275,  -275,  -275,   456,  -275,
    -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,    14,   268,
    -275,    56,    14,   169,    14,  -275,   611,   120,   164,   128,
      67,  -275,  -275,    67,  -275,  -275,    14,    16,   167,   679,
    -275,  -275,  -275,  -275,  -275,    14,    14,   172,   173,    95,
     456,   456,   456,   456,   456,   456,   456,   456,   456,  -275,
     176,  -275,  -275,  -275,  -275,  -275,  -275,   456,   456,   456,
     456,   456,   456,   456,   456,   456,   456,  -275,  -275,   456,
     456,   -14,  -275,   184,  -275,  -275,   184,  -275,   184,  -275,
     647,  -275,  -275,  -275,   177,   679,  -275,  -275,   679,  -275,
    -275,  -275,   184,   -11,  -275,  -275,  -275,  -275,  -275,   180,
      20,   213,  -275,   184,   184,  -275,  -275,   191,   194,   210,
     209,   211,   215,   216,   218,   219,   212,  -275,   147,  -275,
    -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,
     456,  -275,   245,  -275,   248,  -275,  -275,  -275,  -275,  -275,
     190,   231,  -275,  -275,  -275,  -275,  -275,   167,   169,   261,
     196,  -275,  -275,  -275,   250,   196,   573,  -275,  -275,  -275,
     456,   456,  -275,  -275,  -275,  -275,  -275,  -275,   456,  -275,
     157,   265,    18,    67,  -275,   679,  -275,  -275,   167,   167,
    -275,   679,   456,  -275,  -275,  -275,  -275,   679,  -275,  -275,
      22,   246,   247,   253,   253,   157,  -275,  -275,  -275,  -275,
    -275,   157,   222,   240,  -275,   277,   296,  -275,  -275,  -275,
    -275,   300,  -275,  -275,   456,  -275,  -275,  -275,  -275,  -275,
    -275,   157,  -275,  -275,  -275,  -275,   167,   266,  -275,  -275
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,   126,  -275,
     -24,  -275,   288,  -275,  -275,  -275,  -275,  -275,  -275,  -275,
     582,  -275,  -275,   127,    23,   -37,  -275,  -275,  -275,    21,
    -274,    15,    25,  -117,    68,    44,    40,  -275,  -275,  -275,
    -275,  -275,  -275,  -275,  -275,  -275,  -242,  -275,  -275,   143,
     -42,   -31,  -275,    76,    77,  -275,  -275,  -275,  -112,  -275,
    -275,  -275,  -100,  -111,    -1,   -10,   -23,   -32,   -20,  -275,
     193,   195,   223,     2,  -275,   -43,  -245,  -234,  -275,  -275,
    -275,  -275,   262,   264,    -9,  -275
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint16 yytable[] =
{
      37,    43,    44,   119,   179,   182,   107,    87,   110,   285,
      42,   198,   284,   125,    37,    37,   181,     5,   127,   128,
     286,   320,    37,   152,   153,   299,   247,   230,   297,   231,
     302,    43,    44,   138,   193,   124,   300,   248,   303,   154,
     115,   169,   170,    24,    25,   165,   166,   320,   318,   319,
     299,    70,    71,   297,    73,    74,   299,    76,    77,   297,
     129,   300,    39,   314,    40,   315,   129,   300,   179,   182,
      26,   132,   133,   179,   182,    27,   299,     1,   172,   297,
     238,    28,    29,   249,   125,   174,   184,   300,   175,    39,
       6,    40,   191,    30,   183,     7,    10,   125,    23,    37,
      11,    12,    13,    43,    44,    45,   124,    43,    44,    50,
      43,    44,   177,    43,    44,    28,    29,    53,   197,   124,
     208,   209,   210,   211,   212,   213,   214,   215,   216,   207,
     277,   173,    14,    15,   105,   176,   111,   178,   134,   235,
      39,   236,    40,    28,    29,   139,   119,   183,   125,   192,
      51,    52,   140,   125,   141,   244,   125,   142,   203,   204,
     143,   309,   310,   179,   182,   151,   257,   258,    28,    29,
     124,   228,   229,    54,   144,   124,   145,   146,   124,   147,
      55,   293,   294,    56,   197,    57,    58,    59,    60,    61,
      62,    63,   148,    39,   113,    40,    28,    29,   158,   282,
     151,    28,    29,   155,   156,    28,    29,   188,    38,   328,
     269,   187,   157,   129,   289,   205,   206,    28,    29,   217,
     232,    39,   239,    40,   125,    26,    39,   246,    40,   109,
      27,    79,   254,    80,   259,   260,    28,    29,    43,    44,
     290,   291,    39,   125,    40,    26,   124,   278,   292,   125,
      27,   261,   262,   268,   263,   125,    28,    29,   264,   265,
     298,   266,   267,    43,    44,   124,    54,   270,    30,   271,
     273,   124,   275,    55,   279,   280,    56,   124,    57,    58,
      59,    60,    61,    62,    63,   298,    54,   301,   322,   316,
     317,   298,   282,    55,   327,   282,    56,   303,    57,    58,
      59,    60,    61,    62,    63,    26,   323,    26,   302,   329,
      27,   298,    64,   326,    65,    83,    28,    29,    28,    29,
     245,   325,   321,   287,    79,   312,    80,   313,    66,    67,
     324,   243,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    39,    79,    40,    80,    81,    54,   307,
     218,    82,   308,   219,   167,    55,   168,     0,    56,     0,
      57,    58,    59,    60,    61,    62,    63,   159,   160,   161,
     162,   163,   164,     0,    64,     0,    65,     0,     0,     0,
      28,    29,   220,   221,   222,   223,   224,   225,   226,   227,
      66,    67,   137,     0,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    39,    79,    40,    80,    81,
      54,     0,     0,    82,     0,     0,     0,    55,     0,     0,
      56,     0,    57,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,    64,   149,    65,     0,
       0,     0,    28,    29,     0,     0,     0,     0,     0,     0,
       0,     0,    66,    67,     0,     0,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    39,    79,    40,
      80,    81,    54,     0,     0,    82,     0,     0,     0,    55,
       0,     0,    56,     0,    57,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,    64,     0,
      65,     0,     0,     0,    28,    29,     0,     0,     0,     0,
       0,     0,     0,     0,    66,    67,     0,     0,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    39,
      79,    40,    80,    81,    54,     0,     0,    82,     0,     0,
       0,    55,     0,     0,    56,     0,    57,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
      64,     0,    65,     0,     0,     0,    28,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    39,    79,    40,    80,    81,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,   116,     0,   117,     0,     0,
     112,    28,    29,   114,     0,     0,     0,     0,   126,     0,
       0,     0,     0,   135,   136,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    39,     0,    40,     0,
      81,     0,   288,   116,   149,   117,     0,     0,     0,    28,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    39,     0,    40,     0,    81,   116,
     237,   117,     0,     0,     0,    28,    29,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      39,   116,    40,   117,    81,     0,     0,    28,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    39,     0,    40,     0,    81
};

static const yytype_int16 yycheck[] =
{
      10,    11,    11,    45,   116,   116,    29,    27,    32,   254,
      11,   128,   254,    45,    24,    25,   116,    73,     4,     5,
     254,   295,    32,    66,    67,   270,     6,    41,   270,    43,
      12,    41,    41,    53,    18,    45,   270,    17,    20,    82,
      41,    60,    61,    10,    11,    58,    59,   321,   293,   294,
     295,    64,    65,   295,    67,    68,   301,    70,    71,   301,
      46,   295,    73,    41,    75,    43,    46,   301,   180,   180,
      37,    48,    49,   185,   185,    42,   321,    15,    98,   321,
     180,    48,    49,   200,   116,   109,   117,   321,   111,    73,
       0,    75,   123,    60,    27,    35,     3,   129,    14,   109,
       7,     8,     9,   113,   113,    46,   116,   117,   117,    78,
     120,   120,   113,   123,   123,    48,    49,    42,   127,   129,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   139,
     247,   108,    39,    40,    78,   112,    38,   114,    73,   176,
      73,   178,    75,    48,    49,    42,   188,    27,   180,   126,
      24,    25,    42,   185,    42,   192,   188,    42,   135,   136,
      42,   278,   279,   275,   275,    45,   203,   204,    48,    49,
     180,   169,   170,    16,    42,   185,    42,    42,   188,    42,
      23,    24,    25,    26,   193,    28,    29,    30,    31,    32,
      33,    34,    42,    73,    41,    75,    48,    49,    51,    42,
      45,    48,    49,    43,    43,    48,    49,    79,    60,   326,
     230,    47,    50,    46,   256,    43,    43,    48,    49,    43,
      36,    73,    45,    75,   256,    37,    73,    47,    75,    41,
      42,    74,    19,    76,    43,    41,    48,    49,   248,   248,
     260,   261,    73,   275,    75,    37,   256,   248,   268,   281,
      42,    41,    43,    41,    43,   287,    48,    49,    43,    43,
     270,    43,    43,   273,   273,   275,    16,    22,    60,    21,
      80,   281,    41,    23,    13,    79,    26,   287,    28,    29,
      30,    31,    32,    33,    34,   295,    16,    22,    66,    43,
      43,   301,    42,    23,   314,    42,    26,    20,    28,    29,
      30,    31,    32,    33,    34,    37,    66,    37,    12,    43,
      42,   321,    42,    13,    44,    27,    48,    49,    48,    49,
     193,   306,   301,   255,    74,   281,    76,   287,    58,    59,
     305,   188,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    16,   273,
     157,    81,   275,   158,    92,    23,    92,    -1,    26,    -1,
      28,    29,    30,    31,    32,    33,    34,    52,    53,    54,
      55,    56,    57,    -1,    42,    -1,    44,    -1,    -1,    -1,
      48,    49,   159,   160,   161,   162,   163,   164,   165,   166,
      58,    59,    60,    -1,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      16,    -1,    -1,    81,    -1,    -1,    -1,    23,    -1,    -1,
      26,    -1,    28,    29,    30,    31,    32,    33,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    43,    44,    -1,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    59,    -1,    -1,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    16,    -1,    -1,    81,    -1,    -1,    -1,    23,
      -1,    -1,    26,    -1,    28,    29,    30,    31,    32,    33,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      44,    -1,    -1,    -1,    48,    49,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    58,    59,    -1,    -1,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    16,    -1,    -1,    81,    -1,    -1,
      -1,    23,    -1,    -1,    26,    -1,    28,    29,    30,    31,
      32,    33,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    44,    -1,    -1,    -1,    48,    49,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    14,    15,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    31,    -1,    42,    -1,    44,    -1,    -1,
      38,    48,    49,    41,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    -1,    75,    -1,
      77,    -1,    79,    42,    43,    44,    -1,    -1,    -1,    48,
      49,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    -1,    75,    -1,    77,    42,
      43,    44,    -1,    -1,    -1,    48,    49,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    42,    75,    44,    77,    -1,    -1,    48,    49,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    -1,    75,    -1,    77
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    15,    83,    86,    87,    73,     0,    35,    84,    88,
       3,     7,     8,     9,    39,    40,    85,    89,    96,    98,
      99,   100,   101,    14,    10,    11,    37,    42,    48,    49,
      60,    90,    91,    92,    93,    94,    95,   147,    60,    73,
      75,    97,   146,   147,   166,    46,   130,   102,   102,   102,
      78,    90,    90,    42,    16,    23,    26,    28,    29,    30,
      31,    32,    33,    34,    42,    44,    58,    59,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    74,
      76,    77,    81,    94,   128,   147,   149,   150,   151,   152,
     153,   154,   155,   156,   157,   158,   159,   160,   161,   162,
     163,   164,   165,   166,   167,    78,   148,   148,   102,    41,
      92,    38,   102,    41,   102,   146,    42,    44,   131,   132,
     140,   141,   142,   145,   147,   149,   102,     4,     5,    46,
     106,   115,   106,   106,    73,   102,   102,    60,   150,    42,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    43,
     150,    45,   157,   157,   157,    43,    43,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,   164,   165,    60,
      61,   129,   150,   106,    92,   148,   106,   146,   106,   140,
     143,   144,   145,    27,   133,   139,   146,    47,    79,   133,
     135,   133,   106,    18,   103,   104,   105,   166,   115,   117,
     118,   119,   132,   106,   106,    43,    43,   147,   150,   150,
     150,   150,   150,   150,   150,   150,   150,    43,   152,   153,
     154,   154,   154,   154,   154,   154,   154,   154,   155,   155,
      41,    43,    36,   107,   108,   107,   107,    43,   144,    45,
     136,   138,   144,   131,   107,   105,    47,     6,    17,   115,
     121,   122,   123,   124,    19,   126,   120,   107,   107,    43,
      41,    41,    43,    43,    43,    43,    43,    43,    41,   150,
      22,    21,   110,    80,   134,    41,   137,   115,   146,    13,
      79,   116,    42,   127,   128,   158,   159,   116,    79,   132,
     150,   150,   150,    24,    25,   111,   112,   128,   147,   158,
     159,    22,    12,    20,   109,   113,   114,   135,   136,   115,
     115,   125,   117,   118,    41,    43,    43,    43,   158,   158,
     112,   111,    66,    66,   114,   113,    13,   150,   115,    43
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {
      case 62: /* "\"string literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1655 "sparql_parser.c"
	break;
      case 63: /* "\"double literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1660 "sparql_parser.c"
	break;
      case 64: /* "\"double positive literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1665 "sparql_parser.c"
	break;
      case 65: /* "\"double negative literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1670 "sparql_parser.c"
	break;
      case 66: /* "\"integer literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1675 "sparql_parser.c"
	break;
      case 67: /* "\"integer positive literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1680 "sparql_parser.c"
	break;
      case 68: /* "\"integer negative literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1685 "sparql_parser.c"
	break;
      case 69: /* "\"decimal literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1690 "sparql_parser.c"
	break;
      case 70: /* "\"decimal positive literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1695 "sparql_parser.c"
	break;
      case 71: /* "\"decimal negative literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1700 "sparql_parser.c"
	break;
      case 72: /* "\"boolean literal\"" */
#line 233 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1705 "sparql_parser.c"
	break;
      case 73: /* "\"URI literal\"" */
#line 241 "./sparql_parser.y"
	{ if((yyvaluep->uri)) raptor_free_uri((yyvaluep->uri)); };
#line 1710 "sparql_parser.c"
	break;
      case 74: /* "\"URI literal (\"" */
#line 241 "./sparql_parser.y"
	{ if((yyvaluep->uri)) raptor_free_uri((yyvaluep->uri)); };
#line 1715 "sparql_parser.c"
	break;
      case 75: /* "\"QName literal\"" */
#line 244 "./sparql_parser.y"
	{ if((yyvaluep->name)) RASQAL_FREE(cstring, (yyvaluep->name)); };
#line 1720 "sparql_parser.c"
	break;
      case 76: /* "\"QName literal (\"" */
#line 244 "./sparql_parser.y"
	{ if((yyvaluep->name)) RASQAL_FREE(cstring, (yyvaluep->name)); };
#line 1725 "sparql_parser.c"
	break;
      case 77: /* "\"blank node literal\"" */
#line 244 "./sparql_parser.y"
	{ if((yyvaluep->name)) RASQAL_FREE(cstring, (yyvaluep->name)); };
#line 1730 "sparql_parser.c"
	break;
      case 78: /* "\"identifier\"" */
#line 244 "./sparql_parser.y"
	{ if((yyvaluep->name)) RASQAL_FREE(cstring, (yyvaluep->name)); };
#line 1735 "sparql_parser.c"
	break;
      case 89: /* "SelectQuery" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1740 "sparql_parser.c"
	break;
      case 90: /* "SelectExpressionList" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1745 "sparql_parser.c"
	break;
      case 91: /* "SelectExpressionListTail" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1750 "sparql_parser.c"
	break;
      case 92: /* "SelectTerm" */
#line 283 "./sparql_parser.y"
	{ if((yyvaluep->variable)) rasqal_free_variable((yyvaluep->variable)); };
#line 1755 "sparql_parser.c"
	break;
      case 93: /* "SelectExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1760 "sparql_parser.c"
	break;
      case 94: /* "AggregateExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1765 "sparql_parser.c"
	break;
      case 95: /* "CountAggregateExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1770 "sparql_parser.c"
	break;
      case 96: /* "DescribeQuery" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1775 "sparql_parser.c"
	break;
      case 97: /* "VarOrIRIrefList" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1780 "sparql_parser.c"
	break;
      case 98: /* "ConstructQuery" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1785 "sparql_parser.c"
	break;
      case 105: /* "SourceSelector" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1790 "sparql_parser.c"
	break;
      case 111: /* "OrderConditionList" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1795 "sparql_parser.c"
	break;
      case 112: /* "OrderCondition" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1800 "sparql_parser.c"
	break;
      case 115: /* "GroupGraphPattern" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1805 "sparql_parser.c"
	break;
      case 117: /* "GraphPattern" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1810 "sparql_parser.c"
	break;
      case 118: /* "FilteredBasicGraphPattern" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1815 "sparql_parser.c"
	break;
      case 119: /* "TriplesBlockOpt" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1820 "sparql_parser.c"
	break;
      case 120: /* "TriplesSameSubjectDotListOpt" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1825 "sparql_parser.c"
	break;
      case 121: /* "GraphPatternNotTriples" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1830 "sparql_parser.c"
	break;
      case 122: /* "OptionalGraphPattern" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1835 "sparql_parser.c"
	break;
      case 123: /* "GraphGraphPattern" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1840 "sparql_parser.c"
	break;
      case 124: /* "GroupOrUnionGraphPattern" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1845 "sparql_parser.c"
	break;
      case 125: /* "GroupOrUnionGraphPatternList" */
#line 260 "./sparql_parser.y"
	{ if((yyvaluep->graph_pattern)) rasqal_free_graph_pattern((yyvaluep->graph_pattern)); };
#line 1850 "sparql_parser.c"
	break;
      case 126: /* "Filter" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1855 "sparql_parser.c"
	break;
      case 127: /* "Constraint" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1860 "sparql_parser.c"
	break;
      case 128: /* "FunctionCall" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1865 "sparql_parser.c"
	break;
      case 129: /* "ArgList" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1870 "sparql_parser.c"
	break;
      case 130: /* "ConstructTemplate" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1875 "sparql_parser.c"
	break;
      case 131: /* "ConstructTriplesOpt" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1880 "sparql_parser.c"
	break;
      case 132: /* "TriplesSameSubject" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1885 "sparql_parser.c"
	break;
      case 133: /* "PropertyListNotEmpty" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1890 "sparql_parser.c"
	break;
      case 134: /* "PropertyListTailOpt" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1895 "sparql_parser.c"
	break;
      case 135: /* "PropertyList" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1900 "sparql_parser.c"
	break;
      case 136: /* "ObjectList" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1905 "sparql_parser.c"
	break;
      case 137: /* "ObjectTail" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1910 "sparql_parser.c"
	break;
      case 138: /* "Object" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1915 "sparql_parser.c"
	break;
      case 139: /* "Verb" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1920 "sparql_parser.c"
	break;
      case 140: /* "TriplesNode" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1925 "sparql_parser.c"
	break;
      case 141: /* "BlankNodePropertyList" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1930 "sparql_parser.c"
	break;
      case 142: /* "Collection" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1935 "sparql_parser.c"
	break;
      case 143: /* "GraphNodeListNotEmpty" */
#line 247 "./sparql_parser.y"
	{ if((yyvaluep->seq)) raptor_free_sequence((yyvaluep->seq)); };
#line 1940 "sparql_parser.c"
	break;
      case 144: /* "GraphNode" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1945 "sparql_parser.c"
	break;
      case 145: /* "VarOrTerm" */
#line 253 "./sparql_parser.y"
	{ if((yyvaluep->formula)) rasqal_free_formula((yyvaluep->formula)); };
#line 1950 "sparql_parser.c"
	break;
      case 146: /* "VarOrIRIref" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1955 "sparql_parser.c"
	break;
      case 147: /* "Var" */
#line 283 "./sparql_parser.y"
	{ if((yyvaluep->variable)) rasqal_free_variable((yyvaluep->variable)); };
#line 1960 "sparql_parser.c"
	break;
      case 148: /* "VarName" */
#line 283 "./sparql_parser.y"
	{ if((yyvaluep->variable)) rasqal_free_variable((yyvaluep->variable)); };
#line 1965 "sparql_parser.c"
	break;
      case 149: /* "GraphTerm" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 1970 "sparql_parser.c"
	break;
      case 150: /* "Expression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1975 "sparql_parser.c"
	break;
      case 151: /* "ConditionalOrExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1980 "sparql_parser.c"
	break;
      case 152: /* "ConditionalAndExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1985 "sparql_parser.c"
	break;
      case 153: /* "RelationalExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1990 "sparql_parser.c"
	break;
      case 154: /* "AdditiveExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 1995 "sparql_parser.c"
	break;
      case 155: /* "MultiplicativeExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 2000 "sparql_parser.c"
	break;
      case 156: /* "UnaryExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 2005 "sparql_parser.c"
	break;
      case 157: /* "PrimaryExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 2010 "sparql_parser.c"
	break;
      case 158: /* "BrackettedExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 2015 "sparql_parser.c"
	break;
      case 159: /* "BuiltInCall" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 2020 "sparql_parser.c"
	break;
      case 160: /* "RegexExpression" */
#line 267 "./sparql_parser.y"
	{ if((yyvaluep->expr)) rasqal_free_expression((yyvaluep->expr)); };
#line 2025 "sparql_parser.c"
	break;
      case 161: /* "IRIrefBrace" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2030 "sparql_parser.c"
	break;
      case 162: /* "NumericLiteral" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2035 "sparql_parser.c"
	break;
      case 163: /* "NumericLiteralUnsigned" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2040 "sparql_parser.c"
	break;
      case 164: /* "NumericLiteralPositive" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2045 "sparql_parser.c"
	break;
      case 165: /* "NumericLiteralNegative" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2050 "sparql_parser.c"
	break;
      case 166: /* "IRIref" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2055 "sparql_parser.c"
	break;
      case 167: /* "BlankNode" */
#line 276 "./sparql_parser.y"
	{ if((yyvaluep->literal)) rasqal_free_literal((yyvaluep->literal)); };
#line 2060 "sparql_parser.c"
	break;

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */






/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  /* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;

  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 296 "./sparql_parser.y"
    {
}
    break;

  case 3:
#line 302 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  if(sparql->extended)
    ((rasqal_query*)rq)->explain=1;
  else
    sparql_syntax_error((rasqal_query*)rq, "EXPLAIN cannot be used with SPARQL");
}
    break;

  case 4:
#line 311 "./sparql_parser.y"
    {
  /* nothing to do */
}
    break;

  case 5:
#line 319 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->selects=(yyvsp[(1) - (1)].seq);
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_SELECT;
}
    break;

  case 6:
#line 324 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->constructs=(yyvsp[(1) - (1)].seq);
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_CONSTRUCT;
}
    break;

  case 7:
#line 329 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->describes=(yyvsp[(1) - (1)].seq);
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_DESCRIBE;
}
    break;

  case 8:
#line 334 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_ASK;
}
    break;

  case 9:
#line 338 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_DELETE;
}
    break;

  case 10:
#line 342 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_INSERT;
}
    break;

  case 11:
#line 350 "./sparql_parser.y"
    {
  /* nothing to do */
}
    break;

  case 12:
#line 358 "./sparql_parser.y"
    {
  rasqal_query_set_base_uri((rasqal_query*)rq, (yyvsp[(2) - (2)].uri));
}
    break;

  case 13:
#line 362 "./sparql_parser.y"
    {
  /* nothing to do */
}
    break;

  case 14:
#line 370 "./sparql_parser.y"
    {
  raptor_sequence *seq=((rasqal_query*)rq)->prefixes;
  unsigned const char* prefix_string=(yyvsp[(3) - (4)].name);
  size_t l=0;

  if(prefix_string)
    l=strlen((const char*)prefix_string);
  
  if(raptor_namespaces_find_namespace(((rasqal_query*)rq)->namespaces, prefix_string, l)) {
    /* A prefix may be defined only once */
    sparql_syntax_warning(((rasqal_query*)rq), 
                          "PREFIX %s can be defined only once.",
                          prefix_string ? (const char*)prefix_string : ":");
    RASQAL_FREE(cstring, prefix_string);
    raptor_free_uri((yyvsp[(4) - (4)].uri));
  } else {
    rasqal_prefix *p=rasqal_new_prefix(prefix_string, (yyvsp[(4) - (4)].uri));
    if(!p)
      YYERROR_MSG("PrefixDeclOpt: failed to create new prefix");
    if(raptor_sequence_push(seq, p))
      YYERROR_MSG("PrefixDeclOpt: cannot push prefix to seq");
    if(rasqal_query_declare_prefix(((rasqal_query*)rq), p)) {
      YYERROR_MSG("PrefixDeclOpt: cannot declare prefix");
    }
  }
}
    break;

  case 15:
#line 397 "./sparql_parser.y"
    {
  /* nothing to do, rq->prefixes already initialised */
}
    break;

  case 16:
#line 406 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(3) - (6)].seq);
  ((rasqal_query*)rq)->distinct=1;
}
    break;

  case 17:
#line 412 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(3) - (6)].seq);
  ((rasqal_query*)rq)->distinct=2;
}
    break;

  case 18:
#line 418 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (5)].seq);
}
    break;

  case 19:
#line 428 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (1)].seq);
}
    break;

  case 20:
#line 432 "./sparql_parser.y"
    {
  (yyval.seq)=NULL;
  ((rasqal_query*)rq)->wildcard=1;
}
    break;

  case 21:
#line 443 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (2)].seq);
  if(raptor_sequence_push((yyval.seq), (yyvsp[(2) - (2)].variable))) {
    raptor_free_sequence((yyval.seq));
    (yyval.seq)=NULL;
    YYERROR_MSG("SelectExpressionListTail 1: sequence push failed");
  }
}
    break;

  case 22:
#line 452 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (3)].seq);
  if(raptor_sequence_push((yyval.seq), (yyvsp[(3) - (3)].variable))) {
    raptor_free_sequence((yyval.seq));
    (yyval.seq)=NULL;
    YYERROR_MSG("SelectExpressionListTail 2: sequence push failed");
  }
}
    break;

  case 23:
#line 461 "./sparql_parser.y"
    {
  /* The variables are freed from the raptor_query field variables */
  (yyval.seq)=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)rasqal_variable_print);
  if(!(yyval.seq))
    YYERROR_MSG("SelectExpressionListTail 3: failed to create sequence");
  if(raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].variable))) {
    raptor_free_sequence((yyval.seq));
    (yyval.seq)=NULL;
    YYERROR_MSG("SelectExpressionListTail 3: sequence push failed");
  }
}
    break;

  case 24:
#line 479 "./sparql_parser.y"
    {
  (yyval.variable)=(yyvsp[(1) - (1)].variable);
}
    break;

  case 25:
#line 483 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  (yyval.variable)=NULL;
  if(!sparql->extended)
    sparql_syntax_error((rasqal_query*)rq, "SELECT expression AS Variable cannot be used with SPARQL");
  else {
    if(rasqal_expression_mentions_variable((yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].variable))) {
      sparql_query_error_full((rasqal_query*)rq, "SELECT expression contains the AS variable name '%s'", (yyvsp[(3) - (3)].variable)->name);
    } else {
      (yyval.variable)=(yyvsp[(3) - (3)].variable);
      (yyvsp[(3) - (3)].variable)->expression=(yyvsp[(1) - (3)].expr);
    }

  }
}
    break;

  case 26:
#line 504 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 27:
#line 508 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (3)].expr);
}
    break;

  case 28:
#line 512 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (3)].expr);
}
    break;

  case 29:
#line 519 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 30:
#line 526 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  if(!sparql->extended) {
    sparql_syntax_error((rasqal_query*)rq, "COUNT cannot be used with SPARQL");
    (yyval.expr)=NULL;
  } else {
    (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_COUNT, (yyvsp[(3) - (4)].expr));
    if(!(yyval.expr))
      YYERROR_MSG("CountAggregateExpression 1: cannot create expr");
  }
}
    break;

  case 31:
#line 539 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  if(!sparql->extended) {
    sparql_syntax_error((rasqal_query*)rq, "COUNT cannot be used with SPARQL");
    (yyval.expr)=NULL;
  } else {
    rasqal_expression* vs=rasqal_new_0op_expression(RASQAL_EXPR_VARSTAR);
    if(!vs)
      YYERROR_MSG("CountAggregateExpression 2: cannot create varstar expr");
    (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_COUNT, vs);
    if(!(yyval.expr))
      YYERROR_MSG("CountAggregateExpression 2: cannot create expr");
  }
}
    break;

  case 32:
#line 560 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (5)].seq);
}
    break;

  case 33:
#line 565 "./sparql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 34:
#line 573 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (2)].seq);
  if(raptor_sequence_push((yyval.seq), (yyvsp[(2) - (2)].literal))) {
    raptor_free_sequence((yyval.seq));
    (yyval.seq)=NULL;
    YYERROR_MSG("VarOrIRIrefList 1: sequence push failed");
  }
}
    break;

  case 35:
#line 582 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (3)].seq);
  if(raptor_sequence_push((yyval.seq), (yyvsp[(3) - (3)].literal))) {
    raptor_free_sequence((yyval.seq));
    (yyval.seq)=NULL;
    YYERROR_MSG("VarOrIRIrefList 2: sequence push failed");
  }
}
    break;

  case 36:
#line 591 "./sparql_parser.y"
    {
  (yyval.seq)=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_literal, (raptor_sequence_print_handler*)rasqal_literal_print);
  if(!(yyval.seq))
    YYERROR_MSG("VarOrIRIrefList 3: cannot create seq");
  if(raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].literal))) {
    raptor_free_sequence((yyval.seq));
    (yyval.seq)=NULL;
    YYERROR_MSG("VarOrIRIrefList 3: sequence push failed");
  }
}
    break;

  case 37:
#line 607 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (5)].seq);
}
    break;

  case 38:
#line 616 "./sparql_parser.y"
    {
  /* nothing to do */
}
    break;

  case 39:
#line 625 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  if(!sparql->extended)
    sparql_syntax_error((rasqal_query*)rq, "DELETE cannot be used with SPARQL");
}
    break;

  case 40:
#line 637 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  if(!sparql->extended)
    sparql_syntax_error((rasqal_query*)rq, "INSERT cannot be used with SPARQL");
}
    break;

  case 44:
#line 655 "./sparql_parser.y"
    {
  if((yyvsp[(1) - (1)].literal)) {
    raptor_uri* uri=rasqal_literal_as_uri((yyvsp[(1) - (1)].literal));
    if(rasqal_query_add_data_graph((rasqal_query*)rq, uri, NULL,
                                   RASQAL_DATA_GRAPH_BACKGROUND)) {
      rasqal_free_literal((yyvsp[(1) - (1)].literal));
      YYERROR_MSG("DefaultGraphClause: rasqal_query_add_data_graph failed");
    }
    rasqal_free_literal((yyvsp[(1) - (1)].literal));
  }
}
    break;

  case 45:
#line 671 "./sparql_parser.y"
    {
  if((yyvsp[(2) - (2)].literal)) {
    raptor_uri* uri=rasqal_literal_as_uri((yyvsp[(2) - (2)].literal));
    if(rasqal_query_add_data_graph((rasqal_query*)rq, uri, uri,
                                   RASQAL_DATA_GRAPH_NAMED)) {
      rasqal_free_literal((yyvsp[(2) - (2)].literal));
      YYERROR_MSG("NamedGraphClause: rasqal_query_add_data_graph failed");
    }
    rasqal_free_literal((yyvsp[(2) - (2)].literal));
  }
}
    break;

  case 46:
#line 687 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 47:
#line 695 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->query_graph_pattern=(yyvsp[(2) - (2)].graph_pattern);
}
    break;

  case 48:
#line 699 "./sparql_parser.y"
    {
  ((rasqal_query*)rq)->query_graph_pattern=(yyvsp[(1) - (1)].graph_pattern);
}
    break;

  case 51:
#line 713 "./sparql_parser.y"
    {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)(((rasqal_query*)rq)->context);

  if(!sparql->extended)
    sparql_syntax_error((rasqal_query*)rq, "GROUP BY cannot be used with SPARQL");
  else if(((rasqal_query*)rq)->verb == RASQAL_QUERY_VERB_ASK) {
    sparql_query_error((rasqal_query*)rq, "GROUP BY cannot be used with ASK");
  } else {
    raptor_sequence *seq=(yyvsp[(3) - (3)].seq);
    ((rasqal_query*)rq)->group_conditions_sequence=seq;
    if(seq) {
      int i;
      
      for(i=0; i < raptor_sequence_size(seq); i++) {
        rasqal_expression* e=(rasqal_expression*)raptor_sequence_get_at(seq, i);
        if(e->op == RASQAL_EXPR_ORDER_COND_ASC)
          e->op = RASQAL_EXPR_GROUP_COND_ASC;
        else
          e->op = RASQAL_EXPR_GROUP_COND_DESC;
      }
    }
  }
}
    break;

  case 57:
#line 746 "./sparql_parser.y"
    { 
}
    break;

  case 58:
#line 753 "./sparql_parser.y"
    {
  if(((rasqal_query*)rq)->verb == RASQAL_QUERY_VERB_ASK) {
    sparql_query_error((rasqal_query*)rq, "ORDER BY cannot be used with ASK");
  } else {
    ((rasqal_query*)rq)->order_conditions_sequence=(yyvsp[(3) - (3)].seq);
  }
}
    break;

  case 60:
#line 766 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (2)].seq);
  if((yyvsp[(2) - (2)].expr))
    if(raptor_sequence_push((yyval.seq), (yyvsp[(2) - (2)].expr))) {
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("OrderConditionList 1: sequence push failed");
    }
}
    break;

  case 61:
#line 776 "./sparql_parser.y"
    {
  (yyval.seq)=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_expression, (raptor_sequence_print_handler*)rasqal_expression_print);
  if(!(yyval.seq)) {
    if((yyvsp[(1) - (1)].expr))
      rasqal_free_expression((yyvsp[(1) - (1)].expr));
    YYERROR_MSG("OrderConditionList 2: cannot create sequence");
  }
  if((yyvsp[(1) - (1)].expr))
    if(raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].expr))) {
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("OrderConditionList 2: sequence push failed");
    }
}
    break;

  case 62:
#line 795 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ORDER_COND_ASC, (yyvsp[(2) - (2)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("OrderCondition 1: cannot create expr");
}
    break;

  case 63:
#line 801 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ORDER_COND_DESC, (yyvsp[(2) - (2)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("OrderCondition 2: cannot create expr");
}
    break;

  case 64:
#line 807 "./sparql_parser.y"
    {
  /* The direction of ordering is ascending by default */
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ORDER_COND_ASC, (yyvsp[(1) - (1)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("OrderCondition 3: cannot create expr");
}
    break;

  case 65:
#line 814 "./sparql_parser.y"
    {
  rasqal_literal* l;
  rasqal_expression *e;
  l=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
  if(!l)
    YYERROR_MSG("OrderCondition 4: cannot create lit");
  e=rasqal_new_literal_expression(l);
  if(!e)
    YYERROR_MSG("OrderCondition 4: cannot create lit expr");

  /* The direction of ordering is ascending by default */
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ORDER_COND_ASC, e);
  if(!(yyval.expr))
    YYERROR_MSG("OrderCondition 1: cannot create expr");
}
    break;

  case 66:
#line 830 "./sparql_parser.y"
    {
  /* The direction of ordering is ascending by default */
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ORDER_COND_ASC, (yyvsp[(1) - (1)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("OrderCondition 5: cannot create expr");
}
    break;

  case 67:
#line 837 "./sparql_parser.y"
    {
  /* The direction of ordering is ascending by default */
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ORDER_COND_ASC, (yyvsp[(1) - (1)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("OrderCondition 6: cannot create expr");
}
    break;

  case 68:
#line 848 "./sparql_parser.y"
    {
  if(((rasqal_query*)rq)->verb == RASQAL_QUERY_VERB_ASK) {
    sparql_query_error((rasqal_query*)rq, "LIMIT cannot be used with ASK");
  } else {
    if((yyvsp[(2) - (2)].literal) != NULL) {
      ((rasqal_query*)rq)->limit=(yyvsp[(2) - (2)].literal)->value.integer;
      rasqal_free_literal((yyvsp[(2) - (2)].literal));
    }
  }
  
}
    break;

  case 69:
#line 864 "./sparql_parser.y"
    {
  if(((rasqal_query*)rq)->verb == RASQAL_QUERY_VERB_ASK) {
    sparql_query_error((rasqal_query*)rq, "LIMIT cannot be used with ASK");
  } else {
    if((yyvsp[(2) - (2)].literal) != NULL) {
      ((rasqal_query*)rq)->offset=(yyvsp[(2) - (2)].literal)->value.integer;
      rasqal_free_literal((yyvsp[(2) - (2)].literal));
    }
  }
}
    break;

  case 70:
#line 879 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(2) - (3)].graph_pattern);
}
    break;

  case 73:
#line 893 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "GraphPattern 1\n  FilteredBasicGraphPattern=");
  if((yyvsp[(1) - (4)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(1) - (4)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, ", GraphPatternNotTriples=");
  if((yyvsp[(2) - (4)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(2) - (4)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, ", GraphPattern=");
  if((yyvsp[(4) - (4)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(4) - (4)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n", DEBUG_FH);
#endif

  (yyval.graph_pattern)=(yyvsp[(4) - (4)].graph_pattern);
  /* push ($1,$2) to start of $4 graph sequence */
  if((yyvsp[(2) - (4)].graph_pattern))
    if(raptor_sequence_shift((yyval.graph_pattern)->graph_patterns, (yyvsp[(2) - (4)].graph_pattern))) {
      if((yyvsp[(1) - (4)].graph_pattern))
        rasqal_free_graph_pattern((yyvsp[(1) - (4)].graph_pattern));
      rasqal_free_graph_pattern((yyval.graph_pattern));
      (yyval.graph_pattern)=NULL;
      YYERROR_MSG("GraphPattern 1: sequence shift $2 failed");
    }
  if((yyvsp[(1) - (4)].graph_pattern))
    if(raptor_sequence_shift((yyval.graph_pattern)->graph_patterns, (yyvsp[(1) - (4)].graph_pattern))) {
      rasqal_free_graph_pattern((yyval.graph_pattern));
      (yyval.graph_pattern)=NULL;
      YYERROR_MSG("GraphPattern 1: sequence shift $1 failed");
    }

#if RASQAL_DEBUG > 1
  fprintf(DEBUG_FH, "  after grouping graph pattern=");
  if((yyval.graph_pattern))
    rasqal_graph_pattern_print((yyval.graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 74:
#line 940 "./sparql_parser.y"
    {
  raptor_sequence *seq;

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "GraphPattern 2\n  FilteredBasicGraphPattern=");
  if((yyvsp[(1) - (1)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(1) - (1)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n", DEBUG_FH);
#endif

  seq=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_graph_pattern, (raptor_sequence_print_handler*)rasqal_graph_pattern_print);
  if(!seq) {
    if((yyvsp[(1) - (1)].graph_pattern))
      rasqal_free_graph_pattern((yyvsp[(1) - (1)].graph_pattern));
    YYERROR_MSG("GraphPattern 2: cannot create sequence");
  }  
  if((yyvsp[(1) - (1)].graph_pattern))
    if(raptor_sequence_push(seq, (yyvsp[(1) - (1)].graph_pattern))) {
      raptor_free_sequence(seq);
      YYERROR_MSG("GraphPattern 2: sequence push failed");
    }

  (yyval.graph_pattern)=rasqal_new_graph_pattern_from_sequence((rasqal_query*)rq, seq,
                                            RASQAL_GRAPH_PATTERN_OPERATOR_GROUP);

  if(!(yyval.graph_pattern))
    YYERROR_MSG("GraphPattern 2: cannot create gp");

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after grouping graph pattern=");
  rasqal_graph_pattern_print((yyval.graph_pattern), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 75:
#line 981 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "FilteredBasicGraphPattern 1\n  TriplesBlockOpt=");
  if((yyvsp[(1) - (4)].formula))
    rasqal_formula_print((yyvsp[(1) - (4)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, ", Constraint=");
  if((yyvsp[(2) - (4)].expr))
    rasqal_expression_print((yyvsp[(2) - (4)].expr), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, ", FilteredBasicGraphPattern=");
  if((yyvsp[(4) - (4)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(4) - (4)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n", DEBUG_FH);
#endif

  (yyval.graph_pattern)=(yyvsp[(4) - (4)].graph_pattern);

  if((yyvsp[(2) - (4)].expr)) {
    if(rasqal_graph_pattern_add_constraint((yyval.graph_pattern), (yyvsp[(2) - (4)].expr))) {
      if((yyvsp[(1) - (4)].formula))
        rasqal_free_formula((yyvsp[(1) - (4)].formula));
      rasqal_free_graph_pattern((yyval.graph_pattern));
      (yyval.graph_pattern)=NULL;
      YYERROR_MSG("FilteredBasicGraphPattern 1: cannot add constraint");
    }
  }

  /* push $1 to end of $4 graph sequence */
  if((yyvsp[(1) - (4)].formula)) {
    rasqal_graph_pattern *gp;
    gp=rasqal_engine_new_basic_graph_pattern_from_formula((rasqal_query*)rq, (yyvsp[(1) - (4)].formula));
    if(!gp) {
      rasqal_free_graph_pattern((yyval.graph_pattern));
      (yyval.graph_pattern)=NULL;
      YYERROR_MSG("FilteredBasicGraphPattern 1: cannot create gp");
    }
    if(raptor_sequence_push((yyval.graph_pattern)->graph_patterns, gp)) {
      rasqal_free_graph_pattern((yyval.graph_pattern));
      (yyval.graph_pattern)=NULL;
      YYERROR_MSG("FilteredBasicGraphPattern 1: sequence push failed");
    }
  }
 
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after grouping graph pattern=");
  if((yyval.graph_pattern))
    rasqal_graph_pattern_print((yyval.graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 76:
#line 1039 "./sparql_parser.y"
    {
  rasqal_graph_pattern *formula_gp=NULL;
  raptor_sequence *seq;

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "FilteredBasicGraphPattern 2\n  TriplesBlockOpt=");
  if((yyvsp[(1) - (1)].formula))
    rasqal_formula_print((yyvsp[(1) - (1)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n", DEBUG_FH);
#endif

  if((yyvsp[(1) - (1)].formula)) {
    formula_gp=rasqal_engine_new_basic_graph_pattern_from_formula((rasqal_query*)rq, (yyvsp[(1) - (1)].formula));
    if(!formula_gp)
      YYERROR_MSG("FilteredBasicGraphPattern 2: cannot create formula_gp");
  }
  
  seq=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_graph_pattern, (raptor_sequence_print_handler*)rasqal_graph_pattern_print);
  if(!seq) {
    if(formula_gp)
      rasqal_free_graph_pattern(formula_gp);
    YYERROR_MSG("FilteredBasicGraphPattern 2: cannot create sequence");
  }
  if(formula_gp)
    if(raptor_sequence_push(seq, formula_gp)) {
      raptor_free_sequence(seq);
      YYERROR_MSG("FilteredBasicGraphPattern 2: sequence push failed");
    }

  (yyval.graph_pattern)=rasqal_new_graph_pattern_from_sequence((rasqal_query*)rq,
                                            seq,
                                            RASQAL_GRAPH_PATTERN_OPERATOR_GROUP);
  if(!(yyval.graph_pattern))
    YYERROR_MSG("FilteredBasicGraphPattern 2: cannot create gp");


#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after, group graph pattern=");
  rasqal_graph_pattern_print((yyval.graph_pattern), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 77:
#line 1088 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "TriplesBlockOpt\n  TriplesSameSubject=");
  if((yyvsp[(1) - (2)].formula))
    rasqal_formula_print((yyvsp[(1) - (2)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("  TriplesSameSubjectDotListOpt=", DEBUG_FH);
  if((yyvsp[(2) - (2)].formula))
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n", DEBUG_FH);
#endif


  /* $1 and $2 are freed as necessary */
  (yyval.formula)=rasqal_formula_join((yyvsp[(1) - (2)].formula), (yyvsp[(2) - (2)].formula));
  if(!(yyval.formula))
    YYERROR_MSG("TriplesBlockOpt: formula join failed");

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after joining formula=");
  rasqal_formula_print((yyval.formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 78:
#line 1116 "./sparql_parser.y"
    {
  (yyval.formula)=NULL;
}
    break;

  case 79:
#line 1127 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "TriplesSameSubjectDotTriplesOpt\n  TriplesSameSubjectDotListOpt=");
  if((yyvsp[(1) - (2)].formula))
    rasqal_formula_print((yyvsp[(1) - (2)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("  TriplesSameSubject=", DEBUG_FH);
  if((yyvsp[(2) - (2)].formula))
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n", DEBUG_FH);
#endif

  /* $1 and $2 are freed as necessary */
  (yyval.formula)=rasqal_formula_join((yyvsp[(1) - (2)].formula), (yyvsp[(2) - (2)].formula));
  if(!(yyval.formula))
    YYERROR_MSG("TriplesSameSubjectDotTriplesOpt: formula join failed");

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after joining formula=");
  rasqal_formula_print((yyval.formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 80:
#line 1154 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (2)].formula);
}
    break;

  case 81:
#line 1158 "./sparql_parser.y"
    {
  (yyval.formula)=NULL;
}
    break;

  case 82:
#line 1166 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(1) - (1)].graph_pattern);
}
    break;

  case 83:
#line 1170 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(1) - (1)].graph_pattern);
}
    break;

  case 84:
#line 1174 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(1) - (1)].graph_pattern);
}
    break;

  case 85:
#line 1182 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "PatternElementForms 4\n  graphpattern=");
  if((yyvsp[(2) - (2)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(2) - (2)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n\n", DEBUG_FH);
#endif

  if((yyvsp[(2) - (2)].graph_pattern))
    (yyvsp[(2) - (2)].graph_pattern)->op = RASQAL_GRAPH_PATTERN_OPERATOR_OPTIONAL;

  (yyval.graph_pattern)=(yyvsp[(2) - (2)].graph_pattern);
}
    break;

  case 86:
#line 1202 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "GraphGraphPattern 2\n  varoruri=");
  rasqal_literal_print((yyvsp[(2) - (3)].literal), DEBUG_FH);
  fprintf(DEBUG_FH, ", graphpattern=");
  if((yyvsp[(3) - (3)].graph_pattern))
    rasqal_graph_pattern_print((yyvsp[(3) - (3)].graph_pattern), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fputs("\n\n", DEBUG_FH);
#endif

  if((yyvsp[(3) - (3)].graph_pattern)) {
    rasqal_graph_pattern_set_origin((yyvsp[(3) - (3)].graph_pattern), (yyvsp[(2) - (3)].literal));
    (yyvsp[(3) - (3)].graph_pattern)->op = RASQAL_GRAPH_PATTERN_OPERATOR_GRAPH;
  }

  rasqal_free_literal((yyvsp[(2) - (3)].literal));
  (yyval.graph_pattern)=(yyvsp[(3) - (3)].graph_pattern);
}
    break;

  case 87:
#line 1227 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(3) - (3)].graph_pattern);
  if(raptor_sequence_push((yyval.graph_pattern)->graph_patterns, (yyvsp[(1) - (3)].graph_pattern))) {
    rasqal_free_graph_pattern((yyval.graph_pattern));
    (yyval.graph_pattern)=NULL;
    YYERROR_MSG("GroupOrUnionGraphPattern: sequence push failed");
  }

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "UnionGraphPattern\n  graphpattern=");
  rasqal_graph_pattern_print((yyval.graph_pattern), DEBUG_FH);
  fputs("\n\n", DEBUG_FH);
#endif
}
    break;

  case 88:
#line 1242 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(1) - (1)].graph_pattern);
}
    break;

  case 89:
#line 1249 "./sparql_parser.y"
    {
  (yyval.graph_pattern)=(yyvsp[(1) - (3)].graph_pattern);
  if((yyvsp[(3) - (3)].graph_pattern))
    if(raptor_sequence_push((yyval.graph_pattern)->graph_patterns, (yyvsp[(3) - (3)].graph_pattern))) {
      rasqal_free_graph_pattern((yyval.graph_pattern));
      (yyval.graph_pattern)=NULL;
      YYERROR_MSG("GroupOrUnionGraphPatternList 1: sequence push failed");
    }
}
    break;

  case 90:
#line 1259 "./sparql_parser.y"
    {
  raptor_sequence *seq;
  seq=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_graph_pattern, (raptor_sequence_print_handler*)rasqal_graph_pattern_print);
  if(!seq) {
    if((yyvsp[(1) - (1)].graph_pattern))
      rasqal_free_graph_pattern((yyvsp[(1) - (1)].graph_pattern));
    YYERROR_MSG("GroupOrUnionGraphPatternList 2: cannot create sequence");
  }
  if((yyvsp[(1) - (1)].graph_pattern))
    if(raptor_sequence_push(seq, (yyvsp[(1) - (1)].graph_pattern))) {
      raptor_free_sequence(seq);
      YYERROR_MSG("GroupOrUnionGraphPatternList 2: sequence push failed");
    }
  (yyval.graph_pattern)=rasqal_new_graph_pattern_from_sequence((rasqal_query*)rq,
                                            seq,
                                            RASQAL_GRAPH_PATTERN_OPERATOR_UNION);
  if(!(yyval.graph_pattern))
    YYERROR_MSG("GroupOrUnionGraphPatternList 1: cannot create gp");
}
    break;

  case 91:
#line 1283 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (2)].expr);
}
    break;

  case 92:
#line 1291 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 93:
#line 1295 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 94:
#line 1299 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 95:
#line 1307 "./sparql_parser.y"
    {
  raptor_uri* uri=rasqal_literal_as_uri((yyvsp[(1) - (3)].literal));
  
  if(!(yyvsp[(2) - (3)].seq)) {
    (yyvsp[(2) - (3)].seq)=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_expression, (raptor_sequence_print_handler*)rasqal_expression_print);
    if(!(yyvsp[(2) - (3)].seq)) {
      rasqal_free_literal((yyvsp[(1) - (3)].literal));
      YYERROR_MSG("FunctionCall: cannot create sequence");
    }
  }

  uri=raptor_uri_copy(uri);

  if(raptor_sequence_size((yyvsp[(2) - (3)].seq)) == 1 &&
     rasqal_xsd_is_datatype_uri(((rasqal_query*)rq)->world, uri)) {
    rasqal_expression* e=(rasqal_expression*)raptor_sequence_pop((yyvsp[(2) - (3)].seq));
    (yyval.expr)=rasqal_new_cast_expression(uri, e);
    raptor_free_sequence((yyvsp[(2) - (3)].seq));
  } else {
    (yyval.expr)=rasqal_new_function_expression(uri, (yyvsp[(2) - (3)].seq));
  }
  rasqal_free_literal((yyvsp[(1) - (3)].literal));

  if(!(yyval.expr))
    YYERROR_MSG("FunctionCall: cannot create expr");
}
    break;

  case 96:
#line 1338 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (3)].seq);
  if((yyvsp[(3) - (3)].expr))
    if(raptor_sequence_push((yyval.seq), (yyvsp[(3) - (3)].expr))) {
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("ArgList 1: sequence push failed");
    }
}
    break;

  case 97:
#line 1348 "./sparql_parser.y"
    {
  (yyval.seq)=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_expression, (raptor_sequence_print_handler*)rasqal_expression_print);
  if(!(yyval.seq)) {
    if((yyvsp[(1) - (1)].expr))
      rasqal_free_expression((yyvsp[(1) - (1)].expr));
    YYERROR_MSG("ArgList 2: cannot create sequence");
  }
  if((yyvsp[(1) - (1)].expr))
    if(raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].expr))) {
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("ArgList 2: sequence push failed");
    }
}
    break;

  case 98:
#line 1363 "./sparql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 99:
#line 1371 "./sparql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (3)].seq);
}
    break;

  case 100:
#line 1379 "./sparql_parser.y"
    {
  (yyval.seq)=NULL;
 
  if((yyvsp[(1) - (3)].formula)) {
    (yyval.seq)=(yyvsp[(1) - (3)].formula)->triples;
    (yyvsp[(1) - (3)].formula)->triples=NULL;
    rasqal_free_formula((yyvsp[(1) - (3)].formula));
  }
  
  if((yyvsp[(3) - (3)].seq)) {
    if(!(yyval.seq)) {
      (yyval.seq)=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_triple, (raptor_sequence_print_handler*)rasqal_triple_print);
      if(!(yyval.seq)) {
        raptor_free_sequence((yyvsp[(3) - (3)].seq));
        YYERROR_MSG("ConstructTriplesOpt: cannot create sequence");
      }
    }

    if(raptor_sequence_join((yyval.seq), (yyvsp[(3) - (3)].seq))) {
      raptor_free_sequence((yyvsp[(3) - (3)].seq));
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("ConstructTriplesOpt: sequence join failed");
    }
    raptor_free_sequence((yyvsp[(3) - (3)].seq));
  }

 }
    break;

  case 101:
#line 1408 "./sparql_parser.y"
    {
  (yyval.seq)=NULL;
  
  if((yyvsp[(1) - (1)].formula)) {
    (yyval.seq)=(yyvsp[(1) - (1)].formula)->triples;
    (yyvsp[(1) - (1)].formula)->triples=NULL;
    rasqal_free_formula((yyvsp[(1) - (1)].formula));
  }
  
}
    break;

  case 102:
#line 1419 "./sparql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 103:
#line 1427 "./sparql_parser.y"
    {
  int i;

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "TriplesSameSubject 1\n subject=");
  rasqal_formula_print((yyvsp[(1) - (2)].formula), DEBUG_FH);
  if((yyvsp[(2) - (2)].formula)) {
    fprintf(DEBUG_FH, "\n propertyList=");
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
  } else     
    fprintf(DEBUG_FH, "\n and empty propertyList\n");
#endif

  if((yyvsp[(2) - (2)].formula)) {
    raptor_sequence *seq=(yyvsp[(2) - (2)].formula)->triples;
    rasqal_literal *subject=(yyvsp[(1) - (2)].formula)->value;
    
    /* non-empty property list, handle it  */
    for(i=0; i < raptor_sequence_size(seq); i++) {
      rasqal_triple* t2=(rasqal_triple*)raptor_sequence_get_at(seq, i);
      if(t2->subject)
        continue;
      t2->subject=rasqal_new_literal_from_literal(subject);
    }
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, "  after substitution propertyList=");
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
#endif
  }

  (yyval.formula)=rasqal_formula_join((yyvsp[(1) - (2)].formula), (yyvsp[(2) - (2)].formula));
  if(!(yyval.formula))
    YYERROR_MSG("TriplesSameSubject 1: formula join failed");

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after joining formula=");
  rasqal_formula_print((yyval.formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 104:
#line 1470 "./sparql_parser.y"
    {
  int i;

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "TriplesSameSubject 2\n subject=");
  rasqal_formula_print((yyvsp[(1) - (2)].formula), DEBUG_FH);
  if((yyvsp[(2) - (2)].formula)) {
    fprintf(DEBUG_FH, "\n propertyList=");
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
  } else     
    fprintf(DEBUG_FH, "\n and empty propertyList\n");
#endif

  if((yyvsp[(2) - (2)].formula)) {
    raptor_sequence *seq=(yyvsp[(2) - (2)].formula)->triples;
    rasqal_literal *subject=(yyvsp[(1) - (2)].formula)->value;
    
    /* non-empty property list, handle it  */
    for(i=0; i < raptor_sequence_size(seq); i++) {
      rasqal_triple* t2=(rasqal_triple*)raptor_sequence_get_at(seq, i);
      if(t2->subject)
        continue;
      t2->subject=rasqal_new_literal_from_literal(subject);
    }
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, "  after substitution propertyList=");
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
#endif
  }

  (yyval.formula)=rasqal_formula_join((yyvsp[(1) - (2)].formula), (yyvsp[(2) - (2)].formula));
  if(!(yyval.formula))
    YYERROR_MSG("TriplesSameSubject 2: formula join failed");

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "  after joining formula=");
  rasqal_formula_print((yyval.formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 105:
#line 1517 "./sparql_parser.y"
    {
  int i;
  
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "PropertyList 1\n Verb=");
  rasqal_formula_print((yyvsp[(1) - (3)].formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n ObjectList=");
  rasqal_formula_print((yyvsp[(2) - (3)].formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n PropertyListTail=");
  if((yyvsp[(3) - (3)].formula) != NULL)
    rasqal_formula_print((yyvsp[(3) - (3)].formula), DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, "\n");
#endif
  
  if((yyvsp[(2) - (3)].formula) == NULL) {
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, " empty ObjectList not processed\n");
#endif
  } else if((yyvsp[(1) - (3)].formula) && (yyvsp[(2) - (3)].formula)) {
    raptor_sequence *seq=(yyvsp[(2) - (3)].formula)->triples;
    rasqal_literal *predicate=(yyvsp[(1) - (3)].formula)->value;
    rasqal_formula *formula;
    rasqal_triple *t2;

    formula=rasqal_new_formula();
    if(!formula) {
      rasqal_free_formula((yyvsp[(1) - (3)].formula));
      rasqal_free_formula((yyvsp[(2) - (3)].formula));
      if((yyvsp[(3) - (3)].formula))
        rasqal_free_formula((yyvsp[(3) - (3)].formula));
      YYERROR_MSG("PropertyList 1: cannot create formula");
    }
    formula->triples=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_triple, (raptor_sequence_print_handler*)rasqal_triple_print);
    if(!formula->triples) {
      rasqal_free_formula(formula);
      rasqal_free_formula((yyvsp[(1) - (3)].formula));
      rasqal_free_formula((yyvsp[(2) - (3)].formula));
      if((yyvsp[(3) - (3)].formula))
        rasqal_free_formula((yyvsp[(3) - (3)].formula));
      YYERROR_MSG("PropertyList 1: cannot create sequence");
    }

    /* non-empty property list, handle it  */
    for(i=0; i<raptor_sequence_size(seq); i++) {
      t2=(rasqal_triple*)raptor_sequence_get_at(seq, i);
      if(!t2->predicate)
        t2->predicate=(rasqal_literal*)rasqal_new_literal_from_literal(predicate);
    }
  
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, "  after substitution ObjectList=");
    raptor_sequence_print(seq, DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
#endif

    while(raptor_sequence_size(seq)) {
      t2=(rasqal_triple*)raptor_sequence_unshift(seq);
      if(raptor_sequence_push(formula->triples, t2)) {
        rasqal_free_formula(formula);
        rasqal_free_formula((yyvsp[(1) - (3)].formula));
        rasqal_free_formula((yyvsp[(2) - (3)].formula));
        if((yyvsp[(3) - (3)].formula))
          rasqal_free_formula((yyvsp[(3) - (3)].formula));
        YYERROR_MSG("PropertyList 1: sequence push failed");
      }
    }

    (yyvsp[(3) - (3)].formula)=rasqal_formula_join(formula, (yyvsp[(3) - (3)].formula));
    if(!(yyvsp[(3) - (3)].formula)) {
      rasqal_free_formula((yyvsp[(1) - (3)].formula));
      rasqal_free_formula((yyvsp[(2) - (3)].formula));
      YYERROR_MSG("PropertyList 1: formula join failed");
    }

#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, "  after appending ObjectList=");
    rasqal_formula_print((yyvsp[(3) - (3)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n\n");
#endif

    rasqal_free_formula((yyvsp[(2) - (3)].formula));
  }

  if((yyvsp[(1) - (3)].formula))
    rasqal_free_formula((yyvsp[(1) - (3)].formula));

  (yyval.formula)=(yyvsp[(3) - (3)].formula);
}
    break;

  case 106:
#line 1612 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(2) - (2)].formula);
}
    break;

  case 107:
#line 1616 "./sparql_parser.y"
    {
  (yyval.formula)=NULL;
}
    break;

  case 108:
#line 1624 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (1)].formula);
}
    break;

  case 109:
#line 1628 "./sparql_parser.y"
    {
  (yyval.formula)=NULL;
}
    break;

  case 110:
#line 1636 "./sparql_parser.y"
    {
  rasqal_formula *formula;
  rasqal_triple *triple;

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "ObjectList 1\n");
  fprintf(DEBUG_FH, " Object=\n");
  rasqal_formula_print((yyvsp[(1) - (2)].formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n");
  if((yyvsp[(2) - (2)].formula)) {
    fprintf(DEBUG_FH, " ObjectTail=");
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
  } else
    fprintf(DEBUG_FH, " and empty ObjectTail\n");
#endif

  formula=rasqal_new_formula();
  if(!formula) {
    rasqal_free_formula((yyvsp[(1) - (2)].formula));
    if((yyvsp[(2) - (2)].formula))
      rasqal_free_formula((yyvsp[(2) - (2)].formula));
    YYERROR_MSG("ObjectList: cannot create formula");
  }
  
  formula->triples=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_triple, (raptor_sequence_print_handler*)rasqal_triple_print);
  if(!formula->triples) {
    rasqal_free_formula(formula);
    rasqal_free_formula((yyvsp[(1) - (2)].formula));
    if((yyvsp[(2) - (2)].formula))
      rasqal_free_formula((yyvsp[(2) - (2)].formula));
    YYERROR_MSG("ObjectList: cannot create sequence");
  }

  triple=rasqal_new_triple(NULL, NULL, (yyvsp[(1) - (2)].formula)->value);
  (yyvsp[(1) - (2)].formula)->value=NULL; /* value now owned by triple */
  if(!triple) {
    rasqal_free_formula(formula);
    rasqal_free_formula((yyvsp[(1) - (2)].formula));
    if((yyvsp[(2) - (2)].formula))
      rasqal_free_formula((yyvsp[(2) - (2)].formula));
    YYERROR_MSG("ObjectList: cannot create triple");
  }

  if(raptor_sequence_push(formula->triples, triple)) {
    rasqal_free_formula(formula);
    rasqal_free_formula((yyvsp[(1) - (2)].formula));
    if((yyvsp[(2) - (2)].formula))
      rasqal_free_formula((yyvsp[(2) - (2)].formula));
    YYERROR_MSG("ObjectList: sequence push failed");
  }

  (yyval.formula)=rasqal_formula_join(formula, (yyvsp[(1) - (2)].formula));
  if(!(yyval.formula)) {
    if((yyvsp[(2) - (2)].formula))
      rasqal_free_formula((yyvsp[(2) - (2)].formula));
    YYERROR_MSG("ObjectList: formula join $1 failed");
  }

  (yyval.formula)=rasqal_formula_join((yyval.formula), (yyvsp[(2) - (2)].formula));
  if(!(yyval.formula))
    YYERROR_MSG("ObjectList: formula join $2 failed");

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, " objectList is now ");
  if((yyval.formula))
    raptor_sequence_print((yyval.formula)->triples, DEBUG_FH);
  else
    fputs("NULL", DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 111:
#line 1713 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(2) - (2)].formula);
}
    break;

  case 112:
#line 1717 "./sparql_parser.y"
    {
  (yyval.formula)=NULL;
}
    break;

  case 113:
#line 1725 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (1)].formula);
}
    break;

  case 114:
#line 1733 "./sparql_parser.y"
    {
  (yyval.formula)=rasqal_new_formula();
  if(!(yyval.formula)) {
    if((yyvsp[(1) - (1)].literal))
      rasqal_free_literal((yyvsp[(1) - (1)].literal));
    YYERROR_MSG("Verb 1: cannot create formula");
  }
  (yyval.formula)->value=(yyvsp[(1) - (1)].literal);
}
    break;

  case 115:
#line 1743 "./sparql_parser.y"
    {
  raptor_uri *uri;

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "verb Verb=rdf:type (a)\n");
#endif

  uri=raptor_new_uri_for_rdf_concept("type");
  if(!uri)
    YYERROR_MSG("Verb 2: uri for rdf concept type failed");
  (yyval.formula)=rasqal_new_formula();
  if(!(yyval.formula)) {
    raptor_free_uri(uri);
    YYERROR_MSG("Verb 2: cannot create formula");
  }
  (yyval.formula)->value=rasqal_new_uri_literal(((rasqal_query*)rq)->world, uri);
  if(!(yyval.formula)->value) {
    rasqal_free_formula((yyval.formula));
    (yyval.formula)=NULL;
    YYERROR_MSG("Verb 2: cannot create uri literal");
  }
}
    break;

  case 116:
#line 1770 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (1)].formula);
}
    break;

  case 117:
#line 1774 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (1)].formula);
}
    break;

  case 118:
#line 1782 "./sparql_parser.y"
    {
  int i;
  const unsigned char *id;

  if((yyvsp[(2) - (3)].formula) == NULL) {
    (yyval.formula)=rasqal_new_formula();
    if(!(yyval.formula))
      YYERROR_MSG("BlankNodePropertyList: cannot create formula");
  } else {
    (yyval.formula)=(yyvsp[(2) - (3)].formula);
    if((yyval.formula)->value) {
      rasqal_free_literal((yyval.formula)->value);
      (yyval.formula)->value=NULL;
    }
  }
  
  id=rasqal_query_generate_bnodeid((rasqal_query*)rq, NULL);
  if(!id) {
    rasqal_free_formula((yyval.formula));
    (yyval.formula)=NULL;
    YYERROR_MSG("BlankNodeProperyList: cannot create bnodeid");
  }

  (yyval.formula)->value=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_BLANK, id);
  if(!(yyval.formula)->value) {
    rasqal_free_formula((yyval.formula));
    (yyval.formula)=NULL;
    YYERROR_MSG("BlankNodePropertyList: cannot create literal");
  }

  if((yyvsp[(2) - (3)].formula) == NULL) {
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, "TriplesNode\n PropertyList=");
    rasqal_formula_print((yyval.formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
#endif
  } else {
    raptor_sequence *seq=(yyvsp[(2) - (3)].formula)->triples;

    /* non-empty property list, handle it  */
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, "TriplesNode\n PropertyList=");
    raptor_sequence_print(seq, DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
#endif

    for(i=0; i<raptor_sequence_size(seq); i++) {
      rasqal_triple* t2=(rasqal_triple*)raptor_sequence_get_at(seq, i);
      if(t2->subject)
        continue;
      
      t2->subject=(rasqal_literal*)rasqal_new_literal_from_literal((yyval.formula)->value);
    }

#if RASQAL_DEBUG > 1
    fprintf(DEBUG_FH, "  after substitution formula=");
    rasqal_formula_print((yyval.formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n\n");
#endif
  }
}
    break;

  case 119:
#line 1848 "./sparql_parser.y"
    {
  int i;
  rasqal_query* rdf_query=(rasqal_query*)rq;
  rasqal_literal* first_identifier=NULL;
  rasqal_literal* rest_identifier=NULL;
  rasqal_literal* object=NULL;
  rasqal_literal* blank=NULL;

#if RASQAL_DEBUG > 1
  char const *errmsg;
  #define YYERR_MSG_GOTO(label,msg) do { errmsg = msg; goto label; } while(0)
#else
  #define YYERR_MSG_GOTO(label,ignore) goto label
#endif

#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "Collection\n GraphNodeListNotEmpty=");
  raptor_sequence_print((yyvsp[(2) - (3)].seq), DEBUG_FH);
  fprintf(DEBUG_FH, "\n");
#endif

  (yyval.formula)=rasqal_new_formula();
  if(!(yyval.formula))
    YYERR_MSG_GOTO(err_Collection, "Collection: cannot create formula");

  (yyval.formula)->triples=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_triple, (raptor_sequence_print_handler*)rasqal_triple_print);
  if(!(yyval.formula)->triples)
    YYERR_MSG_GOTO(err_Collection, "Collection: cannot create sequence");

  first_identifier=rasqal_new_uri_literal(((rasqal_query*)rq)->world, raptor_uri_copy(rdf_query->world->rdf_first_uri));
  if(!first_identifier)
    YYERR_MSG_GOTO(err_Collection, "Collection: cannot first_identifier");
  
  rest_identifier=rasqal_new_uri_literal(((rasqal_query*)rq)->world, raptor_uri_copy(rdf_query->world->rdf_rest_uri));
  if(!rest_identifier)
    YYERR_MSG_GOTO(err_Collection, "Collection: cannot create rest_identifier");
  
  object=rasqal_new_uri_literal(((rasqal_query*)rq)->world, raptor_uri_copy(rdf_query->world->rdf_nil_uri));
  if(!object)
    YYERR_MSG_GOTO(err_Collection, "Collection: cannot create nil object");

  for(i=raptor_sequence_size((yyvsp[(2) - (3)].seq))-1; i>=0; i--) {
    rasqal_formula* f=(rasqal_formula*)raptor_sequence_get_at((yyvsp[(2) - (3)].seq), i);
    rasqal_triple *t2;
    const unsigned char *blank_id=NULL;

    blank_id=rasqal_query_generate_bnodeid(rdf_query, NULL);
    if(!blank_id)
      YYERR_MSG_GOTO(err_Collection, "Collection: cannot create bnodeid");

    blank=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_BLANK, blank_id);
    if(!blank)
      YYERR_MSG_GOTO(err_Collection, "Collection: cannot create bnode");

    /* Move existing formula triples */
    if(f->triples)
      if(raptor_sequence_join((yyval.formula)->triples, f->triples))
        YYERR_MSG_GOTO(err_Collection, "Collection: sequence join failed");

    /* add new triples we needed */
    t2=rasqal_new_triple(rasqal_new_literal_from_literal(blank),
                         rasqal_new_literal_from_literal(first_identifier),
                         rasqal_new_literal_from_literal(f->value));
    if(!t2)
      YYERR_MSG_GOTO(err_Collection, "Collection: cannot create triple");

    if(raptor_sequence_push((yyval.formula)->triples, t2))
      YYERR_MSG_GOTO(err_Collection, "Collection: cannot create triple");

    t2=rasqal_new_triple(rasqal_new_literal_from_literal(blank),
                         rasqal_new_literal_from_literal(rest_identifier),
                         rasqal_new_literal_from_literal(object));
    if(!t2)
      YYERR_MSG_GOTO(err_Collection, "Collection: cannot create triple 2");

    if(raptor_sequence_push((yyval.formula)->triples, t2))
      YYERR_MSG_GOTO(err_Collection, "Collection: sequence push 2 failed");

    rasqal_free_literal(object);
    object=blank;
    blank=NULL;
  }

  (yyval.formula)->value=object;
  
#if RASQAL_DEBUG > 1
  fprintf(DEBUG_FH, "  after substitution collection=");
  rasqal_formula_print((yyval.formula), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif

  rasqal_free_literal(first_identifier);
  rasqal_free_literal(rest_identifier);

  break; /* success */

  err_Collection:
  
  if(blank)
    rasqal_free_literal(blank);
  if(object)
    rasqal_free_literal(object);
  if(rest_identifier)
    rasqal_free_literal(rest_identifier);
  if(first_identifier)
    rasqal_free_literal(first_identifier);
  if((yyvsp[(2) - (3)].seq))
    raptor_free_sequence((yyvsp[(2) - (3)].seq));
  if((yyval.formula)) {
    rasqal_free_formula((yyval.formula));
    (yyval.formula)=NULL;
  }
  YYERROR_MSG(errmsg);
}
    break;

  case 120:
#line 1968 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "GraphNodeListNotEmpty 1\n");
  if((yyvsp[(2) - (2)].formula)) {
    fprintf(DEBUG_FH, " GraphNode=");
    rasqal_formula_print((yyvsp[(2) - (2)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
  } else  
    fprintf(DEBUG_FH, " and empty GraphNode\n");
  if((yyvsp[(1) - (2)].seq)) {
    fprintf(DEBUG_FH, " GraphNodeListNotEmpty=");
    raptor_sequence_print((yyvsp[(1) - (2)].seq), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
  } else
    fprintf(DEBUG_FH, " and empty GraphNodeListNotEmpty\n");
#endif

  if(!(yyvsp[(2) - (2)].formula))
    (yyval.seq)=NULL;
  else {
    /* FIXME: does not work:
     * $$ not initialized
     * $1 not freed
     * also could need a test case */
    if(raptor_sequence_push((yyval.seq), (yyvsp[(2) - (2)].formula))) {
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("GraphNodeListNotEmpty 1: sequence push failed");
    }
#if RASQAL_DEBUG > 1  
    fprintf(DEBUG_FH, " itemList is now ");
    raptor_sequence_print((yyval.seq), DEBUG_FH);
    fprintf(DEBUG_FH, "\n\n");
#endif
  }

}
    break;

  case 121:
#line 2006 "./sparql_parser.y"
    {
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, "GraphNodeListNotEmpty 2\n");
  if((yyvsp[(1) - (1)].formula)) {
    fprintf(DEBUG_FH, " GraphNode=");
    rasqal_formula_print((yyvsp[(1) - (1)].formula), DEBUG_FH);
    fprintf(DEBUG_FH, "\n");
  } else  
    fprintf(DEBUG_FH, " and empty GraphNode\n");
#endif

  if(!(yyvsp[(1) - (1)].formula))
    (yyval.seq)=NULL;
  else {
    (yyval.seq)=raptor_new_sequence((raptor_sequence_free_handler*)rasqal_free_formula, (raptor_sequence_print_handler*)rasqal_formula_print);
    if(!(yyval.seq)) {
      rasqal_free_formula((yyvsp[(1) - (1)].formula));
      YYERROR_MSG("GraphNodeListNotEmpty 2: cannot create sequence");
    }
    if(raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].formula))) {
      raptor_free_sequence((yyval.seq));
      (yyval.seq)=NULL;
      YYERROR_MSG("GraphNodeListNotEmpty 2: sequence push failed");
    }
  }
#if RASQAL_DEBUG > 1  
  fprintf(DEBUG_FH, " GraphNodeListNotEmpty is now ");
  raptor_sequence_print((yyval.seq), DEBUG_FH);
  fprintf(DEBUG_FH, "\n\n");
#endif
}
    break;

  case 122:
#line 2042 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (1)].formula);
}
    break;

  case 123:
#line 2046 "./sparql_parser.y"
    {
  (yyval.formula)=(yyvsp[(1) - (1)].formula);
}
    break;

  case 124:
#line 2054 "./sparql_parser.y"
    {
  (yyval.formula)=rasqal_new_formula();
  if(!(yyval.formula))
    YYERROR_MSG("VarOrTerm 1: cannot create formula");
  (yyval.formula)->value=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
  if(!(yyval.formula)->value) {
    rasqal_free_formula((yyval.formula));
    (yyval.formula)=NULL;
    YYERROR_MSG("VarOrTerm 1: cannot create literal");
  }
}
    break;

  case 125:
#line 2066 "./sparql_parser.y"
    {
  (yyval.formula)=rasqal_new_formula();
  if(!(yyval.formula)) {
    if((yyvsp[(1) - (1)].literal))
      rasqal_free_literal((yyvsp[(1) - (1)].literal));
    YYERROR_MSG("VarOrTerm 2: cannot create formula");
  }
  (yyval.formula)->value=(yyvsp[(1) - (1)].literal);
}
    break;

  case 126:
#line 2079 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
  if(!(yyval.literal))
    YYERROR_MSG("VarOrIRIref: cannot create literal");
}
    break;

  case 127:
#line 2085 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 128:
#line 2093 "./sparql_parser.y"
    {
  (yyval.variable)=(yyvsp[(2) - (2)].variable);
}
    break;

  case 129:
#line 2097 "./sparql_parser.y"
    {
  (yyval.variable)=(yyvsp[(2) - (2)].variable);
}
    break;

  case 130:
#line 2104 "./sparql_parser.y"
    {
  (yyval.variable)=rasqal_new_variable((rasqal_query*)rq, (yyvsp[(1) - (1)].name), NULL);
  if(!(yyval.variable))
    YYERROR_MSG("VarName: cannot create var");
}
    break;

  case 131:
#line 2115 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 132:
#line 2119 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 133:
#line 2123 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 134:
#line 2127 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 135:
#line 2131 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 136:
#line 2135 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_uri_literal(((rasqal_query*)rq)->world, raptor_uri_copy(((rasqal_query*)rq)->world->rdf_nil_uri));
  if(!(yyval.literal))
    YYERROR_MSG("GraphTerm: cannot create literal");
}
    break;

  case 137:
#line 2144 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 138:
#line 2152 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_OR, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("ConditionalOrExpression: cannot create expr");
}
    break;

  case 139:
#line 2158 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 140:
#line 2166 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_AND, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("ConditionalAndExpression: cannot create expr");
;
}
    break;

  case 141:
#line 2173 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 142:
#line 2182 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_EQ, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RelationalExpression 1: cannot create expr");
}
    break;

  case 143:
#line 2188 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_NEQ, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RelationalExpression 2: cannot create expr");
}
    break;

  case 144:
#line 2194 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_LT, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RelationalExpression 3: cannot create expr");
}
    break;

  case 145:
#line 2200 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_GT, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RelationalExpression 4: cannot create expr");
}
    break;

  case 146:
#line 2206 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_LE, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RelationalExpression 5: cannot create expr");
}
    break;

  case 147:
#line 2212 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_GE, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RelationalExpression 6: cannot create expr");
}
    break;

  case 148:
#line 2218 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 149:
#line 2227 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_PLUS, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("AdditiveExpression 1: cannot create expr");
}
    break;

  case 150:
#line 2233 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_MINUS, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("AdditiveExpression 2: cannot create expr");
}
    break;

  case 151:
#line 2239 "./sparql_parser.y"
    {
  rasqal_expression *e=rasqal_new_literal_expression((yyvsp[(2) - (2)].literal));
  if(!e) {
    rasqal_free_expression((yyvsp[(1) - (2)].expr));
    YYERROR_MSG("AdditiveExpression 3: cannot create expr");
  }
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_PLUS, (yyvsp[(1) - (2)].expr), e);
  if(!(yyval.expr))
    YYERROR_MSG("AdditiveExpression 4: cannot create expr");
}
    break;

  case 152:
#line 2250 "./sparql_parser.y"
    {
  rasqal_expression *e=rasqal_new_literal_expression((yyvsp[(2) - (2)].literal));
  if(!e) {
    rasqal_free_expression((yyvsp[(1) - (2)].expr));
    YYERROR_MSG("AdditiveExpression 5: cannot create expr");
  }
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_PLUS, (yyvsp[(1) - (2)].expr), e);
  if(!(yyval.expr))
    YYERROR_MSG("AdditiveExpression 6: cannot create expr");
}
    break;

  case 153:
#line 2261 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 154:
#line 2268 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_STAR, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("MultiplicativeExpression 1: cannot create expr");
}
    break;

  case 155:
#line 2274 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_SLASH, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("MultiplicativeExpression 2: cannot create expr");
}
    break;

  case 156:
#line 2280 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 157:
#line 2288 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_BANG, (yyvsp[(2) - (2)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("UnaryExpression 1: cannot create expr");
}
    break;

  case 158:
#line 2294 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (2)].expr);
}
    break;

  case 159:
#line 2298 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_UMINUS, (yyvsp[(2) - (2)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("UnaryExpression 3: cannot create expr");
}
    break;

  case 160:
#line 2304 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 161:
#line 2318 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 162:
#line 2322 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 163:
#line 2326 "./sparql_parser.y"
    {
  /* Grammar has IRIrefOrFunction here which is "IRIref ArgList?"
   * and essentially shorthand for FunctionCall | IRIref.  The Rasqal
   * SPARQL lexer distinguishes these for us with IRIrefBrace.
   * IRIref is covered below by GraphTerm.
   */
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 164:
#line 2335 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_literal_expression((yyvsp[(1) - (1)].literal));
  if(!(yyval.expr))
    YYERROR_MSG("PrimaryExpression 4: cannot create expr");
}
    break;

  case 165:
#line 2341 "./sparql_parser.y"
    {
  rasqal_literal *l=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
  if(!l)
    YYERROR_MSG("PrimaryExpression 5: cannot create literal");
  (yyval.expr)=rasqal_new_literal_expression(l);
  if(!(yyval.expr))
    YYERROR_MSG("PrimaryExpression 5: cannot create expr");
}
    break;

  case 166:
#line 2354 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (3)].expr);
}
    break;

  case 167:
#line 2362 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_STR, (yyvsp[(3) - (4)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 1: cannot create expr");
}
    break;

  case 168:
#line 2368 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_LANG, (yyvsp[(3) - (4)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 2: cannot create expr");
}
    break;

  case 169:
#line 2374 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_LANGMATCHES, (yyvsp[(3) - (6)].expr), (yyvsp[(5) - (6)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 3: cannot create expr");
}
    break;

  case 170:
#line 2380 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_DATATYPE, (yyvsp[(3) - (4)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 4: cannot create expr");
}
    break;

  case 171:
#line 2386 "./sparql_parser.y"
    {
  rasqal_literal *l;
  rasqal_expression *e;
  l=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(3) - (4)].variable));
  if(!l)
    YYERROR_MSG("BuiltInCall 5: cannot create literal");
  e=rasqal_new_literal_expression(l);
  if(!e)
    YYERROR_MSG("BuiltInCall 6: cannot create literal expr");
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_BOUND, e);
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 7: cannot create expr");
}
    break;

  case 172:
#line 2400 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_SAMETERM, (yyvsp[(3) - (6)].expr), (yyvsp[(5) - (6)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 8: cannot create expr");
}
    break;

  case 173:
#line 2406 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ISURI, (yyvsp[(3) - (4)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 9: cannot create expr");
}
    break;

  case 174:
#line 2412 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ISBLANK, (yyvsp[(3) - (4)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 10: cannot create expr");
}
    break;

  case 175:
#line 2418 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_ISLITERAL, (yyvsp[(3) - (4)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("BuiltInCall 11: cannot create expr");
}
    break;

  case 176:
#line 2424 "./sparql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 177:
#line 2432 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_3op_expression(RASQAL_EXPR_REGEX, (yyvsp[(3) - (6)].expr), (yyvsp[(5) - (6)].expr), NULL);
  if(!(yyval.expr))
    YYERROR_MSG("RegexExpression 1: cannot create expr");
}
    break;

  case 178:
#line 2438 "./sparql_parser.y"
    {
  (yyval.expr)=rasqal_new_3op_expression(RASQAL_EXPR_REGEX, (yyvsp[(3) - (8)].expr), (yyvsp[(5) - (8)].expr), (yyvsp[(7) - (8)].expr));
  if(!(yyval.expr))
    YYERROR_MSG("RegexExpression 2: cannot create expr");
}
    break;

  case 179:
#line 2451 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_uri_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].uri));
  if(!(yyval.literal))
    YYERROR_MSG("IRIrefBrace 1: cannot create literal");
}
    break;

  case 180:
#line 2457 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_QNAME, (yyvsp[(1) - (1)].name));
  if(!(yyval.literal))
    YYERROR_MSG("IRIrefBrace 2: cannot create literal");
  if(rasqal_literal_expand_qname((rasqal_query*)rq, (yyval.literal))) {
    sparql_query_error_full((rasqal_query*)rq,
                            "QName %s cannot be expanded", (yyvsp[(1) - (1)].name));
    rasqal_free_literal((yyval.literal));
    (yyval.literal)=NULL;
    YYERROR_MSG("IRIrefBrace 2: cannot expand qname");
  }
}
    break;

  case 181:
#line 2476 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 182:
#line 2480 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 183:
#line 2484 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 184:
#line 2491 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 185:
#line 2495 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 186:
#line 2499 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 187:
#line 2507 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 188:
#line 2511 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 189:
#line 2515 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 190:
#line 2523 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 191:
#line 2527 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 192:
#line 2531 "./sparql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 193:
#line 2543 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_uri_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].uri));
  if(!(yyval.literal))
    YYERROR_MSG("IRIref 1: cannot create literal");
}
    break;

  case 194:
#line 2549 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_QNAME, (yyvsp[(1) - (1)].name));
  if(!(yyval.literal))
    YYERROR_MSG("IRIref 2: cannot create literal");
  if(rasqal_literal_expand_qname((rasqal_query*)rq, (yyval.literal))) {
    sparql_query_error_full((rasqal_query*)rq,
                            "QName %s cannot be expanded", (yyvsp[(1) - (1)].name));
    rasqal_free_literal((yyval.literal));
    (yyval.literal)=NULL;
    YYERROR_MSG("IRIrefBrace 2: cannot expand qname");
  }
}
    break;

  case 195:
#line 2568 "./sparql_parser.y"
    {
  (yyval.literal)=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_BLANK, (yyvsp[(1) - (1)].name));
  if(!(yyval.literal))
    YYERROR_MSG("BlankNode 1: cannot create literal");
}
    break;

  case 196:
#line 2573 "./sparql_parser.y"
    {
  const unsigned char *id=rasqal_query_generate_bnodeid((rasqal_query*)rq, NULL);
  if(!id)
    YYERROR_MSG("BlankNode 2: cannot create bnodeid");
  (yyval.literal)=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_BLANK, id);
  if(!(yyval.literal))
    YYERROR_MSG("BlankNode 2: cannot create literal");
}
    break;


/* Line 1267 of yacc.c.  */
#line 4824 "sparql_parser.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 2590 "./sparql_parser.y"



/* Support functions */


/* This is declared in sparql_lexer.h but never used, so we always get
 * a warning unless this dummy code is here.  Used once below in an error case.
 */
static int yy_init_globals (yyscan_t yyscanner ) { return 0; }


/**
 * rasqal_sparql_query_engine_init - Initialise the SPARQL query engine
 *
 * Return value: non 0 on failure
 **/
static int
rasqal_sparql_query_engine_init(rasqal_query* rdf_query, const char *name) {
  rasqal_sparql_query_engine* rqe=(rasqal_sparql_query_engine*)rdf_query->context;

  rdf_query->compare_flags = RASQAL_COMPARE_XQUERY;

  rqe->extended = (strcmp(name, "laqrs") == 0);
  return 0;
}


/**
 * rasqal_sparql_query_engine_terminate - Free the SPARQL query engine
 *
 * Return value: non 0 on failure
 **/
static void
rasqal_sparql_query_engine_terminate(rasqal_query* rdf_query) {
  rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)rdf_query->context;

  if(sparql && sparql->scanner_set) {
    sparql_lexer_lex_destroy(sparql->scanner);
    sparql->scanner_set=0;
  }

}


static int
rasqal_sparql_query_engine_prepare(rasqal_query* rdf_query) {
  /* rasqal_sparql_query_engine* sparql=(rasqal_sparql_query_engine*)rdf_query->context; */
  int rc;
  
  if(!rdf_query->query_string)
    return 1;
  
  rc=sparql_parse(rdf_query);
  if(rc)
    return rc;

  /* FIXME - should check remaining query parts  */
  if(rasqal_engine_sequence_has_qname(rdf_query->triples) ||
     rasqal_engine_sequence_has_qname(rdf_query->constructs) ||
     rasqal_engine_query_constraints_has_qname(rdf_query)) {
    sparql_query_error(rdf_query, "SPARQL query has unexpanded QNames");
    return 1;
  }

  return rasqal_engine_prepare(rdf_query);
}


static int
sparql_parse(rasqal_query* rq) {
  rasqal_sparql_query_engine* rqe=(rasqal_sparql_query_engine*)rq->context;
  raptor_locator *locator=&rq->locator;
  void *buffer;

  if(!rq->query_string)
    return yy_init_globals(NULL); /* 0 but a way to use yy_init_globals */

  locator->line=1;
  locator->column= -1; /* No column info */
  locator->byte= -1; /* No bytes info */

#if RASQAL_DEBUG > 2
  sparql_parser_debug=1;
#endif

  rqe->lineno=1;

  if(sparql_lexer_lex_init(&rqe->scanner))
    return 1;
  rqe->scanner_set=1;

  sparql_lexer_set_extra(((rasqal_query*)rq), rqe->scanner);

  buffer= sparql_lexer__scan_buffer((char*)rq->query_string, rq->query_string_length, rqe->scanner);

  rqe->error_count=0;

  sparql_parser_parse(rq);

  sparql_lexer_lex_destroy(rqe->scanner);
  rqe->scanner_set=0;

  /* Parsing failed */
  if(rq->failed)
    return 1;
  
  return 0;
}


static void
sparql_query_error(rasqal_query *rq, const char *msg) {
  rasqal_sparql_query_engine* rqe=(rasqal_sparql_query_engine*)rq->context;

  if(rqe->error_count++)
    return;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_SPARQL_USE_ERROR_COLUMNS
  /*  rq->locator.column=sparql_lexer_get_column(yyscanner);*/
#endif

  rq->failed=1;
  rasqal_log_error_simple(((rasqal_query*)rq)->world, RAPTOR_LOG_LEVEL_ERROR, &rq->locator,
                          "%s", msg);
}


static void
sparql_query_error_full(rasqal_query *rq, const char *message, ...) {
  va_list arguments;
  rasqal_sparql_query_engine* rqe=(rasqal_sparql_query_engine*)rq->context;

  if(rqe->error_count++)
    return;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_SPARQL_USE_ERROR_COLUMNS
  /*  rq->locator.column=sparql_lexer_get_column(yyscanner);*/
#endif

  va_start(arguments, message);

  rq->failed=1;
  rasqal_log_error_varargs(((rasqal_query*)rq)->world, RAPTOR_LOG_LEVEL_ERROR, &rq->locator,
                           message, arguments);

  va_end(arguments);
}


int
sparql_syntax_error(rasqal_query *rq, const char *message, ...)
{
  rasqal_sparql_query_engine *rqe=(rasqal_sparql_query_engine*)rq->context;
  va_list arguments;

  if(rqe->error_count++)
    return 0;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_SPARQL_USE_ERROR_COLUMNS
  /*  rp->locator.column=sparql_lexer_get_column(yyscanner);*/
#endif

  va_start(arguments, message);
  rq->failed=1;
  rasqal_log_error_varargs(((rasqal_query*)rq)->world, RAPTOR_LOG_LEVEL_ERROR, &rq->locator,
                           message, arguments);
  va_end(arguments);

  return 0;
}


int
sparql_syntax_warning(rasqal_query *rq, const char *message, ...)
{
  rasqal_sparql_query_engine *rqe=(rasqal_sparql_query_engine*)rq->context;
  va_list arguments;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_SPARQL_USE_ERROR_COLUMNS
  /*  rq->locator.column=sparql_lexer_get_column(yyscanner);*/
#endif

  va_start(arguments, message);
  rasqal_log_error_varargs(((rasqal_query*)rq)->world, RAPTOR_LOG_LEVEL_WARNING, &rq->locator,
                           message, arguments);
  va_end(arguments);

  return (0);
}


static int
rasqal_sparql_query_engine_iostream_write_escaped_counted_string(rasqal_query* query,
                                                                 raptor_iostream* iostr,
                                                                 const unsigned char* string,
                                                                 size_t len)
{
  const char delim='"';
  
  raptor_iostream_write_byte(iostr, delim);
  if(raptor_iostream_write_string_ntriples(iostr, string, len, delim))
    return 1;
  
  raptor_iostream_write_byte(iostr, delim);

  return 0;
}


static void
rasqal_sparql_query_engine_register_factory(rasqal_query_engine_factory *factory)
{
  factory->context_length = sizeof(rasqal_sparql_query_engine);

  factory->init      = rasqal_sparql_query_engine_init;
  factory->terminate = rasqal_sparql_query_engine_terminate;
  factory->prepare   = rasqal_sparql_query_engine_prepare;
  factory->iostream_write_escaped_counted_string = rasqal_sparql_query_engine_iostream_write_escaped_counted_string;
}


int
rasqal_init_query_engine_sparql(rasqal_world* world) {
  return rasqal_query_engine_register_factory(world,
                                              "sparql", 
                                              "SPARQL W3C DAWG RDF Query Language",
                                              NULL,
                                              (const unsigned char*)"http://www.w3.org/TR/rdf-sparql-query/",
                                              &rasqal_sparql_query_engine_register_factory);
}

int
rasqal_init_query_engine_laqrs(rasqal_world* world) {
  return rasqal_query_engine_register_factory(world,
                                             "laqrs", 
                                             "LAQRS adds to Querying RDF in SPARQL",
                                             NULL,
                                             NULL,
                                             &rasqal_sparql_query_engine_register_factory);
}


#ifdef STANDALONE
#include <stdio.h>
#include <locale.h>
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef HAVE_GETOPT
#include <rasqal_getopt.h>
#endif

#ifdef NEED_OPTIND_DECLARATION
extern int optind;
extern char *optarg;
#endif

#define GETOPT_STRING "di:"

#define SPARQL_FILE_BUF_SIZE 2048

int
main(int argc, char *argv[]) 
{
  const char *program=rasqal_basename(argv[0]);
  char query_string[SPARQL_FILE_BUF_SIZE];
  rasqal_query *query;
  FILE *fh;
  int rc;
  const char *filename=NULL;
  raptor_uri* base_uri=NULL;
  unsigned char *uri_string;
  const char* query_languages[2]={"sparql", "laqrs"};
  const char* query_language;
  int usage=0;
  int fh_opened_here=0;
  rasqal_world *world;
  
  query_language=query_languages[0];
  
  while(!usage) {
    int c = getopt (argc, argv, GETOPT_STRING);

    if (c == -1)
      break;

    switch (c) {
      case 0:
      case '?': /* getopt() - unknown option */
        usage=1;
        break;
        
      case 'd':
#if RASQAL_DEBUG > 2
        sparql_parser_debug=1;
#endif
        break;
  
      case 'i':
        if(optarg) {
          if(!strcmp(optarg, "laqrs")) {
            query_language=query_languages[1];
          } else if(!strcmp(optarg, "sparql")) {
            query_language=query_languages[0];
          } else {
            fprintf(stderr, "-i laqrs or -i sparql only\n");
            usage=1;
          }
        }
        break;
    }
  }

  if((argc-optind)>1) {
    fprintf(stderr, "%s: Too many arguments.\n", program);
    usage=1;
  }
  
  if(usage) {
    fprintf(stderr, "SPARQL/LAQRS parser test for Rasqal %s\n", 
            rasqal_version_string);
    fprintf(stderr, "USAGE: %s [OPTIONS] [QUERY-FILE]\n", program);
    fprintf(stderr, "OPTIONS:\n");
#if RASQAL_DEBUG > 2
    fprintf(stderr, " -d           Bison parser debugging\n");
#endif
    fprintf(stderr, " -i LANGUAGE  Set query language\n");
    exit(1);
  }

 if(optind == argc-1) {
    filename=argv[optind];
    fh = fopen(argv[optind], "r");
    if(!fh) {
      fprintf(stderr, "%s: Cannot open file %s - %s\n", program, filename,
              strerror(errno));
      exit(1);
    }
    fh_opened_here=1;
 } else {
    filename="<stdin>";
    fh = stdin;
  }

  memset(query_string, 0, SPARQL_FILE_BUF_SIZE);
  rc=fread(query_string, SPARQL_FILE_BUF_SIZE, 1, fh);
  if(rc < SPARQL_FILE_BUF_SIZE) {
    if(ferror(fh)) {
      fprintf(stderr, "%s: file '%s' read failed - %s\n",
              program, filename, strerror(errno));
      fclose(fh);
      return(1);
    }
  }
  
  if(fh_opened_here)
    fclose(fh);

  world=rasqal_new_world();

  query=rasqal_new_query(world, query_language, NULL);

  uri_string=raptor_uri_filename_to_uri_string(filename);
  base_uri=raptor_new_uri(uri_string);

  rc=rasqal_query_prepare(query, (const unsigned char*)query_string, base_uri);

  rasqal_query_print(query, DEBUG_FH);

  rasqal_free_query(query);

  raptor_free_uri(base_uri);

  raptor_free_memory(uri_string);

  rasqal_free_world(world);

  return rc;
}
#endif

