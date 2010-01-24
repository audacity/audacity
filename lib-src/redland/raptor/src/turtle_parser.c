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
#define yyparse turtle_parser_parse
#define yylex   turtle_parser_lex
#define yyerror turtle_parser_error
#define yylval  turtle_parser_lval
#define yychar  turtle_parser_char
#define yydebug turtle_parser_debug
#define yynerrs turtle_parser_nerrs


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




/* Copy the first part of user declarations.  */
#line 30 "./turtle_parser.y"

#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

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

#define YY_DECL int turtle_lexer_lex (YYSTYPE *turtle_parser_lval, yyscan_t yyscanner)
#define YY_NO_UNISTD_H 1
#include <turtle_lexer.h>

#include <turtle_common.h>


/* Make verbose error messages for syntax errors */
#ifdef RAPTOR_DEBUG
#define YYERROR_VERBOSE 1
#endif

/* Slow down the grammar operation and watch it work */
#if RAPTOR_DEBUG > 2
#define YYDEBUG 1
#endif

/* the lexer does not seem to track this */
#undef RAPTOR_TURTLE_USE_ERROR_COLUMNS

/* Prototypes */ 
int turtle_parser_error(void* rdf_parser, const char *msg);

/* Missing turtle_lexer.c/h prototypes */
int turtle_lexer_get_column(yyscan_t yyscanner);
/* Not used here */
/* void turtle_lexer_set_column(int  column_no , yyscan_t yyscanner);*/


/* What the lexer wants */
extern int turtle_lexer_lex (YYSTYPE *turtle_parser_lval, yyscan_t scanner);
#define YYLEX_PARAM ((raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context))->scanner

/* Pure parser argument (a void*) */
#define YYPARSE_PARAM rdf_parser

/* Make the yyerror below use the rdf_parser */
#undef yyerror
#define yyerror(message) turtle_parser_error(rdf_parser, message)

/* Make lex/yacc interface as small as possible */
#undef yylex
#define yylex turtle_lexer_lex


static raptor_triple* raptor_turtle_new_triple(raptor_identifier *subject, raptor_identifier *predicate, raptor_identifier *object);
static void raptor_turtle_free_triple(raptor_triple *triple);

#ifdef RAPTOR_DEBUG
static void raptor_triple_print(raptor_triple *data, FILE *fh);
#endif


/* Prototypes for local functions */
static void raptor_turtle_generate_statement(raptor_parser *parser, raptor_triple *triple);



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
#line 122 "./turtle_parser.y"
{
  unsigned char *string;
  raptor_identifier *identifier;
  raptor_sequence *sequence;
  raptor_uri *uri;
  int integer; /* 0+ for a xsd:integer datatyped RDF literal */
}
/* Line 187 of yacc.c.  */
#line 251 "turtle_parser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 264 "turtle_parser.c"

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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   117

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  29
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  27
/* YYNRULES -- Number of rules.  */
#define YYNRULES  60
/* YYNRULES -- Number of states.  */
#define YYNSTATES  82

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   283

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      25,    26,    27,    28
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     7,     8,     9,    16,    17,    22,
      24,    26,    27,    29,    32,    34,    37,    40,    41,    43,
      45,    47,    50,    53,    57,    59,    62,    64,    66,    68,
      73,    76,    77,    80,    82,    84,    89,    93,    95,    97,
      99,   101,   103,   105,   109,   115,   121,   125,   129,   131,
     133,   135,   137,   139,   141,   143,   145,   147,   151,   153,
     157
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      30,     0,    -1,    39,    -1,    15,    -1,    -1,    -1,    35,
      31,    13,    33,    36,    14,    -1,    -1,    13,    34,    36,
      14,    -1,    53,    -1,    37,    -1,    -1,    41,    -1,    38,
      37,    -1,    38,    -1,    41,     6,    -1,    39,    40,    -1,
      -1,    46,    -1,    32,    -1,    38,    -1,    49,    45,    -1,
       1,     6,    -1,    42,     7,    51,    -1,    51,    -1,    43,
      51,    -1,    51,    -1,    50,    -1,     3,    -1,    45,     8,
      44,    42,    -1,    44,    42,    -1,    -1,    45,     8,    -1,
      47,    -1,    48,    -1,    22,    24,    19,     6,    -1,    23,
      19,     6,    -1,    53,    -1,    54,    -1,    53,    -1,    53,
      -1,    54,    -1,    52,    -1,    18,     4,    24,    -1,    18,
       4,    24,     5,    19,    -1,    18,     4,    24,     5,    21,
      -1,    18,     5,    19,    -1,    18,     5,    21,    -1,    18,
      -1,    25,    -1,    26,    -1,    27,    -1,    16,    -1,    17,
      -1,    19,    -1,    21,    -1,    20,    -1,     9,    45,    10,
      -1,    55,    -1,    11,    43,    12,    -1,    11,    12,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   177,   177,   180,   187,   191,   190,   207,   206,   219,
     222,   223,   227,   229,   231,   234,   237,   238,   241,   242,
     243,   246,   295,   299,   339,   383,   423,   467,   477,   495,
     572,   617,   623,   634,   634,   637,   678,   689,   693,   700,
     707,   711,   715,   728,   738,   752,   766,   780,   793,   803,
     818,   833,   848,   868,   891,   904,   920,   934,   999,  1006,
    1106
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"a\"", "\"@\"", "\"^\"", "\".\"",
  "\",\"", "\";\"", "\"[\"", "\"]\"", "\"(\"", "\")\"", "\"{\"", "\"}\"",
  "\":-\"", "\"true\"", "\"false\"", "\"string literal\"",
  "\"URI literal\"", "\"blank node\"", "\"QName\"", "\"@prefix\"",
  "\"@base\"", "\"identifier\"", "\"integer literal\"",
  "\"floating point literal\"", "\"decimal literal\"", "ERROR_TOKEN",
  "$accept", "Document", "colonMinusOpt", "graph", "@1", "@2", "graphName",
  "graphBody", "triplesList", "terminatedTriples", "statementList",
  "statement", "triples", "objectList", "itemList", "verb", "propertyList",
  "directive", "prefix", "base", "subject", "predicate", "object",
  "literal", "resource", "blank", "collection", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    29,    30,    31,    31,    33,    32,    34,    32,    35,
      36,    36,    37,    37,    37,    38,    39,    39,    40,    40,
      40,    41,    41,    42,    42,    43,    43,    44,    44,    45,
      45,    45,    45,    46,    46,    47,    48,    49,    49,    50,
      51,    51,    51,    52,    52,    52,    52,    52,    52,    52,
      52,    52,    52,    52,    53,    53,    54,    54,    54,    55,
      55
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     0,     0,     6,     0,     4,     1,
       1,     0,     1,     2,     1,     2,     2,     0,     1,     1,
       1,     2,     2,     3,     1,     2,     1,     1,     1,     4,
       2,     0,     2,     1,     1,     4,     3,     1,     1,     1,
       1,     1,     1,     3,     5,     5,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     3,
       2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      17,     0,     0,     1,     0,    31,     0,     7,    54,    56,
      55,     0,     0,    19,     4,    20,    16,     0,    18,    33,
      34,    31,    37,    38,    58,    22,    28,     0,     0,    27,
      39,    60,    52,    53,    48,    49,    50,    51,     0,    26,
      42,    40,    41,     0,     0,     0,     3,     0,    15,    21,
      30,    24,    32,    57,     0,     0,    59,    25,     0,    10,
       0,    12,    37,     0,    36,     5,     0,     0,    43,    46,
      47,     8,    13,    35,     0,    23,    29,     0,     0,    44,
      45,     6
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,    47,    13,    74,    43,    14,    58,    59,    60,
       2,    16,    61,    50,    38,    27,    28,    18,    19,    20,
      21,    29,    51,    40,    41,    42,    24
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -12
static const yytype_int8 yypact[] =
{
     -12,     7,     4,   -12,    12,    28,    58,   -12,   -12,   -12,
     -12,     5,    13,   -12,    21,   -12,   -12,    31,   -12,   -12,
     -12,    28,    -7,   -12,   -12,   -12,   -12,    90,     1,   -12,
     -12,   -12,   -12,   -12,    17,   -12,   -12,   -12,    77,   -12,
     -12,   -12,   -12,    19,    25,    40,   -12,    38,   -12,    44,
      49,   -12,    28,   -12,    33,    -9,   -12,   -12,    46,   -12,
      34,    31,   -12,    55,   -12,   -12,    90,    90,    59,   -12,
     -12,   -12,   -12,   -12,    19,   -12,    49,    -5,    51,   -12,
     -12,   -12
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -11,     6,    66,
     -12,   -12,    69,    14,   -12,    30,    70,   -12,   -12,   -12,
     -12,   -12,    -4,   -12,    -2,    -1,   -12
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -15
static const yytype_int8 yytable[] =
{
      22,    23,    39,    30,    -2,     4,    -9,     3,    -9,    52,
      69,    53,    70,     5,    79,     6,    80,     7,    25,    30,
       4,    54,    55,     8,     9,    10,    11,    12,     5,    44,
       6,    26,    45,   -11,    57,     4,    46,    48,     8,     9,
      10,    62,    23,     5,    63,     6,    64,     8,   -14,    10,
      30,    65,    52,     8,     9,    10,    66,    68,    62,    23,
      71,    73,    75,    78,    77,    81,    72,     5,    15,     6,
      31,    17,    62,    23,    32,    33,    34,     8,     9,    10,
       0,    76,    67,    35,    36,    37,     5,     0,     6,    56,
       0,    49,     0,    32,    33,    34,     8,     9,    10,     5,
       0,     6,    35,    36,    37,     0,    32,    33,    34,     8,
       9,    10,     0,     0,     0,    35,    36,    37
};

static const yytype_int8 yycheck[] =
{
       2,     2,     6,     5,     0,     1,    13,     0,    15,     8,
      19,    10,    21,     9,    19,    11,    21,    13,     6,    21,
       1,     4,     5,    19,    20,    21,    22,    23,     9,    24,
      11,     3,    19,    14,    38,     1,    15,     6,    19,    20,
      21,    43,    43,     9,    19,    11,     6,    19,    14,    21,
      52,    13,     8,    19,    20,    21,     7,    24,    60,    60,
      14,     6,    66,    74,     5,    14,    60,     9,     2,    11,
      12,     2,    74,    74,    16,    17,    18,    19,    20,    21,
      -1,    67,    52,    25,    26,    27,     9,    -1,    11,    12,
      -1,    21,    -1,    16,    17,    18,    19,    20,    21,     9,
      -1,    11,    25,    26,    27,    -1,    16,    17,    18,    19,
      20,    21,    -1,    -1,    -1,    25,    26,    27
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    30,    39,     0,     1,     9,    11,    13,    19,    20,
      21,    22,    23,    32,    35,    38,    40,    41,    46,    47,
      48,    49,    53,    54,    55,     6,     3,    44,    45,    50,
      53,    12,    16,    17,    18,    25,    26,    27,    43,    51,
      52,    53,    54,    34,    24,    19,    15,    31,     6,    45,
      42,    51,     8,    10,     4,     5,    12,    51,    36,    37,
      38,    41,    53,    19,     6,    13,     7,    44,    24,    19,
      21,    14,    37,     6,    33,    51,    42,     5,    36,    19,
      21,    14
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
      case 18: /* "\"string literal\"" */
#line 170 "./turtle_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1239 "turtle_parser.c"
	break;
      case 19: /* "\"URI literal\"" */
#line 171 "./turtle_parser.y"
	{ if((yyvaluep->uri)) raptor_free_uri((yyvaluep->uri)); };
#line 1244 "turtle_parser.c"
	break;
      case 20: /* "\"blank node\"" */
#line 170 "./turtle_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1249 "turtle_parser.c"
	break;
      case 21: /* "\"QName\"" */
#line 171 "./turtle_parser.y"
	{ if((yyvaluep->uri)) raptor_free_uri((yyvaluep->uri)); };
#line 1254 "turtle_parser.c"
	break;
      case 24: /* "\"identifier\"" */
#line 170 "./turtle_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1259 "turtle_parser.c"
	break;
      case 25: /* "\"integer literal\"" */
#line 170 "./turtle_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1264 "turtle_parser.c"
	break;
      case 26: /* "\"floating point literal\"" */
#line 170 "./turtle_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1269 "turtle_parser.c"
	break;
      case 27: /* "\"decimal literal\"" */
#line 170 "./turtle_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1274 "turtle_parser.c"
	break;
      case 35: /* "graphName" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1279 "turtle_parser.c"
	break;
      case 42: /* "objectList" */
#line 173 "./turtle_parser.y"
	{ if((yyvaluep->sequence)) raptor_free_sequence((yyvaluep->sequence)); };
#line 1284 "turtle_parser.c"
	break;
      case 43: /* "itemList" */
#line 173 "./turtle_parser.y"
	{ if((yyvaluep->sequence)) raptor_free_sequence((yyvaluep->sequence)); };
#line 1289 "turtle_parser.c"
	break;
      case 44: /* "verb" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1294 "turtle_parser.c"
	break;
      case 45: /* "propertyList" */
#line 173 "./turtle_parser.y"
	{ if((yyvaluep->sequence)) raptor_free_sequence((yyvaluep->sequence)); };
#line 1299 "turtle_parser.c"
	break;
      case 49: /* "subject" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1304 "turtle_parser.c"
	break;
      case 50: /* "predicate" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1309 "turtle_parser.c"
	break;
      case 51: /* "object" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1314 "turtle_parser.c"
	break;
      case 52: /* "literal" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1319 "turtle_parser.c"
	break;
      case 53: /* "resource" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1324 "turtle_parser.c"
	break;
      case 54: /* "blank" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1329 "turtle_parser.c"
	break;
      case 55: /* "collection" */
#line 172 "./turtle_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1334 "turtle_parser.c"
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
        case 3:
#line 181 "./turtle_parser.y"
    {
  raptor_parser* parser=(raptor_parser *)rdf_parser;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)parser->context;
  if(!turtle_parser->trig)
    turtle_parser_error(rdf_parser, ":- is not allowed in Turtle");
}
    break;

  case 5:
#line 191 "./turtle_parser.y"
    {
    /* action in mid-rule so this is run BEFORE the triples in graphBody */
    raptor_parser* parser=(raptor_parser *)rdf_parser;
    raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)parser->context;
    if(!turtle_parser->trig)
      turtle_parser_error(rdf_parser, "{ ... } is not allowed in Turtle");
    else
      raptor_parser_set_graph_name(parser, (yyvsp[(1) - (3)].identifier)->uri);
  }
    break;

  case 6:
#line 201 "./turtle_parser.y"
    {
  /* free graph name in final action */
  raptor_free_identifier((yyvsp[(1) - (6)].identifier));
}
    break;

  case 7:
#line 207 "./turtle_parser.y"
    {
    /* action in mid-rule so this is run BEFORE the triples in graphBody */
    raptor_parser* parser=(raptor_parser *)rdf_parser;
    raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)parser->context;
    if(!turtle_parser->trig)
      turtle_parser_error(rdf_parser, "{ ... } is not allowed in Turtle");
    else
      raptor_parser_set_graph_name(parser, NULL);
  }
    break;

  case 21:
#line 247 "./turtle_parser.y"
    {
  int i;

#if RAPTOR_DEBUG > 1  
  printf("statement 2\n subject=");
  if((yyvsp[(1) - (2)].identifier))
    raptor_identifier_print(stdout, (yyvsp[(1) - (2)].identifier));
  else
    fputs("NULL", stdout);
  if((yyvsp[(2) - (2)].sequence)) {
    printf("\n propertyList (reverse order to syntax)=");
    raptor_sequence_print((yyvsp[(2) - (2)].sequence), stdout);
    printf("\n");
  } else     
    printf("\n and empty propertyList\n");
#endif

  if((yyvsp[(1) - (2)].identifier) && (yyvsp[(2) - (2)].sequence)) {
    /* have subject and non-empty property list, handle it  */
    for(i=0; i<raptor_sequence_size((yyvsp[(2) - (2)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (2)].sequence), i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_sequence((yyvsp[(2) - (2)].sequence));
        raptor_free_identifier((yyvsp[(1) - (2)].identifier));
        YYERROR;
      }
      raptor_copy_identifier(i2, (yyvsp[(1) - (2)].identifier));
      t2->subject=i2;
      t2->subject->is_malloced=1;
    }
#if RAPTOR_DEBUG > 1  
    printf(" after substitution propertyList=");
    raptor_sequence_print((yyvsp[(2) - (2)].sequence), stdout);
    printf("\n\n");
#endif
    for(i=0; i<raptor_sequence_size((yyvsp[(2) - (2)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (2)].sequence), i);
      raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);
    }
  }

  if((yyvsp[(2) - (2)].sequence))
    raptor_free_sequence((yyvsp[(2) - (2)].sequence));

  if((yyvsp[(1) - (2)].identifier))
    raptor_free_identifier((yyvsp[(1) - (2)].identifier));
}
    break;

  case 23:
#line 300 "./turtle_parser.y"
    {
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 1\n");
  if((yyvsp[(3) - (3)].identifier)) {
    printf(" object=\n");
    raptor_identifier_print(stdout, (yyvsp[(3) - (3)].identifier));
    printf("\n");
  } else  
    printf(" and empty object\n");
  if((yyvsp[(1) - (3)].sequence)) {
    printf(" objectList=");
    raptor_sequence_print((yyvsp[(1) - (3)].sequence), stdout);
    printf("\n");
  } else
    printf(" and empty objectList\n");
#endif

  if(!(yyvsp[(3) - (3)].identifier))
    (yyval.sequence)=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, (yyvsp[(3) - (3)].identifier));
    if(!triple) {
      raptor_free_sequence((yyvsp[(1) - (3)].sequence));
      YYERROR;
    }
    if(raptor_sequence_push((yyvsp[(1) - (3)].sequence), triple)) {
      raptor_free_sequence((yyvsp[(1) - (3)].sequence));
      YYERROR;
    }
    (yyval.sequence)=(yyvsp[(1) - (3)].sequence);
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print((yyval.sequence), stdout);
    printf("\n\n");
#endif
  }
}
    break;

  case 24:
#line 340 "./turtle_parser.y"
    {
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 2\n");
  if((yyvsp[(1) - (1)].identifier)) {
    printf(" object=\n");
    raptor_identifier_print(stdout, (yyvsp[(1) - (1)].identifier));
    printf("\n");
  } else  
    printf(" and empty object\n");
#endif

  if(!(yyvsp[(1) - (1)].identifier))
    (yyval.sequence)=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, (yyvsp[(1) - (1)].identifier));
    if(!triple)
      YYERROR;
#ifdef RAPTOR_DEBUG
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple,
                           (raptor_sequence_print_handler*)raptor_triple_print);
#else
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple, NULL);
#endif
    if(!(yyval.sequence)) {
      raptor_turtle_free_triple(triple);
      YYERROR;
    }
    if(raptor_sequence_push((yyval.sequence), triple)) {
      raptor_free_sequence((yyval.sequence));
      (yyval.sequence)=NULL;
      YYERROR;
    }
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print((yyval.sequence), stdout);
    printf("\n\n");
#endif
  }
}
    break;

  case 25:
#line 384 "./turtle_parser.y"
    {
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 1\n");
  if((yyvsp[(2) - (2)].identifier)) {
    printf(" object=\n");
    raptor_identifier_print(stdout, (yyvsp[(2) - (2)].identifier));
    printf("\n");
  } else  
    printf(" and empty object\n");
  if((yyvsp[(1) - (2)].sequence)) {
    printf(" objectList=");
    raptor_sequence_print((yyvsp[(1) - (2)].sequence), stdout);
    printf("\n");
  } else
    printf(" and empty objectList\n");
#endif

  if(!(yyvsp[(2) - (2)].identifier))
    (yyval.sequence)=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, (yyvsp[(2) - (2)].identifier));
    if(!triple) {
      raptor_free_sequence((yyvsp[(1) - (2)].sequence));
      YYERROR;
    }
    if(raptor_sequence_push((yyvsp[(1) - (2)].sequence), triple)) {
      raptor_free_sequence((yyvsp[(1) - (2)].sequence));
      YYERROR;
    }
    (yyval.sequence)=(yyvsp[(1) - (2)].sequence);
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print((yyval.sequence), stdout);
    printf("\n\n");
#endif
  }
}
    break;

  case 26:
#line 424 "./turtle_parser.y"
    {
  raptor_triple *triple;

#if RAPTOR_DEBUG > 1  
  printf("objectList 2\n");
  if((yyvsp[(1) - (1)].identifier)) {
    printf(" object=\n");
    raptor_identifier_print(stdout, (yyvsp[(1) - (1)].identifier));
    printf("\n");
  } else  
    printf(" and empty object\n");
#endif

  if(!(yyvsp[(1) - (1)].identifier))
    (yyval.sequence)=NULL;
  else {
    triple=raptor_turtle_new_triple(NULL, NULL, (yyvsp[(1) - (1)].identifier));
    if(!triple)
      YYERROR;
#ifdef RAPTOR_DEBUG
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple,
                           (raptor_sequence_print_handler*)raptor_triple_print);
#else
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_turtle_free_triple, NULL);
#endif
    if(!(yyval.sequence)) {
      raptor_turtle_free_triple(triple);
      YYERROR;
    }
    if(raptor_sequence_push((yyval.sequence), triple)) {
      raptor_free_sequence((yyval.sequence));
      (yyval.sequence)=NULL;
      YYERROR;
    }
#if RAPTOR_DEBUG > 1  
    printf(" objectList is now ");
    raptor_sequence_print((yyval.sequence), stdout);
    printf("\n\n");
#endif
  }
}
    break;

  case 27:
#line 468 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("verb predicate=");
  raptor_identifier_print(stdout, (yyvsp[(1) - (1)].identifier));
  printf("\n");
#endif

  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 28:
#line 478 "./turtle_parser.y"
    {
  raptor_uri *uri;

#if RAPTOR_DEBUG > 1  
  printf("verb predicate=rdf:type (a)\n");
#endif

  uri=raptor_new_uri_for_rdf_concept("type");
  if(!uri)
    YYERROR;
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, uri, RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 29:
#line 496 "./turtle_parser.y"
    {
  int i;
  
#if RAPTOR_DEBUG > 1  
  printf("propertyList 1\n verb=");
  raptor_identifier_print(stdout, (yyvsp[(3) - (4)].identifier));
  printf("\n objectList=");
  raptor_sequence_print((yyvsp[(4) - (4)].sequence), stdout);
  printf("\n propertyList=");
  raptor_sequence_print((yyvsp[(1) - (4)].sequence), stdout);
  printf("\n\n");
#endif
  
  if((yyvsp[(4) - (4)].sequence) == NULL) {
#if RAPTOR_DEBUG > 1  
    printf(" empty objectList not processed\n");
#endif
  } else if((yyvsp[(3) - (4)].identifier) && (yyvsp[(4) - (4)].sequence)) {
    /* non-empty property list, handle it  */
    for(i=0; i<raptor_sequence_size((yyvsp[(4) - (4)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(4) - (4)].sequence), i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        if((yyvsp[(1) - (4)].sequence))
          raptor_free_sequence((yyvsp[(1) - (4)].sequence));
        raptor_free_identifier((yyvsp[(3) - (4)].identifier));
        raptor_free_sequence((yyvsp[(4) - (4)].sequence));
        YYERROR;
      }
      if(raptor_copy_identifier(i2, (yyvsp[(3) - (4)].identifier))) {
        if((yyvsp[(1) - (4)].sequence))
          raptor_free_sequence((yyvsp[(1) - (4)].sequence));
        raptor_free_identifier((yyvsp[(3) - (4)].identifier));
        raptor_free_sequence((yyvsp[(4) - (4)].sequence));
        YYERROR;
      }
      t2->predicate=i2;
      t2->predicate->is_malloced=1;
    }
  
#if RAPTOR_DEBUG > 1  
    printf(" after substitution objectList=");
    raptor_sequence_print((yyvsp[(4) - (4)].sequence), stdout);
    printf("\n");
#endif
  }

  if((yyvsp[(1) - (4)].sequence) == NULL) {
#if RAPTOR_DEBUG > 1  
    printf(" empty propertyList not copied\n\n");
#endif
  } else if ((yyvsp[(3) - (4)].identifier) && (yyvsp[(4) - (4)].sequence) && (yyvsp[(1) - (4)].sequence)) {
    while(raptor_sequence_size((yyvsp[(4) - (4)].sequence))) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_unshift((yyvsp[(4) - (4)].sequence));
      if(raptor_sequence_push((yyvsp[(1) - (4)].sequence), t2)) {
        raptor_free_sequence((yyvsp[(1) - (4)].sequence));
        raptor_free_identifier((yyvsp[(3) - (4)].identifier));
        raptor_free_sequence((yyvsp[(4) - (4)].sequence));
        YYERROR;
      }
    }

#if RAPTOR_DEBUG > 1  
    printf(" after appending objectList (reverse order)=");
    raptor_sequence_print((yyvsp[(1) - (4)].sequence), stdout);
    printf("\n\n");
#endif

    raptor_free_sequence((yyvsp[(4) - (4)].sequence));
  }

  if((yyvsp[(3) - (4)].identifier))
    raptor_free_identifier((yyvsp[(3) - (4)].identifier));

  (yyval.sequence)=(yyvsp[(1) - (4)].sequence);
}
    break;

  case 30:
#line 573 "./turtle_parser.y"
    {
  int i;
#if RAPTOR_DEBUG > 1  
  printf("propertyList 2\n verb=");
  raptor_identifier_print(stdout, (yyvsp[(1) - (2)].identifier));
  if((yyvsp[(2) - (2)].sequence)) {
    printf("\n objectList=");
    raptor_sequence_print((yyvsp[(2) - (2)].sequence), stdout);
    printf("\n");
  } else
    printf("\n and empty objectList\n");
#endif

  if((yyvsp[(1) - (2)].identifier) && (yyvsp[(2) - (2)].sequence)) {
    for(i=0; i<raptor_sequence_size((yyvsp[(2) - (2)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (2)].sequence), i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_identifier((yyvsp[(1) - (2)].identifier));
        raptor_free_sequence((yyvsp[(2) - (2)].sequence));
        YYERROR;
      }
      if(raptor_copy_identifier(i2, (yyvsp[(1) - (2)].identifier))) {
        raptor_free_identifier((yyvsp[(1) - (2)].identifier));
        raptor_free_sequence((yyvsp[(2) - (2)].sequence));
        YYERROR;
      }
      t2->predicate=i2;
      t2->predicate->is_malloced=1;
    }

#if RAPTOR_DEBUG > 1  
    printf(" after substitution objectList=");
    raptor_sequence_print((yyvsp[(2) - (2)].sequence), stdout);
    printf("\n\n");
#endif
  }

  if((yyvsp[(1) - (2)].identifier))
    raptor_free_identifier((yyvsp[(1) - (2)].identifier));

  (yyval.sequence)=(yyvsp[(2) - (2)].sequence);
}
    break;

  case 31:
#line 617 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("propertyList 4\n empty returning NULL\n\n");
#endif
  (yyval.sequence)=NULL;
}
    break;

  case 32:
#line 624 "./turtle_parser.y"
    {
  (yyval.sequence)=(yyvsp[(1) - (2)].sequence);
#if RAPTOR_DEBUG > 1  
  printf("propertyList 5\n trailing semicolon returning existing list ");
  raptor_sequence_print((yyval.sequence), stdout);
  printf("\n\n");
#endif
}
    break;

  case 35:
#line 638 "./turtle_parser.y"
    {
  unsigned char *prefix=(yyvsp[(2) - (4)].string);
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);
  raptor_namespace *ns;

#if 0
  Get around bison complaining about not using (yyvsp[(1) - (4)].string)
#endif

#if RAPTOR_DEBUG > 1  
  printf("directive @prefix %s %s\n",((yyvsp[(2) - (4)].string) ? (char*)(yyvsp[(2) - (4)].string) : "(default)"),raptor_uri_as_string((yyvsp[(3) - (4)].uri)));
#endif

  if(prefix) {
    size_t len=strlen((const char*)prefix);
    if(prefix[len-1] == ':') {
      if(len == 1)
         /* declaring default namespace prefix @prefix : ... */
        prefix=NULL;
      else
        prefix[len-1]='\0';
    }
  }

  ns=raptor_new_namespace_from_uri(&turtle_parser->namespaces, prefix, (yyvsp[(3) - (4)].uri), 0);
  if(ns) {
    raptor_namespaces_start_namespace(&turtle_parser->namespaces, ns);
    raptor_parser_start_namespace((raptor_parser*)rdf_parser, ns);
  }

  if((yyvsp[(2) - (4)].string))
    RAPTOR_FREE(cstring, (yyvsp[(2) - (4)].string));
  raptor_free_uri((yyvsp[(3) - (4)].uri));

  if(!ns)
    YYERROR;
}
    break;

  case 36:
#line 679 "./turtle_parser.y"
    {
  raptor_uri *uri=(yyvsp[(2) - (3)].uri);
  /*raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);*/
  raptor_parser* parser=(raptor_parser*)rdf_parser;
  if(parser->base_uri)
    raptor_free_uri(parser->base_uri);
  parser->base_uri=uri;
}
    break;

  case 37:
#line 690 "./turtle_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 38:
#line 694 "./turtle_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 39:
#line 701 "./turtle_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 40:
#line 708 "./turtle_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 41:
#line 712 "./turtle_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 42:
#line 716 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("object literal=");
  raptor_identifier_print(stdout, (yyvsp[(1) - (1)].identifier));
  printf("\n");
#endif

  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 43:
#line 729 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal + language string=\"%s\"\n", (yyvsp[(1) - (3)].string));
#endif

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (3)].string), NULL, (yyvsp[(3) - (3)].string));
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 44:
#line 739 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal + language=\"%s\" datatype string=\"%s\" uri=\"%s\"\n", (yyvsp[(1) - (5)].string), (yyvsp[(3) - (5)].string), raptor_uri_as_string((yyvsp[(5) - (5)].uri)));
#endif

  if((yyvsp[(5) - (5)].uri)) {
    (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (5)].string), (yyvsp[(5) - (5)].uri), (yyvsp[(3) - (5)].string));
    if(!(yyval.identifier))
      YYERROR;
  } else
    (yyval.identifier)=NULL;
    
}
    break;

  case 45:
#line 753 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal + language=\"%s\" datatype string=\"%s\" qname URI=<%s>\n", (yyvsp[(1) - (5)].string), (yyvsp[(3) - (5)].string), raptor_uri_as_string((yyvsp[(5) - (5)].uri)));
#endif

  if((yyvsp[(5) - (5)].uri)) {
    (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (const unsigned char*)(yyvsp[(1) - (5)].string), (yyvsp[(5) - (5)].uri), (yyvsp[(3) - (5)].string));
    if(!(yyval.identifier))
      YYERROR;
  } else
    (yyval.identifier)=NULL;

}
    break;

  case 46:
#line 767 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal + datatype string=\"%s\" uri=\"%s\"\n", (yyvsp[(1) - (3)].string), raptor_uri_as_string((yyvsp[(3) - (3)].uri)));
#endif

  if((yyvsp[(3) - (3)].uri)) {
    (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (3)].string), (yyvsp[(3) - (3)].uri), NULL);
    if(!(yyval.identifier))
      YYERROR;
  } else
    (yyval.identifier)=NULL;
    
}
    break;

  case 47:
#line 781 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal + datatype string=\"%s\" qname URI=<%s>\n", (yyvsp[(1) - (3)].string), raptor_uri_as_string((yyvsp[(3) - (3)].uri)));
#endif

  if((yyvsp[(3) - (3)].uri)) {
    (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (3)].string), (yyvsp[(3) - (3)].uri), NULL);
    if(!(yyval.identifier))
      YYERROR;
  } else
    (yyval.identifier)=NULL;
}
    break;

  case 48:
#line 794 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal string=\"%s\"\n", (yyvsp[(1) - (1)].string));
#endif

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (1)].string), NULL, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 49:
#line 804 "./turtle_parser.y"
    {
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource integer=%s\n", (yyvsp[(1) - (1)].string));
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#integer");
  if(!uri) {
    RAPTOR_FREE(cstring, (yyvsp[(1) - (1)].string));
    YYERROR;
  }
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (1)].string), uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 50:
#line 819 "./turtle_parser.y"
    {
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource double=%s\n", (yyvsp[(1) - (1)].string));
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#double");
  if(!uri) {
    RAPTOR_FREE(cstring, (yyvsp[(1) - (1)].string));
    YYERROR;
  }
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (1)].string), uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 51:
#line 834 "./turtle_parser.y"
    {
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource decimal=%s\n", (yyvsp[(1) - (1)].string));
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#decimal");
  if(!uri) {
    RAPTOR_FREE(cstring, (yyvsp[(1) - (1)].string));
    YYERROR;
  }
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (1)].string), uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 52:
#line 849 "./turtle_parser.y"
    {
  unsigned char *string;
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  fputs("resource boolean true\n", stderr);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#boolean");
  if(!uri)
    YYERROR;
  string=(unsigned char*)RAPTOR_MALLOC(cstring, 5);
  if(!string) {
    raptor_free_uri(uri);
    YYERROR;
  }
  strncpy((char*)string, "true", 5);
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, string, uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 53:
#line 869 "./turtle_parser.y"
    {
  unsigned char *string;
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  fputs("resource boolean false\n", stderr);
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#boolean");
  if(!uri)
    YYERROR;
  string=(unsigned char*)RAPTOR_MALLOC(cstring, 6);
  if(!string) {
    raptor_free_uri(uri);
    YYERROR;
  }
  strncpy((char*)string, "false", 6);
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, string, uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 54:
#line 892 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("resource URI=<%s>\n", raptor_uri_as_string((yyvsp[(1) - (1)].uri)));
#endif

  if((yyvsp[(1) - (1)].uri)) {
    (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, (yyvsp[(1) - (1)].uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
    if(!(yyval.identifier))
      YYERROR;
  } else
    (yyval.identifier)=NULL;
}
    break;

  case 55:
#line 905 "./turtle_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("resource qname URI=<%s>\n", raptor_uri_as_string((yyvsp[(1) - (1)].uri)));
#endif

  if((yyvsp[(1) - (1)].uri)) {
    (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, (yyvsp[(1) - (1)].uri), RAPTOR_URI_SOURCE_ELEMENT, NULL, NULL, NULL, NULL);
    if(!(yyval.identifier))
      YYERROR;
  } else
    (yyval.identifier)=NULL;
}
    break;

  case 56:
#line 921 "./turtle_parser.y"
    {
  const unsigned char *id;
#if RAPTOR_DEBUG > 1  
  printf("subject blank=\"%s\"\n", (yyvsp[(1) - (1)].string));
#endif
  id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, (yyvsp[(1) - (1)].string));
  if(!id)
    YYERROR;

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_BLANK_ID, id, NULL, NULL, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 57:
#line 935 "./turtle_parser.y"
    {
  int i;
  const unsigned char *id;

  id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
  if(!id) {
    if((yyvsp[(2) - (3)].sequence))
      raptor_free_sequence((yyvsp[(2) - (3)].sequence));
    YYERROR;
  }

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_GENERATED, id, NULL, NULL, NULL);
  if(!(yyval.identifier)) {
    if((yyvsp[(2) - (3)].sequence))
      raptor_free_sequence((yyvsp[(2) - (3)].sequence));
    YYERROR;
  }

  if((yyvsp[(2) - (3)].sequence) == NULL) {
#if RAPTOR_DEBUG > 1  
    printf("resource\n propertyList=");
    raptor_identifier_print(stdout, (yyval.identifier));
    printf("\n");
#endif
  } else {
    /* non-empty property list, handle it  */
#if RAPTOR_DEBUG > 1  
    printf("resource\n propertyList=");
    raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
    printf("\n");
#endif

    for(i=0; i<raptor_sequence_size((yyvsp[(2) - (3)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (3)].sequence), i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_sequence((yyvsp[(2) - (3)].sequence));
        raptor_free_identifier((yyval.identifier));
        (yyval.identifier)=NULL;
        YYERROR;
      }
      if(raptor_copy_identifier(i2, (yyval.identifier))) {
        RAPTOR_FREE(raptor_identifier, i2);
        raptor_free_sequence((yyvsp[(2) - (3)].sequence));
        raptor_free_identifier((yyval.identifier));
        (yyval.identifier)=NULL;
        YYERROR;
      }
      t2->subject=i2;
      t2->subject->is_malloced=1;
      raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);
    }

#if RAPTOR_DEBUG > 1
    printf(" after substitution objectList=");
    raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
    printf("\n\n");
#endif

    raptor_free_sequence((yyvsp[(2) - (3)].sequence));

  }
  
}
    break;

  case 58:
#line 1000 "./turtle_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 59:
#line 1007 "./turtle_parser.y"
    {
  int i;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);
  raptor_identifier* first_identifier=NULL;
  raptor_identifier* rest_identifier=NULL;
  raptor_identifier* object=NULL;
  raptor_identifier* blank=NULL;

#if RAPTOR_DEBUG > 1  
  printf("collection\n objectList=");
  raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
  printf("\n");
#endif

  first_identifier=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->first_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!first_identifier)
    goto err_collection;
  rest_identifier=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->rest_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!rest_identifier)
    goto err_collection;
  
  /* non-empty property list, handle it  */
#if RAPTOR_DEBUG > 1  
  printf("resource\n propertyList=");
  raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
  printf("\n");
#endif

  object=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->nil_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!object)
    goto err_collection;

  for(i=raptor_sequence_size((yyvsp[(2) - (3)].sequence))-1; i>=0; i--) {
    raptor_identifier* temp;
    raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (3)].sequence), i);
    const unsigned char *blank_id;

    blank_id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
    if(!blank_id)
      goto err_collection;

    blank=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_GENERATED, blank_id, NULL, NULL, NULL);
    if(!blank)
      goto err_collection;
    
    t2->subject=blank;
    t2->predicate=first_identifier;
    /* t2->object already set to the value we want */
    raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);
    
    temp=t2->object;
    
    t2->subject=blank;
    t2->predicate=rest_identifier;
    t2->object=object;
    raptor_turtle_generate_statement((raptor_parser*)rdf_parser, t2);

    t2->subject=NULL;
    t2->predicate=NULL;
    t2->object=temp;

    raptor_free_identifier(object);
    object=blank;
    blank=NULL;
  }
  
#if RAPTOR_DEBUG > 1
  printf(" after substitution objectList=");
  raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
  printf("\n\n");
#endif

  raptor_free_sequence((yyvsp[(2) - (3)].sequence));

  raptor_free_identifier(first_identifier);
  raptor_free_identifier(rest_identifier);

  (yyval.identifier)=object;

  break; /* success */

  err_collection:

  if(blank)
    raptor_free_identifier(blank);

  if(object)
    raptor_free_identifier(object);

  if(rest_identifier)
    raptor_free_identifier(rest_identifier);

  if(first_identifier)
    raptor_free_identifier(first_identifier);

  raptor_free_sequence((yyvsp[(2) - (3)].sequence));

  YYERROR;
}
    break;

  case 60:
#line 1107 "./turtle_parser.y"
    {
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)(((raptor_parser*)rdf_parser)->context);

#if RAPTOR_DEBUG > 1  
  printf("collection\n empty\n");
#endif

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(turtle_parser->nil_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;


/* Line 1267 of yacc.c.  */
#line 2632 "turtle_parser.c"
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


#line 1121 "./turtle_parser.y"



/* Support functions */

/* This is declared in turtle_lexer.h but never used, so we always get
 * a warning unless this dummy code is here.  Used once below as a return.
 */
static int yy_init_globals (yyscan_t yyscanner ) { return 0; }


/* helper - everything passed in is now owned by triple */
static raptor_triple*
raptor_turtle_new_triple(raptor_identifier *subject,
                         raptor_identifier *predicate,
                         raptor_identifier *object) 
{
  raptor_triple* t;
  
  t=(raptor_triple*)RAPTOR_MALLOC(raptor_triple, sizeof(raptor_triple));
  if(!t) {
    if(subject)
      raptor_free_identifier(subject);
    if(predicate)
      raptor_free_identifier(predicate);
    if(object)
      raptor_free_identifier(object);
    return NULL;
  }
  
  t->subject=subject;
  t->predicate=predicate;
  t->object=object;

  return t;
}

static void
raptor_turtle_free_triple(raptor_triple *t) {
  if(t->subject)
    raptor_free_identifier(t->subject);
  if(t->predicate)
    raptor_free_identifier(t->predicate);
  if(t->object)
    raptor_free_identifier(t->object);
  RAPTOR_FREE(raptor_triple, t);
}
 
#ifdef RAPTOR_DEBUG
static void
raptor_triple_print(raptor_triple *t, FILE *fh) 
{
  fputs("triple(", fh);
  raptor_identifier_print(fh, t->subject);
  fputs(", ", fh);
  raptor_identifier_print(fh, t->predicate);
  fputs(", ", fh);
  raptor_identifier_print(fh, t->object);
  fputc(')', fh);
}
#endif


int
turtle_parser_error(void* ctx, const char *msg)
{
  raptor_parser* rdf_parser=(raptor_parser *)ctx;
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  
  if(turtle_parser->error_count++)
    return 0;

  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  raptor_parser_simple_error(rdf_parser, "%s", msg);
  return yy_init_globals(NULL); /* 0 but a way to use yy_init_globals */
}


int
turtle_syntax_error(raptor_parser *rdf_parser, const char *message, ...)
{
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  va_list arguments;

  if(turtle_parser->error_count++)
    return 0;

  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  va_start(arguments, message);
  
  raptor_parser_error_varargs(((raptor_parser*)rdf_parser), message, arguments);

  va_end(arguments);

  return 0;
}


raptor_uri*
turtle_qname_to_uri(raptor_parser *rdf_parser, unsigned char *name, size_t name_len) 
{
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

  rdf_parser->locator.line=turtle_parser->lineno;
#ifdef RAPTOR_TURTLE_USE_ERROR_COLUMNS
  rdf_parser->locator.column=turtle_lexer_get_column(yyscanner);
#endif

  return raptor_qname_string_to_uri(&turtle_parser->namespaces,
                                    name, name_len,
                                    (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser);
}



static int
turtle_parse(raptor_parser *rdf_parser, const char *string) {
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  void *buffer;
  
  if(!string || !*string)
    return 0;
  
  if(turtle_lexer_lex_init(&turtle_parser->scanner))
    return 1;
  turtle_parser->scanner_set=1;

  turtle_lexer_set_extra(rdf_parser, turtle_parser->scanner);
  buffer= turtle_lexer__scan_string(string, turtle_parser->scanner);

  turtle_parser_parse(rdf_parser);

  turtle_lexer_lex_destroy(turtle_parser->scanner);
  turtle_parser->scanner_set=0;

  return 0;
}


/**
 * raptor_turtle_parse_init - Initialise the Raptor Turtle parser
 *
 * Return value: non 0 on failure
 **/

static int
raptor_turtle_parse_init(raptor_parser* rdf_parser, const char *name) {
  raptor_turtle_parser* turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_uri_get_handler(&uri_handler, &uri_context);

  if(raptor_namespaces_init(&turtle_parser->namespaces,
                            uri_handler, uri_context,
                            (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser, 
                            0))
    return 1;

  turtle_parser->nil_uri=raptor_new_uri_for_rdf_concept("nil");
  turtle_parser->first_uri=raptor_new_uri_for_rdf_concept("first");
  turtle_parser->rest_uri=raptor_new_uri_for_rdf_concept("rest");

  if(!turtle_parser->nil_uri || !turtle_parser->first_uri || !turtle_parser->rest_uri)
    return 1;

  turtle_parser->trig=!strcmp(name, "trig");

  return 0;
}


/* PUBLIC FUNCTIONS */


/*
 * raptor_turtle_parse_terminate - Free the Raptor Turtle parser
 * @rdf_parser: parser object
 * 
 **/
static void
raptor_turtle_parse_terminate(raptor_parser *rdf_parser) {
  raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

  if(turtle_parser->nil_uri)
    raptor_free_uri(turtle_parser->nil_uri);
  if(turtle_parser->first_uri)
    raptor_free_uri(turtle_parser->first_uri);
  if(turtle_parser->rest_uri)
    raptor_free_uri(turtle_parser->rest_uri);

  raptor_namespaces_clear(&turtle_parser->namespaces);

  if(turtle_parser->scanner_set) {
    turtle_lexer_lex_destroy(turtle_parser->scanner);
    turtle_parser->scanner_set=0;
  }

  if(turtle_parser->buffer)
    RAPTOR_FREE(cdata, turtle_parser->buffer);
}


static void
raptor_turtle_generate_statement(raptor_parser *parser, raptor_triple *t)
{
  /* raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)parser->context; */
  raptor_statement *statement=&parser->statement;

  if(!t->subject || !t->predicate || !t->object)
    return;

  /* Two choices for subject for Turtle */
  statement->subject_type=t->subject->type;
  if(t->subject->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
    statement->subject=t->subject->id;
  } else {
    /* RAPTOR_IDENTIFIER_TYPE_RESOURCE */
    RAPTOR_ASSERT(t->subject->type != RAPTOR_IDENTIFIER_TYPE_RESOURCE,
                  "subject type is not resource");
    statement->subject=t->subject->uri;
  }

  /* Predicates are URIs but check for bad ordinals */
  if(!strncmp((const char*)raptor_uri_as_string(t->predicate->uri),
              "http://www.w3.org/1999/02/22-rdf-syntax-ns#_", 44)) {
    unsigned char* predicate_uri_string=raptor_uri_as_string(t->predicate->uri);
    int predicate_ordinal=raptor_check_ordinal(predicate_uri_string+44);
    if(predicate_ordinal <= 0)
      raptor_parser_error(parser, "Illegal ordinal value %d in property '%s'.", predicate_ordinal, predicate_uri_string);
  }
  
  statement->predicate_type=RAPTOR_IDENTIFIER_TYPE_RESOURCE;
  statement->predicate=t->predicate->uri;
  

  /* Three choices for object for Turtle */
  statement->object_type=t->object->type;
  statement->object_literal_language=NULL;
  statement->object_literal_datatype=NULL;

  if(t->object->type == RAPTOR_IDENTIFIER_TYPE_RESOURCE) {
    statement->object=t->object->uri;
  } else if(t->object->type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
    statement->object=t->object->id;
  } else {
    /* RAPTOR_IDENTIFIER_TYPE_LITERAL */
    RAPTOR_ASSERT(t->object->type != RAPTOR_IDENTIFIER_TYPE_LITERAL,
                  "object type is not literal");
    statement->object=t->object->literal;
    statement->object_literal_language=t->object->literal_language;
    statement->object_literal_datatype=t->object->literal_datatype;

    if(statement->object_literal_datatype)
      statement->object_literal_language=NULL;
  }

  if(!parser->statement_handler)
    return;

  /* Generate the statement */
  (*parser->statement_handler)(parser->user_data, statement);
}



static int
raptor_turtle_parse_chunk(raptor_parser* rdf_parser, 
                      const unsigned char *s, size_t len,
                      int is_end)
{
  char *ptr;
  raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)rdf_parser->context;
  
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("adding %d bytes to line buffer\n", (int)len);
#endif

  if(len) {
    turtle_parser->buffer=(char*)RAPTOR_REALLOC(cstring, turtle_parser->buffer, turtle_parser->buffer_length + len + 1);
    if(!turtle_parser->buffer) {
      raptor_parser_fatal_error(rdf_parser, "Out of memory");
      return 1;
    }

    /* move pointer to end of cdata buffer */
    ptr=turtle_parser->buffer+turtle_parser->buffer_length;

    /* adjust stored length */
    turtle_parser->buffer_length += len;

    /* now write new stuff at end of cdata buffer */
    strncpy(ptr, (char*)s, len);
    ptr += len;
    *ptr = '\0';

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("buffer buffer now '%s' (%d bytes)\n", 
                  turtle_parser->buffer, turtle_parser->buffer_length);
#endif
  }
  
  /* if not end, wait for rest of input */
  if(!is_end)
    return 0;

  /* Nothing to do */
  if(!turtle_parser->buffer_length)
    return 0;
  
  turtle_parse(rdf_parser, turtle_parser->buffer);
  
  return 0;
}


static int
raptor_turtle_parse_start(raptor_parser *rdf_parser) 
{
  raptor_locator *locator=&rdf_parser->locator;
  raptor_turtle_parser *turtle_parser=(raptor_turtle_parser*)rdf_parser->context;

  /* base URI required for Turtle */
  if(!rdf_parser->base_uri)
    return 1;

  locator->line=1;
  locator->column= -1; /* No column info */
  locator->byte= -1; /* No bytes info */

  if(turtle_parser->buffer_length) {
    RAPTOR_FREE(cdata, turtle_parser->buffer);
    turtle_parser->buffer=NULL;
    turtle_parser->buffer_length=0;
  }
  
  turtle_parser->lineno=1;

  return 0;
}


static int
raptor_turtle_parse_recognise_syntax(raptor_parser_factory* factory, 
                                     const unsigned char *buffer, size_t len,
                                     const unsigned char *identifier, 
                                     const unsigned char *suffix, 
                                     const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "ttl"))
      score=8;
    if(!strcmp((const char*)suffix, "n3"))
      score=3;
  }
  
  if(mime_type) {
    if(strstr((const char*)mime_type, "turtle"))
      score+=6;
#ifndef RAPTOR_PARSER_N3
    if(strstr((const char*)mime_type, "n3"))
      score+=3;
#endif
  }

  /* FIXME: Should do this as long as N3 is not also present since
   * shares the same syntax */
  if(buffer && len) {
#define  HAS_TURTLE_PREFIX (strstr((const char*)buffer, "@prefix ") != NULL)
/* The following could also be found with N-Triples but not with @prefix */
#define  HAS_TURTLE_RDF_URI (strstr((const char*)buffer, ": <http://www.w3.org/1999/02/22-rdf-syntax-ns#>") != NULL)

    if(HAS_TURTLE_PREFIX) {
      score=6;
      if(HAS_TURTLE_RDF_URI)
        score+=2;
    }
  }
  
  return score;
}


#ifdef RAPTOR_PARSER_TRIG
static int
raptor_trig_parse_recognise_syntax(raptor_parser_factory* factory, 
                                   const unsigned char *buffer, size_t len,
                                   const unsigned char *identifier, 
                                   const unsigned char *suffix, 
                                   const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "trig"))
      score=9;
#ifndef RAPTOR_PARSER_TURTLE
    if(!strcmp((const char*)suffix, "ttl"))
      score=8;
#endif
    if(!strcmp((const char*)suffix, "n3"))
      score=3;
  }
  
  if(mime_type) {
#ifndef RAPTOR_PARSER_TURTLE
    if(strstr((const char*)mime_type, "turtle"))
      score+=6;
#endif
#ifndef RAPTOR_PARSER_N3
    if(strstr((const char*)mime_type, "n3"))
      score+=3;
#endif
  }

#ifndef RAPTOR_PARSER_TURTLE
  /* FIXME: Should do this as long as N3 is not also present since
   * shares the same syntax */
  if(buffer && len) {
#define  HAS_TRIG_PREFIX (strstr((const char*)buffer, "@prefix ") != NULL)
/* The following could also be found with N-Triples but not with @prefix */
#define  HAS_TRIG_RDF_URI (strstr((const char*)buffer, ": <http://www.w3.org/1999/02/22-rdf-syntax-ns#>") != NULL)

    if(HAS_TRIG_PREFIX) {
      score=6;
      if(HAS_TRIG_RDF_URI)
        score+=2;
    }
  }
#endif
  
  return score;
}
#endif


#ifdef RAPTOR_PARSER_TURTLE
static int
raptor_turtle_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_turtle_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_turtle_parse_init;
  factory->terminate = raptor_turtle_parse_terminate;
  factory->start     = raptor_turtle_parse_start;
  factory->chunk     = raptor_turtle_parse_chunk;
  factory->recognise_syntax = raptor_turtle_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_alias(factory, "ntriples-plus") != 0;

  rc+= raptor_parser_factory_add_uri(factory, (const unsigned char*)"http://www.dajobe.org/2004/01/turtle/") != 0;

  /* first one is the default */
  rc+= raptor_parser_factory_add_mime_type(factory, "application/x-turtle", 10) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/turtle", 10) != 0;

#ifndef RAPTOR_PARSER_N3
  rc+= raptor_parser_factory_add_mime_type(factory, "text/n3", 3) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "text/rdf+n3", 3) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/rdf+n3", 3) != 0;
#endif

  return rc;
}
#endif


#ifdef RAPTOR_PARSER_TRIG
static int
raptor_trig_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_turtle_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_turtle_parse_init;
  factory->terminate = raptor_turtle_parse_terminate;
  factory->start     = raptor_turtle_parse_start;
  factory->chunk     = raptor_turtle_parse_chunk;
  factory->recognise_syntax = raptor_trig_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_uri(factory, (const unsigned char*)"http://www.wiwiss.fu-berlin.de/suhl/bizer/TriG/Spec/") != 0;

  rc+= raptor_parser_factory_add_mime_type(factory, "application/x-trig", 10) != 0;

  return rc;
}
#endif


#ifdef RAPTOR_PARSER_TURTLE
int
raptor_init_parser_turtle(void)
{
  return !raptor_parser_register_factory("turtle", "Turtle Terse RDF Triple Language",
                                         &raptor_turtle_parser_register_factory);
}
#endif

#ifdef RAPTOR_PARSER_TRIG
int
raptor_init_parser_trig(void)
{
  return !raptor_parser_register_factory("trig", "TriG - Turtle with Named Graphs",
                                         &raptor_trig_parser_register_factory);
}
#endif


#ifdef STANDALONE
#include <stdio.h>
#include <locale.h>

#define TURTLE_FILE_BUF_SIZE 2048

static
void turtle_parser_print_statement(void *user, const raptor_statement *statement) 
{
  FILE* stream=(FILE*)user;
  raptor_print_statement(statement, stream);
  putc('\n', stream);
}
  


int
main(int argc, char *argv[]) 
{
  char string[TURTLE_FILE_BUF_SIZE];
  raptor_parser rdf_parser; /* static */
  raptor_turtle_parser turtle_parser; /* static */
  raptor_locator *locator=&rdf_parser.locator;
  FILE *fh;
  char *filename;
  int rc;
  
#if RAPTOR_DEBUG > 2
  turtle_parser_debug=1;
#endif

  if(argc > 1) {
    filename=argv[1];
    fh = fopen(filename, "r");
    if(!fh) {
      fprintf(stderr, "%s: Cannot open file %s - %s\n", argv[0], filename,
              strerror(errno));
      exit(1);
    }
  } else {
    filename="<stdin>";
    fh = stdin;
  }

  memset(string, 0, TURTLE_FILE_BUF_SIZE);
  rc=fread(string, TURTLE_FILE_BUF_SIZE, 1, fh);
  if(rc < TURTLE_FILE_BUF_SIZE) {
    if(ferror(fh)) {
      fprintf(stderr, "%s: file '%s' read failed - %s\n",
              argv[0], filename, strerror(errno));
      fclose(fh);
      return(1);
    }
  }
  
  if(argc>1)
    fclose(fh);

  raptor_uri_init();

  memset(&rdf_parser, 0, sizeof(raptor_parser));
  memset(&turtle_parser, 0, sizeof(raptor_turtle_parser));

  locator->line= locator->column = -1;
  locator->file= filename;

  turtle_parser.lineno= 1;

  rdf_parser.context=&turtle_parser;
  rdf_parser.base_uri=raptor_new_uri((const unsigned char*)"http://example.org/fake-base-uri/");

  raptor_set_statement_handler(&rdf_parser, stdout, turtle_parser_print_statement);
  raptor_turtle_parse_init(&rdf_parser, "turtle");
  
  turtle_parser.error_count=0;

  turtle_parse(&rdf_parser, string);

  raptor_free_uri(rdf_parser.base_uri);

  return (0);
}
#endif

