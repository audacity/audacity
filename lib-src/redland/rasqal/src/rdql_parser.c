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
#define yyparse rdql_parser_parse
#define yylex   rdql_parser_lex
#define yyerror rdql_parser_error
#define yylval  rdql_parser_lval
#define yychar  rdql_parser_char
#define yydebug rdql_parser_debug
#define yynerrs rdql_parser_nerrs


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




/* Copy the first part of user declarations.  */
#line 25 "./rdql_parser.y"

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

#include <rdql_parser.h>

#define YY_DECL int rdql_lexer_lex (YYSTYPE *rdql_parser_lval, yyscan_t yyscanner)
#define YY_NO_UNISTD_H 1
#include <rdql_lexer.h>

#include <rdql_common.h>


/* Make verbose error messages for syntax errors */
/*
#ifdef RASQAL_DEBUG
#define YYERROR_VERBOSE 1
#endif
*/
#define YYERROR_VERBOSE 1

/* Slow down the grammar operation and watch it work */
#if RASQAL_DEBUG > 2
#define YYDEBUG 1
#endif

/* the lexer does not seem to track this */
#undef RASQAL_RDQL_USE_ERROR_COLUMNS

/* Missing rdql_lexer.c/h prototypes */
int rdql_lexer_get_column(yyscan_t yyscanner);
/* Not used here */
/* void rdql_lexer_set_column(int  column_no , yyscan_t yyscanner);*/


/* What the lexer wants */
extern int rdql_lexer_lex (YYSTYPE *rdql_parser_lval, yyscan_t scanner);
#define YYLEX_PARAM ((rasqal_rdql_query_engine*)(((rasqal_query*)rq)->context))->scanner

/* Pure parser argument (a void*) */
#define YYPARSE_PARAM rq

/* Make the yyerror below use the rdf_parser */
#undef yyerror
#define yyerror(message) rdql_query_error((rasqal_query*)rq, message)

/* Make lex/yacc interface as small as possible */
#undef yylex
#define yylex rdql_lexer_lex


static int rdql_parse(rasqal_query* rq);
static void rdql_query_error(rasqal_query* rq, const char *message);



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
/* Line 187 of yacc.c.  */
#line 242 "rdql_parser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 255 "rdql_parser.c"

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
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   141

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  54
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  28
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNRULES -- Number of states.  */
#define YYNSTATES  129

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   297

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    44,     2,     2,     2,    42,     2,     2,
      10,    11,    40,    38,     9,    39,     2,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    12,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    43,     2,     2,     2,
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
       5,     6,     7,     8,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    45,
      46,    47,    48,    49,    50,    51,    52,    53
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,    13,    17,    20,    22,    24,    26,
      29,    32,    33,    37,    40,    42,    50,    57,    64,    70,
      73,    74,    78,    82,    84,    87,    88,    94,    99,   103,
     107,   109,   113,   115,   119,   123,   127,   131,   133,   137,
     141,   143,   147,   151,   155,   159,   161,   163,   167,   171,
     173,   177,   181,   185,   187,   190,   193,   195,   198,   201,
     203,   205,   209,   211,   213,   215,   217,   219,   222,   224,
     226,   228,   230,   232,   234,   236,   238,   242
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      55,     0,    -1,    56,    -1,     3,    58,    59,     6,    60,
      62,    64,    -1,    57,     9,    78,    -1,    57,    78,    -1,
      78,    -1,    57,    -1,    40,    -1,     4,    81,    -1,     5,
      81,    -1,    -1,    60,     9,    61,    -1,    60,    61,    -1,
      61,    -1,    10,    76,     9,    76,     9,    77,    11,    -1,
      10,    76,    76,     9,    77,    11,    -1,    10,    76,     9,
      76,    77,    11,    -1,    10,    76,    76,    77,    11,    -1,
       7,    63,    -1,    -1,    63,     9,    66,    -1,    63,     7,
      66,    -1,    66,    -1,    13,    65,    -1,    -1,    53,     8,
      51,     9,    65,    -1,    53,     8,    51,    65,    -1,    53,
       8,    51,    -1,    67,    15,    66,    -1,    67,    -1,    68,
      17,    67,    -1,    68,    -1,    69,    19,    69,    -1,    69,
      21,    69,    -1,    69,    23,    79,    -1,    69,    25,    79,
      -1,    69,    -1,    70,    27,    70,    -1,    70,    29,    70,
      -1,    70,    -1,    71,    31,    71,    -1,    71,    33,    71,
      -1,    71,    35,    71,    -1,    71,    37,    71,    -1,    71,
      -1,    72,    -1,    73,    38,    72,    -1,    73,    39,    72,
      -1,    73,    -1,    74,    40,    73,    -1,    74,    41,    73,
      -1,    74,    42,    73,    -1,    74,    -1,    38,    74,    -1,
      39,    74,    -1,    75,    -1,    43,    74,    -1,    44,    74,
      -1,    78,    -1,    80,    -1,    10,    66,    11,    -1,    78,
      -1,    51,    -1,    52,    -1,    78,    -1,    80,    -1,    12,
      53,    -1,    48,    -1,    51,    -1,    47,    -1,    45,    -1,
      46,    -1,    49,    -1,    50,    -1,    52,    -1,    51,     9,
      81,    -1,    51,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   186,   186,   190,   213,   218,   223,   232,   236,   243,
     247,   252,   261,   266,   271,   284,   288,   292,   296,   307,
     312,   317,   322,   327,   336,   341,   346,   351,   356,   364,
     368,   374,   379,   385,   389,   393,   397,   401,   407,   411,
     415,   421,   425,   429,   433,   437,   443,   450,   454,   458,
     464,   468,   472,   476,   482,   486,   490,   496,   500,   504,
     509,   513,   519,   523,   527,   533,   537,   543,   549,   555,
     559,   563,   567,   571,   575,   578,   585,   590
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SELECT", "SOURCE", "FROM", "WHERE",
  "AND", "FOR", "','", "'('", "')'", "'?'", "USING", "\"||\"", "SC_OR",
  "\"&&\"", "SC_AND", "\"eq\"", "STR_EQ", "\"ne\"", "STR_NE", "\"=~\"",
  "STR_MATCH", "\"!~\"", "STR_NMATCH", "\"=\"", "EQ", "\"!=\"", "NEQ",
  "\"<\"", "LT", "\">\"", "GT", "\"<=\"", "LE", "\">=\"", "GE", "'+'",
  "'-'", "'*'", "'/'", "'%'", "'~'", "'!'", "\"floating point literal\"",
  "\"string literal\"", "\"integer literal\"", "\"pattern literal\"",
  "\"boolean literal\"", "\"null\"", "\"URI literal\"",
  "\"QName literal\"", "\"identifier\"", "$accept", "Document", "Query",
  "VarList", "SelectClause", "SourceClause", "TriplePatternList",
  "TriplePattern", "ConstraintClause", "CommaAndConstraintClause",
  "UsingClause", "PrefixDeclList", "Expression",
  "ConditionalAndExpression", "ValueLogical", "EqualityExpression",
  "RelationalExpression", "NumericExpression", "AdditiveExpression",
  "MultiplicativeExpression", "UnaryExpression",
  "UnaryExpressionNotPlusMinus", "VarOrURI", "VarOrLiteral", "Var",
  "PatternLiteral", "Literal", "URIList", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,    44,
      40,    41,    63,   264,   265,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,   284,   285,   286,   287,   288,    43,    45,
      42,    47,    37,   126,    33,   289,   290,   291,   292,   293,
     294,   295,   296,   297
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    54,    55,    56,    57,    57,    57,    58,    58,    59,
      59,    59,    60,    60,    60,    61,    61,    61,    61,    62,
      62,    63,    63,    63,    64,    64,    65,    65,    65,    66,
      66,    67,    67,    68,    68,    68,    68,    68,    69,    69,
      69,    70,    70,    70,    70,    70,    71,    72,    72,    72,
      73,    73,    73,    73,    74,    74,    74,    75,    75,    75,
      75,    75,    76,    76,    76,    77,    77,    78,    79,    80,
      80,    80,    80,    80,    80,    80,    81,    81
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     7,     3,     2,     1,     1,     1,     2,
       2,     0,     3,     2,     1,     7,     6,     6,     5,     2,
       0,     3,     3,     1,     2,     0,     5,     4,     3,     3,
       1,     3,     1,     3,     3,     3,     3,     1,     3,     3,
       1,     3,     3,     3,     3,     1,     1,     3,     3,     1,
       3,     3,     3,     1,     2,     2,     1,     2,     2,     1,
       1,     3,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     2,     0,     8,     7,    11,     6,     1,
      67,     0,     5,     0,     0,     0,     4,    77,     9,    10,
       0,     0,     0,    20,    14,    76,    63,    64,     0,    62,
       0,     0,    13,    25,     0,     0,     0,     0,     0,     0,
       0,    71,    72,    70,    73,    74,    69,    75,    19,    23,
      30,    32,    37,    40,    45,    46,    49,    53,    56,    59,
      60,    12,     0,     3,     0,     0,     0,    65,    66,     0,
      54,    55,    57,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    24,     0,     0,     0,    18,    61,
      22,    21,    29,    31,    33,    34,    68,    35,    36,    38,
      39,    41,    42,    43,    44,    47,    48,    50,    51,    52,
       0,     0,    17,    16,    28,    15,     0,    27,    26
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,     3,     6,     7,    15,    23,    24,    33,    48,
      63,    94,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    28,    66,    59,   107,    60,    18
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -35
static const yytype_int16 yypact[] =
{
      15,    -5,    25,   -35,   -25,   -35,    -3,    40,   -35,   -35,
     -35,    38,   -35,    17,    17,    81,   -35,    84,   -35,   -35,
     109,    17,    27,    67,   -35,   -35,   -35,   -35,     4,   -35,
      63,   109,   -35,   108,    27,     2,    63,    63,    63,    63,
      63,   -35,   -35,   -35,   -35,   -35,   -35,   -35,    53,   -35,
     114,   113,     1,    56,    51,   -35,    61,    76,   -35,   -35,
     -35,   -35,    78,   -35,    20,    -9,   121,   -35,   -35,   122,
     -35,   -35,   -35,   -35,    63,    63,    63,    63,    63,    63,
      86,    86,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,   127,   -35,    -9,   125,   126,   -35,   -35,
     -35,   -35,   -35,   -35,   -35,   -35,   -35,   -35,   -35,   -35,
     -35,   -35,   -35,   -35,   -35,   -35,   -35,   -35,   -35,   -35,
      87,   128,   -35,   -35,    -7,   -35,    78,   -35,   -35
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -35,   -35,   -35,   -35,   -35,   -35,   -35,   -19,   -35,   -35,
     -35,    -4,   -17,    64,   -35,    45,    43,     5,    39,    13,
      58,   -35,   -11,    16,    -1,    59,   -34,    -6
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
       8,    68,   126,     4,    32,    12,    11,     4,    19,     4,
      16,    65,    61,    34,     4,    25,     4,    35,     1,    69,
      78,    29,    79,    64,    80,     9,    81,    29,    10,    95,
      68,    68,     4,    29,    67,     5,    41,    42,    43,     4,
      44,    45,    46,    47,    13,    14,    93,    41,    42,    43,
       4,    44,    45,    46,    47,    26,    27,   100,   101,   102,
      74,    68,    75,    67,    67,    41,    42,    43,    17,    44,
      45,    46,    47,    36,    30,     4,    31,    22,    26,    27,
      96,    97,    84,    82,    85,    83,    86,    20,    87,   111,
     112,   113,   114,    21,    67,    70,    71,    72,    73,    88,
      89,    37,    38,   117,   118,   119,    39,    40,    41,    42,
      43,   121,    44,    45,    46,    47,    90,    91,    92,    22,
     127,    62,   128,   104,   105,   109,   110,   115,   116,    76,
      77,    93,    98,    99,   106,   120,   122,   123,   124,   125,
     108,   103
};

static const yytype_uint8 yycheck[] =
{
       1,    35,     9,    12,    23,     6,     9,    12,    14,    12,
      11,     9,    31,     9,    12,    21,    12,    28,     3,    36,
      19,    22,    21,    34,    23,     0,    25,    28,    53,     9,
      64,    65,    12,    34,    35,    40,    45,    46,    47,    12,
      49,    50,    51,    52,     4,     5,    53,    45,    46,    47,
      12,    49,    50,    51,    52,    51,    52,    74,    75,    76,
       7,    95,     9,    64,    65,    45,    46,    47,    51,    49,
      50,    51,    52,    10,     7,    12,     9,    10,    51,    52,
      64,    65,    31,    27,    33,    29,    35,     6,    37,    84,
      85,    86,    87,     9,    95,    37,    38,    39,    40,    38,
      39,    38,    39,    90,    91,    92,    43,    44,    45,    46,
      47,    95,    49,    50,    51,    52,    40,    41,    42,    10,
     124,    13,   126,    78,    79,    82,    83,    88,    89,    15,
      17,    53,    11,    11,    48,     8,    11,    11,    51,    11,
      81,    77
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,    55,    56,    12,    40,    57,    58,    78,     0,
      53,     9,    78,     4,     5,    59,    78,    51,    81,    81,
       6,     9,    10,    60,    61,    81,    51,    52,    76,    78,
       7,     9,    61,    62,     9,    76,    10,    38,    39,    43,
      44,    45,    46,    47,    49,    50,    51,    52,    63,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    78,
      80,    61,    13,    64,    76,     9,    77,    78,    80,    66,
      74,    74,    74,    74,     7,     9,    15,    17,    19,    21,
      23,    25,    27,    29,    31,    33,    35,    37,    38,    39,
      40,    41,    42,    53,    65,     9,    77,    77,    11,    11,
      66,    66,    66,    67,    69,    69,    48,    79,    79,    70,
      70,    71,    71,    71,    71,    72,    72,    73,    73,    73,
       8,    77,    11,    11,    51,    11,     9,    65,    65
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
      case 45: /* "\"floating point literal\"" */
#line 179 "./rdql_parser.y"
	{ rasqal_free_literal((yyvaluep->literal)); };
#line 1271 "rdql_parser.c"
	break;
      case 46: /* "\"string literal\"" */
#line 179 "./rdql_parser.y"
	{ rasqal_free_literal((yyvaluep->literal)); };
#line 1276 "rdql_parser.c"
	break;
      case 47: /* "\"integer literal\"" */
#line 179 "./rdql_parser.y"
	{ rasqal_free_literal((yyvaluep->literal)); };
#line 1281 "rdql_parser.c"
	break;
      case 48: /* "\"pattern literal\"" */
#line 179 "./rdql_parser.y"
	{ rasqal_free_literal((yyvaluep->literal)); };
#line 1286 "rdql_parser.c"
	break;
      case 49: /* "\"boolean literal\"" */
#line 179 "./rdql_parser.y"
	{ rasqal_free_literal((yyvaluep->literal)); };
#line 1291 "rdql_parser.c"
	break;
      case 50: /* "\"null\"" */
#line 179 "./rdql_parser.y"
	{ rasqal_free_literal((yyvaluep->literal)); };
#line 1296 "rdql_parser.c"
	break;
      case 51: /* "\"URI literal\"" */
#line 180 "./rdql_parser.y"
	{ raptor_free_uri((yyvaluep->uri)); };
#line 1301 "rdql_parser.c"
	break;
      case 52: /* "\"QName literal\"" */
#line 181 "./rdql_parser.y"
	{ RASQAL_FREE(cstring, (yyvaluep->name)); };
#line 1306 "rdql_parser.c"
	break;
      case 53: /* "\"identifier\"" */
#line 181 "./rdql_parser.y"
	{ RASQAL_FREE(cstring, (yyvaluep->name)); };
#line 1311 "rdql_parser.c"
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
#line 191 "./rdql_parser.y"
    {
  ((rasqal_query*)rq)->selects=(yyvsp[(2) - (7)].seq);
  ((rasqal_query*)rq)->verb=RASQAL_QUERY_VERB_SELECT;

  if((yyvsp[(3) - (7)].seq)) {
    int i;
    
    for(i=0; i < raptor_sequence_size((yyvsp[(3) - (7)].seq)); i++) {
      raptor_uri* uri=(raptor_uri*)raptor_sequence_get_at((yyvsp[(3) - (7)].seq), i);
      rasqal_query_add_data_graph((rasqal_query*)rq, uri, NULL, RASQAL_DATA_GRAPH_BACKGROUND);
    }
    raptor_free_sequence((yyvsp[(3) - (7)].seq));
  }

  /* ignoring $5 sequence, set in TriplePatternList to
   * ((rasqal_query*)rq)->triples=$5; 
   */

  /* ignoring $6 sequence, set in ConstraintClause */
}
    break;

  case 4:
#line 214 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (3)].seq);
  raptor_sequence_push((yyval.seq), (yyvsp[(3) - (3)].variable));
}
    break;

  case 5:
#line 219 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (2)].seq);
  raptor_sequence_push((yyval.seq), (yyvsp[(2) - (2)].variable));
}
    break;

  case 6:
#line 224 "./rdql_parser.y"
    {
  /* The variables are freed from the rasqal_query field variables */
  (yyval.seq)=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)rasqal_variable_print);
  raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].variable));
}
    break;

  case 7:
#line 233 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (1)].seq);
}
    break;

  case 8:
#line 237 "./rdql_parser.y"
    {
  (yyval.seq)=NULL;
  ((rasqal_query*)rq)->wildcard=1;
}
    break;

  case 9:
#line 244 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (2)].seq);
}
    break;

  case 10:
#line 248 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (2)].seq);
}
    break;

  case 11:
#line 252 "./rdql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 12:
#line 262 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (3)].seq);
  raptor_sequence_push((yyval.seq), (yyvsp[(3) - (3)].triple));
}
    break;

  case 13:
#line 267 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(1) - (2)].seq);
  raptor_sequence_push((yyval.seq), (yyvsp[(2) - (2)].triple));
}
    break;

  case 14:
#line 272 "./rdql_parser.y"
    {
  (yyval.seq)=((rasqal_query*)rq)->triples;
  raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].triple));
}
    break;

  case 15:
#line 285 "./rdql_parser.y"
    {
  (yyval.triple)=rasqal_new_triple((yyvsp[(2) - (7)].literal), (yyvsp[(4) - (7)].literal), (yyvsp[(6) - (7)].literal));
}
    break;

  case 16:
#line 289 "./rdql_parser.y"
    {
  (yyval.triple)=rasqal_new_triple((yyvsp[(2) - (6)].literal), (yyvsp[(3) - (6)].literal), (yyvsp[(5) - (6)].literal));
}
    break;

  case 17:
#line 293 "./rdql_parser.y"
    {
  (yyval.triple)=rasqal_new_triple((yyvsp[(2) - (6)].literal), (yyvsp[(4) - (6)].literal), (yyvsp[(5) - (6)].literal));
}
    break;

  case 18:
#line 297 "./rdql_parser.y"
    {
  (yyval.triple)=rasqal_new_triple((yyvsp[(2) - (5)].literal), (yyvsp[(3) - (5)].literal), (yyvsp[(4) - (5)].literal));
}
    break;

  case 19:
#line 308 "./rdql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 20:
#line 312 "./rdql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 21:
#line 318 "./rdql_parser.y"
    {
  raptor_sequence_push(((rasqal_query*)rq)->constraints_sequence, (yyvsp[(3) - (3)].expr));
  (yyval.seq)=NULL;
}
    break;

  case 22:
#line 323 "./rdql_parser.y"
    {
  raptor_sequence_push(((rasqal_query*)rq)->constraints_sequence, (yyvsp[(3) - (3)].expr));
  (yyval.seq)=NULL;
}
    break;

  case 23:
#line 328 "./rdql_parser.y"
    {
  raptor_sequence_push(((rasqal_query*)rq)->constraints_sequence, (yyvsp[(1) - (1)].expr));
  (yyval.seq)=NULL;
}
    break;

  case 24:
#line 337 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(2) - (2)].seq);
}
    break;

  case 25:
#line 341 "./rdql_parser.y"
    {
  (yyval.seq)=NULL;
}
    break;

  case 26:
#line 347 "./rdql_parser.y"
    {
  (yyval.seq)=((rasqal_query*)rq)->prefixes;
  raptor_sequence_shift((yyval.seq), rasqal_new_prefix((yyvsp[(1) - (5)].name), (yyvsp[(3) - (5)].uri)));
}
    break;

  case 27:
#line 352 "./rdql_parser.y"
    {
  (yyval.seq)=((rasqal_query*)rq)->prefixes;
  raptor_sequence_shift((yyval.seq), rasqal_new_prefix((yyvsp[(1) - (4)].name), (yyvsp[(3) - (4)].uri)));
}
    break;

  case 28:
#line 357 "./rdql_parser.y"
    {
  (yyval.seq)=((rasqal_query*)rq)->prefixes;
  raptor_sequence_push((yyval.seq), rasqal_new_prefix((yyvsp[(1) - (3)].name), (yyvsp[(3) - (3)].uri)));
}
    break;

  case 29:
#line 365 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_OR, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 30:
#line 369 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 31:
#line 375 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_AND, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
;
}
    break;

  case 32:
#line 380 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 33:
#line 386 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_STR_EQ, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 34:
#line 390 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_STR_NEQ, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 35:
#line 394 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_string_op_expression(RASQAL_EXPR_STR_MATCH, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].literal));
}
    break;

  case 36:
#line 398 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_string_op_expression(RASQAL_EXPR_STR_NMATCH, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].literal));
}
    break;

  case 37:
#line 402 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 38:
#line 408 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_EQ, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 39:
#line 412 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_NEQ, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 40:
#line 416 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 41:
#line 422 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_LT, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 42:
#line 426 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_GT, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 43:
#line 430 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_LE, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 44:
#line 434 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_GE, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 45:
#line 438 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 46:
#line 444 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 47:
#line 451 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_PLUS, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 48:
#line 455 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_MINUS, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 49:
#line 459 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 50:
#line 465 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_STAR, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 51:
#line 469 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_SLASH, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 52:
#line 473 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_2op_expression(RASQAL_EXPR_REM, (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr));
}
    break;

  case 53:
#line 477 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 54:
#line 483 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (2)].expr);
}
    break;

  case 55:
#line 487 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_UMINUS, (yyvsp[(2) - (2)].expr));
}
    break;

  case 56:
#line 491 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(1) - (1)].expr);
}
    break;

  case 57:
#line 497 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_TILDE, (yyvsp[(2) - (2)].expr));
}
    break;

  case 58:
#line 501 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_1op_expression(RASQAL_EXPR_BANG, (yyvsp[(2) - (2)].expr));
}
    break;

  case 59:
#line 505 "./rdql_parser.y"
    {
  rasqal_literal *l=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
  (yyval.expr)=rasqal_new_literal_expression(l);
}
    break;

  case 60:
#line 510 "./rdql_parser.y"
    {
  (yyval.expr)=rasqal_new_literal_expression((yyvsp[(1) - (1)].literal));
}
    break;

  case 61:
#line 514 "./rdql_parser.y"
    {
  (yyval.expr)=(yyvsp[(2) - (3)].expr);
}
    break;

  case 62:
#line 520 "./rdql_parser.y"
    {
  (yyval.literal)=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
}
    break;

  case 63:
#line 524 "./rdql_parser.y"
    {
  (yyval.literal)=rasqal_new_uri_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].uri));
}
    break;

  case 64:
#line 528 "./rdql_parser.y"
    {
  (yyval.literal)=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_QNAME, (yyvsp[(1) - (1)].name));
}
    break;

  case 65:
#line 534 "./rdql_parser.y"
    {
  (yyval.literal)=rasqal_new_variable_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].variable));
}
    break;

  case 66:
#line 538 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 67:
#line 544 "./rdql_parser.y"
    {
  (yyval.variable)=rasqal_new_variable((rasqal_query*)rq, (yyvsp[(2) - (2)].name), NULL);
}
    break;

  case 68:
#line 550 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 69:
#line 556 "./rdql_parser.y"
    {
  (yyval.literal)=rasqal_new_uri_literal(((rasqal_query*)rq)->world, (yyvsp[(1) - (1)].uri));
}
    break;

  case 70:
#line 560 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 71:
#line 564 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 72:
#line 568 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 73:
#line 572 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 74:
#line 576 "./rdql_parser.y"
    {
  (yyval.literal)=(yyvsp[(1) - (1)].literal);
}
    break;

  case 75:
#line 579 "./rdql_parser.y"
    {
  (yyval.literal)=rasqal_new_simple_literal(((rasqal_query*)rq)->world, RASQAL_LITERAL_QNAME, (yyvsp[(1) - (1)].name));
}
    break;

  case 76:
#line 586 "./rdql_parser.y"
    {
  (yyval.seq)=(yyvsp[(3) - (3)].seq);
  raptor_sequence_shift((yyval.seq), (yyvsp[(1) - (3)].uri));
}
    break;

  case 77:
#line 591 "./rdql_parser.y"
    {
  (yyval.seq)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_free_uri, (raptor_sequence_print_handler*)raptor_sequence_print_uri);
  raptor_sequence_push((yyval.seq), (yyvsp[(1) - (1)].uri));
}
    break;


/* Line 1267 of yacc.c.  */
#line 2181 "rdql_parser.c"
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


#line 597 "./rdql_parser.y"



/* Support functions */


/* This is declared in rdql_lexer.h but never used, so we always get
 * a warning unless this dummy code is here.  Used once below in an error case.
 */
static int yy_init_globals (yyscan_t yyscanner ) { return 0; }



/**
 * rasqal_rdql_query_engine_init - Initialise the RDQL query engine
 *
 * Return value: non 0 on failure
 **/
static int
rasqal_rdql_query_engine_init(rasqal_query* rdf_query, const char *name) {
  /* rasqal_rdql_query_engine* rdql=(rasqal_rdql_query_engine*)rdf_query->context; */

  /* Initialise rdf, rdfs, owl and xsd prefixes and namespaces */
  raptor_namespaces_start_namespace_full(rdf_query->namespaces, 
                                         (const unsigned char*)"rdf",
                                         (const unsigned char*)RAPTOR_RDF_MS_URI,0);
  raptor_namespaces_start_namespace_full(rdf_query->namespaces, 
                                         (const unsigned char*)"rdfs", 
                                         (const unsigned char*)RAPTOR_RDF_SCHEMA_URI,0);
  raptor_namespaces_start_namespace_full(rdf_query->namespaces,
                                         (const unsigned char*)"xsd",
                                         (const unsigned char*)RAPTOR_XMLSCHEMA_DATATYPES_URI, 0);
  raptor_namespaces_start_namespace_full(rdf_query->namespaces,
                                         (const unsigned char*)"owl",
                                         (const unsigned char*)RAPTOR_OWL_URI, 0);

  rdf_query->compare_flags = RASQAL_COMPARE_URI;

  return 0;
}


/**
 * rasqal_rdql_query_engine_terminate - Free the RDQL query engine
 *
 * Return value: non 0 on failure
 **/
static void
rasqal_rdql_query_engine_terminate(rasqal_query* rdf_query) {
  rasqal_rdql_query_engine* rdql=(rasqal_rdql_query_engine*)rdf_query->context;

  if(rdql->scanner_set) {
    rdql_lexer_lex_destroy(rdql->scanner);
    rdql->scanner_set=0;
  }

}


static int
rasqal_rdql_query_engine_prepare(rasqal_query* rdf_query) {
  /* rasqal_rdql_query_engine* rdql=(rasqal_rdql_query_engine*)rdf_query->context; */
  int rc;
  rasqal_graph_pattern *gp;
  
  if(!rdf_query->query_string)
    return 1;

  /* for RDQL only, before the graph pattern is made */
  rdf_query->constraints_sequence=raptor_new_sequence(NULL, (raptor_sequence_print_handler*)rasqal_expression_print);
  
  rc=rdql_parse(rdf_query);
  if(rc)
    return rc;

  gp=rasqal_new_basic_graph_pattern(rdf_query,
                                    rdf_query->triples,
                                    0, raptor_sequence_size(rdf_query->triples)-1);

  rdf_query->query_graph_pattern=gp;

  /* Now assign the constraints to the graph pattern */
  while(raptor_sequence_size(rdf_query->constraints_sequence)) {
    rasqal_expression* e=(rasqal_expression*)raptor_sequence_pop(rdf_query->constraints_sequence);
    rasqal_graph_pattern_add_constraint(gp, e);
  }
  raptor_free_sequence(rdf_query->constraints_sequence);

  /* Only now can we handle the prefixes and qnames */
  if(rasqal_query_declare_prefixes(rdf_query) ||
     rasqal_engine_expand_triple_qnames(rdf_query) ||
     rasqal_engine_expand_query_constraints_qnames(rdf_query))
    return 1;

  return rasqal_engine_prepare(rdf_query);
}


static int
rdql_parse(rasqal_query* rq) {
  rasqal_rdql_query_engine* rqe=(rasqal_rdql_query_engine*)rq->context;
  raptor_locator *locator=&rq->locator;
  void *buffer;
  
  if(!rq->query_string)
    return yy_init_globals(NULL); /* 0 but a way to use yy_init_globals */

  locator->line=1;
  locator->column= -1; /* No column info */
  locator->byte= -1; /* No bytes info */

#if RASQAL_DEBUG > 2
  rdql_parser_debug=1;
#endif

  rqe->lineno=1;

  rdql_lexer_lex_init(&rqe->scanner);
  rqe->scanner_set=1;

  rdql_lexer_set_extra(((rasqal_query*)rq), rqe->scanner);

  buffer= rdql_lexer__scan_buffer((char*)rq->query_string, rq->query_string_length, rqe->scanner);

  rqe->error_count=0;

  rdql_parser_parse(rq);

  rdql_lexer_lex_destroy(rqe->scanner);
  rqe->scanner_set=0;

  /* Parsing failed */
  if(rq->failed)
    return 1;
  
  return 0;
}


void
rdql_query_error(rasqal_query *rq, const char *msg) {
  rasqal_rdql_query_engine* rqe=(rasqal_rdql_query_engine*)rq->context;

  if(rqe->error_count++)
    return;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_RDQL_USE_ERROR_COLUMNS
  /*  rq->locator.column=rdql_lexer_get_column(yyscanner);*/
#endif

  rq->failed=1;
  rasqal_log_error_simple(rq->world, RAPTOR_LOG_LEVEL_FATAL,
                          &rq->locator, "%s", msg);

  return;
}


int
rdql_syntax_error(rasqal_query *rq, const char *message, ...)
{
  rasqal_rdql_query_engine *rqe=(rasqal_rdql_query_engine*)rq->context;
  va_list arguments;

  if(rqe->error_count++)
    return 0;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_RDQL_USE_ERROR_COLUMNS
  /*  rp->locator.column=rdql_lexer_get_column(yyscanner);*/
#endif

  va_start(arguments, message);
  rq->failed=1;
  rasqal_log_error_varargs(rq->world, RAPTOR_LOG_LEVEL_FATAL, &rq->locator,
                           message, arguments);
  va_end(arguments);

  return 0;
}


int
rdql_syntax_warning(rasqal_query *rq, const char *message, ...)
{
  rasqal_rdql_query_engine *rqe=(rasqal_rdql_query_engine*)rq->context;
  va_list arguments;

  rq->locator.line=rqe->lineno;
#ifdef RASQAL_RDQL_USE_ERROR_COLUMNS
  /*  rq->locator.column=rdql_lexer_get_column(yyscanner);*/
#endif

  va_start(arguments, message);
  rasqal_log_error_varargs(rq->world, RAPTOR_LOG_LEVEL_WARNING, &rq->locator,
                           message, arguments);
  va_end(arguments);

   return (0);
}


static void
rasqal_rdql_query_engine_register_factory(rasqal_query_engine_factory *factory)
{
  factory->context_length = sizeof(rasqal_rdql_query_engine);

  factory->init      = rasqal_rdql_query_engine_init;
  factory->terminate = rasqal_rdql_query_engine_terminate;
  factory->prepare   = rasqal_rdql_query_engine_prepare;
}


int
rasqal_init_query_engine_rdql (rasqal_world* world) {
  /* http://www.w3.org/Submission/2004/SUBM-RDQL-20040109/ */

  return rasqal_query_engine_register_factory(world,
                                              "rdql", 
                                              "RDF Data Query Language (RDQL)",
                                              NULL,
                                              (const unsigned char*)"http://jena.hpl.hp.com/2003/07/query/RDQL",
                                              &rasqal_rdql_query_engine_register_factory);
}



#ifdef STANDALONE
#include <stdio.h>
#include <locale.h>

#define RDQL_FILE_BUF_SIZE 2048

int
main(int argc, char *argv[]) 
{
  const char *program=rasqal_basename(argv[0]);
  char query_string[RDQL_FILE_BUF_SIZE];
  rasqal_query *query;
  FILE *fh;
  int rc;
  const char *filename=NULL;
  raptor_uri* base_uri=NULL;
  unsigned char *uri_string;
  rasqal_world *world;

#if RASQAL_DEBUG > 2
  rdql_parser_debug=1;
#endif

  if(argc > 1) {
    filename=argv[1];
    fh = fopen(argv[1], "r");
    if(!fh) {
      fprintf(stderr, "%s: Cannot open file %s - %s\n", program, filename,
              strerror(errno));
      exit(1);
    }
  } else {
    filename="<stdin>";
    fh = stdin;
  }

  memset(query_string, 0, RDQL_FILE_BUF_SIZE);
  rc=fread(query_string, RDQL_FILE_BUF_SIZE, 1, fh);
  if(rc < RDQL_FILE_BUF_SIZE) {
    if(ferror(fh)) {
      fprintf(stderr, "%s: file '%s' read failed - %s\n",
              program, filename, strerror(errno));
      fclose(fh);
      return(1);
    }
  }
  
  if(argc>1)
    fclose(fh);

  world=rasqal_new_world();

  query=rasqal_new_query(world, "rdql", NULL);

  uri_string=raptor_uri_filename_to_uri_string(filename);
  base_uri=raptor_new_uri(uri_string);
  
  rc=rasqal_query_prepare(query, (const unsigned char*)query_string, base_uri);

  rasqal_query_print(query, stdout);

  rasqal_free_query(query);

  raptor_free_uri(base_uri);

  raptor_free_memory(uri_string);

  rasqal_free_world(world);

  return rc;
}
#endif

