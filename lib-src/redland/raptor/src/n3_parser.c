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
#define yyparse n3_parser_parse
#define yylex   n3_parser_lex
#define yyerror n3_parser_error
#define yylval  n3_parser_lval
#define yychar  n3_parser_char
#define yydebug n3_parser_debug
#define yynerrs n3_parser_nerrs


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
     STRING_LITERAL = 268,
     URI_LITERAL = 269,
     BLANK_LITERAL = 270,
     QNAME_LITERAL = 271,
     PREFIX = 272,
     IDENTIFIER = 273,
     INTEGER_LITERAL = 274,
     FLOATING_LITERAL = 275,
     DECIMAL_LITERAL = 276,
     ERROR_TOKEN = 277
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
#define STRING_LITERAL 268
#define URI_LITERAL 269
#define BLANK_LITERAL 270
#define QNAME_LITERAL 271
#define PREFIX 272
#define IDENTIFIER 273
#define INTEGER_LITERAL 274
#define FLOATING_LITERAL 275
#define DECIMAL_LITERAL 276
#define ERROR_TOKEN 277




/* Copy the first part of user declarations.  */
#line 27 "./n3_parser.y"

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

#include <n3_parser.h>

#define YY_DECL int n3_lexer_lex (YYSTYPE *n3_parser_lval, yyscan_t yyscanner)
#define YY_NO_UNISTD_H 1
#include <n3_lexer.h>

#include <n3_common.h>


/* Make verbose error messages for syntax errors */
#ifdef RAPTOR_DEBUG
#define YYERROR_VERBOSE 1
#endif

/* Slow down the grammar operation and watch it work */
#if RAPTOR_DEBUG > 2
#define YYDEBUG 1
#endif

/* the lexer does not seem to track this */
#undef RAPTOR_N3_USE_ERROR_COLUMNS

/* Prototypes */ 
int n3_parser_error(void* rdf_parser, const char *msg);

/* Missing n3_lexer.c/h prototypes */
int n3_lexer_get_column(yyscan_t yyscanner);
/* Not used here */
/* void n3_lexer_set_column(int  column_no , yyscan_t yyscanner);*/


/* What the lexer wants */
extern int n3_lexer_lex (YYSTYPE *n3_parser_lval, yyscan_t scanner);
#define YYLEX_PARAM ((raptor_n3_parser*)(((raptor_parser*)rdf_parser)->context))->scanner

/* Pure parser argument (a void*) */
#define YYPARSE_PARAM rdf_parser

/* Make the yyerror below use the rdf_parser */
#undef yyerror
#define yyerror(message) n3_parser_error(rdf_parser, message)

/* Make lex/yacc interface as small as possible */
#undef yylex
#define yylex n3_lexer_lex


static raptor_triple* raptor_n3_new_triple(raptor_identifier *subject, raptor_identifier *predicate, raptor_identifier *object);
static void raptor_n3_free_triple(raptor_triple *triple);

#ifdef RAPTOR_DEBUG
static void raptor_triple_print(raptor_triple *data, FILE *fh);
#endif


/* Prototypes for local functions */
static void raptor_n3_generate_statement(raptor_parser *parser, raptor_triple *triple);



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
#line 119 "./n3_parser.y"
{
  unsigned char *string;
  raptor_identifier *identifier;
  raptor_sequence *sequence;
  raptor_uri *uri;
  int integer; /* 0+ for a xsd:integer datatyped RDF literal */
  double floating;
}
/* Line 187 of yacc.c.  */
#line 240 "n3_parser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 253 "n3_parser.c"

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
#define YYLAST   76

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  23
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  16
/* YYNRULES -- Number of rules.  */
#define YYNRULES  40
/* YYNRULES -- Number of states.  */
#define YYNSTATES  56

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   277

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
      15,    16,    17,    18,    19,    20,    21,    22
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     8,     9,    11,    15,    18,    22,
      24,    27,    29,    31,    33,    38,    41,    42,    45,    50,
      52,    54,    56,    58,    60,    62,    66,    72,    78,    82,
      86,    88,    90,    92,    94,    96,    98,   100,   104,   106,
     110
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      24,     0,    -1,    25,    -1,    25,    26,    -1,    -1,    31,
      -1,    32,    30,     6,    -1,     1,     6,    -1,    27,     7,
      34,    -1,    34,    -1,    28,    34,    -1,    34,    -1,    33,
      -1,     3,    -1,    30,     8,    29,    27,    -1,    29,    27,
      -1,    -1,    30,     8,    -1,    17,    18,    14,     6,    -1,
      36,    -1,    37,    -1,    36,    -1,    36,    -1,    37,    -1,
      35,    -1,    13,     4,    18,    -1,    13,     4,    18,     5,
      14,    -1,    13,     4,    18,     5,    16,    -1,    13,     5,
      14,    -1,    13,     5,    16,    -1,    13,    -1,    19,    -1,
      20,    -1,    21,    -1,    14,    -1,    16,    -1,    15,    -1,
       9,    30,    10,    -1,    38,    -1,    11,    28,    12,    -1,
      11,    12,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   170,   170,   173,   174,   177,   178,   227,   231,   271,
     315,   355,   399,   409,   427,   499,   540,   546,   557,   598,
     602,   609,   616,   620,   624,   637,   647,   661,   675,   689,
     702,   712,   730,   739,   755,   768,   785,   799,   864,   871,
     972
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"a\"", "\"@\"", "\"^\"", "\".\"",
  "\",\"", "\";\"", "\"[\"", "\"]\"", "\"(\"", "\")\"",
  "\"string literal\"", "\"URI literal\"", "\"blank node\"", "\"QName\"",
  "\"@prefix\"", "\"identifier\"", "\"integer literal\"",
  "\"floating point literal\"", "\"decimal literal\"", "ERROR_TOKEN",
  "$accept", "Document", "statementList", "statement", "objectList",
  "itemList", "verb", "propertyList", "directive", "subject", "predicate",
  "object", "literal", "resource", "blank", "collection", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    23,    24,    25,    25,    26,    26,    26,    27,    27,
      28,    28,    29,    29,    30,    30,    30,    30,    31,    32,
      32,    33,    34,    34,    34,    35,    35,    35,    35,    35,
      35,    35,    35,    35,    36,    36,    37,    37,    37,    38,
      38
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     0,     1,     3,     2,     3,     1,
       2,     1,     1,     1,     4,     2,     0,     2,     4,     1,
       1,     1,     1,     1,     1,     3,     5,     5,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     3,
       2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     0,     0,     1,     0,    16,     0,    34,    36,    35,
       0,     3,     5,    16,    19,    20,    38,     7,    13,     0,
       0,    12,    21,    40,    30,    31,    32,    33,     0,    11,
      24,    22,    23,     0,     0,    15,     9,    17,    37,     0,
       0,    39,    10,     0,     6,     0,     0,    25,    28,    29,
      18,     8,    14,     0,    26,    27
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,     2,    11,    35,    28,    19,    20,    12,    13,
      21,    36,    30,    31,    32,    16
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -13
static const yytype_int8 yypact[] =
{
     -13,     9,     2,   -13,     8,     7,    29,   -13,   -13,   -13,
      12,   -13,   -13,     7,   -13,   -13,   -13,   -13,   -13,    55,
      -3,   -13,   -13,   -13,    24,   -13,   -13,   -13,    42,   -13,
     -13,   -13,   -13,     1,     0,    13,   -13,     7,   -13,    14,
      10,   -13,   -13,    25,   -13,    55,    55,    28,   -13,   -13,
     -13,   -13,    13,    11,   -13,   -13
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -13,   -13,   -13,   -13,   -12,   -13,    -2,    33,   -13,   -13,
     -13,    -6,   -13,    -1,    35,   -13
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yytype_int8 yytable[] =
{
      29,    14,    -2,     4,    22,    37,    44,    38,    37,     3,
      18,     5,    22,     6,    17,    43,     7,     8,     9,    10,
      45,     7,    42,     9,    48,    54,    49,    55,    39,    40,
      33,    50,    47,    53,    52,    46,    22,    15,     5,    51,
       6,    23,    24,     7,     8,     9,    34,     0,    25,    26,
      27,     5,     0,     6,    41,    24,     7,     8,     9,     0,
       0,    25,    26,    27,     5,     0,     6,     0,    24,     7,
       8,     9,     0,     0,    25,    26,    27
};

static const yytype_int8 yycheck[] =
{
       6,     2,     0,     1,     5,     8,     6,    10,     8,     0,
       3,     9,    13,    11,     6,    14,    14,    15,    16,    17,
       7,    14,    28,    16,    14,    14,    16,    16,     4,     5,
      18,     6,    18,     5,    46,    37,    37,     2,     9,    45,
      11,    12,    13,    14,    15,    16,    13,    -1,    19,    20,
      21,     9,    -1,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    20,    21,     9,    -1,    11,    -1,    13,    14,
      15,    16,    -1,    -1,    19,    20,    21
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    24,    25,     0,     1,     9,    11,    14,    15,    16,
      17,    26,    31,    32,    36,    37,    38,     6,     3,    29,
      30,    33,    36,    12,    13,    19,    20,    21,    28,    34,
      35,    36,    37,    18,    30,    27,    34,     8,    10,     4,
       5,    12,    34,    14,     6,     7,    29,    18,    14,    16,
       6,    34,    27,     5,    14,    16
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
      case 13: /* "\"string literal\"" */
#line 162 "./n3_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1193 "n3_parser.c"
	break;
      case 14: /* "\"URI literal\"" */
#line 163 "./n3_parser.y"
	{ if((yyvaluep->uri)) raptor_free_uri((yyvaluep->uri)); };
#line 1198 "n3_parser.c"
	break;
      case 15: /* "\"blank node\"" */
#line 162 "./n3_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1203 "n3_parser.c"
	break;
      case 16: /* "\"QName\"" */
#line 163 "./n3_parser.y"
	{ if((yyvaluep->uri)) raptor_free_uri((yyvaluep->uri)); };
#line 1208 "n3_parser.c"
	break;
      case 18: /* "\"identifier\"" */
#line 162 "./n3_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1213 "n3_parser.c"
	break;
      case 21: /* "\"decimal literal\"" */
#line 162 "./n3_parser.y"
	{ if((yyvaluep->string)) RAPTOR_FREE(cstring, (yyvaluep->string)); };
#line 1218 "n3_parser.c"
	break;
      case 27: /* "objectList" */
#line 166 "./n3_parser.y"
	{ if((yyvaluep->sequence)) raptor_free_sequence((yyvaluep->sequence)); };
#line 1223 "n3_parser.c"
	break;
      case 28: /* "itemList" */
#line 166 "./n3_parser.y"
	{ if((yyvaluep->sequence)) raptor_free_sequence((yyvaluep->sequence)); };
#line 1228 "n3_parser.c"
	break;
      case 29: /* "verb" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1233 "n3_parser.c"
	break;
      case 30: /* "propertyList" */
#line 166 "./n3_parser.y"
	{ if((yyvaluep->sequence)) raptor_free_sequence((yyvaluep->sequence)); };
#line 1238 "n3_parser.c"
	break;
      case 32: /* "subject" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1243 "n3_parser.c"
	break;
      case 33: /* "predicate" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1248 "n3_parser.c"
	break;
      case 34: /* "object" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1253 "n3_parser.c"
	break;
      case 35: /* "literal" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1258 "n3_parser.c"
	break;
      case 36: /* "resource" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1263 "n3_parser.c"
	break;
      case 37: /* "blank" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1268 "n3_parser.c"
	break;
      case 38: /* "collection" */
#line 165 "./n3_parser.y"
	{ if((yyvaluep->identifier)) raptor_free_identifier((yyvaluep->identifier)); };
#line 1273 "n3_parser.c"
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
        case 6:
#line 179 "./n3_parser.y"
    {
  int i;

#if RAPTOR_DEBUG > 1  
  printf("statement 2\n subject=");
  if((yyvsp[(1) - (3)].identifier))
    raptor_identifier_print(stdout, (yyvsp[(1) - (3)].identifier));
  else
    fputs("NULL", stdout);
  if((yyvsp[(2) - (3)].sequence)) {
    printf("\n propertyList (reverse order to syntax)=");
    raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
    printf("\n");
  } else     
    printf("\n and empty propertyList\n");
#endif

  if((yyvsp[(1) - (3)].identifier) && (yyvsp[(2) - (3)].sequence)) {
    /* have subject and non-empty property list, handle it  */
    for(i=0; i<raptor_sequence_size((yyvsp[(2) - (3)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (3)].sequence), i);
      raptor_identifier *i2=(raptor_identifier*)RAPTOR_CALLOC(raptor_identifier, 1, sizeof(raptor_identifier));
      if(!i2) {
        raptor_free_sequence((yyvsp[(2) - (3)].sequence));
        raptor_free_identifier((yyvsp[(1) - (3)].identifier));
        YYERROR;
      }
      raptor_copy_identifier(i2, (yyvsp[(1) - (3)].identifier));
      t2->subject=i2;
      t2->subject->is_malloced=1;
    }
#if RAPTOR_DEBUG > 1  
    printf(" after substitution propertyList=");
    raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
    printf("\n\n");
#endif
    for(i=0; i<raptor_sequence_size((yyvsp[(2) - (3)].sequence)); i++) {
      raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (3)].sequence), i);
      raptor_n3_generate_statement((raptor_parser*)rdf_parser, t2);
    }
  }

  if((yyvsp[(2) - (3)].sequence))
    raptor_free_sequence((yyvsp[(2) - (3)].sequence));

  if((yyvsp[(1) - (3)].identifier))
    raptor_free_identifier((yyvsp[(1) - (3)].identifier));
}
    break;

  case 8:
#line 232 "./n3_parser.y"
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
    triple=raptor_n3_new_triple(NULL, NULL, (yyvsp[(3) - (3)].identifier));
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

  case 9:
#line 272 "./n3_parser.y"
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
    triple=raptor_n3_new_triple(NULL, NULL, (yyvsp[(1) - (1)].identifier));
    if(!triple)
      YYERROR;
#ifdef RAPTOR_DEBUG
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_n3_free_triple,
                           (raptor_sequence_print_handler*)raptor_triple_print);
#else
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_n3_free_triple, NULL);
#endif
    if(!(yyval.sequence)) {
      raptor_n3_free_triple(triple);
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

  case 10:
#line 316 "./n3_parser.y"
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
    triple=raptor_n3_new_triple(NULL, NULL, (yyvsp[(2) - (2)].identifier));
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

  case 11:
#line 356 "./n3_parser.y"
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
    triple=raptor_n3_new_triple(NULL, NULL, (yyvsp[(1) - (1)].identifier));
    if(!triple)
      YYERROR;
#ifdef RAPTOR_DEBUG
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_n3_free_triple,
                           (raptor_sequence_print_handler*)raptor_triple_print);
#else
    (yyval.sequence)=raptor_new_sequence((raptor_sequence_free_handler*)raptor_n3_free_triple, NULL);
#endif
    if(!(yyval.sequence)) {
      raptor_n3_free_triple(triple);
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

  case 12:
#line 400 "./n3_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("verb predicate=");
  raptor_identifier_print(stdout, (yyvsp[(1) - (1)].identifier));
  printf("\n");
#endif

  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 13:
#line 410 "./n3_parser.y"
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

  case 14:
#line 428 "./n3_parser.y"
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
      raptor_copy_identifier(i2, (yyvsp[(3) - (4)].identifier));
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
        if((yyvsp[(3) - (4)].identifier))
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

  case 15:
#line 500 "./n3_parser.y"
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
        raptor_free_sequence((yyvsp[(2) - (2)].sequence));
        raptor_free_identifier((yyvsp[(1) - (2)].identifier));
        YYERROR;
      }
      raptor_copy_identifier(i2, (yyvsp[(1) - (2)].identifier));
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

  case 16:
#line 540 "./n3_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("propertyList 4\n empty returning NULL\n\n");
#endif
  (yyval.sequence)=NULL;
}
    break;

  case 17:
#line 547 "./n3_parser.y"
    {
  (yyval.sequence)=(yyvsp[(1) - (2)].sequence);
#if RAPTOR_DEBUG > 1  
  printf("propertyList 5\n trailing semicolon returning existing list ");
  raptor_sequence_print((yyval.sequence), stdout);
  printf("\n\n");
#endif
}
    break;

  case 18:
#line 558 "./n3_parser.y"
    {
  unsigned char *prefix=(yyvsp[(2) - (4)].string);
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)(((raptor_parser*)rdf_parser)->context);
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

  ns=raptor_new_namespace_from_uri(&n3_parser->namespaces, prefix, (yyvsp[(3) - (4)].uri), 0);
  if(ns) {
    raptor_namespaces_start_namespace(&n3_parser->namespaces, ns);
    raptor_parser_start_namespace((raptor_parser*)rdf_parser, ns);
  }

  if((yyvsp[(2) - (4)].string))
    RAPTOR_FREE(cstring, (yyvsp[(2) - (4)].string));
  raptor_free_uri((yyvsp[(3) - (4)].uri));

  if(!ns)
    YYERROR;
}
    break;

  case 19:
#line 599 "./n3_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 20:
#line 603 "./n3_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 21:
#line 610 "./n3_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 22:
#line 617 "./n3_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 23:
#line 621 "./n3_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 24:
#line 625 "./n3_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("object literal=");
  raptor_identifier_print(stdout, (yyvsp[(1) - (1)].identifier));
  printf("\n");
#endif

  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 25:
#line 638 "./n3_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal + language string=\"%s\"\n", (yyvsp[(1) - (3)].string));
#endif

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (3)].string), NULL, (yyvsp[(3) - (3)].string));
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 26:
#line 648 "./n3_parser.y"
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

  case 27:
#line 662 "./n3_parser.y"
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

  case 28:
#line 676 "./n3_parser.y"
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

  case 29:
#line 690 "./n3_parser.y"
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

  case 30:
#line 703 "./n3_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("literal string=\"%s\"\n", (yyvsp[(1) - (1)].string));
#endif

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (1)].string), NULL, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 31:
#line 713 "./n3_parser.y"
    {
  unsigned char *string;
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource integer=%d\n", (yyvsp[(1) - (1)].integer));
#endif
  string=(unsigned char*)RAPTOR_MALLOC(cstring, 32); /* FIXME */
  if(!string)
    YYERROR;
  sprintf((char*)string, "%d", (yyvsp[(1) - (1)].integer));
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#integer");
  if(!uri)
    YYERROR;
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, string, uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 32:
#line 731 "./n3_parser.y"
    {
#if RAPTOR_DEBUG > 1  
  printf("resource double=%1g\n", (yyvsp[(1) - (1)].floating));
#endif
  (yyval.identifier)=raptor_new_identifier_from_double((yyvsp[(1) - (1)].floating));
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 33:
#line 740 "./n3_parser.y"
    {
  raptor_uri *uri;
#if RAPTOR_DEBUG > 1  
  printf("resource decimal=%s\n", (yyvsp[(1) - (1)].string));
#endif
  uri=raptor_new_uri((const unsigned char*)"http://www.w3.org/2001/XMLSchema#decimal");
  if(!uri)
    YYERROR;
  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_LITERAL, NULL, RAPTOR_URI_SOURCE_ELEMENT, NULL, (yyvsp[(1) - (1)].string), uri, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;

  case 34:
#line 756 "./n3_parser.y"
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

  case 35:
#line 769 "./n3_parser.y"
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

  case 36:
#line 786 "./n3_parser.y"
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

  case 37:
#line 800 "./n3_parser.y"
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
      raptor_n3_generate_statement((raptor_parser*)rdf_parser, t2);
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

  case 38:
#line 865 "./n3_parser.y"
    {
  (yyval.identifier)=(yyvsp[(1) - (1)].identifier);
}
    break;

  case 39:
#line 872 "./n3_parser.y"
    {
  int i;
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)(((raptor_parser*)rdf_parser)->context);
  raptor_identifier* first_identifier=NULL;
  raptor_identifier* rest_identifier=NULL;
  raptor_identifier* object=NULL;
  raptor_identifier* blank=NULL;

#if RAPTOR_DEBUG > 1  
  printf("collection\n objectList=");
  raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
  printf("\n");
#endif

  first_identifier=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(n3_parser->first_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!first_identifier)
    goto err_collection;
  rest_identifier=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(n3_parser->rest_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!rest_identifier)
    goto err_collection;
  
  /* non-empty property list, handle it  */
#if RAPTOR_DEBUG > 1  
  printf("resource\n propertyList=");
  raptor_sequence_print((yyvsp[(2) - (3)].sequence), stdout);
  printf("\n");
#endif

  object=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(n3_parser->nil_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!object)
    goto err_collection;

  for(i=raptor_sequence_size((yyvsp[(2) - (3)].sequence))-1; i>=0; i--) {
    raptor_triple* t2=(raptor_triple*)raptor_sequence_get_at((yyvsp[(2) - (3)].sequence), i);
    const unsigned char *blank_id;
    raptor_identifier* temp;

    blank_id=raptor_parser_internal_generate_id((raptor_parser*)rdf_parser, RAPTOR_GENID_TYPE_BNODEID, NULL);
    if(!blank_id)
      goto err_collection;

    blank=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_ANONYMOUS, NULL, RAPTOR_URI_SOURCE_GENERATED, blank_id, NULL, NULL, NULL);
    if(!blank)
      goto err_collection;
    
    t2->subject=blank;
    t2->predicate=first_identifier;
    /* t2->object already set to the value we want */
    raptor_n3_generate_statement((raptor_parser*)rdf_parser, t2);
    
    temp=t2->object;
    
    t2->subject=blank;
    t2->predicate=rest_identifier;
    t2->object=object;
    raptor_n3_generate_statement((raptor_parser*)rdf_parser, t2);

    raptor_free_identifier(object);
      
    t2->subject=NULL;
    t2->predicate=NULL;
    t2->object=temp;

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

  case 40:
#line 973 "./n3_parser.y"
    {
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)(((raptor_parser*)rdf_parser)->context);

#if RAPTOR_DEBUG > 1  
  printf("collection\n empty\n");
#endif

  (yyval.identifier)=raptor_new_identifier(RAPTOR_IDENTIFIER_TYPE_RESOURCE, raptor_uri_copy(n3_parser->nil_uri), RAPTOR_URI_SOURCE_URI, NULL, NULL, NULL, NULL);
  if(!(yyval.identifier))
    YYERROR;
}
    break;


/* Line 1267 of yacc.c.  */
#line 2456 "n3_parser.c"
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


#line 987 "./n3_parser.y"



/* Support functions */

/* This is declared in n3_lexer.h but never used, so we always get
 * a warning unless this dummy code is here.  Used once below as a return.
 */
static int yy_init_globals (yyscan_t yyscanner ) { return 0; }


/* helper - everything passed in is now owned by triple */
static raptor_triple*
raptor_n3_new_triple(raptor_identifier *subject,
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
raptor_n3_free_triple(raptor_triple *t) {
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
n3_parser_error(void* ctx, const char *msg)
{
  raptor_parser* rdf_parser=(raptor_parser *)ctx;
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;
  
  if(n3_parser->error_count++)
    return 0;

  rdf_parser->locator.line=n3_parser->lineno;
#ifdef RAPTOR_N3_USE_ERROR_COLUMNS
  rdf_parser->locator.column=n3_lexer_get_column(yyscanner);
#endif

  raptor_parser_simple_error(rdf_parser, "%s", msg);
  return yy_init_globals(NULL); /* 0 but a way to use yy_init_globals */
}


int
n3_syntax_error(raptor_parser *rdf_parser, const char *message, ...)
{
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;
  va_list arguments;

  if(n3_parser->error_count++)
    return 0;

  rdf_parser->locator.line=n3_parser->lineno;
#ifdef RAPTOR_N3_USE_ERROR_COLUMNS
  rdf_parser->locator.column=n3_lexer_get_column(yyscanner);
#endif

  va_start(arguments, message);
  
  raptor_parser_error_varargs(((raptor_parser*)rdf_parser), message, arguments);

  va_end(arguments);

  return 0;
}


raptor_uri*
n3_qname_to_uri(raptor_parser *rdf_parser, unsigned char *name, size_t name_len) 
{
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;

  rdf_parser->locator.line=n3_parser->lineno;
#ifdef RAPTOR_N3_USE_ERROR_COLUMNS
  rdf_parser->locator.column=n3_lexer_get_column(yyscanner);
#endif

  return raptor_qname_string_to_uri(&n3_parser->namespaces,
                                    name, name_len,
                                    (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser);
}



static int
n3_parse(raptor_parser *rdf_parser, const char *string) {
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;
  void *buffer;
  
  if(!string || !*string)
    return 0;
  
  if(n3_lexer_lex_init(&n3_parser->scanner))
    return 1;
  n3_parser->scanner_set=1;

  n3_lexer_set_extra(rdf_parser, n3_parser->scanner);
  buffer= n3_lexer__scan_string(string, n3_parser->scanner);

  n3_parser_parse(rdf_parser);

  n3_lexer_lex_destroy(n3_parser->scanner);
  n3_parser->scanner_set=0;

  return 0;
}


/**
 * raptor_n3_parse_init - Initialise the Raptor N3 parser
 *
 * Return value: non 0 on failure
 **/

static int
raptor_n3_parse_init(raptor_parser* rdf_parser, const char *name) {
  raptor_n3_parser* n3_parser=(raptor_n3_parser*)rdf_parser->context;
  const raptor_uri_handler *uri_handler;
  void *uri_context;

  raptor_uri_get_handler(&uri_handler, &uri_context);

  if(raptor_namespaces_init(&n3_parser->namespaces,
                            uri_handler, uri_context,
                            (raptor_simple_message_handler)raptor_parser_simple_error, rdf_parser, 
                            0))
    return 1;

  n3_parser->nil_uri=raptor_new_uri_for_rdf_concept("nil");
  n3_parser->first_uri=raptor_new_uri_for_rdf_concept("first");
  n3_parser->rest_uri=raptor_new_uri_for_rdf_concept("rest");

  if(!n3_parser->nil_uri || !n3_parser->first_uri || !n3_parser->rest_uri)
    return 1;

  return 0;
}


/* PUBLIC FUNCTIONS */


/*
 * raptor_n3_parse_terminate - Free the Raptor N3 parser
 * @rdf_parser: parser object
 * 
 **/
static void
raptor_n3_parse_terminate(raptor_parser *rdf_parser) {
  raptor_n3_parser *n3_parser=(raptor_n3_parser*)rdf_parser->context;

  if(n3_parser->nil_uri)
    raptor_free_uri(n3_parser->nil_uri);
  if(n3_parser->first_uri)
    raptor_free_uri(n3_parser->first_uri);
  if(n3_parser->rest_uri)
    raptor_free_uri(n3_parser->rest_uri);

  raptor_namespaces_clear(&n3_parser->namespaces);

  if(n3_parser->scanner_set) {
    n3_lexer_lex_destroy(n3_parser->scanner);
    n3_parser->scanner_set=0;
  }

  if(n3_parser->buffer_length)
    RAPTOR_FREE(cdata, n3_parser->buffer);
}


static void
raptor_n3_generate_statement(raptor_parser *parser, raptor_triple *t)
{
  /* raptor_n3_parser *n3_parser=(raptor_n3_parser*)parser->context; */
  raptor_statement *statement=&parser->statement;

  if(!t->subject || !t->predicate || !t->object)
    return;

  /* Two choices for subject for N3 */
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
  

  /* Three choices for object for N3 */
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
raptor_n3_parse_chunk(raptor_parser* rdf_parser, 
                      const unsigned char *s, size_t len,
                      int is_end)
{
  char *buffer;
  char *ptr;
  raptor_n3_parser *n3_parser=(raptor_n3_parser*)rdf_parser->context;
  
#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
  RAPTOR_DEBUG2("adding %d bytes to line buffer\n", (int)len);
#endif

  if(len) {
    buffer=(char*)RAPTOR_REALLOC(cstring, n3_parser->buffer, n3_parser->buffer_length + len + 1);
    if(!buffer) {
      raptor_parser_fatal_error(rdf_parser, "Out of memory");
      return 1;
    }
    n3_parser->buffer=buffer;

    /* move pointer to end of cdata buffer */
    ptr=buffer+n3_parser->buffer_length;

    /* adjust stored length */
    n3_parser->buffer_length += len;

    /* now write new stuff at end of cdata buffer */
    strncpy(ptr, (char*)s, len);
    ptr += len;
    *ptr = '\0';

#if defined(RAPTOR_DEBUG) && RAPTOR_DEBUG > 1
    RAPTOR_DEBUG3("buffer buffer now '%s' (%d bytes)\n", 
                  n3_parser->buffer, n3_parser->buffer_length);
#endif
  }
  
  /* if not end, wait for rest of input */
  if(!is_end)
    return 0;

  /* Nothing to do */
  if(!n3_parser->buffer_length)
    return 0;
  
  return n3_parse(rdf_parser, n3_parser->buffer);
}


static int
raptor_n3_parse_start(raptor_parser *rdf_parser) 
{
  raptor_locator *locator=&rdf_parser->locator;
  raptor_n3_parser *n3_parser=(raptor_n3_parser*)rdf_parser->context;

  /* base URI required for N3 */
  if(!rdf_parser->base_uri)
    return 1;

  locator->line=1;
  locator->column= -1; /* No column info */
  locator->byte= -1; /* No bytes info */

  if(n3_parser->buffer_length) {
    RAPTOR_FREE(cdata, n3_parser->buffer);
    n3_parser->buffer=NULL;
    n3_parser->buffer_length=0;
  }
  
  n3_parser->lineno=1;

  return 0;
}


static int
raptor_n3_parse_recognise_syntax(raptor_parser_factory* factory, 
                                 const unsigned char *buffer, size_t len,
                                 const unsigned char *identifier, 
                                 const unsigned char *suffix, 
                                 const char *mime_type)
{
  int score= 0;
  
  if(suffix) {
    if(!strcmp((const char*)suffix, "n3"))
      score=8;
  }
  
  if(mime_type) {
    if(strstr((const char*)mime_type, "n3"))
      score+=6;
  }
  
  return score;
}


static int
raptor_n3_parser_register_factory(raptor_parser_factory *factory) 
{
  int rc=0;

  factory->context_length     = sizeof(raptor_n3_parser);
  
  factory->need_base_uri = 1;
  
  factory->init      = raptor_n3_parse_init;
  factory->terminate = raptor_n3_parse_terminate;
  factory->start     = raptor_n3_parse_start;
  factory->chunk     = raptor_n3_parse_chunk;
  factory->recognise_syntax = raptor_n3_parse_recognise_syntax;

  rc+= raptor_parser_factory_add_mime_type(factory, "text/n3", 6) != 0;
  rc+= raptor_parser_factory_add_mime_type(factory, "application/rdf+n3", 6) != 0;

  return rc;
}


int
raptor_init_parser_n3(void)
{
  return !raptor_parser_register_factory("n3",  "Notation 3",
                                         &raptor_n3_parser_register_factory);
}



#ifdef STANDALONE
#include <stdio.h>
#include <locale.h>

#define N3_FILE_BUF_SIZE 2048

static
void n3_parser_print_statement(void *user, const raptor_statement *statement) 
{
  FILE* stream=(FILE*)user;
  raptor_print_statement(statement, stream);
  putc('\n', stream);
}
  


int
main(int argc, char *argv[]) 
{
  char string[N3_FILE_BUF_SIZE];
  raptor_parser rdf_parser; /* static */
  raptor_n3_parser n3_parser; /* static */
  raptor_locator *locator=&rdf_parser.locator;
  FILE *fh;
  char *filename;
  int rc;
  
#if RAPTOR_DEBUG > 2
  n3_parser_debug=1;
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

  memset(string, 0, N3_FILE_BUF_SIZE);
  rc=fread(string, N3_FILE_BUF_SIZE, 1, fh);
  if(rc < N3_FILE_BUF_SIZE) {
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
  memset(&n3_parser, 0, sizeof(raptor_n3_parser));

  locator->line= locator->column = -1;
  locator->file= filename;

  n3_parser.lineno= 1;

  rdf_parser.context=&n3_parser;
  rdf_parser.base_uri=raptor_new_uri((const unsigned char*)"http://example.org/fake-base-uri/");

  raptor_set_statement_handler(&rdf_parser, stdout, n3_parser_print_statement);
  raptor_n3_parse_init(&rdf_parser, "n3");
  
  n3_parser.error_count=0;

  n3_parse(&rdf_parser, string);

  raptor_free_uri(rdf_parser.base_uri);

  return (0);
}
#endif

