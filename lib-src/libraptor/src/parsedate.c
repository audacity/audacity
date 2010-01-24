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
#define yyparse raptor_parsedate_parse
#define yylex   raptor_parsedate_lex
#define yyerror raptor_parsedate_error
#define yylval  raptor_parsedate_lval
#define yychar  raptor_parsedate_char
#define yydebug raptor_parsedate_debug
#define yynerrs raptor_parsedate_nerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     tAGO = 258,
     tDAY = 259,
     tDAY_UNIT = 260,
     tDAYZONE = 261,
     tDST = 262,
     tHOUR_UNIT = 263,
     tID = 264,
     tTZONE = 265,
     tWZONE = 266,
     tZZONE = 267,
     tMERIDIAN = 268,
     tMINUTE_UNIT = 269,
     tMONTH = 270,
     tMONTH_UNIT = 271,
     tSEC_UNIT = 272,
     tSNUMBER = 273,
     tUNUMBER = 274,
     tYEAR_UNIT = 275,
     tZONE = 276
   };
#endif
/* Tokens.  */
#define tAGO 258
#define tDAY 259
#define tDAY_UNIT 260
#define tDAYZONE 261
#define tDST 262
#define tHOUR_UNIT 263
#define tID 264
#define tTZONE 265
#define tWZONE 266
#define tZZONE 267
#define tMERIDIAN 268
#define tMINUTE_UNIT 269
#define tMONTH 270
#define tMONTH_UNIT 271
#define tSEC_UNIT 272
#define tSNUMBER 273
#define tUNUMBER 274
#define tYEAR_UNIT 275
#define tZONE 276




/* Copy the first part of user declarations.  */
#line 1 "./parsedate.y"

/*
 * Imported from
 *   PHP CVS 1.56.2.2
 *   Fri May 20 07:14:01 2005
 *   http://cvs.php.net/php-src/ext/standard/parsedate.y
 *
 * and patched from there
 *
 * 1.59 removed this from PHP CVS and replaced it with entirely new
 * code written under the PHP license:
 *   http://viewcvs.php.net/viewcvs.cgi/php-src/ext/date/lib/
 * That code is not used here and cannot be used.
 *
 * The old version is now in the CVS Attic:
 *   http://viewcvs.php.net/viewcvs.cgi/php-src/ext/standard/Attic/parsedate.y
 */


/*
**  Originally written by Steven M. Bellovin <smb@research.att.com> while
**  at the University of North Carolina at Chapel Hill.  Later tweaked by
**  a couple of people on Usenet.  Completely overhauled by Rich $alz
**  <rsalz@bbn.com> and Jim Berets <jberets@bbn.com> in August, 1990.
**
**  This code is in the public domain and has no copyright.
*/


#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <ctype.h>

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if defined(_HPUX_SOURCE)
#include <alloca.h>
#endif

#if defined (STDC_HEADERS) || (!defined (isascii) && !defined (HAVE_ISASCII))
# define IN_CTYPE_DOMAIN(c) 1
#else
# define IN_CTYPE_DOMAIN(c) isascii(c)
#endif

#define ISSPACE(c) (IN_CTYPE_DOMAIN (c) && isspace (c))
#define ISALPHA(c) (IN_CTYPE_DOMAIN (c) && isalpha (c))
#define ISUPPER(c) (IN_CTYPE_DOMAIN (c) && isupper (c))
#define ISDIGIT_LOCALE(c) (IN_CTYPE_DOMAIN (c) && isdigit (c))

/* ISDIGIT differs from ISDIGIT_LOCALE, as follows:
   - Its arg may be any int or unsigned int; it need not be an unsigned char.
   - It's guaranteed to evaluate its argument exactly once.
   - It's typically faster.
   Posix 1003.2-1992 section 2.5.2.1 page 50 lines 1556-1558 says that
   only '0' through '9' are digits.  Prefer ISDIGIT to ISDIGIT_LOCALE unless
   it's important to use the locale's definition of `digit' even when the
   host does not conform to Posix.  */
#define ISDIGIT(c) ((unsigned) (c) - '0' <= 9)

#if defined (STDC_HEADERS) || defined (USG)
# include <string.h>
#endif

#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 7)
# define __attribute__(x)
#endif

#ifndef ATTRIBUTE_UNUSED
# define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif

/* Some old versions of bison generate parsers that use bcopy.
   That loses on systems that don't provide the function, so we have
   to redefine it here.  */
#if !defined (HAVE_BCOPY) && defined (HAVE_MEMCPY) && !defined (bcopy)
# define bcopy(from, to, len) memcpy ((to), (from), (len))
#endif

/* Prototypes */ 
static int raptor_parsedate_error(const char *msg);
time_t raptor_parse_date(const char *p, time_t *now);


#define EPOCH		1970
#define HOUR(x)		((x) * 60)

#define MAX_BUFF_LEN    128   /* size of buffer to read the date into */

/*
**  An entry in the lexical lookup table.
*/
typedef struct _TABLE {
    const char	*name;
    int		type;
    int		value;
} TABLE;


/*
**  Meridian:  am, pm, or 24-hour style.
*/
typedef enum _MERIDIAN {
    MERam, MERpm, MER24
} MERIDIAN;

struct date_yy {
	const char	*yyInput;
	int	yyDayOrdinal;
	int	yyDayNumber;
	int	yyHaveDate;
	int	yyHaveDay;
	int	yyHaveRel;
	int	yyHaveTime;
	int	yyHaveZone;
	int	yyTimezone;
	int	yyDay;
	int	yyHour;
	int	yyMinutes;
	int	yyMonth;
	int	yySeconds;
	int	yyYear;
	MERIDIAN	yyMeridian;
	int	yyRelDay;
	int	yyRelHour;
	int	yyRelMinutes;
	int	yyRelMonth;
	int	yyRelSeconds;
	int	yyRelYear;
};

typedef union _date_ll {
    int			Number;
    enum _MERIDIAN	Meridian;
} date_ll;

#define YYPARSE_PARAM parm
#define YYLEX_PARAM parm
#define YYSTYPE date_ll
#define YYLTYPE void

static int yylex (YYSTYPE *lvalp, void *parm);

static int ToHour (int Hours, MERIDIAN Meridian);
static int ToYear (int Year);
static int LookupWord (YYSTYPE *lvalp, char *buff);



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
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 320 "parsedate.c"

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
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
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
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   110

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  26
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  22
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNRULES -- Number of states.  */
#define YYNSTATES  100

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   276

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    24,     2,    22,    25,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    23,     2,
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
      15,    16,    17,    18,    19,    20,    21
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    11,    13,    15,    17,
      19,    21,    24,    26,    30,    34,    38,    42,    44,    46,
      48,    49,    52,    53,    55,    59,    63,    67,    69,    71,
      73,    75,    77,    80,    82,    85,    88,    92,   101,   107,
     109,   111,   115,   119,   122,   127,   130,   134,   138,   142,
     146,   149,   152,   155,   159,   161,   165,   168,   170,   173,
     176,   178,   181,   184,   186,   189,   192,   194,   197,   200,
     202,   205,   208,   210,   213,   216,   218,   220
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      27,     0,    -1,    -1,    27,    28,    -1,    29,    -1,    37,
      -1,    39,    -1,    38,    -1,    44,    -1,    46,    -1,    47,
      -1,    19,    13,    -1,    30,    -1,    35,    32,    44,    -1,
      36,    32,    44,    -1,    35,    32,    31,    -1,    36,    32,
      31,    -1,    33,    -1,    34,    -1,    37,    -1,    -1,    22,
      19,    -1,    -1,    18,    -1,    18,    23,    19,    -1,    36,
      23,    19,    -1,    19,    23,    19,    -1,    10,    -1,    11,
      -1,    12,    -1,    21,    -1,     6,    -1,    21,     7,    -1,
       4,    -1,     4,    24,    -1,    19,     4,    -1,    19,    25,
      19,    -1,    15,    19,    19,    23,    19,    23,    19,    19,
      -1,    19,    25,    19,    25,    19,    -1,    41,    -1,    40,
      -1,    19,    15,    18,    -1,    15,    19,    19,    -1,    15,
      19,    -1,    15,    19,    24,    19,    -1,    19,    15,    -1,
      19,    15,    19,    -1,    41,    10,    43,    -1,    19,    10,
      43,    -1,    19,    18,    18,    -1,    19,    18,    -1,    19,
      42,    -1,    11,    19,    -1,    11,    19,    18,    -1,    30,
      -1,    19,    32,    31,    -1,    45,     3,    -1,    45,    -1,
      19,    20,    -1,    18,    20,    -1,    20,    -1,    19,    16,
      -1,    18,    16,    -1,    16,    -1,    19,     5,    -1,    18,
       5,    -1,     5,    -1,    19,     8,    -1,    18,     8,    -1,
       8,    -1,    19,    14,    -1,    18,    14,    -1,    14,    -1,
      19,    17,    -1,    18,    17,    -1,    17,    -1,    19,    -1,
      13,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   180,   180,   181,   184,   187,   190,   193,   196,   199,
     200,   203,   209,   213,   216,   220,   223,   229,   232,   235,
     238,   241,   243,   246,   256,   262,   268,   284,   287,   290,
     293,   296,   299,   304,   308,   312,   318,   322,   333,   351,
     352,   355,   361,   366,   374,   379,   387,   394,   395,   414,
     420,   426,   438,   441,   447,   448,   473,   487,   490,   493,
     496,   499,   502,   505,   508,   511,   514,   517,   520,   523,
     526,   529,   532,   535,   538,   541,   546,   581
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "tAGO", "tDAY", "tDAY_UNIT", "tDAYZONE",
  "tDST", "tHOUR_UNIT", "tID", "tTZONE", "tWZONE", "tZZONE", "tMERIDIAN",
  "tMINUTE_UNIT", "tMONTH", "tMONTH_UNIT", "tSEC_UNIT", "tSNUMBER",
  "tUNUMBER", "tYEAR_UNIT", "tZONE", "'.'", "':'", "','", "'/'", "$accept",
  "spec", "item", "time", "iso8601time_colon", "iso8601zonepart",
  "sec_fraction_part", "zonepart_numeric_without_colon",
  "zonepart_numeric_with_colon", "HMStime_with_colon", "HMtime_with_colon",
  "zone", "day", "date", "iso8601datetime", "iso8601date",
  "iso8601weekspec", "iso8601time", "rel", "relunit", "number", "o_merid", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,    46,    58,    44,    47
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    26,    27,    27,    28,    28,    28,    28,    28,    28,
      28,    29,    29,    30,    30,    30,    30,    31,    31,    31,
      31,    32,    32,    33,    34,    35,    36,    37,    37,    37,
      37,    37,    37,    38,    38,    38,    39,    39,    39,    39,
      39,    39,    39,    39,    39,    39,    39,    40,    40,    41,
      41,    41,    42,    42,    43,    43,    44,    44,    45,    45,
      45,    45,    45,    45,    45,    45,    45,    45,    45,    45,
      45,    45,    45,    45,    45,    45,    46,    47
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     3,     3,     3,     3,     1,     1,     1,
       0,     2,     0,     1,     3,     3,     3,     1,     1,     1,
       1,     1,     2,     1,     2,     2,     3,     8,     5,     1,
       1,     3,     3,     2,     4,     2,     3,     3,     3,     3,
       2,     2,     2,     3,     1,     3,     2,     1,     2,     2,
       1,     2,     2,     1,     2,     2,     1,     2,     2,     1,
       2,     2,     1,     2,     2,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,    33,    66,    31,    69,    27,    28,    29,
      77,    72,     0,    63,    75,     0,    76,    60,    30,     3,
       4,    12,    22,    22,     5,     7,     6,    40,    39,     8,
      57,     9,    10,    34,    43,    65,    68,    71,    62,    74,
      59,    35,    64,    67,     0,     0,    11,    70,    45,    61,
      73,    50,    58,     0,     0,    51,    32,     0,    20,     0,
      20,     0,    56,    42,     0,    22,    54,    48,    52,    41,
      46,    49,    26,    36,    21,    23,     0,    15,    17,    18,
      19,    13,    25,    16,    14,    47,     0,    44,    20,    53,
       0,     0,     0,    23,    55,    38,    24,     0,     0,    37
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,    19,    20,    66,    77,    58,    78,    79,    22,
      23,    80,    25,    26,    27,    28,    55,    67,    29,    30,
      31,    32
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -60
static const yytype_int8 yypact[] =
{
     -60,     2,   -60,   -13,   -60,   -60,   -60,   -60,   -60,   -60,
     -60,   -60,    13,   -60,   -60,    69,    20,   -60,    32,   -60,
     -60,   -60,    29,     4,   -60,   -60,   -60,   -60,    44,   -60,
      58,   -60,   -60,   -60,   -15,   -60,   -60,   -60,   -60,   -60,
     -60,   -60,   -60,   -60,    46,    48,   -60,   -60,    28,   -60,
     -60,    37,   -60,    56,    57,   -60,   -60,    59,    52,    61,
      52,    46,   -60,    64,    62,    26,   -60,   -60,    66,   -60,
     -60,   -60,   -60,    68,   -60,    36,    74,   -60,   -60,   -60,
     -60,   -60,   -60,   -60,   -60,   -60,    76,   -60,    86,   -60,
      80,    81,    78,    79,   -60,   -60,   -60,    84,    87,   -60
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -60,   -60,   -60,   -60,   104,   -59,   -23,   -60,   -60,   -60,
     -60,   107,   -60,   -60,   -60,   -60,   -60,    49,   -55,   -60,
     -60,   -60
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      60,    83,     2,    81,    63,    84,     3,     4,     5,    64,
       6,    33,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    41,    42,    57,    59,    43,    94,
      44,    45,    34,    46,    47,    48,    49,    50,    51,    56,
      52,    35,    88,    53,    36,    54,    69,    70,    57,    53,
      37,    57,    38,    39,    61,    71,    40,     4,     5,    91,
       6,    62,     7,     8,     9,    65,    11,    68,    13,    14,
      75,    76,    17,    18,    35,    72,    73,    36,    74,    42,
      82,    87,    43,    37,    89,    38,    39,    86,    47,    40,
      49,    50,     5,    90,    52,    92,     7,     8,     9,    95,
      96,    97,    91,    98,    93,    21,    99,    18,    24,     0,
      85
};

static const yytype_int8 yycheck[] =
{
      23,    60,     0,    58,    19,    60,     4,     5,     6,    24,
       8,    24,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     4,     5,    22,    23,     8,    88,
      10,    11,    19,    13,    14,    15,    16,    17,    18,     7,
      20,     5,    65,    23,     8,    25,    18,    19,    22,    23,
      14,    22,    16,    17,    10,    18,    20,     5,     6,    23,
       8,     3,    10,    11,    12,    19,    14,    19,    16,    17,
      18,    19,    20,    21,     5,    19,    19,     8,    19,     5,
      19,    19,     8,    14,    18,    16,    17,    23,    14,    20,
      16,    17,     6,    25,    20,    19,    10,    11,    12,    19,
      19,    23,    23,    19,    18,     1,    19,    21,     1,    -1,
      61
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    27,     0,     4,     5,     6,     8,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    28,
      29,    30,    35,    36,    37,    38,    39,    40,    41,    44,
      45,    46,    47,    24,    19,     5,     8,    14,    16,    17,
      20,     4,     5,     8,    10,    11,    13,    14,    15,    16,
      17,    18,    20,    23,    25,    42,     7,    22,    32,    23,
      32,    10,     3,    19,    24,    19,    30,    43,    19,    18,
      19,    18,    19,    19,    19,    18,    19,    31,    33,    34,
      37,    44,    19,    31,    44,    43,    23,    19,    32,    18,
      25,    23,    19,    18,    31,    19,    19,    23,    19,    19
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
        case 4:
#line 184 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyHaveTime++;
	}
    break;

  case 5:
#line 187 "./parsedate.y"
    {
	        ((struct date_yy *)parm)->yyHaveZone++;
	}
    break;

  case 6:
#line 190 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyHaveDate++;
	}
    break;

  case 7:
#line 193 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyHaveDay++;
	}
    break;

  case 8:
#line 196 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyHaveRel++;
	}
    break;

  case 11:
#line 203 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyHour = (yyvsp[(1) - (2)].Number);
	    ((struct date_yy *)parm)->yyMinutes = 0;
	    ((struct date_yy *)parm)->yySeconds = 0;
	    ((struct date_yy *)parm)->yyMeridian = (yyvsp[(2) - (2)].Meridian);
	}
    break;

  case 13:
#line 213 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMeridian = MER24;
	}
    break;

  case 14:
#line 216 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMeridian = MER24;
	    ((struct date_yy *)parm)->yySeconds = 0;
	}
    break;

  case 15:
#line 220 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMeridian = MER24;
	}
    break;

  case 16:
#line 223 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMeridian = MER24;
	    ((struct date_yy *)parm)->yySeconds = 0;
	}
    break;

  case 17:
#line 229 "./parsedate.y"
    {
		((struct date_yy *)parm)->yyHaveZone++;
	}
    break;

  case 18:
#line 232 "./parsedate.y"
    {
		((struct date_yy *)parm)->yyHaveZone++;
	}
    break;

  case 19:
#line 235 "./parsedate.y"
    {
		((struct date_yy *)parm)->yyHaveZone++;
	}
    break;

  case 21:
#line 241 "./parsedate.y"
    {
	}
    break;

  case 23:
#line 246 "./parsedate.y"
    {
		/* format: [+-]hhmm */
		if ((yyvsp[(1) - (1)].Number) <= -100 || (yyvsp[(1) - (1)].Number) >= 100) {
			((struct date_yy *)parm)->yyTimezone = (-(yyvsp[(1) - (1)].Number) / 100) * 60 + (-(yyvsp[(1) - (1)].Number) % 100);
		} else if ((yyvsp[(1) - (1)].Number) >= -99 || (yyvsp[(1) - (1)].Number) <= 99) {
			((struct date_yy *)parm)->yyTimezone = -(yyvsp[(1) - (1)].Number) * 60;
		}
	}
    break;

  case 24:
#line 256 "./parsedate.y"
    {
		/* format: [+-]hh:mm */
		((struct date_yy *)parm)->yyTimezone = -(yyvsp[(1) - (3)].Number) * 60 + ((yyvsp[(1) - (3)].Number) > 0 ? -(yyvsp[(3) - (3)].Number): (yyvsp[(3) - (3)].Number));
	}
    break;

  case 25:
#line 262 "./parsedate.y"
    {
		/* format: hh:mm:ss */
	    ((struct date_yy *)parm)->yySeconds = (yyvsp[(3) - (3)].Number);
	}
    break;

  case 26:
#line 268 "./parsedate.y"
    {
		/* format: hh:mm */
	    ((struct date_yy *)parm)->yyHour = (yyvsp[(1) - (3)].Number);
	    ((struct date_yy *)parm)->yyMinutes = (yyvsp[(3) - (3)].Number);
	}
    break;

  case 27:
#line 284 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyTimezone = (yyvsp[(1) - (1)].Number);
	}
    break;

  case 28:
#line 287 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyTimezone = (yyvsp[(1) - (1)].Number);
	}
    break;

  case 29:
#line 290 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyTimezone = (yyvsp[(1) - (1)].Number);
	}
    break;

  case 30:
#line 293 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyTimezone = (yyvsp[(1) - (1)].Number);
	}
    break;

  case 31:
#line 296 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyTimezone = (yyvsp[(1) - (1)].Number) - 60;
	}
    break;

  case 32:
#line 299 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyTimezone = (yyvsp[(1) - (2)].Number) - 60;
	}
    break;

  case 33:
#line 304 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyDayOrdinal = 1;
	    ((struct date_yy *)parm)->yyDayNumber = (yyvsp[(1) - (1)].Number);
	}
    break;

  case 34:
#line 308 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyDayOrdinal = 1;
	    ((struct date_yy *)parm)->yyDayNumber = (yyvsp[(1) - (2)].Number);
	}
    break;

  case 35:
#line 312 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyDayOrdinal = (yyvsp[(1) - (2)].Number);
	    ((struct date_yy *)parm)->yyDayNumber = (yyvsp[(2) - (2)].Number);
	}
    break;

  case 36:
#line 318 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(1) - (3)].Number);
	    ((struct date_yy *)parm)->yyDay = (yyvsp[(3) - (3)].Number);
	}
    break;

  case 37:
#line 322 "./parsedate.y"
    {
		((struct date_yy *)parm)->yyYear = (yyvsp[(8) - (8)].Number);
		((struct date_yy *)parm)->yyMonth = (yyvsp[(1) - (8)].Number);
		((struct date_yy *)parm)->yyDay = (yyvsp[(2) - (8)].Number);

		((struct date_yy *)parm)->yyHour = (yyvsp[(3) - (8)].Number);
		((struct date_yy *)parm)->yyMinutes = (yyvsp[(5) - (8)].Number);
		((struct date_yy *)parm)->yySeconds = (yyvsp[(7) - (8)].Number);

		((struct date_yy *)parm)->yyHaveTime = 1;
	}
    break;

  case 38:
#line 333 "./parsedate.y"
    {
	  /* Interpret as YYYY/MM/DD if $1 >= 1000, otherwise as MM/DD/YY.
	     The goal in recognizing YYYY/MM/DD is solely to support legacy
	     machine-generated dates like those in an RCS log listing.  If
	     you want portability, use the ISO 8601 format.  */
	  if ((yyvsp[(1) - (5)].Number) >= 1000)
	    {
	      ((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (5)].Number);
	      ((struct date_yy *)parm)->yyMonth = (yyvsp[(3) - (5)].Number);
	      ((struct date_yy *)parm)->yyDay = (yyvsp[(5) - (5)].Number);
	    }
	  else
	    {
	      ((struct date_yy *)parm)->yyMonth = (yyvsp[(1) - (5)].Number);
	      ((struct date_yy *)parm)->yyDay = (yyvsp[(3) - (5)].Number);
	      ((struct date_yy *)parm)->yyYear = (yyvsp[(5) - (5)].Number);
	    }
	}
    break;

  case 40:
#line 352 "./parsedate.y"
    {
			((struct date_yy *)parm)->yyHaveTime++;
    }
    break;

  case 41:
#line 355 "./parsedate.y"
    {
	    /* e.g. 17-JUN-1992.  */
	    ((struct date_yy *)parm)->yyDay = (yyvsp[(1) - (3)].Number);
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(2) - (3)].Number);
	    ((struct date_yy *)parm)->yyYear = -(yyvsp[(3) - (3)].Number);
	}
    break;

  case 42:
#line 361 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(1) - (3)].Number);
	    ((struct date_yy *)parm)->yyDay = (yyvsp[(2) - (3)].Number);
		((struct date_yy *)parm)->yyYear = (yyvsp[(3) - (3)].Number);
	}
    break;

  case 43:
#line 366 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(1) - (2)].Number);
	    if ((yyvsp[(2) - (2)].Number) > 1000) {
		((struct date_yy *)parm)->yyYear = (yyvsp[(2) - (2)].Number);
	    } else {
		((struct date_yy *)parm)->yyDay = (yyvsp[(2) - (2)].Number);
	    }
	}
    break;

  case 44:
#line 374 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(1) - (4)].Number);
	    ((struct date_yy *)parm)->yyDay = (yyvsp[(2) - (4)].Number);
	    ((struct date_yy *)parm)->yyYear = (yyvsp[(4) - (4)].Number);
	}
    break;

  case 45:
#line 379 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(2) - (2)].Number);
	    if ((yyvsp[(1) - (2)].Number) > 1000) {
		((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (2)].Number);
	    } else {
		((struct date_yy *)parm)->yyDay = (yyvsp[(1) - (2)].Number);
	    }
	}
    break;

  case 46:
#line 387 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyMonth = (yyvsp[(2) - (3)].Number);
	    ((struct date_yy *)parm)->yyDay = (yyvsp[(1) - (3)].Number);
	    ((struct date_yy *)parm)->yyYear = (yyvsp[(3) - (3)].Number);
	}
    break;

  case 48:
#line 395 "./parsedate.y"
    {
		int i = (yyvsp[(1) - (3)].Number);

		if (i >= 10000) {
			/* format: yyyymmdd */
			((struct date_yy *)parm)->yyYear = i / 10000;
			i %= 10000;
			((struct date_yy *)parm)->yyMonth = i / 100;
			i %= 100;
			((struct date_yy *)parm)->yyDay = i;
		} else if (i >= 1000 && i <= 9999) {
			/* format: yyyy */
			((struct date_yy *)parm)->yyYear = i;
			((struct date_yy *)parm)->yyDay= 1;
			((struct date_yy *)parm)->yyMonth = 1;
		}
	}
    break;

  case 49:
#line 414 "./parsedate.y"
    {
	    /* ISO 8601 format.  yyyy-mm-dd.  */
	    ((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (3)].Number);
	    ((struct date_yy *)parm)->yyMonth = -(yyvsp[(2) - (3)].Number);
	    ((struct date_yy *)parm)->yyDay = -(yyvsp[(3) - (3)].Number);
	}
    break;

  case 50:
#line 420 "./parsedate.y"
    {
		/* ISO 8601 format   yyyy-mm */
	    ((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (2)].Number);
	    ((struct date_yy *)parm)->yyMonth = -(yyvsp[(2) - (2)].Number);
	    ((struct date_yy *)parm)->yyDay = 1;
	}
    break;

  case 51:
#line 426 "./parsedate.y"
    {
		const int om = (1 + 9) % 12; /* offset month */
		const int oy = (yyvsp[(1) - (2)].Number) - 1; /* offset year */

		((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (2)].Number);
		((struct date_yy *)parm)->yyMonth = 1;
		/* Zeller's formula */
		((struct date_yy *)parm)->yyDay -= ((13 * om + 12) / 5 +
					oy + oy / 4 + oy / 400 - oy / 100) % 7 - 1;
	}
    break;

  case 52:
#line 438 "./parsedate.y"
    {
		((struct date_yy *)parm)->yyDay = ((yyvsp[(2) - (2)].Number) / 10) * 7 + ((yyvsp[(2) - (2)].Number) % 10) - 8;
	}
    break;

  case 53:
#line 441 "./parsedate.y"
    {
		((struct date_yy *)parm)->yyDay = (yyvsp[(2) - (3)].Number) * 7 - (yyvsp[(3) - (3)].Number) - 8;
	}
    break;

  case 55:
#line 448 "./parsedate.y"
    {
		int i = (yyvsp[(1) - (3)].Number);

		if (i <= -100000 || i >= 100000) {
			((struct date_yy *)parm)->yyHour = i / 10000;
			i %= 10000;
			((struct date_yy *)parm)->yyMinutes = i / 100;
			i %= 100;
	    	((struct date_yy *)parm)->yySeconds = i;
		} else if (i <= -1000 || i >= 1000) {
			((struct date_yy *)parm)->yyHour = i / 100;
			i %= 100;
			((struct date_yy *)parm)->yyMinutes = i;
	    	((struct date_yy *)parm)->yySeconds = 0;
		} else if (i >= -99 || i <= 99) {
			((struct date_yy *)parm)->yyHour = (yyvsp[(1) - (3)].Number);
			((struct date_yy *)parm)->yyMinutes = 0;
	    	((struct date_yy *)parm)->yySeconds = 0;
		} else {
			((struct date_yy *)parm)->yyHaveTime = 0;
		}
	    ((struct date_yy *)parm)->yyMeridian = MER24;
	}
    break;

  case 56:
#line 473 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelSeconds =
			-((struct date_yy *)parm)->yyRelSeconds;
	    ((struct date_yy *)parm)->yyRelMinutes =
			-((struct date_yy *)parm)->yyRelMinutes;
	    ((struct date_yy *)parm)->yyRelHour =
			-((struct date_yy *)parm)->yyRelHour;
	    ((struct date_yy *)parm)->yyRelDay =
			-((struct date_yy *)parm)->yyRelDay;
	    ((struct date_yy *)parm)->yyRelMonth =
			-((struct date_yy *)parm)->yyRelMonth;
	    ((struct date_yy *)parm)->yyRelYear =
			-((struct date_yy *)parm)->yyRelYear;
	}
    break;

  case 58:
#line 490 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelYear += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 59:
#line 493 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelYear += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 60:
#line 496 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelYear += (yyvsp[(1) - (1)].Number);
	}
    break;

  case 61:
#line 499 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelMonth += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 62:
#line 502 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelMonth += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 63:
#line 505 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelMonth += (yyvsp[(1) - (1)].Number);
	}
    break;

  case 64:
#line 508 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelDay += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 65:
#line 511 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelDay += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 66:
#line 514 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelDay += (yyvsp[(1) - (1)].Number);
	}
    break;

  case 67:
#line 517 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelHour += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 68:
#line 520 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelHour += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 69:
#line 523 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelHour += (yyvsp[(1) - (1)].Number);
	}
    break;

  case 70:
#line 526 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelMinutes += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 71:
#line 529 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelMinutes += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 72:
#line 532 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelMinutes += (yyvsp[(1) - (1)].Number);
	}
    break;

  case 73:
#line 535 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelSeconds += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 74:
#line 538 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelSeconds += (yyvsp[(1) - (2)].Number) * (yyvsp[(2) - (2)].Number);
	}
    break;

  case 75:
#line 541 "./parsedate.y"
    {
	    ((struct date_yy *)parm)->yyRelSeconds += (yyvsp[(1) - (1)].Number);
	}
    break;

  case 76:
#line 547 "./parsedate.y"
    {
	    if (((struct date_yy *)parm)->yyHaveTime && 
			((struct date_yy *)parm)->yyHaveDate && 
			!((struct date_yy *)parm)->yyHaveRel)
	      ((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (1)].Number);
	    else
	      {
		if ((yyvsp[(1) - (1)].Number)>10000)
		  {
		    ((struct date_yy *)parm)->yyHaveDate++;
		    ((struct date_yy *)parm)->yyDay= ((yyvsp[(1) - (1)].Number))%100;
		    ((struct date_yy *)parm)->yyMonth= ((yyvsp[(1) - (1)].Number)/100)%100;
		    ((struct date_yy *)parm)->yyYear = (yyvsp[(1) - (1)].Number)/10000;
		  }
		else
		  {
		    ((struct date_yy *)parm)->yyHaveTime++;
		    if ((yyvsp[(1) - (1)].Number) < 100)
		      {
			((struct date_yy *)parm)->yyHour = (yyvsp[(1) - (1)].Number);
			((struct date_yy *)parm)->yyMinutes = 0;
		      }
		    else
		      {
		    	((struct date_yy *)parm)->yyHour = (yyvsp[(1) - (1)].Number) / 100;
		    	((struct date_yy *)parm)->yyMinutes = (yyvsp[(1) - (1)].Number) % 100;
		      }
		    ((struct date_yy *)parm)->yySeconds = 0;
		    ((struct date_yy *)parm)->yyMeridian = MER24;
		  }
	      }
	  }
    break;

  case 77:
#line 582 "./parsedate.y"
    {
			 ((struct date_yy *)parm)->yyMeridian = (yyvsp[(1) - (1)].Meridian);
		  }
    break;


/* Line 1267 of yacc.c.  */
#line 2214 "parsedate.c"
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


#line 587 "./parsedate.y"


time_t get_date (char *p, time_t *now);

/* Month and day table. */
static TABLE const MonthDayTable[] = {
    { "january",	tMONTH,  1 },
    { "february",	tMONTH,  2 },
    { "march",		tMONTH,  3 },
    { "april",		tMONTH,  4 },
    { "may",		tMONTH,  5 },
    { "june",		tMONTH,  6 },
    { "july",		tMONTH,  7 },
    { "august",		tMONTH,  8 },
    { "september",	tMONTH,  9 },
    { "sept",		tMONTH,  9 },
    { "october",	tMONTH, 10 },
    { "november",	tMONTH, 11 },
    { "december",	tMONTH, 12 },
    { "sunday",		tDAY, 0 },
    { "monday",		tDAY, 1 },
    { "tuesday",	tDAY, 2 },
    { "tues",		tDAY, 2 },
    { "wednesday",	tDAY, 3 },
    { "wednes",		tDAY, 3 },
    { "thursday",	tDAY, 4 },
    { "thur",		tDAY, 4 },
    { "thurs",		tDAY, 4 },
    { "friday",		tDAY, 5 },
    { "saturday",	tDAY, 6 },
    { NULL, 0, 0 }
};

/* Time units table. */
static TABLE const UnitsTable[] = {
    { "year",		tYEAR_UNIT,	1 },
    { "month",		tMONTH_UNIT,	1 },
    { "fortnight",	tDAY_UNIT,	14 },
    { "week",		tDAY_UNIT,	7 },
    { "day",		tDAY_UNIT,	1 },
    { "hour",		tHOUR_UNIT,	1 },
    { "minute",		tMINUTE_UNIT,	1 },
    { "min",		tMINUTE_UNIT,	1 },
    { "second",		tSEC_UNIT,	1 },
    { "sec",		tSEC_UNIT,	1 },
    { NULL, 0, 0 }
};

/* Assorted relative-time words. */
static TABLE const OtherTable[] = {
    { "tomorrow",	tDAY_UNIT,	1 },
    { "yesterday",	tDAY_UNIT,	-1 },
    { "today",		tDAY_UNIT,	0 },
    { "now",		tDAY_UNIT,	0 },
    { "last",		tUNUMBER,	-1 },
    { "this",		tUNUMBER,	0 },
    { "next",		tUNUMBER,	1 },
    { "first",		tUNUMBER,	1 },
/*  { "second",		tUNUMBER,	2 }, */
    { "third",		tUNUMBER,	3 },
    { "fourth",		tUNUMBER,	4 },
    { "fifth",		tUNUMBER,	5 },
    { "sixth",		tUNUMBER,	6 },
    { "seventh",	tUNUMBER,	7 },
    { "eighth",		tUNUMBER,	8 },
    { "ninth",		tUNUMBER,	9 },
    { "tenth",		tUNUMBER,	10 },
    { "eleventh",	tUNUMBER,	11 },
    { "twelfth",	tUNUMBER,	12 },
    { "ago",		tAGO,	1 },
    { NULL, 0, 0 }
};

/* The timezone table. */
static TABLE const TimezoneTable[] = {
    { "gmt",	tZONE,     HOUR ( 0) },	/* Greenwich Mean */
    { "ut",	tZONE,     HOUR ( 0) },	/* Universal (Coordinated) */
    { "utc",	tZONE,     HOUR ( 0) },
    { "wet",	tZONE,     HOUR ( 0) },	/* Western European */
    { "bst",	tDAYZONE,  HOUR ( 0) },	/* British Summer */
    { "wat",	tZONE,     HOUR ( 1) },	/* West Africa */
    { "at",	tZONE,     HOUR ( 2) },	/* Azores */
#if	0
    /* For completeness.  BST is also British Summer, and GST is
     * also Guam Standard. */
    { "bst",	tZONE,     HOUR ( 3) },	/* Brazil Standard */
    { "gst",	tZONE,     HOUR ( 3) },	/* Greenland Standard */
#endif
#if 0
    { "nft",	tZONE,     HOUR (3.5) },	/* Newfoundland */
    { "nst",	tZONE,     HOUR (3.5) },	/* Newfoundland Standard */
    { "ndt",	tDAYZONE,  HOUR (3.5) },	/* Newfoundland Daylight */
#endif
    { "ast",	tZONE,     HOUR ( 4) },	/* Atlantic Standard */
    { "adt",	tDAYZONE,  HOUR ( 4) },	/* Atlantic Daylight */
    { "est",	tZONE,     HOUR ( 5) },	/* Eastern Standard */
    { "edt",	tDAYZONE,  HOUR ( 5) },	/* Eastern Daylight */
    { "cst",	tZONE,     HOUR ( 6) },	/* Central Standard */
    { "cdt",	tDAYZONE,  HOUR ( 6) },	/* Central Daylight */
    { "mst",	tZONE,     HOUR ( 7) },	/* Mountain Standard */
    { "mdt",	tDAYZONE,  HOUR ( 7) },	/* Mountain Daylight */
    { "pst",	tZONE,     HOUR ( 8) },	/* Pacific Standard */
    { "pdt",	tDAYZONE,  HOUR ( 8) },	/* Pacific Daylight */
    { "yst",	tZONE,     HOUR ( 9) },	/* Yukon Standard */
    { "ydt",	tDAYZONE,  HOUR ( 9) },	/* Yukon Daylight */
    { "hst",	tZONE,     HOUR (10) },	/* Hawaii Standard */
    { "hdt",	tDAYZONE,  HOUR (10) },	/* Hawaii Daylight */
    { "cat",	tZONE,     HOUR (10) },	/* Central Alaska */
    { "akst",	tZONE,     HOUR (10) }, /* Alaska Standard */
    { "akdt",	tZONE,     HOUR (10) }, /* Alaska Daylight */
    { "ahst",	tZONE,     HOUR (10) },	/* Alaska-Hawaii Standard */
    { "nt",	tZONE,     HOUR (11) },	/* Nome */
    { "idlw",	tZONE,     HOUR (12) },	/* International Date Line West */
    { "cet",	tZONE,     -HOUR (1) },	/* Central European */
    { "cest",	tDAYZONE,  -HOUR (1) },	/* Central European Summer */
    { "met",	tZONE,     -HOUR (1) },	/* Middle European */
    { "mewt",	tZONE,     -HOUR (1) },	/* Middle European Winter */
    { "mest",	tDAYZONE,  -HOUR (1) },	/* Middle European Summer */
    { "mesz",	tDAYZONE,  -HOUR (1) },	/* Middle European Summer */
    { "swt",	tZONE,     -HOUR (1) },	/* Swedish Winter */
    { "sst",	tDAYZONE,  -HOUR (1) },	/* Swedish Summer */
    { "fwt",	tZONE,     -HOUR (1) },	/* French Winter */
    { "fst",	tDAYZONE,  -HOUR (1) },	/* French Summer */
    { "eet",	tZONE,     -HOUR (2) },	/* Eastern Europe, USSR Zone 1 */
    { "bt",	tZONE,     -HOUR (3) },	/* Baghdad, USSR Zone 2 */
#if 0
    { "it",	tZONE,     -HOUR (3.5) },/* Iran */
#endif
    { "zp4",	tZONE,     -HOUR (4) },	/* USSR Zone 3 */
    { "zp5",	tZONE,     -HOUR (5) },	/* USSR Zone 4 */
#if 0
    { "ist",	tZONE,     -HOUR (5.5) },/* Indian Standard */
#endif
    { "zp6",	tZONE,     -HOUR (6) },	/* USSR Zone 5 */
#if	0
    /* For completeness.  NST is also Newfoundland Standard, and SST is
     * also Swedish Summer. */
    { "nst",	tZONE,     -HOUR (6.5) },/* North Sumatra */
    { "sst",	tZONE,     -HOUR (7) },	/* South Sumatra, USSR Zone 6 */
#endif	/* 0 */
    { "wast",	tZONE,     -HOUR (7) },	/* West Australian Standard */
    { "wadt",	tDAYZONE,  -HOUR (7) },	/* West Australian Daylight */
#if 0
    { "jt",	tZONE,     -HOUR (7.5) },/* Java (3pm in Cronusland!) */
#endif
    { "cct",	tZONE,     -HOUR (8) },	/* China Coast, USSR Zone 7 */
    { "jst",	tZONE,     -HOUR (9) },	/* Japan Standard, USSR Zone 8 */
#if 0
    { "cast",	tZONE,     -HOUR (9.5) },/* Central Australian Standard */
    { "cadt",	tDAYZONE,  -HOUR (9.5) },/* Central Australian Daylight */
#endif
    { "east",	tZONE,     -HOUR (10) },	/* Eastern Australian Standard */
    { "eadt",	tDAYZONE,  -HOUR (10) },	/* Eastern Australian Daylight */
    { "gst",	tZONE,     -HOUR (10) },	/* Guam Standard, USSR Zone 9 */
    { "nzt",	tZONE,     -HOUR (12) },	/* New Zealand */
    { "nzst",	tZONE,     -HOUR (12) },	/* New Zealand Standard */
    { "nzdt",	tDAYZONE,  -HOUR (12) },	/* New Zealand Daylight */
    { "idle",	tZONE,     -HOUR (12) },	/* International Date Line East */
    {  NULL, 0, 0  }
};

/* Military timezone table. */
static TABLE const MilitaryTable[] = {
    { "a",	tZONE,	HOUR (- 1) },
    { "b",	tZONE,	HOUR (- 2) },
    { "c",	tZONE,	HOUR (- 3) },
    { "d",	tZONE,	HOUR (- 4) },
    { "e",	tZONE,	HOUR (- 5) },
    { "f",	tZONE,	HOUR (- 6) },
    { "g",	tZONE,	HOUR (- 7) },
    { "h",	tZONE,	HOUR (- 8) },
    { "i",	tZONE,	HOUR (- 9) },
    { "k",	tZONE,	HOUR (-10) },
    { "l",	tZONE,	HOUR (-11) },
    { "m",	tZONE,	HOUR (-12) },
    { "n",	tZONE,	HOUR (  1) },
    { "o",	tZONE,	HOUR (  2) },
    { "p",	tZONE,	HOUR (  3) },
    { "q",	tZONE,	HOUR (  4) },
    { "r",	tZONE,	HOUR (  5) },
    { "s",	tZONE,	HOUR (  6) },
    { "t",	tTZONE,	HOUR (  7) },
    { "u",	tZONE,	HOUR (  8) },
    { "v",	tZONE,	HOUR (  9) },
    { "w",	tWZONE,	HOUR ( 10) },
    { "x",	tZONE,	HOUR ( 11) },
    { "y",	tZONE,	HOUR ( 12) },
    { "z",	tZZONE,	HOUR (  0) },
    { NULL, 0, 0 }
};




/* ARGSUSED */
static int
yyerror(const char *s)
{
  return 0;
}

static int
ToHour(int Hours, MERIDIAN Meridian)
{
  switch (Meridian)
    {
    case MER24:
      if (Hours < 0 || Hours > 23)
	return -1;
      return Hours;
    case MERam:
      if (Hours < 1 || Hours > 12)
	return -1;
      if (Hours == 12)
	Hours = 0;
      return Hours;
    case MERpm:
      if (Hours < 1 || Hours > 12)
	return -1;
      if (Hours == 12)
	Hours = 0;
      return Hours + 12;
    default:
      abort ();
    }
  /* NOTREACHED */
}

static int
ToYear(int Year)
{
  if (Year < 0)
    Year = -Year;

  /* XPG4 suggests that years 00-68 map to 2000-2068, and
     years 69-99 map to 1969-1999.  */
  if (Year < 69)
    Year += 2000;
  else if (Year < 100)
    Year += 1900;

  return Year;
}

static int
LookupWord (YYSTYPE *lvalp, char *buff)
{
  char *p;
  char *q;
  const TABLE *tp;
  int i;
  int abbrev;

  /* Make it lowercase. */
  for (p = buff; *p; p++)
    if (ISUPPER ((unsigned char) *p))
      *p = tolower (*p);

  if (strcmp (buff, "am") == 0 || strcmp (buff, "a.m.") == 0)
    {
      lvalp->Meridian = MERam;
      return tMERIDIAN;
    }
  if (strcmp (buff, "pm") == 0 || strcmp (buff, "p.m.") == 0)
    {
      lvalp->Meridian = MERpm;
      return tMERIDIAN;
    }

  /* See if we have an abbreviation for a month. */
  if (strlen (buff) == 3)
    abbrev = 1;
  else if (strlen (buff) == 4 && buff[3] == '.')
    {
      abbrev = 1;
      buff[3] = '\0';
    }
  else
    abbrev = 0;

  for (tp = MonthDayTable; tp->name; tp++)
    {
      if (abbrev)
	{
	  if (strncmp (buff, tp->name, 3) == 0)
	    {
	      lvalp->Number = tp->value;
	      return tp->type;
	    }
	}
      else if (strcmp (buff, tp->name) == 0)
	{
	  lvalp->Number = tp->value;
	  return tp->type;
	}
    }

  for (tp = TimezoneTable; tp->name; tp++)
    if (strcmp (buff, tp->name) == 0)
      {
	lvalp->Number = tp->value;
	return tp->type;
      }

  if (strcmp (buff, "dst") == 0)
    return tDST;

  for (tp = UnitsTable; tp->name; tp++)
    if (strcmp (buff, tp->name) == 0)
      {
	lvalp->Number = tp->value;
	return tp->type;
      }

  /* Strip off any plural and try the units table again. */
  i = strlen (buff) - 1;
  if (buff[i] == 's')
    {
      buff[i] = '\0';
      for (tp = UnitsTable; tp->name; tp++)
	if (strcmp (buff, tp->name) == 0)
	  {
	    lvalp->Number = tp->value;
	    return tp->type;
	  }
      buff[i] = 's';		/* Put back for "this" in OtherTable. */
    }

  for (tp = OtherTable; tp->name; tp++)
    if (strcmp (buff, tp->name) == 0)
      {
	lvalp->Number = tp->value;
	return tp->type;
      }

  /* Military timezones. */
  if (buff[1] == '\0' && ISALPHA ((unsigned char) *buff))
    {
      for (tp = MilitaryTable; tp->name; tp++)
	if (strcmp (buff, tp->name) == 0)
	  {
	    lvalp->Number = tp->value;
	    return tp->type;
	  }
    }

  /* Drop out any periods and try the timezone table again. */
  for (i = 0, p = q = buff; *q; q++)
    if (*q != '.')
      *p++ = *q;
    else
      i++;
  *p = '\0';
  if (i)
    for (tp = TimezoneTable; tp->name; tp++)
      if (strcmp (buff, tp->name) == 0)
	{
	  lvalp->Number = tp->value;
	  return tp->type;
	}

  return tID;
}

int yylex(YYSTYPE *lvalp, void *parm)
{
  unsigned char c;
  char *p;
  char buff[20];
  int Count;
  int sign;
  struct date_yy * date = (struct date_yy *)parm;

  for (;;)
    {
      while (ISSPACE ((unsigned char) *date->yyInput))
	date->yyInput++;

      if (ISDIGIT (c = *date->yyInput) || c == '-' || c == '+')
	{
	  if (c == '-' || c == '+')
	    {
	      sign = c == '-' ? -1 : 1;
	      if (!ISDIGIT (*++date->yyInput))
		/* skip the '-' sign */
		continue;
	    }
	  else
	    sign = 0;
	  for (lvalp->Number = 0; ISDIGIT (c = *date->yyInput++);)
	    lvalp->Number = 10 * lvalp->Number + c - '0';
	  date->yyInput--;
	  if (sign < 0)
	    lvalp->Number = -lvalp->Number;
	  /* Ignore ordinal suffixes on numbers */
	  c = *date->yyInput;
	  if (c == 's' || c == 'n' || c == 'r' || c == 't') {
	    c = *++date->yyInput;
	    if (c == 't' || c == 'd' || c == 'h') {
	      date->yyInput++;
	    } else {
	      date->yyInput--;
	    }
	  }
	  return sign ? tSNUMBER : tUNUMBER;
	}
      if (ISALPHA (c))
	{
	  for (p = buff; (c = *date->yyInput++, ISALPHA (c)) || c == '.';)
	    if (p < &buff[sizeof buff - 1])
	      *p++ = c;
	  *p = '\0';
	  date->yyInput--;
	  return LookupWord (lvalp, buff);
	}
      if (c != '(')
	return *date->yyInput++;
      Count = 0;
      do
	{
	  c = *date->yyInput++;
	  if (c == '\0')
	    return c;
	  if (c == '(')
	    Count++;
	  else if (c == ')')
	    Count--;
	}
      while (Count > 0);
    }
}

#define TM_YEAR_ORIGIN 1900

/* Yield A - B, measured in seconds.  */
static long
difftm (struct tm *a, struct tm *b)
{
  int ay = a->tm_year + (TM_YEAR_ORIGIN - 1);
  int by = b->tm_year + (TM_YEAR_ORIGIN - 1);
  long days = (
  /* difference in day of year */
		a->tm_yday - b->tm_yday
  /* + intervening leap days */
		+ ((ay >> 2) - (by >> 2))
		- (ay / 100 - by / 100)
		+ ((ay / 100 >> 2) - (by / 100 >> 2))
  /* + difference in years * 365 */
		+ (long) (ay - by) * 365
  );
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
		+ (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

time_t raptor_parse_date(const char *p, time_t *now)
{
  struct tm tm, tm0, *tmp;
  time_t Start;
  struct date_yy date;

  date.yyInput = p;
  Start = now ? *now : time ((time_t *) NULL);
  tmp = localtime (&Start);
  if (!tmp)
    return -1;
  date.yyYear = tmp->tm_year + TM_YEAR_ORIGIN;
  date.yyMonth = tmp->tm_mon + 1;
  date.yyDay = tmp->tm_mday;
  date.yyHour = tmp->tm_hour;
  date.yyMinutes = tmp->tm_min;
  date.yySeconds = tmp->tm_sec;
  tm.tm_isdst = tmp->tm_isdst;
  date.yyMeridian = MER24;
  date.yyRelSeconds = 0;
  date.yyRelMinutes = 0;
  date.yyRelHour = 0;
  date.yyRelDay = 0;
  date.yyRelMonth = 0;
  date.yyRelYear = 0;
  date.yyHaveDate = 0;
  date.yyHaveDay = 0;
  date.yyHaveRel = 0;
  date.yyHaveTime = 0;
  date.yyHaveZone = 0;

  if (yyparse ((void *)&date)
      || date.yyHaveTime > 1 || date.yyHaveZone > 1 
	  || date.yyHaveDate > 1 || date.yyHaveDay > 1) {
    return -1;
  }
  tm.tm_year = ToYear (date.yyYear) - TM_YEAR_ORIGIN + date.yyRelYear;
  tm.tm_mon = date.yyMonth - 1 + date.yyRelMonth;
  tm.tm_mday = date.yyDay + date.yyRelDay;
  if (date.yyHaveTime || (date.yyHaveRel && !date.yyHaveDate && !date.yyHaveDay))
    {
      tm.tm_hour = ToHour (date.yyHour, date.yyMeridian);
      if (tm.tm_hour < 0)
	return -1;
      tm.tm_min = date.yyMinutes;
      tm.tm_sec = date.yySeconds;
    }
  else
    {
      tm.tm_hour = tm.tm_min = tm.tm_sec = 0;
    }
  tm.tm_hour += date.yyRelHour;
  tm.tm_min += date.yyRelMinutes;
  tm.tm_sec += date.yyRelSeconds;

  /* Let mktime deduce tm_isdst if we have an absolute timestamp,
     or if the relative timestamp mentions days, months, or years.  */
  if (date.yyHaveDate | date.yyHaveDay | date.yyHaveTime | date.yyRelDay | date.yyRelMonth | date.yyRelYear)
    tm.tm_isdst = -1;

  tm0 = tm;

  Start = mktime (&tm);

  if (Start == (time_t) -1)
    {

      /* Guard against falsely reporting errors near the time_t boundaries
         when parsing times in other time zones.  For example, if the min
         time_t value is 1970-01-01 00:00:00 UTC and we are 8 hours ahead
         of UTC, then the min localtime value is 1970-01-01 08:00:00; if
         we apply mktime to 1970-01-01 00:00:00 we will get an error, so
         we apply mktime to 1970-01-02 08:00:00 instead and adjust the time
         zone by 24 hours to compensate.  This algorithm assumes that
         there is no DST transition within a day of the time_t boundaries.  */
      if (date.yyHaveZone)
	{
	  tm = tm0;
	  if (tm.tm_year <= EPOCH - TM_YEAR_ORIGIN)
	    {
	      tm.tm_mday++;
	      date.yyTimezone -= 24 * 60;
	    }
	  else
	    {
	      tm.tm_mday--;
	      date.yyTimezone += 24 * 60;
	    }
	  Start = mktime (&tm);
	}

      if (Start == (time_t) -1)
	return Start;
    }

  if (date.yyHaveDay && !date.yyHaveDate)
    {
      tm.tm_mday += ((date.yyDayNumber - tm.tm_wday + 7) % 7
		     + 7 * (date.yyDayOrdinal - (0 < date.yyDayOrdinal)));
      Start = mktime (&tm);
      if (Start == (time_t) -1)
	return Start;
    }

  if (date.yyHaveZone)
    {
      long delta;
      struct tm *gmt = gmtime (&Start);
      if (!gmt)
	return -1;
      delta = date.yyTimezone * 60L + difftm (&tm, gmt);

      if ((Start + delta < Start) != (delta < 0))
	return -1;		/* time_t overflow */
      Start += delta;
    }

  return Start;
}

