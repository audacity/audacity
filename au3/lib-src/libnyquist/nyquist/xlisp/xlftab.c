/* xlftab.c - xlisp function table */
/*	Copyright (c) 1985, by David Michael Betz
    All Rights Reserved
    Permission is granted for unrestricted non-commercial use

HISTORY
23-Apr-03   Mazzoni
        Eliminated some compiler warnings
 1-Apr-88	Dale Amon at CSD
        Added include file hook for local language extensions:
        localdefs.h and localptrs.h

*/

#include <string.h> /* for memcpy */
#include "switches.h"
#include "xlisp.h"

/* include system dependant definitions */
#include "osdefs.h"
#include "localdefs.h"

/* SUBR/FSUBR indicator */
#define S	SUBR
#define F	FSUBR

/* forward declarations */
LOCAL LVAL xnotimp(void);

/* debugging functions */
#ifdef DEBUG_INPUT
LVAL xstartrecordio(void);
LVAL xstoprecordio(void);
#endif

/* the function table */
FUNDEF init_funtab[] = {

    /* read macro functions */
{	NULL,				S, rmhash		}, /*   0 */
{	NULL,				S, rmquote		}, /*   1 */
{	NULL,				S, rmdquote		}, /*   2 */
{	NULL,				S, rmbquote		}, /*   3 */
{	NULL,				S, rmcomma		}, /*   4 */
{	NULL,				S, rmlpar		}, /*   5 */
{	NULL,				S, rmrpar		}, /*   6 */
{	NULL,				S, rmsemi		}, /*   7 */
{	NULL,				S, xnotimp		}, /*   8 */
{	NULL,				S, xnotimp		}, /*   9 */

    /* methods */
{	NULL,				S, clnew		}, /*  10 */
{	NULL,				S, clisnew		}, /*  11 */
{	NULL,				S, clanswer		}, /*  12 */
{	NULL,				S, obisnew		}, /*  13 */
{	NULL,				S, obclass		}, /*  14 */
{	NULL,				S, obshow		}, /*  15 */
{	NULL,				S, obisa		}, /*  16 */
{	NULL,				S, xnotimp		}, /*  17 */
{	NULL,				S, xnotimp		}, /*  18 */
{	NULL,				S, xnotimp		}, /*  19 */

    /* evaluator functions */
{	"EVAL",				S, xeval		}, /*  20 */
{	"APPLY",			S, xapply		}, /*  21 */
{	"FUNCALL",			S, xfuncall		}, /*  22 */
{	"QUOTE",			F, xquote		}, /*  23 */
{	"FUNCTION",			F, xfunction		}, /*  24 */
{	"BACKQUOTE",			F, xbquote		}, /*  25 */
{	"LAMBDA",			F, xlambda		}, /*  26 */

    /* symbol functions */
{	"SET",				S, xset			}, /*  27 */
{	"SETQ",				F, xsetq		}, /*  28 */
{	"SETF",				F, xsetf		}, /*  29 */
{	"DEFUN",			F, xdefun		}, /*  30 */
{	"DEFMACRO",			F, xdefmacro		}, /*  31 */
{	"GENSYM",			S, xgensym		}, /*  32 */
{	"MAKE-SYMBOL",			S, xmakesymbol		}, /*  33 */
{	"INTERN", 			S, xintern		}, /*  34 */
{	"SYMBOL-NAME",			S, xsymname		}, /*  35 */
{	"SYMBOL-VALUE",			S, xsymvalue		}, /*  36 */
{	"SYMBOL-PLIST",			S, xsymplist		}, /*  37 */
{	"GET",				S, xget			}, /*  38 */
{	"PUTPROP", 			S, xputprop		}, /*  39 */
{	"REMPROP",			S, xremprop		}, /*  40 */
{	"HASH",				S, xhash		}, /*  41 */

    /* array functions */
{	"MAKE-ARRAY",			S, xmkarray		}, /*  42 */
{	"AREF",				S, xaref		}, /*  43 */
            
    /* list functions */
{	"CAR",				S, xcar			}, /*  44 */
{	"CDR",				S, xcdr			}, /*  45 */
            
{	"CAAR",				S, xcaar		}, /*  46 */
{	"CADR",				S, xcadr		}, /*  47 */
{	"CDAR",				S, xcdar		}, /*  48 */
{	"CDDR",				S, xcddr		}, /*  49 */

{	"CAAAR",			S, xcaaar		}, /*  50 */
{	"CAADR",			S, xcaadr		}, /*  51 */
{	"CADAR",			S, xcadar		}, /*  52 */
{	"CADDR",			S, xcaddr		}, /*  53 */
{	"CDAAR",			S, xcdaar		}, /*  54 */
{	"CDADR",			S, xcdadr		}, /*  55 */
{	"CDDAR",			S, xcddar		}, /*  56 */
{	"CDDDR",			S, xcdddr		}, /*  57 */

{	"CAAAAR", 			S, xcaaaar		}, /*  58 */
{	"CAAADR",			S, xcaaadr		}, /*  59 */
{	"CAADAR",			S, xcaadar		}, /*  60 */
{	"CAADDR",			S, xcaaddr		}, /*  61 */
{	"CADAAR",		 	S, xcadaar		}, /*  62 */
{	"CADADR",			S, xcadadr		}, /*  63 */
{	"CADDAR",			S, xcaddar		}, /*  64 */
{	"CADDDR",			S, xcadddr		}, /*  65 */
{	"CDAAAR",			S, xcdaaar		}, /*  66 */
{	"CDAADR",			S, xcdaadr		}, /*  67 */
{	"CDADAR",			S, xcdadar		}, /*  68 */
{	"CDADDR",			S, xcdaddr		}, /*  69 */
{	"CDDAAR",			S, xcddaar		}, /*  70 */
{	"CDDADR",			S, xcddadr		}, /*  71 */
{	"CDDDAR",			S, xcdddar		}, /*  72 */
{	"CDDDDR",			S, xcddddr		}, /*  73 */

{	"CONS",				S, xcons		}, /*  74 */
{	"LIST",				S, xlist		}, /*  75 */
{	"APPEND",			S, xappend		}, /*  76 */
{	"REVERSE",			S, xreverse		}, /*  77 */
{	"LAST",				S, xlast		}, /*  78 */
{	"NTH",				S, xnth			}, /*  79 */
{	"NTHCDR",			S, xnthcdr		}, /*  80 */
{	"MEMBER",			S, xmember		}, /*  81 */
{	"ASSOC",			S, xassoc		}, /*  82 */
{	"SUBST", 			S, xsubst		}, /*  83 */
{	"SUBLIS",			S, xsublis		}, /*  84 */
{	"REMOVE",			S, xremove		}, /*  85 */
{	"LENGTH",			S, xlength		}, /*  86 */
{	"MAPC",				S, xmapc		}, /*  87 */
{	"MAPCAR",			S, xmapcar		}, /*  88 */
{	"MAPL",				S, xmapl		}, /*  89 */
{	"MAPLIST",			S, xmaplist		}, /*  90 */
            
    /* destructive list functions */
{	"RPLACA",			S, xrplca		}, /*  91 */
{	"RPLACD",			S, xrplcd		}, /*  92 */
{	"NCONC",			S, xnconc		}, /*  93 */
{	"DELETE",			S, xdelete		}, /*  94 */

    /* predicate functions */
{	"ATOM",				S, xatom		}, /*  95 */
{	"SYMBOLP",			S, xsymbolp		}, /*  96 */
{	"NUMBERP",			S, xnumberp		}, /*  97 */
{	"BOUNDP",			S, xboundp 		}, /*  98 */
{	"NULL",				S, xnull		}, /*  99 */
{	"LISTP",			S, xlistp		}, /* 100 */
{	"CONSP",			S, xconsp		}, /* 101 */
{	"MINUSP",			S, xminusp 		}, /* 102 */
{	"ZEROP",			S, xzerop		}, /* 103 */
{	"PLUSP",			S, xplusp		}, /* 104 */
{	"EVENP",			S, xevenp		}, /* 105 */
{	"ODDP",				S, xoddp		}, /* 106 */
{	"EQ",				S, xeq			}, /* 107 */
{	"EQL",				S, xeql			}, /* 108 */
{	"EQUAL",			S, xequal		}, /* 109 */

    /* special forms */
{	"COND",				F, xcond		}, /* 110 */
{	"CASE",				F, xcase		}, /* 111 */
{	"AND",				F, xand			}, /* 112 */
{	"OR",				F, x_or			}, /* 113 */
{	"LET",				F, xlet			}, /* 114 */
{	"LET*",				F, xletstar		}, /* 115 */
{	"IF",				F, xif			}, /* 116 */
{	"PROG",				F, xprog		}, /* 117 */
{	"PROG*",			F, xprogstar		}, /* 118 */
{	"PROG1",			F, xprog1		}, /* 119 */
{	"PROG2",			F, xprog2		}, /* 120 */
{	"PROGN",			F, xprogn		}, /* 121 */
{	"GO",				F, xgo			}, /* 122 */
{	"RETURN",			F, xreturn  		}, /* 123 */
{	"DO",				F, xdo			}, /* 124 */
{	"DO*",				F, xdostar  		}, /* 125 */
{	"DOLIST",			F, xdolist  		}, /* 126 */
{	"DOTIMES",			F, xdotimes		}, /* 127 */
{	"CATCH",			F, xcatch		}, /* 128 */
{	"THROW",			F, xthrow		}, /* 129 */
    
    /* debugging and error handling functions */
{	"ERROR",			S, xerror		}, /* 130 */
{	"CERROR",			S, xcerror  		}, /* 131 */
{	"BREAK",			S, xbreak		}, /* 132 */
{	"CLEAN-UP",			S, xcleanup		}, /* 133 */
{	"TOP-LEVEL",			S, xtoplevel		}, /* 134 */
{	"CONTINUE",			S, xcontinue		}, /* 135 */
{	"ERRSET", 			F, xerrset  		}, /* 136 */
{	"BAKTRACE",			S, xbaktrace		}, /* 137 */
{	"EVALHOOK",			S, xevalhook		}, /* 138 */

    /* arithmetic functions */
{	"TRUNCATE",			S, xfix			}, /* 139 */
{	"FLOAT",			S, xfloat		}, /* 140 */
{	"+",				S, xadd			}, /* 141 */
{	"-",				S, xsub			}, /* 142 */
{	"*",				S, xmul			}, /* 143 */
{	"/",				S, xdiv			}, /* 144 */
{	"1+",				S, xadd1		}, /* 145 */
{	"1-",				S, xsub1		}, /* 146 */
{	"REM",				S, xrem			}, /* 147 */
{	"MIN",				S, xmin			}, /* 148 */
{	"MAX",				S, xmax			}, /* 149 */
{	"ABS",				S, xabs			}, /* 150 */
{	"SIN",				S, xsin			}, /* 151 */
{	"COS",				S, xcos			}, /* 152 */
{	"TAN",				S, xtan			}, /* 153 */
{	"EXPT",				S, xexpt		}, /* 154 */
{	"EXP",				S, xexp			}, /* 155 */
{	"SQRT",		  		S, xsqrt		}, /* 156 */
{	"RANDOM",			S, xrand		}, /* 157 */
            
    /* bitwise logical functions */
{	"LOGAND",			S, xlogand  		}, /* 158 */
{	"LOGIOR",			S, xlogior  		}, /* 159 */
{	"LOGXOR",			S, xlogxor  		}, /* 160 */
{	"LOGNOT",			S, xlognot  		}, /* 161 */

    /* numeric comparison functions */
{	"<",				S, xlss			}, /* 162 */
{	"<=",				S, xleq			}, /* 163 */
{	"=",				S, xequ			}, /* 164 */
{	"/=",				S, xneq			}, /* 165 */
{	">=",				S, xgeq			}, /* 166 */
{	">",				S, xgtr			}, /* 167 */
            
    /* string functions */
{	"STRCAT",			S, xstrcat  		}, /* 168 */
{	"SUBSEQ",			S, xsubseq  		}, /* 169 */
{	"STRING",			S, xstring  		}, /* 170 */
{	"CHAR",				S, xchar		}, /* 171 */

    /* I/O functions */
{	"READ",				S, xread		}, /* 172 */
{	"PRINT",			S, xprint		}, /* 173 */
{	"PRIN1",			S, xprin1		}, /* 174 */
{	"PRINC",			S, xprinc		}, /* 175 */
{	"TERPRI",			S, xterpri  		}, /* 176 */
{	"FLATSIZE",			S, xflatsize		}, /* 177 */
{	"FLATC",			S, xflatc		}, /* 178 */
            
    /* file I/O functions */
{	"OPEN",				S, xopen		}, /* 179 */
{	"FORMAT",			S, xformat  		}, /* 180 */
{	"CLOSE",			S, xclose		}, /* 181 */
{	"READ-CHAR",			S, xrdchar  		}, /* 182 */
{	"PEEK-CHAR",			S, xpkchar  		}, /* 183 */
{	"WRITE-CHAR",			S, xwrchar  		}, /* 184 */
{	"READ-LINE",			S, xreadline		}, /* 185 */

    /* system functions */
{	"LOAD",				S, xload		}, /* 186 */
{	"DRIBBLE",			S, xtranscript		}, /* 187 */

/* functions specific to xldmem.c */
{	"GC",				S, xgc			}, /* 188 */
{	"EXPAND",			S, xexpand  		}, /* 189 */
{	"ALLOC",			S, xalloc		}, /* 190 */
{	"ROOM",				S, xmem			}, /* 191 */
#ifdef SAVERESTORE
{	"SAVE",				S, xsave		}, /* 192 */
{	"RESTORE",			S, xrestore		}, /* 193 */
#else
{	NULL,				S, xnotimp		}, /* 192 */
{	NULL,				S, xnotimp		}, /* 193 */
#endif
/* end of functions specific to xldmem.c */

{	"TYPE-OF",			S, xtype		}, /* 194 */
/* See Audacity bug 379 */
/* {	"EXIT",				S, xexit		}, */
{	NULL,				S, xexit		}, /* 195 */
#ifdef PEEK_AND_POKE
{	"PEEK",				S, xpeek		}, /* 196 */
{	"POKE",				S, xpoke		}, /* 197 */
{	"ADDRESS-OF",			S, xaddrs		}, /* 198 */
#else
{	NULL,				S, xnotimp		}, /* 196 */
{	NULL,				S, xnotimp		}, /* 197 */
{	NULL,				S, xnotimp		}, /* 198 */
#endif
    /* new functions and special forms */
{	"VECTOR",			S, xvector  		}, /* 199 */
{	"BLOCK",			F, xblock		}, /* 200 */
{	"RETURN-FROM",			F, xrtnfrom		}, /* 201 */
{	"TAGBODY",			F, xtagbody		}, /* 202 */
{	"PSETQ",			F, xpsetq		}, /* 203 */
{	"FLET",				F, xflet		}, /* 204 */
{	"LABELS",			F, xlabels  		}, /* 205 */
{	"MACROLET",			F, xmacrolet		}, /* 206 */
{	"UNWIND-PROTECT",		F, xunwindprotect	}, /* 207 */
{	"PPRINT",			S, xpp			}, /* 208 */
{	"STRING<",			S, xstrlss  		}, /* 209 */
{	"STRING<=",			S, xstrleq  		}, /* 210 */
{	"STRING=",			S, xstreql  		}, /* 211 */
{	"STRING/=",			S, xstrneq  		}, /* 212 */
{	"STRING>=",			S, xstrgeq  		}, /* 213 */
{	"STRING>",			S, xstrgtr  		}, /* 214 */
{	"STRING-LESSP",			S, xstrilss		}, /* 215 */
{	"STRING-NOT-GREATERP",		S, xstrileq		}, /* 216 */
{	"STRING-EQUAL",			S, xstrieql		}, /* 217 */
{	"STRING-NOT-EQUAL",		S, xstrineq		}, /* 218 */
{	"STRING-NOT-LESSP",		S, xstrigeq		}, /* 219 */
{	"STRING-GREATERP",		S, xstrigtr		}, /* 220 */
{	"INTEGERP",			S, xintegerp		}, /* 221 */
{	"FLOATP",			S, xfloatp  		}, /* 222 */
{	"STRINGP",			S, xstringp		}, /* 223 */
{	"ARRAYP",			S, xarrayp  		}, /* 224 */
{	"STREAMP",			S, xstreamp		}, /* 225 */
{	"OBJECTP",			S, xobjectp		}, /* 226 */
{	"STRING-UPCASE",		S, xupcase  		}, /* 227 */
{	"STRING-DOWNCASE",		S, xdowncase		}, /* 228 */
{	"NSTRING-UPCASE",		S, xnupcase		}, /* 229 */
{	"NSTRING-DOWNCASE",		S, xndowncase		}, /* 230 */
{	"STRING-TRIM",			S, xtrim		}, /* 231 */
{	"STRING-LEFT-TRIM",		S, xlefttrim		}, /* 232 */
{	"STRING-RIGHT-TRIM",		S, xrighttrim		}, /* 233 */
{	"WHEN",				F, xwhen		}, /* 234 */
{	"UNLESS",			F, xunless  		}, /* 235 */
{	"LOOP",				F, xloop		}, /* 236 */
{	"SYMBOL-FUNCTION",		S, xsymfunction		}, /* 237 */
{	"FBOUNDP",			S, xfboundp		}, /* 238 */
{	"SEND",				S, xsend		}, /* 239 */
{	"SEND-SUPER",			S, xsendsuper		}, /* 240 */
{	"PROGV",			F, xprogv		}, /* 241 */
{	"CHARACTERP",			S, xcharp		}, /* 242 */
{	"CHAR-INT",			S, xcharint		}, /* 243 */
{	"INT-CHAR",			S, xintchar		}, /* 244 */
{	"READ-BYTE",			S, xrdbyte  		}, /* 245 */
{	"WRITE-BYTE",			S, xwrbyte  		}, /* 246 */
{	"MAKE-STRING-INPUT-STREAM", 	S, xmkstrinput		}, /* 247 */
{	"MAKE-STRING-OUTPUT-STREAM",	S, xmkstroutput		}, /* 248 */
{	"GET-OUTPUT-STREAM-STRING",	S, xgetstroutput	}, /* 249 */
{	"GET-OUTPUT-STREAM-LIST",	S, xgetlstoutput	}, /* 250 */
{	"GCD",				S, xgcd			}, /* 251 */
{	"GET-LAMBDA-EXPRESSION", 	S, xgetlambda		}, /* 252 */
{	"MACROEXPAND",			S, xmacroexpand		}, /* 253 */
{	"MACROEXPAND-1",		S, x1macroexpand	}, /* 254 */
{	"CHAR<",			S, xchrlss  		}, /* 255 */
{	"CHAR<=",			S, xchrleq  		}, /* 256 */
{	"CHAR=",			S, xchreql  		}, /* 257 */
{	"CHAR/=",			S, xchrneq  		}, /* 258 */
{	"CHAR>=",			S, xchrgeq  		}, /* 259 */
{	"CHAR>",			S, xchrgtr  		}, /* 260 */
{	"CHAR-LESSP",			S, xchrilss		}, /* 261 */
{	"CHAR-NOT-GREATERP",		S, xchrileq		}, /* 262 */
{	"CHAR-EQUAL",			S, xchrieql		}, /* 263 */
{	"CHAR-NOT-EQUAL",		S, xchrineq		}, /* 264 */
{	"CHAR-NOT-LESSP",		S, xchrigeq		}, /* 265 */
{	"CHAR-GREATERP",		S, xchrigtr		}, /* 266 */
{	"UPPER-CASE-P",			S, xuppercasep		}, /* 267 */
{	"LOWER-CASE-P",			S, xlowercasep		}, /* 268 */
{	"BOTH-CASE-P",			S, xbothcasep		}, /* 269 */
{	"DIGIT-CHAR-P",			S, xdigitp		}, /* 270 */
{	"ALPHANUMERICP",		S, xalphanumericp	}, /* 271 */
{	"CHAR-UPCASE",			S, xchupcase		}, /* 272 */
{	"CHAR-DOWNCASE",		S, xchdowncase		}, /* 273 */
{	"DIGIT-CHAR",			S, xdigitchar		}, /* 274 */
{	"CHAR-CODE",			S, xcharcode		}, /* 275 */
{	"CODE-CHAR",			S, xcodechar		}, /* 276 */
{	"ENDP",  			S, xendp		}, /* 277 */
{	"REMOVE-IF",			S, xremif	    	}, /* 278 */
{	"REMOVE-IF-NOT",		S, xremifnot		}, /* 279 */
{	"DELETE-IF",			S, xdelif		}, /* 280 */
{	"DELETE-IF-NOT",		S, xdelifnot		}, /* 281 */
{	"TRACE",			F, xtrace		}, /* 282 */
{	"UNTRACE",			F, xuntrace		}, /* 283 */
{	"SORT",				S, xsort		}, /* 284 */


    /* extra table entries */
{	"PROFILE",   			S, xprofile		}, /* 285 */
{	"STRING-SEARCH",		S, xstrsearch		}, /* 286 */
{	"QUIT",		    		S, xquit		}, /* 287 */
{	"OPEN-BINARY",			S, xbopen		}, /* 288 */
{	"SETUP-CONSOLE",		S, xsetupconsole	}, /* 289 */
{       "READ-INT",                     S, xrdint               }, /* 290 */
{	"READ-FLOAT",			S, xrdfloat		}, /* 291 */
{	"WRITE-INT",			S, xwrint		}, /* 292 */
{	"WRITE-FLOAT",			S, xwrfloat		}, /* 293 */
{	"INFO",			    	S, xinfo		}, /* 294 */ /* Ning Hu, Apr 2001 */
{	"RRANDOM",  		        S, xrealrand	        }, /* 295 */
#ifdef DEBUG_INPUT
{	"START-RECORD-IO",  	        S, xstartrecordio       }, /* 296 */
{	"STOP-RECORD-IO",   	        S, xstoprecordio	}, /* 297 */
#else
{	NULL,				S, xnotimp		}, /* 296 */
{	NULL,				S, xnotimp		}, /* 297 */
#endif
{	"ATAN",				S, xatan		}, /* 298 */
{	"BIGENDIANP",		        S, xbigendianp	        }, /* 299 */
{	"SETDIR",                       S, xsetdir		}, /* 300 */    //Added by Ning Hu May.2001
{       "LISTDIR",                      S, xlistdir             }, /* 301 */   // Added by RBD, Mar 2005
{       "ECHOENABLED",                  S, xechoenabled         }, /* 302 */   // Added by RBD, Dec 2005
{       "GET-SLIDER-VALUE",             S, xslider_read         }, /* 303 */
{       "OSC-ENABLE",                   S, xosc_enable          }, /* 304 */
{       "GET-TEMP-PATH",                S, xget_temp_path       }, /* 305 */
{       "GET-USER",                     S, xget_user            }, /* 306 */
{       "FIND-IN-XLISP-PATH",           S, xfind_in_xlisp_path  }, /* 307 */
{       "GET-ENV",                      S, xget_env             }, /* 308 */
{       "GET-RUN-TIME",                 S, xgetruntime          }, /* 309 */
{	"RANDOM-SEED",			S, xsrand		}, /* 310 */
{       "GET-REAL-TIME",                S, xgetrealtime         }, /* 311 */
#ifdef MACINTOSH
#include "macptrs.h"
#endif

    /* include system dependant function pointers */
#include "osptrs.h"
#include "localptrs.h"

{0,0,0} /* end of table marker */

};

FUNDEF *funtab = init_funtab;
static size_t szfuntab = sizeof(init_funtab) / sizeof(*init_funtab);

int xlbindfunctions(const FUNDEF *functions, size_t nfunctions)
{
   /* This is written very generally, imposing no fixed upper limit on the
    growth of the table.  But perhaps a lightweight alternative with such a
    limit could be conditionally compiled.
    */

   /* malloc, not realloc, to leave old table unchanged in case of failure */
   FUNDEF *newfuntab = malloc((szfuntab + nfunctions) * sizeof(FUNDEF));
   if (!newfuntab)
      return FALSE;
   memcpy(newfuntab, funtab, (szfuntab - 1) * sizeof(FUNDEF));
   memcpy(newfuntab + szfuntab - 1, functions, nfunctions * sizeof(FUNDEF));
   FUNDEF sentinel = { 0, 0, 0 };
   newfuntab[szfuntab + nfunctions - 1] = sentinel;
   funtab = newfuntab;
   szfuntab += nfunctions;
   return TRUE;

   /* To do: deallocate funtab when XLisp runtime shuts down */
}

/* xnotimp does not return anything on purpose, so disable
 * "no return value" warning
 */
/* #pragma warning(disable: 4716)*/

/* xnotimp - function table entries that are currently not implemented */
LOCAL LVAL xnotimp(void)
{
    xlfail("function not implemented");
    return NIL; /* never happens */
}

