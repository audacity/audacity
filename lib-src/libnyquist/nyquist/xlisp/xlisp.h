/* xlisp - a small subset of lisp */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use

HISTORY
28-Apr-03   Mazzoni
                Added declarations for path.c (new file)
30-Mar-88	Dale Amon CMU-CSD
                Set it up for unix. Picked _TURBOC_  because defs
                are reasonable.
*/

/* system specific definitions */

#ifndef __XLISP__
#define __XLISP__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h> /* needed for getenv(); note that this was a problem
    for PMAX implementation, but I assume PMAX is obsolete now. 
    - RBD 16apr04 */
#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>

/* NNODES	number of nodes to allocate in each request (1000) */
/* EDEPTH	evaluation stack depth (2000) */
/* ADEPTH	argument stack depth (1000) */
/* FORWARD	type of a forward declaration () */
/* LOCAL	type of a local function (static) */
/* AFMT		printf format for addresses ("%x") */
/* FIXTYPE	data type for fixed point numbers (long) */
/* ITYPE	fixed point input conversion routine type (long atol()) */
/* ICNV		fixed point input conversion routine (atol) */
/* IFMT		printf format for fixed point numbers ("%ld") */
/* FLOTYPE	data type for floating point numbers (float) */
/* OFFTYPE	number the size of an address (int) */

/* for the Win32 environment */
#ifdef WIN32
#define NNODES		2000
#define AFMT		"%p"
// TRY 64bit-ints throughout XLisp even on 32-bit Windows
// #ifdef _WIN64
#define OFFTYPE     long long
#define FIXTYPE     long long
#define IFMT        "%lld"
#define ICNV(n)     atoll(n)
// #else
// #define OFFTYPE		long
// #define IFMT        "%ld"
// #endif
/* #define SAVERESTORE */
#define XL_LITTLE_ENDIAN 
#define _longjmp longjmp
#define _setjmp setjmp
#endif

/* for the Turbo C compiler - MS-DOS, large model */
#ifdef _TURBOC_
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
/* #define SAVERESTORE */
#define XL_LITTLE_ENDIAN
#endif

/* for the AZTEC C compiler - MS-DOS, large model */
#ifdef AZTEC_LM
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define CVPTR(x)	ptrtoabs(x)
#define NIL		(void *)0
extern long ptrtoabs();
/* #define SAVERESTORE */
#define XL_LITTLE_ENDIAN
#endif

/* for the AZTEC C compiler - Macintosh */
#ifdef AZTEC_MAC
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define NIL		(void *)0
#define SAVERESTORE
#define XL_BIG_ENDIAN
#endif

/* for the AZTEC C compiler - Amiga */
#ifdef AZTEC_AMIGA
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define NIL		(void *)0
#define SAVERESTORE
#define XL_BIG_ENDIAN
#endif

/* for the Lightspeed C compiler - Macintosh */
#ifdef LSC
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define NIL		(void *)0
#define SAVERESTORE
#define XL_BIG_ENDIAN
#endif

/* for the Microsoft C compiler - MS-DOS, large model */
#ifdef MSC
#define NNODES		2000
#define AFMT		"%lx"
#define OFFTYPE		long
#define XL_LITTLE_ENDIAN
#endif

/* for the Mark Williams C compiler - Atari ST */
#ifdef MWC
#define AFMT		"%lx"
#define OFFTYPE		long
#define XL_BIG_ENDIAN
#endif

/* for the Lattice C compiler - Atari ST */
#ifdef LATTICE
#define FIXTYPE		int
#define ITYPE		int atoi()
#define ICNV(n)		atoi(n)
#define IFMT		"%d"
#define XL_BIG_ENDIAN
#endif

/* for the Digital Research C compiler - Atari ST */
#ifdef DR
#define LOCAL
#define AFMT		"%lx"
#define OFFTYPE		long
#undef NULL
#define NULL		0L
#define XL_BIG_ENDIAN
#endif

/* Mac Metrowerks CW 6 */
#ifdef __MWERKS__
#define LSC
#undef PATHNAMES
#undef FILETABLE
#undef SAVERESTORE
#undef MEDMEM
#define EDEPTH 4000
#define ADEPTH 3000
#define OSAOPEN osaopen
#define OSBOPEN osbopen
#define NO_EXTENSIONS  /* don't add ".lsp" onto filenames */
#define XL_BIG_ENDIAN
#endif

/* Linux on Pentium */
#if defined(__linux__) || defined(__GLIBC__) || defined(__CYGWIN__)
#define AFMT            "%p"
#include <inttypes.h>
#include <endian.h>
#if __BYTE_ORDER == __LITTLE_ENDIAN
#define XL_LITTLE_ENDIAN
#else
#define XL_BIG_ENDIAN
#endif
#endif

/* FreeBSD / OpenBSD */
#if defined(__FreeBSD__) || defined(__OpenBSD__)
#if BYTE_ORDER == LITTLE_ENDIAN
#define XL_LITTLE_ENDIAN
#else
#define XL_BIG_ENDIAN
#endif
#endif

/* Apple CC (xcode, macOS, macintosh) */
#ifdef __APPLE__
#define NNODES 2000
#define AFMT "%p"
#define OFFTYPE long
#define NIL (void *)0
/* #define SAVERESTORE */
#include <sys/types.h>
/* #if __BYTE_ORDER == __LITTLE_ENDIAN */
#if defined(__LITTLE_ENDIAN__)
#define XL_LITTLE_ENDIAN
#else
#define XL_BIG_ENDIAN
#endif
#endif

/* default important definitions */
#ifndef NNODES
#define NNODES		1000
#endif
#ifndef NTYPES
#define NTYPES		20
#endif
#ifndef EDEPTH
/* originally was 2000 */
#define EDEPTH		4000
#endif
#ifndef ADEPTH
/* originally was 1000 */
#define ADEPTH		2000
#endif
#ifndef FORWARD
#define FORWARD
#endif
#ifndef LOCAL
#define LOCAL		static
#endif
#ifndef AFMT
#define AFMT		"%lx"
#endif
#ifndef FIXTYPE
#define FIXTYPE		long
#endif
#ifndef ITYPE
#ifndef atol  /* if atol is a macro, this will mess things up */
#define ITYPE		long atol()
#endif
#endif
#ifndef ICNV
#define ICNV(n)		atol(n)
#endif
#ifndef IFMT
#define IFMT		"%ld"
#endif
#ifndef FLOTYPE
#define FLOTYPE		double
#endif
#ifndef OFFTYPE
#define OFFTYPE		int
#endif
#ifndef CVPTR
#define CVPTR(x)	(x)
#endif
#ifndef UCHAR
#define UCHAR		unsigned char
#endif

#ifndef STDERR
#define STDERR stderr
#endif

/* useful definitions */
#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif
#define externp(x) ((x) && ntype(x) == EXTERN)
#ifndef NIL
#define NIL	(LVAL )0
#endif

/* include the dynamic memory definitions */
#include "xldmem.h"

/* program limits */
#define STRMAX		250		/* maximum length of a string constant */
/* this was 100 -- I had a perfectly good full path to init.lsp using
   a directory structure created by Apple's Xcode that was about 108
   characters, so I picked a bigger value. -RBD */

#define HSIZE		1499		/* symbol hash table size */
#define SAMPLE		50000		/* control character sample rate */
/* Jul 2014: Under Xcode, debug, 2.4 GHz Intel Core i7: oscheck rate is
   about 66Hz */

/* function table offsets for the initialization functions */
#define FT_RMHASH	0
#define FT_RMQUOTE	1
#define FT_RMDQUOTE	2
#define FT_RMBQUOTE	3
#define FT_RMCOMMA	4
#define FT_RMLPAR	5
#define FT_RMRPAR	6
#define FT_RMSEMI	7
#define FT_CLNEW	10
#define FT_CLISNEW	11
#define FT_CLANSWER	12
#define FT_OBISNEW	13
#define FT_OBCLASS	14
#define FT_OBSHOW	15
#define FT_OBISA        16
        
/* macro to push a value onto the argument stack */
#define pusharg(x)	{if (xlsp >= xlargstktop) xlargstkoverflow();\
                         *xlsp++ = (x);}

/* #define DEBUG_GC */

/* macros to protect pointers */
#ifdef DEBUG_GC
void dbg_gc_xlsave(LVAL *n);

#define xlstkcheck(n)	{if (xlstack - (n) < xlstkbase) xlstkoverflow();}
#define xlsave(n)	{*--xlstack = &n; n = NIL; dbg_gc_xlsave(&n);}
#define xlprotect(n)	{*--xlstack = &n; dbg_gc_xlsave(&n);}

/* check the stack and protect a single pointer */
#define xlsave1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n; n = NIL; dbg_gc_xlsave(&n);}
#define xlprot1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n; dbg_gc_xlsave(&n);}

/* macros to pop pointers off the stack */
#define xlpop()		{++xlstack;}
#define xlpopn(n)	{xlstack+=(n);}

#else

#define xlstkcheck(n)	{if (xlstack - (n) < xlstkbase) xlstkoverflow();}
#define xlsave(n)	{*--xlstack = &n; n = NIL;}
#define xlprotect(n)	{*--xlstack = &n;}

/* check the stack and protect a single pointer */
#define xlsave1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n; n = NIL;}
#define xlprot1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n;}

/* macros to pop pointers off the stack */
#define xlpop()		{++xlstack;}
#define xlpopn(n)	{xlstack+=(n);}
#endif

/* macros to manipulate the lexical environment */
#define xlframe(e)	cons(NIL,e)
#define xlbind(s,v)	xlpbind(s,v,xlenv)
#define xlfbind(s,v)	xlpbind(s,v,xlfenv);
#define xlpbind(s,v,e)	{rplaca(e,cons(cons(s,v),car(e)));}

/* macros to manipulate the dynamic environment */
#define xldbind(s,v)	{xldenv = cons(cons(s,getvalue(s)),xldenv);\
                         setvalue(s,v);}
#define xlunbind(e)	{for (; xldenv != (e); xldenv = cdr(xldenv))\
                           setvalue(car(car(xldenv)),cdr(car(xldenv)));}

/* type predicates */			       
#define atomp(x)		((x) == NIL || ntype(x) != CONS)
#define null(x)		((x) == NIL)
#define listp(x)	((x) == NIL || ntype(x) == CONS)
#define consp(x)	((x) && ntype(x) == CONS)
#define subrp(x)	((x) && ntype(x) == SUBR)
#define fsubrp(x)	((x) && ntype(x) == FSUBR)
#define stringp(x)	((x) && ntype(x) == STRING)
#define symbolp(x)	((x) && ntype(x) == SYMBOL)
#define streamp(x)	((x) && ntype(x) == STREAM)
#define objectp(x)	((x) && ntype(x) == OBJECT)
#define fixp(x)		((x) && ntype(x) == FIXNUM)
#define floatp(x)	((x) && ntype(x) == FLONUM)
#define vectorp(x)	((x) && ntype(x) == VECTOR)
#define closurep(x)	((x) && ntype(x) == CLOSURE)
#define charp(x)	((x) && ntype(x) == CHAR)
#define ustreamp(x)	((x) && ntype(x) == USTREAM)
#define boundp(x)	(getvalue(x) != s_unbound)
#define fboundp(x)	(getfunction(x) != s_unbound)

/* shorthand functions */
#define consa(x)	cons(x,NIL)
#define consd(x)	cons(NIL,x)

/* argument list parsing macros */
#define xlgetarg()	(testarg(nextarg()))
#define xllastarg()	{if (xlargc != 0) xltoomany();}
#define testarg(e)	(moreargs() ? (e) : xltoofew())
#define typearg(tp)	(tp(*xlargv) ? nextarg() : xlbadtype(*xlargv))
#define nextarg()	(--xlargc, *xlargv++)
#define moreargs()	(xlargc > 0)

/* macros to get arguments of a particular type */
#define xlgacons()	(testarg(typearg(consp)))
#define xlgalist()	(testarg(typearg(listp)))
#define xlgasymbol()	(testarg(typearg(symbolp)))
#define xlgastring()	(testarg(typearg(stringp)))
#define xlgaobject()	(testarg(typearg(objectp)))
#define xlgafixnum()	(testarg(typearg(fixp)))
#define xlgaflonum()	(testarg(typearg(floatp)))
#define xlgachar()	(testarg(typearg(charp)))
#define xlgavector()	(testarg(typearg(vectorp)))
#define xlgastream()	(testarg(typearg(streamp)))
#define xlgaustream()	(testarg(typearg(ustreamp)))
#define xlgaclosure()	(testarg(typearg(closurep)))

/* function definition structure */
typedef struct {
    const char *fd_name;	/* function name */
    int fd_type;	/* function type */
    LVAL (*fd_subr)(void);	/* function entry point */
} FUNDEF;

/* execution context flags */
#define CF_GO		0x0001
#define CF_RETURN	0x0002
#define CF_THROW	0x0004
#define CF_ERROR	0x0008
#define CF_CLEANUP	0x0010
#define CF_CONTINUE	0x0020
#define CF_TOPLEVEL	0x0040
#define CF_BRKLEVEL	0x0080
#define CF_UNWIND	0x0100


/* execution context */
typedef struct context {
    int c_flags;			/* context type flags */
    LVAL c_expr;			/* expression (type dependant) */
    jmp_buf c_jmpbuf;			/* _longjmp context */
    struct context *c_xlcontext;	/* old value of xlcontext */
    LVAL **c_xlstack;			/* old value of xlstack */
    LVAL *c_xlargv;			/* old value of xlargv */
    int c_xlargc;			/* old value of xlargc */
    LVAL *c_xlfp;			/* old value of xlfp */
    LVAL *c_xlsp;			/* old value of xlsp */
    LVAL c_xlenv;			/* old value of xlenv */
    LVAL c_xlfenv;			/* old value of xlfenv */
    LVAL c_xldenv;			/* old value of xldenv */
} XLCONTEXT;

/* external variables */
extern LVAL **xlstktop;       	/* top of the evaluation stack */
extern LVAL **xlstkbase;	/* base of the evaluation stack */
extern LVAL **xlstack;		/* evaluation stack pointer */
extern LVAL *xlargstkbase;	/* base of the argument stack */
extern LVAL *xlargstktop;	/* top of the argument stack */
extern LVAL *xlfp;		/* argument frame pointer */
extern LVAL *xlsp;		/* argument stack pointer */
extern LVAL *xlargv;		/* current argument vector */
extern int xlargc;		/* current argument count */

/* more external variables */
extern LVAL xlenv,xlfenv,xldenv,xlvalue,s_true;
extern LVAL lk_optional,lk_rest,lk_key,lk_aux,lk_allow_other_keys;
extern LVAL s_evalhook,s_applyhook,s_tracelist;
extern LVAL s_lambda,s_macro;
extern LVAL s_unbound;
extern int xlsample;
extern char buf[];
extern LVAL obarray,s_gcflag,s_gchook;
extern int xldebug;
extern LVAL s_debugio;
extern LVAL s_tracenable,s_tlimit,s_breakenable;
extern LVAL s_loadingfiles;
extern LVAL k_direction,k_input,k_output;
extern LVAL s_stdin,s_stdout;
extern int xlfsize;
/* external variables */
extern LVAL s_car,s_cdr,s_nth,s_get,s_svalue,s_splist,s_aref;
extern LVAL s_comma,s_comat;
extern char gsprefix[];
extern int gsnumber;


/* additional prototypes */

extern FILE *osaopen (const char *name, const char *mode);
extern FILE *osbopen (const char *name, const char *mode);

#ifdef __MWERKS__
/* macfun.c */

LVAL xptsize(void);
LVAL xhidepen(void);
LVAL xshowpen(void);
LVAL xgetpen(void);
LVAL xpenmode(void);
LVAL xpensize(void);
LVAL xpenpat(void);
LVAL xpennormal(void);
LVAL xmoveto(void);
LVAL xmove(void);
LVAL xdrawto(void);
LVAL xdraw(void);
LVAL xshowgraphics(void);
LVAL xhidegraphics(void);
LVAL xcleargraphics(void);
LVAL xtool(void);
LVAL xtool16(void);
LVAL xtool32(void);
LVAL xnewhandle(void);
LVAL xnewptr(void);
LVAL xhiword(void);
LVAL xloword(void);
LVAL xrdnohang(void);
/* #include "macstuff.h" */
#endif

/* for extern.c */

void inval_caches(void);


/* for xlbfun.c */

LVAL xeval(void);
LVAL xapply(void);
LVAL xfuncall(void);
LVAL xmacroexpand(void);
LVAL x1macroexpand(void);
LVAL xatom(void);
LVAL xsymbolp(void);
LVAL xnumberp(void);
LVAL xintegerp(void);
LVAL xfloatp(void);
LVAL xcharp(void);
LVAL xstringp(void);
LVAL xarrayp(void);
LVAL xstreamp(void);
LVAL xobjectp(void);
LVAL xboundp(void);
LVAL xfboundp(void);
LVAL xnull(void);
LVAL xlistp(void);
LVAL xendp(void);
LVAL xconsp(void);
LVAL xeq(void);
LVAL xeql(void);
LVAL xequal(void);
LVAL xset(void);
LVAL xgensym(void);
LVAL xmakesymbol(void);
LVAL xintern(void);
LVAL xsymname(void);
LVAL xsymvalue(void);
LVAL xsymfunction(void);
LVAL xsymplist(void);
LVAL xget(void);
LVAL xputprop(void);
LVAL xremprop(void);
LVAL xhash(void);
LVAL xaref(void);
LVAL xmkarray(void);
LVAL xvector(void);
LVAL xerror(void);
LVAL xcerror(void);
LVAL xbreak(void);
LVAL xcleanup(void);
LVAL xtoplevel(void);
LVAL xcontinue(void);
LVAL xevalhook(void);


/* xlcont.c */

LVAL xquote(void);
LVAL xfunction(void);
LVAL xbquote(void);
LVAL xlambda(void);
LVAL xgetlambda(void);
LVAL xsetq(void);
LVAL xpsetq(void);
LVAL xsetf(void);
LVAL xdefun(void);
LVAL xdefmacro(void);
LVAL xcond(void);
LVAL xwhen(void);
LVAL xunless(void);
LVAL xcase(void);
LVAL xand(void);
LVAL x_or(void); // xor causes problems for gcc, so I renamed it. -RBD
LVAL xif(void);
LVAL xlet(void);
LVAL xletstar(void);
LVAL xflet(void);
LVAL xlabels(void);
LVAL xmacrolet(void);
LVAL xprog(void);
LVAL xprogstar(void);
LVAL xgo(void);
LVAL xreturn(void);
LVAL xrtnfrom(void);
LVAL xprog1(void);
LVAL xprog2(void);
LVAL xprogn(void);
LVAL xprogv(void);
LVAL xloop(void);
LVAL xdo(void);
LVAL xdostar(void);
LVAL xdolist(void);
LVAL xdotimes(void);
LVAL xblock(void);
LVAL xtagbody(void);
LVAL xcatch(void);
LVAL xthrow(void);
LVAL xunwindprotect(void);
LVAL xerrset(void);
LVAL xtrace(void);
LVAL xuntrace(void);


/* xldbug.c */

void xlabort(const char *emsg);
void xlbreak(const char *emsg, LVAL arg);
void xlfail(const char *emsg);
void xlerror(const char *emsg, LVAL arg);
void xlcerror(const char *cmsg, const char *emsg, LVAL arg);
void xlerrprint(const char *hdr, const char *cmsg, const char *emsg, LVAL arg);
void xlbaktrace(int n);
void xldinit(void);
void close_loadingfiles(void);

/* xldmem.c */
extern long total; /* total bytes allocated by xlisp */

LVAL cons(LVAL x, LVAL y);
LVAL cvstring(const char *str);
LVAL new_string(int size);
LVAL cvsymbol(const char *pname);
LVAL cvsubr(LVAL (*fcn)(void), int type, int offset);
LVAL cvfile(FILE *fp);
LVAL cvfixnum(FIXTYPE n);
LVAL cvflonum(FLOTYPE n);
LVAL cvchar(int n);
LVAL newustream(void);
LVAL newobject(LVAL cls, int size);
LVAL newclosure(LVAL name, LVAL type, LVAL env, LVAL fenv);
LVAL newvector(int size);
void gc(void);
SEGMENT *newsegment(int n);
LVAL xgc(void);
LVAL xexpand(void);
LVAL xalloc(void);
LVAL xmem(void);
LVAL xsave(void);
LVAL xrestore(void);
void xlminit(void);
LVAL cvextern(xtype_desc typeptr, unsigned char *instptr);		/* convert an external type */
LVAL newnode(int type);
void mark(LVAL ptr);


/* xleval.c */

LVAL xleval(LVAL expr);
LVAL xlxeval(LVAL expr);
LVAL xlapply(int argc);
LVAL xlexpandmacros(LVAL form);
int macroexpand(LVAL fun, LVAL args, LVAL *pval);
int pushargs(LVAL fun, LVAL args);
LVAL makearglist(int argc, LVAL *argv);
LVAL xlclose(LVAL name, LVAL type, LVAL fargs, LVAL body, LVAL env, LVAL fenv);
void xlabind(LVAL fun, int argc, LVAL *argv);
void xlunbound(LVAL sym);
void xlfunbound(LVAL sym);
void xlstkoverflow(void);
void xlargstkoverflow(void);


/* xlfio.c */

LVAL xread(void);
LVAL xprint(void);
LVAL xprin1(void);
LVAL xprinc(void);
LVAL xterpri(void);
LVAL xflatsize(void);
LVAL xflatc(void);
LVAL xopen(void);
LVAL xbopen(void);
LVAL xclose(void);
LVAL xrdchar(void);
LVAL xrdbyte(void);
LVAL xpkchar(void);
LVAL xwrchar(void);
LVAL xwrbyte(void);
LVAL xrdint(void);
LVAL xwrint(void);
LVAL xrdfloat(void);
LVAL xwrfloat(void);
LVAL xreadline(void);
LVAL xmkstrinput(void);
LVAL xmkstroutput(void);
LVAL xgetstroutput(void);
LVAL xgetlstoutput(void);
LVAL xformat(void);
LVAL xlistdir(void);
LVAL xbigendianp(void);

/* xlimage.c */

int xlisave(const char *fname);
int xlirestore(const char *fname);


/* xlinit.c */

void xlinit(void);
void xlsymbols(void);


/* xlftab.c */
/* returns true on success,
   false if table limits would be exceeded and the table remains unchanged
   Call this, any number of times, before calling xlisp_main_init
 */
int xlbindfunctions(const FUNDEF *functions, size_t nfunctions);

/* xlio.c */

int xlgetc(LVAL fptr);
void xlungetc(LVAL fptr, int ch);
int xlpeek(LVAL fptr);
void xlputc(LVAL fptr, int ch);
void xloutflush(LVAL fptr);
void xlflush(void);
void stdprint(LVAL expr);
void stdputstr(const char *str);
void stdflush(void);
void errprint(LVAL expr);
void errputstr(const char *str);
void dbgprint(LVAL expr);
void dbgputstr(const char *str);
void trcprin1(LVAL expr);
void trcputstr(const char *str);


/* xlisp.c */
long xlsrand(long seed);
long xlrand(long range);
double xlrealrand(void);
void xlrdsave(LVAL expr);
void xlevsave(LVAL expr);
void xlfatal(const char *msg);
void xlisp_main_init(int, char **);
void xlisp_main(void);
void xlisp_wrapup(void);


/* xljump.c */

void xlbegin(XLCONTEXT *cptr, int flags, LVAL expr);
void xlend(XLCONTEXT *cptr);
void xlgo(LVAL label);
void xlreturn(LVAL name, LVAL val);
void xlthrow(LVAL tag, LVAL val);
void xlsignal(const char *emsg, LVAL arg);
void xltoplevel(void);
void xlbrklevel(void);
void xlcleanup(void);
void xlcontinue(void);
void xljump(XLCONTEXT *target, int mask, LVAL val);


/* xllist.c */

LVAL xcar(void);
LVAL xcdr(void);
LVAL xcaar(void);
LVAL xcadr(void);
LVAL xcdar(void);
LVAL xcddr(void);
LVAL xcaaar(void);
LVAL xcaadr(void);
LVAL xcadar(void);
LVAL xcaddr(void);
LVAL xcdaar(void);
LVAL xcdadr(void);
LVAL xcddar(void);
LVAL xcdddr(void);

/* cxxxxr functions */
LVAL xcaaaar(void);
LVAL xcaaadr(void);
LVAL xcaadar(void);
LVAL xcaaddr(void);
LVAL xcadaar(void);
LVAL xcadadr(void);
LVAL xcaddar(void);
LVAL xcadddr(void);
LVAL xcdaaar(void);
LVAL xcdaadr(void);
LVAL xcdadar(void);
LVAL xcdaddr(void);
LVAL xcddaar(void);
LVAL xcddadr(void);
LVAL xcdddar(void);
LVAL xcddddr(void);
LVAL xcons(void);
LVAL xlist(void);
LVAL xappend(void);
LVAL xreverse(void);
LVAL xlast(void);
LVAL xmember(void);
LVAL xassoc(void);
LVAL xsubst(void);
LVAL xsublis(void);
LVAL xremove(void);
LVAL xremif(void);
LVAL xremifnot(void);
int dotest1(LVAL arg, LVAL fun);
int dotest2(LVAL arg1, LVAL arg2, LVAL fun);
LVAL xnth(void);
LVAL xnthcdr(void);
LVAL xlength(void);
LVAL xmapc(void);
LVAL xmapcar(void);
LVAL xmapl(void);
LVAL xmaplist(void);
LVAL xrplca(void);
LVAL xrplcd(void);
LVAL xnconc(void);
LVAL xdelete(void);
LVAL xdelif(void);
LVAL xdelifnot(void);
LVAL xsort(void);


/* xlmath.c */

LVAL xadd(void);
LVAL xsub(void);
LVAL xmul(void);
LVAL xdiv(void);
LVAL xrem(void);
LVAL xmin(void);
LVAL xmax(void);
LVAL xexpt(void);
LVAL xlogand(void);
LVAL xlogior(void);
LVAL xlogxor(void);
LVAL xgcd(void);
void checkizero(FIXTYPE iarg);
void checkfzero(FLOTYPE farg);
void checkfneg(FLOTYPE farg);
LVAL xlognot(void);
LVAL xabs(void);
LVAL xadd1(void);
LVAL xsub1(void);
LVAL xsin(void);
LVAL xcos(void);
LVAL xtan(void);
LVAL xexp(void);
LVAL xsqrt(void);
LVAL xfix(void);
LVAL xfloat(void);
LVAL xrand(void);
LVAL xsrand(void);
LVAL xminusp(void);
LVAL xzerop(void);
LVAL xplusp(void);
LVAL xevenp(void);
LVAL xoddp(void);
LVAL xlss(void);
LVAL xleq(void);
LVAL xequ(void);
LVAL xneq(void);
LVAL xgeq(void);
LVAL xgtr(void);
LVAL xrealrand(void);
LVAL xtan(void);
LVAL xatan(void);

/* xlobj.c */

LVAL xsend(void);
LVAL xsendsuper(void);
LVAL xlclass(const char *name, int vcnt);
void xladdivar(LVAL cls, const char *var);
void xladdmsg(LVAL cls, const char *msg, int offset);
int xlobgetvalue(LVAL pair, LVAL sym, LVAL *pval);
int xlobsetvalue(LVAL pair, LVAL sym, LVAL val);
LVAL obisnew(void);
LVAL obclass(void);
LVAL obshow(void);
LVAL obisa(void);
LVAL clnew(void);
LVAL clisnew(void);
LVAL clanswer(void);
void obsymbols(void);
void xloinit(void);


/* xlpp.c */

LVAL xpp(void);


/* xlprin.c */

void xlprint(LVAL fptr, LVAL vptr, int flag);
void xlterpri(LVAL fptr);
void xlputstr(LVAL fptr, const char *str);
void putatm(LVAL fptr, const char *tag, LVAL val);


/* xlread.c */

int xlload(const char *fname, int vflag, int pflag);
int xlread(LVAL fptr, LVAL *pval, int rflag);
int readone(LVAL fptr, LVAL *pval);
LVAL rmhash(void);
LVAL rmquote(void);
LVAL rmdquote(void);
LVAL rmbquote(void);
LVAL rmcomma(void);
LVAL rmlpar(void);
LVAL rmrpar(void);
LVAL rmsemi(void);
LVAL tentry(int ch);
int xlisnumber(char *str, LVAL *pval);
void defmacro(int ch, LVAL type, int offset);
LVAL callmacro(LVAL fptr, int ch);
void xlrinit(void);


/* xlstr.c */

LVAL xstrlss(void);
LVAL xstrleq(void);
LVAL xstreql(void);
LVAL xstrneq(void);
LVAL xstrgeq(void);
LVAL xstrgtr(void);
LVAL xstrilss(void);
LVAL xstrileq(void);
LVAL xstrieql(void);
LVAL xstrineq(void);
LVAL xstrigeq(void);
LVAL xstrigtr(void);
LVAL xupcase(void);
LVAL xdowncase(void);
LVAL xnupcase(void);
LVAL xndowncase(void);
LVAL xstrsearch(void);
LVAL xtrim(void);
LVAL xlefttrim(void);
LVAL xrighttrim(void);
LVAL xstrcat(void);
LVAL xsubseq(void);
LVAL xstring(void);
LVAL xchar(void);
LVAL xcharint(void);
LVAL xintchar(void);
LVAL xuppercasep(void);
LVAL xlowercasep(void);
LVAL xbothcasep(void);
LVAL xdigitp(void);
LVAL xcharcode(void);
LVAL xcodechar(void);
LVAL xchupcase(void);
LVAL xchdowncase(void);
LVAL xdigitchar(void);
LVAL xalphanumericp(void);
LVAL xchrlss(void);
LVAL xchrleq(void);
LVAL xchreql(void);
LVAL xchrneq(void);
LVAL xchrgeq(void);
LVAL xchrgtr(void);
LVAL xchrilss(void);
LVAL xchrileq(void);
LVAL xchrieql(void);
LVAL xchrineq(void);
LVAL xchrigeq(void);
LVAL xchrigtr(void);
LVAL xinfo(void);

/* xlsubr.c */

LVAL xlsubr(const char *sname, int type, LVAL (*fcn)(void), int offset);
int xlgetkeyarg(LVAL key, LVAL *pval);
void xltest(LVAL *pfcn, int *ptresult);
int xlgkfixnum(LVAL key, LVAL *pval);
/* argument list parsing functions */
extern LVAL xlgetfile(void);  	/* get a file/stream argument */
extern LVAL xlgetfname(void);	/* get a filename argument */
int needsextension(const char *name);
/* error reporting functions (don't *really* return at all) */
extern LVAL xlbadtype(LVAL arg);	/* report "bad argument type" error */
extern LVAL xltoofew(void);		/* report "too few arguments" error */
extern LVAL xltoomany(void);
int eq(LVAL arg1, LVAL arg2);
int eql(LVAL arg1, LVAL arg2);
int lval_equal(LVAL arg1, LVAL arg2);


/* xlsym.c */

LVAL xlenter(const char *name);	/* enter a symbol */
LVAL xlmakesym(const char *name);	/* make an uninterned symbol */
LVAL xlgetvalue(LVAL sym);	/* get value of a symbol (checked) */
LVAL xlxgetvalue(LVAL sym);	/* get value of a symbol */
void xlsetvalue(LVAL sym, LVAL val);
LVAL xlgetfunction(LVAL sym);	/* get functional value of a symbol */
LVAL xlxgetfunction(LVAL sym);	/* get functional value of a symbol (checked) */
void xlsetfunction(LVAL sym, LVAL val);
LVAL xlgetprop(LVAL sym, LVAL prp);
void xlputprop(LVAL sym, LVAL val, LVAL prp);
void xlremprop(LVAL sym, LVAL prp);
int hash(const char *str, int len);
void xlsinit(void);
LVAL findprop(LVAL sym, LVAL prp);

/* xlsys.c */

LVAL xget_env(void);
LVAL xload(void);
LVAL xtranscript(void);
LVAL xtype(void);
LVAL xbaktrace(void);
LVAL xexit(void);
LVAL xpeek(void);
LVAL xpoke(void);
LVAL xaddrs(void);
LVAL xgetruntime(void);
LVAL xprofile(void);
LVAL xquit(void);

/* macstuff, unixstuff, winstuff, osstuff */

LVAL xgetrealtime(void);
extern const char os_pathchar;
extern const char os_sepchar;

void osinit(const char *banner);
void oserror(const char *msg);
void osfinish(void);
int osclose(FILE *fp);
void osflush(void);
void oscheck(void);
int osaputc(int ch, FILE *fp);
void osoutflush(FILE *fp);
int osbputc(int ch, FILE *fp);
void ostputc(int ch);
void ostoutflush(void);
int osagetc(FILE *fp);
int osbgetc(FILE *fp);
int ostgetc(void);
void ossymbols(void);
LVAL xlinfo(void);
LVAL xsetdir(void);
int osdir_list_start(const char *path);
const char *osdir_list_next(void);
void osdir_list_finish(void);
LVAL xosc_enable(void);
LVAL xget_temp_path(void);
LVAL xfind_in_xlisp_path(void);
LVAL xsetupconsole(void);
LVAL xechoenabled(void);
LVAL xget_user(void);

/* security.c */

extern char *secure_read_path;
extern char *safe_write_path;
extern int run_time_limit;
extern int run_time;
extern int memory_limit;
#define SAFE_NYQUIST (safe_write_path != NULL)
int ok_to_open(const char *filename, const char *mode);

/* These are now implemented in path.c   -dmazzoni */
const char *return_xlisp_path(void);
const char *find_in_xlisp_path(const char *fname);
void set_xlisp_path(const char *p);

/* local.c - these procedures are specific to each implementation */

void localinit(void);
void localsymbols(void);
void print_local_gc_info(void);
void local_toplevel(void);

#ifdef __cplusplus
}
#endif

#endif

