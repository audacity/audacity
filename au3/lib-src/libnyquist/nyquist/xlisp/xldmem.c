/* xldmem - xlisp dynamic memory management routines */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use

 * HISTORY
 * 28-Apr-03    Mazzoni
 *  eliminate some compiler warnings
 * 14-Apr-88	Dannenberg
 *	Call free method when an EXTERN node is garbage collected
 */


// #define DEBUG_MEM 1

#include "stdlib.h"
#include "string.h"
#include "limits.h"
#include "xlisp.h"

#ifdef WIN32
#include "malloc.h" // defines alloca()
#endif

/* node flags */
#define MARK	1
#define LEFT	2

/* macro to compute the size of a segment */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))


#ifdef DEBUG_INPUT
extern FILE *debug_input_fp;
#endif

/* variables local to xldmem.c and xlimage.c */
SEGMENT *segs,*lastseg,*fixseg,*charseg;
int anodes,nsegs,gccalls;
long nnodes,nfree,total;
LVAL fnodes;

#ifdef DEBUG_MEM
long xldmem_trace = 0;	/* debugging */
#endif

/* forward declarations */
FORWARD LOCAL void findmem(void);
FORWARD LVAL newnode(int type);
FORWARD LOCAL unsigned char *stralloc(int size);
FORWARD LOCAL int addseg(void);
FORWARD void mark(LVAL ptr);
FORWARD LOCAL void sweep(void);

extern void freeimage(void);

#ifdef DEBUG_GC
static long dbg_gc_n = 0;	/* counts save operations */
long dbg_gc_count = 0;	        /* says when to stop */
LVAL *dbg_gc_addr = NULL;	/* says what we're looking for */

void dbg_gc_xlsave(LVAL *n)
{
    dbg_gc_n++;
    if (n == dbg_gc_addr) {
        printf("dbg_gc_xlsave: %x at count %d\n",
               dbg_gc_addr, dbg_gc_n);
    }
    if (dbg_gc_count == dbg_gc_n) {
        printf("dbg_gc_xlsave: reached %d\n",
               dbg_gc_count);
    }
}


#endif


/* cons - construct a new cons node */
LVAL cons(LVAL x, LVAL y)
{
    LVAL nnode;
    /* get a free node */
    if ((nnode = fnodes) == NIL) {
        xlstkcheck(2);
        xlprotect(x);
        xlprotect(y);
        findmem();
        if ((nnode = fnodes) == NIL)
            xlabort("insufficient node space");
        xlpop();
        xlpop();
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    --nfree;

    /* initialize the new node */
    nnode->n_type = CONS;
    rplaca(nnode,x);
    rplacd(nnode,y);

    /* return the new node */
    return (nnode);
}

/* cvstring - convert a string to a string node */
LVAL cvstring(const char *str)
{
    LVAL val;
    xlsave1(val);
    size_t len = strlen(str) + 1;
    if (len > 0x7FFFFFFF) {
        xlfail("string too long");
    }
    val = newnode(STRING);
    val->n_strlen = (int) len;
    val->n_string = stralloc(getslength(val));
    strcpy((char *) getstring(val), str);
    xlpop();
    return (val);
}

/* new_string - allocate and initialize a new string */
LVAL new_string(int size)
{
    LVAL val;
    xlsave1(val);
    val = newnode(STRING);
    val->n_strlen = size;
    val->n_string = stralloc(getslength(val));
    strcpy((char *) getstring(val),"");
    xlpop();
    return (val);
}

/* cvsymbol - convert a string to a symbol */
LVAL cvsymbol(const char *pname)
{
    /* pname points to a global buffer space. This is ok unless you have
     * a gc hook that writes things and therefore uses the buffer. Then
     * if newvector causes a GC, pname is overwritten before cvstring is
     * called and the symbol will have the wrong name!
     * The bug is fixed by copying pname to the stack.
     */
    LVAL val;
    size_t len = strlen(pname) + 1; /* don't forget the terminating zero */
    if (len > 0x7FFFFFFF) {  /* how much can we put on stack? */
        xlfail("string too long");
    }
    char *local_pname_copy = (char *) alloca(len);
    memcpy(local_pname_copy, pname, len);
    xlsave1(val);
    val = newvector(SYMSIZE);
    val->n_type = SYMBOL;
    setvalue(val,s_unbound);
    setfunction(val,s_unbound);
    setpname(val,cvstring(local_pname_copy));
    xlpop();
    return (val);
}

/* cvsubr - convert a function to a subr or fsubr */
LVAL cvsubr(LVAL (*fcn)(void), int type, int offset)
{
    LVAL val;
    val = newnode(type);
    val->n_subr = fcn;
    val->n_offset = offset;
    return (val);
}

/* cvfile - convert a file pointer to a stream */
LVAL cvfile(FILE *fp)
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
    return (val);
}

/* cvfixnum - convert an integer to a fixnum node */
LVAL cvfixnum(FIXTYPE n)
{
    LVAL val;
    if (n >= SFIXMIN && n <= SFIXMAX)
        return (&fixseg->sg_nodes[(int)n-SFIXMIN]);
    val = newnode(FIXNUM);
    val->n_fixnum = n;
    return (val);
}

/* cvflonum - convert a floating point number to a flonum node */
LVAL cvflonum(FLOTYPE n)
{
    LVAL val;
    val = newnode(FLONUM);
    val->n_flonum = n;
    return (val);
}

/* cvchar - convert an integer to a character node */
LVAL cvchar(int n)
{
    if (n >= CHARMIN && n <= CHARMAX)
        return (&charseg->sg_nodes[n-CHARMIN]);
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return NIL; /* won't reach this line */
}

/* newustream - create a new unnamed stream */
LVAL newustream(void)
{
    LVAL val;
    val = newnode(USTREAM);
    sethead(val,NIL);
    settail(val,NIL);
    return (val);
}

/* newobject - allocate and initialize a new object */
LVAL newobject(LVAL cls, int size)
{
    LVAL val;
    val = newvector(size+1);
    val->n_type = OBJECT;
    setelement(val,0,cls);
    return (val);
}

/* newclosure - allocate and initialize a new closure */
LVAL newclosure(LVAL name, LVAL type, LVAL env, LVAL fenv)
{
    LVAL val;
    val = newvector(CLOSIZE);
    val->n_type = CLOSURE;
    setname(val,name);
    settype(val,type);
    setenv(val,env);
    setfenv(val,fenv);
    return (val);
}

/* newvector - allocate and initialize a new vector node */
LVAL newvector(int size)
{
    LVAL vect;
    int bsize;
    xlsave1(vect);
    vect = newnode(VECTOR);
    vect->n_vsize = 0;
    if (size < 0) xlfail("negative vector size requested");
    if (size > INT_MAX / sizeof(LVAL))
        xlfail("too large vector size requested");
    if ((bsize = size * sizeof(LVAL))) {
        if ((vect->n_vdata = (LVAL *)calloc(1,bsize)) == NULL) {
            findmem();
            if ((vect->n_vdata = (LVAL *)calloc(1,bsize)) == NULL)
                xlfail("insufficient vector space");
        }
        vect->n_vsize = size;
        total += (long) bsize;
    }
    xlpop();
    return (vect);
}

/* newnode - allocate a new node */
LVAL newnode(int type)
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
        findmem();
        if ((nnode = fnodes) == NIL)
            xlabort("insufficient node space");
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;

    /* initialize the new node */
    nnode->n_type = type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}

/* stralloc - allocate memory for a string adding a byte for the terminator */
LOCAL unsigned char *stralloc(int size)
{
    unsigned char *sptr;

    /* allocate memory for the string copy */
    if ((sptr = (unsigned char *)malloc(size)) == NULL) {
        gc();  
        if ((sptr = (unsigned char *)malloc(size)) == NULL)
            xlfail("insufficient string space");
    }
    total += (long)size;

    /* return the new string memory */
    return (sptr);
}

/* findmem - find more memory by collecting then expanding */
LOCAL void findmem(void)
{
    gc();
    if (nfree < (long)anodes)
        addseg();
}

/* gc - garbage collect (only called here and in xlimage.c) */
void gc(void)
{
    register LVAL **p,*ap,tmp;
    char buf[STRMAX+1];
    LVAL *newfp,fun;
    extern LVAL profile_fixnum;

    /* print the start of the gc message */
    if (s_gcflag && getvalue(s_gcflag)) {
        sprintf(buf,"[ gc: total %ld, ",nnodes);
        stdputstr(buf);
    }

    /* mark the fixnum used by profiler */
    if (!null(profile_fixnum)) mark(profile_fixnum);

    /* mark the obarray, the argument list and the current environment */
    if (obarray)
        mark(obarray);
    if (xlenv)
        mark(xlenv);
    if (xlfenv)
        mark(xlfenv);
    if (xldenv)
        mark(xldenv);

    /* mark the evaluation stack */
    for (p = xlstack; p < xlstktop; ++p)
        if ((tmp = **p))
            mark(tmp);

    /* mark the argument stack */
    for (ap = xlargstkbase; ap < xlsp; ++ap)
        if ((tmp = *ap))
            mark(tmp);

    /* sweep memory collecting all unmarked nodes */
    sweep();

    /* count the gc call */
    ++gccalls;

    /* call the *gc-hook* if necessary */
    if (s_gchook && (fun = getvalue(s_gchook))) {
        newfp = xlsp;
        pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
        pusharg(fun);
        pusharg(cvfixnum((FIXTYPE)2));
        pusharg(cvfixnum((FIXTYPE)nnodes));
        pusharg(cvfixnum((FIXTYPE)nfree));
        xlfp = newfp;
        xlapply(2);
    }

    /* print the end of the gc message */
    if (s_gcflag && getvalue(s_gcflag)) {
        sprintf(buf,"%ld free", nfree);
        stdputstr(buf);
        /* print additional info (e.g. sound blocks in Nyquist) */
        print_local_gc_info();
        stdputstr(" ]\n");
        stdflush(); /* output in a timely fashion so user sees progress */
    }
#ifdef DEBUG_INPUT
    if (debug_input_fp) {
        int c = getc(debug_input_fp);
        ungetc(c, debug_input_fp);
    }
#endif
}

/* mark - mark all accessible nodes */
void mark(LVAL ptr)
{
    register LVAL this,prev,tmp;
    int type,i,n;

    /* initialize */
    prev = NIL;
    this = ptr;

    /* mark this list */
    for (;;) {

        /* descend as far as we can */
        while (!(this->n_flags & MARK))

            /* check cons and symbol nodes */
            if (((type = ntype(this))) == CONS || type == USTREAM) {
                if ((tmp = car(this))) {
                    this->n_flags |= MARK|LEFT;
                    rplaca(this,prev);
                }
                else if ((tmp = cdr(this))) {
                    this->n_flags |= MARK;
                    rplacd(this,prev);
                }
                else {				/* both sides nil */
                    this->n_flags |= MARK;
                    break;
                }
                prev = this;			/* step down the branch */
                this = tmp;
            }

            /* mark other node types */
            else {
                this->n_flags |= MARK;
                switch (type) {
                case SYMBOL:
                case OBJECT:
                case VECTOR:
                case CLOSURE:
                    for (i = 0, n = getsize(this); --n >= 0; ++i)
                        if ((tmp = getelement(this,i)))
                            mark(tmp);
                    break;
                case EXTERN:
                    if (getdesc(this)->mark_meth) { (*(getdesc(this)->mark_meth))(getinst(this));
                    }
                }
                break;
            }

        /* backup to a point where we can continue descending */
        for (;;)

            /* make sure there is a previous node */
            if (prev) {
                if (prev->n_flags & LEFT) {	/* came from left side */
                    prev->n_flags &= ~LEFT;
                    tmp = car(prev);
                    rplaca(prev,this);
                    if ((this = cdr(prev))) {
                        rplacd(prev,tmp);			
                        break;
                    }
                }
                else {				/* came from right side */
                    tmp = cdr(prev);
                    rplacd(prev,this);
                }
                this = prev;			/* step back up the branch */
                prev = tmp;
            }

            /* no previous node, must be done */
            else
                return;
    }
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL void sweep(void)
{
    SEGMENT *seg;
    LVAL p;
    int n;

    /* empty the free list */
    fnodes = NIL;
    nfree = 0L;

    /* add all unmarked nodes */
    for (seg = segs; seg; seg = seg->sg_next) {
        if (seg == fixseg)	 /* don't sweep the fixnum segment */
            continue;
        else if (seg == charseg) /* don't sweep the character segment */
            continue;
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p) {
#ifdef DEBUG_MEM
            if (xldmem_trace &&
                  ntype(p) == EXTERN &&
                  xldmem_trace == getinst(p)) {
                printf("sweep: EXTERN node %p is %smarked, points to %p\n",
                       p, (p->n_flags & MARK ? "" : "un"), getinst(p));
            }
#endif
            if (!(p->n_flags & MARK)) {
                switch (ntype(p)) {
                case STRING:
                        if (getstring(p) != NULL) {
                            total -= (long)getslength(p);
                            free(getstring(p));
                        }
                        break;
                case STREAM:
                        if (getfile(p))
                            osclose(getfile(p));
                        break;
                case SYMBOL:
                case OBJECT:
                case VECTOR:
                case CLOSURE:
                        if (p->n_vsize) {
                            total -= (long) (p->n_vsize * sizeof(LVAL));
                            free((void *) p->n_vdata);
                        }
                        break;
                case EXTERN:
                        /* printf("GC about to free %x\n", p);  
                         * fflush(stdout);
                         */
                        if (getdesc(p)) { (*(getdesc(p)->free_meth))(getinst(p));
                        }
                        break;
                }
                p->n_type = FREE_NODE;
                rplaca(p,NIL);
                rplacd(p,fnodes);
                fnodes = p;
                nfree += 1L;
            } else {
#ifdef DEBUG_MEM
                /* added to find why sample blocks are not being freed - who's got them? */
                if (ntype(p) == EXTERN && strcmp(getdesc(p)->type_name, "SOUND") == 0) {
                    long snd_list_len(void *);
                    printf("gc found but did not free extern %p sound_type %p list len %ld\n",
                           p, getinst(p), snd_list_len(getinst(p)));
                }
#endif
                p->n_flags &= ~MARK;
            }
        }
    }
}

/* addseg - add a segment to the available memory */
LOCAL int addseg(void)
{
    SEGMENT *newseg;
    LVAL p;
    int n;

    /* allocate the new segment */
    if (anodes == 0 || (newseg = newsegment(anodes)) == NULL)
        return (FALSE);

    /* add each new node to the free list */
    p = &newseg->sg_nodes[0];
    for (n = anodes; --n >= 0; ++p) {
        rplacd(p,fnodes);
        fnodes = p;
    }

    /* return successfully */
    return (TRUE);
}

/* newsegment - create a new segment (only called here and in xlimage.c) */
SEGMENT *newsegment(int n)
{
    SEGMENT *newseg;

    /* allocate the new segment */
    if ((newseg = (SEGMENT *)calloc(1,segsize(n))) == NULL)
        return (NULL);

    /* initialize the new segment */
    newseg->sg_size = n;
    newseg->sg_next = NULL;
    if (segs)
        lastseg->sg_next = newseg;
    else
        segs = newseg;
    lastseg = newseg;

    /* update the statistics */
    total += (long)segsize(n);
    nnodes += (long)n;
    nfree += (long)n;
    ++nsegs;

    /* return the new segment */
    return (newseg);
}
 
/* stats - print memory statistics */
LOCAL void stats(void)
{
    sprintf(buf,"Nodes:       %ld\n",nnodes); stdputstr(buf);
    sprintf(buf,"Free nodes:  %ld\n",nfree);  stdputstr(buf);
    sprintf(buf,"Segments:    %d\n",nsegs);   stdputstr(buf);
    sprintf(buf,"Allocate:    %d\n",anodes);  stdputstr(buf);
    sprintf(buf,"Total:       %ld\n",total);  stdputstr(buf);
    sprintf(buf,"Collections: %d\n",gccalls); stdputstr(buf);
}

/* xgc - xlisp function to force garbage collection */
LVAL xgc(void)
{
    /* make sure there aren't any arguments */
    xllastarg();

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
LVAL xexpand(void)
{
    LVAL num;
    int n,i;

    /* get the new number to allocate */
    if (moreargs()) {
        num = xlgafixnum();
        n = (int) getfixnum(num);
    }
    else
        n = 1;
    xllastarg();

    /* allocate more segments */
    for (i = 0; i < n; i++)
        if (!addseg())
            break;

    /* return the number of segments added */
    return (cvfixnum((FIXTYPE)i));
}

/* xalloc - xlisp function to set the number of nodes to allocate */
LVAL xalloc(void)
{
    int n,oldn;
    LVAL num;

    /* get the new number to allocate */
    num = xlgafixnum();
    n = (int) getfixnum(num);

    /* make sure there aren't any more arguments */
    xllastarg();

    /* set the new number of nodes to allocate */
    oldn = anodes;
    anodes = n;

    /* return the old number */
    return (cvfixnum((FIXTYPE)oldn));
}

/* xmem - xlisp function to print memory statistics */
LVAL xmem(void)
{
    /* allow one argument for compatiblity with common lisp */
    if (moreargs()) xlgetarg();
    xllastarg();

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

/* xinfo - show information on control-t */
LVAL xinfo()
{
    char buf[80];

    sprintf(buf,"\n[ Free: %d, GC calls: %d, Total: %d",
            (int)nfree, (int)gccalls, (int)total);
    stdputstr(buf);
    print_local_gc_info();
    stdputstr("]\n");
    return NULL;
}


#ifdef SAVERESTORE
/* xsave - save the memory image */
LVAL xsave(void)
{
    unsigned char *name;

    /* get the file name, verbose flag and print flag */
    name = getstring(xlgetfname());
    xllastarg();

    /* save the memory image */
    return (xlisave((char *) name) ? s_true : NIL);
}

/* xrestore - restore a saved memory image */
LVAL xrestore(void)
{
    extern jmp_buf top_level;
    unsigned char *name;

    /* get the file name, verbose flag and print flag */
    name = getstring(xlgetfname());
    xllastarg();

    /* restore the saved memory image */
    if (!xlirestore((char *) name))
        return (NIL);

    /* return directly to the top level */
    stdputstr("[ returning to the top level ]\n");
    _longjmp(top_level,1);
}
#endif

static unsigned char registered_xlmshutdown = 0;
static void xlmshutdown(void);

/* xlminit - initialize the dynamic memory module */
void xlminit(void)
{
    LVAL p;
    int i;

    /* initialize our internal variables */
    segs = lastseg = NULL;
    nnodes = nfree = total = 0L;
    nsegs = gccalls = 0;
    anodes = NNODES;
    fnodes = NIL;

    /* allocate the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
        xlfatal("insufficient memory");

    /* initialize the fixnum segment */
    p = &fixseg->sg_nodes[0];
    for (i = SFIXMIN; i <= SFIXMAX; ++i) {
        p->n_type = FIXNUM;
        p->n_fixnum = i;
        ++p;
    }

    /* allocate the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
        xlfatal("insufficient memory");

    /* initialize the character segment */
    p = &charseg->sg_nodes[0];
    for (i = CHARMIN; i <= CHARMAX; ++i) {
        p->n_type = CHAR;
        p->n_chcode = i;
        ++p;
    }

    /* initialize structures that are marked by the collector */
    obarray = xlenv = xlfenv = xldenv = NIL;
    s_gcflag = s_gchook = NIL;

    /* allocate the evaluation stack */
    if ((xlstkbase = (LVAL **)malloc(EDEPTH * sizeof(LVAL *))) == NULL)
        xlfatal("insufficient memory");
    xlstack = xlstktop = xlstkbase + EDEPTH;

    /* allocate the argument stack */
    if ((xlargstkbase = (LVAL *)malloc(ADEPTH * sizeof(LVAL))) == NULL)
        xlfatal("insufficient memory");
    // printf("ADEPTH is %d\n", ADEPTH);
    xlargstktop = xlargstkbase + ADEPTH;
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;

    /* Guarantee graceful cleanup of memory */
    if (!registered_xlmshutdown) {
        atexit(xlmshutdown);
        registered_xlmshutdown = 1;
    }
}

static void xlmshutdown(void)
{
    /* This function deallocates all memory used by xlisp.  Should it 
       become non-static, allowing the client to shut down and init 
       again for a "hard" restart? */
 
    /* Free all lisp objects, and free the free-store */
    freeimage();

    /* Free the stacks */
    free(xlstkbase);
    xlstkbase = NULL;
    free(xlargstkbase);
    xlargstkbase = NULL;
}
