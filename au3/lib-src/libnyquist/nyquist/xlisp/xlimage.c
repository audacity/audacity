/* xlimage - xlisp memory image save/restore functions */
/*	Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use	*/

#include "stdlib.h"
#include "string.h"
#include "xlisp.h"

extern struct segment *segs, *lastseg;

void freeimage(void);

/* freeimage - free the current memory image */
void freeimage(void)
{
    SEGMENT *seg,*next;
    FILE *fp;
    LVAL p;
    int n;

    /* free the data portion of SYMBOL/VECTOR/OBJECT/STRING nodes */
    for (seg = segs; seg != NULL; seg = next) {
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p)
            switch (ntype(p)) {
            case SYMBOL:
            case OBJECT:
            case VECTOR:
            case CLOSURE:
                if (p->n_vsize)
                    free(p->n_vdata);
                break;
            case STRING:
                if (getslength(p))
                    free((void *) getstring(p));
                break;
            case STREAM:
                if ((fp = getfile(p)) && (fp != stdin && fp != stdout && fp != STDERR))
                    osclose(getfile(p));
                break;
            case EXTERN:
                /* note: currently, there are 2 EXTERN types: SEQ and SOUND */
                if (getdesc(p)) {
                    (*(getdesc(p)->free_meth))(getinst(p));
                }
                break;
            default:   /* note: SUBR, FSUBR, CONS, SYMBOL, FIXNUM, FLONUM,   */
                break; /* CHAR, USTREAM are ignored here because they do not */
                       /* point outside of the segments that are being freed */
            }
        next = seg->sg_next;
        free((void *) seg);
    }
    segs = lastseg = NULL;
}


#ifdef SAVERESTORE

/* external variables */
extern LVAL obarray,s_gchook,s_gcflag;
extern long nnodes,nfree;
extern int anodes,nsegs,gccalls;
extern struct segment *fixseg,*charseg;
extern XLCONTEXT *xlcontext;
extern LVAL fnodes;
extern struct xtype_desc_struct desc_table[NTYPES];

/* local variables */
static OFFTYPE off,foff;
static FILE *fp;

/* forward declarations */
LOCAL OFFTYPE readptr(void);
LOCAL OFFTYPE cvoptr(LVAL p);
LOCAL LVAL cviptr(OFFTYPE o);
LOCAL void writeptr(OFFTYPE off);
LOCAL void setoffset(void);
LOCAL void writenode(LVAL node);
LOCAL void readnode(int type, LVAL node);


/* xlisave - save the memory image */
int xlisave(const char *fname)
{
    char fullname[STRMAX+1];
    unsigned char *cp;
    SEGMENT *seg;
    int n,i,max;
    LVAL p;

    /* default the extension */
    if (needsextension(fname)) {
        strcpy(fullname,fname);
        strcat(fullname,".wks");
        fname = fullname;
    }

    /* open the output file */
    if ((fp = osbopen(fname,"w")) == NULL)
        return (FALSE);

    /* first call the garbage collector to clean up memory */
    gc();

    /* invalidate extern type descriptor symbol caches */
    inval_caches();	

    /* write out the pointer to the *obarray* symbol */
    writeptr(cvoptr(obarray));

    /* setup the initial file offsets */
    off = foff = (OFFTYPE)2;

    /* write out all nodes that are still in use */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p, off += 2)
            switch (ntype(p)) {
            case FREE_NODE:
                break;
            case CONS:
            case USTREAM:
                setoffset();
                osbputc(p->n_type,fp);
                writeptr(cvoptr(car(p)));
                writeptr(cvoptr(cdr(p)));
                foff += 2;
                break;
            case EXTERN:
                setoffset();
                osbputc(EXTERN, fp);
/*		printf("saving EXTERN p = %x, desc %x\n", p, getdesc(p)); fflush(stdout);*/
                writeptr((OFFTYPE) (getdesc(p) - desc_table)); /* write type index */
                writeptr((OFFTYPE) 0); /* pointer gets reconstructed on input */
                foff += 2;
                break;
            default:
                setoffset();
                writenode(p);
                break;
            }
    }

    /* write the terminator */
    osbputc(FREE_NODE,fp);
    writeptr((OFFTYPE)0);

    /* write out data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p)
            switch (ntype(p)) {
            case SYMBOL:
            case OBJECT:
            case VECTOR:
            case CLOSURE:
                max = getsize(p);
                for (i = 0; i < max; ++i)
                    writeptr(cvoptr(getelement(p,i)));
                break;
            case STRING:
                max = getslength(p);
                for (cp = getstring(p); --max >= 0; )
                    osbputc(*cp++,fp);
                break;
            case EXTERN:
/*		printf("saving extern data for p = %x\n", p);*/
                (*(getdesc(p)->save_meth))(fp, getinst(p));
                break;
            }
    }

    /* close the output file */
    osclose(fp);

    /* return successfully */
    return (TRUE);
}

/* xlirestore - restore a saved memory image */
int xlirestore(const char *fname)
{
    extern FUNDEF *funtab;
    char fullname[STRMAX+1];
    unsigned char *cp;
    int n,i,max,type;
    SEGMENT *seg;
    LVAL p;

    /* default the extension */
    if (needsextension(fname)) {
        strcpy(fullname,fname);
        strcat(fullname,".wks");
        fname = fullname;
    }

   /* open the file */
    if ((fp = osbopen(fname,"r")) == NULL)
        return (FALSE);

    /* free the old memory image */
    freeimage();

    /* initialize */
    off = (OFFTYPE)2;
    total = nnodes = nfree = 0L;
    fnodes = NIL;
    segs = lastseg = NULL;
    nsegs = gccalls = 0;
    xlenv = xlfenv = xldenv = s_gchook = s_gcflag = NIL;
    xlstack = xlstkbase + EDEPTH;
    xlcontext = NULL;

    /* create the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
        xlfatal("insufficient memory - fixnum segment");

    /* create the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
        xlfatal("insufficient memory - character segment");

    /* read the pointer to the *obarray* symbol */
    obarray = cviptr(readptr());

    /* read each node */
    while ((type = osbgetc(fp)) >= 0)
        switch (type) {
        case FREE_NODE:
            if ((off = readptr()) == (OFFTYPE)0)
                goto done;
            break;
        case CONS:
        case USTREAM:
            p = cviptr(off);
            p->n_type = type;
            p->n_flags = 0;
            rplaca(p,cviptr(readptr()));
            rplacd(p,cviptr(readptr()));
            off += 2;
            break;
        case EXTERN:
            p = cviptr(off);
/*	    printf("reading extern node p = %x\n", p);*/
            p->n_type = EXTERN;
            setdesc(p, desc_table + (int) readptr());
/*	    printf("type desc is %x\n", getdesc(p));*/
            setinst(p, (unsigned char *) readptr());
/*	    printf("initial inst is %x\n", getinst(p));*/
            off += 2;
            break;
        default:
            readnode(type,cviptr(off));
            off += 2;
            break;
        }
done:

    /* read the data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p)
            switch (ntype(p)) {
            case SYMBOL:
            case OBJECT:
            case VECTOR:
            case CLOSURE:
                max = getsize(p);
                if ((p->n_vdata = (LVAL *)malloc(max * sizeof(LVAL))) == NULL)
                    xlfatal("insufficient memory - vector");
                total += (long)(max * sizeof(LVAL));
                for (i = 0; i < max; ++i)
                    setelement(p,i,cviptr(readptr()));
                break;
            case STRING:
                max = getslength(p);
                if ((p->n_string = (unsigned char *)malloc(max)) == NULL)
                    xlfatal("insufficient memory - string");
                total += (long)max;
                for (cp = getstring(p); --max >= 0; )
                    *cp++ = osbgetc(fp);
                break;
            case STREAM:
                setfile(p,NULL);
                break;
            case SUBR:
            case FSUBR:
                p->n_subr = funtab[getoffset(p)].fd_subr;
                break;
            case EXTERN:
/*		printf("restoring extern %x\n", p); fflush(stdout); */
                setinst(p, (*(getdesc(p)->restore_meth))(fp));
                break;
            }
    }

    /* close the input file */
    osclose(fp);

    /* collect to initialize the free space */
    gc();

    /* lookup all of the symbols the interpreter uses */
    xlsymbols();

    /* return successfully */
    return (TRUE);
}

/* setoffset - output a positioning command if nodes have been skipped */
LOCAL void setoffset(void)
{
    if (off != foff) {
        osbputc(FREE_NODE,fp);
        writeptr(off);
        foff = off;
    }
}

/* writenode - write a node to a file */
LOCAL void writenode(LVAL node)
{
    char *p = (char *)&node->n_info;
    int n = sizeof(union ninfo);
    osbputc(node->n_type,fp);
    while (--n >= 0)
        osbputc(*p++,fp);
    foff += 2;
}

/* writeptr - write a pointer to a file */
LOCAL void writeptr(OFFTYPE off)
{
    char *p = (char *)&off;
    int n = sizeof(OFFTYPE);
    while (--n >= 0)
        osbputc(*p++,fp);
}

/* readnode - read a node */
LOCAL void readnode(int type, LVAL node)
{
    char *p = (char *)&node->n_info;
    int n = sizeof(union ninfo);
    node->n_type = type;
    node->n_flags = 0;
    while (--n >= 0)
        *p++ = osbgetc(fp);
}

/* readptr - read a pointer */
LOCAL OFFTYPE readptr(void)
{
    OFFTYPE off;
    char *p = (char *)&off;
    int n = sizeof(OFFTYPE);
    while (--n >= 0)
        *p++ = osbgetc(fp);
    return (off);
}

/* cviptr - convert a pointer on input */
LOCAL LVAL cviptr(OFFTYPE o)
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;

    /* check for nil */
    if (o == (OFFTYPE)0)
        return ((LVAL)o);

    /* compute a pointer for this offset */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        if (o >= off && o < off + (OFFTYPE)(seg->sg_size << 1))
            return (seg->sg_nodes + ((int)(o - off) >> 1));
        off += (OFFTYPE)(seg->sg_size << 1);
    }

    /* create new segments if necessary */
    for (;;) {

        /* create the next segment */
        if ((seg = newsegment(anodes)) == NULL)
            xlfatal("insufficient memory - segment");

        /* check to see if the offset is in this segment */
        if (o >= off && o < off + (OFFTYPE)(seg->sg_size << 1))
            return (seg->sg_nodes + ((int)(o - off) >> 1));
        off += (OFFTYPE)(seg->sg_size << 1);
    }
}

/* cvoptr - convert a pointer on output */
LOCAL OFFTYPE cvoptr(LVAL p)
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;

    /* check for nil and small fixnums */
    if (p == NIL)
        return ((OFFTYPE)p);

    /* compute an offset for this pointer */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        if (CVPTR(p) >= CVPTR(&seg->sg_nodes[0]) &&
            CVPTR(p) <  CVPTR(&seg->sg_nodes[0] + seg->sg_size))
            return (off + (OFFTYPE)((p - seg->sg_nodes) << 1));
        off += (OFFTYPE)(seg->sg_size << 1);
    }

    /* pointer not within any segment */
    xlerror("bad pointer found during image save",p);
    /* this point will never be reached because xlerror() does a
       _longjmp(). The return is added to avoid false positive 
       error messages from static analyzers and compilers */
    return ((OFFTYPE)NIL);
}

#endif

