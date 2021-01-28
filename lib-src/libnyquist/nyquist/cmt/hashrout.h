/* hashrout.h -- Rubine's hash table package */
/* Copyright 1989 Carnegie Mellon University */

/* ChangeLog:
 *    2-jan-85 rbd    Added option to count entries: define COUNTER and
 *          HASHENTER routine will increment it on new entries
 *   28-Apr-03 DM     Explicit declaration of int type for HASHENTER() 
 */

/*
 * Generic symbol table functions
 *
 * After writing this code over for a bunch of interpreters
 * I think I know how to do it once and for all, generally
 * enough for most application.
 * There are enough settable parameters to suit anyone, and
 * the defaults usually do something sane.
 *
 * The basic idea is that you have a bunch of symbol table entries
 * that need to be entered or looked up given a character string.
 * Symbol table entries are usually structures, and one element
 * of the structure must be the character string of the key.
 * The structure should be given a type name, and these routines
 * are informed of the type name via
 *    #define    HASHTYPE    type-name-of-symbol-table-entry
 * You must inform the routines how to access the character string
 * given the symbol table entry; for example
 *    #define HASHELEM(p)    ((p).name)
 * There are two size parameters - the number of different hash
 * values and the number of symbol table entries to allocate at one
 * time.  Both default to 256.
 *    #define    HASHVAL        128
 *    #define    HASHENTRIES    512
 * The name of the function that performs both entry and lookup is
 * enter(name) - it takes one argument, a character string.  The name
 * of the function may be changed via
 *    #define HASHENTER    new-name-for-enter-routine
 * Note that if there is no entry in the table for name, name is entered
 * automatically.  The returned structure will have only the name
 * field filled in; the rest of the fields are guarenteed to be zero
 * so an application can check if this is the first time name was entered.
 * If HASHPOINT is defined, the enter routine returns a pointer to a hash
 * table entry.  In the default case, i.e. HASHPOINT undefined, the
 * enter routine returns an integer between zero and HASHENTRIES.
 * The macro HASHENTRY(i) will, given that integer, return the
 * table element (not a pointer to it), so for example,
 *    HASHENTRY(enter(x)).name == x.
 * Any file that wishes to use HASHENTRY should have at the top
 *    #define    HASHTYPE    type-name-of-symbol-table-entry
 *    #include "hash.h" 
 * Note in this case at most HASHENTRIES entries will be allocated
 * before hash table overflow.  If HASHPOINT is defined, allocations
 * will take place until memory runs out.
 * By default, hash strings are copied into space obtained from malloc
 * before being placed in new entry.  This copying can be supressed
 * by defining HASHNOCOPY.
 * The following is an example of using the hash table stuff.

typedef struct {
    char    *n_name;
    int    n_value;
} symbolentry;

#define    HASHTYPE    symbolentry
#define HASHELEM(p)    ((p).n_name)
#define HASHENTRIES    1024

#include "hashrout.h"

*/


/*
 * OK, now the meat.
 */

#ifndef    HASHTYPE
#    include    "-- HASHTYPE undefined"
#endif

#ifndef    HASHELEM
#    include    "-- HASHELEM undefined"
#endif

#ifndef    HASHVAL
#    define    HASHVAL    256
#endif

#ifndef    HASHENTRIES
#    define    HASHENTRIES    256
#endif

#ifndef    HASHENTER
#    define    HASHENTER    enter
#endif

/*
 * HASHNOCOPY, HASHPOINT are undefined by default
 */

/*
 * get definition of hash elem structure
 */

#ifndef HASHENTRY
#    include    "hash.h"
#endif

/*
 * Table of pointers, indexed by hash values
 */

hashelem    *hashtab[HASHVAL];

/*
 * First chunk of elements, pointer to the start,
 * and index for allocation
 */

hashelem    hashfirstchunk[HASHENTRIES];
hashelem    *hashchunk = hashfirstchunk;
int     hashindex = 0;

/*
 * stdio.h if we don't have it yet
 */

#ifndef _NFILE
#    include <stdio.h>
#endif

/*
 * Declare counter if necessary
 */

#ifdef COUNTER
extern COUNTER;
#endif

/*
 * The enter routine
 */

#ifdef HASHPOINT
HASHTYPE *
#else
intptr_t
#endif
HASHENTER (s)
char *s;
{
    register int i, hash;
    register hashelem *elem;

    /*
     * Compute s's hash value
     * I really should look up some good hash functions, but
     * I haven't bothered.
     */

    for (i = 0, hash = 0; s[i] != '\0' && i < 15; i++)
        hash += (i + 1) * s[i];
    hash %= HASHVAL;

    /*
     * search for s in the table
     */

    for (elem = hashtab[hash]; elem != NULL; elem = elem->h_next)
        if (strcmp(s, HASHELEM((elem->h_elem))) == 0) {    /* found it */
#ifdef HASHPOINT
            return &elem->h_elem;
#else
            return (int) (elem - hashfirstchunk);
#endif
        }

    if (hashindex >= HASHENTRIES) {
#ifdef HASHPOINT
        char *calloc();

        hashindex = 0;
        hashchunk = (hashelem *) calloc(HASHENTRIES, sizeof(hashelem));
        if (hashchunk == NULL) {
            gprintf(FATAL, "No mem for hash symbol table\n");
            EXIT(1);
#ifdef COUNTER
            COUNTER++; /* optional symbol counter */
#endif
        }
#else
        gprintf(FATAL, "No hash table space, increase HASHENTRIES\n");
        EXIT(1);
#endif
    }

    /*
     * Splice a new entry into the list and fill in the string field
     */

    elem = &hashchunk[hashindex++];
    elem->h_next = hashtab[hash];
    hashtab[hash] = elem;

#ifdef HASHNOCOPY
    HASHELEM((elem->h_elem)) = s;
#else
    {
        char *strcpy();
        HASHELEM((elem->h_elem)) = memget((strlen(s) + 1));
        strcpy(HASHELEM((elem->h_elem)), s);
    }
#endif

#ifdef HASHPOINT
    return &elem->h_elem;
#else
    return (int) (elem - hashfirstchunk);
#endif
}
