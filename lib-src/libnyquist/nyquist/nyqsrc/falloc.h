/*
 * falloc.h
 * nyquist memory allocation data structures and macros
 * 
 * there is an falloc and ffree for each major type of data structure
 * there is an falloc and ffree for generic (not so common) structures
 * there is an frelease for some structures.  this reduces the 
 *      reference count for the particular structure by 1; it 
 *      does not continue recursively.
 */

/* Debugging support:
 * When DEBUG_MEM is set, each piece of allocated storage will contain
 * a pointer to a string naming the caller or other allocation info,
 * and a sequence number.  (8 extra bytes are allocated for this info).
 *
 * When storage is freed, the ID is set to NULL, and the routine
 * dbg_mem_check(ptr) will abort if ID is NULL.  Call this routine to
 * avoid following a pointer to data that was previously freed.
 *
 * The goal of this support is to allow you to "go back" to the point
 * where memory is corrupted; specifically where a memory block is freed
 * too early.
 *
 * When a memory-related bug is crashing the system:
 * (1) Recompile with DEBUG_MEM on.
 * (2) Develop some Nyquist code that will predictably crash the system.
 * (3) When Nyquist crashes, use a debugger to find where the bad
 *   pointer came from.  See if the source of the pointer was freed.
 * (4) If the source of the pointer was freed, then notice the sequence
 *   number.
 * (5) Rerun with dbg_mem_seq_num set to the number noted in (4).
 * (6) Nyquist will print when the storage in question was allocated and
 *   freed.  Use the debugger to find out why the storage is
 *   freed too early and who did it.
 * (7) If the source of the pointer in (3) was not freed, you're on your
 *   own.
 *
 * The DEBUG_MEM related routines are:
 *    dbg_mem_allocated: called when memory is allocated
 *    dbg_mem_freed: called when memory is freed
 *    dbg_mem_released: called when memory is released
 *    dbg_mem_check: called to check memory
 *
 * see also xldmem.c:
 * by setting xldmem_trace to a pointer, you can trace when the
 * pointer is referenced by anything in the heap
 */


/* to get size_t on pmax: */
#ifdef pmax
#include "sys/types.h"
#endif

#include "cque.h"
#include "debug.h"

#define DEBUG_MEM 0
#define DEBUG_MEM_INFO_SIZE (sizeof(long) + sizeof(char *))

/* special free lists */
extern CQUE *sample_block_free; /* really a sample_block_type */

/* special counts */
extern int sample_block_total;
extern int sample_block_used;
extern int snd_list_used;
extern int sound_used;
extern long table_memory;

/* generic free lists */
#define MAXLISTS 128
extern CQUE *generic_free[MAXLISTS];

/* general memory pool */
#define MAXPOOLSIZE 1000000
extern char *poolp;
extern char *poolend;

/* sample block memory pool */
#define MAXSPOOLSIZE (256 * round_size(sizeof(sample_block_node)))
extern char *spoolp;
extern char *spoolend;

extern int npools;
extern int sample_blocks_since_gc;

#if !defined(TRACK_POOLS)
#define TRACK_POOLS 1
#endif

#if defined(TRACK_POOLS) && TRACK_POOLS
// extern CQUE *pools;
void falloc_gc(void);
#endif

void falloc_init(void);
void new_pool(void);
void new_spool(void);
void find_sample_block(sample_block_type *sp);

char *get_from_pool(size_t siz);

#define round_size(n)  (((n) + 7) & ~7)

/* check_pool -- returns true if enough bytes are available */
#if DEBUG_MEM
#define check_pool(size) (poolp + (size) + DEBUG_MEM_INFO_SIZE <= poolend)
#define check_spool(size) (spoolp + (size) + DEBUG_MEM_INFO_SIZE <= spoolend)
#define DBG_MEM_ALLOCATED(p, who) dbg_mem_allocated(p, who)
#define DBG_MEM_FREED(p, who) dbg_mem_freed(p, who)
#define DBG_MEM_RELEASED(p, who) dbg_mem_released(p, who)
#else
#define check_pool(size) (poolp + (size) <= poolend)
#define check_spool(size) (spoolp + (size) <= spoolend)
#define DBG_MEM_ALLOCATED(p, who)
#define DBG_MEM_FREED(p, who)
#define DBG_MEM_RELEASED(p, who)
#endif

#define BLOCKS_PER_GC 100

/* There used to be a lot of code in this macro. I moved it to
 * find_sample_block, but kept the macro mainly in order to pass sp
 * by reference.
 */
#define falloc_sample_block(sp, who) { \
    find_sample_block(&sp); \
    DBG_MEM_ALLOCATED(sp, who); }


#define ffree_sample_block(sp, who) { \
    /* printf("freeing sample_block@%x\n", sp); */ \
    DBG_MEM_FREED(sp, who); \
    Qenter(sample_block_free, sp); \
    sample_block_used--; \
}

#define frelease_sample_block(sp, who) { \
    sp->refcnt--; \
    DBG_MEM_RELEASED(sp, who); \
    if (sp->refcnt <= 0) { \
        ffree_sample_block(sp); \
    } \
}


/* NOTE: This must not cause garbage collection.
 * LVAL parameters to snd_make_? functions are not
 * protected and falloc_sound is invoked there.
 */
#define snd_list_free (generic_free[round_size(sizeof(snd_list_node)) >> 3])

#define falloc_snd_list(sp, who) {  \
    if (!Qempty(snd_list_free)) \
        Qget(snd_list_free, snd_list_type, sp) \
    else \
        sp = (snd_list_type)get_from_pool(round_size(sizeof(snd_list_node)));\
    snd_list_used++; \
    DBG_MEM_ALLOCATED(sp, who); \
}


#define ffree_snd_list(sp, who) { \
    DBG_MEM_FREED(sp, who); \
    Qenter(snd_list_free, sp); \
    snd_list_used--; \
}


#define frelease_snd_list(sp, who) { \
    sp->refcnt--; \
    DBG_MEM_RELEASED(sp, who); \
    if (sp->refcnt <= 0) { \
        ffree_snd_list(sp, who); \
    } \
}


#define sound_free (generic_free[round_size(sizeof(sound_node)) >> 3])

#define NORMALSOUNDALLOC
#ifdef NORMALSOUNDALLOC
#define falloc_sound(sp, who) {  \
    if (!Qempty(sound_free)) { \
        Qget(sound_free, sound_type, sp); \
    } else { \
        sp = (sound_type) get_from_pool(round_size(sizeof(sound_node))); \
    } \
    sound_used++; \
    DBG_MEM_ALLOCATED(sp, who); \
}
#else
#define falloc_sound(sp) \
    sp =(sound_type) \
        get_from_pool(round_size(sizeof(sound_node)))
#endif

/* note: usually you call sound_unref, not this macro */
#define ffree_sound(sp, who) { \
/*    sound_already_free_test(); */ \
    DBG_MEM_FREED(sp, who); \
    Qenter(sound_free, sp); \
    sound_used--; \
}


/* falloc_generic -- sp gets new node of type sptype */
/**/
#define falloc_generic(sp, sptype, who) { \
    int size = round_size(sizeof(sptype)); \
    falloc_generic_bytes(sp, sptype, size, who) }

/* falloc_generic_n -- sp gets new array of n sptype's */
/**/
#define falloc_generic_n(sp, sptype, n, who) { \
    int min_size = sizeof(sptype) * (n); \
    int size = round_size(min_size); \
    falloc_generic_bytes(sp, sptype, size, who) }

#define falloc_generic_bytes(sp, sptype, size, who) \
    if ((size >> 3) >= MAXLISTS) { \
        stdputstr("falloc_generic problem\n"); \
        sp = (sptype *) malloc(size); \
    } else if (!Qempty(generic_free[size >> 3])) { \
        Qget(generic_free[size >> 3], sptype *, sp); \
    } else { \
        sp = (sptype *) get_from_pool(size); \
    } \
    DBG_MEM_ALLOCATED(sp, who); \
/*    printf("GENERIC ALLOC %x\n", sp);  */


/* ffree_generic puts an item back on proper freelist */
/* NOTE: sIzE is capitalized funny so that it will not
 * match an actual parameter, e.g. if the caller writes
 * ffree_generic(ptr, size), we don't want the expanded
 * code to include: "int size = round_size(size) >> 3"!
 */
#define ffree_generic(sp, nn, who) { \
    int sIzE = round_size(nn) >> 3; \
    DBG_MEM_FREED(sp, who); \
    /* printf("GENERIC FREE %x SIZE %d\n", sp, nnn); */ \
    if ((sIzE) >= MAXLISTS) { \
        free(sp); \
    } else { \
        Qenter(generic_free[sIzE], sp); \
    } \
}
