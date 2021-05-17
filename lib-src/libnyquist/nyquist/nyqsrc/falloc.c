/*
 * falloc.c
 * data for Nyquist memory allocation.
 */

#include <stdio.h>
#include <assert.h>
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"

/* special free lists */
CQUE *sample_block_free = NULL;    /* really a sample_block_type */

/* special counts */
int sample_block_used = 0;
int sample_block_low_water = 0;
int sample_block_total = 0;
int snd_list_used = 0;
int sound_used = 0;

/* generic free lists */
CQUE *generic_free[MAXLISTS];


void falloc_init(void)
{
    int i;
    for (i = 0; i < MAXLISTS; i++) generic_free[i] = NULL;
}


/* memory pool */
char *poolp = NULL;
char *poolend = NULL;

/* sample block memory pool */
char *spoolp = NULL;
char *spoolend = NULL;

int npools = 0;

#if defined(TRACK_POOLS) && TRACK_POOLS
#define POOL_HEAD_SIZE (round_size(sizeof(CQUE)))
CQUE *pools = NULL;
#endif

void sound_already_free_test(sound_type s)
{
    sound_type sp;
    for (sp = (sound_type) sound_free; sp; sp = (sound_type) ((CQUE *) sp)->qnext) {
        if (s == sp) {
            stdputstr("SOUND ALREADY FREE!!!");
            fflush(stdout);
            sp = 0; sp->list = 0;   /* trap to debugger */
        }
    }
}


/* new_pool -- allocate a new pool from which mem is allocated */
/**/
void new_pool(void)
{
    poolp = (char *) malloc(MAXPOOLSIZE);

    if (poolp == NULL) {
        fprintf(STDERR, "Nyquist: out of memory!\n");
        EXIT(1);
    }

    poolend = poolp + MAXPOOLSIZE;
    npools++;
    /* stick to double word boundaries */
    poolp = (char *) round_size(((intptr_t) poolp));
}

/* new_spool -- allocate a new spool from which sample blocks are allocated */
/**/
void new_spool(void)
{
#if defined(TRACK_POOLS) && TRACK_POOLS
    spoolp = (char *) malloc(MAXSPOOLSIZE + POOL_HEAD_SIZE);
#else
    spoolp = (char *) malloc(MAXSPOOLSIZE);
#endif

    if (spoolp == NULL) {
        fprintf(STDERR, "Nyquist: out of memory!\n");
        EXIT(1);
    }

#if defined(TRACK_POOLS) && TRACK_POOLS
    Qenter(pools, spoolp);
    spoolp += POOL_HEAD_SIZE;
#endif

    spoolend = spoolp + MAXSPOOLSIZE;
    npools++;
    /* stick to double word boundaries */
    spoolp = (char *) round_size(((intptr_t) spoolp));
}


/* find_sample_block -- get sample block when freelist is empty */
/* Try these strategies in order:
   1) try free list
   2) use pool to get sample_blocks_low_water + BLOCKS_PER_GC blocks or until
      pool runs out
   3) GC and try free list again, set sample_blocks_low_water to 
      sample_blocks_used
   4) try pool again
   5) allocate new pool and use it
 */
void find_sample_block(sample_block_type *sp)
{
    if (!Qempty(sample_block_free)) {
        Qget(sample_block_free, sample_block_type, *sp);
    } else if (sample_block_total < sample_block_low_water + BLOCKS_PER_GC &&
               sample_block_total < max_sample_blocks &&
               check_spool(round_size(sizeof(sample_block_node)))) {
        if (DEBUG_MEM) spoolp += DEBUG_MEM_INFO_SIZE;
        *sp = (sample_block_type) spoolp;
        spoolp += round_size(sizeof(sample_block_node));
        sample_block_total++;
    } else {
        gc();
        sample_block_low_water = sample_block_used;
        if (!Qempty(sample_block_free)) {
            Qget(sample_block_free, sample_block_type, *sp);
        } else if (sample_block_used >= max_sample_blocks) {
            /* we are not allowed to allocate more */
            stdputstr("The maximum number of sample blocks has been\n");
            stdputstr("reached, so audio computation must be terminated.\n");
            stdputstr("Probably, your program should not be retaining\n");
            stdputstr("so many samples in memory. You can get and set\n");
            stdputstr("the maximum using SND-SET-MAX-AUDIO-MEM.\n");
            xlfail("audio memory exhausted");
        } else if (check_spool(round_size(sizeof(sample_block_node)))) {
            if (DEBUG_MEM) spoolp += DEBUG_MEM_INFO_SIZE;
            *sp = (sample_block_type) spoolp;
            spoolp += round_size(sizeof(sample_block_node));
            sample_block_total++;
        } else if (sample_block_used < max_sample_blocks) {
            new_spool();
            if (DEBUG_MEM) spoolp += DEBUG_MEM_INFO_SIZE;
            *sp = (sample_block_type) spoolp;
            spoolp += round_size(sizeof(sample_block_node));
            sample_block_total++;
        }
    }
    (*sp)->refcnt = 1;   \
    sample_block_used++; \
}



/* get_from_pool -- return size bytes from pool memory */
/**/
char *get_from_pool(size_t siz)
{
    if (!check_pool(siz)) {
        new_pool();
    }
    poolp += siz;
    if (DEBUG_MEM) poolp += DEBUG_MEM_INFO_SIZE; /* allow for debug info */
    return poolp - siz;
}


#if defined(TRACK_POOLS) && TRACK_POOLS

/* falloc_gc -- return empty pools to the system */
/*
 * Algorithm: for each pool, move all free sample blocks 
 * (on the sample_block_free list) to tlist. If tlist
 * has ALL of the blocks in the pool (determined by
 * byte counts), the pool is returned to the heap.
 */
void falloc_gc()
{
   CQUE *lp = NULL;
   CQUE *cp;
   CQUE *np;
   CQUE *tlist = NULL;

   /* Scan all allocated pools */
   for (cp = pools; cp; lp = cp, cp = np) {
      char *str = ((char *)cp) + POOL_HEAD_SIZE;
      char *end = str + MAXSPOOLSIZE;
      intptr_t tsiz = end - str;
      long csiz = 0;
      CQUE *tsave = NULL;
      CQUE *ln = NULL;
      CQUE *cn;
      CQUE *nn;

      /* Save pointer to next pool */
      np = cp->qnext;

      /* Remember head of temp free list */
      tsave = tlist;

      /* Scan all nodes on the free list */
      for (cn = sample_block_free; cn; ln = cn, cn = nn) {

         /* Get next node */
         nn = cn->qnext;

         /* Count it if the node belongs to this pool */
         if (cn >= (CQUE *) str && cn <= (CQUE *) end) {
            csiz += round_size(sizeof(sample_block_node));

            Qenter(tlist, cn);

            /* Unlink the node */
            if (cn == sample_block_free) {
               sample_block_free = nn;
               cn = NULL;
            }
            else {
               ln->qnext = nn;
               cn = ln;
            }
         }
      }

      /* The pool had inuse nodes */
      if (csiz != tsiz) {
         continue;
      }
   
      /* Remove the nodes from the temp free list */
      tlist = tsave;

      /* Maintain stats */
      sample_block_total -= (int)
              (tsiz / round_size(sizeof(sample_block_node)));
      npools--;

      /* If this is the active pool, then reset current pointers */
      if (spoolp >= str && spoolp <= end) {
         spoolp = NULL;
         spoolend = NULL;
      }

      /* Release the pool to the system */
      free(cp);

      /* Unlink this pool from the list */
      if (cp == pools) {
         pools = np;
         cp = NULL;
      }
      else {
         /* lp cannot be null here: On 1st iteration, lp == NULL, but
          * cp == pools, so code above is executed. Before the for-loop
          * iterates, pools == np (assigned above), and cp == NULL. The
          * for-loop update (lp=cp,cp=np) produces lp == NULL, cp == pools.
          * Since cp == pools, this else branch will not be taken.
          * The other path to this code is via the "continue" above. In that
          * case, the update (lp=cp,cp=np) makes lp a valid pointer or else
          * the loop exits.
          * The assert(lp) is here to possibly make static analyzers happy.
          */
         assert(lp);
         lp->qnext = np;
         cp = lp;
      }
   }

   /* Resave list of free nodes */
   sample_block_free = tlist;
}

#endif


