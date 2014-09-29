/* mem.c -- fast memory allocation/deallocation module */

/* Allocate large chunks of memory using malloc.  From the chunks,
   allocate memory as needed on long-word boundaries.  Memory is
   freed by linking memory onto a freelist.  An array of freelists,
   one for each size, is maintained and checked before going to the
   chunck for more memory.  The freelist array only holds lists of
   nodes up to a certain size.  After that, malloc is used directly.
 */
/* CHANGE LOG
 ----------------------------------------------------------------------
   28-Apr-03 | DM  : fix #includes for portability
 ----------------------------------------------------------------------
 */

#include "switches.h"

#include <stdio.h>
#include <stdlib.h>

#include "cext.h"
#include "userio.h"

/* how many bytes in the largest node managed in mem_free_list array */
#define MAX_SIZE_FOR_FREELIST 256

long *mem_free_list[MAX_SIZE_FOR_FREELIST/4];

#define MEM_CHUNK_SIZE 4096
char *mem_chunk;
long mem_chunk_remaining = 0;

void meminit()
{
    int i;
    for (i = 0; i < MAX_SIZE_FOR_FREELIST/4; i++) {
        mem_free_list[i] = NULL;
    }
}


void *memget(register size_t size)
{
    if (size > MAX_SIZE_FOR_FREELIST) {
/*		gprintf(TRANS, "memget calling MALLOC\n"); */
        return MALLOC(size);
    } else {
        long **p = mem_free_list + ((size - 1) >> 2);
        if (*p) {
            register long *result = *p;
            *p = (long *) *result;
/*			gprintf(TRANS, "memget->%lx\n", result); */
            return (char *) result;
        } else if ((size_t) mem_chunk_remaining >= size) {
            register char *result = mem_chunk;
            size = (size + 3) & ~3; /* round up to multiple of 4 */
            mem_chunk += size;
            mem_chunk_remaining -= size;
/*			gprintf(TRANS, "memget->%lx\n", result); */
            return result;
        /* note that we throw away remaining chunk when there isn't enough */
        } else if ((mem_chunk = (char *) MALLOC(MEM_CHUNK_SIZE))) {
            register char *result = mem_chunk;
/*			gprintf(TRANS, "mem_chunk at %lx\n", mem_chunk); */
            size = (size + 3) & ~3; /* round up to multiple of 4 */
            mem_chunk += size;
            mem_chunk_remaining = MEM_CHUNK_SIZE - size;
/*			gprintf(TRANS, "memget->%lx\n", result); */
            return result;
        } else {
            return NULL;
        }
    }
}


void memfree(register void *ptr, register size_t size)
{
    register long **p = (long **) ptr;
    if (size > MAX_SIZE_FOR_FREELIST) {
        FREE(ptr);
    } else {
        register long **head_ptr = mem_free_list + ((size - 1) >> 2);
        *p = *head_ptr;
        *head_ptr = (long *) p;
    }
}



            
