/* timebase.c -- management of calls, time bases and heaps for moxc */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
*  2-Apr-91 | JDW : further changes
* 21-Mar-92 | GWL : abort recovery
* 28-Apr-03 |  DM : true->TRUE
*****************************************************************************/


#include "stdio.h"
#include "cext.h"
#include "userio.h"
#include "midifns.h"
#include "timebase.h"
#include "moxc.h"


timebase_type timebase_queue = NULL;    /* waiting to run timebase queue */
call_type callfree = NULL;      /* free list */

private void fatal(const char *msg);


/****************************************************************************
*                           timebase_create
* Inputs:
*       maxsize is the initial size of the heap
* Outputs:
*       returns an initialized timebase_type
****************************************************************************/

timebase_type timebase_create(maxsize)
  int maxsize;
{
   static char *error_msg = "Out of memory in timebase_create()";
    timebase_type base = (timebase_type) memget(sizeof(timebase_node));
    if (!base) fatal(error_msg);
    base->next = NULL;
    base->next_time = MAXTIME;
    base->virt_base = 0L;
    base->real_base = 0L;
    base->rate = 256L;
    base->heap_size = 0;
    base->heap_max = maxsize;
    base->heap = (call_type *) memget(sizeof(call_type) * maxsize);
    if (!base->heap) fatal(error_msg);
    return base;
}

/****************************************************************************
*                           callinsert
* Inputs:
*       timebase_type base: the time base and heap
*       call_type call: the call to insert in heap
* Outputs:
        none
* Implementation:
*       linear insertion; to be changed to heap
****************************************************************************/

void callinsert(base, call)
  timebase_type base;
  call_type call;
{
    int i;
    register call_type *heap = base->heap;

    /* handle overflow -- this should never be executed if the user 
     * gives the right initial heap size 
     */
    base->heap_size++;
    if (base->heap_size >= base->heap_max) {
        call_type *new_heap = (call_type *)
            memget((base->heap_max << 1) * sizeof(call_type));
        int i;
        call_type *oldptr;
        call_type *newptr;
        if (!new_heap) {
            gprintf(TRANS, "Out of space, can't allocate new heap\n");
            EXIT(1);
        }

        oldptr = base->heap;
        newptr = new_heap;

        for (i = base->heap_max; i > 0; i--) *newptr++ = *oldptr++;
        memfree((char *) heap, base->heap_max * sizeof(call_type));
        base->heap = heap = new_heap;
        base->heap_max = (base->heap_max << 1);
    }

    /* now do the heap insert */
    i = base->heap_size;
    while (i > 1) {
        int parent = i >> 1;
        if (heap[parent]->u.e.time < call->u.e.time ||
            (heap[parent]->u.e.time == call->u.e.time &&
             heap[parent]->u.e.priority <= call->u.e.priority)) break;
        heap[i] = heap[parent];
        i = parent;
    }
    heap[i] = call;

    /* if next_time might change, reinsert base into queue */
    if (heap[1] == call) {
        remove_base(base);
        insert_base(base);
    }
}

/****************************************************************************
*                               callshow
* Inputs:
*       calltype call: the call to show
* Effect: 
*       prints a description of call
* Assumes:
*       call is not null
****************************************************************************/
void callshow(call)
    call_type call;
{
    int i;
    gprintf(TRANS,"address:  %p\n", call);
    gprintf(TRANS,"time:     %ld\n", call->u.e.time);
    gprintf(TRANS,"routine:  %p\n", call->u.e.routine);
    gprintf(TRANS,"parameters:");
    for (i = 0; i < MAX_CALL_ARGS; i++) {
        gprintf(TRANS, " %p", call->u.e.p.arg[i]);
    }
    gprintf(TRANS, "\n");
}

/***************************************************************
*                           fatal
*
* Input : msg: a message to be displayed
* Effect: print message and exit program
***************************************************************/

private void fatal(const char *msg)
{
    gprintf(FATAL, msg);
    EXIT(1);
}

/***************************************************************
*                           timebase_free
*
* Input : a time base
* Effect: deallocate the time base
***************************************************************/

void timebase_free(timebase)
  timebase_type timebase;
{
    remove_base(timebase);
    if (timebase->heap) {
        memfree((char *) timebase->heap,
                (timebase->heap_max * sizeof(call_type)));
    }
    memfree((char *) timebase, sizeof(timebase_node));
}

/***************************************************************
*                           insert_base
*
* Input : a time base not in the list 
* Effect: insert timebase at the appropriate spot in the list
*       computes the next_time field from the top of the heap
***************************************************************/

void insert_base(timebase)
  timebase_type timebase;
{
    register timebase_type *ptr = &timebase_queue;
    register time_type next_time = MAXTIME;
    /* compute next_time */
    if (timebase->heap_size != 0) {
        register call_type call = timebase->heap[1];
        /* virt to real calculation */
        next_time = (virt_to_real_256(timebase, call->u.e.time) &
                     0xFFFFFF00) + call->u.e.priority;
/*      gprintf(TRANS,
                "insert next_time is %ld, vt %ld, rb %ld, vb %ld rt %ld\n",
                next_time, timebase->heap[1]->u.e.time,
                timebase->real_base, timebase->virt_base, timebase->rate);
 */
    }
    timebase->next_time = next_time;

    if (next_time != MAXTIME) {
        /* insert into the list */
        while (TRUE) {
            if (! *ptr) {
                *ptr = timebase;
                timebase->next = NULL;
                return;
            } else if ((*ptr)->next_time >= next_time) {
                timebase->next = *ptr;
                *ptr = timebase;
                return;
            } else ptr = &((*ptr)->next);
        }
    }
}

/***************************************************************
*                           remove_base
*
* Input : timebase
* Effect: if timebase is in the queue, remove it
***************************************************************/

void remove_base(timebase)
  timebase_type timebase;
{
    timebase_type *ptr = &timebase_queue;
    while (*ptr) {
        if (*ptr == timebase) {
            *ptr = timebase->next;
            return;
        } else ptr = &((*ptr)->next);
    }
}

/***************************************************************
*                           remove_call
*
* Input : a timebase -- passed as a global
* Assumes: a_timebase->heap_size > 0
* Returns: the earliest call in the queue
* Effect: removes the earliest call in the queue
***************************************************************/

call_type remove_call(timebase_type a_timebase)
{
    register call_type *heap = a_timebase->heap;
    call_type result = heap[1];
    register call_type large;
    int i = 1;
    int child = i << 1;;
    large = heap[a_timebase->heap_size--];
    while (child <= a_timebase->heap_size) {
        if (child + 1 <= a_timebase->heap_size) {
            if (heap[child + 1]->u.e.time < heap[child]->u.e.time ||
                (heap[child + 1]->u.e.time == heap[child]->u.e.time &&
                 heap[child + 1]->u.e.priority < heap[child]->u.e.priority))
                child++;
        }
        /* child is now the index of the least child */
        if (large->u.e.time < heap[child]->u.e.time ||
            (large->u.e.time == heap[child]->u.e.time &&
             large->u.e.priority <= heap[child]->u.e.priority)) break;
        /* swap */
        heap[i] = heap[child];
        i = child;
        child = i << 1;
    }
    heap[i] = large;
    return result;
}

/***************************************************************
*                           set_rate
*
* Input : timebase and new rate
* Effect: makes the current rate of timebase be rate
***************************************************************/

void set_rate(base, rate)
  timebase_type base;
  time_type rate;
{
    if (base == timebase) base->virt_base = virttime;
    else base->virt_base = real_to_virt(base, eventtime);
    base->real_base = eventtime;
    base->rate = rate;
/*    gprintf(TRANS, "new real_base %ld virt_base %ld\n",
                   base->real_base, base->virt_base);
 */
    remove_base(base);
    insert_base(base);
}
  
/***************************************************************
*                           set_virttime
*
* Input : virtual time
* Effect: makes the current virtual time of timebase be vtime
***************************************************************/

void set_virttime(base, vtime)
  timebase_type base;
  time_type vtime;
{
    base->real_base = eventtime;
    base->virt_base = vtime;
    if (base == timebase) virttime = vtime;
    remove_base(base);
    insert_base(base);
}

/***************************************************************
*                           timebase_use
*
* Input : a timebase to use for scheduling
* Effect: sets up globals: timebase, virttime
***************************************************************/

void timebase_use(base)
  register timebase_type base;
{
    if (timebase != base) {
        timebase = base;
        virttime = real_to_virt(base, eventtime);
    }
}
