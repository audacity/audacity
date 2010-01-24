/* pmutil.c -- some helpful utilities for building midi
               applications that use PortMidi
 */
//#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "portmidi.h"
#include "pmutil.h"
#include "pminternal.h"

#ifdef WIN32
#define bzero(addr, siz) memset(addr, 0, siz)
#endif

// #define QUEUE_DEBUG 1
#ifdef QUEUE_DEBUG
#include "stdio.h"
#endif

/* code is based on 4-byte words -- it should work on a 64-bit machine
   as long as a "long" has 4 bytes. This code could be generalized to
   be independent of the size of "long" */

typedef long int32;

typedef struct {
    long head;
    long tail;
    long len;
    long msg_size; /* number of int32 in a message including extra word */
    long overflow;
    long peek_overflow;
    int32 *buffer;
    int32 *peek;
    int peek_flag;
} PmQueueRep;


PmQueue *Pm_QueueCreate(long num_msgs, long bytes_per_msg)
{
    int int32s_per_msg = ((bytes_per_msg + sizeof(int32) - 1) &
                          ~(sizeof(int32) - 1)) / sizeof(int32);
    PmQueueRep *queue = (PmQueueRep *) pm_alloc(sizeof(PmQueueRep));
    if (!queue) /* memory allocation failed */
        return NULL;

    /* need extra word per message for non-zero encoding */
    queue->len = num_msgs * (int32s_per_msg + 1);
    queue->buffer = (int32 *) pm_alloc(queue->len * sizeof(int32));
    bzero(queue->buffer, queue->len * sizeof(int32));
    if (!queue->buffer) {
        pm_free(queue);
        return NULL;
    } else { /* allocate the "peek" buffer */
        queue->peek = (int32 *) pm_alloc(int32s_per_msg * sizeof(int32));
        if (!queue->peek) {
            /* free everything allocated so far and return */
            pm_free(queue->buffer);
            pm_free(queue);
            return NULL;
        }
    }
    bzero(queue->buffer, queue->len * sizeof(int32));
    queue->head = 0;
    queue->tail = 0;
    /* msg_size is in words */
    queue->msg_size = int32s_per_msg + 1; /* note extra word is counted */
    queue->overflow = FALSE;
    queue->peek_overflow = FALSE;
    queue->peek_flag = FALSE;
    return queue;
}


PmError Pm_QueueDestroy(PmQueue *q)
{
    PmQueueRep *queue = (PmQueueRep *) q;
        
    /* arg checking */
    if (!queue || !queue->buffer || !queue->peek) 
                return pmBadPtr;
    
    pm_free(queue->peek);
    pm_free(queue->buffer);
    pm_free(queue);
    return pmNoError;
}


PmError Pm_Dequeue(PmQueue *q, void *msg)
{
    long head;
    PmQueueRep *queue = (PmQueueRep *) q;
    int i;
    int32 *msg_as_int32 = (int32 *) msg;

    /* arg checking */
    if (!queue)
        return pmBadPtr;
    /* a previous peek operation encountered an overflow, but the overflow
     * has not yet been reported to client, so do it now. No message is
     * returned, but on the next call, we will return the peek buffer.
     */
    if (queue->peek_overflow) {
        queue->peek_overflow = FALSE;
        return pmBufferOverflow;
    }
    if (queue->peek_flag) {
        memcpy(msg, queue->peek, (queue->msg_size - 1) * sizeof(int32));
        queue->peek_flag = FALSE;
        return pmGotData;
    }

    head = queue->head;
    /* if writer overflows, it writes queue->overflow = tail+1 so that
     * when the reader gets to that position in the buffer, it can 
     * return the overflow condition to the reader. The problem is that
     * at overflow, things have wrapped around, so tail == head, and the
     * reader will detect overflow immediately instead of waiting until
     * it reads everything in the buffer, wrapping around again to the
     * point where tail == head. So the condition also checks that
     * queue->buffer[head] is zero -- if so, then the buffer is now
     * empty, and we're at the point in the msg stream where overflow
     * occurred. It's time to signal overflow to the reader. If 
     * queue->buffer[head] is non-zero, there's a message there and we
     * should read all the way around the buffer before signalling overflow.
     * There is a write-order dependency here, but to fail, the overflow
     * field would have to be written while an entire buffer full of 
     * writes are still pending. I'm assuming out-of-order writes are
     * possible, but not that many.
     */
    if (queue->overflow == head + 1 && !queue->buffer[head]) {
        queue->overflow = 0; /* non-overflow condition */
        return pmBufferOverflow;
    }

    /* test to see if there is data in the queue -- test from back
     * to front so if writer is simultaneously writing, we don't
     * waste time discovering the write is not finished 
     */
    for (i = queue->msg_size - 1; i >= 0; i--) {
        if (!queue->buffer[head + i]) {
            return pmNoData;
        }
    }
    memcpy(msg, (char *) &queue->buffer[head + 1], 
           sizeof(int32) * (queue->msg_size - 1));
    /* fix up zeros */
    i = queue->buffer[head];
    while (i < queue->msg_size) {
        int32 j;
        i--; /* msg does not have extra word so shift down */
        j = msg_as_int32[i];
        msg_as_int32[i] = 0;
        i = j;
    }
    /* signal that data has been removed by zeroing: */
    bzero((char *) &queue->buffer[head], sizeof(int32) * queue->msg_size);

    /* update head */
    head += queue->msg_size;
    if (head == queue->len) head = 0;
    queue->head = head;
    return pmGotData; /* success */
}



PmError Pm_SetOverflow(PmQueue *q)
{
    PmQueueRep *queue = (PmQueueRep *) q;
    long tail;
    /* arg checking */
    if (!queue)
        return pmBadPtr;
    /* no more enqueue until receiver acknowledges overflow */
    if (queue->overflow) return pmBufferOverflow;
    tail = queue->tail;
    queue->overflow = tail + 1;
    return pmBufferOverflow;
}


PmError Pm_Enqueue(PmQueue *q, void *msg)
{
    PmQueueRep *queue = (PmQueueRep *) q;
    long tail;
    int i;
    int32 *src = (int32 *) msg;
    int32 *ptr;
    int32 *dest;
    int rslt;
    if (!queue) 
        return pmBadPtr;
    /* no more enqueue until receiver acknowledges overflow */
    if (queue->overflow) return pmBufferOverflow;
    rslt = Pm_QueueFull(q);
    /* already checked above: if (rslt == pmBadPtr) return rslt; */
    tail = queue->tail;
    if (rslt) {
        queue->overflow = tail + 1;
        return pmBufferOverflow;
    }

    /* queue is has room for message, and overflow flag is cleared */
    ptr = &queue->buffer[tail];
    dest = ptr + 1;
    for (i = 1; i < queue->msg_size; i++) {
        int32 j = src[i - 1];
        if (!j) {
            *ptr = i;
            ptr = dest;
        } else {
            *dest = j;
        }
        dest++;
    }
    *ptr = i;
    tail += queue->msg_size;
    if (tail == queue->len) tail = 0;
    queue->tail = tail;
    return pmNoError;
}


int Pm_QueueEmpty(PmQueue *q)
{
    PmQueueRep *queue = (PmQueueRep *) q;
    return (!queue) ||  /* null pointer -> return "empty" */
           (queue->buffer[queue->head] == 0 && !queue->peek_flag);
}


int Pm_QueueFull(PmQueue *q)
{
    int tail;
    int i; 
    PmQueueRep *queue = (PmQueueRep *) q;
    /* arg checking */
    if (!queue)
        return pmBadPtr;
    tail = queue->tail;
    /* test to see if there is space in the queue */
    for (i = 0; i < queue->msg_size; i++) {
        if (queue->buffer[tail + i]) {
            return TRUE;
        }
    }
    return FALSE;
}


void *Pm_QueuePeek(PmQueue *q)
{
    PmError rslt;
    long temp;
    PmQueueRep *queue = (PmQueueRep *) q;
    /* arg checking */
    if (!queue)
        return NULL;

    if (queue->peek_flag) {
        return queue->peek;
    }
    /* this is ugly: if peek_overflow is set, then Pm_Dequeue() 
     * returns immediately with pmBufferOverflow, but here, we
     * want Pm_Dequeue() to really check for data. If data is
     * there, we can return it
     */
    temp = queue->peek_overflow;
    queue->peek_overflow = FALSE;
    rslt = Pm_Dequeue(q, queue->peek);
    queue->peek_overflow = temp;

    if (rslt == 1) {
        queue->peek_flag = TRUE;
        return queue->peek;
    } else if (rslt == pmBufferOverflow) {
        /* when overflow is indicated, the queue is empty and the 
         * first message that was dropped by Enqueue (signalling
         * pmBufferOverflow to its caller) would have been the next
         * message in the queue. Pm_QueuePeek will return NULL, but
         * remember that an overflow occurred. (see Pm_Dequeue)
         */
        queue->peek_overflow = TRUE;
    }
    return NULL;
}

