/* pmutil.h -- some helpful utilities for building midi 
               applications that use PortMidi 
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef void PmQueue;

/*
    A single-reader, single-writer queue is created by
    Pm_QueueCreate(), which takes the number of messages and
    the message size as parameters. The queue only accepts
    fixed sized messages. Returns NULL if memory cannot be allocated.

    This queue implementation uses the "light pipe" algorithm which
    operates correctly even with multi-processors and out-of-order
    memory writes. (see Alexander Dokumentov, "Lock-free Interprocess
    Communication," Dr. Dobbs Portal, http://www.ddj.com/, 
    articleID=189401457, June 15, 2006. This algorithm requires
    that messages be translated to a form where no words contain
    zeros. Each word becomes its own "data valid" tag. Because of
    this translation, we cannot return a pointer to data still in 
    the queue when the "peek" method is called. Instead, a buffer 
    is preallocated so that data can be copied there. Pm_QueuePeek() 
    dequeues a message into this buffer and returns a pointer to 
    it. A subsequent Pm_Dequeue() will copy from this buffer.

    This implementation does not try to keep reader/writer data in
    separate cache lines or prevent thrashing on cache lines. 
    However, this algorithm differs by doing inserts/removals in
    units of messages rather than units of machine words. Some
    performance improvement might be obtained by not clearing data
    immediately after a read, but instead by waiting for the end
    of the cache line, especially if messages are smaller than
    cache lines. See the Dokumentov article for explanation.

    The algorithm is extended to handle "overflow" reporting. To report
    an overflow, the sender writes the current tail position to a field.
    The receiver must acknowlege receipt by zeroing the field. The sender
    will not send more until the field is zeroed.
    
    Pm_QueueDestroy() destroys the queue and frees its storage.
 */

PMEXPORT PmQueue *Pm_QueueCreate(long num_msgs, int32_t bytes_per_msg);
PMEXPORT PmError Pm_QueueDestroy(PmQueue *queue);

/* 
    Pm_Dequeue() removes one item from the queue, copying it into msg.
    Returns 1 if successful, and 0 if the queue is empty.
    Returns pmBufferOverflow if what would have been the next thing
    in the queue was dropped due to overflow. (So when overflow occurs,
    the receiver can receive a queue full of messages before getting the
    overflow report. This protocol ensures that the reader will be 
    notified when data is lost due to overflow.
 */
PMEXPORT PmError Pm_Dequeue(PmQueue *queue, void *msg);


/*
    Pm_Enqueue() inserts one item into the queue, copying it from msg.
    Returns pmNoError if successful and pmBufferOverflow if the queue was 
    already full. If pmBufferOverflow is returned, the overflow flag is set.
 */
PMEXPORT PmError Pm_Enqueue(PmQueue *queue, void *msg);


/*
    Pm_QueueFull() returns non-zero if the queue is full
    Pm_QueueEmpty() returns non-zero if the queue is empty

    Either condition may change immediately because a parallel
    enqueue or dequeue operation could be in progress. Furthermore,
    Pm_QueueEmpty() is optimistic: it may say false, when due to 
    out-of-order writes, the full message has not arrived. Therefore,
    Pm_Dequeue() could still return 0 after Pm_QueueEmpty() returns
    false. On the other hand, Pm_QueueFull() is pessimistic: if it
    returns false, then Pm_Enqueue() is guaranteed to succeed. 

    Error conditions: Pm_QueueFull() returns pmBadPtr if queue is NULL.
    Pm_QueueEmpty() returns FALSE if queue is NULL.
 */
PMEXPORT int Pm_QueueFull(PmQueue *queue);
PMEXPORT int Pm_QueueEmpty(PmQueue *queue);


/*
    Pm_QueuePeek() returns a pointer to the item at the head of the queue,
    or NULL if the queue is empty. The item is not removed from the queue.
    Pm_QueuePeek() will not indicate when an overflow occurs. If you want
    to get and check pmBufferOverflow messages, use the return value of
    Pm_QueuePeek() *only* as an indication that you should call 
    Pm_Dequeue(). At the point where a direct call to Pm_Dequeue() would
    return pmBufferOverflow, Pm_QueuePeek() will return NULL but internally
    clear the pmBufferOverflow flag, enabling Pm_Enqueue() to resume
    enqueuing messages. A subsequent call to Pm_QueuePeek()
    will return a pointer to the first message *after* the overflow. 
    Using this as an indication to call Pm_Dequeue(), the first call
    to Pm_Dequeue() will return pmBufferOverflow. The second call will
    return success, copying the same message pointed to by the previous
    Pm_QueuePeek().

    When to use Pm_QueuePeek(): (1) when you need to look at the message
    data to decide who should be called to receive it. (2) when you need
    to know a message is ready but cannot accept the message.

    Note that Pm_QueuePeek() is not a fast check, so if possible, you 
    might as well just call Pm_Dequeue() and accept the data if it is there.
 */
PMEXPORT void *Pm_QueuePeek(PmQueue *queue);

/*
    Pm_SetOverflow() allows the writer (enqueuer) to signal an overflow
    condition to the reader (dequeuer). E.g. when transfering data from 
    the OS to an application, if the OS indicates a buffer overrun,
    Pm_SetOverflow() can be used to insure that the reader receives a
    pmBufferOverflow result from Pm_Dequeue(). Returns pmBadPtr if queue
    is NULL, returns pmBufferOverflow if buffer is already in an overflow
    state, returns pmNoError if successfully set overflow state.
 */
PMEXPORT PmError Pm_SetOverflow(PmQueue *queue);

#ifdef __cplusplus
}
#endif /* __cplusplus */
