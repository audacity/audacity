/* timebase.h -- management of calls, time bases and heaps for moxc */

#define STOPRATE 0xFFFFL

/***************************************************************************
* call structure
****************************************************************************/

#define MAX_CALL_ARGS 8
typedef struct call_args_struct {
  void *arg[MAX_CALL_ARGS];
} call_args_node, *call_args_type;

typedef struct call {
    union {
        struct {
            time_type time;     /* virtual time of this call */
            int priority;       /* an 8-bit the priority, low priority first */
            void (*routine)(call_args_type args);   /* who to call */
            call_args_node p; /* what to pass */
        } e;
        struct call *p; /* used to link free calls */
    } u;
} *call_type, call_node;


/***************************************************************************
* timebase structure
****************************************************************************/

typedef struct timebase_struct {
    struct timebase_struct *next;       /* used for list */
    time_type next_time;
    time_type virt_base;
    time_type real_base;
    time_type rate; /* ratio of real/virt time, STOPRATE or more is infinity */
    short heap_size;
    short heap_max;
    call_type *heap;
} timebase_node, *timebase_type;

extern timebase_type timebase_queue;

#define call_alloc() ((call_type) memget(sizeof(call_node)))
#define call_free(c) memfree((char *) (c), sizeof(call_node))

timebase_type   timebase_create(int maxsize);
void            callinsert(timebase_type base, call_type call);
void            callshow(call_type call);
void            timebase_free(timebase_type timebase);
void            insert_base(timebase_type timebase);
void            remove_base(timebase_type timebase);
call_type       remove_call(timebase_type a_timebase);
void            set_rate(timebase_type base, time_type rate);
void            set_virttime(timebase_type base, time_type vtime);
void            timebase_use(timebase_type base);

#define real_to_virt(base, rtime) ((base)->rate == 0 ? MAXTIME : \
 ((base)->virt_base + (((rtime) - (base)->real_base) << 8) / (base)->rate))

#define virt_to_real(base, vtime) ((base)->rate >= STOPRATE ? \
 ((base)->virt_base > vtime ? (base)->real_base : MAXTIME) : \
 (base)->real_base + ((((vtime) - (base)->virt_base) * (base)->rate) >> 8))

#define virt_to_real_256(base, vtime) ((base)->rate >= STOPRATE ? \
 ((base)->virt_base > vtime ? (base)->real_base << 8 : MAXTIME) : \
 ((base)->real_base << 8) + ((((vtime) - (base)->virt_base) * (base)->rate)))
