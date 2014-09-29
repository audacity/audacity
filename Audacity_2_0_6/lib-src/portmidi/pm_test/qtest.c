#include "portmidi.h"
#include "pmutil.h"
#include "stdlib.h"
#include "stdio.h"


/* make_msg -- make a psuedo-random message of length n whose content
 *    is purely a function of i 
 */
void make_msg(long msg[], int n, int i)
{
    int j;
    for (j = 0; j < n; j++) {
        msg[j] = i % (j + 5);
    }
} 


/* print_msg -- print the content of msg of length n to stdout */
/**/
void print_msg(long msg[], int n)
{
    int i;
    for (i = 0; i < n; i++) {
        printf(" %li", msg[i]);
    }
}


/* cmp_msg -- compare two messages of length n */
/**/
int cmp_msg(long msg[], long msg2[], int n, int i)
{
    int j;
    for (j = 0; j < n; j++) {
        if (msg[j] != msg2[j]) {
            printf("Received message %d doesn't match sent message\n", i);
            printf("in: "); print_msg(msg, n); printf("\n");
            printf("out:"); print_msg(msg2, n); printf("\n");
            return FALSE;
        }
    }
    return TRUE;
}


int main()
{
    int msg_len;
    for (msg_len = 4; msg_len < 100; msg_len += 5) {
        PmQueue *queue = Pm_QueueCreate(100, msg_len * sizeof(long));
        int i;
        long msg[100];
        long msg2[100];

	printf("msg_len = %d\n", msg_len);
        if (!queue) {
            printf("Could not allocate queue\n");
            return 1;
        }
    
        /* insert/remove 1000 messages */
        printf("test 1\n");
        for (i = 0; i < 1357; i++) {
            make_msg(msg, msg_len, i);
            if (Pm_Enqueue(queue, msg)) {
                printf("Pm_Enqueue error\n");
                return 1;
            }
            if (Pm_Dequeue(queue, msg2) != 1) {
                printf("Pm_Dequeue error\n");
                return 1;
            }
            if (!cmp_msg(msg, msg2, msg_len, i)) {
                return 1;
            }
        }
    
        /* make full */
        printf("test 2\n");
        for (i = 0; i < 100; i++) {
            make_msg(msg, msg_len, i);
            if (Pm_Enqueue(queue, msg)) {
                printf("Pm_Enqueue error\n");
                return 1;
            }
        }
    
        /* alternately remove and insert */
        for (i = 100; i < 1234; i++) {
            make_msg(msg, msg_len, i - 100); /* what we expect */
            if (Pm_Dequeue(queue, msg2) != 1) {
                printf("Pm_Dequeue error\n");
                return 1;
            }
            if (!cmp_msg(msg, msg2, msg_len, i)) {
                return 1;
            }
            make_msg(msg, msg_len, i);
            if (Pm_Enqueue(queue, msg)) {
                printf("Pm_Enqueue error\n");
                return 1;
            }
        }
    
        /* remove all */
        while (!Pm_QueueEmpty(queue)) {
            make_msg(msg, msg_len, i - 100); /* what we expect */
            if (Pm_Dequeue(queue, msg2) != 1) {
                printf("Pm_Dequeue error\n");
                return 1;
            }
            if (!cmp_msg(msg, msg2, msg_len, i)) {
                return 1;
            }
            i++;
        }
        if (i != 1334) {
            printf("Message count error\n");
	    return 1;
        }
    
        /* now test overflow */
        printf("test 3\n");
        for (i = 0; i < 110; i++) {
            make_msg(msg, msg_len, i);
            if (Pm_Enqueue(queue, msg) == pmBufferOverflow) {
	        break; /* this is supposed to execute after 100 messages */
            }
        }
        for (i = 0; i < 100; i++) {
            make_msg(msg, msg_len, i);
            if (Pm_Dequeue(queue, msg2) != 1) {
                printf("Pm_Dequeue error\n");
		return 1;
            }
            if (!cmp_msg(msg, msg2, msg_len, i)) {
                return 1;
            }
        }
	/* we should detect overflow after removing 100 messages */
        if (Pm_Dequeue(queue, msg2) != pmBufferOverflow) {
            printf("Pm_Dequeue overflow expected\n");
	    return 1;
        }
    
	/* after overflow is detected (and cleared), sender can
	 * send again
	 */
        /* see if function is restored, also test peek */
        printf("test 4\n");
        for (i = 0; i < 1357; i++) {
            long *peek;
            make_msg(msg, msg_len, i);
            if (Pm_Enqueue(queue, msg)) {
                printf("Pm_Enqueue error\n");
                return 1;
            }
            peek = (long *) Pm_QueuePeek(queue);
            if (!cmp_msg(msg, peek, msg_len, i)) {
	        return 1;
            }
            if (Pm_Dequeue(queue, msg2) != 1) {
                printf("Pm_Dequeue error\n");
	        return 1;
            }
            if (!cmp_msg(msg, msg2, msg_len, i)) {
	        return 1;
            }
        }
        Pm_QueueDestroy(queue);
    }
    return 0;
}
