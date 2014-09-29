/* cleanup.c -- registers work to do upon exit */

#include "stdio.h"
#include "cext.h"

typedef struct cleanup_struct {
    struct cleanup_struct *next;
    cu_fn_type fn;
    cu_parm_type obj;
} cleanup_node, *cleanup_type;

cleanup_type cleanup_list = NULL;

/* cu_register -- remember function and arg to call in order to clean up */
/**/
void cu_register(cu_fn_type fn, cu_parm_type obj)
{
    cleanup_type cu = (cleanup_type) memget(sizeof(cleanup_node));
    cu->fn = fn;
    cu->obj = obj;
    cu->next = cleanup_list;
    cleanup_list = cu;
}


/* cu_unregister -- erase memory of obj (should be unique in cleanup list) */
/**/
void cu_unregister(obj)
  void *obj;
{
    cleanup_type *cu = &cleanup_list;
    while (*cu) {
        if ((*cu)->obj == obj) {
            cleanup_type found = *cu;
            *cu = (*cu)->next;	/* splice out found */
            memfree((char *) found, sizeof(cleanup_node));
            return;
        }
        cu = &((*cu)->next);
    }
}


/* cu_cleanup -- call the registered functions */
/**/
void cu_cleanup()
{
    while (cleanup_list) {
        cleanup_type cu = cleanup_list;
#ifdef CU_TRACE
        gprintf(GTRANS, "cu_cleanup: node %lx fn %lx obj %lx\n",
                cu, cu->fn, cu->obj);
#endif
        cu->fn(cu->obj);
        cleanup_list = cu->next;
        memfree((char *) cu, sizeof(cleanup_node));
    }
#ifdef CU_TRACE
    gprintf(GTRANS, "cu_cleanup done.\n");
    fflush(stdout);
#endif
}
