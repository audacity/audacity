/* cleanup.c -- registers work to do upon exit */

typedef void *cu_parm_type;
typedef void (*cu_fn_type)(cu_parm_type);
void cu_register(cu_fn_type fn, cu_parm_type obj);
void cu_unregister(void *obj);
void cu_cleanup(void);

#define CLEANUP
