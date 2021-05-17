/* cmtcmd.h -- header for remote action and variable setting interface */

#define var_symb_type 333
#define fn_symb_type 555
#define vec_symb_type 777

#ifdef AMIGA
struct cmd_msg {
    struct Message msg;
    long symb_type;     /* one of var_, fn_, or vec_symb_type */
    char *symbol_name;
    long the_args[8];   /* args for function call.  If var_symb_type,
               the_args[0] is the new value.  If vec_symb_type,
               the_args[0] is index, the_args[1] is new value. */
};
#endif

typedef struct symb_descr {
    char *symbol_name;
    int symb_type;
    int size;     /* for array bounds checking */
    union {
    int *intptr;
    seq_cmd_fn routine;
    } ptr;
} symb_descr_node;

intptr_t hash_lookup(char *s);
void defvar(char *name, int *addr);
void defvec(char *name, int *addr, int size);
/* former defun_type replaced by seq_cmd_fn */
void defun(char *name, seq_cmd_fn addr);

#define HASHTYPE symb_descr_node
#include "hash.h"
