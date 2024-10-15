/* extern.c - external type support for xlisp 2.0 */

/* Description of external types:

        A new node type EXTERN has been created to allow extensions to the
xlisp type system.  If a node is of type EXTERN then it contains two fields:
(1) a pointer to a type descriptor, and (2) a value which is normally a pointer
to an instance of the data type.
        The type descriptor has 6 fields:
type_name: a string name for the type
type_symbol: a pointer to a symbol whose print name is type_name
free_meth: routine to call when the node is freed
print_meth: routine to call to print the node
save_meth: routine to save the value to a file
restore_meth: routine to restore the value from a file
        The routine create_desc() is used to allocate and initialize a type
descriptor.  There should be only one type descriptor for each unique type.
Typically, the module that manages a type will create the type descriptor
at initialization time.
        The routine cvextern() is used to combine a type descriptor and a
value to create an EXTERN node that can be used by xlisp.  This routine is
called whenever a new value is allocated and returned to xlisp.
        The routine exttype() returns the type symbol for an EXTERN type.
        The routine exttypep() tests for a type match.
        If EXTERN objects are dynamically allocated and freed, then there
should only be one EXTERN node whose value field (xe_inst) points to the object.
If there is more than one node (normally created only by cvextern), then one
of the following should hold:
1. the object is not dynamically freed.
2. the object is reference counted and freed when the last EXTERN node is gc'd.
3. the object will be freed when the first node pointing to it is gc'd, resulting
in a dangling pointer bug.
        The save and restore capability of xlisp version 2.0 causes some
difficulties in that symbols get relocated whenever a workspace is loaded.
However, type descriptors must point to symbols and type descriptors must be
present in order to load external types from a workspace.  My solution to this
problem is to store a string name for each type and also to cache a pointer
to the corresponding symbol.  The cache is invalidated whenever a workspace is
loaded (causing symbols to be reallocated).  To make it possible to save and
restore references to type descriptors, all type descriptors are kept in
static storage.  Internally, they are referenced by pointers, but when saved,
an integer index is used.  Because of the integer index, types must always be
allocated in the same order.  This is taken care of in localinit().

*/

#include "xlisp.h"
#include "extern.h"

int extindex = 0;

struct xtype_desc_struct desc_table[NTYPES];


/* create_desc - create a new external type */
/**/
xtype_desc create_desc(
  char *type_name,	/* the type string name */
  void (*fm)(void*),		/* method to free instances of the type */
  void (*pm)(LVAL, void*),		/* method to print instances of the type */
  void (*sm)(FILE*, void*),		/* method to save instances of the type */
  unsigned char * (*rm)(FILE*), /* method to restore instances of the type */
  void (*mm)(void*))		/* method to mark instances of the type for GC */
{
    xtype_desc td;	/* the new type descriptor */
    if (extindex >= NTYPES) xlfail("insufficient type desc space");
    td = &desc_table[extindex++];
    td->type_name = type_name;
    td->type_symbol = NULL;
    td->free_meth = fm;
    td->print_meth = pm;
    td->save_meth = sm;
    td->restore_meth = rm;
    td->mark_meth = mm;
    return td;
}


/* cvextern - create an instance of some type */
/**/
LVAL cvextern(typeptr, instptr)
  xtype_desc typeptr;	/* pointer to type descriptor */
  unsigned char *instptr;	/* pointer to the data */
{
    LVAL xnode;	/* the resulting lisp node */
    xnode = newnode(EXTERN);
    setdesc(xnode, typeptr);
    setinst(xnode, instptr);
    return xnode;
}


/* exttype -- get the type of an EXTERN */
/**/
LVAL exttype(LVAL x)
{
    if (!(getdesc(x)->type_symbol)) {
        getdesc(x)->type_symbol = xlenter(getdesc(x)->type_name);
    }
    return getdesc(x)->type_symbol;
}


/* exttypep -- test for type match */
/*
 * x is a node, type_sym is a symbol
 */
int exttypep(LVAL x, LVAL type_sym)
{
    if ((x) && ntype(x) == EXTERN) {
        if (!(getdesc(x)->type_symbol)) {
            getdesc(x)->type_symbol = xlenter(getdesc(x)->type_name);
        }
        return (getdesc(x)->type_symbol == type_sym);
    }
    return FALSE;
}


/* inval_caches -- set type_symbol fields to NULL */
/**/
void inval_caches()
{
    int i;
    for (i = 0; i < extindex; i++) {
        desc_table[i].type_symbol = NULL;
    }
}
