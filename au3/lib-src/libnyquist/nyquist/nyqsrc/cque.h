/* 
 * cque.h
 * macros for free lists.
 */

typedef struct cque {
    struct cque	*qnext;
} CQUE;

#define	Qinit(q1)	{ (q1) = 0; }

/* q1 points to a stack CQUE*, new is an element to insert */
#define Qenter(q1,new)  {   \
    ((CQUE *)(new))->qnext = ((CQUE *)(q1)); \
    q1 = ((CQUE *)(new)); }


/* q1 points to a list of CQUE*: remove elt and assign to new */
/* NOTE: q1 must be non-empty */
#define Qget(q1,newtype,new) {  \
    (new) = (newtype)(q1);  \
    q1 = ((CQUE *)(q1))->qnext; }


#define	Qempty(q1)	((q1) == 0)
