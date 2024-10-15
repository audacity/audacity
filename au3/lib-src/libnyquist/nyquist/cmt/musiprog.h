/* musiprog.h -- include file for cmt application programs */

#include "stdio.h"
#include "cext.h"
#include "midifns.h"
#include "userio.h"
#include "timebase.h"
#include "moxc.h"

/*
 * override the definition of l_rest - l_rest is not recommended because
 * it stops polling for input.  If you really want to use it, use #undef
 * to make it visible.
 */
#define l_rest(d) m_rest(d)
#define l_restuntil(t) m_restuntil(t)

/*
 * The default implementation of rest() and restuntil() poll for
 * input during the rest.  You might call rest() or restuntil() from
 * mainscore(), but it is generally a bad idea to rest at all. If
 * you are in a rest(), you get an event, e.g. keydown(), and you 
 * make a nested call to rest(), the original rest will be locked out
 * until the nested one returns.  It's better to use cause().
 */
#define rest(x) l_rest( (long) x )
#define restuntil(x) l_restuntil( (long) x)

#define repeat(var, count) {int var; for (var=1; var <= count; var++) {
#define endrep ;}}

