/* localptrs.h -- extend XLISP with these functions
 *
 * CHANGE LOG
 * 28-Apr-03  rbd  Removed "include switches.h" -- already included
 */
 
/* extension to xlisp */
#include "sndfnintptrs.h"
#include "seqfnintptrs.h"
         { "_", SUBR, xlc_gettext },
         { "_C", SUBR, xlc_gettextc },
         { "NGETTEXT", SUBR, xlc_ngettext },
         { "NGETTEXTC", SUBR, xlc_ngettextc },
         { "AUD-DO",  SUBR, xlc_aud_do },
#include "nyquistapiintptrs.h"
