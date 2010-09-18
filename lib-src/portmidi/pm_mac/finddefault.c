/* finddefault.c -- find_default_device() implementation
   Roger Dannenberg, June 2008
*/

#include <stdlib.h>
#include <string.h>
#include "portmidi.h"
#include "pmutil.h"
#include "pminternal.h"
#include "pmmacosxcm.h"
#include "readbinaryplist.h"

/* Parse preference files, find default device, search devices --
   This parses the preference file(s) once for input and once for
   output, which is inefficient but much simpler to manage. Note
   that using the readbinaryplist.c module, you cannot keep two
   plist files (user and system) open at once (due to a simple
   memory management scheme).
*/
PmDeviceID find_default_device(char *path, int input, PmDeviceID id)
/* path -- the name of the preference we are searching for
   input -- true iff this is an input device
   id -- current default device id
   returns matching device id if found, otherwise id
*/
{
    static char *pref_file = "com.apple.java.util.prefs.plist";
    char *pref_str = NULL;
    // read device preferences
    value_ptr prefs = bplist_read_user_pref(pref_file);
    if (prefs) {
        value_ptr pref_val = value_dict_lookup_using_path(prefs, path);
        if (pref_val) {
            pref_str = value_get_asciistring(pref_val);
        }
    }
    if (!pref_str) {
        bplist_free_data(); /* look elsewhere */
        prefs = bplist_read_system_pref(pref_file);
        if (prefs) {
            value_ptr pref_val = value_dict_lookup_using_path(prefs, path);
            if (pref_val) {
                pref_str = value_get_asciistring(pref_val);
            }
        }
    }
    if (pref_str) { /* search devices for match */
        int i = pm_find_default_device(pref_str, input);
        if (i != pmNoDevice) {
            id = i;
	}
    }
    if (prefs) {
        bplist_free_data();
    }
    return id;
}
