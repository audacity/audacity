/* finddefault.c -- find_default_device() implementation
   Roger Dannenberg, Jan 2009
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "portmidi.h"

#define STRING_MAX 256

/* skip over spaces, return first non-space */
void skip_spaces(FILE *inf)
{
    char c;
    while (isspace(c = getc(inf))) ;
    ungetc(c, inf);
}

/* trim leading spaces and match a string */
int match_string(FILE *inf, char *s)
{
    skip_spaces(inf);
    while (*s && *s == getc(inf)) s++;
    return (*s == 0);
} 


/* 
/* Parse preference files, find default device, search devices --
 */
PmDeviceID find_default_device(char *path, int input, PmDeviceID id)
/* path -- the name of the preference we are searching for
   input -- true iff this is an input device
   id -- current default device id
   returns matching device id if found, otherwise id
*/
{
    static char *pref_2 = "/.java/.userPrefs/";
    static char *pref_3 = "prefs.xml";
    char *pref_1 = getenv("HOME");
    char *full_name, *path_ptr;
    FILE *inf;
    int c, i;
    if (!pref_1) goto nopref; // cannot find preference file
    // full_name will be larger than necessary
    full_name  = malloc(strlen(pref_1) + strlen(pref_2) + strlen(pref_3) +
                        strlen(path) + 2);
    strcpy(full_name, pref_1); 
    strcat(full_name, pref_2);
    // copy all but last path segment to full_name
    if (*path == '/') path++; // skip initial slash in path
    path_ptr = strrchr(path, '/'); 
    if (path_ptr) { // copy up to slash after full_name
        path_ptr++;
        int offset = strlen(full_name);
        memcpy(full_name + offset, path, path_ptr - path);
        full_name[offset + path_ptr - path] = 0; // end of string
    } else {
        path_ptr = path;
    }
    strcat(full_name, pref_3);
    inf = fopen(full_name, "r");
    if (!inf) goto nopref; // cannot open preference file
    // We're not going to build or link in a full XML parser.
    // Instead, find the path string and quoute. Then, look for
    // "value", "=", quote. Then get string up to quote.
    while ((c = getc(inf)) != EOF) {
        char pref_str[STRING_MAX];
        if (c != '"') continue; // scan up to quote
        // look for quote string quote
        if (!match_string(inf, path_ptr)) continue; // path not found
        if (getc(inf) != '"') continue; // path not found, keep scanning
        if (!match_string(inf, "value")) goto nopref; // value not found
        if (!match_string(inf, "=")) goto nopref; // = not found
        if (!match_string(inf, "\"")) goto nopref; // quote not found
        // now read the value up to the close quote
        for (i = 0; i < STRING_MAX; i++) {
            if ((c = getc(inf)) == '"') break;
            pref_str[i] = c;
        }
        if (i == STRING_MAX) continue; // value too long, ignore
        pref_str[i] = 0;
        i = pm_find_default_device(pref_str, input);
        if (i != pmNoDevice) {
            id = i;
	}
        break;
    }
 nopref:
    return id;
}
