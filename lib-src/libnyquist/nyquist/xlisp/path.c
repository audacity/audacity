/*
 * New xlisp_path code by Dominic Mazzoni
 *
 * There is now a function provided to set the xlisp_path.
 * This is particularly useful for external programs
 * (e.g. Audacity, or a Nyquist GUI) that have their own
 * mechanism of setting/finding the path.  If xlisp_path
 * is NULL, the old platform-specific methods are still
 * used.
 */
/* CHANGE LOG
 *
 * 24-dec-05  RBD
 *    Made ';' a valid path separator for every system (to work
 *    around a windows installer limitation)
 *
 * 22-jul-07  RBD
 *    Added get_user_id() 
 *
 *  9-jan-08  RBD
 *    Added find-in-xlisp-path as XLISP primitive
 *
 * 22-jun-16  RBD & Paul Licameli
 *    Added cleanup code to free allocated memory
 */

#include <string.h>
#include <stdint.h>

#include "switches.h"
#include "xlisp.h"

// boolean flag to support one-shot atexit() registration:
static unsigned char registered_path_cleanup = 0;

// save a copy of xlisp search path for return_xlisp_path():
static char *g_xlisp_path = NULL;

// return value for find_in_xlisp_path():
static char *g_xlptemp = NULL;


// clean up any allocated memory for this module (path.c)
static void path_cleanup(void)
{
    if (g_xlisp_path) {
        free(g_xlisp_path);
        g_xlisp_path = NULL;
    }

    if (g_xlptemp) {
        free(g_xlptemp);
        g_xlptemp = NULL;
    }
}


// set_xlisp_path - set the search path
//     the caller owns the parameter string, a copy is made
//     and freed at program exit
void set_xlisp_path(const char *p)
{
    // one-time register to free any allocated memory at cleanup
    if (!registered_path_cleanup) {
        atexit(path_cleanup);
        registered_path_cleanup = 1;
    }

    if (g_xlisp_path) {
        free(g_xlisp_path);
        g_xlisp_path = NULL;
    }

    if (p) {
        g_xlisp_path = malloc(strlen(p)+1);
        // if malloc fails, program will crash here -- maybe that's better
        //    than setting g_xlisp_path to NULL, masking a critical problem
        strcpy(g_xlisp_path, p);
    }
}


#ifdef UNIX
const char *unix_return_xlisp_path()
{
    char *paths = getenv("XLISPPATH");
    if (!paths || !*paths) {
        char msg[512];
        sprintf(msg, "\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",
                "Warning: XLISP failed to find XLISPPATH in the environment.",
                "If you are using Nyquist, probably you should cd to the",
                "nyquist directory and type:",
                "    setenv XLISPPATH `pwd`/runtime:`pwd`/lib",
                "or set XLISPPATH in your .login or .cshrc file.",
                "If you use the bash shell, try:",
                "    XLISPPATH=`pwd`/runtime:`pwd`/lib; export XLISPPATH");
        errputstr(msg);
    }
    return paths;
}
#endif

#ifdef WINDOWS
#include "winfun.h"

const char *windows_return_xlisp_path()
{
    #define paths_max 1024
    char paths[paths_max];
    get_xlisp_path(paths, paths_max);
    /* make sure we got paths, and the list is not empty */
    if (!paths[0]) {
        sprintf(paths, "\n%s\n%s\n%s\n",
           "Warning: XLISP failed to find XLISPPATH in the Registry.",
           "You should follow the installation instructions. Enter an",
           "empty string if you really want no search path.");
        errputstr(paths);
    }
    set_xlisp_path(paths);
    /* for debugging: 
      errputstr("windows_return_xlisp_path() returns ");
      errputstr(paths);
      errputstr("\n"); */
    return g_xlisp_path;
}

#endif

#ifdef MACINTOSH
const char *mac_return_xlisp_path()
{
    #define paths_max 1024
    char paths[paths_max];
    int prefs_found = false;
    get_xlisp_path(paths, paths_max, &prefs_found);
    if (!paths[0]) {
        if (prefs_found) {
            sprintf(paths, "\n%s\n%s\n%s\n",
             "Warning: XLISP failed to find XLISPPATH in XLisp Preferences.",
             "You should probably delete XLisp Preferences and let XLisp",
             "create a new one for you.");
        }
        else {
           sprintf(paths, "\n%s\n%s\n%s\n%s\n%s\n",
            "Warning: XLISP failed to find XLisp Preferences.",
            "You should manually locate and load the file runtime:init.lsp",
            "Nyquist will create an XLisp Preferences file to automatically",
            "find the file next time. You may edit XLisp Preferences to add",
            "additional search paths, using a comma as separator.");
        }
        errputstr(paths);
    }
    set_xlisp_path(paths);
    return g_xlisp_path;
}


const char *get_user_id()
{
    // not implemented for MACINTOSH (OS 9), just use "nyquist"
    return "nyquist";
}
#endif


const char *return_xlisp_path()
{
    if (g_xlisp_path)
        return g_xlisp_path;

    // if g_xlisp_path has not been set, use one of the following
    // to get the value, cache it in g_xlisp_path, and return it
#ifdef WINDOWS
    return windows_return_xlisp_path();
#endif
#ifdef MACINTOSH
    return mac_return_xlisp_path();
#endif
#ifdef UNIX
    return unix_return_xlisp_path();
#endif
}


// find_in_xlisp_path -- find fname or fname.lsp by searching XLISP_PATH
//
// NOTE: this module owns the string. The string is valid
// until the next call to find_in_xlisp_path()
//
const char *find_in_xlisp_path(const char *fname)
{
    const char *paths = return_xlisp_path();
    if (!paths)
        return NULL;

    // one-time register to free any allocated memory at cleanup
    if (!registered_path_cleanup) {
        atexit(path_cleanup);
        registered_path_cleanup = 1;
    }

    while (paths && *paths) {
        FILE *fp;
        const char *start;
        intptr_t len;

        /* skip over separator */
        while (*paths == os_sepchar || *paths == ';') paths++;

        /* find next directory */
        start = paths;
        while (*paths && (*paths != os_sepchar && *paths != ';'))
            paths++;

        if (g_xlptemp) {
           free(g_xlptemp);
           g_xlptemp = NULL;
        }

        len = paths - start;
        g_xlptemp = malloc(len + strlen(fname) + 10);
        memcpy(g_xlptemp, start, len);

        if (len == 0)
           continue;

        /* add "/" if needed */
        if (g_xlptemp[len-1] != os_pathchar)
           g_xlptemp[len++] = os_pathchar;
        
        /* append the file name */
        memcpy(&g_xlptemp[len], fname, strlen(fname));
        len += strlen(fname);
        g_xlptemp[len] = 0;


        /* printf("Attempting to open %s, start is %s\n", g_xlptemp, start); */
        fp = osaopen(g_xlptemp, "r");
        if (!fp) {
            /* try appending the .lsp extension */
            if (needsextension(g_xlptemp)) {
                strcat(g_xlptemp, ".lsp");
                fp = osaopen(g_xlptemp, "r");
                if (!fp) {
                    g_xlptemp[strlen(g_xlptemp) - 4] = 0; /* remove .lsp */
                }
            }
        }
        if (fp) {
           fclose(fp);

           #ifdef MACINTOSH
           /* We found the file ok, call setup_preferences to create
            * XLisp Preferences file (this only happens if previous
            * attempt to find the file failed
            */
           setup_preferences(g_xlptemp);
           #endif

           return g_xlptemp;
        }
    }

    /* It wasn't found */
    return NULL;
}


/* xfind_in_xlisp_path -- search XLISPPATH for file, return full path */
LVAL xfind_in_xlisp_path()
{
   LVAL string = xlgastring();
   const char *path = (const char *) getstring(string);
   xllastarg();
   path = find_in_xlisp_path(path);
   return (path ? cvstring(path) : NULL);
}
