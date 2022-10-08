/* security.c -- manage access to files and audio */
/* Roger B. Dannenberg
 * July 2014
 */

#include <stdlib.h>
#include "switches.h"
#if defined(UNIX) || defined(__APPLE__) || defined(__CYGWIN__)
#include <unistd.h>
#endif
#ifdef WIN32
#include <direct.h>
#endif
#include <string.h>
#include <xlisp.h>

static void find_full_path(const char *filename, char *fullname);
static int in_tree(const char *fullname, char *secure_read_path);
static int full_name(const char *filename);
static int file_sep(char c);

/* run_time_limit is a feature to shut down infinite loops
 * calls to oscheck are counted. These occur at around 66Hz
 * on a 2010 vintage laptop, so a value of 66 is roughly 1 CPU
 * second of computation. The rate fluctuates depending on
 * the computation, so it is a very rough guide, but then if
 * you are restricting CPU time, you'll probably want to pad
 * the expected time by a factor of at least 2, so hopefully
 * exact numbers are not important. The default is 0, meaning
 * no limit. Set the limit from the command line with -L. 
 * Read the current run time by calling (GET-RUN-TIME) 
 */
int run_time_limit = 0;

/* memory_limit is a feature to shut down rampant memory
 * allocation which tends to lead to thrashing on a virtual
 * (memory) machine. The number is roughly the number of
 * megabytes of sample and cons cell memory that can be
 * allocated. The default is 0 which means no limit.
 */
int memory_limit = 0;

/* run_time is the current run time -- incremented by oscheck() */
int run_time = 0;

/* secure_read_path is NULL if reading any file is permitted
 * o.w. secure_read_path is a semicolon-separated list of 
 * paths to directory trees that are readable
 */
char *secure_read_path = NULL;

/* safe_write_path is NULL if writing any file is permitted
 * o.w. safe_write_path is a semicolon-separated list of
 * paths to directory tress that are writeable
 */
char *safe_write_path = NULL;

/* ok_to_open - true if it is OK to open the file under
 *       the security model implied by secure_read_path
 *       and safe_write_path
 */
int ok_to_open(const char *filename, const char *mode)
{
    char fullname[STRMAX];
    if (strchr(mode, 'r')) { /* asking for read permission */
	if (secure_read_path) { /* filename must be in path */
	    find_full_path(filename, fullname);
	    if (!in_tree(fullname, secure_read_path)) return FALSE;
	}
    }
    if (strchr(mode, 'w')) { /* asking for write permission */
	if (safe_write_path) { /* filename must be in path */
	    find_full_path(filename, fullname);
	    if (!in_tree(fullname, safe_write_path)) return FALSE;
	}
    }
    return TRUE;
}


/* find_full_path - find full path corresponding to filename */
/**/
void find_full_path(const char *filename, char *fullname)
{
    if (full_name(filename)) {
	strncpy(fullname, filename, STRMAX);
	fullname[STRMAX - 1] = 0;
	return;
    }
    if (!getcwd(fullname, STRMAX)) {
	/* something is really wrong. Pretend we found a
	   cwd that will not match anything */
	goto error;
    }
    /* see if we need a separator (probably) */
    int len = (int) strlen(fullname);
    if (!file_sep(fullname[len - 1])) {
	fullname[len++] = '/';
	if (len >= STRMAX) goto error; 
    }
    /* append filename to fullname */
    strncpy(fullname + len, filename, STRMAX - len);
    fullname[STRMAX - 1] = 0; /* just in case of overflow */
    /* if windows, replace \ with / to simplify the rest */
    char *loc = fullname;
    if (os_pathchar != '/') {
        while ((loc = strchr(loc, os_pathchar))) {
	    *loc = '/';
	}
    }
    /* strip out .. and . */
    while ((loc = strstr(fullname, "/.."))) {
	/* back up to separator */
	if (loc == fullname) goto error;
	char *loc2 = loc - 1;
	while (*loc2 != '/') {
	    loc2--;
	    if (loc2 <= fullname) goto error;
	}
	/* now loc2 points to /parent/.., and loc points to /.. */
	/* copy from beyond /.. to loc2 */
	memmove(loc2, loc, strlen(loc) + 1);
    }
    return;
  error:
    strcpy(fullname, "//////");
    return;
}


static int file_sep(char c)
{
    return (c == os_pathchar || c == '/');
}


/* full_name - test if filename is a full path */
/**/
static int full_name(const char *filename)
{
    if (!filename) return FALSE;
    if (filename[0] == os_pathchar) return TRUE;
    /* windows allows '/' instead of '\' */
    if (filename[0] == '/') return TRUE;
    if (strlen(filename) > 2 &&
	isalpha(filename[0]) &&
	filename[1] == ':') return TRUE;
    return FALSE;
}


static int in_tree(const char *fullname, char *secure_read_path)
{
    /* fullname is in a tree if a path in secure_read_path
       is a prefix of fullname
       Algorithm: extract each path, check for prefix
    */
    char path[STRMAX];
    while (secure_read_path && *secure_read_path) {
	/* skip over separator */
	while (*secure_read_path == os_sepchar ||
	       *secure_read_path == ';') secure_read_path++;
	/* find next directory and copy into path*/
	path[0] = 0;
	int i = 0;
	while (*secure_read_path && (*secure_read_path != os_sepchar &&
				     *secure_read_path != ';')) {
	    path[i++] = *secure_read_path++;
	}
	path[i] = 0;
	/* see if directory is a prefix of fullname */
	if (strstr(fullname, path) == fullname) {
	    return TRUE;
	}
    }
    return FALSE;
}

