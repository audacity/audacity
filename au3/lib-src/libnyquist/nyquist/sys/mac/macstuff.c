/* macstuff.c - macintosh interface routines for xlisp */
/* Written by Brian Kendig. */
/* This file contains the stuff that the other xlisp files call directly. */

#include "cext.h"
#include <stdio.h>
#include <stdarg.h>
#include <QuickDraw.h>	/* for Random */
#include <Memory.h>		/* for DisposePtr */
#include <SegLoad.h>	/* for ExitToShell */
#include "xlisp.h"
#include <string.h>
#include "macint.h"
#include "MacCommandWin.h"
#define DELETE 0x08

/* externals */
extern FILE *tfp;  /* transcript file pointer */
extern int cursorPos;
extern char *macgets (void);

/* local variables */
int lposition;
static char *linebuf = NULL, *lineptr;
static int numChars;

/* system-dependent variable definitions */
static const char os_pathchar = ':';
static const char os_sepchar = ',';


int isascii (char c) { return 1; }  /* every char is an ascii char, isn't it? */

void osinit (char *banner) {
#ifdef SAFE_NYQUIST
SAFE_NYQUIST is not supported in macstuff.c
#endif
    int i;
    char version[] = "\nMacintosh interface by Brian Kendig, Erik A. Dahl, and Dominic Mazzoni.\n";
    InitMac ();  /* initialize the mac interface routines */
    lposition = 0;  /* initialize the line editor */
    for (i = 0; banner[i] != '\0'; i++) macputc (banner[i]);
    for (i = 0; version[i] != '\0'; i++) macputc (version[i]);
}

FILE *osaopen (char *name, char *mode) {
    return fopen (name, mode);
}

FILE *osbopen (char *name, char *mode) {
    FILE *f;
    char nmode[4];
    strcpy (nmode, mode); strcat (nmode, "b");
    f = fopen(name, nmode);
    return f;
}

int osclose (FILE *fp) { return (fclose (fp)); }
int osaputc (int ch, FILE *fp) { return (putc (ch, fp)); }
int osbputc (int ch, FILE *fp) { return (putc (ch, fp)); }

/* osagetc - get a character from an ascii file */
int osagetc(fp)
  FILE *fp;
{
    return (getc(fp));
}

int ostgetc (void) {
    int i;

    if (numChars <= 0) {  /* get some more */
        if (linebuf) DisposePtr (linebuf);
        linebuf = macgets ();
        i = 0;
        while (linebuf[i] != '\0') i++;
        numChars = i;
        if (tfp) for (i = 0; i < numChars; i++) osaputc (linebuf[i], tfp);
        lineptr = linebuf;
    }
    numChars--;
    if (*lineptr == '\r') {
        lineptr++;
        return '\n';
    } else return (*lineptr++);
}

void ostputc (int ch) {
    macputc (ch);
    if (tfp) osaputc (ch, tfp);
}

void osflush (void) {
    lineptr = linebuf;
    numChars = 0;
    lposition = 0;
}

void oscheck (void) { DoEvent (); }

void oserror (char *msg) {
    char line[100], *p;
    sprintf (line,"error: %s\n",msg);
    for (p = line; *p != '\0'; ++p) ostputc (*p);
}

void osfinish(void) {
    /* dispose of everything... */
    if (linebuf) DisposePtr(linebuf);
    portaudio_exit();
    MacWrapUp ();
    ExitToShell ();
}

#define GPRINTF_MESSAGE_LEN 500

/* nyquist_printf -- system independent version of printf */
/*
 * this function prints to console like printf, but using GUI
 * rather than stdio when appropriate.
 *
 */
void nyquist_printf(char *format, ...)
{
    char temp[GPRINTF_MESSAGE_LEN];
    va_list pvar;
    char *p = temp;
    va_start(pvar, format);
    vsnprintf(temp, GPRINTF_MESSAGE_LEN, format, pvar);
    va_end(pvar);
    while (*p) ostputc(*p++);
}

int renamebackup (char *filename) { return 0; }

static FSSpec prefsFSSpec;
static int need_preferences_file = false;
static char xlisp_path[1024]; /* cache for the path */
static int valid_xlisp_path = false;

/* xsetupconsole -- used to configure window in Win32 version */
LVAL xsetupconsole() { return NIL; }


/* this should really be in a header for MacFileUtils.c */
void GetFullPath(FSSpec *theSpec, StringPtr theName);


void get_xlisp_path(char *p, long p_max, int *prefs_found)
{
    Str63 fileName = "\pXLisp Preferences";
    SInt16 foundPrefVRefNum = 0;
    SInt32 foundPrefDirID = 0;
    OSErr err = noErr;
    *p = 0; /* initialize to empty string */
    *prefs_found = false;
    /* if we find path in the cache, copy and return */
    if (valid_xlisp_path) {
        *prefs_found = true;
        strcpy(p, xlisp_path + 10); /* remember, path has XLISPPATH= at head */
        return;
    }
    /* if we've been here before, do not try opening again */
    if (need_preferences_file) return;
    err = FindFolder(kOnSystemDisk, kPreferencesFolderType,
                     kDontCreateFolder, &foundPrefVRefNum,
                     &foundPrefDirID);
    if (err == noErr) {
        err = FSMakeFSSpec(foundPrefVRefNum, foundPrefDirID,
                            fileName, &prefsFSSpec);
        *prefs_found = (err == noErr);
        need_preferences_file = !*prefs_found;
    }
    if (*prefs_found) {
        FILE *pf;
        GetFullPath(&prefsFSSpec, (StringPtr) xlisp_path);
        P2CStr((StringPtr) xlisp_path);
        pf = fopen(xlisp_path, "r");
        if (!pf) {
            return; /* problem opening the path */
        }
        while (fgets(xlisp_path, 1023, pf)) {
            if (strncmp(xlisp_path, "XLISPPATH=", 10) == 0) {
                valid_xlisp_path = true;
                xlisp_path[strlen(xlisp_path) - 1] = 0; /* trim newline */
                strcpy(p, xlisp_path + 10);
                break;
            }
        }
        fclose(pf);
    }
}


/* this is called when we load a file -- if need_preference_file, 
 * we will build a preference file and insert the path of the file
 * we just opened, assuming it will tell us where to find init.lsp
 */
void setup_preferences(char *filename)
{
    if (need_preferences_file) {
        unsigned char prefname[256];
        FILE *pf;
        char *cp;
        int len = 0;
        GetFullPath(&prefsFSSpec, prefname);
        need_preferences_file = false;
        P2CStr(prefname);
        /* we expect file-not-found error, path is valid */
        pf = fopen((char *) prefname, "w");
        if (pf == NULL) return;
        cp = strrchr((char *) filename, ':');
        if (cp == NULL) return;
        cp[1] = 0;
        /* now, filename is the path. If filename ends in runtime, this
         * is probably the standard nyquist runtime folder. We should put
         * the nyquist lib folder on the path too.
         */
        len = cp + 1 - filename;
        if (len >= 9 &&
            strcmp(filename + len - 9, ":runtime:") == 0) {
            filename[len - 8] = 0;
            fprintf(pf, "XLISPPATH=%sruntime:,%slib:\n", filename, filename);
        } else {
            fprintf(pf, "XLISPPATH=%s\n", filename);
        }
        fclose(pf);
    }
}
