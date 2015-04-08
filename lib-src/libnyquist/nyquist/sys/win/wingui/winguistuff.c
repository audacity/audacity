/* winstuff.c - windows interface routines for xlisp */
/* Written by Chris Tchou. */
/* This file contains the stuff that the other xlisp files call directly. */

#include "windows.h"
#include <stdio.h>
//#include <QuickDraw.h>	/* for Random */
#include <memory.h>		/* for DisposPtr */
#include <string.h>
//#include <SegLoad.h>	/* for ExitToShell */
#include "xlisp.h"
#include "textio.h"

#if OSC
#include "sliders.h" /* define sliders */
#include "sound.h" /* define nosc_enabled */
#endif
#include "falloc.h" /* define table_memory */

const char os_pathchar = '\\';
const char os_sepchar = ',';


/* externals */
extern FILE *tfp;  /* transcript file pointer */
extern int cursorPos;
extern char *macgets (void);

#define LBSIZE 200

/* local variables */
static char lbuf[LBSIZE];
static int lpos[LBSIZE];
static int lindex;
static int lcount = 0;
static int lposition;
static int line_edit = TRUE;

//int isascii (char c) { return 1; }  /* every char is an ascii char, isn't it? */

void osinit(const char *banner) {
//	int i;
    char version[] = "\nWindows console interface by Roger Dannenberg.\n";
//	InitMac ();  /* initialize the mac interface routines */
//	lposition = 0;  /* initialize the line editor */
//	for (i = 0; banner[i] != '\0'; i++) macputc (banner[i]);
//	for (i = 0; version[i] != '\0'; i++) macputc (version[i]);
    nyquist_printf(banner);
    nyquist_printf(version);
}


FILE *osaopen (const char *name, const char *mode) {
    FILE *fp = NULL;
    if (ok_to_open(name, mode))
        fp = fopen (name, mode);
    return fp;
}

FILE *osbopen (const char *name, const char *mode) {
    FILE *fp = NULL;
    char nmode[4];
    strcpy (nmode, mode); strcat (nmode, "b");
    if (ok_to_open(name, mode))
        fp = fopen (name, mode);
    return fp;
}

int osclose (FILE *fp) { return (fclose (fp)); }
int osaputc (int ch, FILE *fp) { return (putc (ch, fp)); }
int osbputc (int ch, FILE *fp) { return (putc (ch, fp)); }
void osoutflush(FILE *fp) { fflush(fp); }

/* osagetc - get a character from an ascii file */
int osagetc(fp)
  FILE *fp;
{
    return (getc(fp));
}


extern int abort_flag;


#define OLDGETC
#ifdef OLDGETC

int ostgetc (void) {
/*	int i;

    if (numChars <= 0) {  /* get some more */
/*		if (linebuf) DisposPtr (linebuf);
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
    } else return (*lineptr++);*/

    int ch = ggetchar();
    oscheck(); /* in case user typed ^C */
    if (ch == BREAK_CHAR && abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
    }
    return ch;
}

#else

void end_of_line_edit()
{
    line_edit = FALSE;
    if (tfp) {
    for (lindex = 0; lindex < lcount; ++lindex)
        osaputc(lbuf[lindex], tfp);
    }
    lindex = 0;
}



int ostgetc()
{
/*
 * NOTE: lbuf[] accumulates characters as they are typed
 *   lpos[] is the column position of the characters
 *   lcount is the number of characters in lbuf
 *   lposition is current position
 *   lindex is index of next char to output
 *   line_edit is true iff we're inputing characters
 *
 */
    int ch;

    while (line_edit) {
        ch = ggetchar();
        oscheck(); /* in case user typed ^C */
            if (ch == BREAK_CHAR && abort_flag == BREAK_LEVEL) {
                abort_flag = 0;
            }
        /* assume for now we should add the character */
        lbuf[lcount] = ch;
        lpos[lcount] = lposition;
        lcount++;
        lposition++;

        /* now do all the special character processing */
        switch (ch) {
          case '\n':
            lposition = 0;
            end_of_line_edit();
            gputchar('\r');
            gputchar(ch);
            break;
          /* delete key generates: 1b, 5b, 33, 7E
                 which is: ESC, [, 3, ~ */
          case '\010':	/* backspace */
          case '\177':	/* delete */
            lcount--; /* take out backspace or delete char */
            lposition--;
            if (lcount) {
            lcount--;
            while (lposition > lpos[lcount]) {
                      gputchar('\010');
                      gputchar(' ');
                      gputchar('\010');
                lposition--;
            }
            }
            break;
          case '\025': /* control-u */
            lcount--;
            lposition--;
            if (lcount) {
                while (lposition > lpos[0]) {
                      gputchar('\010');
                      gputchar(' ');
                      gputchar('\010');
                lposition--;
            }
            lcount = 0;
            }
            break;

              /* note that control-z never reaches here */
              case '\003':	/* control-c */
            xltoplevel();
            lcount = 0;
            break;
              case '\007':	/* control-g */
            xlcleanup();
            lcount = 0;
            break;
              case '\020':	/* control-p */
            xlcontinue();
            lcount = 0;
            break;
          case '\002':
            ostputc('\n');	/* control-b */
            xlbreak("BREAK",s_unbound);
            break;
          case '\024':	/* control-t */
            xinfo(); 
            lcount = 0;
            break;
          case '\t':	/* TAB */
            lposition--; /* undo the increment above */
            do {
            lposition++;
                    gputchar(' ');
            } while (lposition & 7);
            break;
          default:
            gputchar(ch);
            break;
        }
    }
    if (lindex + 1 >= lcount) {
        lcount = 0;
        line_edit = TRUE;
    }
    ch = lbuf[lindex++];
    /* printf("[%c]", ch); */
    fflush(stdout);
    return ch;
}
#endif


void ostputc (int ch) {
//	macputc (ch);
    gputchar(ch);			// console

    if (tfp) osaputc (ch, tfp);
}

void ostoutflush()
{
    if (tfp) fflush(tfp);
    /* since ostputc calls gputchar which just calls putchar,
       I'm going to flush stdout rather than extending the
       "g" abstraction with a gflush() call. -RBD
     */
    fflush(stdout);
}

void osflush (void) {
    lindex = lcount = lposition = 0; 
    line_edit = TRUE;
}

extern int abort_flag;

void oscheck (void) 
{

#if OSC
    if (nosc_enabled) nosc_poll();
#endif

    check_aborted(); 
    if (abort_flag == ABORT_LEVEL) {
        abort_flag = 0;
        osflush();
        xltoplevel();
    } else if (abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
        osflush();
        xlbreak("BREAK",s_unbound);
    }
    run_time++;
    if (run_time % 30 == 0) {
        // maybe we should call fflush here like in Unix; I'm not sure if this is 
	// a bug or it is not necessary for Windows - RBD
        if (run_time_limit > 0 && run_time > run_time_limit) {
            xlfatal("Run time limit exceeded");
        }
	if (memory_limit > 0 &&  
	    npools * MAXPOOLSIZE + table_memory + total > 
	    memory_limit * 1000000) {
            xlfatal("Memory limit exceeded");
	}
    }	
}

void oserror(const char *msg) {
    char line[100], *p;
    sprintf (line,"error: %s\n",msg);
    for (p = line; *p != '\0'; ++p) ostputc (*p);
}

void osfinish(void) {
    portaudio_exit();
    /* dispose of everything... */
//	if (linebuf) DisposPtr (linebuf);
//	MacWrapUp ();
//	ExitToShell ();
}

int renamebackup (char *filename) { return 0; }



static WIN32_FIND_DATA FindFileData;
static HANDLE hFind = INVALID_HANDLE_VALUE;
#define OSDIR_LIST_READY 0
#define OSDIR_LIST_STARTED 1
#define OSDIR_LIST_DONE 2
static osdir_list_status = OSDIR_LIST_READY;
#define OSDIR_MAX_PATH 256
static char osdir_path[OSDIR_MAX_PATH];

// osdir_list_start -- prepare to list a directory
int osdir_list_start(const char *path)
{
    if (strlen(path) >= OSDIR_MAX_PATH - 2) {
        xlcerror("LISTDIR path too big", "return nil", NULL);
        return FALSE;
    }
    if (!ok_to_open(path, "r")) return FALSE;
    strcpy(osdir_path, path);
    strcat(osdir_path, "/*"); // make a pattern to match all files
    if (osdir_list_status != OSDIR_LIST_READY) {
        osdir_list_finish(); // close previously interrupted listing
    }
    hFind = FindFirstFile(osdir_path, &FindFileData); // get the "."
    if (hFind == INVALID_HANDLE_VALUE) return FALSE;
    if (FindNextFile(hFind, &FindFileData) == 0) return FALSE; // get the ".."
    osdir_list_status = OSDIR_LIST_STARTED;
    return TRUE;
}


const char *osdir_list_next()
{
    if (FindNextFile(hFind, &FindFileData) == 0) {
        osdir_list_status = OSDIR_LIST_DONE;
        return NULL;
    }
    return FindFileData.cFileName;
}

void osdir_list_finish()
{
    if (osdir_list_status != OSDIR_LIST_READY) {
        FindClose(hFind);
    }
    osdir_list_status = OSDIR_LIST_READY;
}


/* xechoenabled -- set/clear echo_enabled flag (unix only) */
LVAL xechoenabled()
{
	int flag = (xlgetarg() != NULL);
    xllastarg();
	// echo_enabled = flag; -- do nothing in Windows
	return NULL;
}

