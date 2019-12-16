/* userio.c -- handy user interface functions */
/* Copyright 1989 Carnegie Mellon University */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
* 21-May-86 | Created
* 11-Aug-87 | F.H: Added clear_abort(), stop()
*    May-88 | JCD : AMIGA VERSION 
* 11-Jun-88 | RBD: disable printing of GDEBUG messages
* 12-Oct-88 | JCD : EXCLUSIVE AMIGA VERSION
* 13-Apr-89 | JCD : New portable version.
*  5-Apr    | JDW : Further changes
*  2-Mar-92 | GWL : Little changes to satisfy compiler
* 19-Nov-92 | JDZ : Mach tty io threads
* 28-Apr-03 |  DM : portability changes. true->TRUE, false->FALSE
*****************************************************************************/

/* Notes on ascii input:

Input is complicated because different systems have varying input models,
especially with regard to handling ^C.  The CMT model handles ^C and ^G as
special characters, and these do not cause software interrupts.  Also, the
lowest level of the CMT model does not support line editing: Every character
is acted upon immediately.  This has two implications:
(1) CMT must "read ahead" looking for ^C and ^G characters.  This is handled
by the check_aborted() procedure, which reads characters into the 
type_ahead[] array.
(2) CMT must do its own line editing.  This is handled by the ggets() routine.

A number of functions support ascii input, only some of which are visible
to the application programmer.  Let's start at the top-level and work down;
each of the following calls the routine below it:

ggets() - gets a string with line editing support.  This function is fairly
machine independent, except for some backspace-and-erase control character
code sequences.

ggetchar() - gets a raw character.  This function calls wait_ascii()
 and echoes it.  Note that it may return ABORT_CHAR or BREAK_CHAR.

wait_ascii() - gets a raw character without echo and without character
code translation.  wait_ascii() either polls get_ascii() or uses some
kind of system-dependent event waiting.  Returns ABORT_CHAR or BREAK_CHAR
immediately if abort_flag has been set, regardless of whether there is
new ascii input.

get_ascii() - checks to see if a character is available.  (Using
check_aborted().)
The only dependency here is on the Amiga, we restart input when buffer goes
from full to non-full.

check_aborted() - looks for input by calling ascii_input.  If found,
put the input into the type_ahead[] buffer. Returns abort_flag.

ascii_input() - lowest level of input; just gets a character if there is
one.  Does conversion from RETURN (\r) to EOL (\n).  The Amiga handles
this in-line directly in check_aborted().

Here's a quick summary:

ggets = ggetchar + line editing & string building
ggetchar = wait_ascii + character echo
wait_ascii = get_ascii + wait for character
get_ascii = check_aborted + pull char from buffer
check_aborted = ascii_input + test for ^C,^G + put in buffer
ascii_input = poll for char + CR->EOL conversion

*/

#include "switches.h"

#include <stdio.h>
#include <string.h>
#if HAS_STDLIB_H
#include <stdlib.h> /* normal case */
#endif


#ifdef MACINTOSH
# include "StandardFile.h"
  /* added for ThinkC 7 */
# ifdef THINK_C
#  include <pascal.h>
# endif
#endif

#ifdef AMIGA

# ifdef AZTEC
#  include "functions.h"
# else /* LATTICE */
#  include "amiga.h"
#  include "stdarg.h"
# endif

# include "intuition/intuition.h"
# include "devices/console.h"
#endif

#include "ctype.h"
#include "stdio.h"
#include "cext.h"
#include "userio.h"

#ifdef MICROSOFT
#include "signal.h"
#endif

#ifdef UNIX_MACH
#include <varargs.h>
#include <midistruct.h>
extern char a_in;
extern int a_in_flag;
extern int i_am_running;
#ifdef RTMach
extern itc_mutex_t a_mutex;
extern itc_condition_t a_cond, a_in_cond;
#define A_LOCK() itc_mutex_lock(&a_mutex)
#define A_UNLOCK() itc_mutex_unlock(&a_mutex)
#else /* RTMach */
extern struct mutex a_mutex;
extern struct condition a_cond, a_in_cond;
#define A_LOCK() mutex_lock(&a_mutex)
#define A_UNLOCK() mutex_unlock(&a_mutex)
#endif /* RTMach */
#endif

#ifdef DOTS_FOR_ARGS
#include <stdarg.h>
#endif

#ifdef UNIX
#include <sys/param.h>
#include <sys/resource.h>
#include "cmtio.h"
#ifdef _IBMR2
#define NBBY 8
#define OPEN_MAX 2000
#endif
#include <sys/select.h>
#endif

#ifdef linux
#include <sys/time.h> /* for FD_ZERO / FD_SET */
#endif

extern int debug;

#ifdef NYQUIST
/* get definitions for stdputstr, etc. */
#include "xlisp.h"
#endif

int     IOinputfd;      /* input file descriptor (usually 0) */

int     IOnochar;       /* Value to be returned by IOgetchar()
                           where there is no input to be had */

/****************************************************************************
*
*       routines private to this module
*
****************************************************************************/

int GetReadFileName(void);
int GetWriteFileName(void);

#ifdef MACINTOSH
private void PtoC_StrCopy(char *p1, char *p2);
#endif

#ifdef AMIGA
        char ConGetChar();
        ConMayGetChar();
private void ConRead();
private void ConPutStr();
private void ConPutChar();
UBYTE   ascii_signal();
UBYTE   KeybSig();
#endif


/****************************************************************************
*
* variables shared with other modules
*
****************************************************************************/

public int abort_flag;          /* control C or control G equivalent */
public int redirect_flag;		/* check whether the I/O has been redirected--
                                    Added by Ning Hu	Apr.2001*/
/* extern void musicterm(); */ /*DMH: from macmidi.c, to allow abort_check*/
public boolean ascii_input(char *c);

/****************************************************************************
*
* variables private to this module
*
****************************************************************************/

#ifdef AMIGA
struct IntuitionBase *IntuitionBase;
private struct IOStdReq *ConOutReq;
private struct MsgPort *ConOutPort;
private struct IOStdReq *ConInReq;
private struct MsgPort *ConInPort;
private char KeyBuff[16];
private struct Window *Window;
private struct NewWindow NewWindow = {
    0,11,640,189,
    0,1,
    NULL,
    SMART_REFRESH | ACTIVATE | WINDOWDRAG | WINDOWDEPTH |
        WINDOWSIZING,
    NULL,NULL,
    (STRPTR) "Carnegie Mellon University MIDI Toolkit for Commodore AMIGA",
    NULL,NULL,
    100,25,640,200,
    WBENCHSCREEN };
#endif

#ifdef MACINTOSH
private OSType io_file_type = 0x3F3F3F3F; /* '????' */
private OSType io_file_creator = 0x3F3F3F3F; /* '????' */
#endif

#define type_ahead_max 100
char type_ahead[100];
int type_ahead_head = 0;
int type_ahead_tail = 0;
int type_ahead_count = 0;


#ifdef DOS
#ifdef BORLAND
int c_break(void)
{
    gprintf(TRANS, " BREAK ");
    abort_flag = ABORT_LEVEL;
    return 1; /* non-zero means do not exit program */
}
#endif
#ifdef MICROSOFT
void c_break(int sig)
{
    abort_flag = ABORT_LEVEL;
    /* The CTRL+C interrupt must be reset to our handler since
     * by default it is reset to the system handler.
     */
    signal(SIGINT, c_break); /* assume this succeeds */
}
#endif
#endif

#ifdef MACINTOSH
#ifdef NYQUIST
void FlushOutput (void);
#endif
#endif

/* gflush -- flush output produced by gprintf, etc. */
/**/
void gflush(void)
{
#ifdef MACINTOSH
#ifdef NYQUIST
    FlushOutput();
#else
    fflush(stdout);    /* make sure any prompts or errors have been output */
    fflush(STDERR);
#endif /* NYQUIST */
#endif /* MACINTOSH */
#ifdef UNIX
    fflush(stdout);    /* make sure any prompts or errors have been output */
    fflush(STDERR);
#endif
}


/****************************************************************************
*                                io_init
*
*  I added this init function for the AMIGA version. 
*
*  io_init : opens a window
*  and exits if initialisation can not be done properly.
*  registers cleanup calls to carefully deallocate resources.
*
*  io_init is not amiga specific : the simplest version 
*  of io_init could be a clear screen statement for example, and a
*  printf("Good bye !\n") on exit.
*
*  for the Mac, it seems that ascii_input doesn't work unless getchar() is
*  called first.  I assume this is because getchar() initializes the ability
*  of the window to process type-in, so there is probably a way to set this
*  directly.  If you figure it out, let me know.  -RBD
*
*****************************************************************************/

void
io_init()
{

#ifdef AMIGA
    int error;

    /* Window and console initialisation  */
    IntuitionBase = (struct IntuitionBase *)OpenLibrary("intuition.library",1L);
    if (IntuitionBase == NULL) EXIT(1);
    cu_register((cu_fn_type) CloseLibrary, IntuitionBase);
    
    ConOutPort = CreatePort("conoutport", 0L);
    if (ConOutPort == NULL) EXIT(1);
    cu_register((cu_fn_type) DeletePort, ConOutPort);
    
    ConOutReq = CreateStdIO(ConOutPort);
    if (ConOutReq == NULL) EXIT(1);
    cu_register((cu_fn_type) DeleteStdIO, ConOutReq);
    
    ConInPort = CreatePort("coninport", 0L);
    if (ConInPort == NULL) EXIT(1);
    cu_register((cu_fn_type) DeletePort, ConInPort);
    
    ConInReq = CreateStdIO(ConInPort);
    if (ConInReq == NULL) EXIT(1);
    cu_register((cu_fn_type) DeleteStdIO, ConInReq);
    
    Window = OpenWindow(&NewWindow);
    if (Window == NULL) EXIT(1);
    cu_register((cu_fn_type) CloseWindow, Window);
    
    ConOutReq->io_Data = (APTR)Window;
    ConOutReq->io_Length = sizeof(*Window);
    error = OpenDevice("console.device", 0L, (struct IORequest *) ConOutReq, 0L);
    ConInReq->io_Device = ConOutReq->io_Device;
    ConInReq->io_Unit = ConOutReq->io_Unit;
    if (error != NULL) EXIT(1);
    cu_register((cu_fn_type) CloseDevice, ConOutReq);
    
    ConInReq->io_Command = CMD_READ;
    ConInReq->io_Data = (APTR)KeyBuff;
    ConInReq->io_Length = 1;
    SendIO((struct IORequest *) ConInReq);
#endif

#ifdef UNIX
#ifndef BUFFERED_SYNCHRONOUS_INPUT
    IOsetup(0 /* standard input */);
    cu_register((cu_fn_type) IOcleanup, NULL);
#endif
#endif

#ifdef MACINTOSH
#ifndef NYQUIST /* don't need this if we're in Nyquist */
    char s[100];
    printf("Type <return> to start: ");
    fgets(s, 100, stdin);
#endif /* NYQUIST */
#endif

#ifdef DOS
#ifdef MICROSOFT
    if (signal(SIGINT, c_break) == SIG_ERR) {
        gprintf(ERROR, "Couldn't set Ctrl C handler\n");
        EXIT(1);
    }
#else
#ifdef BORLAND
    ctrlbrk(c_break);
#else
    ... we are in DOS, but neither MICROSOFT nor BORLAND,
    please set up a control C handler here...
#endif
#endif
#endif
}

#ifdef MACINTOSH

/****************************************************************************
*                                       abort_check
* Effect:
*       exit nicely if the aborted flag is set
****************************************************************************/

public void abort_check()
{
    if (abort_flag) clean_exit();
}


/****************************************************************************
*                                       clean_exit
* Effect:
*       clean up and exit
****************************************************************************/

public void clean_exit()
{
        gprintf(TRANS, "Exiting...\n");
        EXIT(1);
}

#ifdef MPW
/****************************************************************************
*                                       cleanup_abort_handler
* Effect:
*       shuts down abort watcher
****************************************************************************/

public void cleanup_abort_handler()
{
        (void) sigset(SIGINT, SIG_DFL); /* deactivate abort watcher */
}


/****************************************************************************
*                                       init_abort_handler
* Effect:
*       starts abort watcher
*       aborted flag is set to FALSE
****************************************************************************/

public void init_abort_handler()
{
        abort_flag = FALSE;
        (void) sigset(SIGINT, abort_watcher);   /* activate abort watcher */
}
#endif

#endif


/****************************************************************************
*               askbool
* Inputs:
*    char *prompt: string to prompt for user input
*    int deflt: TRUE or FALSE default
* Returns:
*    boolean: TRUE or FALSE as entered by user
* Effect:
*    prompts user for yes or no input, returns result
****************************************************************************/

int askbool(prompt, deflt)
char *prompt;
int deflt;
{
#define undefined -1
    char defchar;    /* the default answer */
    char c;     /* user input */
    char in_string[100];
    int result = -1;    /* the result: -1 = undefined, 0 = FALSE, 1 = TRUE */
    if (deflt) defchar = 'y';
    else defchar = 'n';
    while (result == undefined) {
        gprintf(TRANS, "%s? [%c]: ", prompt, defchar);
        ggets(in_string);
        c = in_string[0];
        if (islower(c)) c = toupper(c);
        if (c == 'Y') result = TRUE;
        else if (c == 'N') result = FALSE;
        else if (c == EOS) result = deflt;
        else if (abort_flag) result = deflt;
        /* space before Please to separate from user's type-in: */
        else gprintf(TRANS, " Please type Y or N.\n");
    }
    if (abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
        result = deflt;
        gprintf(TRANS, "\n");
    }
    return result;
}


/****************************************************************************
*               fileopen
* Inputs:
*    char *deflt: the default file name (e.g. from command line)
*    char *extension: default extension
*    char *mode: read ("r") or write ("w")
*    char *prompt: prompt for user
* Returns:
*    opened file pointer
* Effect: 
*    opens file, prompts for user input if necessary and warns about
*    possible confusion.  If deflt is a null string or NULL, the user will
*    be prompted for a name.     The routine loops until a file is opened.
*    If the mode is "r", a check is made to see if the file exists
*    with and without the extension.     If both exist a warning is given.
*    For mode "w", a check is made to see if the file will be overwritten.
*    The extension is automatically added if the default or user-typed
*    file has no "."     At the bottom of the loop body, if no file has
*    been opened, the user is prompted for another file name.
****************************************************************************/

char fileopen_name[100];        /* name of the opened file */

FILE *fileopen(deflt, extension, mode, prompt)
  char *deflt;
  char *extension;    /* default extension */
  char *mode;   /* read "r" or write "w" */
  char *prompt;    /* prompt for user */
{
    char extname[100];          /* trial name with extension added */
    FILE *fp = NULL;            /* file corresponding to filename */
    FILE *fpext;                /* file corresponding to extname */
    char *problem = NULL;       /* tells user why he has to try again */

    if (!deflt) deflt = "";     /* treat NULL as the empty string */
    strcpy(fileopen_name, deflt);
    /* keep trying until a good file is found: */
    while (fp == NULL) {
        /* avoid null file names: */
        while (strlen(fileopen_name) == 0) {
#ifndef MACINTOSH
            gprintf(TRANS, "%s : ", prompt);
            ggets(fileopen_name);
            if (abort_flag) {
                if (abort_flag == BREAK_LEVEL) {
                    abort_flag = 0;
                    /* type return since user didn't... */
                    gprintf(TRANS, "\n");
                }
                return NULL;
            }
#else /* use Macintosh file dialog */
            if (mode[0] == 'r') {
                 if (!GetReadFileName(fileopen_name)) return NULL;
             } else if (mode[0] == 'w') {
                 if (!(GetWriteFileName(fileopen_name, prompt))) return NULL;
             } else {
                 gprintf(ERROR, "(fileopen) internal error: bad mode\n");
             }
#endif /* MACINTOSH */
        }
        if (mode[0] == 'r') {
            strcpy(extname, fileopen_name);
            strcat(extname, ".");
            strcat(extname, extension);
            fp = NULL;
            fpext = NULL;
            if (ok_to_open(fileopen_name, mode)) {
                fp = fopen(fileopen_name, mode);
            }
            if (ok_to_open(extname, mode)) {
                fpext = fopen(extname, mode);
            }
            if (fp != NULL && fpext != NULL) {
                gprintf(TRANS,
                "warning: both %s and %s exist.     %s will be used.\n",
                fileopen_name, extname, fileopen_name);
                fclose(fpext);
            } else if (fpext != NULL) {
                fp = fpext;
                strcpy(fileopen_name, extname);  /* remember what was opened */
            }
            if (fp == NULL) problem = "Couldn't find %s.\n";
        } else if (mode[0] == 'w') {
            boolean added_extension = FALSE;
            
            /* add the extension if there is no '.' in the file name */
            if (!strchr(fileopen_name, '.')) {
                strcat(fileopen_name, ".");
                strcat(fileopen_name, extension);
                added_extension = TRUE;
            }
            if (TRUE
#ifdef MACINTOSH
                /* file open dialog already asked user to confirm unless we're
                 * adding an extension
                 */
                && added_extension
#endif
                ) {
                fp = NULL;
                if (ok_to_open(fileopen_name, "r"))
                    fp = fopen(fileopen_name, "r");
                if (fp != NULL) {
                    char question[100];
                    fclose(fp);
                    strcpy(question, "OK to overwrite ");
                    strcat(question, fileopen_name);
                    if (!askbool(question, FALSE)) {
                        fp = NULL;
                        problem = "\n";
                        goto tryagain;
                    }
                }
            }
            fp = NULL;
            if (ok_to_open(fileopen_name, mode))
                fp = fopen(fileopen_name, mode);
            if (fp == NULL) problem = "Couldn't create %s.\n";
        }
  tryagain:
        if (fp == NULL) {
            gprintf(TRANS, problem, fileopen_name);
            gprintf(TRANS,"Try again.\n");
            fileopen_name[0] = EOS;
        }
    }
    return fp;
}

#ifdef MACINTOSH

static int GetReadFileName(name)
char *name;
{
    static Point p = {100,100};
    SFReply loadfile;
    SFTypeList mytypes;
    
    mytypes[0] = 0x54455854; /* 'TEXT' */
    mytypes[1] = 0x4D696469; /* 'Midi' */
    mytypes[2] = 0x3F3F3F3F; /* '????' */
/* could put any filter here (i.e. giofilefileter) */
    SFGetFile(p, "\p", NULL, 3, mytypes, 0L, &loadfile); 
    if (loadfile.good) {
        SetVol(0L,loadfile.vRefNum); 
        PtoC_StrCopy((char *) &loadfile.fName, name);
        return(true);
    } else return(false);
}


static int GetWriteFileName(fn, str)
char *fn, *str;
{
        static Point SFPwhere = { 106, 104 };
        unsigned char Pstr[100], Pfn[100];
        SFReply reply;
 
        strcpy((char *)Pstr, str);
        CtoPstr((char *)Pstr);
        strcpy((char *)Pfn, fn);
        CtoPstr((char *)Pfn);

        SFPutFile(SFPwhere, (ConstStr255Param) Pstr, (ConstStr255Param) Pfn,
                  0L, &reply);
        if (reply.good) {
                SetVol (0L,reply.vRefNum);
                PtoC_StrCopy((char *) &reply.fName, fn); 
                return(true);
        }
        else return(false);
}


void PtoC_StrCopy(p1, p2)
  register char *p1, *p2;
/* copies a pascal string from p1 to p2 */
{
        register int len;
        
        len = *p1++;
        while (--len>=0) *p2++=*p1++;
        *p2 = '\0';
}


boolean get_file_info(char *filename, OSType *file_type, OSType *file_creator)
{
    short rc;           /* toolbox return code */
    FInfo fi;           /* toolbox file info */
    char fn[101];       /* temporary file name */

    strcpy(fn, filename);
    CtoPstr(fn);
    if (rc = GetFInfo((byte*)fn, 0, &fi)) {
        gprintf(ERROR, "rc from GetFInfo=%d\n", rc);
        gprintf(ERROR, "unable to get file type\n");
        *file_type = 0x3F3F3F3F; /* '????' */
        *file_creator = 0x3F3F3F3F; /* '????' */
        return FALSE;
    } else /* set file type & creator */ {
        if (debug) gprintf(TRANS, "File Type: '%.4s'  File Creator: '%.4s'\n",
                           &fi.fdType, &fi.fdCreator );
        *file_type = fi.fdType;
        *file_creator = fi.fdCreator;
    }
    return TRUE;
}


boolean put_file_info(char *filename, OSType file_type, OSType file_creator)
{
    short rc;           /* toolbox return code */
    FInfo fi;           /* toolbox file info */
    char fn[101];       /* temporary file name */

    if (debug) gprintf(TRANS,"set file %s to become type '%.4s'\n", filename, &file_type);
    strcpy(fn, filename);
    CtoPstr(fn);
    if (rc = GetFInfo((byte*)fn, 0, &fi)) {
        gprintf(TRANS, "rc from GetFInfo=%d\n", rc);
        gprintf(TRANS, "unable to set file type\n");
    } else /* set file type & creator */ {
        if (debug) gprintf(TRANS, "File Type: '%.4s'  File Creator: '%.4s'\n",
                           &fi.fdType, &fi.fdCreator );
        fi.fdType = file_type;
        fi.fdCreator = file_creator;
        if (rc=SetFInfo((byte*)fn, 0, &fi)) {
            gprintf(TRANS, "rc from SetFInfo=%d\n", rc);
            gprintf(TRANS, "unable to set file type\n");
        } else if (rc=GetFInfo((byte*)fn, 0, &fi)) {
            gprintf(TRANS, "rc from GetFInfo=%d\n", rc);
            gprintf(TRANS, "unable to verify file type\n");
        } else {
            if (debug) gprintf(TRANS, "File Type: '%.4s'  File Creator: '%.4s'\n",
                               &fi.fdType, &fi.fdCreator );
        }
    }
}
#endif /* MACINTOSH */



#ifdef AMIGA
/***************************************************************
*                           ascii_signal
*
* Input : none
* Ouput : none
* Return: the signal that will be raised on ascii input
* Effect: none
***************************************************************/

UBYTE ascii_signal()
{
    return ConInPort->mp_SigBit;
}
#endif

/* check_aborted -- see if any characters are available, check for ctrl C */

int check_aborted()
{
        char in_c;
#ifdef AMIGA 
    if (GetMsg(ConInPort)) {
        in_c = KeyBuff[0];
        if (in_c == '\r') in_c = '\n';
#endif
#ifndef AMIGA   /* DOS or MACINTOSH or UNIX */
    if (type_ahead_count < type_ahead_max && ascii_input(&in_c)) {
#endif
        type_ahead[type_ahead_tail] = in_c;
        if (in_c == ABORT_CHAR) abort_flag = ABORT_LEVEL;
        else if (!abort_flag && in_c == BREAK_CHAR) 
            abort_flag = BREAK_LEVEL;

        /* go ahead and insert anything into buffer, including ^C, ^G: */
        type_ahead_count++;
        type_ahead_tail++;
        if (type_ahead_tail == type_ahead_max) type_ahead_tail = 0;

#ifdef AMIGA
        if (type_ahead_count < type_ahead_max) ConRead();
#endif
    }
    return abort_flag;
}    


/****************************************************************************
*                   readln
* Inputs:
*    FILE * fp: File to read from
* Effect: 
*    Reads and discards characters until a newline is seen
****************************************************************************/

void readln(fp)
  register FILE *fp;
{
    register int c;
    while (((c = getc(fp)) != '\n') && (c != EOF)) ;
}


/****************************************************************************
*                   gprintf
* Inputs:
*    int * handler: pointer to output handler (say, a window)
*        or one of {TRANS, ERROR, FATAL, GDEBUG} from userio.h
*    char * format: a null-terminated printf style format string
*    int arg0 through arg14: a variable number of arguments for printf
* Effect:
*    formats and outputs the specified information to an output handler.
*    this is a system-independent approach to output.  On
*    a simple machine, it is like printf.  on a more complex machine,
*    output is directed to the appropriate window.
* Implementation
*    Note that to handle the variable argument list, a number of different
*    approaches are implemented.  The first part of the implementation selects
*    one of 4 ways to build temp1, a formatted string.  The 4 ways arise from
*    use or non-use of vsnprintf, and use or non-use of ... in the arg list.
*    After building temp1, non-Amiga systems write to stdout or stderr, 
*    whereas AMIGA writes to a special console.  Why? Because the Amiga
*    needs a new console so we can set up a signal upon character typein.
****************************************************************************/

#ifndef gprintf
#define GPRINTF_MESSAGE_LEN 512
#ifdef HAVE_VSNPRINTF
#ifdef DOTS_FOR_ARGS

/* define with ... in arg list and use vsnprintf to get temp1 */
public void gprintf(long where, const char *format, ...)
{
    char temp1[GPRINTF_MESSAGE_LEN];
#ifdef AMIGA
    char temp2[GPRINTF_MESSAGE_LEN];
#endif
    va_list ap;

    va_start(ap, format);
    vsnprintf(temp1, GPRINTF_MESSAGE_LEN, format, ap);
    va_end(ap);

#else /* !DOTS_FOR_ARGS */

/* define with va_alist and use vsnprintf to get temp1 */
public void gprintf(where, format, va_alist)
long where;
char *format;
va_dcl
{
    char temp1[GPRINTF_MESSAGE_LEN];
    va_list pvar;
/* this is a syntax error - if you don't have to remove this, */
/* then this whole section of code is unnecessary. */
    va_start(pvar);
    vsnprintf(temp1, GPRINTF_MESSAGE_LEN, format, pvar);
    va_end(pvar);

#endif /* DOTS_FOR_ARGS */

#else /* !HAVE_VSNPRINTF */
#define MAX_GPRINTF_ARGS 10
typedef struct gp_args_struct {
    long arg[MAX_GPRINTF_ARGS];
} gp_args_node;

#ifdef DOTS_FOR_ARGS
/* use ... but not vsnprintf */
public void gprintf(long where, char *format, ...)
{
    char temp1[GPRINTF_MESSAGE_LEN];
#ifdef AMIGA
    char temp2[GPRINTF_MESSAGE_LEN];
#endif
    va_list ap;
    gp_args_node args;
    va_start(ap, format);
    args = va_arg(ap, gp_args_node);
    va_end(ap);
#else /* !DOTS_FOR_ARGS */
/* don't use ... and don't use vsnprintf */
public void gprintf(where, format, args)
  long where;
  char *format;
  gp_args_node args;
{
    char temp1[GPRINTF_MESSAGE_LEN];
#ifdef AMIGA
    char temp2[GPRINTF_MESSAGE_LEN];
#endif /* AMIGA*/
#endif /* DOTS_FOR_ARGS */

    snprintf(temp1, GPRINTF_MESSAGE_LEN, format, args);

#endif /* HAVE_VSNPRINTF */

/*
 * Now we've got formatted output in temp1.  Write it out.
 */
#ifdef NYQUIST
    switch ((long) where) {
      case TRANS:
          stdputstr(temp1);
          break;
      case ERROR:
        errputstr(temp1);
        break;
      case FATAL:
        errputstr("FATAL: ");
        errputstr(temp1);
        break;
      case GDEBUG:
        errputstr("DEBUG: ");
        errputstr(temp1);
        break;
      default:
        errputstr("UNKNOWN: ");
        errputstr(temp1);
        break;
    }
    gflush();
#else /* not NYQUIST */
#ifdef AMIGA

    switch((long) where) {
      case TRANS:
        strcpy(temp2, temp1);
        break;
      case ERROR:
        strcpy(temp2, temp1);
        break;
      case FATAL:
        strcpy(temp2, "FATAL: ");
        strcat(temp2, temp1);
        break;
      case GDEBUG:
        strcpy(temp2,"DEBUG: ");
        strcat(temp2, temp1);
        break;
      default:
        strcpy(temp2, "UNKNOWN: ");
        strcat(temp2, temp1);
        break;
    }
    ConOutReq->io_Command = CMD_WRITE;
    ConOutReq->io_Data = (APTR)temp2;
    ConOutReq->io_Length = -1;   /* NULL terminated string */
    DoIO((struct IORequest *) ConOutReq);            
#else /* not NYQUIST or AMIGA */
    switch(where) {
      case TRANS:
        printf("%s", temp1);
        break;
      case ERROR:
        fprintf(STDERR, "%s", temp1);
        break;
      case GDEBUG:
        fprintf(STDERR, "DEBUG %s", temp1);
        break;
      case FATAL:
        fprintf(STDERR, "FATAL %s", temp1);
        break;
      default:
        fprintf(STDERR, "UNKNOWN %s", temp1);
        break;
    }
#endif /* AMIGA */
#endif /* NYQUIST */
}

#endif  /* ifndef gprintf */


/**************************************************************************
*                               gputchar
* General putchar
**************************************************************************/

#ifndef gputchar

#ifdef AMIGA
public int gputchar(c)
int c;
{
    ConPutChar((char)c);
    return(c);
}
#else
public int gputchar(c)
int c;
{
    putchar((char)c);
    return(c);
}
#endif

#endif  /* ifndef gputchar */

/**************************************************************************
*                               ggetchar
* General getchar
**************************************************************************/

public int ggetchar()
{
#ifdef BUFFERED_SYNCHRONOUS_INPUT
    return getchar();
#else
    int key = wait_ascii();
    if (key != ABORT_CHAR && key != '\b') gputchar((char)key);
    return(key);
#endif
}


/**************************************************************************
*                                  ggets
* General gets
**************************************************************************/


#ifndef ggets

public char *ggets(str)
  char *str;
{
    char *s = str;
    int c;

    do {
        c = ggetchar();
        if (c == '\b' /* backspace */) {
            if (s != str) {
                gputchar('\b');
                gputchar((int)' ');
                gputchar('\b');
                s--;
            } else {
#ifdef AMIGA
                gputchar((int)0x9b);
                gputchar((int)0x43);
#else
                /* gputchar((int)' '); */
#endif
                gputchar((int)0x07);
            }
        } else *s++ = (char) c;
    } while (c != (int) '\n' && !abort_flag);

    *(s-1) = EOS;
    if (abort_flag) *str = EOS;
    return str;
}

#endif  /* ifndef ggets */


/****************************************************************************
*                 get_ascii
* Returns:
*    boolean: TRUE if a character was found
*    int * c: pointer to int into which to store the character, if any
* Effect:
*    polls (doesn't wait) for an ascii character and says if it got one
*    the character is returned in *c.
****************************************************************************/

public boolean get_ascii(c)
  char *c;
{
    check_aborted(); /* input buffer check */
    if (type_ahead_count == 0) return FALSE;
#ifdef AMIGA
    /* if the buffer is full, then there is no outstanding read, restart it: */
    if (type_ahead_count == type_ahead_max) ConRead();
#endif
    type_ahead_count--;
    *c = type_ahead[type_ahead_head++];
    if (type_ahead_head == type_ahead_max) type_ahead_head = 0;
    return TRUE;
}

#ifdef MACINTOSH  /** Macintosh direct ascii input**/
public boolean ascii_input(c)
char *c;
{
    EventRecord theEvent;

    (void) GetNextEvent((keyDownMask | autoKeyMask), &theEvent);
    if ((theEvent.what == keyDown) || (theEvent.what == autoKey)) {
        *c = theEvent.message & charCodeMask;
        if (*c == '\r') *c = '\n';
        return(true);
    } 
    else {
        return(false);
    }
}
#endif

#ifdef WINDOWS
#include "conio.h"
#define kbhit _kbhit
#define getch _getch
#endif

#ifdef DOS
public boolean ascii_input(c)
char *c;
{
    if (abort_flag == ABORT_LEVEL) {
            *c=ABORT_CHAR;
        return((boolean)TRUE);
    }
    if (kbhit()) {              /* If the keyboard was hit */
        *c = getch();           /* Don't echo it */
//      printf("now break");
        if (*c == '\r') *c = '\n';
        return((boolean)TRUE);
    }
    return((boolean)FALSE);     /* Keeps Lattice compiler happy */
}
#endif

#ifdef UNIX
public boolean ascii_input(char *c)
{
#ifdef UNIX_MACH
        /*
         * we can't read from stdin directly, because the ascii
         * input thread is already doing so, so instead we'll
         * wait for that thread to read a character and then take
         * it
         */
        boolean ret = FALSE;

        A_LOCK();
                if (a_in_flag) {
                        (*c) = a_in;
                        a_in_flag = 0;
                        ret = TRUE;
                }
        A_UNLOCK();
        if (ret) {
#ifdef RTMach
                itc_condition_signal(&a_cond);
#else /* RTMach */
                condition_signal(&a_cond);
#endif /* RTMach */
        }
        if ((*c) == '\r')
                (*c) = '\n';
        return(ret);
#else /* __APPLE__ */
#ifndef BUFFERED_SYNCHRONOUS_INPUT
    int input = IOgetchar();
    if (input != IOnochar) {
        *c = input;
        if (*c == '\r') *c = '\n';
        return TRUE;
    }
#endif /* BUFFERED_SYNCHRONOUS_INPUT */
    return FALSE;
#endif /* __APPLE__ */
}
#endif

#ifndef AMIGA /*DOS and MAC and UNIX */
public void unget_ascii(char c)
{
        if (type_ahead_head == 0) type_ahead_head = type_ahead_max;
        type_ahead_head--;
        type_ahead[type_ahead_head] = c;
        type_ahead_count++;
}


public boolean check_ascii(void)
{
        char c;
        
        if(get_ascii(&c)) {
                unget_ascii(c);
                return TRUE;
        }
        else return FALSE;
}
#endif


/****************************************************************************
*                   wait_ascii
* Returns:
*    int: character for key pressed
* Effect:
*    waits for the user to type a key on the terminal keyboard
*    (versus the synthesizer keyboard) and returns the key typed
****************************************************************************/

#ifdef MACINTOSH
public int wait_ascii()
{
    char key ;    /* key typed */

    if (abort_flag == ABORT_LEVEL) return ABORT_CHAR;
    if (abort_flag == BREAK_LEVEL) return BREAK_CHAR;
    gflush();
    while (!get_ascii(&key)) ;
    return(key); 
}
#endif

#ifdef DOS
public int wait_ascii()
{
    char key ;    /* key typed */

    if (abort_flag == ABORT_LEVEL) return ABORT_CHAR;
    if (abort_flag == BREAK_LEVEL) return BREAK_CHAR;
    if (!get_ascii(&key)) {
        key = _getch(); // block until we get an input
    }
    /* GWL - check for abort on previos line */
    return (int)key; 
}
#endif

#ifndef MACINTOSH
#ifndef DOS
public int wait_ascii()
{
#ifdef UNIX /* was defined (UNIX) || defined(ITC) */
#ifndef UNIX_MACH
        fd_set readfds;
#endif /* !UNIX_MACH */
#endif
    char c;
    struct rlimit file_limit;

    if (abort_flag == ABORT_LEVEL) return ABORT_CHAR;
    if (abort_flag == BREAK_LEVEL) return BREAK_CHAR;
    while (!get_ascii(&c)) {
#ifdef AMIGA
        WaitPort(ConInPort);
#endif
#ifdef UNIX
        fflush(stdout);
#ifdef UNIX_MACH
        /*
         * we can't select, since another thread is reading
         * from stdin, and we don't want to have an input war
         * so instead, the ascii input thread will signal
         * a_in_cond when it gets input, so we just wait
         * for that to happen
         */
        A_LOCK();
#ifdef RTMach
                itc_condition_wait(&a_in_cond, &a_mutex);
#else /* RTMach */
                condition_wait(&a_in_cond, &a_mutex);
#endif /* RTMach */
        A_UNLOCK();
#else /* UNIX_MACH */
        FD_ZERO(&readfds);
        FD_SET(IOinputfd, &readfds);
        gflush();
        getrlimit(RLIMIT_NOFILE, &file_limit);
        select((int) (file_limit.rlim_max+1), &readfds, 0, 0, NULL);
#endif /* !__APPLE__ */
#endif /* ifdef UNIX */
    }
    return (int) c;
}
#endif
#endif

#ifdef AMIGA
/******************************************************************
                              AMIGA 2000.
                         Console IO Functions
                             JCD 25-Apr-88
*******************************************************************/

UBYTE KeybSig()
{
    return ConInPort->mp_SigBit;
}

private void ConPutChar(c)
char c;
{
    ConOutReq->io_Command = CMD_WRITE;
    ConOutReq->io_Data = (APTR)&c;
    ConOutReq->io_Length = 1;
    DoIO((struct IORequest *) ConOutReq);
}

private void ConPutStr(str)
char *str;
{
    ConOutReq->io_Command = CMD_WRITE;
    ConOutReq->io_Data = (APTR)str;
    ConOutReq->io_Length = -1;
    DoIO((struct IORequest *) ConOutReq);
}

private void ConRead()
{
    ConInReq->io_Command = CMD_READ;
    ConInReq->io_Data = (APTR)KeyBuff;
    ConInReq->io_Length = 1;
    SendIO((struct IORequest *) ConInReq);
}
#endif
