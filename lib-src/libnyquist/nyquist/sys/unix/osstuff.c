/* unixtuff.c - unix interface routines for xlisp

 * HISTORY
 * 5-Mar-07 Dannenberg
 *  worked on hidden_msg() and hidden message handling
 *
 * 23-Dec-05	Dannenberg
 *  still more hacks: Mac and Linux don't disable character echo like 
 *  windows does using a pipe to an IDE. To make UNIX versions match
 *  the Windows behavior (which is preferable), added
 *  echo_enabled flag and a function to set/clear it from XLisp.
 *  This will give unix-specific behavior to compensate for the
 *  unix-specific character echo. This worked, but printed
 *  (echoenabled nil) on the console, which was pretty ugly, so I
 *  added ctrl-e and ctrl-f handlers to turn echo on and off. Now
 *  Java can just send ctrl-f before anything else. Windows must
 *  ignore ctrl-f.
 *
 * 28-Apr-03	Mazzoni
 *  many changes for new conditional compilation organization
 *
 * 28-Jun-95	Dannenberg
 *	removed buffering (which could overflow) from ostgetc.
 *
 * 2-Aprl-88	Dale Amon at CMU-CSD
 *	Upgraded to xlisp 2.0. Used msstuff.c as a template.
 *
 * 20-Apr-87	Dale Amon at CMU-CSD
 *	Added control-c interrupt handler. Puts user in breakloop and allows
 *	continue. Prints line at which the interrupt occured. Interrupt
 *	occurs at first eval after ^C has been typed.
 *
 * 19-APR-87	Dale Amon at CMU-CSD
 *	switched from rand to random package. Corrected bug in osrand(). It
 *	did not use the argument n to calculate a rand in range 0 to n-1 as
 *	advertised.
 * 28-OCT-05    Roger Dannenberg at CMU-SCS
 *      added directory listing functions
 * 9-JUL-2020   Roger Dannenberg
 *      added get-real-time lisp function
 */

#include "switches.h"
#include <errno.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>

#include <sys/types.h>
#include <dirent.h>

#include "xlisp.h"
#include "term.h"
#include "cext.h"
#include "userio.h"
#include "exitpa.h"
#include "nyq-osc-server.h"
#include "sliderdata.h" /* define sliders -- not just for OSC */
#include "sound.h" /* define nosc_enabled and mark_sound_time */
#include "falloc.h" /* define table_memory */
#define LBSIZE 200

/* external variables */
extern LVAL s_unbound,s_true;
extern FILE *tfp;

/* local variables */
static int lindex;
static int lcount = 0;
static int lposition;
static int line_edit = TRUE;

#ifndef READ_LINE
// typeahead is high for humans but allows NyquistIDE to 
// send envelope data and preferences
#define typeahead_max 1024
static char typeahead[typeahead_max];
static int typeahead_tail = 0;
static int typeahead_head = 0;
static char lbuf[LBSIZE];
static int lpos[LBSIZE];
#endif

#if USE_RAND
#if __APPLE__
#else
#include <time.h>
#endif
#endif

static int echo_enabled = 1;

/* forward declarations */
FORWARD LOCAL void xflush(void);
FORWARD LOCAL int xcheck(void);
FORWARD LOCAL void hidden_msg(void);

/*==========================================================================*/
/* control-c interrupt handling routines and variables. Uses B4.2 signal
   handling. Previous SIGINT handler is saved just in case someday we want
   to play with turning control c on and off.
*/

#include	<signal.h>

static int		ctc = FALSE;
static void control_c(int x) {ctc = TRUE;}
void ctcinit(void)           {signal ( SIGINT, control_c );}
static void ctcreset(void)   {signal ( SIGINT, control_c );}


/*==========================================================================*/

const char os_pathchar = '/';
const char os_sepchar = ':';


/* osinit - initialize */
void osinit(const char *banner)
{	
    printf("%s\n",banner);
    if (max_sample_block_len < 64) {
        printf("Warning: max_sample_block_len %d is small. Maybe you forgot"
               " to reset it after debugging.\n", max_sample_block_len);
    }
    /* start the random number generator. Older version was srand(1)
       seed of 1 makes the sequence repeatable. Random gives better
       pseudo randomness than does rand().
    */
#if USE_RAND
    // srand(1);
#if __APPLE__
    sranddev(); // initialize to a random seed
#else
    srand(time(NULL));
#endif
#endif

#if USE_RANDOM
#define USE_RANDOM something // see if compiler will tell us who
    // set this first
// USE_RANDOM is not supported, or at least the code should
// be inspected carefully if USE_RANDOM is set. Things to look
// for are: Is USE_RAND undefined to avoid conflicts? Should
// the seed be initialized (as in sranddev() above under USE_RAND)?
// Who uses random()? Nyquist uses random numbers in XLISP, in the
// noise() function, in STK's Noise class, and probably other places.
you must die here
    srandom(1);
#endif

#ifndef UNIX
    /* set control c trap to local routine */
    ctcinit();
#else
    /* sets terminal for raw input and calls ctcinit too */
    term_init();
    term_character();
#endif

    lposition = 0;
    lindex = 0;
    lcount = 0;
}

/* osfinish - clean up before returning to the operating system */
void osfinish(void) 
{
    term_exit();
    portaudio_exit();
}

/* oserror - print an error message */
void oserror(const char *msg) {printf("error: %s\n",msg);}


/* osaopen - open an ascii file */
FILE *osaopen(const char *name, const char *mode)
{
    FILE *fp = NULL;
    if (ok_to_open(name, mode))
        fp = fopen(name,mode);
#ifdef DEBUG_INPUT
    printf("osaopen on %s yields %x\n", name, fp);
    if (strcmp(name, "/home/rbd/nyquist/lib/xm-test.lsp") == 0) {
        // when DEBUG_INPUT is set, this generates a compiler error
        // on linux -RBD
        debug_input_fp = fp;
        printf("osaopen: debug_input_fp gets %x\n", debug_input_fp);
    }
#endif    
    return fp;
}

/* osbopen - open a binary file */
FILE *osbopen(name,mode) const char *name,*mode;
 {  char bmode[10];
    FILE *fp = NULL;
    strcpy(bmode,mode); strcat(bmode,"b");
    if (ok_to_open(name, bmode))
        fp = fopen(name,bmode);
    return fp;
 }

/* osclose - close a file */
int osclose(fp) FILE *fp;
{
#ifdef DEBUG_INPUT
    if (debug_input_fp == fp) {
        debug_input_fp = NULL;
        printf("osclose: debug_input_fp gets %x\n", debug_input_fp);
    }
#endif
    /* when XLISP is loading files and an error is encountered, the files
     * are automatically closed so that the OS will not lock them, confusing
     * the user. So we could get here and the file could already be closed
     */
    return (fp ? fclose(fp) : 0);
}

/* osagetc - get a character from an ascii file */
int osagetc(fp) FILE *fp; {
#ifdef DEBUG_INPUT
    int c = getc(fp);
    ungetc(c, fp);
#endif
    return (getc(fp));
}

/* osaputc - put a character to an ascii file */
int osaputc(int ch, FILE *fp) { return (putc(ch,fp)); }

/* osoutflush - flush output to a file */
void osoutflush(FILE *fp) { fflush(fp); }

extern int dbgflg;

/* osbgetc - get a character from a binary file */
/* int osbgetc(fp) FILE *fp; {return (getc(fp));} */
int osbgetc(FILE *fp) { 
    int c = (getc(fp));
    /*	if (dbgflg) printf("osbgetc: got %d from FILE %x\n", c, fp);
     */
    return c;
}

/* osbputc - put a character to a binary file */
int osbputc(ch,fp) int ch; FILE *fp; {return (putc(ch,fp));}

#ifdef OLDEST_OSTGETC
/* ostgetc - get a character from the terminal */
int ostgetc(void)
{
    int ch;
    switch (ch = term_getchar()) {
    case '\n':
        lbuf[lcount++] = '\n';
        lposition = 0;
        if (tfp)
            for (lindex = 0; lindex < lcount; ++lindex)
            osaputc(lbuf[lindex],tfp);
        lindex = 0; lcount = 0;
        return (ch);
    case '\010':
    case '\177':
        if (lcount) {
            lcount--;
            while (lposition > lpos[lcount]) {
            lposition--;
            }
        }
        break;
    case '\032':
        xflush();
        return (EOF);
    default:
        if (ch == '\t' || (ch >= 0x20 && ch < 0x7F)) {
            lbuf[lcount] = ch;
            lpos[lcount] = lposition;
            if (ch == '\t')
            do {} while (++lposition & 7);
            else {lposition++;}
            lcount++;
            return (ch);
        }
        else {
            xflush();
            switch (ch) {
            case '\003':	xltoplevel();	/* control-c */
            case '\007':	xlcleanup();	/* control-g */
            case '\020':	xlcontinue();	/* control-p */
            case '\032':	return (EOF);	/* control-z */

            /* moved from oscheck until I figure out how to
               set up interrupt to handle these two */
            case '\002':	xflush(); xlbreak("BREAK",s_unbound);
                    break;		/* control-b */
            case '\024':	xinfo(); break;	/* control-t */

            default:		return (ch);
            }
        }
    }
}
#else
#if OLD_OSTGETC
/* ostgetc - get a character from the terminal */
int ostgetc(void)
{   int ch;

    for (;;) {
    ch = term_getchar();
    oscheck();
    switch (ch) {
      case '\003':	xltoplevel();	/* control-c */
      case '\007':	xlcleanup();	/* control-g */
      case '\020':	xlcontinue();	/* control-p */
      case '\032':	return EOF;	/* control-z */
      case '\002':	xflush(); xlbreak("BREAK",s_unbound);
            break;		/* control-b */
      case '\024':	xinfo(); break;	/* control-t */
      case '\t':
      case '\n':
      default:
        if (tfp) osaputc(ch, tfp);
        return ch;
    }
    }
}
#else
#ifdef READLINE

#include <readline/readline.h>
#include <readline/history.h>

char *readline_line = NULL;
int readline_pos = 0;
int readline_len = 0;
int readline_first = 1;

extern int xldebug;

int ostgetc(void)
{
   int rval;

   if (readline_first)
      using_history();

   if (!readline_line) {
      char prompt[10];
      if (xldebug==0)
         sprintf(prompt, "> ");
      else
         sprintf(prompt, "%d> ", xldebug);
      readline_line = readline(prompt);
      if (readline_line == NULL)
         return EOF;
      add_history(readline_line);
      readline_len = strlen(readline_line);
      readline_pos = 0;
   }

   rval = readline_line[readline_pos];
   if (readline_pos == readline_len) {
      free(readline_line);
      readline_line = NULL;
      return '\n';
   }
   readline_pos++;
   
   return rval;
}


#else /* no readline */


void end_of_line_edit(void)
{
    line_edit = FALSE;
    if (tfp) {
    for (lindex = 0; lindex < lcount; ++lindex)
        osaputc(lbuf[lindex], tfp);
    }
    lindex = 0;
}

/* THIS IS THE "REAL" ostgetc(): */
LOCAL int rawtchar(void)
{
    int ch;
    if (typeahead_tail != typeahead_head) {
        ch = typeahead[typeahead_head++];
        typeahead_head &= (typeahead_max - 1);
        /* printf("[%c]", ch); */
        if (ch == 0xFF) ch = -1; /* char to int conversion of EOF */
    } else {
        fflush(stdout); /* necessary on OS X with Java IDE - I don't know why. */
        /* don't use getchar() or buffering will cause out-of-order input */
        ch = term_getchar();
        /* printf("{%c}", ch); */
    }
    return ch;
}

int ostgetc(void)
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
        ch = rawtchar();
        if (ch == EOF) xlisp_wrapup();
        oscheck(); /* in case user typed ^C */
        /* assume for now we should add the character */
        lbuf[lcount] = ch;
        lpos[lcount] = lposition;
        lcount++;
        lposition++;
        
        /* now do all the special character processing */
        switch (ch) {
        case '\001': /* take out non-printing character */
            lcount--;
            lposition--;
            mark_audio_time();
            break;
        case '\n':
            lposition = 0;
            end_of_line_edit();
            if (echo_enabled) {
		        osaputc('\r', stdout);
                osaputc(ch, stdout);
            }
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
		 		    if (echo_enabled) {
                        putchar('\010');
                        putchar(' ');
                        putchar('\010');
					}
			    	lposition--;
                }
           }
           break;
        case '\025': /* control-u */
            lcount--;
            lposition--;
            if (lcount) {
                while (lposition > lpos[0]) {
				    if (echo_enabled) {
                        putchar('\010');
                        putchar(' ');
                        putchar('\010');
					}
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
            lcount--; /* take out non-printing char */
            lposition--;
           xlcleanup();
           lcount = 0;
           break;
        case '\016':
            lcount--; /* take out non-printing char */
            lposition--;
            hidden_msg(); /* process hidden msg chars */
            break;
        case '\020':	/* control-p */
            lcount--; /* take out non-printing char */
            lposition--;
           xlcontinue();
           lcount = 0;
           break;
        case '\002':
            lcount--; /* take out non-printing char */
            lposition--;
           xflush();	/* control-b */
           xlbreak("BREAK",s_unbound);
           break;
        case '\005':	/* control-e */
            lcount--; /* take out non-printing char */
            lposition--;
	    echo_enabled = TRUE;
	    break;
        case '\006':    /* control-f */
            lcount--; /* take out non-printing char */
            lposition--;
	    echo_enabled = FALSE;
	    break;
        case '\024':	/* control-t */
            lcount--; /* take out non-printing char */
            lposition--;
           xinfo(); 
           lcount = 0;
           break;
		   
        case '\t':	/* TAB */
           lposition--; /* undo the increment above */
           do {
               lposition++;
               if (echo_enabled) osaputc(' ', stdout);
           } while (lposition & 7);
           break;
        default:
           if (echo_enabled) osaputc(ch, stdout);
           break;
        }
        // avoid line buffer overflow here:
        if (lposition > LBSIZE - 10) {
            // buffer is about to overflow, so write newline and
            // feed chars to XLISP
		    if (echo_enabled) {
                osaputc('\r', stdout);
                osaputc('\n', stdout);
		    }
            lposition = 0;
            end_of_line_edit();
        }
    }
    if (lindex + 1 >= lcount) {
       lcount = 0;
       line_edit = TRUE;
    }
    ch = lbuf[lindex++];
    /* printf("-%c-", ch); */
    if (echo_enabled) fflush(stdout);
    return ch;
}
#endif
#endif
#endif


/* ostputc - put a character to the terminal */
void ostputc(int ch)
{     
    oscheck();		/* check for control characters */

    /* output the character */
    if (ch == '\n') {lposition = 0;}
    else	    {lposition++;}

    /* output the character to the transcript file */
    if (tfp) osaputc(ch,tfp);
    putchar(((char) ch));
}
 
/* ostoutflush - flush output buffer */
void ostoutflush(void)
{
    if (tfp) fflush(tfp);
    fflush(stdout);
}

/* osflush - flush the terminal input buffer */
void osflush(void)
{
    lindex = lcount = lposition = 0; 
    line_edit = TRUE;
}


/* hidden_msg - process a "hidden message" 
 *
 * NOTE: a "hidden message" is a sequence of characters starting
 * with '\016' and ending with '\021'. These are designed to allow
 * a graphical interface, namely jNyqIDE, to control sliders in
 * real-time (during synthesis). The character sequences are hidden
 * meaning they are not echoed and they are not interpreted as LISP.
 *
 * This function assumes that '\016' has been received already.
 */
LOCAL void hidden_msg(void)
{
#define MSGBUF_MAX 64
    char msgbuf[MSGBUF_MAX];
    int msgbufx = 0;
    char type_char = rawtchar();
    char ch;
    // message is terminated by '\021'
    while ((ch = term_getchar()) != '\021' && ch != EOF &&
        msgbufx < MSGBUF_MAX - 1) {
        msgbuf[msgbufx++] = ch;
    }
    msgbuf[msgbufx++] = 0;
    // printf("hidden message: %s, len %ld\n", msgbuf, (long) strlen(msgbuf));
    if (msgbufx < MSGBUF_MAX) {
        if (type_char == 'S') { // slider change message
            int index;
            float value;
            if (sscanf(msgbuf, "%d %g", &index, &value) == 2) {
                set_slider(index, value);
            }
        }
    } /* other hidden messages could be parsed here */
}


/* oscheck - check for control characters during execution */
/*
 * NOTE: to support type-ahead, unused characters are put
 * into a queue to be removed by ostgetc
 */
void oscheck(void)
{
    int ch;
		
#if OSC
    if (nosc_enabled) nosc_poll();
#endif

    if (ctc) { /* control-c */
        /* printf("[oscheck: control-c detected]"); */
        ctc=FALSE; ctcreset();
        xflush(); xltoplevel(); return;
    } 

    if ((ch = xcheck())) {
        switch (ch) {
          case BREAK_CHAR:	/* control-b */
            /* printf("BREAK_CHAR\n"); */
            xflush(); xlbreak("BREAK",s_unbound); break; 
          case '\024':      /* control-t */
            /* printf("control-t\n"); */
            xinfo(); break;
          case '\025':      /* control-u */
            /* printf("control-u\n"); */
            xcleanup();
          case '\016': {    /* begin hidden message */
            /* printf("hidden msg\n"); */
            hidden_msg();
            break;
          }
          case '\001':  /* control-a -- mark audio time */
            mark_audio_time(); break;
          case -1: /* EOF - lost connection, so die */
            xlisp_wrapup();
            break;
          case -2: /* no character was ready */
            break;
          default:
            /* printf("Got %d\n", ch); */
#ifndef READ_LINE
            /* printf("+%c+", ch); */
            typeahead[typeahead_tail++] = ch;
            typeahead_tail &= (typeahead_max - 1);
            if (typeahead_tail == typeahead_head) {
                oserror("Input buffer overflow\n");
            }
#endif
            break;
        }
    }

    // when compute-bound, run_time is incremented by 10000 in about 15s, so
    // that's about 700 Hz. We want to flush any output at about 2Hz, so 
    // we'll pick 400 as a round number.
    // It's 2014, and now I'm seeing 3000 Hz. That's very high, so I 
    // changed SAMPLE to get this down to about 66Hz. Using % 30 to get
    // 2Hz flush rate.
    if (run_time % 30 == 0) {
        fflush(stdout);
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

/* xflush - flush the input line buffer and start a new line */
LOCAL void xflush(void)
{
   osflush();
   ostputc('\n');
}

/* xsystem - execute a system command */
LVAL xsystem(void)
{   /*LVAL strval;*/
    unsigned char *cmd = NULL;
    if (SAFE_NYQUIST) return NULL;
    if (moreargs())
        cmd = (unsigned char *)getstring(xlgastring());
    xllastarg();
    return (system((char *) cmd) == -1 ? cvfixnum((FIXTYPE)errno) : s_true);
}


/* xsetdir -- set current directory of the process */
LVAL xsetdir(void)
{
    char *dir = (char *)getstring(xlgastring());
    int result = -1;
    LVAL cwd = NULL;
    int verbose = TRUE;
    if (moreargs()) {
        verbose = (xlgetarg() != NIL);
    }
    xllastarg();
    if (ok_to_open(dir, "r"))
        result = chdir(dir);
    if (result) {
        /* perror("SETDIR"); -- Nyquist uses SETDIR to search for directories
         * at startup, so failures are normal, and seeing error messages
         * could be confusing, so don't print them. The NULL return indicates
         * an error, but doesn't tell which one it is.
         * But now, SETDIR has a second verbose parameter that is nil when
         * searching for directories. -RBD
         */
        if (verbose) perror("Directory Setting Error");
        return NULL;
    }
    dir = getcwd(NULL, 1000);
    if (dir) {
        cwd = cvstring(dir);
        free(dir);
    }
    return cwd;
}

/* xget_temp_path -- get a path to create temp files */
LVAL xget_temp_path(void)
{
    return cvstring("/tmp/");
}

/* xget_user -- get a string identifying the user, for use in file names */
LVAL xget_user(void)
{
    const char *user = getenv("USER");
    if (!user || !*user) {
        errputstr("Warning: could not get user ID, using 'nyquist'\n");
        user = "nyquist";
    }
    return cvstring(user);
}


/* xechoenabled -- set/clear echo_enabled flag (unix only) */
LVAL xechoenabled(void)
{
    int flag = (xlgetarg() != NULL);
    xllastarg();
    echo_enabled = flag;
    return NULL;
}


#define OSDIR_LIST_READY 0
#define OSDIR_LIST_STARTED 1
#define OSDIR_LIST_DONE 2
static int osdir_list_status = OSDIR_LIST_READY;
static DIR *osdir_dir;

/* osdir_list_start -- open a directory listing */
int osdir_list_start(const char *path)
{    
    if (osdir_list_status != OSDIR_LIST_READY) {
        osdir_list_finish(); /* close current listing */
    }
    osdir_dir = NULL;
    if (ok_to_open(path, "r"))
        osdir_dir = opendir(path);
    if (!osdir_dir) {
        return FALSE;
    }
    osdir_list_status = OSDIR_LIST_STARTED;
    return TRUE;
}


/* osdir_list_next -- read the next entry from a directory */
const char *osdir_list_next(void)
{
    if (osdir_list_status != OSDIR_LIST_STARTED) {
        return NULL;
    }
    struct dirent *entry = readdir(osdir_dir);
    if (!entry) {
        osdir_list_status = OSDIR_LIST_DONE;
        return NULL;
    } else {
        return entry->d_name;
    }
}


/* osdir_list_finish -- close an open directory */
void osdir_list_finish(void)
{
    if (osdir_list_status != OSDIR_LIST_READY) {
        closedir(osdir_dir);
    }
    osdir_list_status = OSDIR_LIST_READY;
}


/* xcheck -- return a character if one is present */
LOCAL int xcheck(void)
{
    int ch = term_testchar();
    return ch;
}

/* xgetkey - get a key from the keyboard */
LVAL xgetkey(void) {xllastarg(); return (cvfixnum((FIXTYPE)term_getchar()));}

/* ossymbols - enter os specific symbols */
void ossymbols(void) {}

/* xsetupconsole -- used to configure window in Win32 version */
LVAL xsetupconsole(void) { return NIL; }


/* xgetrealtime - get current time in seconds */
LVAL xgetrealtime(void)
{
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    return cvflonum((double) te.tv_sec + (te.tv_usec * 1e-6));
}
