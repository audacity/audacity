/* Copyright 1989 Carnegie Mellon University */

/*****************************************************************************
*       Change Log
*  Date | Change
*-----------+-----------------------------------------------------------------
* 5-Apr |JDW : Further changes
*****************************************************************************/

/* classes of output for gprintf */
#ifdef MACINTOSH
#undef false
#undef true
#include <MacTypes.h>
#define TRANS    (long)    0
#define ERROR    (long)    1
#define FATAL    (long)    2
#define GDEBUG   (long)    3
#endif

#ifdef  DONT_USE_CMT_IO
#define TRANS   stdout
#define ERROR   stdout
#define FATAL   stdout
#define GDEBUG   stdout
#endif

#ifndef TRANS   /* default */
#define TRANS   0
#define ERROR   1
#define FATAL   2
#define GDEBUG   3
#endif

#define CR '\n'
#define ABORT_CHAR 0x03
#ifdef NYQUIST
#define BREAK_CHAR 0x02
#else
#define BREAK_CHAR 0x07
#endif

#define BREAK_LEVEL 1
#define ABORT_LEVEL 2

#define read_to_eol(ch) if (ch != CR) { char temp[100]; ggets(temp); }

extern char fileopen_name[];
extern int abort_flag;
extern int redirect_flag;		/* added by Ning Hu, Apr 2001 */

boolean get_ascii(char *c);    /* polls for an ascii character */
#ifdef DOTS_FOR_ARGS
/* was (defined(ITC_MACH) && defined(__STDC__)) || defined(MACINTOSH) || defined(AZTEC) || (defined(AMIGA) && defined(LATTICE)) || defined(UNIX_ITC) */
void    gprintf(long where, const char *format, ...); /* general printf */
#else
void gprintf();
#endif

char    *ggets(char *str);       /* general gets */
int     wait_ascii(void);   /* a waiting version of get_ascii */
void    clean_exit(void);   /* exit the program after cleaning up */
void    io_init(void);      /* overall initialization */
void    abort_check(void);  /* exit if aborted */

int check_aborted(void);        /* looks to see if user typed ctrl-C */

int     askbool(char *prompt, int deflt);
FILE    *fileopen(char *deflt, char *extension, char *mode, char *prompt);
void    readln(FILE *fp);
void gflush(void);
int gputchar(int c);
int ggetchar(void);
char *ggets(char *str);
boolean ascii_input(char *c);
void unget_ascii(char c);
boolean check_ascii(void);

#ifdef MACINTOSH
boolean get_file_info(char *filename, OSType *file_type, OSType *file_creator);
boolean put_file_info(char *filename, OSType file_type, OSType file_creator);
#endif

#ifdef DONT_USE_CMT_IO
#define ggetchar        getchar
#define ggets           gets
#define gprintf         fprintf
#define gputchar        putchar
#define gprintf fprintf
#define gputchar putchar
#endif

#ifdef MICROSOFT
void c_break(int sig);
#endif

