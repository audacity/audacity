/* term.c -- Routines for managing terminal I/O settings by Alan Cox.
 * From LJ 17 */

/* Thanks to Dave Cook for rescuing it */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  include ioctl.h and declare void ctcinit();
 */


#include <termios.h>
#ifndef __APPLE__
#include <asm/ioctls.h>
#endif
#include <sys/ioctl.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

void ctcinit(void);

/* This will be used for new terminal settings. */
static struct termios current;

/* This will hold the initial state so that we can restor it later. */
static struct termios initial;

/* Restor the termianl settings to those saved when term_init was called. */
void term_restore(void)
{
    tcsetattr(0, TCSANOW, &initial);
}

/* Clean up termianl; called on exit. */
void term_exit(void)
{
    term_restore();
}

/* Will be called when contrl-Z is pressed;
 * this correctly handles the terminal. */
void term_ctrlz(int i)
{
    signal(SIGTSTP, term_ctrlz);
    term_restore();
    kill(getpid(), SIGSTOP);
}

/* Will be called when the application is continued
 * after having been stopped. */
void term_cont(int i)
{
    signal(SIGCONT, term_cont);
    tcsetattr(0, TCSANOW, &current);
}

/* callback for SIGQUIT is different type from atexit callback */
void term_quit(int i)
{
    term_exit();
}

/* Needs to be called to initialize the terminal. */
void term_init(void)
{
    /* If stdin isn't a terminal this fails.
       But then so does tcsetattr(), so it doesn't matter. */
    tcgetattr(0, &initial);
    /* Save a copy to work with later. */
    current = initial;
    /* We _must_ clean up when we exit. */
    /* signal(SIGINT, term_exit); */
    ctcinit(); /* XLisp wants to catch ctrl C */
    signal(SIGQUIT, term_quit);
    /* Control-Z must also be handled. */
    signal(SIGTSTP, term_ctrlz);
    signal(SIGCONT, term_cont);
    atexit(term_exit);
}

/* Set character-by-character input mode. */
void term_character(void)
{
    /* One or more characters are sufficient to cause a read return. */
    current.c_cc[VMIN] = 1;
    /* No timeout; read waits forever until ready. */
    current.c_cc[VTIME] = 0;
    /* Line-by-line mode off */
    current.c_lflag &= ~ICANON;
#ifndef READ_LINE
    current.c_lflag &= ~ECHO;
#endif
    tcsetattr(0, TCSANOW, &current);
}

/* Return to line-by-line input mode. */
void term_line(void)
{
    current.c_lflag |= ICANON;
    tcsetattr(0, TCSANOW, &current);
}


#define ERROR(s)        return (perror(s), -1)

/* term_testchar -- tell whether character is ready or not,
 *
 * if ready, return it, otherwise return -2
 */
int term_testchar(void)
{
    int n;
    char c;

    if (ioctl(0, FIONREAD, &n) < 0)
        ERROR("IOgetchar");
    if (n == 0) return -2;
    switch(read(0, &c, 1)) {
      case 1:
        return c;
      case 0:
        return EOF;
      default:
        ERROR("IOgetchar-read");
    }
}


/* term_getchar -- get a character (block if necessary) */
/**/
int term_getchar(void)
{
    char c;
    int rslt = read(0, &c, 1);
    return (rslt == 1 ? c : EOF);
}

