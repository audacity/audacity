// textio.cpp -- handles text input and output to edit control


/*
Overview of character input:
    ostgetc is called to get characters from stdin for XLisp
    ostgetc gets characters using ggetchar() and performs line editing
    ggetchar gets characters using wait_ascii()
    wait_ascii() get characters from typein:queue; it calls
        process_win_events if there are no characters, and it returns
        ABORT_CHAR or BREAK_CHAR if abort_flag is set
    characters get into typein::queue when the key handler is called

    get_ascii is similar to wait_ascii, but it doesn't wait: it just
        checks for input with process_win_events and returns a character
        if there is one, otherwise, it returns false
*/

#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <windows.h>
#include "cppext.h"
#include "longque.h"
#include "textio.h"
#include "typein.h"

#include "button.h"
#include "slider.h"
#include "winmain.h"
#include "assert.h"
extern "C" {
#include "xlisp.h"
}

#define GPRINTF_MESSAGE_LEN 500


//istvanmerge
//int abort_flag = 0;

longque typein::queue;


void typein::init()
{
    queue.init(100);
}


void typein::finish()
{
    queue.finish();
}



void typein::handler(char *inp)
{
    if (!queue.fullp()) {
        if (*inp == ABORT_CHAR) {
            abort_flag = ABORT_LEVEL;
            free(inp);
        } else if (!abort_flag && *inp == BREAK_CHAR) {
            abort_flag = BREAK_LEVEL;
            free(inp);
        } else if (!abort_flag && *inp == INFO_CHAR) {
            xinfo();
            free(inp);
        } else queue.insert((long) inp);
    }
}    


extern char *next_ascii;

extern "C"
int get_ascii(char *c)
{
    check_aborted(); /* input buffer check */
    if (!next_ascii && typein::queue.emptyp()) return false;
    *c = wait_ascii();
    return true; // recurse with new string
}


/* check_aborted -- see if any characters are available, check for ctrl C */
extern "C"
int check_aborted()
{
    process_win_events(USE_PEEK);
    /* handle input messages, if ^C, handler will set abort flag */
    return abort_flag;
}    



/* define with va_alist and use vsprintf to get temp */
extern "C"
void gprintf(long where, char *format, ...)
{
    char temp[GPRINTF_MESSAGE_LEN];
    va_list pvar;
    va_start(pvar, format);
    _vsnprintf(temp, GPRINTF_MESSAGE_LEN, format, pvar);
    va_end(pvar);

    switch((long) where) {
      case GTRANS:
        break;
      case GERROR:
                break;
      case GFATAL:
                edit_append("FATAL: ");
                edit_append(temp);
                break;
      case GDEBUG:
                edit_append("DEBUG: ");
                edit_append(temp);
                break;
      default:
                edit_append("UNKNOWN: ");
                edit_append(temp);
                break;
    }
        edit_append(temp);              
}

#if defined(nyquist_printf)
#error "nyquist_printf should not be defined yet"
#endif

extern "C"
void nyquist_printf(const char *format, ...)
{
    char temp[GPRINTF_MESSAGE_LEN];
    va_list pvar;
    va_start(pvar, format);
    _vsnprintf(temp, GPRINTF_MESSAGE_LEN, format, pvar);
    va_end(pvar);
    edit_append(temp);
}

/**************************************************************************
*                               gputchar
* General putchar
**************************************************************************/
extern "C"
void gputchar(int c)
{
    char tmp[4];
    tmp[0] = c;
    tmp[1] = 0;
    edit_append(tmp);
}


/**************************************************************************
*                               ggetchar
* General getchar
**************************************************************************/

extern "C"
int ggetchar()
{
    return wait_ascii();
}

/**************************************************************************
*                                  ggets
* General gets
**************************************************************************/

extern "C"
char *ggets(char *str)
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
                gputchar((int)0x07);
            }
        } else *s++ = (char) c;
    } while (c != (int) '\n' && !abort_flag);

        *(s-1) = EOS;
        if (abort_flag) *str = EOS;
        return str;
}


/****************************************************************************
*               askbool
* Inputs:
*    char *prompt: string to prompt for user input
*    int deflt: true or false default
* Returns:
*    boolean: true or false as entered by user
* Effect:
*    prompts user for yes or no input, returns result
****************************************************************************/
extern "C"
int askbool(char *prompt, int deflt)
{
#define undefined -1
    char defchar;    /* the default answer */
    char c;     /* user input */
    char in_string[100];
    int result = -1;    /* the result: -1 = undefined, 0 = false, 1 = true */
    if (deflt) defchar = 'y';
    else defchar = 'n';
    while (result == undefined) {
        gprintf(GTRANS, "%s? [%c]: ", prompt, defchar);
        ggets(in_string);
        c = in_string[0];
        if (islower(c)) c = toupper(c);
        if (c == 'Y') result = true;
        else if (c == 'N') result = false;
        else if (c == EOS) result = deflt;
        else if (abort_flag) result = deflt;
        /* space before Please to separate from user's type-in: */
        else gprintf(GTRANS, " Please type Y or N.\n");
    }
    if (abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
        result = deflt;
        gprintf(GTRANS, "\n");
    }
    return result;
}


extern "C"
void io_init()
{
}




