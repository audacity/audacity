/* this was written to test the term.c code, which was
   found on the web for use in XLisp to enable ^C and other
   character-by-character handling 
 */

#include "term.h"
#include <signal.h>

void ctcinit()
{
    signal(SIGINT, term_exit);
}


main()
{
    int c;
    int count = 0;
    term_init();
    term_character();
    while ((c = term_testchar()) == -2) count++;
    printf("got %c after %d\n", c, count);
    while ((c = getchar()) != 'x') {
        printf("got '%c' = %x\n", c, c);
    }
    term_exit();
}

