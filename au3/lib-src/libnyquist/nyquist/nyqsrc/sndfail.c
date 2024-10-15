
#include "stdio.h"
#include "snd.h"
#include "stdlib.h"
#include "xlisp.h"
#include "string.h"

void snd_fail(char *msg)
{
    char *bigger = (char *) malloc(strlen(msg) + 16);
    if (!bigger) xlfail("no memory");
    strcpy(bigger, "(snd)");
    strcat(bigger, msg);
    xlfail(bigger);
    // NOTE: there is a memory leak here
}


void snd_warn(char *msg)
{
    stdputstr(msg);
    stdputstr("\n");
}
