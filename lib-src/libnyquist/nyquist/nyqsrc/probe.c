/* probe.c -- used to test resampling */

#include "stdio.h"
#include "string.h"
#include "xlisp.h"
#include "sound.h"

static FILE* probefile = NULL;
static long line_num = 0;

void probe_init(int readflag)
{
    line_num = 0;
    probefile = fopen("probe.log", (readflag ? "r" : "w"));
}


double probe(char *s, double x)
{
    fprintf(probefile, "%s %g\n", s, x);
    return x;
}


double probe2(char *s, double x)
{
    char buf1[100], buf2[100];
    sprintf(buf1, "%s %g\n", s, x);
    fgets(buf2, 100, probefile);
    line_num++;
    if (strcmp(buf1, buf2)) {
        nyquist_printf("probe2: difference at line %ld: \n", line_num);
        nyquist_printf("correct: %s", buf2);
        nyquist_printf("actual:  %s", buf1);
        abort();
    }
    return x;
}

