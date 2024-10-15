/*
 * stats.c
 *
 * produce statistics.
 */
#include "switches.h"
#include <stdio.h>
#ifndef mips
#include "stdlib.h"
#endif
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"

void stats()
{
    nyquist_printf("\n\nNyquist statistics:\n\n");
    nyquist_printf("Memory usage:\n");
    nyquist_printf("\tconsumed %d pools of size %d\n", npools, MAXPOOLSIZE);
    nyquist_printf("\tdata structure usage:\n");
    nyquist_printf("\t\tsounds\t%d\n", sound_used);
    nyquist_printf("\t\tsnd lists\t%d\n", snd_list_used);
    nyquist_printf("\t\tsample blocks\t%d\n", sample_block_used);
    nyquist_printf("\t\ttable space in bytes\t%ld\n", table_memory);
    nyquist_printf("\n");
}
