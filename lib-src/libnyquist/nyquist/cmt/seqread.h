/* seqread.h -- header file for users of seqread.c */
/* Copyright 1989 Carnegie Mellon University */

#define name_length 255
void seq_read(seq_type seq, FILE *fp); /* LISP: (SEQ-READ SEQ FILE) */
time_type scale(ulong, ulong, ulong);    /*now public for smf_write, smf_read*/

