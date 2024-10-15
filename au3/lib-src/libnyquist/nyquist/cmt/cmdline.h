/* Copyright 1989 Carnegie Mellon University */

char *cl_arg(int n);
boolean cl_init(char *av[], int ac);
long cl_int_option(char *name, long deflt);
char *cl_option(char *name);
boolean cl_switch(char *name);
boolean cl_syntax(char *name);
