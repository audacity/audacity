/* cmtcmd.c -- routines for the moxc side of the command interface */

#include "switches.h"
#include "stdio.h"
#ifdef  AMIGA
#include "exec/types.h"
#include "exec/exec.h"
#endif
#include "cmtcmd.h"
#include "cext.h"
#include "userio.h"
#include "string.h"

#define HASHELEM(p) ((p).symbol_name)
#define HASHVAL 50
#define HASHENTRIES 50
#define HASHENTER lookup
#define HASHNOCOPY

#include "hashrout.h"

void defvar(name, addr)
  char *name;
  int *addr;
{
    int i = lookup(name);
    HASHENTRY(i).symb_type = var_symb_type;
    HASHENTRY(i).ptr.intptr = addr;
}


void defun(name, addr)
  char *name;
  int (*addr)();
{
    int i = lookup(name);
    HASHENTRY(i).symb_type = fn_symb_type;
    HASHENTRY(i).ptr.routine = addr;
}


void defvec(name, addr, size)
  char *name;
  int *addr;
  int size;
{
    int i = lookup(name);
    HASHENTRY(i).symb_type = vec_symb_type;
    HASHENTRY(i).size = size;
    HASHENTRY(i).ptr.intptr = addr;
}


