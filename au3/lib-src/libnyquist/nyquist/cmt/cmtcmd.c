/* cmtcmd.c -- routines for the moxc side of the command interface */

#include "switches.h"
#include "stdio.h"
#ifdef  AMIGA
#include "exec/types.h"
#include "exec/exec.h"
#endif
#include "cext.h"
#include "midifns.h"
#include "timebase.h"
#include "moxc.h"
#include "seq.h"
#include "cmtcmd.h"
#include "userio.h"
#include "string.h"

#define HASHELEM(p) ((p).symbol_name)
#define HASHVAL 50
#define HASHENTRIES 50
#define HASHENTER hash_lookup
#define HASHNOCOPY

#include "hashrout.h"

void defvar(char *name, int *addr)

{
    intptr_t i = hash_lookup(name);
    HASHENTRY(i).symb_type = var_symb_type;
    HASHENTRY(i).ptr.intptr = addr;
}


void defun(char *name, seq_cmd_fn addr)
{
    intptr_t i = hash_lookup(name);
    HASHENTRY(i).symb_type = fn_symb_type;
    HASHENTRY(i).ptr.routine = addr;
}


void defvec(char *name, int *addr, int size)
{
    intptr_t i = hash_lookup(name);
    HASHENTRY(i).symb_type = vec_symb_type;
    HASHENTRY(i).size = size;
    HASHENTRY(i).ptr.intptr = addr;
}


