/******************************************************************
*               modified JCD 27 Apr-88 for AMIGA
*      cext.h -- extensions to c to make it more portable
* Copyright 1989 Carnegie Mellon University
*
*******************************************************************

cext must provide the following definitions:

true     -- a  constant
false     -- a boolean constant
private -- defined as static, used to declare local functions
public     -- defined as empty string, used to declare exported functions
boolean -- a new type
byte     -- unsigned 8-bit quantity
ushort     -- unsigned 16-bit quantity
ulong     -- unsigned 32-bit quantity
Pointer -- pointer to char, a generic pointer
ABS()     -- absolute value of any type of number
MAX()     -- maximum of two numbers
MIN()     -- minimum of two numbers
ROUND32() -- round a double to int
ROUNDBIG() -- round a double to intptr_t

NULL     -- pointer to nothing, a constant
EOS      -- end of string, a constant '\0'
MALLOC(x)     -- allocates x bytes
FREE(x)     -- frees something from MALLOC
AVAILMEM     -- tells how much memory is available.
                   (N.B.: no parens, no args.)
EXIT(n)  -- calls exit(n) after shutting down/deallocating resources

*****************************************************************************/

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  many changes for new conditional compilation switches
 * 28Apr03  rbd removed macro redefinitions: min, max
 */

#ifndef CEXT_H
#ifndef SWITCHES
#include "switches.h"
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <inttypes.h>

#if HAS_STDLIB_H
#include <stdlib.h>
#endif

#if HAS_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAS_MALLOC_H
#include <malloc.h>
#endif

#if NEED_ULONG
typedef unsigned long ulong;
#endif

#if NEED_USHORT
typedef unsigned long ushort;
#endif

#if NEED_BYTE
typedef unsigned char byte;
#endif

/* There's a name conflict between true/false as an enum type in
 * Apple #includes:Types.h on the Mac, and true/false as #defined below
 */
#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define private static
#define public

#if NEED_DEFINE_MALLOC
public void *malloc();
#endif

typedef char *Pointer;

#ifdef UNIX_MACH
typedef int boolean;
#else
/* hopefully, unsigned short will save sign extension instructions */
typedef unsigned char boolean;
#endif

#ifndef ABS
#define ABS(a) (((a) > 0) ? (a) : -(a))
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#define MAXULONG 0xffffffff

#ifndef NULL
#define NULL 0L
#endif

#ifndef EOS
#define EOS '\0'
#endif

#define SAFETYBUF    10    /* Safety buffer when allocating memory */
#define BIGGEST_BLOCK    32765    /* Should find a happy medium for this  */

#ifdef MACINTOSH /*DMH: gets AVAILMEM in record.c*/
#include <stddef.h>
#define MALLOC(x)       malloc((size_t)(x))  /*DMH: size_t is ulong, for MAC*/
#define FREE(x)    free((char *)(x))
#define AVAILMEM    MyMaxMem(NULL)/*???*/
#endif

#ifdef LATTICE322
#define MALLOC  malloc
#define FREE    free
#define AVAILMEM    MyMaxMem(NULL)

#else

#ifdef DOS /* was MICROSOFT */
#define MALLOC  malloc
#define FREE    free
#define AVAILMEM MyMaxMem(NULL)
#endif
#endif

#ifdef UNIX
#define MALLOC  malloc
#define FREE     free
#define AVAILMEM     10000000 /* since we have virtual memory, assume 10Mb */
#endif

#ifdef AMIGA
#define MALLOC  malloc
#define FREE    free
#define AVAILMEM     128000
#endif

public ulong MyMaxMem(ushort *);

#ifndef MEM
#include "mem.h"
#endif

#ifndef CLEANUP
#include "cleanup.h"
#endif

#ifdef CMTSTUFF
#define EXIT cmt_exit
public void EXIT(int);
/* don't allow anyone to call exit directly */
#define exit(n) PLEASE_CALL_EXIT_NOT_exit
#else
#define EXIT(n) exit(n)
#endif

#define _cext

#ifndef MALLOC
MALLOC is not defined!
#endif

#define ROUND32(x) ((int) ((x) + 0.5))
#define ROUNDBIG(x) ((int64_t) ((x) + 0.5))
/* With the addition of these 2 functions, we never
   "NEED_ROUND" and trying to use round will cause an error.
  */
#ifdef NEED_ROUND
// #define round ROUND
#define round you should not use round
#endif

#ifndef min
#define min MIN
#define max MAX
#endif

#define CEXT_H
#endif
