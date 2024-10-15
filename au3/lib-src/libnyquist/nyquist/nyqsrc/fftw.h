/* -*- C -*- */
/*
 * Copyright (c) 1997-1999 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/* fftw.h -- system-wide definitions */
/* $Id: fftw.h,v 1.1.1.1 2004/11/10 16:07:38 rbd Exp $ */

#ifndef FFTW_H
#define FFTW_H

#include <stdlib.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif				/* __cplusplus */

/* Define for using single precision */
/*
 * If you can, use configure --enable-float instead of changing this
 * flag directly 
 */
/* #undef FFTW_ENABLE_FLOAT */

/* our real numbers */
#ifdef FFTW_ENABLE_FLOAT
typedef float fftw_real;
#else
typedef double fftw_real;
#endif

/*********************************************
 * Complex numbers and operations 
 *********************************************/
typedef struct {
     fftw_real re, im;
} fftw_complex;

#define c_re(c)  ((c).re)
#define c_im(c)  ((c).im)

typedef enum {
     FFTW_FORWARD = -1, FFTW_BACKWARD = 1
} fftw_direction;

/* backward compatibility with FFTW-1.3 */
typedef fftw_complex FFTW_COMPLEX;
typedef fftw_real FFTW_REAL;

#ifndef FFTW_1_0_COMPATIBILITY
#define FFTW_1_0_COMPATIBILITY 0
#endif

#if FFTW_1_0_COMPATIBILITY
/* backward compatibility with FFTW-1.0 */
#define REAL fftw_real
#define COMPLEX fftw_complex
#endif

/*********************************************
 * Success or failure status
 *********************************************/

typedef enum {
     FFTW_SUCCESS = 0, FFTW_FAILURE = -1
} fftw_status;

/*********************************************
 *              Codelets
 *********************************************/
typedef void (fftw_notw_codelet) 
     (const fftw_complex *, fftw_complex *, int, int);
typedef void (fftw_twiddle_codelet)
     (fftw_complex *, const fftw_complex *, int,
      int, int);
typedef void (fftw_generic_codelet) 
     (fftw_complex *, const fftw_complex *, int,
      int, int, int);
typedef void (fftw_real2hc_codelet)
     (const fftw_real *, fftw_real *, fftw_real *,
      int, int, int);
typedef void (fftw_hc2real_codelet)
     (const fftw_real *, const fftw_real *,
      fftw_real *, int, int, int);
typedef void (fftw_hc2hc_codelet)
     (fftw_real *, const fftw_complex *,
      int, int, int);
typedef void (fftw_rgeneric_codelet)
     (fftw_real *, const fftw_complex *, int,
      int, int, int);

/*********************************************
 *     Configurations
 *********************************************/
/*
 * A configuration is a database of all known codelets
 */

enum fftw_node_type {
     FFTW_NOTW, FFTW_TWIDDLE, FFTW_GENERIC, FFTW_RADER,
     FFTW_REAL2HC, FFTW_HC2REAL, FFTW_HC2HC, FFTW_RGENERIC
};

/* description of a codelet */
typedef struct {
     const char *name;		/* name of the codelet */
     void (*codelet) ();	/* pointer to the codelet itself */
     int size;			/* size of the codelet */
     fftw_direction dir;	/* direction */
     enum fftw_node_type type;	/* TWIDDLE or NO_TWIDDLE */
     int signature;		/* unique id */
     int ntwiddle;		/* number of twiddle factors */
     const int *twiddle_order;	/* 
                                 * array that determines the order 
                                 * in which the codelet expects
                                 * the twiddle factors
                                 */
} fftw_codelet_desc;

/* On Win32, you need to do funny things to access global variables
   in shared libraries.  Thanks to Andrew Sterian for this hack. */
#if defined(__WIN32__) || defined(WIN32) || defined(_WINDOWS)
#  if defined(BUILD_FFTW_DLL)
#    define DL_IMPORT(type) __declspec(dllexport) type
#  elif defined(USE_FFTW_DLL)
#    define DL_IMPORT(type) __declspec(dllimport) type
#  else
#    define DL_IMPORT(type) type
#  endif
#else
#  define DL_IMPORT(type) type
#endif

extern DL_IMPORT(const char *) fftw_version;

/*****************************
 *        Plans
 *****************************/
/*
 * A plan is a sequence of reductions to compute a FFT of
 * a given size.  At each step, the FFT algorithm can:
 *
 * 1) apply a notw codelet, or
 * 2) recurse and apply a twiddle codelet, or
 * 3) apply the generic codelet.
 */

/* structure that contains twiddle factors */
typedef struct fftw_twiddle_struct {
     int n;
     const fftw_codelet_desc *cdesc;
     fftw_complex *twarray;
     struct fftw_twiddle_struct *next;
     int refcnt;
} fftw_twiddle;

typedef struct fftw_rader_data_struct {
     struct fftw_plan_struct *plan;
     fftw_complex *omega;
     int g, ginv;
     int p, flags, refcount;
     struct fftw_rader_data_struct *next;
     fftw_codelet_desc *cdesc;
} fftw_rader_data;

typedef void (fftw_rader_codelet) 
     (fftw_complex *, const fftw_complex *, int,
      int, int, fftw_rader_data *);

/* structure that holds all the data needed for a given step */
typedef struct fftw_plan_node_struct {
     enum fftw_node_type type;

     union {
          /* nodes of type FFTW_NOTW */
          struct {
               int size;
               fftw_notw_codelet *codelet;
               const fftw_codelet_desc *codelet_desc;
          } notw;

          /* nodes of type FFTW_TWIDDLE */
          struct {
               int size;
               fftw_twiddle_codelet *codelet;
               fftw_twiddle *tw;
               struct fftw_plan_node_struct *recurse;
               const fftw_codelet_desc *codelet_desc;
          } twiddle;

          /* nodes of type FFTW_GENERIC */
          struct {
               int size;
               fftw_generic_codelet *codelet;
               fftw_twiddle *tw;
               struct fftw_plan_node_struct *recurse;
          } generic;

          /* nodes of type FFTW_RADER */
          struct {
               int size;
               fftw_rader_codelet *codelet;
               fftw_rader_data *rader_data;
               fftw_twiddle *tw;
               struct fftw_plan_node_struct *recurse;
          } rader;

          /* nodes of type FFTW_REAL2HC */
          struct {
               int size;
               fftw_real2hc_codelet *codelet;
               const fftw_codelet_desc *codelet_desc;
          } real2hc;

          /* nodes of type FFTW_HC2REAL */
          struct {
               int size;
               fftw_hc2real_codelet *codelet;
               const fftw_codelet_desc *codelet_desc;
          } hc2real;

          /* nodes of type FFTW_HC2HC */
          struct {
               int size;
               fftw_direction dir;
               fftw_hc2hc_codelet *codelet;
               fftw_twiddle *tw;
               struct fftw_plan_node_struct *recurse;
               const fftw_codelet_desc *codelet_desc;
          } hc2hc;

          /* nodes of type FFTW_RGENERIC */
          struct {
               int size;
               fftw_direction dir;
               fftw_rgeneric_codelet *codelet;
               fftw_twiddle *tw;
               struct fftw_plan_node_struct *recurse;
          } rgeneric;
     } nodeu;

     int refcnt;
} fftw_plan_node;

struct fftw_plan_struct {
     int n;
     int refcnt;
     fftw_direction dir;
     int flags;
     int wisdom_signature;
     enum fftw_node_type wisdom_type;
     struct fftw_plan_struct *next;
     fftw_plan_node *root;
     double cost;
};

/* a plan is just an array of instructions */
typedef struct fftw_plan_struct *fftw_plan;

/* flags for the planner */
#define  FFTW_ESTIMATE (0)
#define  FFTW_MEASURE  (1)

#define FFTW_OUT_OF_PLACE (0)
#define FFTW_IN_PLACE (8)
#define FFTW_USE_WISDOM (16)

#define FFTW_THREADSAFE (128)  /* guarantee plan is read-only so that the
                                  same plan can be used in parallel by
                                  multiple threads */

#define FFTWND_FORCE_BUFFERED (256)	/* internal, undocumented flag */

extern fftw_plan fftw_create_plan_specific(int n, fftw_direction dir,
                                           int flags,
                                           fftw_complex *in, int istride,
                                         fftw_complex *out, int ostride);
#define FFTW_HAS_PLAN_SPECIFIC
extern fftw_plan fftw_create_plan(int n, fftw_direction dir, int flags);
extern void fftw_print_plan(fftw_plan plan);
extern void fftw_destroy_plan(fftw_plan plan);
extern void fftw(fftw_plan plan, int howmany, fftw_complex *in, int istride,
                 int idist, fftw_complex *out, int ostride, int odist);
extern void fftw_one(fftw_plan plan, fftw_complex *in, fftw_complex *out);
extern void fftw_die(const char *s);
extern void *fftw_malloc(size_t n);
extern void fftw_free(void *p);
extern void fftw_check_memory_leaks(void);
extern void fftw_print_max_memory_usage(void);

typedef void *(*fftw_malloc_type_function) (size_t n);
typedef void  (*fftw_free_type_function) (void *p);
typedef void  (*fftw_die_type_function) (const char *errString);
extern DL_IMPORT(fftw_malloc_type_function) fftw_malloc_hook;
extern DL_IMPORT(fftw_free_type_function) fftw_free_hook;
extern DL_IMPORT(fftw_die_type_function) fftw_die_hook;

extern size_t fftw_sizeof_fftw_real(void);

/* Wisdom: */
/*
 * define this symbol so that users know we are using a version of FFTW
 * with wisdom
 */
#define FFTW_HAS_WISDOM
extern void fftw_forget_wisdom(void);
extern void fftw_export_wisdom(void (*emitter) (char c, void *), void *data);
extern fftw_status fftw_import_wisdom(int (*g) (void *), void *data);
extern void fftw_export_wisdom_to_file(FILE *output_file);
extern fftw_status fftw_import_wisdom_from_file(FILE *input_file);
extern char *fftw_export_wisdom_to_string(void);
extern fftw_status fftw_import_wisdom_from_string(const char *input_string);

/*
 * define symbol so we know this function is available (it is not in
 * older FFTWs)
 */
#define FFTW_HAS_FPRINT_PLAN
extern void fftw_fprint_plan(FILE *f, fftw_plan plan);

/*****************************
 *    N-dimensional code
 *****************************/
typedef struct {
     int is_in_place;		/* 1 if for in-place FFTs, 0 otherwise */

     int rank;			/* 
                                 * the rank (number of dimensions) of the
                                 * array to be FFTed 
                                 */
     int *n;			/*
                                 * the dimensions of the array to the
                                 * FFTed 
                                 */
     fftw_direction dir;

     int *n_before;		/*
                                 * n_before[i] = product of n[j] for j < i 
                                 */
     int *n_after;		/* n_after[i] = product of n[j] for j > i */

     fftw_plan *plans;		/* 1d fftw plans for each dimension */

     int nbuffers, nwork;
     fftw_complex *work;	/* 
                                 * work array big enough to hold
                                 * nbuffers+1 of the largest dimension 
                                 * (has nwork elements)
                                 */
} fftwnd_data;

typedef fftwnd_data *fftwnd_plan;

/* Initializing the FFTWND plan: */
extern fftwnd_plan fftw2d_create_plan(int nx, int ny, fftw_direction dir,
                                      int flags);
extern fftwnd_plan fftw3d_create_plan(int nx, int ny, int nz,
                                      fftw_direction dir, int flags);
extern fftwnd_plan fftwnd_create_plan(int rank, const int *n,
                                      fftw_direction dir,
                                      int flags);

extern fftwnd_plan fftw2d_create_plan_specific(int nx, int ny,
                                               fftw_direction dir,
                                               int flags,
                                           fftw_complex *in, int istride,
                                         fftw_complex *out, int ostride);
extern fftwnd_plan fftw3d_create_plan_specific(int nx, int ny, int nz,
                                           fftw_direction dir, int flags,
                                           fftw_complex *in, int istride,
                                         fftw_complex *out, int ostride);
extern fftwnd_plan fftwnd_create_plan_specific(int rank, const int *n,
                                               fftw_direction dir,
                                               int flags,
                                           fftw_complex *in, int istride,
                                         fftw_complex *out, int ostride);

/* Freeing the FFTWND plan: */
extern void fftwnd_destroy_plan(fftwnd_plan plan);

/* Printing the plan: */
extern void fftwnd_fprint_plan(FILE *f, fftwnd_plan p);
extern void fftwnd_print_plan(fftwnd_plan p);
#define FFTWND_HAS_PRINT_PLAN

/* Computing the N-Dimensional FFT */
extern void fftwnd(fftwnd_plan plan, int howmany,
                   fftw_complex *in, int istride, int idist,
                   fftw_complex *out, int ostride, int odist);
extern void fftwnd_one(fftwnd_plan p, fftw_complex *in, fftw_complex *out);

#ifdef __cplusplus
}                               /* extern "C" */

#endif				/* __cplusplus */
#endif				/* FFTW_H */
