/*
   =================================================================================================
   Definitions and prototypes for LAPACK as provided Apple Computer.

   Documentation of the LAPACK interfaces, including reference implementations, can be found on the web
   starting from the LAPACK FAQ page at this URL (verified live as of April 2002):
        http://netlib.org/lapack/faq.html
        
   A hardcopy maanual is:
        LAPACK Users' Guide, Third Edition. 
        @BOOK{laug,
            AUTHOR = {Anderson, E. and Bai, Z. and Bischof, C. and
                        Blackford, S. and Demmel, J. and Dongarra, J. and
                        Du Croz, J. and Greenbaum, A. and Hammarling, S. and
                        McKenney, A. and Sorensen, D.},
            TITLE = {{LAPACK} Users' Guide},
            EDITION = {Third},
            PUBLISHER = {Society for Industrial and Applied Mathematics},
            YEAR = {1999},
            ADDRESS = {Philadelphia, PA},
            ISBN = {0-89871-447-8 (paperback)} }

   =================================================================================================
*/
#ifndef __CLAPACK_H
#define __CLAPACK_H
 
#ifdef __cplusplus
extern "C" {
#endif


#if defined(__LP64__) /* In LP64 match sizes with the 32 bit ABI */
typedef int 		__CLPK_integer;
typedef int 		__CLPK_logical;
typedef float 		__CLPK_real;
typedef double 		__CLPK_doublereal;
typedef 		__CLPK_logical 	(*__CLPK_L_fp)();
typedef int 		__CLPK_ftnlen;
#else
typedef long int 	__CLPK_integer;
typedef long int 	__CLPK_logical;
typedef float 		__CLPK_real;
typedef double 		__CLPK_doublereal;
typedef __CLPK_logical 	(*__CLPK_L_fp)();
typedef long int 	__CLPK_ftnlen;
#endif

typedef struct { __CLPK_real r, i; } __CLPK_complex;
typedef struct { __CLPK_doublereal r, i; } __CLPK_doublecomplex;

/* Subroutine */ int cbdsqr_(char *uplo, __CLPK_integer *n, __CLPK_integer *ncvt, __CLPK_integer *
	nru, __CLPK_integer *ncc, __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *vt, __CLPK_integer *ldvt, 
	__CLPK_complex *u, __CLPK_integer *ldu, __CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cgbbrd_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *ncc,
	 __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_complex *q, __CLPK_integer *ldq, __CLPK_complex *pt, __CLPK_integer *ldpt, 
	__CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgbcon_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *rcond, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgbequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real 
	*colcnd, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int cgbrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *afb, __CLPK_integer *
	ldafb, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *
	ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cgbsv_(__CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_integer *
	nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *
	ldb, __CLPK_integer *info);
 
/* Subroutine */ int cgbsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *kl,
	 __CLPK_integer *ku, __CLPK_integer *nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *afb,
	 __CLPK_integer *ldafb, __CLPK_integer *ipiv, char *equed, __CLPK_real *r__, __CLPK_real *c__, 
	__CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real 
	*ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgbtf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int cgbtrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int cgbtrs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_complex 
	*b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cgebak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_real *scale, __CLPK_integer *m, __CLPK_complex *v, __CLPK_integer *ldv, 
	__CLPK_integer *info);
 
/* Subroutine */ int cgebal_(char *job, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *scale, __CLPK_integer *info);
 
/* Subroutine */ int cgebd2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *tauq, __CLPK_complex *taup, __CLPK_complex *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int cgebrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *tauq, __CLPK_complex *taup, __CLPK_complex *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgecon_(char *norm, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgeequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real *colcnd, __CLPK_real *amax, 
	__CLPK_integer *info);
 
/* Subroutine */ int cgees_(char *jobvs, char *sort, __CLPK_L_fp select, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *sdim, __CLPK_complex *w, __CLPK_complex *vs, 
	__CLPK_integer *ldvs, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_logical *
	bwork, __CLPK_integer *info);
 
/* Subroutine */ int cgeesx_(char *jobvs, char *sort, __CLPK_L_fp select, char *
	sense, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *sdim, __CLPK_complex *
	w, __CLPK_complex *vs, __CLPK_integer *ldvs, __CLPK_real *rconde, __CLPK_real *rcondv, __CLPK_complex *
	work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int cgeev_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *w, __CLPK_complex *vl, __CLPK_integer *ldvl, __CLPK_complex *vr, 
	__CLPK_integer *ldvr, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *w, __CLPK_complex *vl, 
	__CLPK_integer *ldvl, __CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_integer *ilo, __CLPK_integer *ihi,
	 __CLPK_real *scale, __CLPK_real *abnrm, __CLPK_real *rconde, __CLPK_real *rcondv, __CLPK_complex *work, 
	__CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgegs_(char *jobvsl, char *jobvsr, __CLPK_integer *n, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *alpha, __CLPK_complex *
	beta, __CLPK_complex *vsl, __CLPK_integer *ldvsl, __CLPK_complex *vsr, __CLPK_integer *ldvsr, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgegv_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *alpha, __CLPK_complex *beta,
	 __CLPK_complex *vl, __CLPK_integer *ldvl, __CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_complex *
	work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgehd2_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cgehrd_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer 
	*info);
 
/* Subroutine */ int cgelq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cgelqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgels_(char *trans, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgelsx_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *jpvt, __CLPK_real *rcond,
	 __CLPK_integer *rank, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgelsy_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *jpvt, __CLPK_real *rcond,
	 __CLPK_integer *rank, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cgeql2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cgeqlf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgeqp3_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *jpvt, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgeqpf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *jpvt, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cgeqr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cgeqrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgerfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, __CLPK_complex *
	b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgerq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cgerqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgesc2_(__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *
	rhs, __CLPK_integer *ipiv, __CLPK_integer *jpiv, __CLPK_real *scale);
 
/* Subroutine */ int cgesv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cgesvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_integer *
	ipiv, char *equed, __CLPK_real *r__, __CLPK_real *c__, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgetc2_(__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *
	ipiv, __CLPK_integer *jpiv, __CLPK_integer *info);
 
/* Subroutine */ int cgetf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int cgetrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int cgetri_(__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *
	ipiv, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgetrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int cggbak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_real *lscale, __CLPK_real *rscale, __CLPK_integer *m, __CLPK_complex *v, 
	__CLPK_integer *ldv, __CLPK_integer *info);
 
/* Subroutine */ int cggbal_(char *job, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, 
	__CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *lscale, 
	__CLPK_real *rscale, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int cgges_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	selctg, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *
	ldb, __CLPK_integer *sdim, __CLPK_complex *alpha, __CLPK_complex *beta, __CLPK_complex *vsl, 
	__CLPK_integer *ldvsl, __CLPK_complex *vsr, __CLPK_integer *ldvsr, __CLPK_complex *work, __CLPK_integer *
	lwork, __CLPK_real *rwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int cggesx_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	selctg, char *sense, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b,
	 __CLPK_integer *ldb, __CLPK_integer *sdim, __CLPK_complex *alpha, __CLPK_complex *beta, __CLPK_complex *
	vsl, __CLPK_integer *ldvsl, __CLPK_complex *vsr, __CLPK_integer *ldvsr, __CLPK_real *rconde, __CLPK_real 
	*rcondv, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int cggev_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *alpha, __CLPK_complex *beta,
	 __CLPK_complex *vl, __CLPK_integer *ldvl, __CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_complex *
	work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb,
	 __CLPK_complex *alpha, __CLPK_complex *beta, __CLPK_complex *vl, __CLPK_integer *ldvl, __CLPK_complex *
	vr, __CLPK_integer *ldvr, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *lscale, __CLPK_real *
	rscale, __CLPK_real *abnrm, __CLPK_real *bbnrm, __CLPK_real *rconde, __CLPK_real *rcondv, __CLPK_complex 
	*work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *iwork, __CLPK_logical *bwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cggglm_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *d__, __CLPK_complex *x, 
	__CLPK_complex *y, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cgghrd_(char *compq, char *compz, __CLPK_integer *n, __CLPK_integer *
	ilo, __CLPK_integer *ihi, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb,
	 __CLPK_complex *q, __CLPK_integer *ldq, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *info);
 
/* Subroutine */ int cgglse_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *p, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *c__, __CLPK_complex *d__, 
	__CLPK_complex *x, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cggqrf_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *taua, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *taub, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cggrqf_(__CLPK_integer *m, __CLPK_integer *p, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *taua, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *taub, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cggsvd_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *p, __CLPK_integer *k, __CLPK_integer *l, __CLPK_complex *a, __CLPK_integer *
	lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *alpha, __CLPK_real *beta, __CLPK_complex *u, 
	__CLPK_integer *ldu, __CLPK_complex *v, __CLPK_integer *ldv, __CLPK_complex *q, __CLPK_integer *ldq, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int cggsvp_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer 
	*ldb, __CLPK_real *tola, __CLPK_real *tolb, __CLPK_integer *k, __CLPK_integer *l, __CLPK_complex *u, 
	__CLPK_integer *ldu, __CLPK_complex *v, __CLPK_integer *ldv, __CLPK_complex *q, __CLPK_integer *ldq, 
	__CLPK_integer *iwork, __CLPK_real *rwork, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *
	info);
 
/* Subroutine */ int cgtcon_(char *norm, __CLPK_integer *n, __CLPK_complex *dl, __CLPK_complex *
	d__, __CLPK_complex *du, __CLPK_complex *du2, __CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *
	rcond, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cgtrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	dl, __CLPK_complex *d__, __CLPK_complex *du, __CLPK_complex *dlf, __CLPK_complex *df, __CLPK_complex *
	duf, __CLPK_complex *du2, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *
	x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cgtsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *dl, __CLPK_complex *
	d__, __CLPK_complex *du, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cgtsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *dl, __CLPK_complex *d__, __CLPK_complex *du, __CLPK_complex *dlf, __CLPK_complex *
	df, __CLPK_complex *duf, __CLPK_complex *du2, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *
	ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cgttrf_(__CLPK_integer *n, __CLPK_complex *dl, __CLPK_complex *d__, __CLPK_complex *
	du, __CLPK_complex *du2, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int cgttrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	dl, __CLPK_complex *d__, __CLPK_complex *du, __CLPK_complex *du2, __CLPK_integer *ipiv, __CLPK_complex *
	b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cgtts2_(__CLPK_integer *itrans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_complex *dl, __CLPK_complex *d__, __CLPK_complex *du, __CLPK_complex *du2, __CLPK_integer *ipiv, 
	__CLPK_complex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int chbev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int chbevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *lrwork, __CLPK_integer *
	iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int chbevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *q, __CLPK_integer *ldq, 
	__CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *
	m, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int chbgst_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *bb, __CLPK_integer *ldbb, 
	__CLPK_complex *x, __CLPK_integer *ldx, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int chbgv_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *bb, __CLPK_integer *ldbb, 
	__CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int chbgvx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *ka, __CLPK_integer *kb, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *bb, 
	__CLPK_integer *ldbb, __CLPK_complex *q, __CLPK_integer *ldq, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *
	il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_complex *z__, 
	__CLPK_integer *ldz, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *iwork, __CLPK_integer *
	ifail, __CLPK_integer *info);
 
/* Subroutine */ int chbtrd_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *q, __CLPK_integer *
	ldq, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int checon_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_integer *
	info);
 
/* Subroutine */ int cheev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *w, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cheevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *w, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, 
	__CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int cheevr_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *
	iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_integer *isuppz, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *
	lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int cheevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *
	iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *iwork, __CLPK_integer *
	ifail, __CLPK_integer *info);
 
/* Subroutine */ int chegs2_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int chegst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int chegv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *w, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int chegvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *w, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *lrwork, __CLPK_integer *
	iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int chegvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *
	m, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_integer *lwork,
	 __CLPK_real *rwork, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int cherfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, __CLPK_complex *
	b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int chesv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *a,
	 __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *work,
	 __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int chesvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_integer *
	ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond,
	 __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int chetf2_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int chetrd_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int chetrf_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int chetri_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int chetrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int chgeqz_(char *job, char *compq, char *compz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, 
	__CLPK_integer *ldb, __CLPK_complex *alpha, __CLPK_complex *beta, __CLPK_complex *q, __CLPK_integer *ldq,
	 __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int chpcon_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	ipiv, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int chpev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_complex *ap, 
	__CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int chpevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_complex *ap, 
	__CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_integer *lwork, 
	__CLPK_real *rwork, __CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int chpevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_complex *ap, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *
	abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *
	work, __CLPK_real *rwork, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int chpgst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, __CLPK_complex *
	ap, __CLPK_complex *bp, __CLPK_integer *info);
 
/* Subroutine */ int chpgv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_complex *ap, __CLPK_complex *bp, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int chpgvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_complex *ap, __CLPK_complex *bp, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *lrwork, __CLPK_integer *
	iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int chpgvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_complex *bp, __CLPK_real *vl, __CLPK_real *vu, 
	__CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_complex *
	z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *iwork, 
	__CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int chprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_complex *afp, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x,
	 __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int chpsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int chpsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *ap, __CLPK_complex *afp, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *
	ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int chptrd_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_complex *tau, __CLPK_integer *info);
 
/* Subroutine */ int chptrf_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	ipiv, __CLPK_integer *info);
 
/* Subroutine */ int chptri_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	ipiv, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int chptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int chsein_(char *side, char *eigsrc, char *initv, __CLPK_logical *
	select, __CLPK_integer *n, __CLPK_complex *h__, __CLPK_integer *ldh, __CLPK_complex *w, __CLPK_complex *
	vl, __CLPK_integer *ldvl, __CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *
	m, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *ifaill, __CLPK_integer *ifailr, 
	__CLPK_integer *info);
 
/* Subroutine */ int chseqr_(char *job, char *compz, __CLPK_integer *n, __CLPK_integer *ilo,
	 __CLPK_integer *ihi, __CLPK_complex *h__, __CLPK_integer *ldh, __CLPK_complex *w, __CLPK_complex *z__, 
	__CLPK_integer *ldz, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int clabrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *tauq, __CLPK_complex *taup, 
	__CLPK_complex *x, __CLPK_integer *ldx, __CLPK_complex *y, __CLPK_integer *ldy);
 
/* Subroutine */ int clacgv_(__CLPK_integer *n, __CLPK_complex *x, __CLPK_integer *incx);
 
/* Subroutine */ int clacon_(__CLPK_integer *n, __CLPK_complex *v, __CLPK_complex *x, __CLPK_real *est, 
	__CLPK_integer *kase);
 
/* Subroutine */ int clacp2_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int clacpy_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int clacrm_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *b, __CLPK_integer *ldb, __CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_real *rwork);
 
/* Subroutine */ int clacrt_(__CLPK_integer *n, __CLPK_complex *cx, __CLPK_integer *incx, __CLPK_complex *
	cy, __CLPK_integer *incy, __CLPK_complex *c__, __CLPK_complex *s);
 
/* Subroutine */ int claed0_(__CLPK_integer *qsiz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_complex *q, __CLPK_integer *ldq, __CLPK_complex *qstore, __CLPK_integer *ldqs, __CLPK_real *rwork,
	 __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int claed7_(__CLPK_integer *n, __CLPK_integer *cutpnt, __CLPK_integer *qsiz, 
	__CLPK_integer *tlvls, __CLPK_integer *curlvl, __CLPK_integer *curpbm, __CLPK_real *d__, __CLPK_complex *
	q, __CLPK_integer *ldq, __CLPK_real *rho, __CLPK_integer *indxq, __CLPK_real *qstore, __CLPK_integer *
	qptr, __CLPK_integer *prmptr, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *
	givcol, __CLPK_real *givnum, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int claed8_(__CLPK_integer *k, __CLPK_integer *n, __CLPK_integer *qsiz, __CLPK_complex *
	q, __CLPK_integer *ldq, __CLPK_real *d__, __CLPK_real *rho, __CLPK_integer *cutpnt, __CLPK_real *z__, 
	__CLPK_real *dlamda, __CLPK_complex *q2, __CLPK_integer *ldq2, __CLPK_real *w, __CLPK_integer *indxp, 
	__CLPK_integer *indx, __CLPK_integer *indxq, __CLPK_integer *perm, __CLPK_integer *givptr, 
	__CLPK_integer *givcol, __CLPK_real *givnum, __CLPK_integer *info);
 
/* Subroutine */ int claein_(__CLPK_logical *rightv, __CLPK_logical *noinit, __CLPK_integer *n, 
	__CLPK_complex *h__, __CLPK_integer *ldh, __CLPK_complex *w, __CLPK_complex *v, __CLPK_complex *b, 
	__CLPK_integer *ldb, __CLPK_real *rwork, __CLPK_real *eps3, __CLPK_real *smlnum, __CLPK_integer *info);
 
/* Subroutine */ int claesy_(__CLPK_complex *a, __CLPK_complex *b, __CLPK_complex *c__, __CLPK_complex *
	rt1, __CLPK_complex *rt2, __CLPK_complex *evscal, __CLPK_complex *cs1, __CLPK_complex *sn1);
 
/* Subroutine */ int claev2_(__CLPK_complex *a, __CLPK_complex *b, __CLPK_complex *c__, __CLPK_real *rt1, 
	__CLPK_real *rt2, __CLPK_real *cs1, __CLPK_complex *sn1);
 
/* Subroutine */ int clags2_(__CLPK_logical *upper, __CLPK_real *a1, __CLPK_complex *a2, __CLPK_real *a3, 
	__CLPK_real *b1, __CLPK_complex *b2, __CLPK_real *b3, __CLPK_real *csu, __CLPK_complex *snu, __CLPK_real *csv, 
	__CLPK_complex *snv, __CLPK_real *csq, __CLPK_complex *snq);
 
/* Subroutine */ int clagtm_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *
	alpha, __CLPK_complex *dl, __CLPK_complex *d__, __CLPK_complex *du, __CLPK_complex *x, __CLPK_integer *
	ldx, __CLPK_real *beta, __CLPK_complex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int clahef_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_integer *kb,
	 __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *w, __CLPK_integer *ldw, 
	__CLPK_integer *info);
 
/* Subroutine */ int clahqr_(__CLPK_logical *wantt, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_complex *h__, __CLPK_integer *ldh, __CLPK_complex *w, 
	__CLPK_integer *iloz, __CLPK_integer *ihiz, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *
	info);
 
/* Subroutine */ int clahrd_(__CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *nb, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *t, __CLPK_integer *ldt, __CLPK_complex *y, 
	__CLPK_integer *ldy);
 
/* Subroutine */ int claic1_(__CLPK_integer *job, __CLPK_integer *j, __CLPK_complex *x, __CLPK_real *sest,
	 __CLPK_complex *w, __CLPK_complex *gamma, __CLPK_real *sestpr, __CLPK_complex *s, __CLPK_complex *c__);
 
/* Subroutine */ int clals0_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_integer *nrhs, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *bx, 
	__CLPK_integer *ldbx, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *givcol, 
	__CLPK_integer *ldgcol, __CLPK_real *givnum, __CLPK_integer *ldgnum, __CLPK_real *poles, __CLPK_real *
	difl, __CLPK_real *difr, __CLPK_real *z__, __CLPK_integer *k, __CLPK_real *c__, __CLPK_real *s, __CLPK_real *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int clalsa_(__CLPK_integer *icompq, __CLPK_integer *smlsiz, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *bx, __CLPK_integer *ldbx, 
	__CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, __CLPK_integer *k, __CLPK_real *difl, __CLPK_real *difr, 
	__CLPK_real *z__, __CLPK_real *poles, __CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_integer *
	ldgcol, __CLPK_integer *perm, __CLPK_real *givnum, __CLPK_real *c__, __CLPK_real *s, __CLPK_real *rwork, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int clapll_(__CLPK_integer *n, __CLPK_complex *x, __CLPK_integer *incx, __CLPK_complex *
	y, __CLPK_integer *incy, __CLPK_real *ssmin);
 
/* Subroutine */ int clapmt_(__CLPK_logical *forwrd, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex 
	*x, __CLPK_integer *ldx, __CLPK_integer *k);
 
/* Subroutine */ int claqgb_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real 
	*colcnd, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int claqge_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real *colcnd, __CLPK_real *amax, char *
	equed);
 
/* Subroutine */ int claqhb_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int claqhe_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int claqhp_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_real *s, 
	__CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int claqp2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, __CLPK_complex 
	*a, __CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_complex *tau, __CLPK_real *vn1, __CLPK_real *vn2, 
	__CLPK_complex *work);
 
/* Subroutine */ int claqps_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, __CLPK_integer 
	*nb, __CLPK_integer *kb, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_complex *
	tau, __CLPK_real *vn1, __CLPK_real *vn2, __CLPK_complex *auxv, __CLPK_complex *f, __CLPK_integer *ldf);
 
/* Subroutine */ int claqsb_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int claqsp_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_real *s, 
	__CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int claqsy_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int clar1v_(__CLPK_integer *n, __CLPK_integer *b1, __CLPK_integer *bn, __CLPK_real *
	sigma, __CLPK_real *d__, __CLPK_real *l, __CLPK_real *ld, __CLPK_real *lld, __CLPK_real *gersch, __CLPK_complex 
	*z__, __CLPK_real *ztz, __CLPK_real *mingma, __CLPK_integer *r__, __CLPK_integer *isuppz, __CLPK_real *
	work);
 
/* Subroutine */ int clar2v_(__CLPK_integer *n, __CLPK_complex *x, __CLPK_complex *y, __CLPK_complex *z__,
	 __CLPK_integer *incx, __CLPK_real *c__, __CLPK_complex *s, __CLPK_integer *incc);
 
/* Subroutine */ int clarcm_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_real *rwork);
 
/* Subroutine */ int clarf_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *v, 
	__CLPK_integer *incv, __CLPK_complex *tau, __CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *
	work);
 
/* Subroutine */ int clarfb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *v, __CLPK_integer *ldv, 
	__CLPK_complex *t, __CLPK_integer *ldt, __CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work, 
	__CLPK_integer *ldwork);
 
/* Subroutine */ int clarfg_(__CLPK_integer *n, __CLPK_complex *alpha, __CLPK_complex *x, __CLPK_integer *
	incx, __CLPK_complex *tau);
 
/* Subroutine */ int clarft_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_complex *v, __CLPK_integer *ldv, __CLPK_complex *tau, __CLPK_complex *t, __CLPK_integer *ldt);
 
/* Subroutine */ int clarfx_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *v, 
	__CLPK_complex *tau, __CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work);
 
/* Subroutine */ int clargv_(__CLPK_integer *n, __CLPK_complex *x, __CLPK_integer *incx, __CLPK_complex *
	y, __CLPK_integer *incy, __CLPK_real *c__, __CLPK_integer *incc);
 
/* Subroutine */ int clarnv_(__CLPK_integer *idist, __CLPK_integer *iseed, __CLPK_integer *n, 
	__CLPK_complex *x);
 
/* Subroutine */ int clarrv_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *l, __CLPK_integer *isplit, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_integer *iblock, __CLPK_real *gersch, __CLPK_real *tol, 
	__CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_real *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int clartg_(__CLPK_complex *f, __CLPK_complex *g, __CLPK_real *cs, __CLPK_complex *sn, 
	__CLPK_complex *r__);
 
/* Subroutine */ int clartv_(__CLPK_integer *n, __CLPK_complex *x, __CLPK_integer *incx, __CLPK_complex *
	y, __CLPK_integer *incy, __CLPK_real *c__, __CLPK_complex *s, __CLPK_integer *incc);
 
/* Subroutine */ int clarz_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, 
	__CLPK_complex *v, __CLPK_integer *incv, __CLPK_complex *tau, __CLPK_complex *c__, __CLPK_integer *ldc, 
	__CLPK_complex *work);
 
/* Subroutine */ int clarzb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_complex *v, 
	__CLPK_integer *ldv, __CLPK_complex *t, __CLPK_integer *ldt, __CLPK_complex *c__, __CLPK_integer *ldc, 
	__CLPK_complex *work, __CLPK_integer *ldwork);
 
/* Subroutine */ int clarzt_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_complex *v, __CLPK_integer *ldv, __CLPK_complex *tau, __CLPK_complex *t, __CLPK_integer *ldt);
 
/* Subroutine */ int clascl_(char *type__, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_real *
	cfrom, __CLPK_real *cto, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int claset_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *
	alpha, __CLPK_complex *beta, __CLPK_complex *a, __CLPK_integer *lda);
 
/* Subroutine */ int clasr_(char *side, char *pivot, char *direct, __CLPK_integer *m,
	 __CLPK_integer *n, __CLPK_real *c__, __CLPK_real *s, __CLPK_complex *a, __CLPK_integer *lda);
 
/* Subroutine */ int classq_(__CLPK_integer *n, __CLPK_complex *x, __CLPK_integer *incx, __CLPK_real *
	scale, __CLPK_real *sumsq);
 
/* Subroutine */ int claswp_(__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *
	k1, __CLPK_integer *k2, __CLPK_integer *ipiv, __CLPK_integer *incx);
 
/* Subroutine */ int clasyf_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_integer *kb,
	 __CLPK_complex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *w, __CLPK_integer *ldw, 
	__CLPK_integer *info);
 
/* Subroutine */ int clatbs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *
	x, __CLPK_real *scale, __CLPK_real *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int clatdf_(__CLPK_integer *ijob, __CLPK_integer *n, __CLPK_complex *z__, __CLPK_integer 
	*ldz, __CLPK_complex *rhs, __CLPK_real *rdsum, __CLPK_real *rdscal, __CLPK_integer *ipiv, __CLPK_integer 
	*jpiv);
 
/* Subroutine */ int clatps_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_complex *x, __CLPK_real *scale, __CLPK_real *cnorm,
	 __CLPK_integer *info);
 
/* Subroutine */ int clatrd_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *e, __CLPK_complex *tau, __CLPK_complex *w, __CLPK_integer *ldw);
 
/* Subroutine */ int clatrs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *x, __CLPK_real *scale,
	 __CLPK_real *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int clatrz_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work);
 
/* Subroutine */ int clatzm_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *v, 
	__CLPK_integer *incv, __CLPK_complex *tau, __CLPK_complex *c1, __CLPK_complex *c2, __CLPK_integer *ldc, 
	__CLPK_complex *work);
 
/* Subroutine */ int clauu2_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *info);
 
/* Subroutine */ int clauum_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *info);
 
/* Subroutine */ int cpbcon_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cpbequ_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int cpbrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *afb, __CLPK_integer *ldafb, 
	__CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *
	berr, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cpbstf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int cpbsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int cpbsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_integer *nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *afb, __CLPK_integer *
	ldafb, char *equed, __CLPK_real *s, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, 
	__CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, 
	__CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cpbtf2_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int cpbtrf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_complex *ab,
	 __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int cpbtrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int cpocon_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cpoequ_(__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_real *s, 
	__CLPK_real *scond, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int cporfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_complex *b, __CLPK_integer *ldb,
	 __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, 
	__CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cposv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *a,
	 __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cposvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, char *
	equed, __CLPK_real *s, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, 
	__CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cpotf2_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *info);
 
/* Subroutine */ int cpotrf_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *info);
 
/* Subroutine */ int cpotri_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *info);
 
/* Subroutine */ int cpotrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cppcon_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_real *anorm,
	 __CLPK_real *rcond, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cppequ_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_real *s, 
	__CLPK_real *scond, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int cpprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_complex *afp, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, 
	__CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cppsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cppsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *ap, __CLPK_complex *afp, char *equed, __CLPK_real *s, __CLPK_complex *b, 
	__CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real 
	*berr, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cpptrf_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	info);
 
/* Subroutine */ int cpptri_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	info);
 
/* Subroutine */ int cpptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cptcon_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_complex *e, __CLPK_real *anorm, 
	__CLPK_real *rcond, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cptrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__,
	 __CLPK_complex *e, __CLPK_real *df, __CLPK_complex *ef, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex 
	*x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cptsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__, __CLPK_complex *e, 
	__CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cptsvx_(char *fact, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__,
	 __CLPK_complex *e, __CLPK_real *df, __CLPK_complex *ef, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex 
	*x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, 
	__CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int cpttrf_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_complex *e, __CLPK_integer *info);
 
/* Subroutine */ int cpttrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__,
	 __CLPK_complex *e, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cptts2_(__CLPK_integer *iuplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *
	d__, __CLPK_complex *e, __CLPK_complex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int crot_(__CLPK_integer *n, __CLPK_complex *cx, __CLPK_integer *incx, __CLPK_complex *
	cy, __CLPK_integer *incy, __CLPK_real *c__, __CLPK_complex *s);
 
/* Subroutine */ int cspcon_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	ipiv, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cspmv_(char *uplo, __CLPK_integer *n, __CLPK_complex *alpha, __CLPK_complex *
	ap, __CLPK_complex *x, __CLPK_integer *incx, __CLPK_complex *beta, __CLPK_complex *y, __CLPK_integer *
	incy);
 
/* Subroutine */ int cspr_(char *uplo, __CLPK_integer *n, __CLPK_complex *alpha, __CLPK_complex *x,
	 __CLPK_integer *incx, __CLPK_complex *ap);
 
/* Subroutine */ int csprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_complex *afp, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x,
	 __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int cspsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int cspsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *ap, __CLPK_complex *afp, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *
	ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int csptrf_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	ipiv, __CLPK_integer *info);
 
/* Subroutine */ int csptri_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_integer *
	ipiv, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int csptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	ap, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int csrot_(__CLPK_integer *n, __CLPK_complex *cx, __CLPK_integer *incx, __CLPK_complex *
	cy, __CLPK_integer *incy, __CLPK_real *c__, __CLPK_real *s);
 
/* Subroutine */ int csrscl_(__CLPK_integer *n, __CLPK_real *sa, __CLPK_complex *sx, __CLPK_integer *incx);
 
/* Subroutine */ int cstedc_(char *compz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *
	rwork, __CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cstein_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_integer *m, __CLPK_real 
	*w, __CLPK_integer *iblock, __CLPK_integer *isplit, __CLPK_complex *z__, __CLPK_integer *ldz, 
	__CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int csteqr_(char *compz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int csycon_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_integer *
	info);
 
/* Subroutine */ int csymv_(char *uplo, __CLPK_integer *n, __CLPK_complex *alpha, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *x, __CLPK_integer *incx, __CLPK_complex *beta, __CLPK_complex *y,
	 __CLPK_integer *incy);
 
/* Subroutine */ int csyr_(char *uplo, __CLPK_integer *n, __CLPK_complex *alpha, __CLPK_complex *x,
	 __CLPK_integer *incx, __CLPK_complex *a, __CLPK_integer *lda);
 
/* Subroutine */ int csyrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, __CLPK_complex *
	b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int csysv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *a,
	 __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *work,
	 __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int csysvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *af, __CLPK_integer *ldaf, __CLPK_integer *
	ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *rcond,
	 __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int csytf2_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int csytrf_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int csytri_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_integer *ipiv, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int csytrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int ctbcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *rcond, __CLPK_complex *work, 
	__CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctbrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *b, 
	__CLPK_integer *ldb, __CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, 
	__CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctbtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ctgevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_complex *vl, __CLPK_integer *ldvl, __CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, 
	__CLPK_integer *m, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctgex2_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *q, 
	__CLPK_integer *ldq, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *j1, __CLPK_integer *info);
 
/* Subroutine */ int ctgexc_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *q, 
	__CLPK_integer *ldq, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *ifst, __CLPK_integer *
	ilst, __CLPK_integer *info);
 
/* Subroutine */ int ctgsen_(__CLPK_integer *ijob, __CLPK_logical *wantq, __CLPK_logical *wantz, 
	__CLPK_logical *select, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, 
	__CLPK_integer *ldb, __CLPK_complex *alpha, __CLPK_complex *beta, __CLPK_complex *q, __CLPK_integer *ldq,
	 __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *m, __CLPK_real *pl, __CLPK_real *pr, __CLPK_real *
	dif, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctgsja_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_complex *a, __CLPK_integer *
	lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *tola, __CLPK_real *tolb, __CLPK_real *alpha, 
	__CLPK_real *beta, __CLPK_complex *u, __CLPK_integer *ldu, __CLPK_complex *v, __CLPK_integer *ldv, 
	__CLPK_complex *q, __CLPK_integer *ldq, __CLPK_complex *work, __CLPK_integer *ncycle, __CLPK_integer *
	info);
 
/* Subroutine */ int ctgsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_complex *vl, __CLPK_integer *ldvl, __CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_real *s, __CLPK_real 
	*dif, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer 
	*iwork, __CLPK_integer *info);
 
/* Subroutine */ int ctgsy2_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *d__, __CLPK_integer *ldd, __CLPK_complex *e, __CLPK_integer *lde, 
	__CLPK_complex *f, __CLPK_integer *ldf, __CLPK_real *scale, __CLPK_real *rdsum, __CLPK_real *rdscal, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctgsyl_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *d__, __CLPK_integer *ldd, __CLPK_complex *e, __CLPK_integer *lde, 
	__CLPK_complex *f, __CLPK_integer *ldf, __CLPK_real *scale, __CLPK_real *dif, __CLPK_complex *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int ctpcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_complex *ap, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctprfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_complex *ap, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_complex *x, 
	__CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctptri_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_complex *ap, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctptrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_complex *ap, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ctrcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_real *rcond, __CLPK_complex *work, __CLPK_real *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctrevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_complex *t, __CLPK_integer *ldt, __CLPK_complex *vl, __CLPK_integer *ldvl, 
	__CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_complex *work, 
	__CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctrexc_(char *compq, __CLPK_integer *n, __CLPK_complex *t, __CLPK_integer *
	ldt, __CLPK_complex *q, __CLPK_integer *ldq, __CLPK_integer *ifst, __CLPK_integer *ilst, __CLPK_integer *
	info);
 
/* Subroutine */ int ctrrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_complex *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_complex *work, __CLPK_real 
	*rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctrsen_(char *job, char *compq, __CLPK_logical *select, __CLPK_integer 
	*n, __CLPK_complex *t, __CLPK_integer *ldt, __CLPK_complex *q, __CLPK_integer *ldq, __CLPK_complex *w, 
	__CLPK_integer *m, __CLPK_real *s, __CLPK_real *sep, __CLPK_complex *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctrsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_complex *t, __CLPK_integer *ldt, __CLPK_complex *vl, __CLPK_integer *ldvl, 
	__CLPK_complex *vr, __CLPK_integer *ldvr, __CLPK_real *s, __CLPK_real *sep, __CLPK_integer *mm, __CLPK_integer *
	m, __CLPK_complex *work, __CLPK_integer *ldwork, __CLPK_real *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ctrsyl_(char *trana, char *tranb, __CLPK_integer *isgn, __CLPK_integer 
	*m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_real *scale, __CLPK_integer *info);
 
/* Subroutine */ int ctrti2_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int ctrtri_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int ctrtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int ctzrqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_integer *info);
 
/* Subroutine */ int ctzrzf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cung2l_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cung2r_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cungbr_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int cunghr_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer 
	*info);
 
/* Subroutine */ int cungl2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cunglq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cungql_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cungqr_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cungr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cungrq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cungtr_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_complex *tau, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cunm2l_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cunm2r_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cunmbr_(char *vect, char *side, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, 
	__CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cunmhr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, 
	__CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cunml2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cunmlq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cunmql_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cunmqr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cunmr2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cunmr3_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, 
	__CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cunmrq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cunmrz_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, 
	__CLPK_complex *c__, __CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int cunmtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_complex *tau, __CLPK_complex *c__, 
	__CLPK_integer *ldc, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int cupgtr_(char *uplo, __CLPK_integer *n, __CLPK_complex *ap, __CLPK_complex *
	tau, __CLPK_complex *q, __CLPK_integer *ldq, __CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int cupmtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_complex *ap, __CLPK_complex *tau, __CLPK_complex *c__, __CLPK_integer *ldc, 
	__CLPK_complex *work, __CLPK_integer *info);
 
/* Subroutine */ int dbdsdc_(char *uplo, char *compq, __CLPK_integer *n, __CLPK_doublereal *
	d__, __CLPK_doublereal *e, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *vt, 
	__CLPK_integer *ldvt, __CLPK_doublereal *q, __CLPK_integer *iq, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dbdsqr_(char *uplo, __CLPK_integer *n, __CLPK_integer *ncvt, __CLPK_integer *
	nru, __CLPK_integer *ncc, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *vt, 
	__CLPK_integer *ldvt, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *c__, __CLPK_integer *
	ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int ddisna_(char *job, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	d__, __CLPK_doublereal *sep, __CLPK_integer *info);
 
/* Subroutine */ int dgbbrd_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *ncc,
	 __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *
	d__, __CLPK_doublereal *e, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *pt, 
	__CLPK_integer *ldpt, __CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dgbcon_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_doublereal *anorm, 
	__CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgbequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *r__, __CLPK_doublereal *c__, 
	__CLPK_doublereal *rowcnd, __CLPK_doublereal *colcnd, __CLPK_doublereal *amax, __CLPK_integer *
	info);
 
/* Subroutine */ int dgbrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *afb, 
	__CLPK_integer *ldafb, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgbsv_(__CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_integer *
	nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dgbsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *kl,
	 __CLPK_integer *ku, __CLPK_integer *nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, 
	__CLPK_doublereal *afb, __CLPK_integer *ldafb, __CLPK_integer *ipiv, char *equed, 
	__CLPK_doublereal *r__, __CLPK_doublereal *c__, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgbtf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dgbtrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dgbtrs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dgebak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_doublereal *scale, __CLPK_integer *m, __CLPK_doublereal *v, __CLPK_integer *
	ldv, __CLPK_integer *info);
 
/* Subroutine */ int dgebal_(char *job, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *scale, __CLPK_integer *info);
 
/* Subroutine */ int dgebd2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *tauq, __CLPK_doublereal *
	taup, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgebrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *tauq, __CLPK_doublereal *
	taup, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgecon_(char *norm, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgeequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *r__, __CLPK_doublereal *c__, __CLPK_doublereal *rowcnd, __CLPK_doublereal 
	*colcnd, __CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int dgees_(char *jobvs, char *sort, __CLPK_L_fp select, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *sdim, __CLPK_doublereal *wr, 
	__CLPK_doublereal *wi, __CLPK_doublereal *vs, __CLPK_integer *ldvs, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int dgeesx_(char *jobvs, char *sort, __CLPK_L_fp select, char *
	sense, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *sdim, 
	__CLPK_doublereal *wr, __CLPK_doublereal *wi, __CLPK_doublereal *vs, __CLPK_integer *ldvs, 
	__CLPK_doublereal *rconde, __CLPK_doublereal *rcondv, __CLPK_doublereal *work, __CLPK_integer *
	lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int dgeev_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *wr, __CLPK_doublereal *wi, __CLPK_doublereal *vl, 
	__CLPK_integer *ldvl, __CLPK_doublereal *vr, __CLPK_integer *ldvr, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *wr, 
	__CLPK_doublereal *wi, __CLPK_doublereal *vl, __CLPK_integer *ldvl, __CLPK_doublereal *vr, 
	__CLPK_integer *ldvr, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *scale, 
	__CLPK_doublereal *abnrm, __CLPK_doublereal *rconde, __CLPK_doublereal *rcondv, __CLPK_doublereal 
	*work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgegs_(char *jobvsl, char *jobvsr, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *
	alphar, __CLPK_doublereal *alphai, __CLPK_doublereal *beta, __CLPK_doublereal *vsl, 
	__CLPK_integer *ldvsl, __CLPK_doublereal *vsr, __CLPK_integer *ldvsr, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgegv_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *alphar, 
	__CLPK_doublereal *alphai, __CLPK_doublereal *beta, __CLPK_doublereal *vl, __CLPK_integer *ldvl, 
	__CLPK_doublereal *vr, __CLPK_integer *ldvr, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dgehd2_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dgehrd_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgelq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgelqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgels_(char *trans, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgelsd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *
	s, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublereal *work, __CLPK_integer *lwork,
	 __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgelss_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *
	s, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublereal *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int dgelsx_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	jpvt, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublereal *work, __CLPK_integer *
	info);
 
/* Subroutine */ int dgelsy_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	jpvt, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublereal *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgeql2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgeqlf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgeqp3_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *jpvt, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int dgeqpf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *jpvt, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgeqr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgeqrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgerfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *af, __CLPK_integer *ldaf, __CLPK_integer *
	ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dgerq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgerqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgesc2_(__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, 
	__CLPK_doublereal *rhs, __CLPK_integer *ipiv, __CLPK_integer *jpiv, __CLPK_doublereal *scale);
 
/* Subroutine */ int dgesdd_(char *jobz, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *s, __CLPK_doublereal *u, __CLPK_integer *ldu, 
	__CLPK_doublereal *vt, __CLPK_integer *ldvt, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgesv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *a, __CLPK_integer 
	*lda, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dgesvd_(char *jobu, char *jobvt, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *s, __CLPK_doublereal *u, __CLPK_integer *
	ldu, __CLPK_doublereal *vt, __CLPK_integer *ldvt, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dgesvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *af, __CLPK_integer *ldaf, 
	__CLPK_integer *ipiv, char *equed, __CLPK_doublereal *r__, __CLPK_doublereal *c__, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgetc2_(__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer 
	*ipiv, __CLPK_integer *jpiv, __CLPK_integer *info);
 
/* Subroutine */ int dgetf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dgetrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dgetri_(__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer 
	*ipiv, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dgetrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *
	ldb, __CLPK_integer *info);
 
/* Subroutine */ int dggbak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_doublereal *lscale, __CLPK_doublereal *rscale, __CLPK_integer *m, 
	__CLPK_doublereal *v, __CLPK_integer *ldv, __CLPK_integer *info);
 
/* Subroutine */ int dggbal_(char *job, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublereal *lscale, __CLPK_doublereal *rscale, __CLPK_doublereal *work, __CLPK_integer *
	info);
 
/* Subroutine */ int dgges_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	delctg, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_integer *sdim, __CLPK_doublereal *alphar, __CLPK_doublereal *alphai, 
	__CLPK_doublereal *beta, __CLPK_doublereal *vsl, __CLPK_integer *ldvsl, __CLPK_doublereal *vsr, 
	__CLPK_integer *ldvsr, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_logical *bwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dggesx_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	delctg, char *sense, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *sdim, __CLPK_doublereal *alphar, 
	__CLPK_doublereal *alphai, __CLPK_doublereal *beta, __CLPK_doublereal *vsl, __CLPK_integer *ldvsl,
	 __CLPK_doublereal *vsr, __CLPK_integer *ldvsr, __CLPK_doublereal *rconde, __CLPK_doublereal *
	rcondv, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *
	liwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int dggev_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *alphar, 
	__CLPK_doublereal *alphai, __CLPK_doublereal *beta, __CLPK_doublereal *vl, __CLPK_integer *ldvl, 
	__CLPK_doublereal *vr, __CLPK_integer *ldvr, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *alphar, __CLPK_doublereal *alphai, __CLPK_doublereal *
	beta, __CLPK_doublereal *vl, __CLPK_integer *ldvl, __CLPK_doublereal *vr, __CLPK_integer *ldvr, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *lscale, __CLPK_doublereal *rscale, 
	__CLPK_doublereal *abnrm, __CLPK_doublereal *bbnrm, __CLPK_doublereal *rconde, __CLPK_doublereal *
	rcondv, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_logical *
	bwork, __CLPK_integer *info);
 
/* Subroutine */ int dggglm_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *d__, 
	__CLPK_doublereal *x, __CLPK_doublereal *y, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dgghrd_(char *compq, char *compz, __CLPK_integer *n, __CLPK_integer *
	ilo, __CLPK_integer *ihi, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *z__, __CLPK_integer *
	ldz, __CLPK_integer *info);
 
/* Subroutine */ int dgglse_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *p, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *c__, 
	__CLPK_doublereal *d__, __CLPK_doublereal *x, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dggqrf_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *taua, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *taub, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dggrqf_(__CLPK_integer *m, __CLPK_integer *p, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *taua, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *taub, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dggsvd_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *p, __CLPK_integer *k, __CLPK_integer *l, __CLPK_doublereal *a, 
	__CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *alpha, 
	__CLPK_doublereal *beta, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *v, __CLPK_integer 
	*ldv, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dggsvp_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *tola, __CLPK_doublereal *tolb, __CLPK_integer *k, __CLPK_integer 
	*l, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *v, __CLPK_integer *ldv, 
	__CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_integer *iwork, __CLPK_doublereal *tau, 
	__CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dgtcon_(char *norm, __CLPK_integer *n, __CLPK_doublereal *dl, 
	__CLPK_doublereal *d__, __CLPK_doublereal *du, __CLPK_doublereal *du2, __CLPK_integer *ipiv, 
	__CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgtrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *dl, __CLPK_doublereal *d__, __CLPK_doublereal *du, __CLPK_doublereal *dlf, 
	__CLPK_doublereal *df, __CLPK_doublereal *duf, __CLPK_doublereal *du2, __CLPK_integer *ipiv, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int dgtsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *dl, 
	__CLPK_doublereal *d__, __CLPK_doublereal *du, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer 
	*info);
 
/* Subroutine */ int dgtsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *dl, __CLPK_doublereal *d__, __CLPK_doublereal *du, __CLPK_doublereal *
	dlf, __CLPK_doublereal *df, __CLPK_doublereal *duf, __CLPK_doublereal *du2, __CLPK_integer *ipiv, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dgttrf_(__CLPK_integer *n, __CLPK_doublereal *dl, __CLPK_doublereal *d__, 
	__CLPK_doublereal *du, __CLPK_doublereal *du2, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dgttrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *dl, __CLPK_doublereal *d__, __CLPK_doublereal *du, __CLPK_doublereal *du2, 
	__CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dgtts2_(__CLPK_integer *itrans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *dl, __CLPK_doublereal *d__, __CLPK_doublereal *du, __CLPK_doublereal *du2, 
	__CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb);
 
/* Subroutine */ int dhgeqz_(char *job, char *compq, char *compz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *
	b, __CLPK_integer *ldb, __CLPK_doublereal *alphar, __CLPK_doublereal *alphai, __CLPK_doublereal *
	beta, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *z__, __CLPK_integer *ldz, 
	__CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dhsein_(char *side, char *eigsrc, char *initv, __CLPK_logical *
	select, __CLPK_integer *n, __CLPK_doublereal *h__, __CLPK_integer *ldh, __CLPK_doublereal *wr, 
	__CLPK_doublereal *wi, __CLPK_doublereal *vl, __CLPK_integer *ldvl, __CLPK_doublereal *vr, 
	__CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_doublereal *work, __CLPK_integer *
	ifaill, __CLPK_integer *ifailr, __CLPK_integer *info);
 
/* Subroutine */ int dhseqr_(char *job, char *compz, __CLPK_integer *n, __CLPK_integer *ilo,
	 __CLPK_integer *ihi, __CLPK_doublereal *h__, __CLPK_integer *ldh, __CLPK_doublereal *wr, 
	__CLPK_doublereal *wi, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dlabad_(__CLPK_doublereal *small, __CLPK_doublereal *large);
 
/* Subroutine */ int dlabrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *tauq, 
	__CLPK_doublereal *taup, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *y, __CLPK_integer 
	*ldy);
 
/* Subroutine */ int dlacon_(__CLPK_integer *n, __CLPK_doublereal *v, __CLPK_doublereal *x, 
	__CLPK_integer *isgn, __CLPK_doublereal *est, __CLPK_integer *kase);
 
/* Subroutine */ int dlacpy_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb);
 
/* Subroutine */ int dladiv_(__CLPK_doublereal *a, __CLPK_doublereal *b, __CLPK_doublereal *c__, 
	__CLPK_doublereal *d__, __CLPK_doublereal *p, __CLPK_doublereal *q);
 
/* Subroutine */ int dlae2_(__CLPK_doublereal *a, __CLPK_doublereal *b, __CLPK_doublereal *c__, 
	__CLPK_doublereal *rt1, __CLPK_doublereal *rt2);
 
/* Subroutine */ int dlaebz_(__CLPK_integer *ijob, __CLPK_integer *nitmax, __CLPK_integer *n, 
	__CLPK_integer *mmax, __CLPK_integer *minp, __CLPK_integer *nbmin, __CLPK_doublereal *abstol, 
	__CLPK_doublereal *reltol, __CLPK_doublereal *pivmin, __CLPK_doublereal *d__, __CLPK_doublereal *
	e, __CLPK_doublereal *e2, __CLPK_integer *nval, __CLPK_doublereal *ab, __CLPK_doublereal *c__, 
	__CLPK_integer *mout, __CLPK_integer *nab, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dlaed0_(__CLPK_integer *icompq, __CLPK_integer *qsiz, __CLPK_integer *n, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *q, __CLPK_integer *ldq, 
	__CLPK_doublereal *qstore, __CLPK_integer *ldqs, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dlaed1_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *q, 
	__CLPK_integer *ldq, __CLPK_integer *indxq, __CLPK_doublereal *rho, __CLPK_integer *cutpnt, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlaed2_(__CLPK_integer *k, __CLPK_integer *n, __CLPK_integer *n1, __CLPK_doublereal *
	d__, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_integer *indxq, __CLPK_doublereal *rho, 
	__CLPK_doublereal *z__, __CLPK_doublereal *dlamda, __CLPK_doublereal *w, __CLPK_doublereal *q2, 
	__CLPK_integer *indx, __CLPK_integer *indxc, __CLPK_integer *indxp, __CLPK_integer *coltyp, 
	__CLPK_integer *info);
 
/* Subroutine */ int dlaed3_(__CLPK_integer *k, __CLPK_integer *n, __CLPK_integer *n1, __CLPK_doublereal *
	d__, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *rho, __CLPK_doublereal *dlamda,
	 __CLPK_doublereal *q2, __CLPK_integer *indx, __CLPK_integer *ctot, __CLPK_doublereal *w, 
	__CLPK_doublereal *s, __CLPK_integer *info);
 
/* Subroutine */ int dlaed4_(__CLPK_integer *n, __CLPK_integer *i__, __CLPK_doublereal *d__, 
	__CLPK_doublereal *z__, __CLPK_doublereal *delta, __CLPK_doublereal *rho, __CLPK_doublereal *dlam,
	 __CLPK_integer *info);
 
/* Subroutine */ int dlaed5_(__CLPK_integer *i__, __CLPK_doublereal *d__, __CLPK_doublereal *z__, 
	__CLPK_doublereal *delta, __CLPK_doublereal *rho, __CLPK_doublereal *dlam);
 
/* Subroutine */ int dlaed6_(__CLPK_integer *kniter, __CLPK_logical *orgati, __CLPK_doublereal *
	rho, __CLPK_doublereal *d__, __CLPK_doublereal *z__, __CLPK_doublereal *finit, __CLPK_doublereal *
	tau, __CLPK_integer *info);
 
/* Subroutine */ int dlaed7_(__CLPK_integer *icompq, __CLPK_integer *n, __CLPK_integer *qsiz, 
	__CLPK_integer *tlvls, __CLPK_integer *curlvl, __CLPK_integer *curpbm, __CLPK_doublereal *d__, 
	__CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_integer *indxq, __CLPK_doublereal *rho, __CLPK_integer 
	*cutpnt, __CLPK_doublereal *qstore, __CLPK_integer *qptr, __CLPK_integer *prmptr, __CLPK_integer *
	perm, __CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_doublereal *givnum, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlaed8_(__CLPK_integer *icompq, __CLPK_integer *k, __CLPK_integer *n, __CLPK_integer 
	*qsiz, __CLPK_doublereal *d__, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_integer *indxq, 
	__CLPK_doublereal *rho, __CLPK_integer *cutpnt, __CLPK_doublereal *z__, __CLPK_doublereal *dlamda,
	 __CLPK_doublereal *q2, __CLPK_integer *ldq2, __CLPK_doublereal *w, __CLPK_integer *perm, __CLPK_integer 
	*givptr, __CLPK_integer *givcol, __CLPK_doublereal *givnum, __CLPK_integer *indxp, __CLPK_integer 
	*indx, __CLPK_integer *info);
 
/* Subroutine */ int dlaed9_(__CLPK_integer *k, __CLPK_integer *kstart, __CLPK_integer *kstop, 
	__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *
	rho, __CLPK_doublereal *dlamda, __CLPK_doublereal *w, __CLPK_doublereal *s, __CLPK_integer *lds, 
	__CLPK_integer *info);
 
/* Subroutine */ int dlaeda_(__CLPK_integer *n, __CLPK_integer *tlvls, __CLPK_integer *curlvl, 
	__CLPK_integer *curpbm, __CLPK_integer *prmptr, __CLPK_integer *perm, __CLPK_integer *givptr, 
	__CLPK_integer *givcol, __CLPK_doublereal *givnum, __CLPK_doublereal *q, __CLPK_integer *qptr, 
	__CLPK_doublereal *z__, __CLPK_doublereal *ztemp, __CLPK_integer *info);
 
/* Subroutine */ int dlaein_(__CLPK_logical *rightv, __CLPK_logical *noinit, __CLPK_integer *n, 
	__CLPK_doublereal *h__, __CLPK_integer *ldh, __CLPK_doublereal *wr, __CLPK_doublereal *wi, 
	__CLPK_doublereal *vr, __CLPK_doublereal *vi, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *work, __CLPK_doublereal *eps3, __CLPK_doublereal *smlnum, __CLPK_doublereal *
	bignum, __CLPK_integer *info);
 
/* Subroutine */ int dlaev2_(__CLPK_doublereal *a, __CLPK_doublereal *b, __CLPK_doublereal *c__, 
	__CLPK_doublereal *rt1, __CLPK_doublereal *rt2, __CLPK_doublereal *cs1, __CLPK_doublereal *sn1);
 
/* Subroutine */ int dlaexc_(__CLPK_logical *wantq, __CLPK_integer *n, __CLPK_doublereal *t, 
	__CLPK_integer *ldt, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_integer *j1, __CLPK_integer *n1, 
	__CLPK_integer *n2, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlag2_(__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *safmin, __CLPK_doublereal *scale1, __CLPK_doublereal *
	scale2, __CLPK_doublereal *wr1, __CLPK_doublereal *wr2, __CLPK_doublereal *wi);
 
/* Subroutine */ int dlags2_(__CLPK_logical *upper, __CLPK_doublereal *a1, __CLPK_doublereal *a2, 
	__CLPK_doublereal *a3, __CLPK_doublereal *b1, __CLPK_doublereal *b2, __CLPK_doublereal *b3, 
	__CLPK_doublereal *csu, __CLPK_doublereal *snu, __CLPK_doublereal *csv, __CLPK_doublereal *snv, 
	__CLPK_doublereal *csq, __CLPK_doublereal *snq);
 
/* Subroutine */ int dlagtf_(__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_doublereal *lambda, 
	__CLPK_doublereal *b, __CLPK_doublereal *c__, __CLPK_doublereal *tol, __CLPK_doublereal *d__, 
	__CLPK_integer *in, __CLPK_integer *info);
 
/* Subroutine */ int dlagtm_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *alpha, __CLPK_doublereal *dl, __CLPK_doublereal *d__, __CLPK_doublereal *du, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *beta, __CLPK_doublereal *b, __CLPK_integer 
	*ldb);
 
/* Subroutine */ int dlagts_(__CLPK_integer *job, __CLPK_integer *n, __CLPK_doublereal *a, 
	__CLPK_doublereal *b, __CLPK_doublereal *c__, __CLPK_doublereal *d__, __CLPK_integer *in, 
	__CLPK_doublereal *y, __CLPK_doublereal *tol, __CLPK_integer *info);
 
/* Subroutine */ int dlagv2_(__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *alphar, __CLPK_doublereal *alphai, __CLPK_doublereal *
	beta, __CLPK_doublereal *csl, __CLPK_doublereal *snl, __CLPK_doublereal *csr, __CLPK_doublereal *
	snr);
 
/* Subroutine */ int dlahqr_(__CLPK_logical *wantt, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *h__, __CLPK_integer *ldh, __CLPK_doublereal 
	*wr, __CLPK_doublereal *wi, __CLPK_integer *iloz, __CLPK_integer *ihiz, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_integer *info);
 
/* Subroutine */ int dlahrd_(__CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *nb, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *t, __CLPK_integer *ldt, 
	__CLPK_doublereal *y, __CLPK_integer *ldy);
 
/* Subroutine */ int dlaic1_(__CLPK_integer *job, __CLPK_integer *j, __CLPK_doublereal *x, 
	__CLPK_doublereal *sest, __CLPK_doublereal *w, __CLPK_doublereal *gamma, __CLPK_doublereal *
	sestpr, __CLPK_doublereal *s, __CLPK_doublereal *c__);
 
/* Subroutine */ int dlaln2_(__CLPK_logical *ltrans, __CLPK_integer *na, __CLPK_integer *nw, 
	__CLPK_doublereal *smin, __CLPK_doublereal *ca, __CLPK_doublereal *a, __CLPK_integer *lda, 
	__CLPK_doublereal *d1, __CLPK_doublereal *d2, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *wr, __CLPK_doublereal *wi, __CLPK_doublereal *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *scale, __CLPK_doublereal *xnorm, __CLPK_integer *info);
 
/* Subroutine */ int dlals0_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_integer *nrhs, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal 
	*bx, __CLPK_integer *ldbx, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *givcol, 
	__CLPK_integer *ldgcol, __CLPK_doublereal *givnum, __CLPK_integer *ldgnum, __CLPK_doublereal *
	poles, __CLPK_doublereal *difl, __CLPK_doublereal *difr, __CLPK_doublereal *z__, __CLPK_integer *
	k, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlalsa_(__CLPK_integer *icompq, __CLPK_integer *smlsiz, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *bx, __CLPK_integer *
	ldbx, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *vt, __CLPK_integer *k, 
	__CLPK_doublereal *difl, __CLPK_doublereal *difr, __CLPK_doublereal *z__, __CLPK_doublereal *
	poles, __CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_integer *
	perm, __CLPK_doublereal *givnum, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_doublereal *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlalsd_(char *uplo, __CLPK_integer *smlsiz, __CLPK_integer *n, __CLPK_integer 
	*nrhs, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dlamc1_(__CLPK_integer *beta, __CLPK_integer *t, __CLPK_logical *rnd, __CLPK_logical 
	*ieee1);
 
/* Subroutine */ int dlamc2_(__CLPK_integer *beta, __CLPK_integer *t, __CLPK_logical *rnd, 
	__CLPK_doublereal *eps, __CLPK_integer *emin, __CLPK_doublereal *rmin, __CLPK_integer *emax, 
	__CLPK_doublereal *rmax);
 
/* Subroutine */ int dlamc4_(__CLPK_integer *emin, __CLPK_doublereal *start, __CLPK_integer *base);
 
/* Subroutine */ int dlamc5_(__CLPK_integer *beta, __CLPK_integer *p, __CLPK_integer *emin, 
	__CLPK_logical *ieee, __CLPK_integer *emax, __CLPK_doublereal *rmax);
 
/* Subroutine */ int dlamrg_(__CLPK_integer *n1, __CLPK_integer *n2, __CLPK_doublereal *a, __CLPK_integer 
	*dtrd1, __CLPK_integer *dtrd2, __CLPK_integer *index);
 
/* Subroutine */ int dlanv2_(__CLPK_doublereal *a, __CLPK_doublereal *b, __CLPK_doublereal *c__, 
	__CLPK_doublereal *d__, __CLPK_doublereal *rt1r, __CLPK_doublereal *rt1i, __CLPK_doublereal *rt2r,
	 __CLPK_doublereal *rt2i, __CLPK_doublereal *cs, __CLPK_doublereal *sn);
 
/* Subroutine */ int dlapll_(__CLPK_integer *n, __CLPK_doublereal *x, __CLPK_integer *incx, 
	__CLPK_doublereal *y, __CLPK_integer *incy, __CLPK_doublereal *ssmin);
 
/* Subroutine */ int dlapmt_(__CLPK_logical *forwrd, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_integer *k);
 
/* Subroutine */ int dlaqgb_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *r__, __CLPK_doublereal *c__, 
	__CLPK_doublereal *rowcnd, __CLPK_doublereal *colcnd, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int dlaqge_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *r__, __CLPK_doublereal *c__, __CLPK_doublereal *rowcnd, __CLPK_doublereal 
	*colcnd, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int dlaqp2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_doublereal *tau, 
	__CLPK_doublereal *vn1, __CLPK_doublereal *vn2, __CLPK_doublereal *work);
 
/* Subroutine */ int dlaqps_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, __CLPK_integer 
	*nb, __CLPK_integer *kb, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *jpvt, 
	__CLPK_doublereal *tau, __CLPK_doublereal *vn1, __CLPK_doublereal *vn2, __CLPK_doublereal *auxv, 
	__CLPK_doublereal *f, __CLPK_integer *ldf);
 
/* Subroutine */ int dlaqsb_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *
	ab, __CLPK_integer *ldab, __CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax,
	 char *equed);
 
/* Subroutine */ int dlaqsp_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int dlaqsy_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int dlaqtr_(__CLPK_logical *ltran, __CLPK_logical *lreal, __CLPK_integer *n, 
	__CLPK_doublereal *t, __CLPK_integer *ldt, __CLPK_doublereal *b, __CLPK_doublereal *w, __CLPK_doublereal 
	*scale, __CLPK_doublereal *x, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlar1v_(__CLPK_integer *n, __CLPK_integer *b1, __CLPK_integer *bn, __CLPK_doublereal 
	*sigma, __CLPK_doublereal *d__, __CLPK_doublereal *l, __CLPK_doublereal *ld, __CLPK_doublereal *
	lld, __CLPK_doublereal *gersch, __CLPK_doublereal *z__, __CLPK_doublereal *ztz, __CLPK_doublereal 
	*mingma, __CLPK_integer *r__, __CLPK_integer *isuppz, __CLPK_doublereal *work);
 
/* Subroutine */ int dlar2v_(__CLPK_integer *n, __CLPK_doublereal *x, __CLPK_doublereal *y, 
	__CLPK_doublereal *z__, __CLPK_integer *incx, __CLPK_doublereal *c__, __CLPK_doublereal *s, 
	__CLPK_integer *incc);
 
/* Subroutine */ int dlarf_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *v,
	 __CLPK_integer *incv, __CLPK_doublereal *tau, __CLPK_doublereal *c__, __CLPK_integer *ldc, 
	__CLPK_doublereal *work);
 
/* Subroutine */ int dlarfb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *v, __CLPK_integer *
	ldv, __CLPK_doublereal *t, __CLPK_integer *ldt, __CLPK_doublereal *c__, __CLPK_integer *ldc, 
	__CLPK_doublereal *work, __CLPK_integer *ldwork);
 
/* Subroutine */ int dlarfg_(__CLPK_integer *n, __CLPK_doublereal *alpha, __CLPK_doublereal *x, 
	__CLPK_integer *incx, __CLPK_doublereal *tau);
 
/* Subroutine */ int dlarft_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_doublereal *v, __CLPK_integer *ldv, __CLPK_doublereal *tau, __CLPK_doublereal *t, 
	__CLPK_integer *ldt);
 
/* Subroutine */ int dlarfx_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	v, __CLPK_doublereal *tau, __CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work);
 
/* Subroutine */ int dlargv_(__CLPK_integer *n, __CLPK_doublereal *x, __CLPK_integer *incx, 
	__CLPK_doublereal *y, __CLPK_integer *incy, __CLPK_doublereal *c__, __CLPK_integer *incc);
 
/* Subroutine */ int dlarnv_(__CLPK_integer *idist, __CLPK_integer *iseed, __CLPK_integer *n, 
	__CLPK_doublereal *x);
 
/* Subroutine */ int dlarrb_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *l, 
	__CLPK_doublereal *ld, __CLPK_doublereal *lld, __CLPK_integer *ifirst, __CLPK_integer *ilast, 
	__CLPK_doublereal *sigma, __CLPK_doublereal *reltol, __CLPK_doublereal *w, __CLPK_doublereal *
	wgap, __CLPK_doublereal *werr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int dlarre_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublereal *tol, __CLPK_integer *nsplit, __CLPK_integer *isplit, __CLPK_integer *m, 
	__CLPK_doublereal *w, __CLPK_doublereal *woff, __CLPK_doublereal *gersch, __CLPK_doublereal *work,
	 __CLPK_integer *info);
 
/* Subroutine */ int dlarrf_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *l, 
	__CLPK_doublereal *ld, __CLPK_doublereal *lld, __CLPK_integer *ifirst, __CLPK_integer *ilast, 
	__CLPK_doublereal *w, __CLPK_doublereal *dplus, __CLPK_doublereal *lplus, __CLPK_doublereal *work,
	 __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlarrv_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *l, 
	__CLPK_integer *isplit, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_integer *iblock, 
	__CLPK_doublereal *gersch, __CLPK_doublereal *tol, __CLPK_doublereal *z__, __CLPK_integer *ldz, 
	__CLPK_integer *isuppz, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlartg_(__CLPK_doublereal *f, __CLPK_doublereal *g, __CLPK_doublereal *cs, 
	__CLPK_doublereal *sn, __CLPK_doublereal *r__);
 
/* Subroutine */ int dlartv_(__CLPK_integer *n, __CLPK_doublereal *x, __CLPK_integer *incx, 
	__CLPK_doublereal *y, __CLPK_integer *incy, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_integer 
	*incc);
 
/* Subroutine */ int dlaruv_(__CLPK_integer *iseed, __CLPK_integer *n, __CLPK_doublereal *x);
 
/* Subroutine */ int dlarz_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, 
	__CLPK_doublereal *v, __CLPK_integer *incv, __CLPK_doublereal *tau, __CLPK_doublereal *c__, 
	__CLPK_integer *ldc, __CLPK_doublereal *work);
 
/* Subroutine */ int dlarzb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_doublereal *v,
	 __CLPK_integer *ldv, __CLPK_doublereal *t, __CLPK_integer *ldt, __CLPK_doublereal *c__, __CLPK_integer *
	ldc, __CLPK_doublereal *work, __CLPK_integer *ldwork);
 
/* Subroutine */ int dlarzt_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_doublereal *v, __CLPK_integer *ldv, __CLPK_doublereal *tau, __CLPK_doublereal *t, 
	__CLPK_integer *ldt);
 
/* Subroutine */ int dlas2_(__CLPK_doublereal *f, __CLPK_doublereal *g, __CLPK_doublereal *h__, 
	__CLPK_doublereal *ssmin, __CLPK_doublereal *ssmax);
 
/* Subroutine */ int dlascl_(char *type__, __CLPK_integer *kl, __CLPK_integer *ku, 
	__CLPK_doublereal *cfrom, __CLPK_doublereal *cto, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int dlasd0_(__CLPK_integer *n, __CLPK_integer *sqre, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *vt, __CLPK_integer *
	ldvt, __CLPK_integer *smlsiz, __CLPK_integer *iwork, __CLPK_doublereal *work, __CLPK_integer *
	info);
 
/* Subroutine */ int dlasd1_(__CLPK_integer *nl, __CLPK_integer *nr, __CLPK_integer *sqre, 
	__CLPK_doublereal *d__, __CLPK_doublereal *alpha, __CLPK_doublereal *beta, __CLPK_doublereal *u, 
	__CLPK_integer *ldu, __CLPK_doublereal *vt, __CLPK_integer *ldvt, __CLPK_integer *idxq, __CLPK_integer *
	iwork, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlasd2_(__CLPK_integer *nl, __CLPK_integer *nr, __CLPK_integer *sqre, __CLPK_integer 
	*k, __CLPK_doublereal *d__, __CLPK_doublereal *z__, __CLPK_doublereal *alpha, __CLPK_doublereal *
	beta, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *vt, __CLPK_integer *ldvt, 
	__CLPK_doublereal *dsigma, __CLPK_doublereal *u2, __CLPK_integer *ldu2, __CLPK_doublereal *vt2, 
	__CLPK_integer *ldvt2, __CLPK_integer *idxp, __CLPK_integer *idx, __CLPK_integer *idxc, __CLPK_integer *
	idxq, __CLPK_integer *coltyp, __CLPK_integer *info);
 
/* Subroutine */ int dlasd3_(__CLPK_integer *nl, __CLPK_integer *nr, __CLPK_integer *sqre, __CLPK_integer 
	*k, __CLPK_doublereal *d__, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *dsigma, 
	__CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *u2, __CLPK_integer *ldu2, 
	__CLPK_doublereal *vt, __CLPK_integer *ldvt, __CLPK_doublereal *vt2, __CLPK_integer *ldvt2, 
	__CLPK_integer *idxc, __CLPK_integer *ctot, __CLPK_doublereal *z__, __CLPK_integer *info);
 
/* Subroutine */ int dlasd4_(__CLPK_integer *n, __CLPK_integer *i__, __CLPK_doublereal *d__, 
	__CLPK_doublereal *z__, __CLPK_doublereal *delta, __CLPK_doublereal *rho, __CLPK_doublereal *
	sigma, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlasd5_(__CLPK_integer *i__, __CLPK_doublereal *d__, __CLPK_doublereal *z__, 
	__CLPK_doublereal *delta, __CLPK_doublereal *rho, __CLPK_doublereal *dsigma, __CLPK_doublereal *
	work);
 
/* Subroutine */ int dlasd6_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_doublereal *d__, __CLPK_doublereal *vf, __CLPK_doublereal *vl, 
	__CLPK_doublereal *alpha, __CLPK_doublereal *beta, __CLPK_integer *idxq, __CLPK_integer *perm, 
	__CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_doublereal *givnum,
	 __CLPK_integer *ldgnum, __CLPK_doublereal *poles, __CLPK_doublereal *difl, __CLPK_doublereal *
	difr, __CLPK_doublereal *z__, __CLPK_integer *k, __CLPK_doublereal *c__, __CLPK_doublereal *s, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlasd7_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_integer *k, __CLPK_doublereal *d__, __CLPK_doublereal *z__, 
	__CLPK_doublereal *zw, __CLPK_doublereal *vf, __CLPK_doublereal *vfw, __CLPK_doublereal *vl, 
	__CLPK_doublereal *vlw, __CLPK_doublereal *alpha, __CLPK_doublereal *beta, __CLPK_doublereal *
	dsigma, __CLPK_integer *idx, __CLPK_integer *idxp, __CLPK_integer *idxq, __CLPK_integer *perm, 
	__CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_doublereal *givnum,
	 __CLPK_integer *ldgnum, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_integer *info);
 
/* Subroutine */ int dlasd8_(__CLPK_integer *icompq, __CLPK_integer *k, __CLPK_doublereal *d__, 
	__CLPK_doublereal *z__, __CLPK_doublereal *vf, __CLPK_doublereal *vl, __CLPK_doublereal *difl, 
	__CLPK_doublereal *difr, __CLPK_integer *lddifr, __CLPK_doublereal *dsigma, __CLPK_doublereal *
	work, __CLPK_integer *info);
 
/* Subroutine */ int dlasd9_(__CLPK_integer *icompq, __CLPK_integer *ldu, __CLPK_integer *k, 
	__CLPK_doublereal *d__, __CLPK_doublereal *z__, __CLPK_doublereal *vf, __CLPK_doublereal *vl, 
	__CLPK_doublereal *difl, __CLPK_doublereal *difr, __CLPK_doublereal *dsigma, __CLPK_doublereal *
	work, __CLPK_integer *info);
 
/* Subroutine */ int dlasda_(__CLPK_integer *icompq, __CLPK_integer *smlsiz, __CLPK_integer *n, 
	__CLPK_integer *sqre, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *u, __CLPK_integer 
	*ldu, __CLPK_doublereal *vt, __CLPK_integer *k, __CLPK_doublereal *difl, __CLPK_doublereal *difr, 
	__CLPK_doublereal *z__, __CLPK_doublereal *poles, __CLPK_integer *givptr, __CLPK_integer *givcol, 
	__CLPK_integer *ldgcol, __CLPK_integer *perm, __CLPK_doublereal *givnum, __CLPK_doublereal *c__, 
	__CLPK_doublereal *s, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dlasdq_(char *uplo, __CLPK_integer *sqre, __CLPK_integer *n, __CLPK_integer *
	ncvt, __CLPK_integer *nru, __CLPK_integer *ncc, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublereal *vt, __CLPK_integer *ldvt, __CLPK_doublereal *u, __CLPK_integer *ldu, 
	__CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlasdt_(__CLPK_integer *n, __CLPK_integer *lvl, __CLPK_integer *nd, __CLPK_integer *
	inode, __CLPK_integer *ndiml, __CLPK_integer *ndimr, __CLPK_integer *msub);
 
/* Subroutine */ int dlaset_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	alpha, __CLPK_doublereal *beta, __CLPK_doublereal *a, __CLPK_integer *lda);
 
/* Subroutine */ int dlasq1_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dlasq2_(__CLPK_integer *n, __CLPK_doublereal *z__, __CLPK_integer *info);
 
/* Subroutine */ int dlasq3_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_doublereal *z__, 
	__CLPK_integer *pp, __CLPK_doublereal *dmin__, __CLPK_doublereal *sigma, __CLPK_doublereal *desig,
	 __CLPK_doublereal *qmax, __CLPK_integer *nfail, __CLPK_integer *iter, __CLPK_integer *ndiv, 
	__CLPK_logical *ieee);
 
/* Subroutine */ int dlasq4_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_doublereal *z__, 
	__CLPK_integer *pp, __CLPK_integer *n0in, __CLPK_doublereal *dmin__, __CLPK_doublereal *dmin1, 
	__CLPK_doublereal *dmin2, __CLPK_doublereal *dn, __CLPK_doublereal *dn1, __CLPK_doublereal *dn2, 
	__CLPK_doublereal *tau, __CLPK_integer *ttype);
 
/* Subroutine */ int dlasq5_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_doublereal *z__, 
	__CLPK_integer *pp, __CLPK_doublereal *tau, __CLPK_doublereal *dmin__, __CLPK_doublereal *dmin1, 
	__CLPK_doublereal *dmin2, __CLPK_doublereal *dn, __CLPK_doublereal *dnm1, __CLPK_doublereal *dnm2,
	 __CLPK_logical *ieee);
 
/* Subroutine */ int dlasq6_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_doublereal *z__, 
	__CLPK_integer *pp, __CLPK_doublereal *dmin__, __CLPK_doublereal *dmin1, __CLPK_doublereal *dmin2,
	 __CLPK_doublereal *dn, __CLPK_doublereal *dnm1, __CLPK_doublereal *dnm2);
 
/* Subroutine */ int dlasr_(char *side, char *pivot, char *direct, __CLPK_integer *m,
	 __CLPK_integer *n, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_doublereal *a, __CLPK_integer *
	lda);
 
/* Subroutine */ int dlasrt_(char *id, __CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_integer *
	info);
 
/* Subroutine */ int dlassq_(__CLPK_integer *n, __CLPK_doublereal *x, __CLPK_integer *incx, 
	__CLPK_doublereal *scale, __CLPK_doublereal *sumsq);
 
/* Subroutine */ int dlasv2_(__CLPK_doublereal *f, __CLPK_doublereal *g, __CLPK_doublereal *h__, 
	__CLPK_doublereal *ssmin, __CLPK_doublereal *ssmax, __CLPK_doublereal *snr, __CLPK_doublereal *
	csr, __CLPK_doublereal *snl, __CLPK_doublereal *csl);
 
/* Subroutine */ int dlaswp_(__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer 
	*k1, __CLPK_integer *k2, __CLPK_integer *ipiv, __CLPK_integer *incx);
 
/* Subroutine */ int dlasy2_(__CLPK_logical *ltranl, __CLPK_logical *ltranr, __CLPK_integer *isgn, 
	__CLPK_integer *n1, __CLPK_integer *n2, __CLPK_doublereal *tl, __CLPK_integer *ldtl, __CLPK_doublereal *
	tr, __CLPK_integer *ldtr, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *scale, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *xnorm, __CLPK_integer *info);
 
/* Subroutine */ int dlasyf_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_integer *kb,
	 __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublereal *w, __CLPK_integer *
	ldw, __CLPK_integer *info);
 
/* Subroutine */ int dlatbs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *ab, __CLPK_integer *ldab, 
	__CLPK_doublereal *x, __CLPK_doublereal *scale, __CLPK_doublereal *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int dlatdf_(__CLPK_integer *ijob, __CLPK_integer *n, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *rhs, __CLPK_doublereal *rdsum, __CLPK_doublereal *rdscal, 
	__CLPK_integer *ipiv, __CLPK_integer *jpiv);
 
/* Subroutine */ int dlatps_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_doublereal *x, __CLPK_doublereal *scale, 
	__CLPK_doublereal *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int dlatrd_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *e, __CLPK_doublereal *tau, __CLPK_doublereal *w, 
	__CLPK_integer *ldw);
 
/* Subroutine */ int dlatrs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *x, 
	__CLPK_doublereal *scale, __CLPK_doublereal *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int dlatrz_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work);
 
/* Subroutine */ int dlatzm_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	v, __CLPK_integer *incv, __CLPK_doublereal *tau, __CLPK_doublereal *c1, __CLPK_doublereal *c2, 
	__CLPK_integer *ldc, __CLPK_doublereal *work);
 
/* Subroutine */ int dlauu2_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *info);
 
/* Subroutine */ int dlauum_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *info);
 
/* Subroutine */ int dopgtr_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, 
	__CLPK_doublereal *tau, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dopmtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_doublereal *tau, __CLPK_doublereal *c__, __CLPK_integer 
	*ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dorg2l_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dorg2r_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dorgbr_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dorghr_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dorgl2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dorglq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dorgql_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dorgqr_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dorgr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dorgrq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dorgtr_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dorm2l_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dorm2r_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dormbr_(char *vect, char *side, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, 
	__CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dormhr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *
	tau, __CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dorml2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dormlq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dormql_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dormqr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dormr2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dormr3_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, 
	__CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dormrq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dormrz_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, 
	__CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dormtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *tau, __CLPK_doublereal *
	c__, __CLPK_integer *ldc, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dpbcon_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *
	ab, __CLPK_integer *ldab, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dpbequ_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *
	ab, __CLPK_integer *ldab, __CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax,
	 __CLPK_integer *info);
 
/* Subroutine */ int dpbrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *afb, __CLPK_integer *ldafb, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int dpbstf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *
	ab, __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int dpbsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int dpbsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_integer *nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *afb, 
	__CLPK_integer *ldafb, char *equed, __CLPK_doublereal *s, __CLPK_doublereal *b, __CLPK_integer *
	ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr,
	 __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dpbtf2_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *
	ab, __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int dpbtrf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublereal *
	ab, __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int dpbtrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int dpocon_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dpoequ_(__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int dporfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *af, __CLPK_integer *ldaf, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int dposv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal 
	*a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dposvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *af, __CLPK_integer *ldaf, 
	char *equed, __CLPK_doublereal *s, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *
	x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *
	berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dpotf2_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *info);
 
/* Subroutine */ int dpotrf_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *info);
 
/* Subroutine */ int dpotri_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *info);
 
/* Subroutine */ int dpotrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int dppcon_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, 
	__CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dppequ_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int dpprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *ap, __CLPK_doublereal *afp, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dppsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal 
	*ap, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dppsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *ap, __CLPK_doublereal *afp, char *equed, __CLPK_doublereal *s, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dpptrf_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_integer *
	info);
 
/* Subroutine */ int dpptri_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_integer *
	info);
 
/* Subroutine */ int dpptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *ap, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dptcon_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dpteqr_(char *compz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dptrfs_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *df, __CLPK_doublereal *ef, __CLPK_doublereal *b, __CLPK_integer 
	*ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr,
	 __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dptsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dptsvx_(char *fact, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *df, __CLPK_doublereal *ef, 
	__CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *
	rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *
	info);
 
/* Subroutine */ int dpttrf_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_integer *info);
 
/* Subroutine */ int dpttrs_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dptts2_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *b, __CLPK_integer *ldb);
 
/* Subroutine */ int drscl_(__CLPK_integer *n, __CLPK_doublereal *sa, __CLPK_doublereal *sx, 
	__CLPK_integer *incx);
 
/* Subroutine */ int dsbev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dsbevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dsbevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *q, __CLPK_integer *
	ldq, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, 
	__CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *ifail, 
	__CLPK_integer *info);
 
/* Subroutine */ int dsbgst_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *bb, __CLPK_integer *
	ldbb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dsbgv_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *bb, __CLPK_integer *
	ldbb, __CLPK_doublereal *w, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dsbgvd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *bb, __CLPK_integer *
	ldbb, __CLPK_doublereal *w, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dsbgvx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *ka, __CLPK_integer *kb, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *
	bb, __CLPK_integer *ldbb, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *vl, 
	__CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer 
	*m, __CLPK_doublereal *w, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int dsbtrd_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dspcon_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_integer *
	ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer 
	*iwork, __CLPK_integer *info);
 
/* Subroutine */ int dspev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_doublereal *
	ap, __CLPK_doublereal *w, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dspevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_doublereal *
	ap, __CLPK_doublereal *w, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dspevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_doublereal *ap, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *
	iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *ifail, 
	__CLPK_integer *info);
 
/* Subroutine */ int dspgst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, 
	__CLPK_doublereal *ap, __CLPK_doublereal *bp, __CLPK_integer *info);
 
/* Subroutine */ int dspgv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublereal *ap, __CLPK_doublereal *bp, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dspgvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublereal *ap, __CLPK_doublereal *bp, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dspgvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_doublereal *bp, __CLPK_doublereal *vl, 
	__CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer 
	*m, __CLPK_doublereal *w, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int dsprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *ap, __CLPK_doublereal *afp, __CLPK_integer *ipiv, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dspsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal 
	*ap, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dspsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *ap, __CLPK_doublereal *afp, __CLPK_integer *ipiv, __CLPK_doublereal *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, 
	__CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dsptrd_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *tau, __CLPK_integer *info);
 
/* Subroutine */ int dsptrf_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_integer *
	ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dsptri_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, __CLPK_integer *
	ipiv, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dsptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *ap, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int dstebz_(char *range, char *order, __CLPK_integer *n, __CLPK_doublereal 
	*vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_integer *m, __CLPK_integer *nsplit, 
	__CLPK_doublereal *w, __CLPK_integer *iblock, __CLPK_integer *isplit, __CLPK_doublereal *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dstedc_(char *compz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dstegr_(char *jobz, char *range, __CLPK_integer *n, __CLPK_doublereal *
	d__, __CLPK_doublereal *e, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, 
	__CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dstein_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_integer *m, __CLPK_doublereal *w, __CLPK_integer *iblock, __CLPK_integer *isplit, 
	__CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int dsteqr_(char *compz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dsterf_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_integer *info);
 
/* Subroutine */ int dstev_(char *jobz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int dstevd_(char *jobz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dstevr_(char *jobz, char *range, __CLPK_integer *n, __CLPK_doublereal *
	d__, __CLPK_doublereal *e, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, 
	__CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dstevx_(char *jobz, char *range, __CLPK_integer *n, __CLPK_doublereal *
	d__, __CLPK_doublereal *e, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, 
	__CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int dsycon_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dsyev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_doublereal *a,
	 __CLPK_integer *lda, __CLPK_doublereal *w, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dsyevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublereal *w, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dsyevr_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *
	il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dsyevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *
	il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *lwork, 
	__CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int dsygs2_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int dsygst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int dsygv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *w, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dsygvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *w, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int dsygvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer 
	*ldb, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, 
	__CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublereal *z__, 
	__CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int dsyrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *af, __CLPK_integer *ldaf, __CLPK_integer *
	ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dsysv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal 
	*a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dsysvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *af, __CLPK_integer *ldaf, 
	__CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *
	ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dsytd2_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *tau, __CLPK_integer *info);
 
/* Subroutine */ int dsytf2_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int dsytrd_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *tau, __CLPK_doublereal *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dsytrf_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dsytri_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_integer *ipiv, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dsytrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *
	ldb, __CLPK_integer *info);
 
/* Subroutine */ int dtbcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *rcond, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtbrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal 
	*b, __CLPK_integer *ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtbtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal 
	*b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int dtgevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *vl, __CLPK_integer *ldvl, __CLPK_doublereal *vr, __CLPK_integer *ldvr, __CLPK_integer 
	*mm, __CLPK_integer *m, __CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dtgex2_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *
	q, __CLPK_integer *ldq, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_integer *j1, __CLPK_integer *
	n1, __CLPK_integer *n2, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dtgexc_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *
	q, __CLPK_integer *ldq, __CLPK_doublereal *z__, __CLPK_integer *ldz, __CLPK_integer *ifst, 
	__CLPK_integer *ilst, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int dtgsen_(__CLPK_integer *ijob, __CLPK_logical *wantq, __CLPK_logical *wantz, 
	__CLPK_logical *select, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *
	b, __CLPK_integer *ldb, __CLPK_doublereal *alphar, __CLPK_doublereal *alphai, __CLPK_doublereal *
	beta, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_doublereal *z__, __CLPK_integer *ldz, 
	__CLPK_integer *m, __CLPK_doublereal *pl, __CLPK_doublereal *pr, __CLPK_doublereal *dif, 
	__CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dtgsja_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_doublereal *a, 
	__CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublereal *tola, 
	__CLPK_doublereal *tolb, __CLPK_doublereal *alpha, __CLPK_doublereal *beta, __CLPK_doublereal *u, 
	__CLPK_integer *ldu, __CLPK_doublereal *v, __CLPK_integer *ldv, __CLPK_doublereal *q, __CLPK_integer *
	ldq, __CLPK_doublereal *work, __CLPK_integer *ncycle, __CLPK_integer *info);
 
/* Subroutine */ int dtgsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *vl, __CLPK_integer *ldvl, __CLPK_doublereal *vr, __CLPK_integer *ldvr, 
	__CLPK_doublereal *s, __CLPK_doublereal *dif, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_doublereal *
	work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtgsy2_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *d__, __CLPK_integer *ldd, 
	__CLPK_doublereal *e, __CLPK_integer *lde, __CLPK_doublereal *f, __CLPK_integer *ldf, __CLPK_doublereal *
	scale, __CLPK_doublereal *rdsum, __CLPK_doublereal *rdscal, __CLPK_integer *iwork, __CLPK_integer 
	*pq, __CLPK_integer *info);
 
/* Subroutine */ int dtgsyl_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *d__, __CLPK_integer *ldd, 
	__CLPK_doublereal *e, __CLPK_integer *lde, __CLPK_doublereal *f, __CLPK_integer *ldf, __CLPK_doublereal *
	scale, __CLPK_doublereal *dif, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtpcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublereal *ap, __CLPK_doublereal *rcond, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int dtprfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublereal *ap, __CLPK_doublereal *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtptri_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_doublereal *
	ap, __CLPK_integer *info);
 
/* Subroutine */ int dtptrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublereal *ap, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int dtrcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *rcond, __CLPK_doublereal *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtrevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublereal *t, __CLPK_integer *ldt, __CLPK_doublereal *vl, __CLPK_integer *
	ldvl, __CLPK_doublereal *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *m, 
	__CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dtrexc_(char *compq, __CLPK_integer *n, __CLPK_doublereal *t, __CLPK_integer *
	ldt, __CLPK_doublereal *q, __CLPK_integer *ldq, __CLPK_integer *ifst, __CLPK_integer *ilst, 
	__CLPK_doublereal *work, __CLPK_integer *info);
 
/* Subroutine */ int dtrrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *
	ldb, __CLPK_doublereal *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtrsen_(char *job, char *compq, __CLPK_logical *select, __CLPK_integer 
	*n, __CLPK_doublereal *t, __CLPK_integer *ldt, __CLPK_doublereal *q, __CLPK_integer *ldq, 
	__CLPK_doublereal *wr, __CLPK_doublereal *wi, __CLPK_integer *m, __CLPK_doublereal *s, __CLPK_doublereal 
	*sep, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *
	liwork, __CLPK_integer *info);
 
/* Subroutine */ int dtrsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublereal *t, __CLPK_integer *ldt, __CLPK_doublereal *vl, __CLPK_integer *
	ldvl, __CLPK_doublereal *vr, __CLPK_integer *ldvr, __CLPK_doublereal *s, __CLPK_doublereal *sep, 
	__CLPK_integer *mm, __CLPK_integer *m, __CLPK_doublereal *work, __CLPK_integer *ldwork, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int dtrsyl_(char *trana, char *tranb, __CLPK_integer *isgn, __CLPK_integer 
	*m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *
	ldb, __CLPK_doublereal *c__, __CLPK_integer *ldc, __CLPK_doublereal *scale, __CLPK_integer *info);
 
/* Subroutine */ int dtrti2_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int dtrtri_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int dtrtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *
	ldb, __CLPK_integer *info);
 
/* Subroutine */ int dtzrqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_integer *info);
 
/* Subroutine */ int dtzrzf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublereal *tau, __CLPK_doublereal *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
__CLPK_integer icmax1_(__CLPK_integer *n, __CLPK_complex *cx, __CLPK_integer *incx);
 
__CLPK_integer ieeeck_(__CLPK_integer *ispec, __CLPK_real *zero, __CLPK_real *one);
 
__CLPK_integer ilaenv_(__CLPK_integer *ispec, char *name__, char *opts, __CLPK_integer *n1, 
	__CLPK_integer *n2, __CLPK_integer *n3, __CLPK_integer *n4, __CLPK_ftnlen name_len, __CLPK_ftnlen 
	opts_len);
 
__CLPK_integer izmax1_(__CLPK_integer *n, __CLPK_doublecomplex *cx, __CLPK_integer *incx);
 
/* Subroutine */ int sbdsdc_(char *uplo, char *compq, __CLPK_integer *n, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, __CLPK_integer *ldvt, __CLPK_real *q, 
	__CLPK_integer *iq, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sbdsqr_(char *uplo, __CLPK_integer *n, __CLPK_integer *ncvt, __CLPK_integer *
	nru, __CLPK_integer *ncc, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *vt, __CLPK_integer *ldvt, __CLPK_real *
	u, __CLPK_integer *ldu, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sdisna_(char *job, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *d__, 
	__CLPK_real *sep, __CLPK_integer *info);
 
/* Subroutine */ int sgbbrd_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *ncc,
	 __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *d__, __CLPK_real *
	e, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *pt, __CLPK_integer *ldpt, __CLPK_real *c__, __CLPK_integer 
	*ldc, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgbcon_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *rcond, 
	__CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgbequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real *
	colcnd, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int sgbrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *afb, __CLPK_integer *ldafb,
	 __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *
	ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgbsv_(__CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_integer *
	nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int sgbsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *kl,
	 __CLPK_integer *ku, __CLPK_integer *nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *afb, 
	__CLPK_integer *ldafb, __CLPK_integer *ipiv, char *equed, __CLPK_real *r__, __CLPK_real *c__, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr,
	 __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgbtf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int sgbtrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int sgbtrs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_real *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sgebak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_real *scale, __CLPK_integer *m, __CLPK_real *v, __CLPK_integer *ldv, __CLPK_integer 
	*info);
 
/* Subroutine */ int sgebal_(char *job, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *scale, __CLPK_integer *info);
 
/* Subroutine */ int sgebd2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *d__, __CLPK_real *e, __CLPK_real *tauq, __CLPK_real *taup, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgebrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *d__, __CLPK_real *e, __CLPK_real *tauq, __CLPK_real *taup, __CLPK_real *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgecon_(char *norm, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *anorm, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgeequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real *colcnd, __CLPK_real *amax, __CLPK_integer 
	*info);
 
/* Subroutine */ int sgees_(char *jobvs, char *sort, __CLPK_L_fp select, __CLPK_integer *n, 
	__CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *sdim, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real *vs, 
	__CLPK_integer *ldvs, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_logical *bwork, __CLPK_integer *
	info);
 
/* Subroutine */ int sgeesx_(char *jobvs, char *sort, __CLPK_L_fp select, char *
	sense, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *sdim, __CLPK_real *wr, 
	__CLPK_real *wi, __CLPK_real *vs, __CLPK_integer *ldvs, __CLPK_real *rconde, __CLPK_real *rcondv, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_logical *bwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int sgeev_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real *vl, __CLPK_integer *ldvl, __CLPK_real *vr, 
	__CLPK_integer *ldvr, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real *
	vl, __CLPK_integer *ldvl, __CLPK_real *vr, __CLPK_integer *ldvr, __CLPK_integer *ilo, __CLPK_integer *
	ihi, __CLPK_real *scale, __CLPK_real *abnrm, __CLPK_real *rconde, __CLPK_real *rcondv, __CLPK_real *work,
	 __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgegs_(char *jobvsl, char *jobvsr, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real 
	*beta, __CLPK_real *vsl, __CLPK_integer *ldvsl, __CLPK_real *vsr, __CLPK_integer *ldvsr, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgegv_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real 
	*beta, __CLPK_real *vl, __CLPK_integer *ldvl, __CLPK_real *vr, __CLPK_integer *ldvr, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgehd2_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgehrd_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgelq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgelqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgels_(char *trans, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgelsd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *s, __CLPK_real *rcond, __CLPK_integer *
	rank, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgelss_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *s, __CLPK_real *rcond, __CLPK_integer *
	rank, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgelsx_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *jpvt, __CLPK_real *rcond, 
	__CLPK_integer *rank, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgelsy_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *jpvt, __CLPK_real *rcond, 
	__CLPK_integer *rank, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgeql2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgeqlf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgeqp3_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *jpvt, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgeqpf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *jpvt, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgeqr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgeqrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgerfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, __CLPK_real *b, 
	__CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgerq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgerqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgesc2_(__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *rhs, 
	__CLPK_integer *ipiv, __CLPK_integer *jpiv, __CLPK_real *scale);
 
/* Subroutine */ int sgesdd_(char *jobz, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *s, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, __CLPK_integer *ldvt,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgesv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sgesvd_(char *jobu, char *jobvt, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *s, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, 
	__CLPK_integer *ldvt, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgesvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, 
	char *equed, __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, 
	__CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgetc2_(__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *ipiv,
	 __CLPK_integer *jpiv, __CLPK_integer *info);
 
/* Subroutine */ int sgetf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int sgetrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int sgetri_(__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *ipiv,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgetrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sggbak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_real *lscale, __CLPK_real *rscale, __CLPK_integer *m, __CLPK_real *v, 
	__CLPK_integer *ldv, __CLPK_integer *info);
 
/* Subroutine */ int sggbal_(char *job, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *lscale, __CLPK_real 
	*rscale, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgges_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	selctg, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, 
	__CLPK_integer *sdim, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real *beta, __CLPK_real *vsl, 
	__CLPK_integer *ldvsl, __CLPK_real *vsr, __CLPK_integer *ldvsr, __CLPK_real *work, __CLPK_integer *lwork,
	 __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int sggesx_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	selctg, char *sense, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, 
	__CLPK_integer *ldb, __CLPK_integer *sdim, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real *beta, 
	__CLPK_real *vsl, __CLPK_integer *ldvsl, __CLPK_real *vsr, __CLPK_integer *ldvsr, __CLPK_real *rconde, 
	__CLPK_real *rcondv, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *
	liwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int sggev_(char *jobvl, char *jobvr, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real 
	*beta, __CLPK_real *vl, __CLPK_integer *ldvl, __CLPK_real *vr, __CLPK_integer *ldvr, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real 
	*alphar, __CLPK_real *alphai, __CLPK_real *beta, __CLPK_real *vl, __CLPK_integer *ldvl, __CLPK_real *vr, 
	__CLPK_integer *ldvr, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *lscale, __CLPK_real *rscale,
	 __CLPK_real *abnrm, __CLPK_real *bbnrm, __CLPK_real *rconde, __CLPK_real *rcondv, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int sggglm_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *d__, __CLPK_real *x, __CLPK_real *y, 
	__CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sgghrd_(char *compq, char *compz, __CLPK_integer *n, __CLPK_integer *
	ilo, __CLPK_integer *ihi, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real 
	*q, __CLPK_integer *ldq, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_integer *info);
 
/* Subroutine */ int sgglse_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *p, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *c__, __CLPK_real *d__, __CLPK_real *x, 
	__CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sggqrf_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *taua, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *taub, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sggrqf_(__CLPK_integer *m, __CLPK_integer *p, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *taua, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *taub, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sggsvd_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *p, __CLPK_integer *k, __CLPK_integer *l, __CLPK_real *a, __CLPK_integer *lda,
	 __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *alpha, __CLPK_real *beta, __CLPK_real *u, __CLPK_integer *
	ldu, __CLPK_real *v, __CLPK_integer *ldv, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sggsvp_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, 
	__CLPK_real *tola, __CLPK_real *tolb, __CLPK_integer *k, __CLPK_integer *l, __CLPK_real *u, __CLPK_integer *ldu,
	 __CLPK_real *v, __CLPK_integer *ldv, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_integer *iwork, __CLPK_real *
	tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sgtcon_(char *norm, __CLPK_integer *n, __CLPK_real *dl, __CLPK_real *d__, 
	__CLPK_real *du, __CLPK_real *du2, __CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_real *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgtrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *dl,
	 __CLPK_real *d__, __CLPK_real *du, __CLPK_real *dlf, __CLPK_real *df, __CLPK_real *duf, __CLPK_real *du2, 
	__CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *
	ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sgtsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *dl, __CLPK_real *d__, 
	__CLPK_real *du, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sgtsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *dl, __CLPK_real *d__, __CLPK_real *du, __CLPK_real *dlf, __CLPK_real *df, __CLPK_real *duf, 
	__CLPK_real *du2, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *
	ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int sgttrf_(__CLPK_integer *n, __CLPK_real *dl, __CLPK_real *d__, __CLPK_real *du, __CLPK_real *
	du2, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int sgttrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *dl,
	 __CLPK_real *d__, __CLPK_real *du, __CLPK_real *du2, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb,
	 __CLPK_integer *info);
 
/* Subroutine */ int sgtts2_(__CLPK_integer *itrans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real 
	*dl, __CLPK_real *d__, __CLPK_real *du, __CLPK_real *du2, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *
	ldb);
 
/* Subroutine */ int shgeqz_(char *job, char *compq, char *compz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *
	ldb, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real *beta, __CLPK_real *q, __CLPK_integer *ldq, 
	__CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int shsein_(char *side, char *eigsrc, char *initv, __CLPK_logical *
	select, __CLPK_integer *n, __CLPK_real *h__, __CLPK_integer *ldh, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real 
	*vl, __CLPK_integer *ldvl, __CLPK_real *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *m, 
	__CLPK_real *work, __CLPK_integer *ifaill, __CLPK_integer *ifailr, __CLPK_integer *info);
 
/* Subroutine */ int shseqr_(char *job, char *compz, __CLPK_integer *n, __CLPK_integer *ilo,
	 __CLPK_integer *ihi, __CLPK_real *h__, __CLPK_integer *ldh, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real *z__,
	 __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int slabad_(__CLPK_real *small, __CLPK_real *large);
 
/* Subroutine */ int slabrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *tauq, __CLPK_real *taup, __CLPK_real *x, 
	__CLPK_integer *ldx, __CLPK_real *y, __CLPK_integer *ldy);
 
/* Subroutine */ int slacon_(__CLPK_integer *n, __CLPK_real *v, __CLPK_real *x, __CLPK_integer *isgn, 
	__CLPK_real *est, __CLPK_integer *kase);
 
/* Subroutine */ int slacpy_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb);
 
/* Subroutine */ int sladiv_(__CLPK_real *a, __CLPK_real *b, __CLPK_real *c__, __CLPK_real *d__, __CLPK_real *p, 
	__CLPK_real *q);
 
/* Subroutine */ int slae2_(__CLPK_real *a, __CLPK_real *b, __CLPK_real *c__, __CLPK_real *rt1, __CLPK_real *rt2);
 
/* Subroutine */ int slaebz_(__CLPK_integer *ijob, __CLPK_integer *nitmax, __CLPK_integer *n, 
	__CLPK_integer *mmax, __CLPK_integer *minp, __CLPK_integer *nbmin, __CLPK_real *abstol, __CLPK_real *
	reltol, __CLPK_real *pivmin, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *e2, __CLPK_integer *nval, 
	__CLPK_real *ab, __CLPK_real *c__, __CLPK_integer *mout, __CLPK_integer *nab, __CLPK_real *work, __CLPK_integer 
	*iwork, __CLPK_integer *info);
 
/* Subroutine */ int slaed0_(__CLPK_integer *icompq, __CLPK_integer *qsiz, __CLPK_integer *n, __CLPK_real 
	*d__, __CLPK_real *e, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *qstore, __CLPK_integer *ldqs, 
	__CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int slaed1_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *q, __CLPK_integer *ldq, 
	__CLPK_integer *indxq, __CLPK_real *rho, __CLPK_integer *cutpnt, __CLPK_real *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int slaed2_(__CLPK_integer *k, __CLPK_integer *n, __CLPK_integer *n1, __CLPK_real *d__, 
	__CLPK_real *q, __CLPK_integer *ldq, __CLPK_integer *indxq, __CLPK_real *rho, __CLPK_real *z__, __CLPK_real *
	dlamda, __CLPK_real *w, __CLPK_real *q2, __CLPK_integer *indx, __CLPK_integer *indxc, __CLPK_integer *
	indxp, __CLPK_integer *coltyp, __CLPK_integer *info);
 
/* Subroutine */ int slaed3_(__CLPK_integer *k, __CLPK_integer *n, __CLPK_integer *n1, __CLPK_real *d__, 
	__CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *rho, __CLPK_real *dlamda, __CLPK_real *q2, __CLPK_integer *
	indx, __CLPK_integer *ctot, __CLPK_real *w, __CLPK_real *s, __CLPK_integer *info);
 
/* Subroutine */ int slaed4_(__CLPK_integer *n, __CLPK_integer *i__, __CLPK_real *d__, __CLPK_real *z__, 
	__CLPK_real *delta, __CLPK_real *rho, __CLPK_real *dlam, __CLPK_integer *info);
 
/* Subroutine */ int slaed5_(__CLPK_integer *i__, __CLPK_real *d__, __CLPK_real *z__, __CLPK_real *delta, 
	__CLPK_real *rho, __CLPK_real *dlam);
 
/* Subroutine */ int slaed6_(__CLPK_integer *kniter, __CLPK_logical *orgati, __CLPK_real *rho, 
	__CLPK_real *d__, __CLPK_real *z__, __CLPK_real *finit, __CLPK_real *tau, __CLPK_integer *info);
 
/* Subroutine */ int slaed7_(__CLPK_integer *icompq, __CLPK_integer *n, __CLPK_integer *qsiz, 
	__CLPK_integer *tlvls, __CLPK_integer *curlvl, __CLPK_integer *curpbm, __CLPK_real *d__, __CLPK_real *q, 
	__CLPK_integer *ldq, __CLPK_integer *indxq, __CLPK_real *rho, __CLPK_integer *cutpnt, __CLPK_real *
	qstore, __CLPK_integer *qptr, __CLPK_integer *prmptr, __CLPK_integer *perm, __CLPK_integer *
	givptr, __CLPK_integer *givcol, __CLPK_real *givnum, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int slaed8_(__CLPK_integer *icompq, __CLPK_integer *k, __CLPK_integer *n, __CLPK_integer 
	*qsiz, __CLPK_real *d__, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_integer *indxq, __CLPK_real *rho, 
	__CLPK_integer *cutpnt, __CLPK_real *z__, __CLPK_real *dlamda, __CLPK_real *q2, __CLPK_integer *ldq2, 
	__CLPK_real *w, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_real *
	givnum, __CLPK_integer *indxp, __CLPK_integer *indx, __CLPK_integer *info);
 
/* Subroutine */ int slaed9_(__CLPK_integer *k, __CLPK_integer *kstart, __CLPK_integer *kstop, 
	__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *rho, __CLPK_real *dlamda,
	 __CLPK_real *w, __CLPK_real *s, __CLPK_integer *lds, __CLPK_integer *info);
 
/* Subroutine */ int slaeda_(__CLPK_integer *n, __CLPK_integer *tlvls, __CLPK_integer *curlvl, 
	__CLPK_integer *curpbm, __CLPK_integer *prmptr, __CLPK_integer *perm, __CLPK_integer *givptr, 
	__CLPK_integer *givcol, __CLPK_real *givnum, __CLPK_real *q, __CLPK_integer *qptr, __CLPK_real *z__, 
	__CLPK_real *ztemp, __CLPK_integer *info);
 
/* Subroutine */ int slaein_(__CLPK_logical *rightv, __CLPK_logical *noinit, __CLPK_integer *n, 
	__CLPK_real *h__, __CLPK_integer *ldh, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real *vr, __CLPK_real *vi, __CLPK_real 
	*b, __CLPK_integer *ldb, __CLPK_real *work, __CLPK_real *eps3, __CLPK_real *smlnum, __CLPK_real *bignum, 
	__CLPK_integer *info);
 
/* Subroutine */ int slaev2_(__CLPK_real *a, __CLPK_real *b, __CLPK_real *c__, __CLPK_real *rt1, __CLPK_real *
	rt2, __CLPK_real *cs1, __CLPK_real *sn1);
 
/* Subroutine */ int slaexc_(__CLPK_logical *wantq, __CLPK_integer *n, __CLPK_real *t, __CLPK_integer *
	ldt, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_integer *j1, __CLPK_integer *n1, __CLPK_integer *n2, 
	__CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int slag2_(__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, 
	__CLPK_real *safmin, __CLPK_real *scale1, __CLPK_real *scale2, __CLPK_real *wr1, __CLPK_real *wr2, __CLPK_real *
	wi);
 
/* Subroutine */ int slags2_(__CLPK_logical *upper, __CLPK_real *a1, __CLPK_real *a2, __CLPK_real *a3, 
	__CLPK_real *b1, __CLPK_real *b2, __CLPK_real *b3, __CLPK_real *csu, __CLPK_real *snu, __CLPK_real *csv, __CLPK_real *
	snv, __CLPK_real *csq, __CLPK_real *snq);
 
/* Subroutine */ int slagtf_(__CLPK_integer *n, __CLPK_real *a, __CLPK_real *lambda, __CLPK_real *b, __CLPK_real 
	*c__, __CLPK_real *tol, __CLPK_real *d__, __CLPK_integer *in, __CLPK_integer *info);
 
/* Subroutine */ int slagtm_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *
	alpha, __CLPK_real *dl, __CLPK_real *d__, __CLPK_real *du, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *
	beta, __CLPK_real *b, __CLPK_integer *ldb);
 
/* Subroutine */ int slagts_(__CLPK_integer *job, __CLPK_integer *n, __CLPK_real *a, __CLPK_real *b, __CLPK_real 
	*c__, __CLPK_real *d__, __CLPK_integer *in, __CLPK_real *y, __CLPK_real *tol, __CLPK_integer *info);
 
/* Subroutine */ int slagv2_(__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, 
	__CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real *beta, __CLPK_real *csl, __CLPK_real *snl, __CLPK_real *
	csr, __CLPK_real *snr);
 
/* Subroutine */ int slahqr_(__CLPK_logical *wantt, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *h__, __CLPK_integer *ldh, __CLPK_real *wr, __CLPK_real *
	wi, __CLPK_integer *iloz, __CLPK_integer *ihiz, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_integer *
	info);
 
/* Subroutine */ int slahrd_(__CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *nb, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *t, __CLPK_integer *ldt, __CLPK_real *y, __CLPK_integer *ldy);
 
/* Subroutine */ int slaic1_(__CLPK_integer *job, __CLPK_integer *j, __CLPK_real *x, __CLPK_real *sest, 
	__CLPK_real *w, __CLPK_real *gamma, __CLPK_real *sestpr, __CLPK_real *s, __CLPK_real *c__);
 
/* Subroutine */ int slaln2_(__CLPK_logical *ltrans, __CLPK_integer *na, __CLPK_integer *nw, __CLPK_real *
	smin, __CLPK_real *ca, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *d1, __CLPK_real *d2, __CLPK_real *b, 
	__CLPK_integer *ldb, __CLPK_real *wr, __CLPK_real *wi, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *scale, 
	__CLPK_real *xnorm, __CLPK_integer *info);
 
/* Subroutine */ int slals0_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_integer *nrhs, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *bx, 
	__CLPK_integer *ldbx, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *givcol, 
	__CLPK_integer *ldgcol, __CLPK_real *givnum, __CLPK_integer *ldgnum, __CLPK_real *poles, __CLPK_real *
	difl, __CLPK_real *difr, __CLPK_real *z__, __CLPK_integer *k, __CLPK_real *c__, __CLPK_real *s, __CLPK_real *
	work, __CLPK_integer *info);
 
/* Subroutine */ int slalsa_(__CLPK_integer *icompq, __CLPK_integer *smlsiz, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *bx, __CLPK_integer *ldbx, __CLPK_real *
	u, __CLPK_integer *ldu, __CLPK_real *vt, __CLPK_integer *k, __CLPK_real *difl, __CLPK_real *difr, __CLPK_real *
	z__, __CLPK_real *poles, __CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, 
	__CLPK_integer *perm, __CLPK_real *givnum, __CLPK_real *c__, __CLPK_real *s, __CLPK_real *work, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int slalsd_(char *uplo, __CLPK_integer *smlsiz, __CLPK_integer *n, __CLPK_integer 
	*nrhs, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *rcond, 
	__CLPK_integer *rank, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int slamc1_(__CLPK_integer *beta, __CLPK_integer *t, __CLPK_logical *rnd, __CLPK_logical 
	*ieee1);
 
/* Subroutine */ int slamc2_(__CLPK_integer *beta, __CLPK_integer *t, __CLPK_logical *rnd, __CLPK_real *
	eps, __CLPK_integer *emin, __CLPK_real *rmin, __CLPK_integer *emax, __CLPK_real *rmax);
 
/* Subroutine */ int slamc4_(__CLPK_integer *emin, __CLPK_real *start, __CLPK_integer *base);
 
/* Subroutine */ int slamc5_(__CLPK_integer *beta, __CLPK_integer *p, __CLPK_integer *emin, 
	__CLPK_logical *ieee, __CLPK_integer *emax, __CLPK_real *rmax);
 
/* Subroutine */ int slamrg_(__CLPK_integer *n1, __CLPK_integer *n2, __CLPK_real *a, __CLPK_integer *
	strd1, __CLPK_integer *strd2, __CLPK_integer *index);
 
/* Subroutine */ int slanv2_(__CLPK_real *a, __CLPK_real *b, __CLPK_real *c__, __CLPK_real *d__, __CLPK_real *
	rt1r, __CLPK_real *rt1i, __CLPK_real *rt2r, __CLPK_real *rt2i, __CLPK_real *cs, __CLPK_real *sn);
 
/* Subroutine */ int slapll_(__CLPK_integer *n, __CLPK_real *x, __CLPK_integer *incx, __CLPK_real *y, 
	__CLPK_integer *incy, __CLPK_real *ssmin);
 
/* Subroutine */ int slapmt_(__CLPK_logical *forwrd, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *x,
	 __CLPK_integer *ldx, __CLPK_integer *k);
 
/* Subroutine */ int slaqgb_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real *
	colcnd, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int slaqge_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *r__, __CLPK_real *c__, __CLPK_real *rowcnd, __CLPK_real *colcnd, __CLPK_real *amax, char *
	equed);
 
/* Subroutine */ int slaqp2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, __CLPK_real *a,
	 __CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_real *tau, __CLPK_real *vn1, __CLPK_real *vn2, __CLPK_real *
	work);
 
/* Subroutine */ int slaqps_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, __CLPK_integer 
	*nb, __CLPK_integer *kb, __CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_real *tau, 
	__CLPK_real *vn1, __CLPK_real *vn2, __CLPK_real *auxv, __CLPK_real *f, __CLPK_integer *ldf);
 
/* Subroutine */ int slaqsb_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int slaqsp_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *s, __CLPK_real *
	scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int slaqsy_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, char *equed);
 
/* Subroutine */ int slaqtr_(__CLPK_logical *ltran, __CLPK_logical *lreal, __CLPK_integer *n, __CLPK_real 
	*t, __CLPK_integer *ldt, __CLPK_real *b, __CLPK_real *w, __CLPK_real *scale, __CLPK_real *x, __CLPK_real *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int slar1v_(__CLPK_integer *n, __CLPK_integer *b1, __CLPK_integer *bn, __CLPK_real *
	sigma, __CLPK_real *d__, __CLPK_real *l, __CLPK_real *ld, __CLPK_real *lld, __CLPK_real *gersch, __CLPK_real *
	z__, __CLPK_real *ztz, __CLPK_real *mingma, __CLPK_integer *r__, __CLPK_integer *isuppz, __CLPK_real *
	work);
 
/* Subroutine */ int slar2v_(__CLPK_integer *n, __CLPK_real *x, __CLPK_real *y, __CLPK_real *z__, __CLPK_integer 
	*incx, __CLPK_real *c__, __CLPK_real *s, __CLPK_integer *incc);
 
/* Subroutine */ int slarf_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *v, 
	__CLPK_integer *incv, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *work);
 
/* Subroutine */ int slarfb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *v, __CLPK_integer *ldv, 
	__CLPK_real *t, __CLPK_integer *ldt, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *work, __CLPK_integer *
	ldwork);
 
/* Subroutine */ int slarfg_(__CLPK_integer *n, __CLPK_real *alpha, __CLPK_real *x, __CLPK_integer *incx, 
	__CLPK_real *tau);
 
/* Subroutine */ int slarft_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_real *v, __CLPK_integer *ldv, __CLPK_real *tau, __CLPK_real *t, __CLPK_integer *ldt);
 
/* Subroutine */ int slarfx_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *v, 
	__CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *work);
 
/* Subroutine */ int slargv_(__CLPK_integer *n, __CLPK_real *x, __CLPK_integer *incx, __CLPK_real *y, 
	__CLPK_integer *incy, __CLPK_real *c__, __CLPK_integer *incc);
 
/* Subroutine */ int slarnv_(__CLPK_integer *idist, __CLPK_integer *iseed, __CLPK_integer *n, __CLPK_real 
	*x);
 
/* Subroutine */ int slarrb_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *l, __CLPK_real *ld, __CLPK_real *
	lld, __CLPK_integer *ifirst, __CLPK_integer *ilast, __CLPK_real *sigma, __CLPK_real *reltol, __CLPK_real 
	*w, __CLPK_real *wgap, __CLPK_real *werr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int slarre_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *tol, 
	__CLPK_integer *nsplit, __CLPK_integer *isplit, __CLPK_integer *m, __CLPK_real *w, __CLPK_real *woff, 
	__CLPK_real *gersch, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int slarrf_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *l, __CLPK_real *ld, __CLPK_real *
	lld, __CLPK_integer *ifirst, __CLPK_integer *ilast, __CLPK_real *w, __CLPK_real *dplus, __CLPK_real *
	lplus, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int slarrv_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *l, __CLPK_integer *isplit, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_integer *iblock, __CLPK_real *gersch, __CLPK_real *tol, __CLPK_real *
	z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int slartg_(__CLPK_real *f, __CLPK_real *g, __CLPK_real *cs, __CLPK_real *sn, __CLPK_real *r__);
 
/* Subroutine */ int slartv_(__CLPK_integer *n, __CLPK_real *x, __CLPK_integer *incx, __CLPK_real *y, 
	__CLPK_integer *incy, __CLPK_real *c__, __CLPK_real *s, __CLPK_integer *incc);
 
/* Subroutine */ int slaruv_(__CLPK_integer *iseed, __CLPK_integer *n, __CLPK_real *x);
 
/* Subroutine */ int slarz_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, 
	__CLPK_real *v, __CLPK_integer *incv, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *
	work);
 
/* Subroutine */ int slarzb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_real *v, 
	__CLPK_integer *ldv, __CLPK_real *t, __CLPK_integer *ldt, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *
	work, __CLPK_integer *ldwork);
 
/* Subroutine */ int slarzt_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_real *v, __CLPK_integer *ldv, __CLPK_real *tau, __CLPK_real *t, __CLPK_integer *ldt);
 
/* Subroutine */ int slas2_(__CLPK_real *f, __CLPK_real *g, __CLPK_real *h__, __CLPK_real *ssmin, __CLPK_real *
	ssmax);
 
/* Subroutine */ int slascl_(char *type__, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_real *
	cfrom, __CLPK_real *cto, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int slasd0_(__CLPK_integer *n, __CLPK_integer *sqre, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, __CLPK_integer *ldvt, __CLPK_integer *smlsiz, 
	__CLPK_integer *iwork, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int slasd1_(__CLPK_integer *nl, __CLPK_integer *nr, __CLPK_integer *sqre, __CLPK_real *
	d__, __CLPK_real *alpha, __CLPK_real *beta, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, 
	__CLPK_integer *ldvt, __CLPK_integer *idxq, __CLPK_integer *iwork, __CLPK_real *work, __CLPK_integer *
	info);
 
/* Subroutine */ int slasd2_(__CLPK_integer *nl, __CLPK_integer *nr, __CLPK_integer *sqre, __CLPK_integer 
	*k, __CLPK_real *d__, __CLPK_real *z__, __CLPK_real *alpha, __CLPK_real *beta, __CLPK_real *u, __CLPK_integer *
	ldu, __CLPK_real *vt, __CLPK_integer *ldvt, __CLPK_real *dsigma, __CLPK_real *u2, __CLPK_integer *ldu2, 
	__CLPK_real *vt2, __CLPK_integer *ldvt2, __CLPK_integer *idxp, __CLPK_integer *idx, __CLPK_integer *idxc,
	 __CLPK_integer *idxq, __CLPK_integer *coltyp, __CLPK_integer *info);
 
/* Subroutine */ int slasd3_(__CLPK_integer *nl, __CLPK_integer *nr, __CLPK_integer *sqre, __CLPK_integer 
	*k, __CLPK_real *d__, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *dsigma, __CLPK_real *u, __CLPK_integer *
	ldu, __CLPK_real *u2, __CLPK_integer *ldu2, __CLPK_real *vt, __CLPK_integer *ldvt, __CLPK_real *vt2, 
	__CLPK_integer *ldvt2, __CLPK_integer *idxc, __CLPK_integer *ctot, __CLPK_real *z__, __CLPK_integer *
	info);
 
/* Subroutine */ int slasd4_(__CLPK_integer *n, __CLPK_integer *i__, __CLPK_real *d__, __CLPK_real *z__, 
	__CLPK_real *delta, __CLPK_real *rho, __CLPK_real *sigma, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int slasd5_(__CLPK_integer *i__, __CLPK_real *d__, __CLPK_real *z__, __CLPK_real *delta, 
	__CLPK_real *rho, __CLPK_real *dsigma, __CLPK_real *work);
 
/* Subroutine */ int slasd6_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_real *d__, __CLPK_real *vf, __CLPK_real *vl, __CLPK_real *alpha, __CLPK_real *beta,
	 __CLPK_integer *idxq, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *givcol, 
	__CLPK_integer *ldgcol, __CLPK_real *givnum, __CLPK_integer *ldgnum, __CLPK_real *poles, __CLPK_real *
	difl, __CLPK_real *difr, __CLPK_real *z__, __CLPK_integer *k, __CLPK_real *c__, __CLPK_real *s, __CLPK_real *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int slasd7_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_integer *k, __CLPK_real *d__, __CLPK_real *z__, __CLPK_real *zw, __CLPK_real *vf, 
	__CLPK_real *vfw, __CLPK_real *vl, __CLPK_real *vlw, __CLPK_real *alpha, __CLPK_real *beta, __CLPK_real *dsigma,
	 __CLPK_integer *idx, __CLPK_integer *idxp, __CLPK_integer *idxq, __CLPK_integer *perm, __CLPK_integer *
	givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_real *givnum, __CLPK_integer *
	ldgnum, __CLPK_real *c__, __CLPK_real *s, __CLPK_integer *info);
 
/* Subroutine */ int slasd8_(__CLPK_integer *icompq, __CLPK_integer *k, __CLPK_real *d__, __CLPK_real *
	z__, __CLPK_real *vf, __CLPK_real *vl, __CLPK_real *difl, __CLPK_real *difr, __CLPK_integer *lddifr, 
	__CLPK_real *dsigma, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int slasd9_(__CLPK_integer *icompq, __CLPK_integer *ldu, __CLPK_integer *k, __CLPK_real *
	d__, __CLPK_real *z__, __CLPK_real *vf, __CLPK_real *vl, __CLPK_real *difl, __CLPK_real *difr, __CLPK_real *
	dsigma, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int slasda_(__CLPK_integer *icompq, __CLPK_integer *smlsiz, __CLPK_integer *n, 
	__CLPK_integer *sqre, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *vt, 
	__CLPK_integer *k, __CLPK_real *difl, __CLPK_real *difr, __CLPK_real *z__, __CLPK_real *poles, __CLPK_integer *
	givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_integer *perm, __CLPK_real *givnum,
	 __CLPK_real *c__, __CLPK_real *s, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int slasdq_(char *uplo, __CLPK_integer *sqre, __CLPK_integer *n, __CLPK_integer *
	ncvt, __CLPK_integer *nru, __CLPK_integer *ncc, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *vt, 
	__CLPK_integer *ldvt, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *
	work, __CLPK_integer *info);
 
/* Subroutine */ int slasdt_(__CLPK_integer *n, __CLPK_integer *lvl, __CLPK_integer *nd, __CLPK_integer *
	inode, __CLPK_integer *ndiml, __CLPK_integer *ndimr, __CLPK_integer *msub);
 
/* Subroutine */ int slaset_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *alpha, 
	__CLPK_real *beta, __CLPK_real *a, __CLPK_integer *lda);
 
/* Subroutine */ int slasq1_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int slasq2_(__CLPK_integer *n, __CLPK_real *z__, __CLPK_integer *info);
 
/* Subroutine */ int slasq3_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_real *z__, __CLPK_integer *pp,
	 __CLPK_real *dmin__, __CLPK_real *sigma, __CLPK_real *desig, __CLPK_real *qmax, __CLPK_integer *nfail, 
	__CLPK_integer *iter, __CLPK_integer *ndiv, __CLPK_logical *ieee);
 
/* Subroutine */ int slasq4_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_real *z__, __CLPK_integer *pp,
	 __CLPK_integer *n0in, __CLPK_real *dmin__, __CLPK_real *dmin1, __CLPK_real *dmin2, __CLPK_real *dn, 
	__CLPK_real *dn1, __CLPK_real *dn2, __CLPK_real *tau, __CLPK_integer *ttype);
 
/* Subroutine */ int slasq5_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_real *z__, __CLPK_integer *pp,
	 __CLPK_real *tau, __CLPK_real *dmin__, __CLPK_real *dmin1, __CLPK_real *dmin2, __CLPK_real *dn, __CLPK_real *
	dnm1, __CLPK_real *dnm2, __CLPK_logical *ieee);
 
/* Subroutine */ int slasq6_(__CLPK_integer *i0, __CLPK_integer *n0, __CLPK_real *z__, __CLPK_integer *pp,
	 __CLPK_real *dmin__, __CLPK_real *dmin1, __CLPK_real *dmin2, __CLPK_real *dn, __CLPK_real *dnm1, __CLPK_real *
	dnm2);
 
/* Subroutine */ int slasr_(char *side, char *pivot, char *direct, __CLPK_integer *m,
	 __CLPK_integer *n, __CLPK_real *c__, __CLPK_real *s, __CLPK_real *a, __CLPK_integer *lda);
 
/* Subroutine */ int slasrt_(char *id, __CLPK_integer *n, __CLPK_real *d__, __CLPK_integer *info);
 
/* Subroutine */ int slassq_(__CLPK_integer *n, __CLPK_real *x, __CLPK_integer *incx, __CLPK_real *scale, 
	__CLPK_real *sumsq);
 
/* Subroutine */ int slasv2_(__CLPK_real *f, __CLPK_real *g, __CLPK_real *h__, __CLPK_real *ssmin, __CLPK_real *
	ssmax, __CLPK_real *snr, __CLPK_real *csr, __CLPK_real *snl, __CLPK_real *csl);
 
/* Subroutine */ int slaswp_(__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *k1, 
	__CLPK_integer *k2, __CLPK_integer *ipiv, __CLPK_integer *incx);
 
/* Subroutine */ int slasy2_(__CLPK_logical *ltranl, __CLPK_logical *ltranr, __CLPK_integer *isgn, 
	__CLPK_integer *n1, __CLPK_integer *n2, __CLPK_real *tl, __CLPK_integer *ldtl, __CLPK_real *tr, __CLPK_integer *
	ldtr, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *scale, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real 
	*xnorm, __CLPK_integer *info);
 
/* Subroutine */ int slasyf_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_integer *kb,
	 __CLPK_real *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_real *w, __CLPK_integer *ldw, __CLPK_integer 
	*info);
 
/* Subroutine */ int slatbs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *x, 
	__CLPK_real *scale, __CLPK_real *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int slatdf_(__CLPK_integer *ijob, __CLPK_integer *n, __CLPK_real *z__, __CLPK_integer *
	ldz, __CLPK_real *rhs, __CLPK_real *rdsum, __CLPK_real *rdscal, __CLPK_integer *ipiv, __CLPK_integer *
	jpiv);
 
/* Subroutine */ int slatps_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *x, __CLPK_real *scale, __CLPK_real *cnorm, 
	__CLPK_integer *info);
 
/* Subroutine */ int slatrd_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *e, __CLPK_real *tau, __CLPK_real *w, __CLPK_integer *ldw);
 
/* Subroutine */ int slatrs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *x, __CLPK_real *scale, __CLPK_real 
	*cnorm, __CLPK_integer *info);
 
/* Subroutine */ int slatrz_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work);
 
/* Subroutine */ int slatzm_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *v, 
	__CLPK_integer *incv, __CLPK_real *tau, __CLPK_real *c1, __CLPK_real *c2, __CLPK_integer *ldc, __CLPK_real *
	work);
 
/* Subroutine */ int slauu2_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int slauum_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int sopgtr_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *tau, 
	__CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sopmtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_real *ap, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc, __CLPK_real *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int sorg2l_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sorg2r_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sorgbr_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer 
	*info);
 
/* Subroutine */ int sorghr_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorgl2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sorglq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorgql_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorgqr_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorgr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sorgrq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorgtr_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorm2l_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sorm2r_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sormbr_(char *vect, char *side, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, 
	__CLPK_integer *ldc, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sormhr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *
	c__, __CLPK_integer *ldc, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sorml2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sormlq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sormql_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sormqr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sormr2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sormr3_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, 
	__CLPK_integer *ldc, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sormrq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sormrz_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, 
	__CLPK_integer *ldc, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int sormtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *tau, __CLPK_real *c__, __CLPK_integer *ldc,
	 __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int spbcon_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int spbequ_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_real *s, __CLPK_real *scond, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int spbrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *afb, __CLPK_integer *ldafb, __CLPK_real *b, 
	__CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int spbstf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int spbsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int spbsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_integer *nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *afb, __CLPK_integer *ldafb, 
	char *equed, __CLPK_real *s, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, 
	__CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int spbtf2_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int spbtrf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int spbtrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int spocon_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *anorm, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int spoequ_(__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *s, __CLPK_real 
	*scond, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int sporfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *af, __CLPK_integer *ldaf, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x,
	 __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int sposv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sposvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *af, __CLPK_integer *ldaf, char *equed, 
	__CLPK_real *s, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *rcond, 
	__CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int spotf2_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int spotrf_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int spotri_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *info);
 
/* Subroutine */ int spotrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sppcon_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *anorm, 
	__CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sppequ_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *s, __CLPK_real *
	scond, __CLPK_real *amax, __CLPK_integer *info);
 
/* Subroutine */ int spprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *ap, 
	__CLPK_real *afp, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *ferr, 
	__CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sppsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *ap, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sppsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *ap, __CLPK_real *afp, char *equed, __CLPK_real *s, __CLPK_real *b, __CLPK_integer *
	ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real 
	*work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int spptrf_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_integer *info);
 
/* Subroutine */ int spptri_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_integer *info);
 
/* Subroutine */ int spptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *ap, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sptcon_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *anorm, 
	__CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int spteqr_(char *compz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sptrfs_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *df, __CLPK_real *ef, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, 
	__CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sptsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sptsvx_(char *fact, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__,
	 __CLPK_real *e, __CLPK_real *df, __CLPK_real *ef, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer 
	*ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int spttrf_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_integer *info);
 
/* Subroutine */ int spttrs_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sptts2_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *b, __CLPK_integer *ldb);
 
/* Subroutine */ int srscl_(__CLPK_integer *n, __CLPK_real *sa, __CLPK_real *sx, __CLPK_integer *incx);
 
/* Subroutine */ int ssbev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work,
	 __CLPK_integer *info);
 
/* Subroutine */ int ssbevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work,
	 __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int ssbevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *vl,
	 __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *
	w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *
	ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssbgst_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *bb, __CLPK_integer *ldbb, __CLPK_real *
	x, __CLPK_integer *ldx, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int ssbgv_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *bb, __CLPK_integer *ldbb, __CLPK_real *
	w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int ssbgvd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *bb, __CLPK_integer *ldbb, __CLPK_real *
	w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *
	iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int ssbgvx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *ka, __CLPK_integer *kb, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *bb, __CLPK_integer *
	ldbb, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer 
	*iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real 
	*work, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssbtrd_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *q, __CLPK_integer *ldq, 
	__CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sspcon_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_integer *ipiv, 
	__CLPK_real *anorm, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sspev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_real *ap, 
	__CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sspevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_real *ap, 
	__CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer 
	*iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int sspevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_real *ap, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *
	iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int sspgst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, __CLPK_real *ap,
	 __CLPK_real *bp, __CLPK_integer *info);
 
/* Subroutine */ int sspgv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_real *ap, __CLPK_real *bp, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int sspgvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_real *ap, __CLPK_real *bp, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int sspgvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *bp, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il,
	 __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *
	ldz, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *ap, 
	__CLPK_real *afp, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *
	ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int sspsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *ap, 
	__CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sspsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *ap, __CLPK_real *afp, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real 
	*x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int ssptrd_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_real *tau, __CLPK_integer *info);
 
/* Subroutine */ int ssptrf_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_integer *ipiv, 
	__CLPK_integer *info);
 
/* Subroutine */ int ssptri_(char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_integer *ipiv, 
	__CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int ssptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *ap, 
	__CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int sstebz_(char *range, char *order, __CLPK_integer *n, __CLPK_real *vl, 
	__CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_integer *m, __CLPK_integer *nsplit, __CLPK_real *w, __CLPK_integer *iblock, __CLPK_integer *
	isplit, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int sstedc_(char *compz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int sstegr_(char *jobz, char *range, __CLPK_integer *n, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int sstein_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_integer *m, __CLPK_real 
	*w, __CLPK_integer *iblock, __CLPK_integer *isplit, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *
	work, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssteqr_(char *compz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int ssterf_(__CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_integer *info);
 
/* Subroutine */ int sstev_(char *jobz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_real *
	z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int sstevd_(char *jobz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, __CLPK_real 
	*z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int sstevr_(char *jobz, char *range, __CLPK_integer *n, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int sstevx_(char *jobz, char *range, __CLPK_integer *n, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *
	iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssycon_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_real *anorm, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ssyev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *w, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int ssyevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *w, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int ssyevr_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, 
	__CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_integer *
	isuppz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ssyevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, 
	__CLPK_real *abstol, __CLPK_integer *m, __CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *
	work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssygs2_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ssygst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ssygv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *w, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int ssygvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *w, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int ssygvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *
	vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, __CLPK_integer *m, 
	__CLPK_real *w, __CLPK_real *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer 
	*iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int ssyrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_real *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, __CLPK_real *b, 
	__CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *
	work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int ssysv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int ssysvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *af, __CLPK_integer *ldaf, __CLPK_integer *ipiv, 
	__CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *rcond, __CLPK_real *ferr,
	 __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int ssytd2_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *d__, __CLPK_real *e, __CLPK_real *tau, __CLPK_integer *info);
 
/* Subroutine */ int ssytf2_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int ssytrd_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *d__, __CLPK_real *e, __CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int ssytrf_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int ssytri_(char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int ssytrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int stbcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *rcond, __CLPK_real *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int stbrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *b, __CLPK_integer 
	*ldb, __CLPK_real *x, __CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, 
	__CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int stbtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *b, __CLPK_integer 
	*ldb, __CLPK_integer *info);
 
/* Subroutine */ int stgevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *vl, 
	__CLPK_integer *ldvl, __CLPK_real *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_real 
	*work, __CLPK_integer *info);
 
/* Subroutine */ int stgex2_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, __CLPK_real 
	*a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *
	z__, __CLPK_integer *ldz, __CLPK_integer *j1, __CLPK_integer *n1, __CLPK_integer *n2, __CLPK_real *work, 
	__CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int stgexc_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, __CLPK_real 
	*a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *
	z__, __CLPK_integer *ldz, __CLPK_integer *ifst, __CLPK_integer *ilst, __CLPK_real *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int stgsen_(__CLPK_integer *ijob, __CLPK_logical *wantq, __CLPK_logical *wantz, 
	__CLPK_logical *select, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *
	ldb, __CLPK_real *alphar, __CLPK_real *alphai, __CLPK_real *beta, __CLPK_real *q, __CLPK_integer *ldq, 
	__CLPK_real *z__, __CLPK_integer *ldz, __CLPK_integer *m, __CLPK_real *pl, __CLPK_real *pr, __CLPK_real *dif, 
	__CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *
	info);
 
/* Subroutine */ int stgsja_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_real *a, __CLPK_integer *lda,
	 __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *tola, __CLPK_real *tolb, __CLPK_real *alpha, __CLPK_real *
	beta, __CLPK_real *u, __CLPK_integer *ldu, __CLPK_real *v, __CLPK_integer *ldv, __CLPK_real *q, __CLPK_integer *
	ldq, __CLPK_real *work, __CLPK_integer *ncycle, __CLPK_integer *info);
 
/* Subroutine */ int stgsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *vl, 
	__CLPK_integer *ldvl, __CLPK_real *vr, __CLPK_integer *ldvr, __CLPK_real *s, __CLPK_real *dif, __CLPK_integer *
	mm, __CLPK_integer *m, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *
	info);
 
/* Subroutine */ int stgsy2_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *c__, __CLPK_integer *
	ldc, __CLPK_real *d__, __CLPK_integer *ldd, __CLPK_real *e, __CLPK_integer *lde, __CLPK_real *f, __CLPK_integer 
	*ldf, __CLPK_real *scale, __CLPK_real *rdsum, __CLPK_real *rdscal, __CLPK_integer *iwork, __CLPK_integer 
	*pq, __CLPK_integer *info);
 
/* Subroutine */ int stgsyl_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *c__, __CLPK_integer *
	ldc, __CLPK_real *d__, __CLPK_integer *ldd, __CLPK_real *e, __CLPK_integer *lde, __CLPK_real *f, __CLPK_integer 
	*ldf, __CLPK_real *scale, __CLPK_real *dif, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *
	iwork, __CLPK_integer *info);
 
/* Subroutine */ int stpcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_real *ap, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int stprfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_real *ap, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, __CLPK_integer *ldx,
	 __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int stptri_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_real *ap, 
	__CLPK_integer *info);
 
/* Subroutine */ int stptrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_real *ap, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int strcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_real *a, __CLPK_integer *lda, __CLPK_real *rcond, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int strevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_real *t, __CLPK_integer *ldt, __CLPK_real *vl, __CLPK_integer *ldvl, __CLPK_real *vr, 
	__CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_real *work, __CLPK_integer *info);
 
/* Subroutine */ int strexc_(char *compq, __CLPK_integer *n, __CLPK_real *t, __CLPK_integer *ldt, 
	__CLPK_real *q, __CLPK_integer *ldq, __CLPK_integer *ifst, __CLPK_integer *ilst, __CLPK_real *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int strrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *x, 
	__CLPK_integer *ldx, __CLPK_real *ferr, __CLPK_real *berr, __CLPK_real *work, __CLPK_integer *iwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int strsen_(char *job, char *compq, __CLPK_logical *select, __CLPK_integer 
	*n, __CLPK_real *t, __CLPK_integer *ldt, __CLPK_real *q, __CLPK_integer *ldq, __CLPK_real *wr, __CLPK_real *wi, 
	__CLPK_integer *m, __CLPK_real *s, __CLPK_real *sep, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *
	iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int strsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_real *t, __CLPK_integer *ldt, __CLPK_real *vl, __CLPK_integer *ldvl, __CLPK_real *vr, 
	__CLPK_integer *ldvr, __CLPK_real *s, __CLPK_real *sep, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_real *
	work, __CLPK_integer *ldwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int strsyl_(char *trana, char *tranb, __CLPK_integer *isgn, __CLPK_integer 
	*m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_real *
	c__, __CLPK_integer *ldc, __CLPK_real *scale, __CLPK_integer *info);
 
/* Subroutine */ int strti2_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int strtri_(char *uplo, char *diag, __CLPK_integer *n, __CLPK_real *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int strtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int stzrqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_integer *info);
 
/* Subroutine */ int stzrzf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *tau, __CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int xerbla_(char *srname, __CLPK_integer *info);
 
/* Subroutine */ int zbdsqr_(char *uplo, __CLPK_integer *n, __CLPK_integer *ncvt, __CLPK_integer *
	nru, __CLPK_integer *ncc, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *vt, 
	__CLPK_integer *ldvt, __CLPK_doublecomplex *u, __CLPK_integer *ldu, __CLPK_doublecomplex *c__, 
	__CLPK_integer *ldc, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zdrot_(__CLPK_integer *n, __CLPK_doublecomplex *cx, __CLPK_integer *incx, 
	__CLPK_doublecomplex *cy, __CLPK_integer *incy, __CLPK_doublereal *c__, __CLPK_doublereal *s);
 
/* Subroutine */ int zdrscl_(__CLPK_integer *n, __CLPK_doublereal *sa, __CLPK_doublecomplex *sx, 
	__CLPK_integer *incx);
 
/* Subroutine */ int zgbbrd_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *ncc,
	 __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *q, __CLPK_integer *ldq, 
	__CLPK_doublecomplex *pt, __CLPK_integer *ldpt, __CLPK_doublecomplex *c__, __CLPK_integer *ldc, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgbcon_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_doublereal *anorm, 
	__CLPK_doublereal *rcond, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zgbequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *r__, __CLPK_doublereal *c__, 
	__CLPK_doublereal *rowcnd, __CLPK_doublereal *colcnd, __CLPK_doublereal *amax, __CLPK_integer *
	info);
 
/* Subroutine */ int zgbrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *
	afb, __CLPK_integer *ldafb, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgbsv_(__CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_doublecomplex *
	b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zgbsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *kl,
	 __CLPK_integer *ku, __CLPK_integer *nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, 
	__CLPK_doublecomplex *afb, __CLPK_integer *ldafb, __CLPK_integer *ipiv, char *equed, 
	__CLPK_doublereal *r__, __CLPK_doublereal *c__, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zgbtf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zgbtrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zgbtrs_(char *trans, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *
	ku, __CLPK_integer *nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *ipiv, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zgebak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_doublereal *scale, __CLPK_integer *m, __CLPK_doublecomplex *v, 
	__CLPK_integer *ldv, __CLPK_integer *info);
 
/* Subroutine */ int zgebal_(char *job, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer 
	*lda, __CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *scale, __CLPK_integer *info);
 
/* Subroutine */ int zgebd2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *tauq, 
	__CLPK_doublecomplex *taup, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zgebrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *tauq, 
	__CLPK_doublecomplex *taup, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zgecon_(char *norm, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeequ_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *r__, __CLPK_doublereal *c__, __CLPK_doublereal *rowcnd, 
	__CLPK_doublereal *colcnd, __CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int zgees_(char *jobvs, char *sort, __CLPK_L_fp select, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *sdim, __CLPK_doublecomplex *w, 
	__CLPK_doublecomplex *vs, __CLPK_integer *ldvs, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_doublereal *rwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeesx_(char *jobvs, char *sort, __CLPK_L_fp select, char *
	sense, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *sdim, 
	__CLPK_doublecomplex *w, __CLPK_doublecomplex *vs, __CLPK_integer *ldvs, __CLPK_doublereal *
	rconde, __CLPK_doublereal *rcondv, __CLPK_doublecomplex *work, __CLPK_integer *lwork, 
	__CLPK_doublereal *rwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeev_(char *jobvl, char *jobvr, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *w, __CLPK_doublecomplex *vl, 
	__CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *w, 
	__CLPK_doublecomplex *vl, __CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *scale, __CLPK_doublereal *abnrm, 
	__CLPK_doublereal *rconde, __CLPK_doublereal *rcondv, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgegs_(char *jobvsl, char *jobvsr, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *alpha, __CLPK_doublecomplex *beta, __CLPK_doublecomplex *vsl, 
	__CLPK_integer *ldvsl, __CLPK_doublecomplex *vsr, __CLPK_integer *ldvsr, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgegv_(char *jobvl, char *jobvr, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *alpha, __CLPK_doublecomplex *beta, __CLPK_doublecomplex *vl, __CLPK_integer 
	*ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, __CLPK_doublecomplex *work, __CLPK_integer 
	*lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgehd2_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zgehrd_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zgelq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zgelqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zgels_(char *trans, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zgelsx_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *jpvt, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublecomplex *work, 
	__CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgelsy_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *jpvt, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeql2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zgeqlf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zgeqp3_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeqpf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, 
	__CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgeqr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zgeqrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zgerfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *ldaf, 
	__CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, 
	__CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work,
	 __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgerq2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zgerqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zgesc2_(__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublecomplex *rhs, __CLPK_integer *ipiv, __CLPK_integer *jpiv, __CLPK_doublereal *scale);
 
/* Subroutine */ int zgesv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *
	info);
 
/* Subroutine */ int zgesvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *
	ldaf, __CLPK_integer *ipiv, char *equed, __CLPK_doublereal *r__, __CLPK_doublereal *c__, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgetc2_(__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_integer *jpiv, __CLPK_integer *info);
 
/* Subroutine */ int zgetf2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zgetrf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zgetri_(__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zgetrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zggbak_(char *job, char *side, __CLPK_integer *n, __CLPK_integer *ilo, 
	__CLPK_integer *ihi, __CLPK_doublereal *lscale, __CLPK_doublereal *rscale, __CLPK_integer *m, 
	__CLPK_doublecomplex *v, __CLPK_integer *ldv, __CLPK_integer *info);
 
/* Subroutine */ int zggbal_(char *job, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer 
	*lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublereal *lscale, __CLPK_doublereal *rscale, __CLPK_doublereal *work, __CLPK_integer *
	info);
 
/* Subroutine */ int zgges_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	delctg, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_integer *sdim, __CLPK_doublecomplex *alpha, __CLPK_doublecomplex *
	beta, __CLPK_doublecomplex *vsl, __CLPK_integer *ldvsl, __CLPK_doublecomplex *vsr, __CLPK_integer 
	*ldvsr, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, 
	__CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int zggesx_(char *jobvsl, char *jobvsr, char *sort, __CLPK_L_fp 
	delctg, char *sense, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *sdim, __CLPK_doublecomplex *alpha, 
	__CLPK_doublecomplex *beta, __CLPK_doublecomplex *vsl, __CLPK_integer *ldvsl, 
	__CLPK_doublecomplex *vsr, __CLPK_integer *ldvsr, __CLPK_doublereal *rconde, __CLPK_doublereal *
	rcondv, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, 
	__CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int zggev_(char *jobvl, char *jobvr, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *alpha, __CLPK_doublecomplex *beta, __CLPK_doublecomplex *vl, __CLPK_integer 
	*ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, __CLPK_doublecomplex *work, __CLPK_integer 
	*lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublecomplex *alpha, __CLPK_doublecomplex *beta, 
	__CLPK_doublecomplex *vl, __CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublereal *lscale, __CLPK_doublereal *rscale, 
	__CLPK_doublereal *abnrm, __CLPK_doublereal *bbnrm, __CLPK_doublereal *rconde, __CLPK_doublereal *
	rcondv, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, 
	__CLPK_integer *iwork, __CLPK_logical *bwork, __CLPK_integer *info);
 
/* Subroutine */ int zggglm_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *d__, __CLPK_doublecomplex *x, __CLPK_doublecomplex *y, __CLPK_doublecomplex 
	*work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zgghrd_(char *compq, char *compz, __CLPK_integer *n, __CLPK_integer *
	ilo, __CLPK_integer *ihi, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *z__, 
	__CLPK_integer *ldz, __CLPK_integer *info);
 
/* Subroutine */ int zgglse_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *p, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *c__, __CLPK_doublecomplex *d__, __CLPK_doublecomplex *x, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zggqrf_(__CLPK_integer *n, __CLPK_integer *m, __CLPK_integer *p, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *taua, __CLPK_doublecomplex *b,
	 __CLPK_integer *ldb, __CLPK_doublecomplex *taub, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int zggrqf_(__CLPK_integer *m, __CLPK_integer *p, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *taua, __CLPK_doublecomplex *b,
	 __CLPK_integer *ldb, __CLPK_doublecomplex *taub, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int zggsvd_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *p, __CLPK_integer *k, __CLPK_integer *l, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublereal *alpha, 
	__CLPK_doublereal *beta, __CLPK_doublecomplex *u, __CLPK_integer *ldu, __CLPK_doublecomplex *v, 
	__CLPK_integer *ldv, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *work, 
	__CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int zggsvp_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex 
	*b, __CLPK_integer *ldb, __CLPK_doublereal *tola, __CLPK_doublereal *tolb, __CLPK_integer *k, 
	__CLPK_integer *l, __CLPK_doublecomplex *u, __CLPK_integer *ldu, __CLPK_doublecomplex *v, __CLPK_integer 
	*ldv, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_integer *iwork, __CLPK_doublereal *
	rwork, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zgtcon_(char *norm, __CLPK_integer *n, __CLPK_doublecomplex *dl, 
	__CLPK_doublecomplex *d__, __CLPK_doublecomplex *du, __CLPK_doublecomplex *du2, __CLPK_integer *
	ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublecomplex *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int zgtrfs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *dl, __CLPK_doublecomplex *d__, __CLPK_doublecomplex *du, 
	__CLPK_doublecomplex *dlf, __CLPK_doublecomplex *df, __CLPK_doublecomplex *duf, 
	__CLPK_doublecomplex *du2, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zgtsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublecomplex *dl, 
	__CLPK_doublecomplex *d__, __CLPK_doublecomplex *du, __CLPK_doublecomplex *b, __CLPK_integer *ldb,
	 __CLPK_integer *info);
 
/* Subroutine */ int zgtsvx_(char *fact, char *trans, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *dl, __CLPK_doublecomplex *d__, __CLPK_doublecomplex *du, 
	__CLPK_doublecomplex *dlf, __CLPK_doublecomplex *df, __CLPK_doublecomplex *duf, 
	__CLPK_doublecomplex *du2, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zgttrf_(__CLPK_integer *n, __CLPK_doublecomplex *dl, __CLPK_doublecomplex *
	d__, __CLPK_doublecomplex *du, __CLPK_doublecomplex *du2, __CLPK_integer *ipiv, __CLPK_integer *
	info);
 
/* Subroutine */ int zgttrs_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *dl, __CLPK_doublecomplex *d__, __CLPK_doublecomplex *du, 
	__CLPK_doublecomplex *du2, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zgtts2_(__CLPK_integer *itrans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *dl, __CLPK_doublecomplex *d__, __CLPK_doublecomplex *du, 
	__CLPK_doublecomplex *du2, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int zhbev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, 
	__CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhbevd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, 
	__CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, 
	__CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int zhbevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *q, 
	__CLPK_integer *ldq, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *
	iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublecomplex *z__,
	 __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *iwork,
	 __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int zhbgst_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *bb, 
	__CLPK_integer *ldbb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublecomplex *work, 
	__CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhbgv_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *bb, 
	__CLPK_integer *ldbb, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhbgvx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_integer *ka, __CLPK_integer *kb, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, 
	__CLPK_doublecomplex *bb, __CLPK_integer *ldbb, __CLPK_doublecomplex *q, __CLPK_integer *ldq, 
	__CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *
	abstol, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *
	ifail, __CLPK_integer *info);
 
/* Subroutine */ int zhbtrd_(char *vect, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zhecon_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, 
	__CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zheev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_doublecomplex 
	*a, __CLPK_integer *lda, __CLPK_doublereal *w, __CLPK_doublecomplex *work, __CLPK_integer *lwork, 
	__CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zheevd_(char *jobz, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *w, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *lrwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int zheevr_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *vl, __CLPK_doublereal *vu, 
	__CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *
	w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *lrwork, __CLPK_integer *
	iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int zheevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *vl, __CLPK_doublereal *vu, 
	__CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *
	w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *
	info);
 
/* Subroutine */ int zhegs2_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zhegst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zhegv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *w, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zhegvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *w, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork,
	 __CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int zhegvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *
	iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublecomplex *z__,
	 __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork,
	 __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int zherfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *ldaf, 
	__CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, 
	__CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work,
	 __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhesv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zhesvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *
	ldaf, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x,
	 __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhetf2_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zhetrd_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zhetrf_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int zhetri_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zhetrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zhgeqz_(char *job, char *compq, char *compz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *alpha, __CLPK_doublecomplex *
	beta, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *z__, __CLPK_integer *
	ldz, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zhpcon_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zhpev_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_doublecomplex 
	*ap, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhpevd_(char *jobz, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *ap, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *
	lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int zhpevx_(char *jobz, char *range, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *ap, __CLPK_doublereal *vl, __CLPK_doublereal *vu, __CLPK_integer *il, 
	__CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_doublereal *
	rwork, __CLPK_integer *iwork, __CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int zhpgst_(__CLPK_integer *itype, char *uplo, __CLPK_integer *n, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *bp, __CLPK_integer *info);
 
/* Subroutine */ int zhpgv_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *bp, __CLPK_doublereal *w, __CLPK_doublecomplex 
	*z__, __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zhpgvd_(__CLPK_integer *itype, char *jobz, char *uplo, __CLPK_integer *
	n, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *bp, __CLPK_doublereal *w, __CLPK_doublecomplex 
	*z__, __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *
	rwork, __CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zhpgvx_(__CLPK_integer *itype, char *jobz, char *range, char *
	uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *bp, __CLPK_doublereal *
	vl, __CLPK_doublereal *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, 
	__CLPK_integer *m, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *
	ifail, __CLPK_integer *info);
 
/* Subroutine */ int zhprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *afp, __CLPK_integer *ipiv, __CLPK_doublecomplex *
	b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zhpsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zhpsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *afp, __CLPK_integer *ipiv, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zhptrd_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *tau, __CLPK_integer *info);
 
/* Subroutine */ int zhptrf_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zhptri_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zhptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zhsein_(char *side, char *eigsrc, char *initv, __CLPK_logical *
	select, __CLPK_integer *n, __CLPK_doublecomplex *h__, __CLPK_integer *ldh, __CLPK_doublecomplex *
	w, __CLPK_doublecomplex *vl, __CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr,
	 __CLPK_integer *mm, __CLPK_integer *m, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, 
	__CLPK_integer *ifaill, __CLPK_integer *ifailr, __CLPK_integer *info);
 
/* Subroutine */ int zhseqr_(char *job, char *compz, __CLPK_integer *n, __CLPK_integer *ilo,
	 __CLPK_integer *ihi, __CLPK_doublecomplex *h__, __CLPK_integer *ldh, __CLPK_doublecomplex *w, 
	__CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zlabrd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nb, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_doublecomplex *tauq, __CLPK_doublecomplex *taup, __CLPK_doublecomplex *x, __CLPK_integer *
	ldx, __CLPK_doublecomplex *y, __CLPK_integer *ldy);
 
/* Subroutine */ int zlacgv_(__CLPK_integer *n, __CLPK_doublecomplex *x, __CLPK_integer *incx);
 
/* Subroutine */ int zlacon_(__CLPK_integer *n, __CLPK_doublecomplex *v, __CLPK_doublecomplex *x, 
	__CLPK_doublereal *est, __CLPK_integer *kase);
 
/* Subroutine */ int zlacp2_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *
	a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int zlacpy_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int zlacrm_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_doublecomplex *c__, 
	__CLPK_integer *ldc, __CLPK_doublereal *rwork);
 
/* Subroutine */ int zlacrt_(__CLPK_integer *n, __CLPK_doublecomplex *cx, __CLPK_integer *incx, 
	__CLPK_doublecomplex *cy, __CLPK_integer *incy, __CLPK_doublecomplex *c__, __CLPK_doublecomplex *
	s);
 
/* Subroutine */ int zlaed0_(__CLPK_integer *qsiz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *qstore, 
	__CLPK_integer *ldqs, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int zlaed7_(__CLPK_integer *n, __CLPK_integer *cutpnt, __CLPK_integer *qsiz, 
	__CLPK_integer *tlvls, __CLPK_integer *curlvl, __CLPK_integer *curpbm, __CLPK_doublereal *d__, 
	__CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublereal *rho, __CLPK_integer *indxq, 
	__CLPK_doublereal *qstore, __CLPK_integer *qptr, __CLPK_integer *prmptr, __CLPK_integer *perm, 
	__CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_doublereal *givnum, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int zlaed8_(__CLPK_integer *k, __CLPK_integer *n, __CLPK_integer *qsiz, 
	__CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublereal *d__, __CLPK_doublereal *rho, 
	__CLPK_integer *cutpnt, __CLPK_doublereal *z__, __CLPK_doublereal *dlamda, __CLPK_doublecomplex *
	q2, __CLPK_integer *ldq2, __CLPK_doublereal *w, __CLPK_integer *indxp, __CLPK_integer *indx, 
	__CLPK_integer *indxq, __CLPK_integer *perm, __CLPK_integer *givptr, __CLPK_integer *givcol, 
	__CLPK_doublereal *givnum, __CLPK_integer *info);
 
/* Subroutine */ int zlaein_(__CLPK_logical *rightv, __CLPK_logical *noinit, __CLPK_integer *n, 
	__CLPK_doublecomplex *h__, __CLPK_integer *ldh, __CLPK_doublecomplex *w, __CLPK_doublecomplex *v, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublereal *rwork, __CLPK_doublereal *eps3, 
	__CLPK_doublereal *smlnum, __CLPK_integer *info);
 
/* Subroutine */ int zlaesy_(__CLPK_doublecomplex *a, __CLPK_doublecomplex *b, 
	__CLPK_doublecomplex *c__, __CLPK_doublecomplex *rt1, __CLPK_doublecomplex *rt2, 
	__CLPK_doublecomplex *evscal, __CLPK_doublecomplex *cs1, __CLPK_doublecomplex *sn1);
 
/* Subroutine */ int zlaev2_(__CLPK_doublecomplex *a, __CLPK_doublecomplex *b, 
	__CLPK_doublecomplex *c__, __CLPK_doublereal *rt1, __CLPK_doublereal *rt2, __CLPK_doublereal *cs1,
	 __CLPK_doublecomplex *sn1);
 
/* Subroutine */ int zlags2_(__CLPK_logical *upper, __CLPK_doublereal *a1, __CLPK_doublecomplex *
	a2, __CLPK_doublereal *a3, __CLPK_doublereal *b1, __CLPK_doublecomplex *b2, __CLPK_doublereal *b3,
	 __CLPK_doublereal *csu, __CLPK_doublecomplex *snu, __CLPK_doublereal *csv, __CLPK_doublecomplex *
	snv, __CLPK_doublereal *csq, __CLPK_doublecomplex *snq);
 
/* Subroutine */ int zlagtm_(char *trans, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *alpha, __CLPK_doublecomplex *dl, __CLPK_doublecomplex *d__, 
	__CLPK_doublecomplex *du, __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *beta, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int zlahef_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_integer *kb,
	 __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *w, 
	__CLPK_integer *ldw, __CLPK_integer *info);
 
/* Subroutine */ int zlahqr_(__CLPK_logical *wantt, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublecomplex *h__, __CLPK_integer *ldh, 
	__CLPK_doublecomplex *w, __CLPK_integer *iloz, __CLPK_integer *ihiz, __CLPK_doublecomplex *z__, 
	__CLPK_integer *ldz, __CLPK_integer *info);
 
/* Subroutine */ int zlahrd_(__CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *nb, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *t, 
	__CLPK_integer *ldt, __CLPK_doublecomplex *y, __CLPK_integer *ldy);
 
/* Subroutine */ int zlaic1_(__CLPK_integer *job, __CLPK_integer *j, __CLPK_doublecomplex *x, 
	__CLPK_doublereal *sest, __CLPK_doublecomplex *w, __CLPK_doublecomplex *gamma, __CLPK_doublereal *
	sestpr, __CLPK_doublecomplex *s, __CLPK_doublecomplex *c__);
 
/* Subroutine */ int zlals0_(__CLPK_integer *icompq, __CLPK_integer *nl, __CLPK_integer *nr, 
	__CLPK_integer *sqre, __CLPK_integer *nrhs, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *bx, __CLPK_integer *ldbx, __CLPK_integer *perm, __CLPK_integer *givptr, 
	__CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_doublereal *givnum, __CLPK_integer *ldgnum,
	 __CLPK_doublereal *poles, __CLPK_doublereal *difl, __CLPK_doublereal *difr, __CLPK_doublereal *
	z__, __CLPK_integer *k, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_doublereal *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int zlalsa_(__CLPK_integer *icompq, __CLPK_integer *smlsiz, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *bx, 
	__CLPK_integer *ldbx, __CLPK_doublereal *u, __CLPK_integer *ldu, __CLPK_doublereal *vt, __CLPK_integer *
	k, __CLPK_doublereal *difl, __CLPK_doublereal *difr, __CLPK_doublereal *z__, __CLPK_doublereal *
	poles, __CLPK_integer *givptr, __CLPK_integer *givcol, __CLPK_integer *ldgcol, __CLPK_integer *
	perm, __CLPK_doublereal *givnum, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_doublereal *
	rwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int zlapll_(__CLPK_integer *n, __CLPK_doublecomplex *x, __CLPK_integer *incx, 
	__CLPK_doublecomplex *y, __CLPK_integer *incy, __CLPK_doublereal *ssmin);
 
/* Subroutine */ int zlapmt_(__CLPK_logical *forwrd, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_integer *k);
 
/* Subroutine */ int zlaqgb_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku,
	 __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *r__, __CLPK_doublereal *c__, 
	__CLPK_doublereal *rowcnd, __CLPK_doublereal *colcnd, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int zlaqge_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *r__, __CLPK_doublereal *c__, __CLPK_doublereal *rowcnd, 
	__CLPK_doublereal *colcnd, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int zlaqhb_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *s, __CLPK_doublereal *scond, 
	__CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int zlaqhe_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, 
	char *equed);
 
/* Subroutine */ int zlaqhp_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int zlaqp2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *jpvt, __CLPK_doublecomplex *tau, 
	__CLPK_doublereal *vn1, __CLPK_doublereal *vn2, __CLPK_doublecomplex *work);
 
/* Subroutine */ int zlaqps_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *offset, __CLPK_integer 
	*nb, __CLPK_integer *kb, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *jpvt, 
	__CLPK_doublecomplex *tau, __CLPK_doublereal *vn1, __CLPK_doublereal *vn2, __CLPK_doublecomplex *
	auxv, __CLPK_doublecomplex *f, __CLPK_integer *ldf);
 
/* Subroutine */ int zlaqsb_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *s, __CLPK_doublereal *scond, 
	__CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int zlaqsp_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, char *equed);
 
/* Subroutine */ int zlaqsy_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, 
	char *equed);
 
/* Subroutine */ int zlar1v_(__CLPK_integer *n, __CLPK_integer *b1, __CLPK_integer *bn, __CLPK_doublereal 
	*sigma, __CLPK_doublereal *d__, __CLPK_doublereal *l, __CLPK_doublereal *ld, __CLPK_doublereal *
	lld, __CLPK_doublereal *gersch, __CLPK_doublecomplex *z__, __CLPK_doublereal *ztz, 
	__CLPK_doublereal *mingma, __CLPK_integer *r__, __CLPK_integer *isuppz, __CLPK_doublereal *work);
 
/* Subroutine */ int zlar2v_(__CLPK_integer *n, __CLPK_doublecomplex *x, __CLPK_doublecomplex *y, 
	__CLPK_doublecomplex *z__, __CLPK_integer *incx, __CLPK_doublereal *c__, __CLPK_doublecomplex *s, 
	__CLPK_integer *incc);
 
/* Subroutine */ int zlarcm_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
	lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *c__, __CLPK_integer *ldc,
	 __CLPK_doublereal *rwork);
 
/* Subroutine */ int zlarf_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex 
	*v, __CLPK_integer *incv, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *c__, __CLPK_integer *
	ldc, __CLPK_doublecomplex *work);
 
/* Subroutine */ int zlarfb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_doublecomplex *v, __CLPK_integer 
	*ldv, __CLPK_doublecomplex *t, __CLPK_integer *ldt, __CLPK_doublecomplex *c__, __CLPK_integer *
	ldc, __CLPK_doublecomplex *work, __CLPK_integer *ldwork);
 
/* Subroutine */ int zlarfg_(__CLPK_integer *n, __CLPK_doublecomplex *alpha, __CLPK_doublecomplex *
	x, __CLPK_integer *incx, __CLPK_doublecomplex *tau);
 
/* Subroutine */ int zlarft_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_doublecomplex *v, __CLPK_integer *ldv, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	t, __CLPK_integer *ldt);
 
/* Subroutine */ int zlarfx_(char *side, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *v, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *c__, __CLPK_integer *
	ldc, __CLPK_doublecomplex *work);
 
/* Subroutine */ int zlargv_(__CLPK_integer *n, __CLPK_doublecomplex *x, __CLPK_integer *incx, 
	__CLPK_doublecomplex *y, __CLPK_integer *incy, __CLPK_doublereal *c__, __CLPK_integer *incc);
 
/* Subroutine */ int zlarnv_(__CLPK_integer *idist, __CLPK_integer *iseed, __CLPK_integer *n, 
	__CLPK_doublecomplex *x);
 
/* Subroutine */ int zlarrv_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *l, 
	__CLPK_integer *isplit, __CLPK_integer *m, __CLPK_doublereal *w, __CLPK_integer *iblock, 
	__CLPK_doublereal *gersch, __CLPK_doublereal *tol, __CLPK_doublecomplex *z__, __CLPK_integer *ldz,
	 __CLPK_integer *isuppz, __CLPK_doublereal *work, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int zlartg_(__CLPK_doublecomplex *f, __CLPK_doublecomplex *g, __CLPK_doublereal *
	cs, __CLPK_doublecomplex *sn, __CLPK_doublecomplex *r__);
 
/* Subroutine */ int zlartv_(__CLPK_integer *n, __CLPK_doublecomplex *x, __CLPK_integer *incx, 
	__CLPK_doublecomplex *y, __CLPK_integer *incy, __CLPK_doublereal *c__, __CLPK_doublecomplex *s, 
	__CLPK_integer *incc);
 
/* Subroutine */ int zlarz_(char *side, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, 
	__CLPK_doublecomplex *v, __CLPK_integer *incv, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work);
 
/* Subroutine */ int zlarzb_(char *side, char *trans, char *direct, char *
	storev, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_doublecomplex 
	*v, __CLPK_integer *ldv, __CLPK_doublecomplex *t, __CLPK_integer *ldt, __CLPK_doublecomplex *c__, 
	__CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *ldwork);
 
/* Subroutine */ int zlarzt_(char *direct, char *storev, __CLPK_integer *n, __CLPK_integer *
	k, __CLPK_doublecomplex *v, __CLPK_integer *ldv, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	t, __CLPK_integer *ldt);
 
/* Subroutine */ int zlascl_(char *type__, __CLPK_integer *kl, __CLPK_integer *ku, 
	__CLPK_doublereal *cfrom, __CLPK_doublereal *cto, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int zlaset_(char *uplo, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *alpha, __CLPK_doublecomplex *beta, __CLPK_doublecomplex *a, __CLPK_integer *
	lda);
 
/* Subroutine */ int zlasr_(char *side, char *pivot, char *direct, __CLPK_integer *m,
	 __CLPK_integer *n, __CLPK_doublereal *c__, __CLPK_doublereal *s, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda);
 
/* Subroutine */ int zlassq_(__CLPK_integer *n, __CLPK_doublecomplex *x, __CLPK_integer *incx, 
	__CLPK_doublereal *scale, __CLPK_doublereal *sumsq);
 
/* Subroutine */ int zlaswp_(__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_integer *k1, __CLPK_integer *k2, __CLPK_integer *ipiv, __CLPK_integer *incx);
 
/* Subroutine */ int zlasyf_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, __CLPK_integer *kb,
	 __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *w, 
	__CLPK_integer *ldw, __CLPK_integer *info);
 
/* Subroutine */ int zlatbs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, 
	__CLPK_doublecomplex *x, __CLPK_doublereal *scale, __CLPK_doublereal *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int zlatdf_(__CLPK_integer *ijob, __CLPK_integer *n, __CLPK_doublecomplex *z__, 
	__CLPK_integer *ldz, __CLPK_doublecomplex *rhs, __CLPK_doublereal *rdsum, __CLPK_doublereal *
	rdscal, __CLPK_integer *ipiv, __CLPK_integer *jpiv);
 
/* Subroutine */ int zlatps_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *x, __CLPK_doublereal *
	scale, __CLPK_doublereal *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int zlatrd_(char *uplo, __CLPK_integer *n, __CLPK_integer *nb, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *e, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *w, __CLPK_integer *ldw);
 
/* Subroutine */ int zlatrs_(char *uplo, char *trans, char *diag, char *
	normin, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *x, 
	__CLPK_doublereal *scale, __CLPK_doublereal *cnorm, __CLPK_integer *info);
 
/* Subroutine */ int zlatrz_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *l, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work);
 
/* Subroutine */ int zlatzm_(char *side, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *v, __CLPK_integer *incv, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	c1, __CLPK_doublecomplex *c2, __CLPK_integer *ldc, __CLPK_doublecomplex *work);
 
/* Subroutine */ int zlauu2_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int zlauum_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int zpbcon_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *anorm, __CLPK_doublereal *
	rcond, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zpbequ_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *s, __CLPK_doublereal *scond, 
	__CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int zpbrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *afb, __CLPK_integer *
	ldafb, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx,
	 __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int zpbstf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int zpbsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *b, __CLPK_integer *
	ldb, __CLPK_integer *info);
 
/* Subroutine */ int zpbsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_integer *nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *afb, 
	__CLPK_integer *ldafb, char *equed, __CLPK_doublereal *s, __CLPK_doublecomplex *b, __CLPK_integer 
	*ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *
	ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int zpbtf2_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int zpbtrf_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_integer *info);
 
/* Subroutine */ int zpbtrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *kd, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *b, __CLPK_integer *
	ldb, __CLPK_integer *info);
 
/* Subroutine */ int zpocon_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zpoequ_(__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int zporfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *ldaf, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int zposv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zposvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *
	ldaf, char *equed, __CLPK_doublereal *s, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zpotf2_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int zpotrf_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int zpotri_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int zpotrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zppcon_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublecomplex *work, __CLPK_doublereal 
	*rwork, __CLPK_integer *info);
 
/* Subroutine */ int zppequ_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *s, __CLPK_doublereal *scond, __CLPK_doublereal *amax, __CLPK_integer *info);
 
/* Subroutine */ int zpprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *afp, __CLPK_doublecomplex *b, __CLPK_integer *ldb,
	 __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zppsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zppsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *afp, char *equed, __CLPK_doublereal *
	s, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zpptrf_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *info);
 
/* Subroutine */ int zpptri_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *info);
 
/* Subroutine */ int zpptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zptcon_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublecomplex *e, 
	__CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zptrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *d__, __CLPK_doublecomplex *e, __CLPK_doublereal *df, __CLPK_doublecomplex *ef, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int zptsv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *d__, 
	__CLPK_doublecomplex *e, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int zptsvx_(char *fact, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *d__, __CLPK_doublecomplex *e, __CLPK_doublereal *df, __CLPK_doublecomplex *ef, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zpttrf_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublecomplex *e, 
	__CLPK_integer *info);
 
/* Subroutine */ int zpttrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *d__, __CLPK_doublecomplex *e, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zptts2_(__CLPK_integer *iuplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublereal *d__, __CLPK_doublecomplex *e, __CLPK_doublecomplex *b, __CLPK_integer *ldb);
 
/* Subroutine */ int zrot_(__CLPK_integer *n, __CLPK_doublecomplex *cx, __CLPK_integer *incx, 
	__CLPK_doublecomplex *cy, __CLPK_integer *incy, __CLPK_doublereal *c__, __CLPK_doublecomplex *s);
 
/* Subroutine */ int zspcon_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zspmv_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *alpha, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *x, __CLPK_integer *incx, __CLPK_doublecomplex *
	beta, __CLPK_doublecomplex *y, __CLPK_integer *incy);
 
/* Subroutine */ int zspr_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *alpha, 
	__CLPK_doublecomplex *x, __CLPK_integer *incx, __CLPK_doublecomplex *ap);
 
/* Subroutine */ int zsprfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_doublecomplex *afp, __CLPK_integer *ipiv, __CLPK_doublecomplex *
	b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int zspsv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zspsvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *afp, __CLPK_integer *ipiv, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zsptrf_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zsptri_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zsptrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *ap, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int zstedc_(char *compz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *lrwork, __CLPK_integer *iwork, 
	__CLPK_integer *liwork, __CLPK_integer *info);
 
/* Subroutine */ int zstein_(__CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e, 
	__CLPK_integer *m, __CLPK_doublereal *w, __CLPK_integer *iblock, __CLPK_integer *isplit, 
	__CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, __CLPK_integer *iwork, 
	__CLPK_integer *ifail, __CLPK_integer *info);
 
/* Subroutine */ int zsteqr_(char *compz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);
 
/* Subroutine */ int zsycon_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublereal *anorm, __CLPK_doublereal *rcond, 
	__CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zsymv_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *alpha, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *x, __CLPK_integer *incx, 
	__CLPK_doublecomplex *beta, __CLPK_doublecomplex *y, __CLPK_integer *incy);
 
/* Subroutine */ int zsyr_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *alpha, 
	__CLPK_doublecomplex *x, __CLPK_integer *incx, __CLPK_doublecomplex *a, __CLPK_integer *lda);
 
/* Subroutine */ int zsyrfs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *ldaf, 
	__CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, 
	__CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work,
	 __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zsysv_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zsysvx_(char *fact, char *uplo, __CLPK_integer *n, __CLPK_integer *
	nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *af, __CLPK_integer *
	ldaf, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x,
	 __CLPK_integer *ldx, __CLPK_doublereal *rcond, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int zsytf2_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_integer *info);
 
/* Subroutine */ int zsytrf_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *lwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int zsytri_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zsytrs_(char *uplo, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *ipiv, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ztbcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *rcond, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztbrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, 
	__CLPK_doublereal *ferr, __CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *
	rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztbtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *kd, __CLPK_integer *nrhs, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ztgevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer 
	*ldb, __CLPK_doublecomplex *vl, __CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *
	ldvr, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int ztgex2_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_integer *j1, __CLPK_integer *info);
 
/* Subroutine */ int ztgexc_(__CLPK_logical *wantq, __CLPK_logical *wantz, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_integer *ifst, __CLPK_integer *ilst, __CLPK_integer *info);
 
/* Subroutine */ int ztgsen_(__CLPK_integer *ijob, __CLPK_logical *wantq, __CLPK_logical *wantz, 
	__CLPK_logical *select, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublecomplex *alpha, __CLPK_doublecomplex *
	beta, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *z__, __CLPK_integer *
	ldz, __CLPK_integer *m, __CLPK_doublereal *pl, __CLPK_doublereal *pr, __CLPK_doublereal *dif, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);
 
/* Subroutine */ int ztgsja_(char *jobu, char *jobv, char *jobq, __CLPK_integer *m, 
	__CLPK_integer *p, __CLPK_integer *n, __CLPK_integer *k, __CLPK_integer *l, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, __CLPK_doublereal *tola, 
	__CLPK_doublereal *tolb, __CLPK_doublereal *alpha, __CLPK_doublereal *beta, __CLPK_doublecomplex *
	u, __CLPK_integer *ldu, __CLPK_doublecomplex *v, __CLPK_integer *ldv, __CLPK_doublecomplex *q, 
	__CLPK_integer *ldq, __CLPK_doublecomplex *work, __CLPK_integer *ncycle, __CLPK_integer *info);
 
/* Subroutine */ int ztgsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer 
	*ldb, __CLPK_doublecomplex *vl, __CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *
	ldvr, __CLPK_doublereal *s, __CLPK_doublereal *dif, __CLPK_integer *mm, __CLPK_integer *m, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int ztgsy2_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *d__, __CLPK_integer *ldd, 
	__CLPK_doublecomplex *e, __CLPK_integer *lde, __CLPK_doublecomplex *f, __CLPK_integer *ldf, 
	__CLPK_doublereal *scale, __CLPK_doublereal *rdsum, __CLPK_doublereal *rdscal, __CLPK_integer *
	info);
 
/* Subroutine */ int ztgsyl_(char *trans, __CLPK_integer *ijob, __CLPK_integer *m, __CLPK_integer *
	n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *d__, __CLPK_integer *ldd, 
	__CLPK_doublecomplex *e, __CLPK_integer *lde, __CLPK_doublecomplex *f, __CLPK_integer *ldf, 
	__CLPK_doublereal *scale, __CLPK_doublereal *dif, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_integer *iwork, __CLPK_integer *info);
 
/* Subroutine */ int ztpcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublecomplex *ap, __CLPK_doublereal *rcond, __CLPK_doublecomplex *work, __CLPK_doublereal 
	*rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztprfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, __CLPK_doublereal *berr, 
	__CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztptri_(char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublecomplex *ap, __CLPK_integer *info);
 
/* Subroutine */ int ztptrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_integer *info);
 
/* Subroutine */ int ztrcon_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *rcond, __CLPK_doublecomplex *
	work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztrevc_(char *side, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublecomplex *t, __CLPK_integer *ldt, __CLPK_doublecomplex *vl, 
	__CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, __CLPK_integer *mm, __CLPK_integer 
	*m, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztrexc_(char *compq, __CLPK_integer *n, __CLPK_doublecomplex *t, 
	__CLPK_integer *ldt, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_integer *ifst, __CLPK_integer *
	ilst, __CLPK_integer *info);
 
/* Subroutine */ int ztrrfs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublecomplex *x, __CLPK_integer *ldx, __CLPK_doublereal *ferr, 
	__CLPK_doublereal *berr, __CLPK_doublecomplex *work, __CLPK_doublereal *rwork, __CLPK_integer *
	info);
 
/* Subroutine */ int ztrsen_(char *job, char *compq, __CLPK_logical *select, __CLPK_integer 
	*n, __CLPK_doublecomplex *t, __CLPK_integer *ldt, __CLPK_doublecomplex *q, __CLPK_integer *ldq, 
	__CLPK_doublecomplex *w, __CLPK_integer *m, __CLPK_doublereal *s, __CLPK_doublereal *sep, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int ztrsna_(char *job, char *howmny, __CLPK_logical *select, 
	__CLPK_integer *n, __CLPK_doublecomplex *t, __CLPK_integer *ldt, __CLPK_doublecomplex *vl, 
	__CLPK_integer *ldvl, __CLPK_doublecomplex *vr, __CLPK_integer *ldvr, __CLPK_doublereal *s, 
	__CLPK_doublereal *sep, __CLPK_integer *mm, __CLPK_integer *m, __CLPK_doublecomplex *work, 
	__CLPK_integer *ldwork, __CLPK_doublereal *rwork, __CLPK_integer *info);
 
/* Subroutine */ int ztrsyl_(char *trana, char *tranb, __CLPK_integer *isgn, __CLPK_integer 
	*m, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublereal *scale, 
	__CLPK_integer *info);
 
/* Subroutine */ int ztrti2_(char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int ztrtri_(char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_integer *info);
 
/* Subroutine */ int ztrtrs_(char *uplo, char *trans, char *diag, __CLPK_integer *n, 
	__CLPK_integer *nrhs, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, 
	__CLPK_integer *ldb, __CLPK_integer *info);
 
/* Subroutine */ int ztzrqf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_integer *info);
 
/* Subroutine */ int ztzrzf_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zung2l_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zung2r_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zungbr_(char *vect, __CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zunghr_(__CLPK_integer *n, __CLPK_integer *ilo, __CLPK_integer *ihi, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zungl2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zunglq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zungql_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zungqr_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zungr2_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zungrq_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zungtr_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zunm2l_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zunm2r_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zunmbr_(char *vect, char *side, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex 
	*tau, __CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int zunmhr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *ilo, __CLPK_integer *ihi, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublecomplex *tau, __CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *
	work, __CLPK_integer *lwork, __CLPK_integer *info);
 
/* Subroutine */ int zunml2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zunmlq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zunmql_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zunmqr_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zunmr2_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *info);
 
/* Subroutine */ int zunmr3_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex 
	*tau, __CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *
	info);
 
/* Subroutine */ int zunmrq_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zunmrz_(char *side, char *trans, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_integer *l, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex 
	*tau, __CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *
	lwork, __CLPK_integer *info);
 
/* Subroutine */ int zunmtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *tau, 
	__CLPK_doublecomplex *c__, __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *lwork,
	 __CLPK_integer *info);
 
/* Subroutine */ int zupgtr_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublecomplex *tau, __CLPK_doublecomplex *q, __CLPK_integer *ldq, __CLPK_doublecomplex *
	work, __CLPK_integer *info);
 
/* Subroutine */ int zupmtr_(char *side, char *uplo, char *trans, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_doublecomplex *ap, __CLPK_doublecomplex *tau, __CLPK_doublecomplex *c__,
	 __CLPK_integer *ldc, __CLPK_doublecomplex *work, __CLPK_integer *info);

/*
 The following prototypes are not present in the reference clapack.h distributed via netlib.
 Nevertheless, the supporting code has always been present in the library.
*/

/* Subroutine */ int cgelsd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *s, __CLPK_real *rcond, 
	__CLPK_integer *rank, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *
	iwork, __CLPK_integer *info);

/* Subroutine */ int cgelss_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_complex *
	a, __CLPK_integer *lda, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *s, __CLPK_real *rcond, 
	__CLPK_integer *rank, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *
	info);

/* Subroutine */ int cgesdd_(char *jobz, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *s, __CLPK_complex *u, __CLPK_integer *ldu, __CLPK_complex *vt, __CLPK_integer 
	*ldvt, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, __CLPK_integer *iwork, 
	__CLPK_integer *info);

/* Subroutine */ int cgesvd_(char *jobu, char *jobvt, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_complex *a, __CLPK_integer *lda, __CLPK_real *s, __CLPK_complex *u, __CLPK_integer *ldu, __CLPK_complex *
	vt, __CLPK_integer *ldvt, __CLPK_complex *work, __CLPK_integer *lwork, __CLPK_real *rwork, 
	__CLPK_integer *info);

/* Subroutine */ int chbgvd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_complex *bb, __CLPK_integer *ldbb, 
	__CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_complex *work, __CLPK_integer *lwork, 
	__CLPK_real *rwork, __CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);

/* Subroutine */ int chetd2_(char *uplo, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda,
	 __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *tau, __CLPK_integer *info);

/* Complex */ void cladiv_(__CLPK_complex * ret_val, __CLPK_complex *x, __CLPK_complex *y);

/* Subroutine */ int clalsd_(char *uplo, __CLPK_integer *smlsiz, __CLPK_integer *n, __CLPK_integer 
	*nrhs, __CLPK_real *d__, __CLPK_real *e, __CLPK_complex *b, __CLPK_integer *ldb, __CLPK_real *rcond, 
	__CLPK_integer *rank, __CLPK_complex *work, __CLPK_real *rwork, __CLPK_integer *iwork, 
	__CLPK_integer *info);

__CLPK_doublereal clangb_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, 
	__CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal clange_(char *norm, __CLPK_integer *m, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *work);

__CLPK_doublereal clangt_(char *norm, __CLPK_integer *n, __CLPK_complex *dl, __CLPK_complex *d__, 
	__CLPK_complex *du);

__CLPK_doublereal clanhb_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal clanhe_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *work);

__CLPK_doublereal clanhp_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_complex *ap, 
	__CLPK_real *work);

__CLPK_doublereal clanhs_(char *norm, __CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, 
	__CLPK_real *work);

__CLPK_doublereal clanht_(char *norm, __CLPK_integer *n, __CLPK_real *d__, __CLPK_complex *e);

__CLPK_doublereal clansb_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal clansp_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_complex *ap, 
	__CLPK_real *work);

__CLPK_doublereal clansy_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_complex *a, 
	__CLPK_integer *lda, __CLPK_real *work);

__CLPK_doublereal clantb_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_integer *k, __CLPK_complex *ab, __CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal clantp_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_complex *ap, __CLPK_real *work);

__CLPK_doublereal clantr_(char *norm, char *uplo, char *diag, __CLPK_integer *m, 
	__CLPK_integer *n, __CLPK_complex *a, __CLPK_integer *lda, __CLPK_real *work);

/* Subroutine */ int cpteqr_(char *compz, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e, 
	__CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_real *work, __CLPK_integer *info);

/* Subroutine */ int cstegr_(char *jobz, char *range, __CLPK_integer *n, __CLPK_real *d__, 
	__CLPK_real *e, __CLPK_real *vl, __CLPK_real *vu, __CLPK_integer *il, __CLPK_integer *iu, __CLPK_real *abstol, 
	__CLPK_integer *m, __CLPK_real *w, __CLPK_complex *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, 
	__CLPK_real *work, __CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, 
	__CLPK_integer *info);

__CLPK_doublereal dlamc3_(__CLPK_doublereal *a, __CLPK_doublereal *b);

__CLPK_doublereal dlamch_(char *cmach);

__CLPK_doublereal dlangb_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, 
	__CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal dlange_(char *norm, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublereal *a, 
	__CLPK_integer *lda, __CLPK_doublereal *work);

__CLPK_doublereal dlangt_(char *norm, __CLPK_integer *n, __CLPK_doublereal *dl, __CLPK_doublereal *d__, 
	__CLPK_doublereal *du);

__CLPK_doublereal dlanhs_(char *norm, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *lda, 
	__CLPK_doublereal *work);

__CLPK_doublereal dlansb_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal dlansp_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_doublereal *ap, 
	__CLPK_doublereal *work);

__CLPK_doublereal dlanst_(char *norm, __CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublereal *e);

__CLPK_doublereal dlansy_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, 
	__CLPK_integer *lda, __CLPK_doublereal *work);

__CLPK_doublereal dlantb_(char *norm, char *uplo, char *diag, __CLPK_integer *n, __CLPK_integer *k,
	 __CLPK_doublereal *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal dlantp_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublereal *ap, __CLPK_doublereal *work);

__CLPK_doublereal dlantr_(char *norm, char *uplo, char *diag, __CLPK_integer *m, __CLPK_integer *n,
	 __CLPK_doublereal *a, __CLPK_integer *lda, __CLPK_doublereal *work);

__CLPK_doublereal dlapy2_(__CLPK_doublereal *x, __CLPK_doublereal *y);

__CLPK_doublereal dlapy3_(__CLPK_doublereal *x, __CLPK_doublereal *y, __CLPK_doublereal *z__);

__CLPK_doublereal dsecnd_();

__CLPK_doublereal dzsum1_(__CLPK_integer *n, __CLPK_doublecomplex *cx, __CLPK_integer *incx);

__CLPK_logical lsame_(char *ca, char *cb);

__CLPK_logical lsamen_(__CLPK_integer *n, char *ca, char *cb);

__CLPK_doublereal scsum1_(__CLPK_integer *n, __CLPK_complex *cx, __CLPK_integer *incx);

__CLPK_doublereal second_();

__CLPK_doublereal slamc3_(__CLPK_real *a, __CLPK_real *b);

__CLPK_doublereal slamch_(char *cmach);

__CLPK_doublereal slangb_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, __CLPK_real *ab,
	 __CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal slange_(char *norm, __CLPK_integer *m, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *work);

__CLPK_doublereal slangt_(char *norm, __CLPK_integer *n, __CLPK_real *dl, __CLPK_real *d__, __CLPK_real *du);

__CLPK_doublereal slanhs_(char *norm, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *work);

__CLPK_doublereal slansb_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_integer *k, __CLPK_real *ab, 
	__CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal slansp_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_real *ap, __CLPK_real *work);

__CLPK_doublereal slanst_(char *norm, __CLPK_integer *n, __CLPK_real *d__, __CLPK_real *e);

__CLPK_doublereal slansy_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_real *a, __CLPK_integer *lda, 
	__CLPK_real *work);

__CLPK_doublereal slantb_(char *norm, char *uplo, char *diag, __CLPK_integer *n, __CLPK_integer *k,
	 __CLPK_real *ab, __CLPK_integer *ldab, __CLPK_real *work);

__CLPK_doublereal slantp_(char *norm, char *uplo, char *diag, __CLPK_integer *n, __CLPK_real *ap, 
	__CLPK_real *work);

__CLPK_doublereal slantr_(char *norm, char *uplo, char *diag, __CLPK_integer *m, __CLPK_integer *n,
	 __CLPK_real *a, __CLPK_integer *lda, __CLPK_real *work);

__CLPK_doublereal slapy2_(__CLPK_real *x, __CLPK_real *y);

__CLPK_doublereal slapy3_(__CLPK_real *x, __CLPK_real *y, __CLPK_real *z__);

/* Subroutine */ int zgelsd_(__CLPK_integer *m, __CLPK_integer *n, __CLPK_integer *nrhs, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublecomplex *b, __CLPK_integer *ldb, 
	__CLPK_doublereal *s, __CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *info);

/* Subroutine */ int zgesdd_(char *jobz, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *s, __CLPK_doublecomplex *u, 
	__CLPK_integer *ldu, __CLPK_doublecomplex *vt, __CLPK_integer *ldvt, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *info);

/* Subroutine */ int zgesvd_(char *jobu, char *jobvt, __CLPK_integer *m, __CLPK_integer *n, 
	__CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *s, __CLPK_doublecomplex *u, 
	__CLPK_integer *ldu, __CLPK_doublecomplex *vt, __CLPK_integer *ldvt, __CLPK_doublecomplex *work, 
	__CLPK_integer *lwork, __CLPK_doublereal *rwork, __CLPK_integer *info);

/* Subroutine */ int zhbgvd_(char *jobz, char *uplo, __CLPK_integer *n, __CLPK_integer *ka, 
	__CLPK_integer *kb, __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublecomplex *bb, 
	__CLPK_integer *ldbb, __CLPK_doublereal *w, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, 
	__CLPK_doublecomplex *work, __CLPK_integer *lwork, __CLPK_doublereal *rwork, 
	__CLPK_integer *lrwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);

/* Subroutine */ int zhetd2_(char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *tau, 
	__CLPK_integer *info);

/* Double Complex */ void zladiv_(__CLPK_doublecomplex * ret_val, __CLPK_doublecomplex *x, 
	__CLPK_doublecomplex *y);

/* Subroutine */ int zlalsd_(char *uplo, __CLPK_integer *smlsiz, __CLPK_integer *n, __CLPK_integer 
	*nrhs, __CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublecomplex *b, __CLPK_integer *ldb,
	__CLPK_doublereal *rcond, __CLPK_integer *rank, __CLPK_doublecomplex *work, 
	__CLPK_doublereal *rwork, __CLPK_integer *iwork, __CLPK_integer *info);

__CLPK_doublereal zlangb_(char *norm, __CLPK_integer *n, __CLPK_integer *kl, __CLPK_integer *ku, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal zlange_(char *norm, __CLPK_integer *m, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *work);

__CLPK_doublereal zlangt_(char *norm, __CLPK_integer *n, __CLPK_doublecomplex *dl, 
	__CLPK_doublecomplex *d__, __CLPK_doublecomplex *du);

__CLPK_doublereal zlanhb_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal zlanhe_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *work);

__CLPK_doublereal zlanhp_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *work);

__CLPK_doublereal zlanhs_(char *norm, __CLPK_integer *n, __CLPK_doublecomplex *a, __CLPK_integer *lda, 
	__CLPK_doublereal *work);

__CLPK_doublereal zlanht_(char *norm, __CLPK_integer *n, __CLPK_doublereal *d__, __CLPK_doublecomplex *e);

__CLPK_doublereal zlansb_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_integer *k, 
	__CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal zlansp_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *ap, 
	__CLPK_doublereal *work);

__CLPK_doublereal zlansy_(char *norm, char *uplo, __CLPK_integer *n, __CLPK_doublecomplex *a, 
	__CLPK_integer *lda, __CLPK_doublereal *work);

__CLPK_doublereal zlantb_(char *norm, char *uplo, char *diag, __CLPK_integer *n, __CLPK_integer *k,
	 __CLPK_doublecomplex *ab, __CLPK_integer *ldab, __CLPK_doublereal *work);

__CLPK_doublereal zlantp_(char *norm, char *uplo, char *diag, __CLPK_integer *n, 
	__CLPK_doublecomplex *ap, __CLPK_doublereal *work);

__CLPK_doublereal zlantr_(char *norm, char *uplo, char *diag, __CLPK_integer *m, __CLPK_integer *n,
	 __CLPK_doublecomplex *a, __CLPK_integer *lda, __CLPK_doublereal *work);

/* Subroutine */ int zpteqr_(char *compz, __CLPK_integer *n, __CLPK_doublereal *d__, 
	__CLPK_doublereal *e, __CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_doublereal *work, 
	__CLPK_integer *info);

/* Subroutine */ int zstegr_(char *jobz, char *range, __CLPK_integer *n, 
	__CLPK_doublereal *d__, __CLPK_doublereal *e, __CLPK_doublereal *vl, __CLPK_doublereal *vu, 
	__CLPK_integer *il, __CLPK_integer *iu, __CLPK_doublereal *abstol, __CLPK_integer *m, __CLPK_doublereal *w, 
	__CLPK_doublecomplex *z__, __CLPK_integer *ldz, __CLPK_integer *isuppz, __CLPK_doublereal *work, 
	__CLPK_integer *lwork, __CLPK_integer *iwork, __CLPK_integer *liwork, __CLPK_integer *info);

#ifdef __cplusplus
}
#endif
#endif /* __CLAPACK_H */
