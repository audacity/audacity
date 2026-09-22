/* CLAPACK 3.0 BLAS wrapper macros
 * Feb 5, 2000
 */

#ifndef __BLASWRAP_H
#define __BLASWRAP_H

#ifndef NO_BLAS_WRAP
 
/* BLAS1 routines */
#define srotg_ f2c_srotg
#define crotg_ f2c_crotg
#define drotg_ f2c_drotg
#define zrotg_ f2c_zrotg
#define srotmg_ f2c_srotmg
#define drotmg_ f2c_drotmg
#define srot_ f2c_srot
#define drot_ f2c_drot
#define srotm_ f2c_srotm
#define drotm_ f2c_drotm
#define sswap_ f2c_sswap
#define dswap_ f2c_dswap
#define cswap_ f2c_cswap
#define zswap_ f2c_zswap
#define sscal_ f2c_sscal
#define dscal_ f2c_dscal
#define cscal_ f2c_cscal
#define zscal_ f2c_zscal
#define csscal_ f2c_csscal
#define zdscal_ f2c_zdscal
#define scopy_ f2c_scopy
#define dcopy_ f2c_dcopy
#define ccopy_ f2c_ccopy
#define zcopy_ f2c_zcopy
#define saxpy_ f2c_saxpy
#define daxpy_ f2c_daxpy
#define caxpy_ f2c_caxpy
#define zaxpy_ f2c_zaxpy
#define sdot_ f2c_sdot
#define ddot_ f2c_ddot
#define cdotu_ f2c_cdotu
#define zdotu_ f2c_zdotu
#define cdotc_ f2c_cdotc
#define zdotc_ f2c_zdotc
#define snrm2_ f2c_snrm2
#define dnrm2_ f2c_dnrm2
#define scnrm2_ f2c_scnrm2
#define dznrm2_ f2c_dznrm2
#define sasum_ f2c_sasum
#define dasum_ f2c_dasum
#define scasum_ f2c_scasum
#define dzasum_ f2c_dzasum
#define isamax_ f2c_isamax
#define idamax_ f2c_idamax
#define icamax_ f2c_icamax
#define izamax_ f2c_izamax
 
/* BLAS2 routines */
#define sgemv_ f2c_sgemv
#define dgemv_ f2c_dgemv
#define cgemv_ f2c_cgemv
#define zgemv_ f2c_zgemv
#define sgbmv_ f2c_sgbmv
#define dgbmv_ f2c_dgbmv
#define cgbmv_ f2c_cgbmv
#define zgbmv_ f2c_zgbmv
#define chemv_ f2c_chemv
#define zhemv_ f2c_zhemv
#define chbmv_ f2c_chbmv
#define zhbmv_ f2c_zhbmv
#define chpmv_ f2c_chpmv
#define zhpmv_ f2c_zhpmv
#define ssymv_ f2c_ssymv
#define dsymv_ f2c_dsymv
#define ssbmv_ f2c_ssbmv
#define dsbmv_ f2c_dsbmv
#define sspmv_ f2c_sspmv
#define dspmv_ f2c_dspmv
#define strmv_ f2c_strmv
#define dtrmv_ f2c_dtrmv
#define ctrmv_ f2c_ctrmv
#define ztrmv_ f2c_ztrmv
#define stbmv_ f2c_stbmv
#define dtbmv_ f2c_dtbmv
#define ctbmv_ f2c_ctbmv
#define ztbmv_ f2c_ztbmv
#define stpmv_ f2c_stpmv
#define dtpmv_ f2c_dtpmv
#define ctpmv_ f2c_ctpmv
#define ztpmv_ f2c_ztpmv
#define strsv_ f2c_strsv
#define dtrsv_ f2c_dtrsv
#define ctrsv_ f2c_ctrsv
#define ztrsv_ f2c_ztrsv
#define stbsv_ f2c_stbsv
#define dtbsv_ f2c_dtbsv
#define ctbsv_ f2c_ctbsv
#define ztbsv_ f2c_ztbsv
#define stpsv_ f2c_stpsv
#define dtpsv_ f2c_dtpsv
#define ctpsv_ f2c_ctpsv
#define ztpsv_ f2c_ztpsv
#define sger_ f2c_sger
#define dger_ f2c_dger
#define cgeru_ f2c_cgeru
#define zgeru_ f2c_zgeru
#define cgerc_ f2c_cgerc
#define zgerc_ f2c_zgerc
#define cher_ f2c_cher
#define zher_ f2c_zher
#define chpr_ f2c_chpr
#define zhpr_ f2c_zhpr
#define cher2_ f2c_cher2
#define zher2_ f2c_zher2
#define chpr2_ f2c_chpr2
#define zhpr2_ f2c_zhpr2
#define ssyr_ f2c_ssyr
#define dsyr_ f2c_dsyr
#define sspr_ f2c_sspr
#define dspr_ f2c_dspr
#define ssyr2_ f2c_ssyr2
#define dsyr2_ f2c_dsyr2
#define sspr2_ f2c_sspr2
#define dspr2_ f2c_dspr2
 
/* BLAS3 routines */
#define sgemm_ f2c_sgemm
#define dgemm_ f2c_dgemm
#define cgemm_ f2c_cgemm
#define zgemm_ f2c_zgemm
#define ssymm_ f2c_ssymm
#define dsymm_ f2c_dsymm
#define csymm_ f2c_csymm
#define zsymm_ f2c_zsymm
#define chemm_ f2c_chemm
#define zhemm_ f2c_zhemm
#define ssyrk_ f2c_ssyrk
#define dsyrk_ f2c_dsyrk
#define csyrk_ f2c_csyrk
#define zsyrk_ f2c_zsyrk
#define cherk_ f2c_cherk
#define zherk_ f2c_zherk
#define ssyr2k_ f2c_ssyr2k
#define dsyr2k_ f2c_dsyr2k
#define csyr2k_ f2c_csyr2k
#define zsyr2k_ f2c_zsyr2k
#define cher2k_ f2c_cher2k
#define zher2k_ f2c_zher2k
#define strmm_ f2c_strmm
#define dtrmm_ f2c_dtrmm
#define ctrmm_ f2c_ctrmm
#define ztrmm_ f2c_ztrmm
#define strsm_ f2c_strsm
#define dtrsm_ f2c_dtrsm
#define ctrsm_ f2c_ctrsm
#define ztrsm_ f2c_ztrsm

#endif /* NO_BLAS_WRAP */

#endif /* __BLASWRAP_H */
