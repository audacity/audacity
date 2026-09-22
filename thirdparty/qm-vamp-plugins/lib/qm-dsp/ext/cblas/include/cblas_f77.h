/*
 * cblas_f77.h
 * Written by Keita Teranishi
 *
 * Updated by Jeff Horner
 * Merged cblas_f77.h and cblas_fortran_header.h
 */

#ifndef CBLAS_F77_H
#define CBLAS_F77_H

#ifdef CRAY
   #include <fortran.h>
   #define F77_CHAR _fcd
   #define C2F_CHAR(a) ( _cptofcd( (a), 1 ) )
   #define C2F_STR(a, i) ( _cptofcd( (a), (i) ) )
   #define F77_STRLEN(a) (_fcdlen)
#endif

#ifdef WeirdNEC
   #define F77_INT long
#endif

#ifdef  F77_CHAR
   #define FCHAR F77_CHAR
#else
   #define FCHAR char *
#endif

#ifdef F77_INT
   #define FINT const F77_INT *
   #define FINT2 F77_INT *
#else
   #define FINT const int *
   #define FINT2 int *
#endif

#if defined(ADD_)
/*
 * Level 1 BLAS
 */
#define F77_xerbla xerbla_
   #define F77_srotg      srotg_
   #define F77_srotmg     srotmg_
   #define F77_srot       srot_
   #define F77_srotm      srotm_
   #define F77_drotg      drotg_
   #define F77_drotmg     drotmg_
   #define F77_drot       drot_
   #define F77_drotm      drotm_
   #define F77_sswap      sswap_
   #define F77_scopy      scopy_
   #define F77_saxpy      saxpy_
   #define F77_isamax_sub isamaxsub_
   #define F77_dswap      dswap_
   #define F77_dcopy      dcopy_
   #define F77_daxpy      daxpy_
   #define F77_idamax_sub idamaxsub_
   #define F77_cswap      cswap_
   #define F77_ccopy      ccopy_
   #define F77_caxpy      caxpy_
   #define F77_icamax_sub icamaxsub_
   #define F77_zswap      zswap_
   #define F77_zcopy      zcopy_
   #define F77_zaxpy      zaxpy_
   #define F77_izamax_sub izamaxsub_
   #define F77_sdot_sub   sdotsub_
   #define F77_ddot_sub   ddotsub_
   #define F77_dsdot_sub   dsdotsub_
   #define F77_sscal      sscal_
   #define F77_dscal      dscal_
   #define F77_cscal      cscal_
   #define F77_zscal      zscal_
   #define F77_csscal      csscal_
   #define F77_zdscal      zdscal_
   #define F77_cdotu_sub  cdotusub_
   #define F77_cdotc_sub  cdotcsub_
   #define F77_zdotu_sub  zdotusub_
   #define F77_zdotc_sub  zdotcsub_
   #define F77_snrm2_sub  snrm2sub_
   #define F77_sasum_sub  sasumsub_
   #define F77_dnrm2_sub  dnrm2sub_
   #define F77_dasum_sub  dasumsub_
   #define F77_scnrm2_sub  scnrm2sub_
   #define F77_scasum_sub  scasumsub_
   #define F77_dznrm2_sub  dznrm2sub_
   #define F77_dzasum_sub  dzasumsub_
   #define F77_sdsdot_sub   sdsdotsub_
/*
 * Level 2 BLAS
 */
   #define F77_ssymv      ssymv_
   #define F77_ssbmv      ssbmv_
   #define F77_sspmv      sspmv_
   #define F77_sger       sger_
   #define F77_ssyr       ssyr_
   #define F77_sspr       sspr_
   #define F77_ssyr2      ssyr2_
   #define F77_sspr2      sspr2_
   #define F77_dsymv      dsymv_
   #define F77_dsbmv      dsbmv_
   #define F77_dspmv      dspmv_
   #define F77_dger       dger_
   #define F77_dsyr       dsyr_
   #define F77_dspr       dspr_
   #define F77_dsyr2      dsyr2_
   #define F77_dspr2      dspr2_
   #define F77_chemv      chemv_
   #define F77_chbmv      chbmv_
   #define F77_chpmv      chpmv_
   #define F77_cgeru      cgeru_
   #define F77_cgerc      cgerc_
   #define F77_cher       cher_
   #define F77_chpr       chpr_
   #define F77_cher2      cher2_
   #define F77_chpr2      chpr2_
   #define F77_zhemv      zhemv_
   #define F77_zhbmv      zhbmv_
   #define F77_zhpmv      zhpmv_
   #define F77_zgeru      zgeru_
   #define F77_zgerc      zgerc_
   #define F77_zher       zher_
   #define F77_zhpr       zhpr_
   #define F77_zher2      zher2_
   #define F77_zhpr2      zhpr2_
   #define F77_sgemv      sgemv_
   #define F77_sgbmv      sgbmv_
   #define F77_strmv      strmv_
   #define F77_stbmv      stbmv_
   #define F77_stpmv      stpmv_
   #define F77_strsv      strsv_
   #define F77_stbsv      stbsv_
   #define F77_stpsv      stpsv_
   #define F77_dgemv      dgemv_
   #define F77_dgbmv      dgbmv_
   #define F77_dtrmv      dtrmv_
   #define F77_dtbmv      dtbmv_
   #define F77_dtpmv      dtpmv_
   #define F77_dtrsv      dtrsv_
   #define F77_dtbsv      dtbsv_
   #define F77_dtpsv      dtpsv_
   #define F77_cgemv      cgemv_
   #define F77_cgbmv      cgbmv_
   #define F77_ctrmv      ctrmv_
   #define F77_ctbmv      ctbmv_
   #define F77_ctpmv      ctpmv_
   #define F77_ctrsv      ctrsv_
   #define F77_ctbsv      ctbsv_
   #define F77_ctpsv      ctpsv_
   #define F77_zgemv      zgemv_
   #define F77_zgbmv      zgbmv_
   #define F77_ztrmv      ztrmv_
   #define F77_ztbmv      ztbmv_
   #define F77_ztpmv      ztpmv_
   #define F77_ztrsv      ztrsv_
   #define F77_ztbsv      ztbsv_
   #define F77_ztpsv      ztpsv_
/*
 * Level 3 BLAS
 */
   #define F77_chemm      chemm_
   #define F77_cherk      cherk_
   #define F77_cher2k     cher2k_
   #define F77_zhemm      zhemm_
   #define F77_zherk      zherk_
   #define F77_zher2k     zher2k_
   #define F77_sgemm      sgemm_
   #define F77_ssymm      ssymm_
   #define F77_ssyrk      ssyrk_
   #define F77_ssyr2k     ssyr2k_
   #define F77_strmm      strmm_
   #define F77_strsm      strsm_
   #define F77_dgemm      dgemm_
   #define F77_dsymm      dsymm_
   #define F77_dsyrk      dsyrk_
   #define F77_dsyr2k     dsyr2k_
   #define F77_dtrmm      dtrmm_
   #define F77_dtrsm      dtrsm_
   #define F77_cgemm      cgemm_
   #define F77_csymm      csymm_
   #define F77_csyrk      csyrk_
   #define F77_csyr2k     csyr2k_
   #define F77_ctrmm      ctrmm_
   #define F77_ctrsm      ctrsm_
   #define F77_zgemm      zgemm_
   #define F77_zsymm      zsymm_
   #define F77_zsyrk      zsyrk_
   #define F77_zsyr2k     zsyr2k_
   #define F77_ztrmm      ztrmm_
   #define F77_ztrsm      ztrsm_
#elif defined(UPCASE)
/*
 * Level 1 BLAS
 */
#define F77_xerbla  XERBLA
   #define F77_srotg      SROTG
   #define F77_srotmg     SROTMG
   #define F77_srot       SROT
   #define F77_srotm      SROTM
   #define F77_drotg      DROTG
   #define F77_drotmg     DROTMG
   #define F77_drot       DROT
   #define F77_drotm      DROTM
   #define F77_sswap      SSWAP
   #define F77_scopy      SCOPY
   #define F77_saxpy      SAXPY
   #define F77_isamax_sub ISAMAXSUB
   #define F77_dswap      DSWAP
   #define F77_dcopy      DCOPY
   #define F77_daxpy      DAXPY
   #define F77_idamax_sub IDAMAXSUB
   #define F77_cswap      CSWAP
   #define F77_ccopy      CCOPY
   #define F77_caxpy      CAXPY
   #define F77_icamax_sub ICAMAXSUB
   #define F77_zswap      ZSWAP
   #define F77_zcopy      ZCOPY
   #define F77_zaxpy      ZAXPY
   #define F77_izamax_sub IZAMAXSUB
   #define F77_sdot_sub   SDOTSUB
   #define F77_ddot_sub   DDOTSUB
   #define F77_dsdot_sub   DSDOTSUB
   #define F77_sscal      SSCAL
   #define F77_dscal      DSCAL
   #define F77_cscal      CSCAL
   #define F77_zscal      ZSCAL
   #define F77_csscal      CSSCAL
   #define F77_zdscal      ZDSCAL
   #define F77_cdotu_sub  CDOTUSUB
   #define F77_cdotc_sub  CDOTCSUB
   #define F77_zdotu_sub  ZDOTUSUB
   #define F77_zdotc_sub  ZDOTCSUB
   #define F77_snrm2_sub  SNRM2SUB
   #define F77_sasum_sub  SASUMSUB
   #define F77_dnrm2_sub  DNRM2SUB
   #define F77_dasum_sub  DASUMSUB
   #define F77_scnrm2_sub  SCNRM2SUB
   #define F77_scasum_sub  SCASUMSUB
   #define F77_dznrm2_sub  DZNRM2SUB
   #define F77_dzasum_sub  DZASUMSUB
   #define F77_sdsdot_sub   SDSDOTSUB
/*
 * Level 2 BLAS
 */
   #define F77_ssymv      SSYMV
   #define F77_ssbmv      SSBMV
   #define F77_sspmv      SSPMV
   #define F77_sger       SGER
   #define F77_ssyr       SSYR
   #define F77_sspr       SSPR
   #define F77_ssyr2      SSYR2
   #define F77_sspr2      SSPR2
   #define F77_dsymv      DSYMV
   #define F77_dsbmv      DSBMV
   #define F77_dspmv      DSPMV
   #define F77_dger       DGER
   #define F77_dsyr       DSYR
   #define F77_dspr       DSPR
   #define F77_dsyr2      DSYR2
   #define F77_dspr2      DSPR2
   #define F77_chemv      CHEMV
   #define F77_chbmv      CHBMV
   #define F77_chpmv      CHPMV
   #define F77_cgeru      CGERU
   #define F77_cgerc      CGERC
   #define F77_cher       CHER
   #define F77_chpr       CHPR
   #define F77_cher2      CHER2
   #define F77_chpr2      CHPR2
   #define F77_zhemv      ZHEMV
   #define F77_zhbmv      ZHBMV
   #define F77_zhpmv      ZHPMV
   #define F77_zgeru      ZGERU
   #define F77_zgerc      ZGERC
   #define F77_zher       ZHER
   #define F77_zhpr       ZHPR
   #define F77_zher2      ZHER2
   #define F77_zhpr2      ZHPR2
   #define F77_sgemv      SGEMV
   #define F77_sgbmv      SGBMV
   #define F77_strmv      STRMV
   #define F77_stbmv      STBMV
   #define F77_stpmv      STPMV
   #define F77_strsv      STRSV
   #define F77_stbsv      STBSV
   #define F77_stpsv      STPSV
   #define F77_dgemv      DGEMV
   #define F77_dgbmv      DGBMV
   #define F77_dtrmv      DTRMV
   #define F77_dtbmv      DTBMV
   #define F77_dtpmv      DTPMV
   #define F77_dtrsv      DTRSV
   #define F77_dtbsv      DTBSV
   #define F77_dtpsv      DTPSV
   #define F77_cgemv      CGEMV
   #define F77_cgbmv      CGBMV
   #define F77_ctrmv      CTRMV
   #define F77_ctbmv      CTBMV
   #define F77_ctpmv      CTPMV
   #define F77_ctrsv      CTRSV
   #define F77_ctbsv      CTBSV
   #define F77_ctpsv      CTPSV
   #define F77_zgemv      ZGEMV
   #define F77_zgbmv      ZGBMV
   #define F77_ztrmv      ZTRMV
   #define F77_ztbmv      ZTBMV
   #define F77_ztpmv      ZTPMV
   #define F77_ztrsv      ZTRSV
   #define F77_ztbsv      ZTBSV
   #define F77_ztpsv      ZTPSV
/*
 * Level 3 BLAS
 */
   #define F77_chemm      CHEMM
   #define F77_cherk      CHERK
   #define F77_cher2k     CHER2K
   #define F77_zhemm      ZHEMM
   #define F77_zherk      ZHERK
   #define F77_zher2k     ZHER2K
   #define F77_sgemm      SGEMM
   #define F77_ssymm      SSYMM
   #define F77_ssyrk      SSYRK
   #define F77_ssyr2k     SSYR2K
   #define F77_strmm      STRMM
   #define F77_strsm      STRSM
   #define F77_dgemm      DGEMM
   #define F77_dsymm      DSYMM
   #define F77_dsyrk      DSYRK
   #define F77_dsyr2k     DSYR2K
   #define F77_dtrmm      DTRMM
   #define F77_dtrsm      DTRSM
   #define F77_cgemm      CGEMM
   #define F77_csymm      CSYMM
   #define F77_csyrk      CSYRK
   #define F77_csyr2k     CSYR2K
   #define F77_ctrmm      CTRMM
   #define F77_ctrsm      CTRSM
   #define F77_zgemm      ZGEMM
   #define F77_zsymm      ZSYMM
   #define F77_zsyrk      ZSYRK
   #define F77_zsyr2k     ZSYR2K
   #define F77_ztrmm      ZTRMM
   #define F77_ztrsm      ZTRSM
#elif defined(NOCHANGE)
/*
 * Level 1 BLAS
 */
#define F77_xerbla  xerbla
   #define F77_srotg      srotg
   #define F77_srotmg     srotmg
   #define F77_srot       srot
   #define F77_srotm      srotm
   #define F77_drotg      drotg
   #define F77_drotmg     drotmg
   #define F77_drot       drot
   #define F77_drotm      drotm
   #define F77_sswap      sswap
   #define F77_scopy      scopy
   #define F77_saxpy      saxpy
   #define F77_isamax_sub isamaxsub
   #define F77_dswap      dswap
   #define F77_dcopy      dcopy
   #define F77_daxpy      daxpy
   #define F77_idamax_sub idamaxsub
   #define F77_cswap      cswap
   #define F77_ccopy      ccopy
   #define F77_caxpy      caxpy
   #define F77_icamax_sub icamaxsub
   #define F77_zswap      zswap
   #define F77_zcopy      zcopy
   #define F77_zaxpy      zaxpy
   #define F77_izamax_sub izamaxsub
   #define F77_sdot_sub   sdotsub
   #define F77_ddot_sub   ddotsub
   #define F77_dsdot_sub   dsdotsub
   #define F77_sscal      sscal
   #define F77_dscal      dscal
   #define F77_cscal      cscal
   #define F77_zscal      zscal
   #define F77_csscal      csscal
   #define F77_zdscal      zdscal
   #define F77_cdotu_sub  cdotusub
   #define F77_cdotc_sub  cdotcsub
   #define F77_zdotu_sub  zdotusub
   #define F77_zdotc_sub  zdotcsub
   #define F77_snrm2_sub  snrm2sub
   #define F77_sasum_sub  sasumsub
   #define F77_dnrm2_sub  dnrm2sub
   #define F77_dasum_sub  dasumsub
   #define F77_scnrm2_sub  scnrm2sub
   #define F77_scasum_sub  scasumsub
   #define F77_dznrm2_sub  dznrm2sub
   #define F77_dzasum_sub  dzasumsub
   #define F77_sdsdot_sub   sdsdotsub
/*
 * Level 2 BLAS
 */
   #define F77_ssymv      ssymv
   #define F77_ssbmv      ssbmv
   #define F77_sspmv      sspmv
   #define F77_sger       sger
   #define F77_ssyr       ssyr
   #define F77_sspr       sspr
   #define F77_ssyr2      ssyr2
   #define F77_sspr2      sspr2
   #define F77_dsymv      dsymv
   #define F77_dsbmv      dsbmv
   #define F77_dspmv      dspmv
   #define F77_dger       dger
   #define F77_dsyr       dsyr
   #define F77_dspr       dspr
   #define F77_dsyr2      dsyr2
   #define F77_dspr2      dspr2
   #define F77_chemv      chemv
   #define F77_chbmv      chbmv
   #define F77_chpmv      chpmv
   #define F77_cgeru      cgeru
   #define F77_cgerc      cgerc
   #define F77_cher       cher
   #define F77_chpr       chpr
   #define F77_cher2      cher2
   #define F77_chpr2      chpr2
   #define F77_zhemv      zhemv
   #define F77_zhbmv      zhbmv
   #define F77_zhpmv      zhpmv
   #define F77_zgeru      zgeru
   #define F77_zgerc      zgerc
   #define F77_zher       zher
   #define F77_zhpr       zhpr
   #define F77_zher2      zher2
   #define F77_zhpr2      zhpr2
   #define F77_sgemv      sgemv
   #define F77_sgbmv      sgbmv
   #define F77_strmv      strmv
   #define F77_stbmv      stbmv
   #define F77_stpmv      stpmv
   #define F77_strsv      strsv
   #define F77_stbsv      stbsv
   #define F77_stpsv      stpsv
   #define F77_dgemv      dgemv
   #define F77_dgbmv      dgbmv
   #define F77_dtrmv      dtrmv
   #define F77_dtbmv      dtbmv
   #define F77_dtpmv      dtpmv
   #define F77_dtrsv      dtrsv
   #define F77_dtbsv      dtbsv
   #define F77_dtpsv      dtpsv
   #define F77_cgemv      cgemv
   #define F77_cgbmv      cgbmv
   #define F77_ctrmv      ctrmv
   #define F77_ctbmv      ctbmv
   #define F77_ctpmv      ctpmv
   #define F77_ctrsv      ctrsv
   #define F77_ctbsv      ctbsv
   #define F77_ctpsv      ctpsv
   #define F77_zgemv      zgemv
   #define F77_zgbmv      zgbmv
   #define F77_ztrmv      ztrmv
   #define F77_ztbmv      ztbmv
   #define F77_ztpmv      ztpmv
   #define F77_ztrsv      ztrsv
   #define F77_ztbsv      ztbsv
   #define F77_ztpsv      ztpsv
/*
 * Level 3 BLAS
 */
   #define F77_chemm      chemm
   #define F77_cherk      cherk
   #define F77_cher2k     cher2k
   #define F77_zhemm      zhemm
   #define F77_zherk      zherk
   #define F77_zher2k     zher2k
   #define F77_sgemm      sgemm
   #define F77_ssymm      ssymm
   #define F77_ssyrk      ssyrk
   #define F77_ssyr2k     ssyr2k
   #define F77_strmm      strmm
   #define F77_strsm      strsm
   #define F77_dgemm      dgemm
   #define F77_dsymm      dsymm
   #define F77_dsyrk      dsyrk
   #define F77_dsyr2k     dsyr2k
   #define F77_dtrmm      dtrmm
   #define F77_dtrsm      dtrsm
   #define F77_cgemm      cgemm
   #define F77_csymm      csymm
   #define F77_csyrk      csyrk
   #define F77_csyr2k     csyr2k
   #define F77_ctrmm      ctrmm
   #define F77_ctrsm      ctrsm
   #define F77_zgemm      zgemm
   #define F77_zsymm      zsymm
   #define F77_zsyrk      zsyrk
   #define F77_zsyr2k     zsyr2k
   #define F77_ztrmm      ztrmm
   #define F77_ztrsm      ztrsm
#endif

#ifdef __cplusplus
extern "C" {
#endif

   void F77_xerbla(FCHAR, void *);
/*
 * Level 1 Fortran Prototypes
 */

/* Single Precision */

   void F77_srot(FINT, float *, FINT, float *, FINT, const float *, const float *);
   void F77_srotg(float *,float *,float *,float *);    
   void F77_srotm( FINT, float *, FINT, float *, FINT, const float *);
   void F77_srotmg(float *,float *,float *,const float *, float *);
   void F77_sswap( FINT, float *, FINT, float *, FINT);
   void F77_scopy( FINT, const float *, FINT, float *, FINT);
   void F77_saxpy( FINT, const float *, const float *, FINT, float *, FINT);
   void F77_sdot_sub(FINT, const float *, FINT, const float *, FINT, float *);
   void F77_sdsdot_sub( FINT, const float *, const float *, FINT, const float *, FINT, float *);
   void F77_sscal( FINT, const float *, float *, FINT);
   void F77_snrm2_sub( FINT, const float *, FINT, float *);
   void F77_sasum_sub( FINT, const float *, FINT, float *);
   void F77_isamax_sub( FINT, const float * , FINT, FINT2);

/* Double Precision */

   void F77_drot(FINT, double *, FINT, double *, FINT, const double *, const double *);
   void F77_drotg(double *,double *,double *,double *);    
   void F77_drotm( FINT, double *, FINT, double *, FINT, const double *);
   void F77_drotmg(double *,double *,double *,const double *, double *);
   void F77_dswap( FINT, double *, FINT, double *, FINT);
   void F77_dcopy( FINT, const double *, FINT, double *, FINT);
   void F77_daxpy( FINT, const double *, const double *, FINT, double *, FINT);
   void F77_dswap( FINT, double *, FINT, double *, FINT);
   void F77_dsdot_sub(FINT, const float *, FINT, const float *, FINT, double *);
   void F77_ddot_sub( FINT, const double *, FINT, const double *, FINT, double *);
   void F77_dscal( FINT, const double *, double *, FINT);
   void F77_dnrm2_sub( FINT, const double *, FINT, double *);
   void F77_dasum_sub( FINT, const double *, FINT, double *);
   void F77_idamax_sub( FINT, const double * , FINT, FINT2);

/* Single Complex Precision */

   void F77_cswap( FINT, void *, FINT, void *, FINT);
   void F77_ccopy( FINT, const void *, FINT, void *, FINT);
   void F77_caxpy( FINT, const void *, const void *, FINT, void *, FINT);
   void F77_cswap( FINT, void *, FINT, void *, FINT);
   void F77_cdotc_sub( FINT, const void *, FINT, const void *, FINT, void *);
   void F77_cdotu_sub( FINT, const void *, FINT, const void *, FINT, void *);
   void F77_cscal( FINT, const void *, void *, FINT);
   void F77_icamax_sub( FINT, const void *, FINT, FINT2);
   void F77_csscal( FINT, const float *, void *, FINT);
   void F77_scnrm2_sub( FINT, const void *, FINT, float *);
   void F77_scasum_sub( FINT, const void *, FINT, float *);

/* Double Complex Precision */

   void F77_zswap( FINT, void *, FINT, void *, FINT);
   void F77_zcopy( FINT, const void *, FINT, void *, FINT);
   void F77_zaxpy( FINT, const void *, const void *, FINT, void *, FINT);
   void F77_zswap( FINT, void *, FINT, void *, FINT);
   void F77_zdotc_sub( FINT, const void *, FINT, const void *, FINT, void *);
   void F77_zdotu_sub( FINT, const void *, FINT, const void *, FINT, void *);
   void F77_zdscal( FINT, const double *, void *, FINT);
   void F77_zscal( FINT, const void *, void *, FINT);
   void F77_dznrm2_sub( FINT, const void *, FINT, double *);
   void F77_dzasum_sub( FINT, const void *, FINT, double *);
   void F77_izamax_sub( FINT, const void *, FINT, FINT2);

/*
 * Level 2 Fortran Prototypes
 */

/* Single Precision */

   void F77_sgemv(FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_sgbmv(FCHAR, FINT, FINT, FINT, FINT, const float *,  const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_ssymv(FCHAR, FINT, const float *, const float *, FINT, const float *,  FINT, const float *, float *, FINT);
   void F77_ssbmv(FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_sspmv(FCHAR, FINT, const float *, const float *, const float *, FINT, const float *, float *, FINT);
   void F77_strmv( FCHAR, FCHAR, FCHAR, FINT, const float *, FINT, float *, FINT);
   void F77_stbmv( FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, FINT, float *, FINT);
   void F77_strsv( FCHAR, FCHAR, FCHAR, FINT, const float *, FINT, float *, FINT);
   void F77_stbsv( FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, FINT, float *, FINT);
   void F77_stpmv( FCHAR, FCHAR, FCHAR, FINT, const float *, float *, FINT);
   void F77_stpsv( FCHAR, FCHAR, FCHAR, FINT, const float *, float *, FINT);
   void F77_sger( FINT, FINT, const float *, const float *, FINT, const float *, FINT, float *, FINT);
   void F77_ssyr(FCHAR, FINT, const float *, const float *, FINT, float *, FINT);
   void F77_sspr(FCHAR, FINT, const float *, const float *, FINT, float *); 
   void F77_sspr2(FCHAR, FINT, const float *, const float *, FINT, const float *, FINT,  float *); 
   void F77_ssyr2(FCHAR, FINT, const float *, const float *, FINT, const float *, FINT,  float *, FINT);

/* Double Precision */

   void F77_dgemv(FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_dgbmv(FCHAR, FINT, FINT, FINT, FINT, const double *,  const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_dsymv(FCHAR, FINT, const double *, const double *, FINT, const double *,  FINT, const double *, double *, FINT);
   void F77_dsbmv(FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_dspmv(FCHAR, FINT, const double *, const double *, const double *, FINT, const double *, double *, FINT);
   void F77_dtrmv( FCHAR, FCHAR, FCHAR, FINT, const double *, FINT, double *, FINT);
   void F77_dtbmv( FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, FINT, double *, FINT);
   void F77_dtrsv( FCHAR, FCHAR, FCHAR, FINT, const double *, FINT, double *, FINT);
   void F77_dtbsv( FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, FINT, double *, FINT);
   void F77_dtpmv( FCHAR, FCHAR, FCHAR, FINT, const double *, double *, FINT);
   void F77_dtpsv( FCHAR, FCHAR, FCHAR, FINT, const double *, double *, FINT);
   void F77_dger( FINT, FINT, const double *, const double *, FINT, const double *, FINT, double *, FINT);
   void F77_dsyr(FCHAR, FINT, const double *, const double *, FINT, double *, FINT);
   void F77_dspr(FCHAR, FINT, const double *, const double *, FINT, double *); 
   void F77_dspr2(FCHAR, FINT, const double *, const double *, FINT, const double *, FINT,  double *); 
   void F77_dsyr2(FCHAR, FINT, const double *, const double *, FINT, const double *, FINT,  double *, FINT);

/* Single Complex Precision */

   void F77_cgemv(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_cgbmv(FCHAR, FINT, FINT, FINT, FINT, const void *,  const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_chemv(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_chbmv(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_chpmv(FCHAR, FINT, const void *, const void *, const void *, FINT, const void *, void *, FINT);
   void F77_ctrmv( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void F77_ctbmv( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void F77_ctpmv( FCHAR, FCHAR, FCHAR, FINT, const void *, void *, FINT);
   void F77_ctrsv( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void F77_ctbsv( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void F77_ctpsv( FCHAR, FCHAR, FCHAR, FINT, const void *, void *,FINT);
   void F77_cgerc( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void F77_cgeru( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *,  FINT);
   void F77_cher(FCHAR, FINT, const float *, const void *, FINT, void *, FINT);
   void F77_cher2(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void F77_chpr(FCHAR, FINT, const float *, const void *, FINT, void *);
   void F77_chpr2(FCHAR, FINT, const float *, const void *, FINT, const void *, FINT, void *);

/* Double Complex Precision */

   void F77_zgemv(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_zgbmv(FCHAR, FINT, FINT, FINT, FINT, const void *,  const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_zhemv(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_zhbmv(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void F77_zhpmv(FCHAR, FINT, const void *, const void *, const void *, FINT, const void *, void *, FINT);
   void F77_ztrmv( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void F77_ztbmv( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void F77_ztpmv( FCHAR, FCHAR, FCHAR, FINT, const void *, void *, FINT);
   void F77_ztrsv( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void F77_ztbsv( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void F77_ztpsv( FCHAR, FCHAR, FCHAR, FINT, const void *, void *,FINT);
   void F77_zgerc( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void F77_zgeru( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *,  FINT);
   void F77_zher(FCHAR, FINT, const double *, const void *, FINT, void *, FINT);
   void F77_zher2(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void F77_zhpr(FCHAR, FINT, const double *, const void *, FINT, void *);
   void F77_zhpr2(FCHAR, FINT, const double *, const void *, FINT, const void *, FINT, void *);

/*
 * Level 3 Fortran Prototypes
 */

/* Single Precision */

   void F77_sgemm(FCHAR, FCHAR, FINT, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_ssymm(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_ssyrk(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT);
   void F77_ssyr2k(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_strmm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);
   void F77_strsm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);

/* Double Precision */

   void F77_dgemm(FCHAR, FCHAR, FINT, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_dsymm(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_dsyrk(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT);
   void F77_dsyr2k(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_dtrmm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);
   void F77_dtrsm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);

/* Single Complex Precision */

   void F77_cgemm(FCHAR, FCHAR, FINT, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_csymm(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_chemm(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_csyrk(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT);
   void F77_cherk(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT);
   void F77_csyr2k(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_cher2k(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void F77_ctrmm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);
   void F77_ctrsm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);

/* Double Complex Precision */

   void F77_zgemm(FCHAR, FCHAR, FINT, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_zsymm(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_zhemm(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_zsyrk(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT);
   void F77_zherk(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT);
   void F77_zsyr2k(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_zher2k(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void F77_ztrmm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);
   void F77_ztrsm(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);

#ifdef __cplusplus
}
#endif

#endif /*  CBLAS_F77_H */
