dnl @synopsis AX_COMPILER_VENDOR
dnl @summary find the vendor (gnu, intel, etc.) of the C/C++ compiler
dnl @category C
dnl @category C++
dnl
dnl Determine the vendor of the C/C++ compiler, e.g., gnu, intel, ibm,
dnl sun, hp, borland, comeau, dec, cray, kai, lcc, metrowerks, sgi, 
dnl microsoft, watcom, etc.  The vendor is returned in the cache variable
dnl $ax_cv_c_compiler_vendor for C and $ax_cv_cxx_compiler_vendor for C++.
dnl
dnl @version 2007-08-01
dnl @license GPLWithACException
dnl @author Steven G. Johnson <stevenj@alum.mit.edu> with Matteo Frigo

AC_DEFUN([AX_COMPILER_VENDOR],
[
AC_CACHE_CHECK([for _AC_LANG compiler vendor], ax_cv_[]_AC_LANG_ABBREV[]_compiler_vendor,
 [ax_cv_[]_AC_LANG_ABBREV[]_compiler_vendor=unknown
  # note: don't check for gcc first since some other compilers define __GNUC__
  for ventest in intel:__ICC,__ECC,__INTEL_COMPILER ibm:__xlc__,__xlC__,__IBMC__,__IBMCPP__ pathscale:__PATHCC__,__PATHSCALE__ gnu:__GNUC__ sun:__SUNPRO_C,__SUNPRO_CC hp:__HP_cc,__HP_aCC dec:__DECC,__DECCXX,__DECC_VER,__DECCXX_VER borland:__BORLANDC__,__TURBOC__ comeau:__COMO__ cray:_CRAYC kai:__KCC lcc:__LCC__ metrowerks:__MWERKS__ sgi:__sgi,sgi microsoft:_MSC_VER watcom:__WATCOMC__ portland:__PGI; do 
    vencpp="defined("`echo $ventest | cut -d: -f2 | sed 's/,/) || defined(/g'`")"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[
#if !($vencpp)
      thisisanerror;
#endif
])], [ax_cv_]_AC_LANG_ABBREV[_compiler_vendor=`echo $ventest | cut -d: -f1`; break])
  done
 ])
])

