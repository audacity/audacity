dnl @synopsis AX_GCC_VERSION(MAJOR, MINOR, PATCHLEVEL, [ACTION-SUCCESS], [ACTION-FAILURE])
dnl @summary check wither gcc is at least version MAJOR.MINOR.PATCHLEVEL
dnl @category InstalledPackages
dnl
dnl Check whether we are using gcc and, if so, whether its version
dnl is at least MAJOR.MINOR.PATCHLEVEL
dnl
dnl ACTION-SUCCESS/ACTION-FAILURE are shell commands to execute on
dnl success/failure.
dnl
dnl @version 2005-05-30
dnl @license GPLWithACException
dnl @author Steven G. Johnson <stevenj@alum.mit.edu> and Matteo Frigo.
AC_DEFUN([AX_GCC_VERSION_ATLEAST],
[
AC_REQUIRE([AC_PROG_CC])
AC_CACHE_CHECK(whether we are using gcc $1.$2.$3 or later, ax_cv_gcc_$1_$2_$3,
[
ax_cv_gcc_$1_$2_$3=no
if test "$GCC" = "yes"; then
dnl The semicolon after "yes" below is to pacify NeXT's syntax-checking cpp.
AC_EGREP_CPP(yes, [
#ifdef __GNUC__
#  if (__GNUC__ > $1) || (__GNUC__ == $1 && __GNUC_MINOR__ > $2) \
   || (__GNUC__ == $1 && __GNUC_MINOR__ == $2 && __GNUC_PATCHLEVEL__ >= $3)
     yes;
#  endif
#endif
], [ax_cv_gcc_$1_$2_$3=yes])
fi
])
if test "$ax_cv_gcc_$1_$2_$3" = yes; then
	m4_default([$4], :)
else
	m4_default([$5], :)
fi
])

