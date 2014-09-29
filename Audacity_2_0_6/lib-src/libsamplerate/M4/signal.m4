dnl @synopsis AC_CHECK_SIGNAL(SIGNAME)
dnl
dnl
dnl @category C
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl @version 1.0	Jul 07 2007

AC_DEFUN([AC_CHECK_SIGNAL],
[AC_CACHE_CHECK(for $1,
  ac_cv_signal_$1,
[
AC_TRY_LINK([
#include <signal.h>

], signal($1, SIG_DFL) ;, ac_cv_signal_$1=yes, ac_cv_signal_$1=no)

])

if test "$ac_cv_signal_$1" = yes; then
  AC_DEFINE(HAVE_$1, 1,
            [Define if you have signal $1.])
fi
])# AC_CHECK_SIGNAL

