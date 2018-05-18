dnl add audacity / liblo license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_liblo.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBLO], [
   AC_ARG_WITH(liblo,
               [AS_HELP_STRING([--without-liblo],
                               [disable support for liblo, OSC (Open Sound Control) support])],
               [LIBLO_ARGUMENT=$withval],
               [LIBLO_ARGUMENT=yes])

   dnl See if LIBLO is installed in the system and use it
   if test "x$LIBLO_ARGUMENT" = "xyes"; then
      PKG_CHECK_MODULES([LIBLO], [liblo >= 0.26],
                        [LIBLO_SYSTEM_AVAILABLE="yes"],
                        [LIBLO_SYSTEM_AVAILABLE="no"])

      if test "$LIBLO_SYSTEM_AVAILABLE" = "yes"; then
         AC_MSG_NOTICE([LIBLO libraries are available as system libraries])
         LIBLO_SYSTEM_LIBS=$LIBLO_LIBS
      else
         AC_MSG_ERROR([LIBLO libraries are NOT available as system libraries])
      fi
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBLO], [
   AM_CONDITIONAL([USE_LIBLO], [test "$LIBLO_USE_SYSTEM" = yes])

   if test "$LIBLO_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBLO, 1,
                [Define if the LIBLO library is present])
   fi
])
