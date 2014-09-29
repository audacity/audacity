dnl Add audacity / libmad licence?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libid3tag.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_LIBID3TAG], [

   AC_ARG_WITH(libid3tag,
               [AS_HELP_STRING([--with-libid3tag],
                               [use libid3tag for mp3 id3 tag support])],
               LIBID3TAG_ARGUMENT=$withval,
               LIBID3TAG_ARGUMENT="unspecified")

   dnl see if libid3tag is installed on the system

   PKG_CHECK_MODULES(ID3TAG, id3tag,
                     LIBID3TAG_SYSTEM_AVAILABLE="yes",
                     LIBID3TAG_SYSTEM_AVAILABLE="no")

   if test "$LIBID3TAG_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Libid3tag libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Libid3tag libraries are NOT available as system libraries])
   fi

   dnl see if libid3tag is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libid3tag/frame.h,
                 LIBID3TAG_LOCAL_AVAILABLE="yes",
                 LIBID3TAG_LOCAL_AVAILABLE="no")

   if test "$LIBID3TAG_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([libid3tag libraries are available in the local tree])
   else
      AC_MSG_NOTICE([libid3tag libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBID3TAG], [
   if test "$LIBID3TAG_USE_LOCAL" = yes; then
      ID3TAG_CFLAGS='-I$(top_srcdir)/lib-src/libid3tag'
      ID3TAG_LIBS='$(top_builddir)/lib-src/libid3tag/libid3tag.la'
      AC_CONFIG_SUBDIRS([lib-src/libid3tag])
   fi

   AC_SUBST([ID3TAG_CFLAGS])
   AC_SUBST([ID3TAG_LIBS])

   AM_CONDITIONAL([USE_LIBID3TAG], [test "$LIBID3TAG_USE_LOCAL" = yes -o "$LIBID3TAG_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBID3TAG], [test "$LIBID3TAG_USE_LOCAL" = yes])

   if test "$LIBID3TAG_USE_LOCAL" = yes -o "$LIBID3TAG_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBID3TAG, 1,
                [Define if libid3tag is present])
   fi
])
