dnl Add audacity / libsndfile license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsndfile.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBSNDFILE], [

   AC_ARG_WITH(libsndfile,
               [AS_HELP_STRING([--with-libsndfile],
                               [which libsndfile to use (required): [system,local]])],
               LIBSNDFILE_ARGUMENT=$withval,
               LIBSNDFILE_ARGUMENT="unspecified")

   dnl see if libsndfile is installed in the system

   PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.0,
                     LIBSNDFILE_SYSTEM_AVAILABLE="yes",
                     LIBSNDFILE_SYSTEM_AVAILABLE="no")

   if test "$LIBSNDFILE_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Libsndfile libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Libsndfile libraries are NOT available as system libraries])
   fi

   dnl see if libsndfile is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsndfile/src/sndfile.h.in,
                 LIBSNDFILE_LOCAL_AVAILABLE="yes",
                 LIBSNDFILE_LOCAL_AVAILABLE="no")

   if test "$LIBSNDFILE_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([libsndfile libraries are available in this source tree])

      dnl These must be visible so libvamp and sbsms can find us
      dnl export SNDFILE_LIBS="'`pwd`/lib-src/libsndfile.a'"
      dnl export SNDFILE_CFLAGS="'-I`pwd`/lib-src/libsndfile/src'"

      dnl Temporary fix for bug #248
      LIBSNDFILE_LOCAL_CONFIGURE_ARGS="--disable-sqlite --disable-external-libs --disable-alsa"
   else
      AC_MSG_NOTICE([libsndfile libraries are NOT available in this source tree])
   fi
   LIBSNDFILE_MIMETYPES="audio/basic;audio/x-aiff;audio/x-wav;"
])

AC_DEFUN([AUDACITY_CONFIG_LIBSNDFILE], [
   if test "$LIBSNDFILE_USE_LOCAL" = yes; then
      SNDFILE_CFLAGS='-I$(top_srcdir)/lib-src/libsndfile/src'
      SNDFILE_LIBS='$(top_builddir)/lib-src/libsndfile/src/.libs/libsndfile.a'
      AC_CONFIG_SUBDIRS([lib-src/libsndfile])
   fi

   AC_SUBST([SNDFILE_CFLAGS])
   AC_SUBST([SNDFILE_LIBS])

   AM_CONDITIONAL([USE_LOCAL_LIBSNDFILE], [test "$LIBSNDFILE_USE_LOCAL" = yes])
])
