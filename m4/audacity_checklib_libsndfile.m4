dnl Add audacity / libsndfile license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsndfile.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_LIBSNDFILE], [

   AC_ARG_WITH(libsndfile,
               [AS_HELP_STRING([--with-libsndfile],
                               [which libsndfile to use (required): [system,local]])],
               LIBSNDFILE_ARGUMENT=$withval,
               LIBSNDFILE_ARGUMENT="unspecified")

   dnl see if libsndfile is installed in the system

   PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.0,
                     sndfile_available_system="yes",
                     sndfile_available_system="no")

   if test "x$sndfile_available_system" = "xyes" ; then
      LIBSNDFILE_SYSTEM_AVAILABLE="yes"
      LIBSNDFILE_SYSTEM_LIBS=$SNDFILE_LIBS
      LIBSNDFILE_SYSTEM_CXXFLAGS=$SNDFILE_CFLAGS
      AC_MSG_NOTICE([Libsndfile libraries are available as system libraries])
   else
      LIBSNDFILE_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsndfile libraries are NOT available as system libraries])
   fi

   dnl see if libsndfile is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsndfile/src/sndfile.h.in,
                 libsndfile_found="yes",
                 libsndfile_found="no")

   if test "x$libsndfile_found" = "xyes" ; then
      LIBSNDFILE_LOCAL_AVAILABLE="yes"
      LIBSNDFILE_LOCAL_LIBS="libsndfile.a"
      LIBSNDFILE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsndfile/src'
      AC_MSG_NOTICE([libsndfile libraries are available in this source tree])

      dnl These must be visible so libvamp and sbsms can find us
      dnl export SNDFILE_LIBS="'`pwd`/lib-src/libsndfile.a'"
      dnl export SNDFILE_CFLAGS="'-I`pwd`/lib-src/libsndfile/src'"

      dnl Temporary fix for bug #248
      LIBSNDFILE_LOCAL_CONFIGURE_ARGS="--disable-sqlite --disable-external-libs --disable-alsa"
   else
      LIBSNDFILE_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsndfile libraries are NOT available in this source tree])
   fi
   LIBSNDFILE_MIMETYPES="audio/basic;audio/x-aiff;audio/x-wav;"
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBSNDFILE], [
   if test "$LIBSNDFILE_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/libsndfile])
   fi
])
