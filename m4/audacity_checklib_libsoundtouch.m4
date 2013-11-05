dnl Add Audacity / Soundtouch license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsoundtouch.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOUNDTOUCH], [
   AC_ARG_WITH(soundtouch,
               [AS_HELP_STRING([--with-soundtouch],
                      [use libSoundTouch for pitch and tempo changing])],
               LIBSOUNDTOUCH_ARGUMENT=$withval,
               LIBSOUNDTOUCH_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_SOUNDTOUCH, 1,
                [Define if SoundTouch support should be enabled])
   fi

   dnl see if soundtouch is installed on the system
   dnl Fustratingly, the name of the pkg-config file keeps being changed
   dnl by upstream, despite the fact that they don't need to and shouldn't.
   dnl as a result (given that 1.3.x and 1.4.0 seem to have idenditcal APIs)
   dnl we have to check for two possible pkg-config files for the same package.

   dnl check for 1.4.x first
   PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch-1.4 >= 1.3.0,
                     soundtouch_available_system="yes",
                     soundtouch_available_system="no")
   dnl if not there, check for 1.3.x
   if test "x$soundtouch_available_system" = "xno" ; then
      PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch-1.0 >= 1.3.0,
                     soundtouch_available_system="yes",
                     soundtouch_available_system="no")
   fi
   if test "x$soundtouch_available_system" = "xno" ; then
      PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch >= 1.3.0,
                     soundtouch_available_system="yes",
                     soundtouch_available_system="no")
   fi

   if test "x$soundtouch_available_system" = "xyes" ; then
      LIBSOUNDTOUCH_SYSTEM_AVAILABLE="yes"
      LIBSOUNDTOUCH_SYSTEM_LIBS=$SOUNDTOUCH_LIBS
      LIBSOUNDTOUCH_SYSTEM_CXXFLAGS=$SOUNDTOUCH_CFLAGS
      LIBSOUNDTOUCH_SYSTEM_CPPSYMBOLS="USE_SOUNDTOUCH"
      AC_MSG_NOTICE([Libsoundtouch libraries are available as system libraries])
   else
      LIBSOUNDTOUCH_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsoundtouch libraries are NOT available as system libraries])
   fi

   dnl see if libsoundtouch is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/soundtouch/include/SoundTouch.h,
                 soundtouch_h_found="yes",
                 soundtouch_h_found="no")

   if test "x$soundtouch_h_found" = "xyes" ; then
      LIBSOUNDTOUCH_LOCAL_AVAILABLE="yes"
      LIBSOUNDTOUCH_LOCAL_LIBS="libSoundTouch.a"
      LIBSOUNDTOUCH_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/soundtouch/include'
      LIBSOUNDTOUCH_LOCAL_CPPSYMBOLS="USE_SOUNDTOUCH"
      AC_MSG_NOTICE([libsoundtouch libraries are available in the local tree])
   else
      LIBSOUNDTOUCH_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsoundtouch libraries are NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBSOUNDTOUCH], [
   if test "$LIBSOUNDTOUCH_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/soundtouch])
   fi
])
