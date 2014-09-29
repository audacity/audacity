dnl Add Audacity / Soundtouch license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsoundtouch.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOUNDTOUCH], [
   AC_ARG_WITH(soundtouch,
               [AS_HELP_STRING([--with-soundtouch],
                      [use libSoundTouch for pitch and tempo changing])],
               LIBSOUNDTOUCH_ARGUMENT=$withval,
               LIBSOUNDTOUCH_ARGUMENT="unspecified")

   dnl see if soundtouch is installed on the system
   dnl Fustratingly, the name of the pkg-config file keeps being changed
   dnl by upstream, despite the fact that they don't need to and shouldn't.
   dnl as a result (given that 1.3.x and 1.4.0 seem to have idenditcal APIs)
   dnl we have to check for two possible pkg-config files for the same package.

   PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch >= 1.3.0,
                     LIBSOUNDTOUCH_SYSTEM_AVAILABLE="yes",
                     LIBSOUNDTOUCH_SYSTEM_AVAILABLE="no")
   dnl if not there, check for 1.4.x
   if test "$LIBSOUNDTOUCH_SYSTEM_AVAILABLE" = "no"; then
      PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch-1.4 >= 1.3.0,
                     LIBSOUNDTOUCH_SYSTEM_AVAILABLE="yes",
                     LIBSOUNDTOUCH_SYSTEM_AVAILABLE="no")
   fi
   dnl if not there, check for 1.3.x
   if test "$LIBSOUNDTOUCH_SYSTEM_AVAILABLE" = "no"; then
      PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch-1.0 >= 1.3.0,
                     LIBSOUNDTOUCH_SYSTEM_AVAILABLE="yes",
                     LIBSOUNDTOUCH_SYSTEM_AVAILABLE="no")
   fi

   if test "$LIBSOUNDTOUCH_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Libsoundtouch libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Libsoundtouch libraries are NOT available as system libraries])
   fi

   dnl see if libsoundtouch is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/soundtouch/include/SoundTouch.h,
                 LIBSOUNDTOUCH_LOCAL_AVAILABLE="yes",
                 LIBSOUNDTOUCH_LOCAL_AVAILABLE="no")

   if test "$LIBSOUNDTOUCH_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([libsoundtouch libraries are available in the local tree])
   else
      AC_MSG_NOTICE([libsoundtouch libraries are NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_LIBSOUNDTOUCH], [
   if test "$LIBSOUNDTOUCH_USE_LOCAL" = yes; then
      SOUNDTOUCH_CFLAGS='-I$(top_srcdir)/lib-src/soundtouch/include'
      SOUNDTOUCH_LIBS='$(top_builddir)/lib-src/soundtouch/source/SoundTouch/.libs/libSoundTouch.a'
      AC_CONFIG_SUBDIRS([lib-src/soundtouch])
   fi

   AC_SUBST([SOUNDTOUCH_CFLAGS])
   AC_SUBST([SOUNDTOUCH_LIBS])

   AM_CONDITIONAL([USE_LIBSOUNDTOUCH], [test "$LIBSOUNDTOUCH_USE_LOCAL" = yes -o "$LIBSOUNDTOUCH_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBSOUNDTOUCH], [test "$LIBSOUNDTOUCH_USE_LOCAL" = yes])

   if test "$LIBSOUNDTOUCH_USE_LOCAL" = yes -o "$LIBSOUNDTOUCH_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_SOUNDTOUCH, 1,
                [Define if SoundTouch support should be enabled])
   fi
])
