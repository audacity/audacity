dnl Todo: Add Audacity / FFmpeg license
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_ffmpeg.m4 serial 2
dnl Check for a copy of ffmpeg, whoose headers we will use for the importer
AC_DEFUN([AUDACITY_CHECKLIB_FFMPEG], [
   AC_ARG_WITH(ffmpeg,
               [AS_HELP_STRING([--with-ffmpeg],
                               [use ffmpeg for import and export support ])],
               FFMPEG_ARGUMENT=$withval,
               FFMPEG_ARGUMENT="unspecified")

   dnl Check for a system copy of ffmpeg to use. For now I'm insiting on a
   dnl current version to make maintenance easier. We need both avcodec and
   dnl avformat, so I'm going to check for both

   PKG_CHECK_MODULES(AVCODEC, libavcodec >= 51.53,
                     avcodec_available_system="yes",
                     avcodec_available_system="no")
   PKG_CHECK_MODULES(AVFORMAT, libavformat >= 52.12,
                     avformat_available_system="yes",
                     avformat_available_system="no")
   PKG_CHECK_MODULES(AVUTIL, libavutil,
                     libavutil_available_system="yes",
                     libavutil_available_system="no")

   if test "$avcodec_available_system" = "yes" -a "$avformat_available_system" = "yes" -a "$libavutil_available_system" = "yes"; then
      FFMPEG_SYSTEM_AVAILABLE="yes"
      AC_MSG_NOTICE([FFmpeg library available as system library])
   else
      FFMPEG_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([FFmpeg library NOT available as system library])
   fi

   dnl see if ffmpeg is available locally, or rather that we have some headers
   dnl in lib-src/ffmpeg/ we can use.
   AC_CHECK_FILE(${srcdir}/lib-src/ffmpeg/libavcodec/avcodec.h,
                 avcodec_h_found="yes",
                 avcodec_h_found="no")

   AC_CHECK_FILE(${srcdir}/lib-src/ffmpeg/libavformat/avformat.h,
                 avformat_h_found="yes",
                 avformat_h_found="no")

   if test "$avcodec_h_found" = "yes" -a "$avformat_h_found" = "yes"; then
      FFMPEG_LOCAL_AVAILABLE="yes"
      AC_MSG_NOTICE([FFmpeg headers are available in the local tree])
   else
      FFMPEG_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([ffmpeg library is NOT available in the local tree])
   fi

   FFMPEG_MIMETYPES="audio/aac;audio/ac3;audio/mp4;audio/x-ms-wma;video/mpeg;"
])

AC_DEFUN([AUDACITY_CONFIG_FFMPEG], [
   if test "$FFMPEG_USE_LOCAL" = yes; then
      FFMPEG_CFLAGS='-I$(top_srcdir)/lib-src/ffmpeg'
      FFMPEG_LIBS=""
   fi
   if test "$FFMPEG_USE_SYSTEM" = yes; then
      FFMPEG_CFLAGS="$AVCODEC_CFLAGS $AVFORMAT_CFLAGS $AVUTIL_CFLAGS"
      if test "$dynamic_loading" = "no"; then
         FFMPEG_LIBS="$AVCODEC_LIBS $AVFORMAT_LIBS $AVUTIL_LIBS"
         AC_DEFINE(DISABLE_DYNAMIC_LOADING_FFMPEG, 1,
                   [Use system FFmpeg library and disable dynamic loading of it.])
      else
         FFMPEG_LIBS=""
      fi
   fi

   AC_SUBST([FFMPEG_CFLAGS])
   AC_SUBST([FFMPEG_LIBS])

   AM_CONDITIONAL([USE_FFMPEG], [test "$FFMPEG_USE_LOCAL" = yes -o "$FFMPEG_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_FFMPEG], [test "$FFMPEG_USE_LOCAL" = yes])

   if test "$FFMPEG_USE_LOCAL" = yes -o "$FFMPEG_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_FFMPEG, 1,
                [Define if ffmpeg (multi-format import and export) support should be enabled])
   fi
])
