dnl Add audacity / vorbis license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libvorbis.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBVORBIS], [

   if false ; then
      AC_DEFINE(USE_LIBVORBIS, 1,
                [Define if the ogg vorbis decoding library is present])
   fi

   AC_ARG_WITH(libvorbis,
               [AS_HELP_STRING([--with-libvorbis],
                               [use libvorbis for Ogg Vorbis support])],
               LIBVORBIS_ARGUMENT=$withval,
               LIBVORBIS_ARGUMENT="unspecified")

   dnl See if Vorbis is installed in the system

   AC_CHECK_LIB(vorbisfile,
                vorbis_bitrate_addblock,
                lib_found="yes",
                lib_found="no",
                -lvorbis -logg)

   AC_CHECK_HEADER(vorbis/vorbisfile.h,
                   header_found="yes",
                   header_found="no")

   if test "x$lib_found" = "xyes" && test "x$header_found" = "xyes" ; then
      LIBVORBIS_SYSTEM_AVAILABLE="yes"
      LIBVORBIS_SYSTEM_LIBS="-lvorbisenc -lvorbisfile -lvorbis -logg"
      LIBVORBIS_SYSTEM_CPPSYMBOLS="USE_LIBVORBIS"
      AC_MSG_NOTICE([Vorbis libraries are available as system libraries])
   else
      LIBVORBIS_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Vorbis libraries are NOT available as system libraries])
   fi

   dnl see if Vorbis is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/libvorbis/include/vorbis/vorbisenc.h,
                 vorbisenc_h_available="yes",
                 vorbisenc_h_available="no")

   AC_CHECK_FILE(${srcdir}/lib-src/libogg/include/ogg/ogg.h,
                 ogg_h_available="yes",
                 ogg_h_available="no")

   if test "x$vorbisenc_h_available" = "xyes" && test "x$ogg_h_available" = "xyes" ; then
      LIBVORBIS_LOCAL_AVAILABLE="yes"

      LIBVORBIS_LOCAL_LIBS="libvorbisenc.a libvorbisfile.a libvorbis.a libogg.a"

      LIBVORBIS_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libogg/include'
      LIBVORBIS_LOCAL_CXXFLAGS="$LIBVORBIS_LOCAL_CXXFLAGS -I\$(top_srcdir)/lib-src/libvorbis/include"

      LIBVORBIS_LOCAL_CPPSYMBOLS="USE_LIBVORBIS"

      libogg_dir="$(pwd)/lib-src/libogg"
      LIBVORBIS_LOCAL_CONFIGURE_ARGS="--disable-oggtest OGG_CFLAGS=-I${libogg_dir}/include OGG_LIBS=${libogg_dir}/src/.libs/libogg.a"

      AC_MSG_NOTICE([Vorbis libraries are available in this source tree])
   else
      AC_MSG_NOTICE([Vorbis libraries are NOT available in this source tree])
   fi
   LIBVORBIS_MIMETYPES="application/ogg;audio/x-vorbis+ogg;"
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBVORBIS], [
   if test "$LIBVORBIS_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/libogg lib-src/libvorbis])
   fi
])
