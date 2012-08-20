dnl add audacity / flac license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libflac.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_LIBFLAC], [

   if false ; then
      AC_DEFINE(USE_LIBFLAC, 1,
                [Define if the FLAC library is present])
   fi

   AC_ARG_WITH(libflac,
               [AS_HELP_STRING([--with-libflac],
                               [use libFLAC for FLAC support])],
               LIBFLAC_ARGUMENT=$withval,
               LIBFLAC_ARGUMENT="unspecified")

   dnl See if FLAC is installed in the system

   AC_CHECK_LIB(FLAC,
                FLAC__stream_decoder_new,
                lib_found="yes",
                lib_found="no",
                -lFLAC++ -lFLAC)

   AC_CHECK_HEADER(FLAC/format.h,
                   header_found="yes",
                   header_found="no")

   if test "x$lib_found" = "xyes" && test "x$header_found" = "xyes" ; then
      LIBFLAC_SYSTEM_AVAILABLE="yes"
      LIBFLAC_SYSTEM_LIBS="-lFLAC++ -lFLAC"
      LIBFLAC_SYSTEM_CPPSYMBOLS="USE_LIBFLAC"
      # this file shouldn't be built at if no libflac is available
      LIBFLAC_SYSTEM_OPTOBJS="ondemand/ODDecodeFlacTask.o"
      AC_MSG_NOTICE([FLAC libraries are available as system libraries])
   else
      LIBFLAC_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([FLAC/FLAC++ libraries are NOT available as system libraries])
   fi

   dnl see if FLAC is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/libflac/include/FLAC/format.h,
                 flac_h_available="yes",
                 flac_h_available="no")

   AC_CHECK_FILE(${srcdir}/lib-src/libflac/include/FLAC++/decoder.h,
                 flacpp_h_available="yes",
                 flacpp_h_available="no")

   if test "x$flac_h_available" = "xyes" && test "x$flacpp_h_available" = "xyes" ; then
      LIBFLAC_LOCAL_AVAILABLE="yes"

      LIBFLAC_LOCAL_LIBS="libFLAC++.a libFLAC.a"

      LIBFLAC_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libflac/include'
      LIBFLAC_LOCAL_CXXFLAGS="$LIBFLAC_LOCAL_CXXFLAGS -I\$(top_srcdir)/lib-src/libflac/include"

      LIBFLAC_LOCAL_CPPSYMBOLS="USE_LIBFLAC"
      # this file shouldn't be built at if no libflac is available
      LIBFLAC_LOCAL_OPTOBJS="ondemand/ODDecodeFlacTask.o"
      LIBFLAC_LOCAL_CONFIG_SUBDIRS="lib-src/libflac"
	
      AC_MSG_NOTICE([FLAC libraries are available in this source tree])
   else
      AC_MSG_NOTICE([FLAC libraries are NOT available in this source tree])
   fi

   LIBFLAC_MIMETYPES="audio/flac;audio/x-flac;"
])

