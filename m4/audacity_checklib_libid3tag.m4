dnl Add audacity / libmad licence?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libid3tag.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBID3TAG], [

   AC_ARG_WITH(libid3tag,
               [AS_HELP_STRING([--with-libid3tag],
                               [use libid3tag for mp3 id3 tag support])],
               LIBID3TAG_ARGUMENT=$withval,
               LIBID3TAG_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBID3TAG, 1,
                [Define if libid3tag is present])
   fi

   dnl see if libid3tag is installed on the system

   AC_CHECK_LIB(id3tag, id3_file_open,
                libid3tag_found="yes",
                libid3tag_found="no",
               -lz)

   AC_CHECK_HEADER(id3tag.h,
                   id3tag_h_found="yes",
                   id3tag_h_found="no")

   if test "x$libid3tag_found" = "xyes" && test "x$id3tag_h_found" = "xyes" ; then
      LIBID3TAG_SYSTEM_AVAILABLE="yes"
      LIBID3TAG_SYSTEM_LIBS=-lid3tag
      LIBID3TAG_SYSTEM_CPPSYMBOLS="USE_LIBID3TAG"
      AC_MSG_NOTICE([Libid3tag libraries are available as system libraries])
   else
      LIBID3TAG_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libid3tag libraries are NOT available as system libraries])
   fi

   dnl see if libid3tag is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libid3tag/frame.h,
                 frame_h_found="yes",
                 frame_h_found="no")


   if test "x$frame_h_found" = "xyes" ; then
      LIBID3TAG_LOCAL_AVAILABLE="yes"
      LIBID3TAG_LOCAL_LIBS="libid3tag.a"
      LIBID3TAG_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libid3tag'
      LIBID3TAG_LOCAL_CPPSYMBOLS="USE_LIBID3TAG"
      LIBS="${LIBS} -lz"
      AC_MSG_NOTICE([libid3tag libraries are available in the local tree])
   else
      LIBID3TAG_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libid3tag libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBID3TAG], [
   if test "$LIBID3TAG_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/libid3tag])
   fi
])
