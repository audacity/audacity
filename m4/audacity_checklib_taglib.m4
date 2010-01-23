dnl add Audacity / TagLib license ?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_taglib.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_TAGLIB], [
   AC_ARG_WITH(taglib,
               [AS_HELP_STRING([--with-taglib],
                               [use TagLib for metadata support ])],
               TAGLIB_ARGUMENT=$withval,
               TAGLIB_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_TAGLIB, 1,
                [Define if TagLib (metadata export) support should be enabled])
   fi

   dnl Check for a system copy of TagLib to use

   PKG_CHECK_MODULES(TAGLIB, taglib >= 1.5,
                     taglib_available_system="yes",
                     taglib_available_system="no")

   if test "x$taglib_available_system" = "xyes" ; then
      TAGLIB_SYSTEM_AVAILABLE="yes"
      TAGLIB_SYSTEM_LIBS=$TAGLIB_LIBS
      TAGLIB_SYSTEM_CXXFLAGS=$TAGLIB_CFLAGS
      TAGLIB_SYSTEM_CPPSYMBOLS="USE_TAGLIB"
      AC_MSG_NOTICE([TagLib library available as system library])
   else
      TAGLIB_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([TagLib library NOT available as system library])
   fi

   dnl see if TagLib is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/taglib/taglib/tag.h,
                 tag_h_found="yes",
                 tag_h_found="no")

   if test "x$tag_h_found" = "xyes" ; then
      TAGLIB_LOCAL_AVAILABLE="yes"
      TAGLIB_LOCAL_LIBS="taglib.a"
      TAGLIB_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/taglib/taglib'
      TAGLIB_LOCAL_CPPSYMBOLS="USE_TAGLIB"
	
      dnl request library is configured
      TAGLIB_LOCAL_CONFIG_SUBDIRS="lib-src/taglib"

      AC_MSG_NOTICE([TagLib library is available in the local tree])
   else
      TAGLIB_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([TagLib library is NOT available in the local tree])
   fi
])

