dnl add license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libexpat.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBEXPAT], [

   AC_ARG_WITH(expat,
               [AS_HELP_STRING([--with-expat],
                               [which expat to use for XML file support: [system,local]])],
               LIBEXPAT_ARGUMENT=$withval,
               LIBEXPAT_ARGUMENT="unspecified")

   dnl see if libexpat is installed on the system

   AC_CHECK_LIB(expat, XML_ParserCreate,
                libexpat_found="yes",
                libexpat_found="no")

   expat_h_found="no"

   AC_CHECK_HEADER(expat.h,
                   expat_h_found="yes",
                   expat_h_found="no")

   if test "x$libexpat_found" = "xyes" && test "x$expat_h_found" = "xyes" ; then
      LIBEXPAT_SYSTEM_AVAILABLE="yes"
      LIBEXPAT_SYSTEM_LIBS="-lexpat"
      LIBEXPAT_SYSTEM_CPPSYMBOLS="USE_SYSTEM_EXPAT"
      AC_MSG_NOTICE([Expat libraries are available as system libraries])
   else
      LIBEXPAT_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Expat libraries are NOT available as system libraries])
   fi

   dnl see if expat is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/expat/xmlparse/xmlparse.h,
                 xmlparse_h_found="yes",
                 xmlparse_h_found="no")

   if test "x$xmlparse_h_found" = "xyes" ; then
      LIBEXPAT_LOCAL_AVAILABLE="yes"
      LIBEXPAT_LOCAL_LIBS="expat.a"
      LIBEXPAT_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/expat'
      LIBEXPAT_LOCAL_CPPSYMBOLS="USE_LOCAL_EXPAT"

      AC_MSG_NOTICE([Expat libraries are available in the local tree])
   else
      LIBEXPAT_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([Expat libraries are NOT available in the local tree])
   fi
])
