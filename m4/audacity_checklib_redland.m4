dnl add Audacity / Redland license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_redland.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_REDLAND], [
   AC_ARG_WITH(redland,
               [AS_HELP_STRING([--with-redland],
                               [use Redland for reading RDF data ])],
               REDLAND_ARGUMENT=$withval,
               REDLAND_ARGUMENT="unspecified")

   dnl Check for a system copy of Redland to use.
   PKG_CHECK_MODULES(REDLAND, redland >= 1.0.7,
                     redland_available_system="yes",
                     redland_available_system="no")
   REDLAND_SYSTEM_AVAILABLE="no"
   if test "x$redland_available_system" = "xyes" ; then
      REDLAND_SYSTEM_AVAILABLE="yes"
      REDLAND_SYSTEM_LIBS="$REDLAND_LIBS"
      REDLAND_SYSTEM_CXXFLAGS="$REDLAND_CFLAGS"
      AC_MSG_NOTICE([Redland available as system library])
   fi
   if test "x$REDLAND_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([Redland NOT available as system library])
   fi

   dnl Check if Redland is available locally.
   AC_CHECK_FILE(${srcdir}/lib-src/redland/librdf/librdf.h,
                 librdf_h_found="yes",
                 librdf_h_found="no")

   if test "x$librdf_h_found" = "xyes" ; then
      REDLAND_LOCAL_AVAILABLE="yes"
      REDLAND_LOCAL_LIBS="librdf.a libraptor.a librasqal.a"
      REDLAND_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/redland/librdf -I$(top_srcdir)/lib-src/redland/raptor/src -I$(top_srcdir)/lib-src/redland/rasqal/src'
      if test "x$LIBEXPAT_SYSTEM_AVAILABLE" = "xno" ; then
         # This is a horrible hack to keep from having to modify the raptor/configure.ac.  It makes
         # the raptor configure think there's a full expat source tree.  But, all we have is expat.h
         # tucked away in audacity/src/include.  So, we trick it...
         REDLAND_LOCAL_CONFIGURE_ARGS="\"--with-expat-source=dummy_magic\" CPPFLAGS='$CPPFLAGS -I../../../src/include'"
      fi
      REDLAND_LOCAL_CONFIGURE_ARGS="$REDLAND_LOCAL_CONFIGURE_ARGS RAPTOR_CFLAGS='-I../../redland/raptor/src' RAPTOR_LIBS='-L.. -L../.. -lraptor' REDLAND_CFLAGS='-I../../redland/raptor/src -I../../redland/rasqal/src -I../../redland/librdf' REDLAND_LIBS='-L.. -L../.. -lrdf -lraptor -lrasqal'"
      AC_MSG_NOTICE([Redland is available in the local tree])
   else
      REDLAND_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([Redland is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_REDLAND], [
   if test "$REDLAND_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/redland])
   fi
])
