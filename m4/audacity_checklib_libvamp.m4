dnl Todo: add Vamp / Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libvamp.m4 serial 2

dnl Check for system copy of libvamp we can use for Vamp plug-in support
AC_DEFUN([AUDACITY_CHECKLIB_LIBVAMP], [
   AC_ARG_WITH(libvamp,
               [AS_HELP_STRING([--with-libvamp],
                               [use libvamp for Vamp plug-in support [default=yes]])],
               LIBVAMP_ARGUMENT=$withval,
               LIBVAMP_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_VAMP, 1,
                [Define if Vamp analysis plugin support should be enabled])
   fi

   dnl System may include Vamp headers and library, though we prefer local ones

   PKG_CHECK_MODULES(VAMP, vamp-hostsdk >= 2.0,
                     vamp_available_system="yes",
                     vamp_available_system="no")

   if test "x$vamp_available_system" = "xyes" ; then
      LIBVAMP_SYSTEM_AVAILABLE="yes"
      LIBVAMP_SYSTEM_LIBS=$VAMP_LIBS
      LIBVAMP_SYSTEM_CXXFLAGS=$VAMP_CFLAGS
	  dnl still need these local objects for the support in audacity
      LIBVAMP_SYSTEM_OPTOBJS="effects/vamp/VampEffect.o effects/vamp/LoadVamp.o"
      LIBVAMP_SYSTEM_CPPSYMBOLS="USE_VAMP"
      AC_MSG_NOTICE([Vamp libraries are available as system libraries])
   else
      LIBVAMP_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Vamp libraries are NOT available as system libraries])
   fi

   dnl see if Vamp is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libvamp/vamp-hostsdk/PluginLoader.h,
                 vamp_h_found="yes",
                 vamp_h_found="no")

   if test "x$vamp_h_found" = "xyes" ; then
      LIBVAMP_LOCAL_AVAILABLE="yes"
	  dnl Add vamp to the list of things to build in lib-src
      LIBVAMP_LOCAL_BUILD="vamp-sdk"
	  dnl compiler and linker flags
      LIBVAMP_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libvamp'
      LIBVAMP_LOCAL_LDFLAGS='-L$(top_builddir)/lib-src/libvamp/src -lvamp-hostsdk'
	  dnl add some extra object files we can build
      LIBVAMP_LOCAL_OPTOBJS="effects/vamp/VampEffect.o effects/vamp/LoadVamp.o"
	  dnl define a pre-processor symbol to tell other code that the vamp host
	  dnl SDK is available
      LIBVAMP_LOCAL_CPPSYMBOLS="USE_VAMP"
	  dnl schedule the directory to be configured
      LIBVAMP_LOCAL_CONFIG_SUBDIRS="lib-src/libvamp"
	  ac_configure_args="$ac_configure_args --disable-programs"
	  dnl do not build programs we don't need

      AC_MSG_NOTICE([Vamp libraries are available in the local tree])
   else
      LIBVAMP_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([Vamp libraries are NOT available in the local tree])
   fi
])

