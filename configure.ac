dnl
dnl audacity configure.ac script
dnl
dnl Joshua Haberman
dnl Dominic Mazzoni
dnl

dnl
dnl Instructions: to create "configure" from "configure.ac", run:
dnl
dnl aclocal
dnl autoconf
dnl
dnl Note: you need pkgconfig installed for this to work.  If pkg.m4 is
dnl not in the standard place, like /usr/share/aclocal, use the -I option
dnl to aclocal, for example on Mac OS X when pkgconfig was installed by
dnl fink:
dnl  aclocal -I /sw/share/aclocal
dnl

dnl TODO:
dnl
dnl automatically choose whether or not to make some libs
dnl based on:
dnl
dnl AC_CHECK_LIB(z, compress2, build_libz=false, build_libz=true)
dnl AM_CONDITIONAL(BUILD_LIBZ, test "$build_libz" = true)
dnl
dnl

dnl Process this file with autoconf to produce a configure script.

dnl Require autoconf >= 2.59
AC_PREREQ([2.59])

dnl Init autoconf
AC_INIT([audacity], [2.1.2])
dnl Check for existence of Audacity.h
AC_CONFIG_SRCDIR([src/Audacity.h])
AC_CONFIG_AUX_DIR([autotools])
dnl we have some extra macros in m4/
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.9 dist-xz foreign no-dist-gzip subdir-objects -Wall -Wno-override tar-ustar])
AM_MAINTAINER_MODE([disable])

AM_GNU_GETTEXT_VERSION([0.18])
AM_GNU_GETTEXT([external])

dnl -------------------------------------------------------
dnl Checks for programs.
dnl -------------------------------------------------------
dnl save $CFLAGS etc. since AC_PROG_CC likes to insert "-g -O2"
dnl if $CFLAGS is blank and it finds GCC
cflags_save="$CFLAGS"
cppflags_save="$CPPFLAGS"
cxxflags_save="$CXXFLAGS"
AX_COMPILER_VENDOR
AC_PROG_CC
AC_LANG([C++])
AC_PROG_CXX
AC_PROG_CXXCPP
CFLAGS="$cflags_save"
CPPFLAGS="$cppflags_save"
CXXFLAGS="$cxxflags_save"

AM_PROG_AR

LT_INIT([disable-shared])

AC_PROG_INSTALL
dnl pkg-config is required for lots of things
AC_CHECK_PROG(HAVE_PKG_CONFIG, pkg-config, yes, no)
if test x$HAVE_PKG_CONFIG = xno ; then
   AC_MSG_ERROR([pkg-config is required to compile audacity!])
fi

dnl extra variables
dnl Extra things that need to be built during make (makefile targets)
AC_SUBST(EXTRATARGETS)
AC_SUBST(CDEPEND)
AC_SUBST(PRECOMP_CFLAGS)
AC_SUBST(INSTALL_PREFIX)
dnl the header file with #defines from configure in it
AC_SUBST(CONFIGHEADER)
dnl LDFLAGS that will be valid for the build, but aren't yet valid so shouldn't
dnl be used for configure script tests (they are needed for main build).
AC_SUBST(BUILD_LDFLAGS)

AC_SUBST(MIMETYPES)

dnl allow the user to specify options to configure that change the
dnl name "audacity" anywhere it appears in a pathname.   This allows
dnl multiple versions of Audacity to be installed concurrently
dnl without collision

AC_ARG_PROGRAM
AC_SUBST(AUDACITY_NAME)

dnl autoconf's program_transform_name is set to spit out a sed expression --
dnl however it's "helpfully" already escaped for make.   Since we want to use
dnl it in shell and not make, we have to unescape this: this translates into
dnl turning two dollar signs into one.
dnl
dnl I feign at this monstrosity, but no one depends on this particular
dnl feature but me, as Debian package maintainer, so no one else should
dnl need to worry about understanding it...
program_transform_name=`echo $program_transform_name | sed 's/\\$\\$/$/'`
AUDACITY_NAME=`echo audacity | sed $program_transform_name`

if [[ "$AUDACITY_NAME" = "audacity" ]] ; then
    AC_DEFINE(AUDACITY_NAME, "audacity",
        [define if Audacity is being installed under a name other than "audacity",
         so it can find the files it needs at runtime])
else
    AC_DEFINE_UNQUOTED(AUDACITY_NAME, "$AUDACITY_NAME")
fi

dnl
dnl Make the install prefix available to the program so that it
dnl knows where to look for help files, plug-ins, etc.
dnl

AC_PREFIX_DEFAULT(/usr/local)
if test x$prefix = "xNONE" ; then
   prefix="/usr/local/"
fi
AC_DEFINE_UNQUOTED(INSTALL_PREFIX, "$prefix", [define as prefix where Audacity is installed])

### Configuration of Audacity module support ###
################################################
dnl Some code (headers) can be built either as part of audacity, or as part of
dnl audacity plug-ins. In order to tell this code what is going on, we need to
dnl define BUILDING_AUDACITY during the audacity build, and not during the
dnl plug-in build. This code puts the relevant content into configunix.h
AC_DEFINE(BUILDING_AUDACITY, 1,
   [Define we are compiling Audacity itself, not an Audacity plug-in])
# Add -rdynamic to linker flags so the right symbols are exported for
# plug-ins. This might not work on all architectures / compilers, so we need
# to check if it does or not.
AX_LD_CHECK_FLAG([-rdynamic],[],[],
               have_dynamic_link=yes,have_dynamic_link=no)
if test "x$have_dynamic_link" = "xyes" ; then
   BUILD_LDFLAGS="${BUILD_LDFLAGS} -rdynamic "
else
   AC_MSG_WARN(["Linker does not support -rdynamic. Could not enable exporting all symbols"])
   AC_MSG_WARN(["Audacity module support will probably not work"])
fi

### Build Options ###
#####################

AC_ARG_ENABLE(static-wx,
            [AS_HELP_STRING([--enable-static-wx],
                            [link wx statically [default=no]])],
            static_wx_preference="--static=$enableval",
            static_wx_preference="")

AC_ARG_ENABLE(unicode,
            [AS_HELP_STRING([--enable-unicode],
                            [enable unicode support [default=yes]])],
            unicode_preference="--unicode=$enableval",
            unicode_preference="--unicode=yes")

AC_ARG_ENABLE(debug,
            [AS_HELP_STRING([--enable-debug],
                            [enable debug support [default=no]])],
            debug_preference="$enableval",
            debug_preference="no")

AC_ARG_WITH(lib-preference,
            [AS_HELP_STRING([--with-lib-preference],
                            [whether to use local and/or system libraries, in order of preference (default="system local")])],
            lib_preference=$withval,
            lib_preference="system local")

AC_ARG_ENABLE(sse, [AS_HELP_STRING([--enable-sse],[enable SSE optimizations])], enable_sse=$enableval, enable_sse=yes)

AC_ARG_ENABLE(universal_binary,[  --enable-universal_binary enable universal binary build: (default: disable)],[enable_universal_binary=$enableval],[enable_universal_binary=no])

AC_ARG_ENABLE(dynamic-loading,
            [AS_HELP_STRING([--enable-dynamic-loading],
                            [enable dynamic loading of lame and FFmpeg [default=yes]])],
            [dynamic_loading="$enableval"],
            [dynamic_loading="yes"])

AC_ARG_WITH(wx-version,
           [AS_HELP_STRING([--with-wx-version],
                           [select wxWidgets version (if both installed) [3.0,]])],
           wx_preference="--version=$withval",
           wx_preference="")

dnl ----------------------------------------------------
dnl If user asked for debug, put debug in compiler flags
dnl ----------------------------------------------------

if test x$enable_universal_binary = xyes; then
   case "$target_os" in
     darwin*)
        CPPFLAGS="${CPPFLAGS} -mmacosx-version-min=10.4 -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.4u.sdk -Xarch_i386 -DAPPLE_I386 -Xarch_ppc -DAPPLE_PPC"
	;;
     *)
	;;
   esac
fi

if test x$enable_sse = xyes; then

   if test "${ax_cv_cxx_compiler_vendor}" = "gnu"; then
      AX_CHECK_COMPILER_FLAGS(-msse, [SBSMS_CFLAGS="$SBSMS_CFLAGS -msse"],[AC_MSG_ERROR([Need a version of gcc with -msse])])
   fi

   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <xmmintrin.h>]], [[__m128 v,w; float p[8]; v = _mm_loadu_ps(p); _mm_storeu_ps(p,v); w = _mm_loadl_pi(w,(const __m64*)p); w = _mm_loadh_pi(w,(const __m64*)p); _mm_storel_pi((__m64 *)p, w); _mm_storeh_pi((__m64 *)p, w); v = _mm_add_ps(v,w); v = _mm_sub_ps(v,w); v = _mm_mul_ps(v,w); v = _mm_shuffle_ps(v,w,_MM_SHUFFLE(0,1,2,3)); w = _mm_set1_ps(0.0f);]])], [sse_ok=yes], [sse_ok=no])

   if test x$sse_ok = xyes; then
      AC_DEFINE(ENABLE_SSE,1,[Define to enable sse])
   fi
fi

if test x"$debug_preference" = "xyes" ; then
   dnl we want debuging on
   AC_MSG_NOTICE([Adding -g for debugging to CFLAGS and CXXFLAGS ...])
   CFLAGS="${CFLAGS} -g -DwxDEBUG_LEVEL=1"
   CXXFLAGS="${CXXFLAGS} -g -DwxDEBUG_LEVEL=1"
else
   CFLAGS="${CFLAGS} -DwxDEBUG_LEVEL=0"
   CXXFLAGS="${CXXFLAGS} -DwxDEBUG_LEVEL=0"
fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

dnl check if alloca.h exists. If it does, we need to include it for
dnl Solaris builds, by defining HAVE_ALLOCA_H
AC_CHECK_HEADERS_ONCE(alloca.h)


dnl --------------------------------------------------------------------------
dnl We would like warnings enabled on the builds, but different compilers need
dnl different options for these. This bit tries to work out what flags we
dnl should add to the compiler we are using.
dnl --------------------------------------------------------------------------

dnl Strict prototypes flags for C (only C because doesn't work for C++)
AX_CFLAGS_STRICT_PROTOTYPES(CFLAGS)

dnl Sensible warnings for C
AX_CFLAGS_WARN_ALL(wall_flags)
CFLAGS="${CFLAGS} $wall_flags"
dnl try and use it on C++ as well
AX_CXX_CHECK_FLAG([$wall_flags], [[int foo;]], [[foo = 1;]], cxx_does_wall="yes", cxx_does_wall="no")
if test "x$cxx_does_wall" = "xyes" ; then
   dnl can use all warnings flag on the C++ compiler
   CXXFLAGS="${CXXFLAGS} $wall_flags"
fi
AX_CXXCPP_CHECK_FLAG([$wall_flags], [[int foo;]], [[foo = 1;]], cpp_does_wall="yes", cpp_does_wall="no")
if test "x$cpp_does_wall" = "xyes" ; then
   dnl can use all warnings flag on the C++ pre-processor
   CPPFLAGS="${CPPFLAGS} $wall_flags"
fi

dnl-------------------------------------------------------------------------
dnl If the C++ compiler supports making symbols within audacity hidden then
dnl we would like to do so. This means that only the required symbols for
dnl plug-in functionality are exposed, rather than everything in the program.
dnl-------------------------------------------------------------------------
gl_VISIBILITY

dnl-------------------------------------------------------------
dnl Configure the libraries which are essential to audacity.
dnl These are mostly only libraries for licensing flexibility, so
dnl there isn't a way to turn them off.
dnl-------------------------------------------------------------

dnl wxWidgets -- we assume that if wx-config is found, wxWidgets is successfully installed.
AC_PATH_PROGS(WX_CONFIG, $WX_CONFIG wx-config wx-config-3.0, no, $PATH:/usr/local/bin )
if [[ "$WX_CONFIG" = "no" ]] ; then
   AC_MSG_ERROR([Could not find wx-config: is wxWidgets installed? is wx-config in your path?])
fi

if test "x$debug_preference" = "xyes"; then
   dnl want debug wx as well
   wxconfigargs="--debug=yes"
else
   dnl normal wx
   wxconfigargs=""
fi

dnl more things we always pass to wx-config
wxconfigargs="$static_wx_preference $unicode_preference $wxconfigargs $wx_preference"
wx_version=`${WX_CONFIG} $wxconfigargs --version`

AC_MSG_NOTICE([Checking that the chosen version of wxWidgets is 3.0.x])

case "${wx_version}" in
  3.0.*)
    echo "Great, you're using wxWidgets ${wx_version}!"
    ;;
  *)
  wx_list=`${WX_CONFIG} --list`
  AC_MSG_ERROR([Unable to locate a suitable configuration of wxWidgets v3.0.x or higher.
The currently available configurations are listed below.  If necessary, either
install the package for your distribution or download the latest version of
wxWidgets
from http://wxwidgets.org.
${wx_list}])
esac

dnl Pass wx config path to lower levels
ac_configure_args="$ac_configure_args --with-wx-config=\"$WX_CONFIG\""

dnl Gather wx arguments
WX_CXXFLAGS=$($WX_CONFIG $wxconfigargs --cxxflags)
WX_LIBS=$($WX_CONFIG $wxconfigargs --libs)
AC_SUBST([WX_CXXFLAGS])
AC_SUBST([WX_LIBS])

dnl-----------------------------------------------------------------
dnl Pull in library Cflags and so on for the non-optional libraries

dnl All libraries in lib-src should build static libraries, but not shared ones.
ac_configure_args="$ac_configure_args --enable-static=yes --enable-shared=no"

dnl Include "external" headers
CXXFLAGS="$CXXFLAGS -I\$(top_srcdir)/include"

dnl Include FileDialog
AC_CONFIG_SUBDIRS([lib-src/FileDialog lib-src/mod-nyq-bench])
CXXFLAGS="$CXXFLAGS -I\$(top_srcdir)/lib-src/FileDialog"
FILEDIALOG_LIBS='$(top_builddir)/lib-src/FileDialog/libFileDialog.la'
AC_SUBST([FILEDIALOG_LIBS])

dnl-------------------------------------------------------------
dnl Optional library support. Lots of things we could use, but
dnl can do without if they aren't available.
dnl-------------------------------------------------------------
dnl GSTREAMER removed for 2.0.6 release
LIBRARIES="EXPAT FFMPEG LAME LIBFLAC LIBID3TAG LIBMAD LIBNYQUIST LIBSBSMS LIBSNDFILE LIBSOUNDTOUCH LIBSOXR LIBTWOLAME LIBVAMP LIBVORBIS LV2 PORTAUDIO PORTSMF WIDGETEXTRA"

AC_MSG_NOTICE([Determining what libraries are available in this tree and on the system])

AUDACITY_CHECKLIB_EXPAT
AUDACITY_CHECKLIB_FFMPEG
dnl AUDACITY_CHECKLIB_GSTREAMER  removed for 2.0.6 release
AUDACITY_CHECKLIB_LAME
AUDACITY_CHECKLIB_LIBFLAC
AUDACITY_CHECKLIB_LIBID3TAG
AUDACITY_CHECKLIB_LIBMAD
AUDACITY_CHECKLIB_LIBNYQUIST
AUDACITY_CHECKLIB_LIBSBSMS
AUDACITY_CHECKLIB_LIBSNDFILE
AUDACITY_CHECKLIB_LIBSOUNDTOUCH
AUDACITY_CHECKLIB_LIBSOXR
AUDACITY_CHECKLIB_LIBTWOLAME
AUDACITY_CHECKLIB_LIBVAMP
AUDACITY_CHECKLIB_LIBVORBIS
AUDACITY_CHECKLIB_LV2
AUDACITY_CHECKLIB_PORTAUDIO
AUDACITY_CHECKLIB_PORTSMF
AUDACITY_CHECKLIB_WIDGETEXTRA

dnl Decide what libraries to build with, and whether to use system or local libraries
dnl Set variables based on choices.

AC_MSG_NOTICE([Figuring out what libraries to enable])

lib_preference=`echo $lib_preference | tr [[:lower:]], "[[:upper:]] "`

for lib in $LIBRARIES ; do
   eval LIB_ARGUMENT="\$${lib}_ARGUMENT"
   eval LIB_SYSTEM_AVAILABLE="\$${lib}_SYSTEM_AVAILABLE"
   eval LIB_LOCAL_AVAILABLE="\$${lib}_LOCAL_AVAILABLE"
   if test x"$LIB_ARGUMENT" = x"unspecified" -o x"$LIB_ARGUMENT" = x"yes" ; then
      lib_activated="no"
      for sys_or_local in $lib_preference ; do
         # example: LIBMAD_SYSTEM_AVAILABLE
         eval AVAILABLE="\$${lib}_${sys_or_local}_AVAILABLE"
         if test "$AVAILABLE" = "yes" ; then
            eval ${lib}_USE_${sys_or_local}="yes"
            lib_activated="yes"
            AC_MSG_NOTICE([Using $sys_or_local libraries for $lib])
            break
         fi
      done
      if test "$lib_activated" = no ; then
         AC_MSG_NOTICE([disabling $lib])
      fi
   else
      dnl lib_ARGUMENT is something other than "unspecified"
      if test "$LIB_ARGUMENT" = local ; then
         if test "$LIB_LOCAL_AVAILABLE" = yes ; then
            eval ${lib}_USE_LOCAL="yes"
            AC_MSG_NOTICE([Using the LOCAL libraries for $lib because you specifically requested this])
         else
            AC_MSG_ERROR([You requested using the local libraries for $lib but they are not available])
         fi
      elif test "$LIB_ARGUMENT" = system ; then
         if test "$LIB_SYSTEM_AVAILABLE" = yes ; then
            eval ${lib}_USE_SYSTEM="yes"
            AC_MSG_NOTICE([Using the SYSTEM libraries for $lib because you specifically requested this])
         else
            AC_MSG_ERROR([You requested using the system libraries for $lib but they are not available])
         fi
      elif test "$LIB_ARGUMENT" = no ; then
         AC_MSG_NOTICE([disabling $lib at your request])
      else
         AC_MSG_ERROR([I did not understand the argument $LIB_ARGUMENT for $lib])
      fi
   fi
done

# In some cases the choices made above might be inappropriate.
#   * we need to have expat one way or another
#   * we need to have libwidgetextra one way or another
#   * we need to have libsndfile one way or another
#   * we need to have portaudio way or another
#   * we need to have libsoxr

if test "$EXPAT_USE_LOCAL" != "yes" -a "$EXPAT_USE_SYSTEM" != "yes"; then
   AC_MSG_ERROR([Audacity requires expat to be enabled.])
fi

if test "$WIDGETEXTRA_USE_LOCAL" != "yes" -a "$WIDGETEXTRA_USE_SYSTEM" != "yes"; then
   AC_MSG_ERROR([Audacity requires libwidgetextra to be enabled.])
fi

if test "$LIBSNDFILE_USE_LOCAL" != "yes" && test "$LIBSNDFILE_USE_SYSTEM" != "yes" ; then
   AC_MSG_ERROR([Audacity requires libsndfile to be enabled])
fi

if test "$PORTAUDIO_USE_LOCAL" != "yes" && test "$PORTAUDIO_USE_SYSTEM" != "yes" ; then
   AC_MSG_ERROR([Audacity requires portaudio to be enabled])
fi

if test "$LIBSOXR_USE_LOCAL" != "yes" && test "$LIBSOXR_USE_SYSTEM" != "yes" ; then
   AC_MSG_ERROR([Audacity requires libsoxr to be enabled])
fi

dnl-----------------------------------------------------------------
dnl Based on the optional lib selections, modify CXXFLAGS, etc
for lib in $LIBRARIES ; do
   eval LIB_USE_LOCAL=\$${lib}_USE_LOCAL
   eval LIB_USE_SYSTEM=\$${lib}_USE_SYSTEM
   if test "$LIB_USE_LOCAL" = yes -o "$LIB_USE_SYSTEM" = yes; then
      eval MIMETYPES=\"${MIMETYPES}\$${lib}_MIMETYPES\"
   fi
   if test "x$LIB_USE_LOCAL" = "xyes" ; then
      eval CXXFLAGS=\"\$CXXFLAGS \$${lib}_LOCAL_CXXFLAGS\"
      eval ac_configure_args=\"\$ac_configure_args \$${lib}_LOCAL_CONFIGURE_ARGS\"

      eval CPPSYMBOLS=\"\$${lib}_LOCAL_CPPSYMBOLS\"
      for symbol in $CPPSYMBOLS ; do
         AC_DEFINE_UNQUOTED($symbol, 1)
      done
   elif test "$LIB_USE_SYSTEM" = "yes" ; then
      eval LIB_LIBS=\"\$${lib}_SYSTEM_LIBS\"
      AC_MSG_NOTICE([${lib}: adding ${LIB_LIBS} to libraries])
      eval LIBS=\"$LIBS \$${lib}_SYSTEM_LIBS\"
      eval CXXFLAGS=\"\$CXXFLAGS \$${lib}_SYSTEM_CXXFLAGS\"

      eval CPPSYMBOLS=\"\$${lib}_SYSTEM_CPPSYMBOLS\"
      for symbol in $CPPSYMBOLS ; do
         AC_DEFINE_UNQUOTED($symbol, 1)
      done
   fi
done

dnl " This is included purely to close an otherwise endless string in vim

AUDACITY_CONFIG_EXPAT
AUDACITY_CONFIG_FFMPEG
AUDACITY_CONFIG_GSTREAMER
AUDACITY_CONFIG_LAME
AUDACITY_CONFIG_LIBFLAC
AUDACITY_CONFIG_LIBID3TAG
AUDACITY_CONFIG_LIBMAD
AUDACITY_CONFIG_LIBNYQUIST
AUDACITY_CONFIG_LIBSBSMS
AUDACITY_CONFIG_LIBSNDFILE
AUDACITY_CONFIG_LIBSOUNDTOUCH
AUDACITY_CONFIG_LIBSOXR
AUDACITY_CONFIG_LIBTWOLAME
AUDACITY_CONFIG_LIBVAMP
AUDACITY_CONFIG_LIBVORBIS
AUDACITY_CONFIG_LV2
AUDACITY_CONFIG_PORTAUDIO
AUDACITY_CONFIG_PORTSMF
AUDACITY_CONFIG_WIDGETEXTRA

dnl--------------------------------------------------------------------------
dnl Optional features (which don't use libraries - just compile-time on/off)
dnl--------------------------------------------------------------------------

AC_ARG_ENABLE(audiounits,
   [AS_HELP_STRING([--enable-audiounits],
      [enable audio unit plug-in support (Mac OS X only)
       [default=auto]])], use_audiounits=$enableval,
   use_audiounits="auto")

AC_ARG_ENABLE(ladspa,
   [AS_HELP_STRING([--enable-ladspa],
      [enable LADSPA plug-in support [default=yes]])],
   use_ladspa=$enableval,
   use_ladspa="yes")
AM_CONDITIONAL([USE_LADSPA], [test "$use_ladspa" = yes])

AC_ARG_ENABLE(quicktime,
   [AS_HELP_STRING([--enable-quicktime],
      [enable QuickTime import support (Mac OS X only) [default=auto]])],
   use_quicktime=$enableval,
   use_quicktime="auto")

AC_ARG_ENABLE(vst,
   [AS_HELP_STRING([--enable-vst],
      [enable VST plug-in support [default=yes]])],
   use_vst=$enableval,
   use_vst="yes")

dnl In-tree libraries (ones we write, so only options are off and local)

AC_ARG_WITH(portmixer,
  [AS_HELP_STRING([--with-portmixer],
     [compile with PortMixer [default=yes]])],
  use_portmixer=$withval,
  use_portmixer="yes")


AC_CANONICAL_HOST

dnl OS-specific configuration

case "${host_os}" in
   darwin* | rhapsody*)
      dnl Mac OS X configuration
      CDEPEND="AudacityHeaders.h.gch"
      PRECOMP_CFLAGS="-include AudacityHeaders.h"
      CONFIGHEADER="configunix.h"
      CXXFLAGS="-I\$(top_srcdir)/mac $CXXFLAGS"
      EXTRATARGETS="../Audacity.app"
      if [[ "$use_audiounits" = "auto" ]] ; then
         use_audiounits="yes"
      fi
      if [[ "$use_quicktime" = "auto" ]] ; then
         use_quicktime="yes"
      fi
      ;;

   cygwin*)
      dnl Windows/CygWin configuration
      LIBS="$LIBS -lkernel32 -luser32 -lgdi32 -lwinspool -lcomdlg32 -ladvapi32 -lshell32 -lole32 -loleaut32 -luuid -lodbc32 -lodbccp32 -lwsock32 -lwinmm"
      CONFIGHEADER="configwin.h"

      AC_DEFINE(__CYGWIN__,1,[We're using cygwin])
      dnl ' (end endless string in vim)
      AC_DEFINE(_FILE_OFFSET_BITS,32,[Placeholder for large file support])
      ;;

   *)
      dnl Unix configuration
      CONFIGHEADER="configunix.h"

      AC_CHECK_HEADERS_ONCE(libudev.h)

      dnl On Unix we always use dlopen
      AC_SEARCH_LIBS([dlopen], [dl])
      if [[ "$ac_cv_search_dlopen" = no ]]; then
         AC_MSG_ERROR([dlopen not found, required by Audacity])
      fi

      AC_MSG_CHECKING([for gtk3 use in wxWidgets])
      CPPFLAGS="${WX_CXXFLAGS}"
      AC_EGREP_CPP(wxWidgets built with GTK3,
      [#include <wx/wx.h>
      #if defined(__WXGTK3__)
         wxWidgets built with GTK3
      #endif
      ], enable_gtk3=yes, enable_gtk3=no)
      
      if [[ "$enable_gtk3" = yes ]]; then
         gtk_version=gtk+-3.0
         AC_MSG_RESULT([yes])
      else
         gtk_version=gtk+-2.0
         AC_MSG_RESULT([no])
      fi
      
      AC_SUBST(HAVE_GTK)
      PKG_CHECK_MODULES(GTK, $gtk_version, have_gtk=yes, have_gtk=no)
      if [[ "$have_gtk" = "yes" ]]
      then
         AC_DEFINE(HAVE_GTK, 1, [Define if GTK is available])
         CPPFLAGS="$CPPFLAGS $GTK_CFLAGS"
         LIBS="$LIBS $GTK_LIBS"
      fi
   ;;
esac


case "${host_os}" in
   darwin* | rhapsody*)
      LIBS="-framework AudioUnit -framework AudioToolbox $LIBS"
      LIBS="-framework CoreAudio $LIBS -lz"
      ;;
   cygwin*)
      ;;
   *)
      dnl Unix
      AC_CHECK_LIB(asound, snd_pcm_open, have_alsa=yes, have_alsa=no)
      if [[ $have_alsa = "yes" ]] ; then
         LIBS="$LIBS -lasound"
      fi
      PKG_CHECK_MODULES(JACK, jack, have_jack=yes, have_jack=no)
      if [[ $have_jack = "yes" ]] ; then
         LIBS="$LIBS $JACK_LIBS"
      fi
      AC_CHECK_LIB(hpi, HPI_SubSysCreate, have_asihpi=yes, have_asihpi=no, -lm)
      if [[ $have_asihpi = "yes" ]] ; then
         LIBS="$LIBS -lhpi"
      fi
      AC_CHECK_LIB(rt, clock_gettime, [rt_libs=" -lrt"])
      LIBS="${LIBS}${rt_libs}"
      AC_CHECK_FUNCS([clock_gettime nanosleep])

      AC_CHECK_LIB(udev, udev_new, [udev_libs=" -ludev"])
      LIBS="${LIBS}${udev_libs}"
      ;;
esac


if [[ "$use_portmixer" = "yes" ]] ; then
   AC_DEFINE(USE_PORTMIXER, 1,
   [Define if PortMixer support should be enabled])

   CXXFLAGS="-I\$(top_srcdir)/lib-src/portmixer/include $CXXFLAGS"
   PORTMIXER_LIBS='$(top_builddir)/lib-src/portmixer/src/libportmixer.la'

   AC_CONFIG_SUBDIRS([lib-src/portmixer])
fi
AC_SUBST([PORTMIXER_LIBS])

dnl Check for lrint/lrintf
AC_C99_FUNC_LRINT
AC_C99_FUNC_LRINTF


if [[ "$use_ladspa" = "yes" ]] ; then
   AC_DEFINE(USE_LADSPA, 1,
             [Define if LADSPA plug-ins are enabled])

   dnl Special configuration for LADSPA on Mac OS X
   case "$host_os" in
      darwin7*)
         LIBS="$LIBS -ldl"
         ;;
      *)
         ;;
   esac
fi

AM_CONDITIONAL([USE_AUDIO_UNITS], [test "$use_audiounits" = yes])
if [[ "$use_audiounits" = "yes" ]] ; then
   AC_DEFINE(USE_AUDIO_UNITS, 1,
             [Define if Audio Unit plug-ins are enabled (Mac OS X only)])
fi

AM_CONDITIONAL([USE_QUICKTIME], [test "$use_quicktime" = yes])
if [[ "$use_quicktime" = "yes" ]] ; then
   AC_DEFINE(USE_QUICKTIME, 1,
             [Define if QuickTime importing is enabled (Mac OS X only)])
fi

AM_CONDITIONAL([USE_VST], [test "$use_vst" = yes])
if [[ "$use_vst" = "yes" ]] ; then
   AC_DEFINE(USE_VST, 1,
             [Define if VST plug-in support is enabled])
fi

case "${host_os}" in
   cygwin*)
      AC_CONFIG_HEADER(src/configwin.h:src/configtemplate.h)
      ;;
   *)
      AC_CONFIG_HEADER(src/configunix.h:src/configtemplate.h)
      ;;
esac

# process Makefile.in's to generate Makefiles
AC_CONFIG_FILES([
   Makefile
   help/Makefile
   images/Makefile
   lib-src/Makefile
   po/Makefile.in
   src/audacity.desktop
   src/Makefile
   tests/Makefile
])

AC_OUTPUT

echo ""
echo "Finished configure:"

# When building from a temporary directory and not the top level directory,
# the locale files get copied and the Makefile.in.in adjusted.  Otherwise,
# the generated locale files will be placed in the top level "po" directory.
#
# The temporary Makefile.in.in is updated so we don't have to worry about
# committing changes that might get lost when regenerating config files.
if [[ ! -e "${ac_abs_top_builddir}/po/LINGUAS" ]]
then
   pushd >/dev/null "${ac_abs_top_builddir}/po/"
   if [[ "${ac_abs_top_builddir}" != "${ac_abs_top_srcdir}" ]]
   then
      cp -pr "${ac_abs_top_srcdir}/po/." .
      sed -e "s/@srcdir@/./" "${ac_abs_top_srcdir}/po/Makefile.in.in" >"Makefile.in.in"
   fi

   for lang in $(cat "${ac_abs_top_srcdir}/po/LINGUAS")
   do
      mkdir -p "${lang}/LC_MESSAGES"
      pushd >/dev/null "${lang}/LC_MESSAGES"
      ln -s "../../${lang}.gmo" "audacity.mo"
      popd >/dev/null
   done
   popd >/dev/null
fi

for lib in $LIBRARIES ; do
   eval LIB_USE_LOCAL=\$${lib}_USE_LOCAL
   eval LIB_USE_SYSTEM=\$${lib}_USE_SYSTEM
   if test "$LIB_USE_LOCAL" = "yes" ; then
      echo "$lib: using LOCAL libraries"
   elif test "$LIB_USE_SYSTEM" = "yes" ; then
      echo "$lib: using SYSTEM libraries"
   else
      echo "$lib: disabled"
   fi
done

if [[ "$use_ladspa" = "yes" ]] ; then
   echo "ladspa plugin support: enabled"
else
   echo "ladspa plugin support: disabled"
fi

if [[ "$use_audiounits" = "yes" ]] ; then
   echo "audiounit plugin support: enabled"
else
   echo "audiounit plugin support: disabled"
fi

if [[ "$use_vst" = "yes" ]] ; then
   echo "VST plugin support: enabled"
else
   echo "VST plugin support: disabled"
fi

echo "prefix=$prefix";

echo ""
echo "Run 'configure --help' for an explanation of these options,"
echo "otherwise run 'make' to build Audacity."
