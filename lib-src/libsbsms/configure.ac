AC_INIT([sbsms],[2.2.0])

dnl Use the m4/ directory to contain libtool macros that will be needed to 
dnl go with the ltmain.sh script (both will be created/updated via the command
dnl libtoolize --copy --force
AC_CONFIG_MACRO_DIR([m4])

#------------------------------------------------------------------------------------
# Library's "official" release version:

LIBSBSMS_RELEASE_MAJOR=2
LIBSBSMS_RELEASE_MINOR=1
LIBSBSMS_RELEASE_BUILD=0
AC_CONFIG_SRCDIR([src/sbsms.cpp])
AC_CANONICAL_TARGET([])
AC_CONFIG_HEADERS([src/config.h])

AC_DISABLE_SHARED dnl allows for optimizations

#------------------------------------------------------------------------------------
# The following is the libtool / shared library version. This doesn't have to
# do anything with the release version. It MUST conform to the following rules:
#
#  1. Start with version information of `0:0:0' for each libtool library.
#  2. Update the version information only immediately before a public release of
#     your software. More frequent updates are unnecessary, and only guarantee
#     that the current interface number gets larger faster.
#  3. If the library source code has changed at all since the last update, then
#     increment revision (`c:r:a' becomes `c:r+1:a').
#  4. If any interfaces have been added, removed, or changed since the last update,
#     increment current, and set revision to 0.
#  5. If any interfaces have been added since the last public release, then increment
#     age.
#  6. If any interfaces have been removed since the last public release, then set age
#     to 0.

SHARED_VERSION_INFO="10:0:0"

AM_PROG_AR
LT_INIT

# save $CXXFLAGS etc. since AC_PROG_CXX likes to insert "-g -O2"
# if $CXXFLAGS is blank and it finds GCC
cflags_save="$CFLAGS"
cppflags_save="$CPPFLAGS"
cxxflags_save="$CXXFLAGS"
AC_LANG([C++])
AC_PROG_CXX
AC_PROG_CXXCPP
AX_COMPILER_VENDOR
# restore those variables back
CFLAGS="$cflags_save"
CPPFLAGS="$cppflags_save"
CXXFLAGS="$cxxflags_save"

AC_C99_FUNC_LRINT()
AC_C99_FUNC_LRINTF()

AC_SUBST(SHLIB_VERSION_ARG)
AC_SUBST(SHARED_VERSION_INFO)

AC_ARG_ENABLE(static,[  --enable-static enable static build: (default: disable)],[enable_static=$enableval],[enable_static=no])

AC_ARG_ENABLE(debug,[  --enable-debug enable debug build: (default: disable)],[enable_debug=$enableval],[enable_debug=no])

AC_ARG_ENABLE(multithreaded,[  --enable-multithreaded enable multithreaded build: (default: disable)],[enable_multithreaded=$enableval],[enable_multithreaded=no])

AC_ARG_ENABLE(sse, [AS_HELP_STRING([--enable-sse],[enable SSE optimizations])], enable_sse=$enableval, enable_sse=yes)

AC_ARG_ENABLE(universal_binary,[  --enable-universal_binary enable universal binary build: (default: disable)],[enable_universal_binary=$enableval],[enable_universal_binary=no])

SBSMS_CFLAGS="-ffast-math -funroll-loops"

if test x$enable_static = xno; then
  case "$target_os" in
     darwin*)
	SBSMS_CFLAGS="$SBSMS_CLFAGS -mdynamic-no-pic"
	;;
	*)
	;;
   esac
fi

if test x$enable_debug = xyes; then
   SBSMS_CFLAGS="$SBSMS_CFLAGS -g -O3"
else
   SBSMS_CFLAGS="$SBSMS_CFLAGS -O3 -fomit-frame-pointer"
fi

if test x$enable_multithreaded = xyes; then
    AC_DEFINE(MULTITHREADED,1,[Define to compile multithreaded sbsms])
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

if test x$enable_universal_binary = xyes; then
   case "$target_os" in
     darwin*)
        SBSMS_CFLAGS="$SBSMS_CFLAGS -mmacosx-version-min=10.4 -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.4u.sdk -Xarch_i386 -DAPPLE_I386 -Xarch_ppc -DAPPLE_PPC"
	;;
     *)
	;;
   esac
fi

AC_SUBST(SBSMS_CFLAGS)

AM_INIT_AUTOMAKE([no-dependencies])

AM_MAINTAINER_MODE([disable])

AC_CONFIG_FILES([Makefile src/Makefile libsbsms.spec sbsms.pc])

AC_OUTPUT
