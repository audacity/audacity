#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2001,2002,2003,2004,2005,2006,2007,2008,2009  Josh Coalson
#
#  This file is part the FLAC project.  FLAC is comprised of several
#  components distributed under different licenses.  The codec libraries
#  are distributed under Xiph.Org's BSD-like license (see the file
#  COPYING.Xiph in this distribution).  All other programs, libraries, and
#  plugins are distributed under the GPL (see COPYING.GPL).  The documentation
#  is distributed under the Gnu FDL (see COPYING.FDL).  Each file in the
#  FLAC distribution contains at the top the terms under which it may be
#  distributed.
#
#  Since this particular file is relevant to all components of FLAC,
#  it may be distributed under the Xiph.Org license, which is the least
#  restrictive of those mentioned above.  See the file COPYING.Xiph in this
#  distribution.

# NOTE that for many of the AM_CONDITIONALs we use the prefix FLaC__
# instead of FLAC__ since autoconf triggers off 'AC_' in strings

AC_PREREQ(2.60)
AC_INIT([flac], [1.3.1], [flac-dev@xiph.org], [flac], [https://www.xiph.org/flac/])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/flac/main.c])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign 1.11 -Wall tar-pax no-dist-gzip dist-xz subdir-objects])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

user_cflags=$CFLAGS

#Prefer whatever the current ISO standard is.
AC_PROG_CC_STDC
AC_USE_SYSTEM_EXTENSIONS
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
LT_INIT([win32-dll disable-static pic-only])
AM_PROG_AS
AC_PROG_CXX
XIPH_C_COMPILER_IS_CLANG
XIPH_GCC_REALLY_IS_GCC
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

AC_SYS_LARGEFILE
AC_FUNC_FSEEKO

AC_CHECK_SIZEOF(off_t,1)	# Fake default value.
AC_CHECK_SIZEOF([void*])
AC_SEARCH_LIBS([lround],[m], [AC_DEFINE(HAVE_LROUND,1,lround support)])

AC_LANG_PUSH([C++])
# c++ flavor first
AC_C_VARARRAYS
if test $ac_cv_c_vararrays = yes; then
	AC_DEFINE([HAVE_CXX_VARARRAYS], 1, [Define to 1 if C++ supports variable-length arrays.])
fi
AC_LANG_POP([C++])

# c flavor
AC_HEADER_STDC
AM_PROG_CC_C_O
AC_C_INLINE
AC_C_VARARRAYS
AC_C_TYPEOF

AC_CHECK_HEADERS([stdint.h inttypes.h byteswap.h sys/param.h termios.h x86intrin.h cpuid.h])

AC_HEADER_TIOCGWINSZ

XIPH_C_BSWAP32
XIPH_C_BSWAP16

ac_cv_c_big_endian=0
ac_cv_c_little_endian=0
AC_C_BIGENDIAN([ac_cv_c_big_endian=1], [ac_cv_c_little_endian=1], [
	AC_MSG_WARN([[*****************************************************************]])
	AC_MSG_WARN([[*** Not able to determine endian-ness of target processor.       ]])
	AC_MSG_WARN([[*** The constants CPU_IS_BIG_ENDIAN and CPU_IS_LITTLE_ENDIAN in  ]])
	AC_MSG_WARN([[*** config.h may need to be hand editied.                        ]])
	AC_MSG_WARN([[*****************************************************************]])
])
AC_DEFINE_UNQUOTED(CPU_IS_BIG_ENDIAN, ${ac_cv_c_big_endian},
					[Target processor is big endian.])
AC_DEFINE_UNQUOTED(CPU_IS_LITTLE_ENDIAN, ${ac_cv_c_little_endian},
					[Target processor is little endian.])
AC_DEFINE_UNQUOTED(WORDS_BIGENDIAN, ${ac_cv_c_big_endian},
					[Target processor is big endian.])

AC_ARG_ENABLE(asm-optimizations, AC_HELP_STRING([--disable-asm-optimizations], [Don't use any assembly optimization routines]), asm_opt=no, asm_opt=yes)
dnl ' Terminate the damn single quote
AM_CONDITIONAL(FLaC__NO_ASM, test "x$asm_opt" = xno)
if test "x$asm_opt" = xno ; then
AC_DEFINE(FLAC__NO_ASM)
AH_TEMPLATE(FLAC__NO_ASM, [define to disable use of assembly code])
fi

# For the XMMS plugin.
AC_CHECK_TYPES(socklen_t, [], [])

dnl check for getopt in standard library
dnl AC_CHECK_FUNCS(getopt_long , , [LIBOBJS="$LIBOBJS getopt.o getopt1.o"] )
AC_CHECK_FUNCS(getopt_long, [], [])

AC_CHECK_SIZEOF(void*,1)

asm_optimisation=no
case "$host_cpu" in
	x86_64)
		if test $ac_cv_sizeof_voidp = 4 ; then
			# This must be a 32 bit user space running on 64 bit kernel so treat
			# this as ia32.
			cpu_ia32=true
			AC_DEFINE(FLAC__CPU_IA32)
			AH_TEMPLATE(FLAC__CPU_IA32, [define if building for ia32/i386])
		else
			# x86_64 user space and kernel.
			cpu_x86_64=true
			AC_DEFINE(FLAC__CPU_X86_64)
			AH_TEMPLATE(FLAC__CPU_X86_64, [define if building for x86_64])
			fi
		asm_optimisation=$asm_opt
		;;
	i*86)
		cpu_ia32=true
		AC_DEFINE(FLAC__CPU_IA32)
		AH_TEMPLATE(FLAC__CPU_IA32, [define if building for ia32/i386])
		asm_optimisation=$asm_opt
		;;
	powerpc|powerpc64)
		cpu_ppc=true
		AC_DEFINE(FLAC__CPU_PPC)
		AH_TEMPLATE(FLAC__CPU_PPC, [define if building for PowerPC])
		asm_optimisation=$asm_opt
		;;
	sparc)
		cpu_sparc=true
		AC_DEFINE(FLAC__CPU_SPARC)
		AH_TEMPLATE(FLAC__CPU_SPARC, [define if building for SPARC])
		asm_optimisation=$asm_opt
		;;
esac
AM_CONDITIONAL(FLAC__CPU_X86_64, test "x$cpu_x86_64" = xtrue)
AM_CONDITIONAL(FLaC__CPU_IA32, test "x$cpu_ia32" = xtrue)
AM_CONDITIONAL(FLaC__CPU_PPC, test "x$cpu_ppc" = xtrue)
AM_CONDITIONAL(FLaC__CPU_SPARC, test "x$cpu_sparc" = xtrue)

AC_DEFINE_UNQUOTED([FLAC__HAS_X86INTRIN],${HAVE_X86INTRIN_H}, [Set to 1 if <x86intrin.h> is available.])

case "$host" in
	i386-*-openbsd3.[[0-3]]) OBJ_FORMAT=aoutb ;;
	*-*-cygwin|*mingw*) OBJ_FORMAT=win32 ;;
	*-*-darwin*) OBJ_FORMAT=macho ;;
	*emx*) OBJ_FORMAT=aout ;;
	*) OBJ_FORMAT=elf ;;
esac
AC_SUBST(OBJ_FORMAT)
case "$host" in
	*-gnuspe)
		abi_spe=true
		AC_DEFINE(FLAC__CPU_PPC_SPE)
		AH_TEMPLATE(FLAC__CPU_PPC_SPE, [define if building for PowerPC with SPE ABI])
		;;
esac
AM_CONDITIONAL(FLaC__CPU_PPC_SPE, test "x$abi_spe" = xtrue)

os_is_windows=no
case "$host" in
	*-*-cygwin|*mingw*)
		CPPFLAGS="-D__MSVCRT_VERSION__=0x0601 $CPPFLAGS"
		os_is_windows=yes
		;;
esac

AM_CONDITIONAL(OS_IS_WINDOWS, test "x$os_is_windows" = xyes)

case "$host" in
	*-pc-linux-gnu)
		sys_linux=true
		AC_DEFINE(FLAC__SYS_LINUX)
		AH_TEMPLATE(FLAC__SYS_LINUX, [define if building for Linux])
		;;
	*-*-darwin*)
		sys_darwin=true
		AC_DEFINE(FLAC__SYS_DARWIN)
		AH_TEMPLATE(FLAC__SYS_DARWIN, [define if building for Darwin / MacOS X])
		;;
esac
AM_CONDITIONAL(FLaC__SYS_DARWIN, test "x$sys_darwin" = xtrue)
AM_CONDITIONAL(FLaC__SYS_LINUX, test "x$sys_linux" = xtrue)

if test "x$cpu_ia32" = xtrue || test "x$cpu_x86_64" = xtrue ; then
AC_DEFINE(FLAC__ALIGN_MALLOC_DATA)
AH_TEMPLATE(FLAC__ALIGN_MALLOC_DATA, [define to align allocated memory on 32-byte boundaries])
fi

AC_ARG_ENABLE(debug,
AC_HELP_STRING([--enable-debug], [Turn on debugging]),
[case "${enableval}" in
	yes) debug=true ;;
	no)  debug=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
esac],[debug=false])
AM_CONDITIONAL(DEBUG, test "x$debug" = xtrue)

AC_ARG_ENABLE(sse,
AC_HELP_STRING([--disable-sse], [Disable SSE if the OS does not support SSE instructions]),
[case "${enableval}" in
	yes) sse_os=yes ;;
	no)  sse_os=no ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-sse) ;;
esac],[sse_os=yes])
AM_CONDITIONAL(FLaC__SSE_OS, test "x$sse_os" = xtrue)
if test "x$sse_os" = xyes ; then
AC_DEFINE(FLAC__SSE_OS)
AH_TEMPLATE(FLAC__SSE_OS, [define if your operating system supports SSE instructions])
fi

AC_ARG_ENABLE(altivec,
AC_HELP_STRING([--disable-altivec], [Disable Altivec optimizations]),
[case "${enableval}" in
	yes) use_altivec=true ;;
	no)  use_altivec=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-altivec) ;;
esac],[use_altivec=true])
AM_CONDITIONAL(FLaC__USE_ALTIVEC, test "x$use_altivec" = xtrue)
if test "x$use_altivec" = xtrue ; then
AC_DEFINE(FLAC__USE_ALTIVEC)
AH_TEMPLATE(FLAC__USE_ALTIVEC, [define to enable use of Altivec instructions])
fi

AC_ARG_ENABLE(thorough-tests,
AC_HELP_STRING([--disable-thorough-tests], [Disable thorough (long) testing, do only basic tests]),
[case "${enableval}" in
	yes) thorough_tests=true ;;
	no)  thorough_tests=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-thorough-tests) ;;
esac],[thorough_tests=true])
AC_ARG_ENABLE(exhaustive-tests,
AC_HELP_STRING([--enable-exhaustive-tests], [Enable exhaustive testing (VERY long)]),
[case "${enableval}" in
	yes) exhaustive_tests=true ;;
	no)  exhaustive_tests=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-exhaustive-tests) ;;
esac],[exhaustive_tests=false])
if test "x$thorough_tests" = xfalse ; then
FLAC__TEST_LEVEL=0
elif test "x$exhaustive_tests" = xfalse ; then
FLAC__TEST_LEVEL=1
else
FLAC__TEST_LEVEL=2
fi
AC_SUBST(FLAC__TEST_LEVEL)

AC_ARG_ENABLE(werror,
	AC_HELP_STRING([--enable-werror], [Enable -Werror in all Makefiles]))

AC_ARG_ENABLE(stack-smash-protection,
	AC_HELP_STRING([--enable-stack-smash-protection], [Enable GNU GCC stack smash protection]))

AC_ARG_ENABLE(valgrind-testing,
AC_HELP_STRING([--enable-valgrind-testing], [Run all tests inside Valgrind]),
[case "${enableval}" in
	yes) FLAC__TEST_WITH_VALGRIND=yes ;;
	no)  FLAC__TEST_WITH_VALGRIND=no ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-valgrind-testing) ;;
esac],[FLAC__TEST_WITH_VALGRIND=no])
AC_SUBST(FLAC__TEST_WITH_VALGRIND)

AC_ARG_ENABLE(doxygen-docs,
AC_HELP_STRING([--disable-doxygen-docs], [Disable API documentation building via Doxygen]),
[case "${enableval}" in
	yes) enable_doxygen_docs=true ;;
	no)  enable_doxygen_docs=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-doxygen-docs) ;;
esac],[enable_doxygen_docs=true])
if test "x$enable_doxygen_docs" != xfalse ; then
	AC_CHECK_PROGS(DOXYGEN, doxygen)
fi
AM_CONDITIONAL(FLaC__HAS_DOXYGEN, test -n "$DOXYGEN")

AC_ARG_ENABLE(local-xmms-plugin,
AC_HELP_STRING([--enable-local-xmms-plugin], [Install XMMS plugin to ~/.xmms/Plugins instead of system location]),
[case "${enableval}" in
	yes) install_xmms_plugin_locally=true ;;
	no)  install_xmms_plugin_locally=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-local-xmms-plugin) ;;
esac],[install_xmms_plugin_locally=false])
AM_CONDITIONAL(FLaC__INSTALL_XMMS_PLUGIN_LOCALLY, test "x$install_xmms_plugin_locally" = xtrue)

AC_ARG_ENABLE(xmms-plugin,
AC_HELP_STRING([--disable-xmms-plugin], [Do not build XMMS plugin]),
[case "${enableval}" in
	yes) enable_xmms_plugin=true ;;
	no)  enable_xmms_plugin=false ;;
	*) AC_MSG_ERROR(bad value ${enableval} for --enable-xmms-plugin) ;;
esac],[enable_xmms_plugin=true])
if test "x$enable_xmms_plugin" != xfalse ; then
	AM_PATH_XMMS(0.9.5.1, , AC_MSG_WARN([*** XMMS >= 0.9.5.1 not installed - XMMS support will not be built]))
fi
AM_CONDITIONAL(FLaC__HAS_XMMS, test -n "$XMMS_INPUT_PLUGIN_DIR")

dnl build FLAC++ or not
AC_ARG_ENABLE([cpplibs],
AC_HELP_STRING([--disable-cpplibs], [Do not build libFLAC++]),
[case "${enableval}" in
	yes) disable_cpplibs=false ;;
	no)  disable_cpplibs=true ;;
	*)   AC_MSG_ERROR(bad value ${enableval} for --enable-cpplibs) ;;
esac], [disable_cpplibs=false])
AM_CONDITIONAL(FLaC__WITH_CPPLIBS, [test "x$disable_cpplibs" != xtrue])

dnl check for ogg library
AC_ARG_ENABLE([ogg],
	AC_HELP_STRING([--disable-ogg], [Disable ogg support (default: test for libogg)]),
	[ want_ogg=$enableval ], [ want_ogg=yes ] )

if test "x$want_ogg" != "xno"; then
	XIPH_PATH_OGG(have_ogg=yes, AC_MSG_WARN([*** Ogg development enviroment not installed - Ogg support will not be built]))
fi

AM_CONDITIONAL(FLaC__HAS_OGG, [test "x$have_ogg" = xyes])
if test "x$have_ogg" = xyes ; then
	AC_DEFINE_UNQUOTED([FLAC__HAS_OGG],1,[define if you have the ogg library])
	OGG_PACKAGE="ogg"
else
	AC_DEFINE_UNQUOTED([FLAC__HAS_OGG],0)
	have_ogg=no
fi
AC_SUBST(OGG_PACKAGE)

dnl check for i18n(internationalization); these are from libiconv/gettext
AM_ICONV
AM_LANGINFO_CODESET

AC_CHECK_PROGS(DOCBOOK_TO_MAN, docbook-to-man docbook2man)
AM_CONDITIONAL(FLaC__HAS_DOCBOOK_TO_MAN, test -n "$DOCBOOK_TO_MAN")
if test -n "$DOCBOOK_TO_MAN" ; then
AC_DEFINE(FLAC__HAS_DOCBOOK_TO_MAN)
AH_TEMPLATE(FLAC__HAS_DOCBOOK_TO_MAN, [define if you have docbook-to-man or docbook2man])
fi

# only matters for x86
AC_CHECK_PROGS(NASM, nasm)
AM_CONDITIONAL(FLaC__HAS_NASM, test -n "$NASM")
if test -n "$NASM" ; then
AC_DEFINE(FLAC__HAS_NASM)
AH_TEMPLATE(FLAC__HAS_NASM, [define if you are compiling for x86 and have the NASM assembler])
fi

if test "x$debug" = xtrue; then
	CPPFLAGS="-DDEBUG $CPPFLAGS"
	CFLAGS=$(echo "$CFLAGS" | sed 's/-g//')
	CFLAGS="-g $CFLAGS"
else
	CPPFLAGS="-DNDEBUG $CPPFLAGS"
	CFLAGS=$(echo "$CFLAGS" | sed 's/-O2//;s/-g//')
	CFLAGS="-O3 -funroll-loops $CFLAGS"
fi

XIPH_GCC_VERSION

if test x$ac_cv_c_compiler_gnu = xyes ; then
	CFLAGS="$CFLAGS -Wall -Wextra -Wstrict-prototypes -Wmissing-prototypes -Waggregate-return -Wcast-align -Wnested-externs -Wshadow -Wundef -Wmissing-declarations -Wunreachable-code -Winline " # -Wcast-qual -Wbad-function-cast -Wwrite-strings -Wconversion
	CXXFLAGS="$CXXFLAGS -Wall -Wextra -Wcast-align -Wshadow -Wwrite-strings -Wctor-dtor-privacy -Wnon-virtual-dtor -Wreorder -Wsign-promo -Wundef -Wunreachable-code " # -Wcast-qual -Wbad-function-cast -Wwrite-strings -Woverloaded-virtual -Wmissing-declarations

	XIPH_ADD_CFLAGS([-Wdeclaration-after-statement])
	XIPH_ADD_CFLAGS([-D_FORTIFY_SOURCE=2])

	AC_LANG_PUSH([C++])
	XIPH_ADD_CXXFLAGS([-Weffc++])
	AC_LANG_POP([C++])

	if test "$GCC_MAJOR_VERSION" -ge 4 && test "$OBJ_FORMAT" = elf; then
		CPPFLAGS="$CPPFLAGS -DFLAC__USE_VISIBILITY_ATTR"
		CFLAGS="$CFLAGS -fvisibility=hidden"
		CXXFLAGS="$CXXFLAGS -fvisibility=hidden"
		fi

	if test "$GCC_MAJOR_VERSION" -ge 4 && test "$OBJ_FORMAT" = macho; then
		CPPFLAGS="$CPPFLAGS -DFLAC__USE_VISIBILITY_ATTR"
		CFLAGS="$CFLAGS -fvisibility=hidden"
		CXXFLAGS="$CXXFLAGS -fvisibility=hidden"
		fi

	if test "x$GCC_MAJOR_VERSION$GCC_MINOR_VERSION" = "x42" ; then
		XIPH_ADD_CFLAGS([-fgnu89-inline])
		fi

	if test "x$asm_optimisation$sse_os" = "xyesyes" ; then
		XIPH_ADD_CFLAGS([-msse2])
		fi

	fi

XIPH_ADD_CFLAGS([-Wextra])

if test x$enable_werror = "xyes" ; then
	XIPH_ADD_CFLAGS([-Werror])
	AC_LANG_PUSH([C++])
	XIPH_ADD_CXXFLAGS([-Werror])
	AC_LANG_POP([C++])
	fi

if test x$enable_stack_smash_protection = "xyes" ; then
	XIPH_GCC_STACK_PROTECTOR
	XIPH_GXX_STACK_PROTECTOR
	fi

AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	src/libFLAC/Makefile \
	src/libFLAC/flac.pc \
	src/libFLAC/ia32/Makefile \
	src/libFLAC/include/Makefile \
	src/libFLAC/include/private/Makefile \
	src/libFLAC/include/protected/Makefile \
	src/libFLAC++/Makefile \
	src/libFLAC++/flac++.pc \
	src/flac/Makefile \
	src/metaflac/Makefile \
	src/plugin_common/Makefile \
	src/plugin_xmms/Makefile \
	src/share/Makefile \
	src/test_grabbag/Makefile \
	src/test_grabbag/cuesheet/Makefile \
	src/test_grabbag/picture/Makefile \
	src/test_libs_common/Makefile \
	src/test_libFLAC/Makefile \
	src/test_libFLAC++/Makefile \
	src/test_seeking/Makefile \
	src/test_streams/Makefile \
	src/utils/Makefile \
	src/utils/flacdiff/Makefile \
	src/utils/flactimer/Makefile \
	examples/Makefile \
	examples/c/Makefile \
	examples/c/decode/Makefile \
	examples/c/decode/file/Makefile \
	examples/c/encode/Makefile \
	examples/c/encode/file/Makefile \
	examples/cpp/Makefile \
	examples/cpp/decode/Makefile \
	examples/cpp/decode/file/Makefile \
	examples/cpp/encode/Makefile \
	examples/cpp/encode/file/Makefile \
	include/Makefile \
	include/FLAC/Makefile \
	include/FLAC++/Makefile \
	include/share/Makefile \
	include/share/grabbag/Makefile \
	include/test_libs_common/Makefile \
	doc/Doxyfile \
	doc/Makefile \
	doc/html/Makefile \
	doc/html/images/Makefile \
	m4/Makefile \
	man/Makefile \
	test/common.sh \
	test/Makefile \
	test/cuesheets/Makefile \
	test/flac-to-flac-metadata-test-files/Makefile \
	test/metaflac-test-files/Makefile \
	test/pictures/Makefile \
	build/Makefile \
	objs/Makefile \
	objs/debug/Makefile \
	objs/debug/bin/Makefile \
	objs/debug/lib/Makefile \
	objs/release/Makefile \
	objs/release/bin/Makefile \
	objs/release/lib/Makefile \
])
AC_OUTPUT

AC_MSG_RESULT([
-=-=-=-=-=-=-=-=-=-= Configuration Complete =-=-=-=-=-=-=-=-=-=-

  Configuration summary :

    FLAC version : ........................ ${VERSION}

    Host CPU : ............................ ${host_cpu}
    Host Vendor : ......................... ${host_vendor}
    Host OS : ............................. ${host_os}
])

	echo "    Compiler is GCC : ..................... ${ac_cv_c_compiler_gnu}"
if test x$ac_cv_c_compiler_gnu = xyes ; then
	echo "    GCC version : ......................... ${GCC_VERSION}"
fi
	echo "    Compiler is Clang : ................... ${xiph_cv_c_compiler_clang}"
	echo "    SSE optimizations : ................... ${sse_os}"
	echo "    Asm optimizations : ................... ${asm_optimisation}"
	echo "    Ogg/FLAC support : .................... ${have_ogg}"
echo
