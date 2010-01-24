# Copyright (C) 1999-2008 Erik de Castro Lopo (erikd AT mega-nerd DOT com).

dnl Require autoconf version
AC_PREREQ(2.57)

AC_INIT([libsndfile],[1.0.18],[erikd@mega-nerd.com])

# Put config stuff in Cfg.
AC_CONFIG_AUX_DIR(Cfg)

AC_CONFIG_SRCDIR([src/sndfile.c])
AC_CANONICAL_TARGET([])

AC_CONFIG_MACRO_DIR([M4])

AM_INIT_AUTOMAKE($PACKAGE_NAME,$PACKAGE_VERSION)
dnl Audacity policy: don't enable automatic rebuild of configure et al if 
dnl sources change
AM_MAINTAINER_MODE([enabled])

AC_CONFIG_HEADERS([src/config.h])

dnl Add parameters for aclocal
AC_SUBST(ACLOCAL_AMFLAGS, "-I M4")

AC_LANG([C])

#------------------------------------------------------------------------------------
# Rules for library version information:
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

SHARED_VERSION_INFO="1:18:0"

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX

# Do not check for F77.
define([AC_LIBTOOL_LANG_F77_CONFIG], [:])dnl

AM_PROG_LIBTOOL

AC_CHECK_PROG(HAVE_AUTOGEN, autogen, yes, no)
AC_CHECK_PROG(HAVE_WINE, wine, yes, no)

AC_PROG_INSTALL
AC_PROG_LN_S

AC_HEADER_STDC

AC_CHECK_HEADERS(endian.h)
AC_CHECK_HEADERS(byteswap.h)
AC_CHECK_HEADERS(locale.h)
AC_CHECK_HEADERS(sys/time.h)

AC_HEADER_SYS_WAIT

AC_CHECK_DECLS(S_IRGRP)
if test x$ac_cv_have_decl_S_IRGRP = xyes ; then
	AC_DEFINE_UNQUOTED([HAVE_DECL_S_IRGRP],1,[Set to 1 if S_IRGRP is defined.])
else
	AC_DEFINE_UNQUOTED([HAVE_DECL_S_IRGRP],0)
	fi

AM_CONDITIONAL([LINUX_MINGW_CROSS_TEST],
	[test "$build_os:$target_os:$host_os:$HAVE_WINE" = "linux-gnu:mingw32msvc:mingw32msvc:yes"])

#====================================================================================
# Check for support of the struct hack.

AC_C99_FLEXIBLE_ARRAY

if test x$ac_cv_c99_flexible_array = xyes ; then
	AC_DEFINE([HAVE_FLEXIBLE_ARRAY],1, [Set to 1 if the compile supports the struct hack.])
else
	AC_MSG_WARN([[*** This compiler does not support the 1999 ISO C Standard ***]])
	AC_MSG_WARN([[*** feature known as the flexible array struct member.     ***]])
	AC_DEFINE([HAVE_FLEXIBLE_ARRAY],0)
	fi

#====================================================================================
# Couple of initializations here. Fill in real values later.

SHLIB_VERSION_ARG=""

#====================================================================================
# Finished checking, handle options.

AC_ARG_ENABLE(experimental,
	AC_HELP_STRING([--enable-experimental], [enable experimental code]))

EXPERIMENTAL_CODE=0
if test x$enable_experimental = xyes ; then
	EXPERIMENTAL_CODE=1
	fi
AC_DEFINE_UNQUOTED([ENABLE_EXPERIMENTAL_CODE],${EXPERIMENTAL_CODE}, [Set to 1 to enable experimental code.])

AC_ARG_ENABLE(gcc-werror,
	AC_HELP_STRING([--enable-gcc-werror], [enable -Werror in all Makefiles]))

AC_ARG_ENABLE(gcc-pipe,
	AC_HELP_STRING([--disable-gcc-pipe], [disable gcc -pipe option]))

AC_ARG_ENABLE(gcc-opt,
	AC_HELP_STRING([--disable-gcc-opt], [disable gcc optimisations]))

AC_ARG_ENABLE(cpu-clip,
	AC_HELP_STRING([--disable-cpu-clip], [disable tricky cpu specific clipper]))

AC_ARG_ENABLE(bow-docs,
	AC_HELP_STRING([--enable-bow-docs], [enable black-on-white html docs]))

AC_ARG_ENABLE(sqlite,
	AC_HELP_STRING([--disable-sqlite], [disable use of sqlite]))

AC_ARG_ENABLE(alsa,
	AC_HELP_STRING([--disable-alsa], [disable use of ALSA]))

AC_ARG_ENABLE(external-libs,
	AC_HELP_STRING([--disable-external-libs], [disable use of FLAC, Ogg and Vorbis]))

AC_ARG_ENABLE(octave,
	AC_HELP_STRING([--enable-octave], [disable building of GNU Octave module]))

AC_ARG_ENABLE(test-coverage,
	AC_HELP_STRING([--enable-test-coverage], [enable test coverage]))
AM_CONDITIONAL([ENABLE_TEST_COVERAGE], [test "$enable_test_coverage" = yes])

#====================================================================================
# Check types and their sizes.

AC_CHECK_SIZEOF(short,2)
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(long,4)
AC_CHECK_SIZEOF(float,4)
AC_CHECK_SIZEOF(double,4)
AC_CHECK_SIZEOF(void*,8)
AC_CHECK_SIZEOF(size_t,4)
AC_CHECK_SIZEOF(int64_t,8)
AC_CHECK_SIZEOF(long long,8)

#====================================================================================
# Find an appropriate type for sf_count_t.
# On systems supporting files larger than 2 Gig, sf_count_t must be a 64 bit value.
# Unfortunately there is more than one way of ensuring this so need to do some
# pretty rigourous testing here.

unset ac_cv_sizeof_off_t

AC_CHECK_SIZEOF(off_t,1)	# Fake default value.

case "$host_os" in
	mingw*)
		TYPEOF_SF_COUNT_T="__int64"
		SF_COUNT_MAX="0x7FFFFFFFFFFFFFFFLL"
		SIZEOF_SF_COUNT_T=8
		;;
	*)
		if test "x$ac_cv_sizeof_off_t" = "x8" ; then
			# If sizeof (off_t) is 8, no further checking is needed.
			TYPEOF_SF_COUNT_T="off_t"
			SF_COUNT_MAX="0x7FFFFFFFFFFFFFFFLL"
			SIZEOF_SF_COUNT_T=8
		else
			# Check for common 64 bit file offset types.
			AC_CHECK_SIZEOF(loff_t,1)	# Fake default value.
			AC_CHECK_SIZEOF(off64_t,1)	# Fake default value.

			TYPEOF_SF_COUNT_T="unknown"
			if test "x$ac_cv_sizeof_loff_t" = "x8" ; then
				TYPEOF_SF_COUNT_T="loff_t"
				SIZEOF_SF_COUNT_T=8
			elif test "x$ac_cv_sizeof_off64_t" = "x8" ; then
				TYPEOF_SF_COUNT_T="off64_t"
				SIZEOF_SF_COUNT_T=8
				fi

			# Save the old sizeof (off_t) value  and then unset it to see if it
			# changes when Large File Support is enabled.

			pre_largefile_sizeof_off_t=$ac_cv_sizeof_off_t
			unset ac_cv_sizeof_off_t

			AC_SYS_EXTRA_LARGEFILE

			if test "x$ac_cv_sys_largefile_CFLAGS" = "xno" ; then
				ac_cv_sys_largefile_CFLAGS=""
				fi
			if test "x$ac_cv_sys_largefile_LDFLAGS" = "xno" ; then
				ac_cv_sys_largefile_LDFLAGS=""
				fi
			if test "x$ac_cv_sys_largefile_LIBS" = "xno" ; then
				ac_cv_sys_largefile_LIBS=""
				fi

			AC_CHECK_SIZEOF(off_t,1)	# Fake default value.

			if test "x$ac_cv_sizeof_off_t" = "x8" ; then
				SF_COUNT_MAX="0x7FFFFFFFFFFFFFFFLL"
			elif test "x$ac_cv_sizeof_off_t" = "x$pre_largefile_sizeof_off_t" ; then
				AC_MSG_WARN([[This machine does not seem to support 64 bit file offsets.]])
				TYPEOF_SF_COUNT_T="off_t"
				SIZEOF_SF_COUNT_T=$ac_cv_sizeof_off_t
			elif test "x$TYPEOF_SF_COUNT_T" = "xunknown" ; then
				echo
				echo "*** The configure process has determined that this system is capable"
				echo "*** of Large File Support but has not been able to find a type which"
				echo "*** is an unambiguous 64 bit file offset."
				echo "*** Please contact the author to help resolve this problem."
				echo
				AC_MSG_ERROR([[Bad file offset type.]])
				fi
			fi
		;;
	esac

if test $SIZEOF_SF_COUNT_T = 4 ; then
	SF_COUNT_MAX="0x7FFFFFFF"
	fi

AC_DEFINE_UNQUOTED([TYPEOF_SF_COUNT_T],${TYPEOF_SF_COUNT_T}, [Set to long if unknown.])
AC_SUBST(TYPEOF_SF_COUNT_T)

AC_DEFINE_UNQUOTED([SIZEOF_SF_COUNT_T],${SIZEOF_SF_COUNT_T}, [Set to sizeof (long) if unknown.])
AC_SUBST(SIZEOF_SF_COUNT_T)

AC_DEFINE_UNQUOTED([SF_COUNT_MAX],${SF_COUNT_MAX}, [Set to maximum allowed value of sf_count_t type.])
AC_SUBST(SF_COUNT_MAX)

AC_CHECK_TYPES(ssize_t)
AC_CHECK_SIZEOF(ssize_t,4)

#====================================================================================
# Determine endian-ness of target processor.

AC_C_FIND_ENDIAN

AC_DEFINE_UNQUOTED(CPU_IS_BIG_ENDIAN, ${ac_cv_c_big_endian},
	[Target processor is big endian.])
AC_DEFINE_UNQUOTED(CPU_IS_LITTLE_ENDIAN, ${ac_cv_c_little_endian},
	[Target processor is little endian.])
AC_DEFINE_UNQUOTED(WORDS_BIGENDIAN, ${ac_cv_c_big_endian},
	[Target processor is big endian.])

#====================================================================================
# Check for functions.

AC_CHECK_FUNCS(malloc calloc realloc free)
AC_CHECK_FUNCS(open read write lseek pread pwrite)
AC_CHECK_FUNCS(fstat ftruncate fsync)
AC_CHECK_FUNCS(snprintf vsnprintf)
AC_CHECK_FUNCS(gmtime gmtime_r localtime localtime_r gettimeofday)
AC_CHECK_FUNCS(mmap getpagesize)
AC_CHECK_FUNCS(setlocale)

AC_CHECK_LIB([m],floor)
AC_CHECK_FUNCS(floor ceil fmod)

AC_C99_FUNC_LRINT
AC_C99_FUNC_LRINTF

#====================================================================================
# Check for requirements for building plugins for other languages/enviroments.

dnl Octave maths environment http://www.octave.org/
if test x$cross_compiling = xno ; then
	if test x$enable_octave = xno ; then
		AM_CONDITIONAL(BUILD_OCTAVE_MOD, false)
	else
		AC_OCTAVE_BUILD
		fi
else
	AM_CONDITIONAL(BUILD_OCTAVE_MOD, false)
	fi

#====================================================================================
# Check for Ogg, Vorbis and FLAC.

HAVE_EXTERNAL_LIBS=0
EXTERNAL_CFLAGS=""
EXTERNAL_LIBS=""

# Check for pkg-config outside the if statement.
PKG_PROG_PKG_CONFIG

if test x$enable_external_libs = xno ; then
	AC_MSG_WARN([[*** External libs (FLAC, Ogg, Vorbis) disabled. ***]])
else
	PKG_CHECK_MOD_VERSION(FLAC, flac >= 1.2.1, ac_cv_flac=yes, ac_cv_flac=no)
	PKG_CHECK_MOD_VERSION(OGG, ogg >= 1.1.3, ac_cv_ogg=yes, ac_cv_ogg=no)

	# Yes we really do need vorbis >= 1.2.1 because earlier versions are
	# not const correct at the API level.
	PKG_CHECK_MOD_VERSION(VORBIS, vorbis >= 1.2.1, ac_cv_vorbis=yes, ac_cv_vorbis=no)
	PKG_CHECK_MOD_VERSION(VORBISENC, vorbisenc >= 1.2.1, ac_cv_vorbisenc=yes, ac_cv_vorbisenc=no)
	fi

if test x$ac_cv_flac$ac_cv_ogg$ac_cv_vorbis$ac_cv_vorbisenc = "xyesyesyesyes" ; then
	HAVE_EXTERNAL_LIBS=1
	enable_external_libs=yes

	EXTERNAL_CFLAGS="$FLAC_CFLAGS $OGG_CFLAGS $VORBISENC_CFLAGS"
	EXTERNAL_LIBS="$FLAC_LIBS $VORBISENC_LIBS"
	fi

AC_DEFINE_UNQUOTED([HAVE_EXTERNAL_LIBS],$HAVE_EXTERNAL_LIBS,[Will be set to 1 if flac, ogg and vorbis are available.])

#====================================================================================
# Check for libsqlite3 (only used in regtest).

ac_cv_sqlite3=no
if test x$enable_sqlite != xno ; then
	PKG_CHECK_MOD_VERSION(SQLITE3, sqlite3 >= 3.2, ac_cv_sqlite3=yes, ac_cv_sqlite3=no)
	fi

if test x$ac_cv_sqlite3 = "xyes" ; then
	HAVE_SQLITE3=1
else
	HAVE_SQLITE3=0
	fi

AC_DEFINE_UNQUOTED([HAVE_SQLITE3],$HAVE_SQLITE3,[Set to 1 if you have libsqlite3.])

#====================================================================================
# Check for JACK (only used for examples/sndfile-jackplay).

PKG_CHECK_MOD_VERSION(JACK, jack >= 0.100, ac_cv_jack=yes, ac_cv_jack=no)

if test x$ac_cv_jack = "xyes" ; then
	HAVE_JACK=1
else
	HAVE_JACK=0
	fi

AC_DEFINE_UNQUOTED([HAVE_JACK],$HAVE_JACK,[Set to 1 if you have JACK.])

#====================================================================================
# Determine if the processor can do clipping on float to int conversions.

if test x$enable_cpu_clip != "xno" ; then
	AC_C_CLIP_MODE
else
	echo "checking processor clipping capabilities... disabled"
	ac_cv_c_clip_positive=0
	ac_cv_c_clip_negative=0
	fi

AC_DEFINE_UNQUOTED(CPU_CLIPS_POSITIVE, ${ac_cv_c_clip_positive},
	[Target processor clips on positive float to int conversion.])
AC_DEFINE_UNQUOTED(CPU_CLIPS_NEGATIVE, ${ac_cv_c_clip_negative},
	[Target processor clips on negative float to int conversion.])

#====================================================================================
# Target OS specific stuff.

OS_SPECIFIC_CFLAGS=""
OS_SPECIFIC_LINKS=""
os_is_win32=0
os_is_macosx=0
use_windows_api=0

case "$host_os" in
	darwin* | rhapsody*)
		os_is_macosx=1
		OS_SPECIFIC_CFLAGS="-fpascal-strings -I/Developer/Headers/FlatCarbon"
		OS_SPECIFIC_LINKS="-framework CoreAudio"
		;;
	mingw*)
		os_is_win32=1
		use_windows_api=1
		OS_SPECIFIC_LINKS="-lwinmm"
		;;
	esac

AC_DEFINE_UNQUOTED(OS_IS_WIN32, ${os_is_win32}, [Set to 1 if compiling for Win32])
AC_DEFINE_UNQUOTED(OS_IS_MACOSX, ${os_is_macosx}, [Set to 1 if compiling for MacOSX])
AC_DEFINE_UNQUOTED(USE_WINDOWS_API, ${use_windows_api}, [Set to 1 to use the native windows API])

#====================================================================================
# Check for ALSA.

ALSA_LIBS=""

if test x$enable_alsa != xno ; then
	AC_CHECK_HEADERS(alsa/asoundlib.h)
	if test x$ac_cv_header_alsa_asoundlib_h = xyes ; then
		ALSA_LIBS="-lasound"
		enable_alsa=yes
		fi
	fi

#====================================================================================
# Test for sanity when cross-compiling.

if test x$cross_compiling = xyes ; then
	AC_MSG_WARN([[******************************************************************]])
	AC_MSG_WARN([[*** We are cross-compiling, so have to assume sizeof (short) == 2 ]])
	AC_MSG_WARN([[*** and sizeof (int) == 4. If this is not the case there is no    ]])
	AC_MSG_WARN([[*** chance of this working. Please contact the mantainer.         ]])
	AC_MSG_WARN([[******************************************************************]])
	fi

if test $ac_cv_sizeof_short != 2 ; then
	AC_MSG_WARN([[******************************************************************]])
	AC_MSG_WARN([[*** sizeof (short) != 2.                                          ]])
	AC_MSG_WARN([[******************************************************************]])
	fi

if test $ac_cv_sizeof_int != 4 ; then
	AC_MSG_WARN([[******************************************************************]])
	AC_MSG_WARN([[*** sizeof (int) != 4                                             ]])
	AC_MSG_WARN([[******************************************************************]])
	fi

if test $ac_cv_sizeof_float != 4 ; then
	AC_MSG_WARN([[******************************************************************]])
	AC_MSG_WARN([[*** sizeof (float) != 4.                                          ]])
	AC_MSG_WARN([[******************************************************************]])
	fi

if test $ac_cv_sizeof_double != 8 ; then
	AC_MSG_WARN([[******************************************************************]])
	AC_MSG_WARN([[*** sizeof (double) != 8.                                         ]])
	AC_MSG_WARN([[******************************************************************]])
	fi

if test x"$ac_cv_prog_HAVE_AUTOGEN" = "xno" ; then
	AC_MSG_WARN([[Touching files in directory tests/.]])
	touch tests/*.c tests/*.h
	fi

#====================================================================================
# Settings for the HTML documentation.

htmldocdir=$prefix/share/doc/libsndfile1-dev/html

if test $prefix = "NONE" ; then
	htmldocdir=/usr/local/share/doc/libsndfile1-dev/html
else
	htmldocdir=$prefix/share/doc/libsndfile1-dev/html
	fi

if test x$enable_bow_docs = "xyes" ; then
	HTML_BGCOLOUR="white"
	HTML_FGCOLOUR="black"
else
	HTML_BGCOLOUR="black"
	HTML_FGCOLOUR="white"
	fi

#====================================================================================
# Now use the information from the checking stage.

win32_target_dll=0
COMPILER_IS_GCC=0

if test x$ac_cv_c_compiler_gnu = xyes ; then
	AC_ADD_CFLAGS(-std=gnu99)

	AC_GCC_VERSION

	if test "x$GCC_MAJOR_VERSION$GCC_MINOR_VERSION" = "x42" ; then
		AC_MSG_WARN([****************************************************************])
		AC_MSG_WARN([** GCC version 4.2 warns about the inline keyword for no good **])
		AC_MSG_WARN([** reason but the maintainers do not see it as a bug.         **])
		AC_MSG_WARN([** See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=33995      **])
		AC_MSG_WARN([** Using -fgnu-inline to avoid this stupidity.                **])
		AC_MSG_WARN([****************************************************************])
		AC_ADD_CFLAGS([-fgnu89-inline])
		fi

	CFLAGS="$CFLAGS -Wall"
	CXXFLAGS="$CXXFLAGS -Wall"

	AC_ADD_CFLAGS([-Wextra])

	AC_LANG_PUSH([C++])
	AC_ADD_CXXFLAGS([-Wextra])
	AC_LANG_POP([C++])

	AC_ADD_CFLAGS([-Wdeclaration-after-statement])
	AC_ADD_CFLAGS([-Wpointer-arith])
	AC_ADD_CFLAGS([-funsigned-char])

	if test x$enable_gcc_werror = "xyes" ; then
		CFLAGS="-Werror $CFLAGS"
		CXXFLAGS="-Werror $CXXFLAGS"
		fi

	if test x$enable_test_coverage = "xyes" ; then
		# AC_ADD_CFLAGS([-ftest-coverage])
		AC_ADD_CFLAGS([-coverage])
		fi

	CFLAGS="$CFLAGS -Wstrict-prototypes -Wmissing-prototypes -Waggregate-return -Wcast-align -Wcast-qual -Wnested-externs -Wshadow -Wbad-function-cast -Wwrite-strings "
	# -Wundef -Wmissing-declarations -Winline -Wconversion"
	CXXFLAGS="$CXXFLAGS -Wcast-align -Wcast-qual -Wshadow -Wwrite-strings -Wctor-dtor-privacy -Wnon-virtual-dtor -Woverloaded-virtual -Wreorder -Wsign-promo "

	if test "x$enable_gcc_opt" = "xno" ; then
		temp_CFLAGS=`echo $CFLAGS | sed "s/O2/O0/"`
		CFLAGS=$temp_CFLAGS
		AC_MSG_WARN([[*** Compiler optimisations switched off. ***]])
		fi

	# OS specific tweaks.
	case "$host_os" in
		darwin* | rhapsody*)
			# Disable -Wall, -pedantic and -Wshadow for Apple Darwin/Rhapsody.
			# System headers on these systems are broken.
			temp_CFLAGS=`echo $CFLAGS | sed "s/-Wall -pedantic//" | sed "s/-Wshadow//" | sed "s/-Waggregate-return//"`
			CFLAGS=$temp_CFLAGS
			SHLIB_VERSION_ARG="-Wl,-exported_symbols_list -Wl,\$(srcdir)/Symbols.darwin"
			;;
		linux*)
			SHLIB_VERSION_ARG="-Wl,--version-script=\$(srcdir)/Symbols.linux"
			;;
		mingw*)
			# Linker flag '-Wl,--out-implib' does not work with mingw cross compiler
			# so we don't use it here.
			SHLIB_VERSION_ARG="-Wl,\$(srcdir)/libsndfile-1.def"
			win32_target_dll=1
			if test x"$enable_shared" = xno ; then
				win32_target_dll=0
				fi
			;;
		os2*)
			SHLIB_VERSION_ARG="-Wl,-export-symbols \$(srcdir)/Symbols.os2"
			;;
		*)
			;;
		esac
	if test x$enable_gcc_pipe != "xno" ; then
	 	CFLAGS="$CFLAGS -pipe"
	 	fi

	COMPILER_IS_GCC=1
	fi

AC_DEFINE_UNQUOTED([WIN32_TARGET_DLL], ${win32_target_dll}, [Set to 1 if windows DLL is being built.])
AC_DEFINE_UNQUOTED([COMPILER_IS_GCC], ${COMPILER_IS_GCC}, [Set to 1 if the compile is GNU GCC.])

CFLAGS="$CFLAGS $OS_SPECIFIC_CFLAGS"

if test x"$CFLAGS" = x ; then
	echo "Error in configure script. CFLAGS has been screwed up."
	exit
	fi

#-------------------------------------------------------------------------------

AC_SUBST(htmldocdir)
AC_SUBST(HTML_BGCOLOUR)
AC_SUBST(HTML_FGCOLOUR)

AC_SUBST(SHLIB_VERSION_ARG)
AC_SUBST(SHARED_VERSION_INFO)
AC_SUBST(OS_SPECIFIC_CFLAGS)
AC_SUBST(OS_SPECIFIC_LINKS)
AC_SUBST(ALSA_LIBS)
AC_SUBST(ENABLE_EXPERIMENTAL_CODE)

AC_SUBST(EXTERNAL_CFLAGS)
AC_SUBST(EXTERNAL_LIBS)

AC_SUBST(COMPILER_IS_GCC)
AC_SUBST(GCC_MAJOR_VERSION)

AC_SUBST(HAVE_JACK)

dnl The following line causes the libtool distributed with the source
dnl to be replaced if the build system has a more recent version.
AC_SUBST(LIBTOOL_DEPS)

AC_CONFIG_FILES([ \
	src/sndfile.h src/Makefile src/GSM610/Makefile src/G72x/Makefile \
	man/Makefile examples/Makefile tests/Makefile regtest/Makefile \
	M4/Makefile doc/Makefile Win32/Makefile Octave/Makefile \
	programs/Makefile doc/libsndfile.css \
	Makefile libsndfile.spec sndfile.pc \
	])
AC_OUTPUT

#====================================================================================

AC_MSG_RESULT([
-=-=-=-=-=-=-=-=-=-= Configuration Complete =-=-=-=-=-=-=-=-=-=-

  Configuration summary :

    Version : ............................. ${VERSION}
    Experimental code : ................... ${enable_experimental:-no}
    Using ALSA in example programs : ...... ${enable_alsa:-no}
    External FLAC/Ogg/Vorbis : ............ ${enable_external_libs:-no}

])

if test x$ac_cv_c_compiler_gnu = xyes ; then
	echo -e "  Tools :\n"
	echo "    Compiler is GCC : ..................... ${ac_cv_c_compiler_gnu}"
	echo "    GCC version : ......................... ${GCC_VERSION}"
	if test $GCC_MAJOR_VERSION -lt 3 ; then
		echo -e "\n    ** This compiler version allows applications to write"
		echo "    ** to static strings within the library."
		echo "    ** Compile with GCC version 3.X or above to avoid this problem."
		fi
	fi

if test $libdir = "\${exec_prefix}/lib" ; then
	libdir="$prefix/lib"
	fi

if test $bindir = "\${exec_prefix}/bin" ; then
	bindir="$prefix/bin"
	fi

AC_MSG_RESULT([[
  Installation directories :

    Library directory : ................... $libdir
    Program directory : ................... $bindir
    Pkgconfig directory : ................. $libdir/pkgconfig
    HTML docs directory : ................. $htmldocdir
]])

if test x$prefix != "x/usr" ; then
	echo "Compiling some other packages against libsndfile may require"
	echo -e "the addition of \"$libdir/pkgconfig\" to the"
	echo -e "PKG_CONFIG_PATH environment variable.\n"
	fi

(cd src && make genfiles)
(cd tests && make genfiles)
