# ===========================================================================
#   https://www.gnu.org/software/autoconf-archive/ax_compiler_version.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_COMPILER_VERSION
#
# DESCRIPTION
#
#   This macro retrieves the compiler version and returns it in the cache
#   variable $ax_cv_c_compiler_version for C and $ax_cv_cxx_compiler_version
#   for C++.
#
#   Version is returned as epoch:major.minor.patchversion
#
#   Epoch is used in order to have an increasing version number in case of
#   marketing change.
#
#   Epoch use: * borland compiler use chronologically 0turboc for turboc
#   era,
#
#     1borlanc BORLANDC++ before 5, 2cppbuilder for cppbuilder era,
#     3borlancpp for return of BORLANDC++ (after version 5.5),
#     4cppbuilder for cppbuilder with year version,
#     and 5xe for XE era.
#
#   An empty string is returned otherwise.
#
# LICENSE
#
#   Copyright (c) 2014 Bastien ROUCARIES <roucaries.bastien+autoconf@gmail.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 9

# for intel
AC_DEFUN([_AX_COMPILER_VERSION_INTEL],
  [ dnl
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    [__INTEL_COMPILER/100],,
    AC_MSG_FAILURE([[[$0]] unknown intel compiler version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    [(__INTEL_COMPILER%100)/10],,
    AC_MSG_FAILURE([[[$0]] unknown intel compiler version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [(__INTEL_COMPILER%10)],,
    AC_MSG_FAILURE([[[$0]] unknown intel compiler version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# for IBM
AC_DEFUN([_AX_COMPILER_VERSION_IBM],
  [ dnl
  dnl check between z/OS C/C++  and XL C/C++
  AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([],
      [
        #if defined(__COMPILER_VER__)
        choke me;
        #endif
      ])],
    [
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
        [__xlC__/100],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler major version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
        [__xlC__%100],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler minor version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
        [__xlC_ver__/0x100],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler patch version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_build,
        [__xlC_ver__%0x100],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler build version]))
      ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_build"
    ],
    [
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
        [__xlC__%1000],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler patch version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
        [(__xlC__/10000)%10],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler minor version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
        [(__xlC__/100000)%10],,
      	AC_MSG_FAILURE([[[$0]] unknown IBM compiler major version]))
      ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
    ])
])

# for pathscale
AC_DEFUN([_AX_COMPILER_VERSION_PATHSCALE],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    __PATHCC__,,
    AC_MSG_FAILURE([[[$0]] unknown pathscale major]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    __PATHCC_MINOR__,,
    AC_MSG_FAILURE([[[$0]] unknown pathscale minor]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [__PATHCC_PATCHLEVEL__],,
    AC_MSG_FAILURE([[[$0]] unknown pathscale patch level]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# for clang
AC_DEFUN([_AX_COMPILER_VERSION_CLANG],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    __clang_major__,,
    AC_MSG_FAILURE([[[$0]] unknown clang major]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    __clang_minor__,,
    AC_MSG_FAILURE([[[$0]] unknown clang minor]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [__clang_patchlevel__],,0)
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# for crayc
AC_DEFUN([_AX_COMPILER_VERSION_CRAY],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    _RELEASE,,
    AC_MSG_FAILURE([[[$0]] unknown crayc release]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    _RELEASE_MINOR,,
    AC_MSG_FAILURE([[[$0]] unknown crayc minor]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor"
  ])

# for fujitsu
AC_DEFUN([_AX_COMPILER_VERSION_FUJITSU],[
  AC_COMPUTE_INT(ax_cv_[]_AC_LANG_ABBREV[]_compiler_version,
                 __FCC_VERSION,,
		 AC_MSG_FAILURE([[[$0]]unknown fujitsu release]))
  ])

# for GNU
AC_DEFUN([_AX_COMPILER_VERSION_GNU],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    __GNUC__,,
    AC_MSG_FAILURE([[[$0]] unknown gcc major]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    __GNUC_MINOR__,,
    AC_MSG_FAILURE([[[$0]] unknown gcc minor]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [__GNUC_PATCHLEVEL__],,
    AC_MSG_FAILURE([[[$0]] unknown gcc patch level]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# For sun
AC_DEFUN([_AX_COMPILER_VERSION_SUN],[
  m4_define([_AX_COMPILER_VERSION_SUN_NUMBER],
            [
	     #if defined(__SUNPRO_CC)
	     __SUNPRO_CC
	     #else
	     __SUNPRO_C
	     #endif
	    ])
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_until59,
    !!(_AX_COMPILER_VERSION_SUN_NUMBER < 0x1000),,
    AC_MSG_FAILURE([[[$0]] unknown sun release version]))
  AS_IF([test "X$_ax_[]_AC_LANG_ABBREV[]_compiler_version_until59" = X1],
    [dnl
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
        _AX_COMPILER_VERSION_SUN_NUMBER % 0x10,,
	AC_MSG_FAILURE([[[$0]] unknown sun patch version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
        (_AX_COMPILER_VERSION_SUN_NUMBER / 0x10) % 0x10,,
        AC_MSG_FAILURE([[[$0]] unknown sun minor version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
        (_AX_COMPILER_VERSION_SUN_NUMBER / 0x100),,
        AC_MSG_FAILURE([[[$0]] unknown sun major version]))
    ],
    [dnl
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
        _AX_COMPILER_VERSION_SUN_NUMBER % 0x10,,
        AC_MSG_FAILURE([[[$0]] unknown sun patch version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
        (_AX_COMPILER_VERSION_SUN_NUMBER / 0x100) % 0x100,,
        AC_MSG_FAILURE([[[$0]] unknown sun minor version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
        (_AX_COMPILER_VERSION_SUN_NUMBER / 0x1000),,
        AC_MSG_FAILURE([[[$0]] unknown sun major version]))
    ])
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
])

AC_DEFUN([_AX_COMPILER_VERSION_HP],[
  m4_define([_AX_COMPILER_VERSION_HP_NUMBER],
            [
	     #if defined(__HP_cc)
	     __HP_cc
	     #else
	     __HP_aCC
	     #endif
	    ])
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_untilA0121,
    !!(_AX_COMPILER_VERSION_HP_NUMBER <= 1),,
    AC_MSG_FAILURE([[[$0]] unknown hp release version]))
  AS_IF([test "X$_ax_[]_AC_LANG_ABBREV[]_compiler_version_untilA0121" = X1],
    [dnl By default output last version with this behavior.
     dnl it is so old
      ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="01.21.00"
    ],
    [dnl
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
        (_AX_COMPILER_VERSION_HP_NUMBER % 100),,
        AC_MSG_FAILURE([[[$0]] unknown hp release version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
        ((_AX_COMPILER_VERSION_HP_NUMBER / 100)%100),,
        AC_MSG_FAILURE([[[$0]] unknown hp minor version]))
      AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
        ((_AX_COMPILER_VERSION_HP_NUMBER / 10000)%100),,
        AC_MSG_FAILURE([[[$0]] unknown hp major version]))
      ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
    ])
])

AC_DEFUN([_AX_COMPILER_VERSION_DEC],[dnl
  m4_define([_AX_COMPILER_VERSION_DEC_NUMBER],
            [
	     #if defined(__DECC_VER)
	     __DECC_VER
	     #else
	     __DECCXX_VER
	     #endif
	    ])
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    (_AX_COMPILER_VERSION_DEC_NUMBER % 10000),,
    AC_MSG_FAILURE([[[$0]] unknown dec release version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    ((_AX_COMPILER_VERSION_DEC_NUMBER / 100000UL)%100),,
    AC_MSG_FAILURE([[[$0]] unknown dec minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    ((_AX_COMPILER_VERSION_DEC_NUMBER / 10000000UL)%100),,
    AC_MSG_FAILURE([[[$0]] unknown dec major version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# borland
AC_DEFUN([_AX_COMPILER_VERSION_BORLAND],[dnl
  m4_define([_AX_COMPILER_VERSION_TURBOC_NUMBER],
            [
	     #if defined(__TURBOC__)
	     __TURBOC__
	     #else
	     choke me
	     #endif
	    ])
  m4_define([_AX_COMPILER_VERSION_BORLANDC_NUMBER],
            [
	     #if defined(__BORLANDC__)
	     __BORLANDC__
	     #else
	     __CODEGEARC__
	     #endif
	    ])
 AC_COMPILE_IFELSE(
   [AC_LANG_PROGRAM(,
     _AX_COMPILER_VERSION_TURBOC_NUMBER)],
   [dnl TURBOC
     AC_COMPUTE_INT(
       _ax_[]_AC_LANG_ABBREV[]_compiler_version_turboc_raw,
       _AX_COMPILER_VERSION_TURBOC_NUMBER,,
       AC_MSG_FAILURE([[[$0]] unknown turboc version]))
     AS_IF(
       [test $_ax_[]_AC_LANG_ABBREV[]_compiler_version_turboc_raw -lt 661 || test $_ax_[]_AC_LANG_ABBREV[]_compiler_version_turboc_raw -gt 1023],
       [dnl compute normal version
        AC_COMPUTE_INT(
	  _ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
	  _AX_COMPILER_VERSION_TURBOC_NUMBER % 0x100,,
	  AC_MSG_FAILURE([[[$0]] unknown turboc minor version]))
	AC_COMPUTE_INT(
	  _ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
	  (_AX_COMPILER_VERSION_TURBOC_NUMBER/0x100)%0x100,,
	  AC_MSG_FAILURE([[[$0]] unknown turboc major version]))
	ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="0turboc:$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor"],
      [dnl special version
       AS_CASE([$_ax_[]_AC_LANG_ABBREV[]_compiler_version_turboc_raw],
         [661],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="0turboc:1.00"],
	 [662],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="0turboc:1.01"],
         [663],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="0turboc:2.00"],
	 [
	 AC_MSG_WARN([[[$0]] unknown turboc version between 0x295 and 0x400 please report bug])
	 ax_cv_[]_AC_LANG_ABBREV[]_compiler_version=""
	 ])
      ])
    ],
    # borlandc
    [
    AC_COMPUTE_INT(
      _ax_[]_AC_LANG_ABBREV[]_compiler_version_borlandc_raw,
      _AX_COMPILER_VERSION_BORLANDC_NUMBER,,
      AC_MSG_FAILURE([[[$0]] unknown borlandc version]))
    AS_CASE([$_ax_[]_AC_LANG_ABBREV[]_compiler_version_borlandc_raw],
      dnl BORLANDC++ before 5.5
      [512] ,[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:2.00"],
      [1024],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:3.00"],
      [1024],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:3.00"],
      [1040],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:3.1"],
      [1106],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:4.0"],
      [1280],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:5.0"],
      [1312],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="1borlanc:5.02"],
      dnl C++ Builder era
      [1328],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="2cppbuilder:3.0"],
      [1344],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="2cppbuilder:4.0"],
      dnl BORLANDC++ after 5.5
      [1360],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="3borlancpp:5.5"],
      [1361],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="3borlancpp:5.51"],
      [1378],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="3borlancpp:5.6.4"],
      dnl C++ Builder with year number
      [1392],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="4cppbuilder:2006"],
      [1424],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="4cppbuilder:2007"],
      [1555],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="4cppbuilder:2009"],
      [1569],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="4cppbuilder:2010"],
      dnl XE version
      [1584],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="5xe"],
      [1600],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="5xe:2"],
      [1616],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="5xe:3"],
      [1632],[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="5xe:4"],
      [
      AC_MSG_WARN([[[$0]] Unknown borlandc compiler version $_ax_[]_AC_LANG_ABBREV[]_compiler_version_borlandc_raw please report bug])
      ])
    ])
  ])

# COMO
AC_DEFUN([_AX_COMPILER_VERSION_COMEAU],
  [ dnl
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    [__COMO_VERSION__%100],,
    AC_MSG_FAILURE([[[$0]] unknown comeau compiler minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    [(__COMO_VERSION__/100)%10],,
    AC_MSG_FAILURE([[[$0]] unknown comeau compiler major version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor"
  ])

# KAI
AC_DEFUN([_AX_COMPILER_VERSION_KAI],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [__KCC_VERSION%100],,
    AC_MSG_FAILURE([[[$0]] unknown kay compiler patch version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    [(__KCC_VERSION/100)%10],,
    AC_MSG_FAILURE([[[$0]] unknown kay compiler minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    [(__KCC_VERSION/1000)%10],,
    AC_MSG_FAILURE([[[$0]] unknown kay compiler major version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

dnl LCC
dnl LCC does not output version...

# SGI
AC_DEFUN([_AX_COMPILER_VERSION_SGI],[
   m4_define([_AX_COMPILER_VERSION_SGI_NUMBER],
            [
	     #if defined(_COMPILER_VERSION)
	     _COMPILER_VERSION
	     #else
	     _SGI_COMPILER_VERSION
	     #endif
	    ])
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [_AX_COMPILER_VERSION_SGI_NUMBER%10],,
    AC_MSG_FAILURE([[[$0]] unknown SGI compiler patch version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    [(_AX_COMPILER_VERSION_SGI_NUMBER/10)%10],,
    AC_MSG_FAILURE([[[$0]] unknown SGI compiler minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    [(_AX_COMPILER_VERSION_SGI_NUMBER/100)%10],,
    AC_MSG_FAILURE([[[$0]] unknown SGI compiler major version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# microsoft
AC_DEFUN([_AX_COMPILER_VERSION_MICROSOFT],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    _MSC_VER%100,,
    AC_MSG_FAILURE([[[$0]] unknown microsoft compiler minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    (_MSC_VER/100)%100,,
    AC_MSG_FAILURE([[[$0]] unknown microsoft compiler major version]))
  dnl could be overridden
  _ax_[]_AC_LANG_ABBREV[]_compiler_version_patch=0
  _ax_[]_AC_LANG_ABBREV[]_compiler_version_build=0
  # special case for version 6
  AS_IF([test "X$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major" = "X12"],
    [AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
       _MSC_FULL_VER%1000,,
       _ax_[]_AC_LANG_ABBREV[]_compiler_version_patch=0)])
  # for version 7
  AS_IF([test "X$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major" = "X13"],
    [AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
       _MSC_FULL_VER%1000,,
       AC_MSG_FAILURE([[[$0]] unknown microsoft compiler patch version]))
    ])
  # for version > 8
 AS_IF([test $_ax_[]_AC_LANG_ABBREV[]_compiler_version_major -ge 14],
    [AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
       _MSC_FULL_VER%10000,,
       AC_MSG_FAILURE([[[$0]] unknown microsoft compiler patch version]))
    ])
 AS_IF([test $_ax_[]_AC_LANG_ABBREV[]_compiler_version_major -ge 15],
    [AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_build,
       _MSC_BUILD,,
       AC_MSG_FAILURE([[[$0]] unknown microsoft compiler build version]))
    ])
 ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_build"
 ])

# for metrowerks
AC_DEFUN([_AX_COMPILER_VERSION_METROWERKS],[dnl
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    __MWERKS__%0x100,,
    AC_MSG_FAILURE([[[$0]] unknown metrowerks compiler patch version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    (__MWERKS__/0x100)%0x10,,
    AC_MSG_FAILURE([[[$0]] unknown metrowerks compiler minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    (__MWERKS__/0x1000)%0x10,,
    AC_MSG_FAILURE([[[$0]] unknown metrowerks compiler major version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# for watcom
AC_DEFUN([_AX_COMPILER_VERSION_WATCOM],[dnl
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    __WATCOMC__%100,,
    AC_MSG_FAILURE([[[$0]] unknown watcom compiler minor version]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    (__WATCOMC__/100)%100,,
    AC_MSG_FAILURE([[[$0]] unknown watcom compiler major version]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor"
  ])

# for PGI
AC_DEFUN([_AX_COMPILER_VERSION_PORTLAND],[
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_major,
    __PGIC__,,
    AC_MSG_FAILURE([[[$0]] unknown pgi major]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor,
    __PGIC_MINOR__,,
    AC_MSG_FAILURE([[[$0]] unknown pgi minor]))
  AC_COMPUTE_INT(_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch,
    [__PGIC_PATCHLEVEL__],,
    AC_MSG_FAILURE([[[$0]] unknown pgi patch level]))
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version="$_ax_[]_AC_LANG_ABBREV[]_compiler_version_major.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_minor.$_ax_[]_AC_LANG_ABBREV[]_compiler_version_patch"
  ])

# tcc
AC_DEFUN([_AX_COMPILER_VERSION_TCC],[
  ax_cv_[]_AC_LANG_ABBREV[]_compiler_version=[`tcc -v | $SED 's/^[ ]*tcc[ ]\+version[ ]\+\([0-9.]\+\).*/\1/g'`]
  ])
# main entry point
AC_DEFUN([AX_COMPILER_VERSION],[dnl
  AC_REQUIRE([AX_COMPILER_VENDOR])
  AC_REQUIRE([AC_PROG_SED])
  AC_CACHE_CHECK([for _AC_LANG compiler version],
    ax_cv_[]_AC_LANG_ABBREV[]_compiler_version,
    [ dnl
      AS_CASE([$ax_cv_[]_AC_LANG_ABBREV[]_compiler_vendor],
        [intel],[_AX_COMPILER_VERSION_INTEL],
	[ibm],[_AX_COMPILER_VERSION_IBM],
	[pathscale],[_AX_COMPILER_VERSION_PATHSCALE],
	[clang],[_AX_COMPILER_VERSION_CLANG],
	[cray],[_AX_COMPILER_VERSION_CRAY],
	[fujitsu],[_AX_COMPILER_VERSION_FUJITSU],
        [gnu],[_AX_COMPILER_VERSION_GNU],
	[sun],[_AX_COMPILER_VERSION_SUN],
	[hp],[_AX_COMPILER_VERSION_HP],
	[dec],[_AX_COMPILER_VERSION_DEC],
	[borland],[_AX_COMPILER_VERSION_BORLAND],
	[comeau],[_AX_COMPILER_VERSION_COMEAU],
	[kai],[_AX_COMPILER_VERSION_KAI],
	[sgi],[_AX_COMPILER_VERSION_SGI],
	[microsoft],[_AX_COMPILER_VERSION_MICROSOFT],
	[metrowerks],[_AX_COMPILER_VERSION_METROWERKS],
	[watcom],[_AX_COMPILER_VERSION_WATCOM],
	[portland],[_AX_COMPILER_VERSION_PORTLAND],
	[tcc],[_AX_COMPILER_VERSION_TCC],
  	[ax_cv_[]_AC_LANG_ABBREV[]_compiler_version=""])
    ])
])
