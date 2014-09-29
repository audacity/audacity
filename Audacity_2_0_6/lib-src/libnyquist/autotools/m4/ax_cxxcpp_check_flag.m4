##### http://autoconf-archive.cryp.to/ax_cxxcpp_check_flag.html
#
# SYNOPSIS
#
#   AX_CXXCPP_CHECK_FLAG(FLAG-TO-CHECK,[PROLOGUE],[BODY],[ACTION-IF-SUCCESS],[ACTION-IF-FAILURE])
#
# DESCRIPTION
#
#   This macro tests if the C++ preprocessor supports the flag
#   FLAG-TO-CHECK. If successfull execute ACTION-IF-SUCCESS otherwise
#   ACTION-IF-FAILURE. PROLOGUE and BODY are optional and should be
#   used as in AC_LANG_PROGRAM macro.
#
#   This code is inspired from KDE_CHECK_COMPILER_FLAG macro. Thanks to
#   Bogdan Drozdowski <bogdandr@op.pl> for testing and bug fixes.
#
# LAST MODIFICATION
#
#   2007-11-26
#
# COPYLEFT
#
#   Copyright (c) 2007 Francesco Salvestrini <salvestrini@users.sourceforge.net>
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#   02111-1307, USA.
#
#   As a special exception, the respective Autoconf Macro's copyright
#   owner gives unlimited permission to copy, distribute and modify the
#   configure scripts that are the output of Autoconf when processing
#   the Macro. You need not follow the terms of the GNU General Public
#   License when using or distributing such scripts, even though
#   portions of the text of the Macro appear in them. The GNU General
#   Public License (GPL) does govern all other use of the material that
#   constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the
#   Autoconf Macro released by the Autoconf Macro Archive. When you
#   make and distribute a modified version of the Autoconf Macro, you
#   may extend this special exception to the GPL to apply to your
#   modified version as well.

AC_DEFUN([AX_CXXCPP_CHECK_FLAG],[
  AC_PREREQ([2.61])
  AC_REQUIRE([AC_PROG_CXXCPP])
  AC_REQUIRE([AC_PROG_SED])

  flag=`echo "$1" | $SED 'y% .=/+-(){}<>:*,%_______________%'`

  AC_CACHE_CHECK([whether the C++ preprocessor accepts the $1 flag],
    [ax_cv_cxxcpp_check_flag_$flag],[

    AC_LANG_PUSH([C++])

    save_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS $1"
    AC_PREPROC_IFELSE([
      AC_LANG_PROGRAM([$2],[$3])
    ],[
      eval "ax_cv_cxxcpp_check_flag_$flag=yes"
    ],[
      eval "ax_cv_cxxcpp_check_flag_$flag=no"
    ])

    CXXFLAGS="$save_CXXFLAGS"

    AC_LANG_POP

  ])

  AS_IF([eval "test \"`echo '$ax_cv_cxxcpp_check_flag_'$flag`\" = yes"],[
    :
    $4
  ],[
    :
    $5
  ])
])
