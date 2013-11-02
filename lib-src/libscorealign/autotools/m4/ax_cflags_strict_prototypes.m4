# ===============================================================================
#  http://www.gnu.org/software/autoconf-archive/ax_cflags_strict_prototypes.html
# ===============================================================================
#
# SYNOPSIS
#
#   AX_CFLAGS_STRICT_PROTOTYPES [(shellvar [,default, [A/NA]]
#
# DESCRIPTION
#
#   Try to find a compiler option that requires strict prototypes.
#
#   The sanity check is done by looking at sys/signal.h which has a set of
#   macro-definitions SIG_DFL and SIG_IGN that are cast to the local
#   signal-handler type. If that signal-handler type is not fully qualified
#   then the system headers are not seen as strictly prototype clean.
#
#   For the GNU CC compiler it will be -fstrict-prototypes
#   -Wstrict-prototypes The result is added to the shellvar being CFLAGS by
#   default.
#
#   DEFAULTS:
#
#    - $1 shell-variable-to-add-to : CFLAGS
#    - $2 add-value-if-not-found : nothing
#    - $3 action-if-found : add value to shellvariable
#    - $4 action-if-not-found : nothing
#
#   NOTE: These macros depend on AX_APPEND_FLAG.
#
# LICENSE
#
#   Copyright (c) 2008 Guido U. Draheim <guidod@gmx.de>
#
#   This program is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation; either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

#serial 12

AC_DEFUN([AX_FLAGS_STRICT_PROTOTYPES],[dnl
AS_VAR_PUSHDEF([FLAGS],[_AC_LANG_PREFIX[]FLAGS])dnl
AS_VAR_PUSHDEF([VAR],[ac_cv_[]_AC_LANG_ABBREV[]flags_strict_prototypes])dnl
AC_CACHE_CHECK([m4_ifval($1,$1,FLAGS) for strict prototypes],
VAR,[VAR="no, unknown"
ac_save_[]FLAGS="$[]FLAGS"
for ac_arg dnl
in "-pedantic -Werror % -fstrict-prototypes -Wstrict-prototypes" dnl   GCC
   "-pedantic -Werror % -Wstrict-prototypes" dnl try to warn atleast
   "-pedantic -Werror % -Wmissing-prototypes" dnl try to warn atleast
   "-pedantic -Werror % -Werror-implicit-function-declaration" dnl
   "-pedantic -Werror % -Wimplicit-function-declaration" dnl
   "-pedantic % -Wstrict-prototypes %% no, unsupported" dnl oops
   #
do FLAGS="$ac_save_[]FLAGS "`echo $ac_arg | sed -e 's,%%.*,,' -e 's,%,,'`
   AC_TRY_COMPILE([],[return 0;],
   [VAR=`echo $ac_arg | sed -e 's,.*% *,,'` ; break])
done
case ".$VAR" in
   .|.no|.no,*) ;;
   *) # sanity check with signal() from sys/signal.h
    cp config.log config.tmp
    AC_TRY_COMPILE([#include <signal.h>],[
    if (signal (SIGINT, SIG_IGN) == SIG_DFL) return 1;
    if (signal (SIGINT, SIG_IGN) != SIG_DFL) return 2;],
    dnl the original did use test -n `$CC testprogram.c`
    [if test `diff config.log config.tmp | grep -i warning | wc -l` != 0
then if test `diff config.log config.tmp | grep -i warning | wc -l` != 1
then VAR="no, suppressed, signal.h," ; fi ; fi],
    [VAR="no, suppressed, signal.h"])
    rm config.tmp
  ;;
esac
FLAGS="$ac_save_[]FLAGS"
])
AS_VAR_POPDEF([FLAGS])dnl
AC_REQUIRE([AX_APPEND_FLAG])
case ".$VAR" in
     .ok|.ok,*) m4_ifvaln($3,$3) ;;
   .|.no|.no,*) m4_default($4,[m4_ifval($2,[AX_APPEND_FLAG([$2], [$1])])]) ;;
   *) m4_default($3,[AX_APPEND_FLAG([$VAR], [$1])]) ;;
esac
AS_VAR_POPDEF([VAR])dnl
])dnl AX_FLAGS_STRICT_PROTOTYPES

AC_DEFUN([AX_CFLAGS_STRICT_PROTOTYPES],[dnl
AC_LANG_PUSH([C])
AX_FLAGS_STRICT_PROTOTYPES([$1], [$2], [$3], [$4])
AC_LANG_POP([C])
])

AC_DEFUN([AX_CXXFLAGS_STRICT_PROTOTYPES],[dnl
AC_LANG_PUSH([C++])
AX_FLAGS_STRICT_PROTOTYPES([$1], [$2], [$3], [$4])
AC_LANG_POP([C++])
])
