#!/bin/sh
# Run this to set up the build system: configure, makefiles, etc.
# (based on the version in enlightenment's cvs)

package="libsndfile"

olddir=`pwd`
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

cd "$srcdir"
DIE=0

printf "checking for autogen ... "
result="yes"
(autogen --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have GNU autogen installed to compile $package."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
		result="no"
        DIE=1
}
echo $result

printf "checking for autoconf ... "
result="yes"
(autoconf --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have autoconf installed to compile $package."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
		result="no"
        DIE=1
}
echo $result

VERSIONGREP="sed -e s/.*[^0-9\.]\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/"
VERSIONMKMAJ="sed -e s/\([0-9][0-9]*\)[^0-9].*/\\1/"
VERSIONMKMIN="sed -e s/.*[0-9][0-9]*\.//"

# do we need automake?
if test -r Makefile.am; then
  AM_OPTIONS=`fgrep AUTOMAKE_OPTIONS Makefile.am`
  AM_NEEDED=`echo $AM_OPTIONS | $VERSIONGREP`
  if test x"$AM_NEEDED" = "x$AM_OPTIONS"; then
    AM_NEEDED=""
  fi
  if test -z $AM_NEEDED; then
    printf "checking for automake ... "
    AUTOMAKE=automake
    ACLOCAL=aclocal
    if ($AUTOMAKE --version < /dev/null > /dev/null 2>&1); then
      echo "yes"
    else
      echo "no"
      AUTOMAKE=
    fi
  else
    printf "checking for automake $AM_NEEDED or later ... "
    majneeded=`echo $AM_NEEDED | $VERSIONMKMAJ`
    minneeded=`echo $AM_NEEDED | $VERSIONMKMIN`
    for am in automake-$AM_NEEDED automake$AM_NEEDED \
	automake automake-1.7 automake-1.8 automake-1.9 automake-1.10; do
      ($am --version < /dev/null > /dev/null 2>&1) || continue
      ver=`$am --version < /dev/null | head -n 1 | $VERSIONGREP`
      maj=`echo $ver | $VERSIONMKMAJ`
      min=`echo $ver | $VERSIONMKMIN`
      if test $maj -eq $majneeded -a $min -ge $minneeded; then
        AUTOMAKE=$am
        echo $AUTOMAKE
        break
      fi
    done
    test -z $AUTOMAKE &&  echo "no"
    printf "checking for aclocal $AM_NEEDED or later ... "
    for ac in aclocal-$AM_NEEDED aclocal$AM_NEEDED \
	aclocal aclocal-1.7 aclocal-1.8 aclocal-1.9 aclocal-1.10; do
      ($ac --version < /dev/null > /dev/null 2>&1) || continue
      ver=`$ac --version < /dev/null | head -n 1 | $VERSIONGREP`
      maj=`echo $ver | $VERSIONMKMAJ`
      min=`echo $ver | $VERSIONMKMIN`
      if test $maj -eq $majneeded -a $min -ge $minneeded; then
        ACLOCAL=$ac
        echo $ACLOCAL
        break
      fi
    done
    test -z $ACLOCAL && echo "no"
  fi
  test -z $AUTOMAKE || test -z $ACLOCAL && {
        echo
        echo "You must have automake installed to compile $package."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        exit 1
  }
fi

printf "checking for libtool ... "
for LIBTOOLIZE in libtoolize glibtoolize nope; do
  ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 && break
done
if test x$LIBTOOLIZE = xnope; then
  echo "nope."
  LIBTOOLIZE=libtoolize
else
  echo $LIBTOOLIZE
fi
($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
	echo
	echo "You must have libtool installed to compile $package."
	echo "Download the appropriate package for your system,"
	echo "or get the source from one of the GNU ftp sites"
	echo "listed in http://www.gnu.org/order/ftp.html"
	DIE=1
}

printf "checking for pkg-config ... "
result="yes"
(pkg-config --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have pkg-config installed to compile $package."
        echo "Download the appropriate package for your distribution."
		result="no"
        DIE=1
}
echo $result


printf "checking for python ... "
result="yes"
(python --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have Python installed to compile $package."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at http://python.org/"
		result="no"
        DIE=1
}
echo $result

if test "$DIE" -eq 1; then
        exit 1
fi

echo "Generating configuration files for $package, please wait ... "

echo "  $ACLOCAL $ACLOCAL_FLAGS"
$ACLOCAL $ACLOCAL_FLAGS || exit 1
echo "  $LIBTOOLIZE --automake --force"
$LIBTOOLIZE --automake --force || exit 1
echo "  autoheader"
autoheader || exit 1
echo "  $AUTOMAKE --add-missing $AUTOMAKE_FLAGS"
$AUTOMAKE --add-missing $AUTOMAKE_FLAGS || exit 1
echo "  autoconf"
autoconf || exit 1

# Generate the src/cmake-config.h.in from src/config.h.in.
# CMake process src/cmake-config.h to create src/config.h.
rm -f src/config.h src/cmake-config.h

cd $olddir

if test -d .git/ ; then
	fprecommit=.git/hooks/pre-commit
	if test ! -f $fprecommit ; then
		echo
		echo "Installing git pre-commit hook for this project."
		printf "#/bin/sh\nexec Scripts/git-pre-commit-hook\n" > $fprecommit
		chmod u+x $fprecommit
		echo
		fi
	fi

