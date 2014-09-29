
AC_INIT(vamp-plugin-sdk, 2.5, cannam@all-day-breakfast.com)

AC_CONFIG_SRCDIR(vamp/vamp.h)
AC_PROG_CXX
AC_HEADER_STDC
AC_C_BIGENDIAN

if pkg-config --modversion vamp-sdk >/dev/null 2>&1; then
  echo "WARNING: A version of the Vamp plugin SDK is already installed."
  echo "         Expect worries and sorrows if you install a new version"
  echo "         without removing the old one first.  (Continuing)"
fi

AC_SEARCH_LIBS([dlopen],[dl])

dnl See if the user wants to build programs, or just the SDK
AC_ARG_ENABLE(programs,	[AS_HELP_STRING([--enable-programs],
	[enable building of example host and RDF generator [default=yes]])],
	PROGS_ARGUMENT=$enableval,
	PROGS_ARGUMENT="yes")

dnl if the user wants progs, then we need to check for libsndfile
if test "x$PROGS_ARGUMENT" = "xyes" ; then 
	PKG_CHECK_MODULES([SNDFILE],[sndfile],have_sndfile="yes",have_sndfile="no")
	if test "x$have_sndfile" = "xyes" ; then
		dnl all good, build everything
		TARGETS="sdk plugins host rdfgen test"
	else
		dnl no libsndfile - can't build host!
		AC_MSG_ERROR([libsndfile not found - cannot build example Vamp host!
If you don't want to build the host, configure with --disable-programs.])
	fi
else
	dnl user didn't want programs - so we won't build them
	TARGETS="sdk plugins"
fi

AC_SUBST(SNDFILE_CFLAGS)
AC_SUBST(SNDFILE_LIBS)
AC_SUBST(TARGETS)

changequote(,)dnl
if test "x$GCC" = "xyes"; then
  case " $CXXFLAGS " in
    *[\ \	]-fPIC\ -Wall[\ \	]*) ;;
    *) CXXFLAGS="$CXXFLAGS -fPIC -Wall" ;;
  esac
fi
changequote([,])dnl

AC_SUBST(CXXFLAGS)
AC_OUTPUT([Makefile])

