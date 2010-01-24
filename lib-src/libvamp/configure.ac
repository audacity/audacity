
AC_INIT(vamp-plugin-sdk, 2.0, cannam@all-day-breakfast.com)

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

TARGETS="plugins"
dnl to start with, example plug-ins. Next bit will build a list
dnl of other things like static and dynamic SDKs

dnl See if the user wants to build static SDK
AC_ARG_ENABLE(static,	[AS_HELP_STRING([--enable-static],
	[enable building of static linked vamp SDK [default=no]])],
	SSDK_ARGUMENT=$enableval,
	SSDK_ARGUMENT="no")

if test "x$SSDK_ARGUMENT" = "xyes" ; then
	TARGETS="$TARGETS sdkstatic"
fi

dnl See if the user wants to build shared SDK
AC_ARG_ENABLE(shared,	[AS_HELP_STRING([--enable-shared],
	[enable building of dynamic linked vamp SDK [default=yes]])],
	DSDK_ARGUMENT=$enableval,
	DSDK_ARGUMENT="yes")

if test "x$DSDK_ARGUMENT" = "xyes" ; then
	TARGETS="$TARGETS sdk"
fi

dnl See if the user wants to build static SDK
AC_ARG_ENABLE(static-sdk,	[AS_HELP_STRING([--enable-static-sdk],
	[enable building of static linked vamp SDK [default=no]])],
	SSDK_ARGUMENT=$enableval,
	SSDK_ARGUMENT="no")

if test "x$SSDK_ARGUMENT" = "xyes" ; then
	TARGETS="$TARGETS sdkstatic"
fi


dnl See if the user wants to build programs
AC_ARG_ENABLE(programs,	[AS_HELP_STRING([--enable-programs],
	[enable building of example host and RDF generator [default=yes]])],
	PROGS_ARGUMENT=$enableval,
	PROGS_ARGUMENT="yes")



dnl if the user wants progs, then we need to check for libsndfile
if test "x$PROGS_ARGUMENT" = "xyes" ; then 
	PKG_CHECK_MODULES([SNDFILE],[sndfile], 
					  have_sndfile="yes", have_sndfile="no")
	if test "x$have_sndfile" = "xyes" ; then
			dnl all good, build everything (test depends on host)
		TARGETS="$TARGETS host rdfgen test"
	else
		dnl no libsndfile - can't build host!
		AC_MSG_ERROR([libsndfile not found - cannot build example Vamp host!])
	fi
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

AC_OUTPUT([Makefile])

