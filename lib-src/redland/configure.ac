dnl -*- Mode: autoconf -*-
dnl
dnl configure.in - top level autoconf file for Redland
dnl (Process this file with autoconf to produce a configure script.)
dnl
dnl Copyright (C) 2000-2007 David Beckett http://purl.org/net/dajobe/
dnl Copyright (C) 2000-2005 University of Bristol, UK http://www.bristol.ac.uk/
dnl 
dnl This package is Free Software and part of Redland http://librdf.org/
dnl 
dnl It is licensed under the following three licenses as alternatives:
dnl   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
dnl   2. GNU General Public License (GPL) V2 or any newer version
dnl   3. Apache License, V2.0 or any newer version
dnl 
dnl You may not use this file except in compliance with at least one of
dnl the above three licenses.
dnl 
dnl See LICENSE.html or LICENSE.txt at the top of this package for the
dnl complete terms and further detail along with the license texts for
dnl the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
dnl 
dnl 


AC_INIT([Redland RDF Application Framework], 1.0.8, http://bugs.librdf.org/, redland)
AC_PREREQ(2.53)
AC_CONFIG_SRCDIR(librdf/rdf_node.c)
AC_REVISION($Revision: 1.2 $)dnl

AM_INIT_AUTOMAKE([1.7 check-news std-options -Wobsolete -Wportability -Wsyntax -Wunsupported])
AM_CONFIG_HEADER(librdf/rdf_config.h)
AM_MAINTAINER_MODE

AC_CANONICAL_HOST


changequote(<<, >>)dnl
version_major=`echo $VERSION | sed -e 's/^\([^\.]*\)\.\([^\.]*\)\.\(.*\)$/\1/'`
version_minor=`echo $VERSION | sed -e 's/^\([^\.]*\)\.\([^\.]*\)\.\(.*\)$/\2/'`
version_release=`echo $VERSION | sed -e 's/^\([^\.]*\)\.\([^\.]*\)\.\(.*\)$/\3/'`
changequote([, ])dnl

version_decimal=`expr $version_major \* 10000 + $version_minor \* 100 + $version_release`

AC_DEFINE_UNQUOTED(LIBRDF_VERSION_MAJOR, $version_major, [Major version number])
AC_DEFINE_UNQUOTED(LIBRDF_VERSION_MINOR, $version_minor, [Minor version number])
AC_DEFINE_UNQUOTED(LIBRDF_VERSION_RELEASE, $version_release, [Release version number])
AC_DEFINE_UNQUOTED(LIBRDF_VERSION_DECIMAL, $version_decimal, [Release version as a decimal])

# for redland-config.in
LIBRDF_VERSION_DECIMAL=$version_decimal
AC_SUBST(LIBRDF_VERSION_DECIMAL)


# Libtool versioning
#
# CURRENT
#      The most recent interface number that this library implements.
# 
# REVISION
#      The implementation number of the CURRENT interface.
# 
# AGE
#      The difference between the newest and oldest interfaces that this
#      library implements.  In other words, the library implements all the
#      interface numbers in the range from number `CURRENT - AGE' to
#      `CURRENT'.
#
# Rules:
#  1. Start with version information of `0:0:0' for each libtool library.
#
#  2. Update the version information only immediately before a public
#     release of your software.  More frequent updates are unnecessary,
#     and only guarantee that the current interface number gets larger
#     faster.
#
#  3. If the library source code has changed at all since the last
#     update, then increment REVISION (`C:R:A' becomes `C:r+1:A').
#
#  4. If any interfaces have been added, removed, or changed since the
#     last update, increment CURRENT, and set REVISION to 0.
#
#  5. If any interfaces have been added since the last public release,
#     then increment AGE.
#
#  6. If any interfaces have been removed since the last public release,
#     then set AGE to 0.
#
# syntax: CURRENT[:REVISION[:AGE]]
LIBRDF_LIBTOOL_VERSION=0:0:0
AC_SUBST(LIBRDF_LIBTOOL_VERSION)

dnl Checks for programs.
AM_SANITY_CHECK
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])
AC_LIBTOOL_DLOPEN
AC_LIBLTDL_INSTALLABLE
AC_PROG_LIBTOOL

# Find a tar command for 'make dist'
AC_CHECK_PROGS(TAR, gnutar gtar tar)

AC_CHECK_PROGS(PERL, perl)
# Gnome
AC_CHECK_PROGS(PKG_CONFIG, pkg-config)


AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)


release_version=no
AC_ARG_ENABLE(release, [  --enable-release        Turn on optimizations (for maintainer).  ], \
 if test "$enableval" = "yes"; then \
	  release_version=yes
fi;)	

modular=no
AC_ARG_ENABLE(modular, [  --enable-modular        Modularize storage backends.  ], \
 if test "$enableval" = "yes"; then \
	  modular=yes
fi;)	

dnl compiler checks

AC_DEFUN([REDLAND_CC_TRY_FLAG], [
  AC_MSG_CHECKING([whether $CC supports $1])
  redland_save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS $1"
  AC_COMPILE_IFELSE([ ], [redland_cc_flag=yes], [redland_cc_flag=no])
  CFLAGS="$redland_save_CFLAGS"
  if test "X$redland_cc_flag" = "Xyes"; then
    ifelse([$2], , :, [$2])
  else
    ifelse([$3], , :, [$3])
  fi
  AC_MSG_RESULT($redland_cc_flag)
])

possible_warnings="-Wall -Wextra \
-Wshadow -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes \
-Wmissing-declarations -Wnested-externs -Wredundant-decls -Wswitch-enum \
-Wsign-compare -Werror-implicit-function-declaration -Wwrite-strings
-Wpacked -Wmissing-format-attribute -Wpointer-arith -Wcast-align -Winit-self \
-Wunsafe-loop-optimizations -Wdeclaration-after-statement \
-Wold-style-definition \
-Wno-missing-field-initializers -Wno-unused-parameter \
-Wformat-security"

warning_cflags=
if test "$USE_MAINTAINER_MODE" = yes; then
  AC_MSG_CHECKING(for supported $CC warning flags)
  AC_MSG_RESULT($warning_cflags)
  for warning in $possible_warnings; do
    REDLAND_CC_TRY_FLAG([$warning], [warning_cflags="$warning_cflags $warning"])
  done
  AC_MSG_CHECKING($CC supports warning flags)
  AC_MSG_RESULT($warning_cflags)
fi

STANDARD_CFLAGS=
MAINTAINER_CFLAGS="$warning_cflags"


# Externally linked libraries - appear in redland-config
# -Dfoo -Idir
LIBRDF_CPPFLAGS=$CPPFLAGS
# -Ldir
LIBRDF_LDFLAGS=$LDFLAGS
# -llib
LIBRDF_LIBS=$LIBS

# Internally linked libraries - never given to users
# -Dfoo -Idir
LIBRDF_INTERNAL_CPPFLAGS=
# -Ldir -llib
LIBRDF_INTERNAL_LIBS=

# Libraries linked externally also may be internal - presently just
# raptor and rasqal but in future may include others
# Currently only substituted into redland-src-config.
# -Dfoo -Idir
LIBRDF_EXTERNAL_CPPFLAGS=
# -Ldir -llib
LIBRDF_EXTERNAL_LIBS=

# do these first because somehow they get messed up by bdb checks
AC_CHECK_TYPES([byte])
AC_CHECK_TYPES([u32])
AC_CHECK_TYPES([u64])

AC_CHECK_SIZEOF(unsigned char, 1)
AC_CHECK_SIZEOF(unsigned int, 4)
AC_CHECK_SIZEOF(unsigned long, 4)
AC_CHECK_SIZEOF(unsigned long long, 8)
  
dnl Checks for libraries.


dnl Berkeley DB
AC_ARG_WITH(bdb, [  --with-bdb=DIR          Berkeley DB install area (default=/usr)], bdb_prefix="$withval", bdb_prefix="no") 
AC_ARG_WITH(bdb_lib, [  --with-bdb-lib=DIR      Berkeley DB lib directory (default=/usr/lib)], bdb_lib_dir="$withval", bdb_lib_dir="none") 
AC_ARG_WITH(bdb_include, [  --with-bdb-include=DIR  Berkeley DB include directory (default=/usr/include)], bdb_include_dir="$withval", bdb_include_dir="none") 
AC_ARG_WITH(bdb_dbname, [  --with-bdb-dbname=NAME  Berkeley DB library name (auto)], bdb_dbname="$withval", bdb_dbname="none")

bdb_version=unknown
bdb_available=Missing

if test "x$bdb_prefix" != "xno" ; then

  if test "x$bdb_prefix" = "xyes" ; then
    bdb_prefix="none"
  fi

  # Only prefix given - set lib and include
  if test "X$bdb_prefix" != "Xnone" -a "X$bdb_lib_dir" = "Xnone" -a "X$bdb_include_dir" = "Xnone" ; then
    bdb_lib_dir="$bdb_prefix/lib"
    bdb_include_dir="$bdb_prefix/include"
  fi

  # Nothing given - search
  if test "X$bdb_prefix" = "Xnone" -a "X$bdb_lib_dir" = "Xnone" -a "X$bdb_include_dir" = "Xnone" -a "X$bdb_dbname" = "Xnone" ; then
    bdb_prefix="/usr"
    bdb_lib_dir="/usr/lib"
    bdb_include_dir="/usr/include"

    extraprefix=
    # OSX fink area added if found and /sw/bin is in the PATH
    if test -d /sw/include -a -d /sw/lib ; then
      if echo $PATH |grep /sw/bin >/dev/null; then
	extraprefix=/sw
      fi
    fi

    # Sigh!  And this still might not be good enough.
    for bdbc_version in 4.6 4.5 4.4 4.3 4.2 4.1 4.0 4 3.3 3.2 3.1 3 2; do
      bdbc_major=`echo $bdbc_version | sed -e 's/^\(.\).*$/\1/'`;
      bdbc_version2=`echo $bdbc_version | sed -e 's/^\(.\).\(.\)$/\1\2/'`

      for bdbc_prefix in $extraprefix /usr/local/BerkeleyDB.$bdbc_version /usr/local /opt/local /usr; do
	bdbc_libdir=$bdbc_prefix/lib

	for bdbc_incdir in $bdbc_prefix/include/db$bdbc_version $bdbc_prefix/include/db$bdbc_version2 $bdbc_prefix/include/db$bdbc_major $bdbc_prefix/include ; do
	  if test -r $bdbc_incdir/db.h ; then
	    header_bdbc_version=`sed -ne 's/^.*DB_VERSION_STRING.*Berkeley DB \(...\).*/\1/p' $bdbc_incdir/db.h`
	    if test "X$header_bdbc_version" = "X$bdbc_version" ; then 
	      for bdbc_name in db-$bdbc_version db$bdbc_version db-$bdbc_version2 db$bdbc_version2; do
	        for bdbc_libdir2 in $bdbc_libdir $bdbc_libdir/db$bdbc_version $bdbc_libdir/db$bdbc_version2; do
		  if test -r $bdbc_libdir2/lib$bdbc_name.a -o -r $bdbc_libdir2/lib$bdbc_name.so -o -r $bdbc_libdir2/lib$bdbc_name.dylib; then
		    bdb_version=$bdbc_version
		    bdb_prefix=$bdbc_prefix
		    bdb_include_dir=$bdbc_incdir
		    bdb_lib_dir=$bdbc_libdir2
		    bdb_dbname=$bdbc_name
		    break 5
		  fi
	        done
	      done
	    fi
	  fi

	done
       done
     done

  fi

  AC_MSG_CHECKING(BDB version)
  AC_MSG_RESULT($bdb_version)
  AC_MSG_CHECKING(location of BDB libs)
  AC_MSG_RESULT($bdb_lib_dir)
  AC_MSG_CHECKING(location of BDB includes)
  AC_MSG_RESULT($bdb_include_dir)

  if test "$bdb_prefix" != no; then
    nLDFLAGS=$LDFLAGS
    nCPPFLAGS=

    if test "$bdb_lib_dir" != /usr/lib; then
      nLDFLAGS="-L$bdb_lib_dir";
    fi

    if test "$bdb_dbname" = none; then
      # Yuck, yuck, yuck - can't they create a bdb-config program like GNOME?
      bdb_dbname=`cd $bdb_lib_dir; ls -1 libdb*.a 2>/dev/null |sed -e 's/^lib//' -e 's/\.a$//' | head -1`
      if test "X$bdb_dbname" = X; then
	AC_MSG_WARN(Cannot find the BDB library name from the files in $bdb_lib_dir)
	AC_MSG_WARN(Assuming it is 'db' and will link like -ldb)
	AC_MSG_WARN(Use --with-bdb-dbname=NAME if this is wrong)
	bdb_dbname=db
      fi
    fi

    AC_MSG_CHECKING(name of BDB library)
    AC_MSG_RESULT($bdb_dbname)

    if test "$bdb_include_dir" != /usr/include; then
      nCPPFLAGS="-I$bdb_include_dir";
    fi

    LDFLAGS="$nLDFLAGS $LDFLAGS"
    CPPFLAGS="$nCPPFLAGS $CPPFLAGS"
    LIBS="-l$bdb_dbname $LIBS"

    AC_CHECK_HEADERS(db.h)
    if test "$ac_cv_header_db_h" = yes ; then
      have_libdb=no

      AC_MSG_CHECKING(for BDB V1 dbopen)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [dbopen(NULL, 0, 0, DB_BTREE, NULL);],
		  AC_DEFINE(HAVE_DBOPEN, 1, [BDB has dbopen method])
		  if test $have_libdb = no; then
		    bdb_available="Version 1.x - not supported"
		    have_libdb=no
		  fi
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB V2 db_open)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [db_open(NULL, DB_BTREE, 0, 0, NULL, NULL, NULL);],
		  AC_DEFINE(HAVE_DB_OPEN, 1, [BDB has db_open method])
		  have_libdb=yes
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB V3/V4.x db_create)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [db_create(NULL, NULL, 0);],
		  AC_DEFINE(HAVE_DB_CREATE, 1, [BDB has db_create method])
		  have_libdb=yes
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB V4.0 DB->open with 6 arguments)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB *bdb; bdb->open(bdb, NULL, NULL, DB_BTREE, 0, 0);],
		  AC_DEFINE(HAVE_BDB_OPEN_6_ARGS, 1, [BDB has open method with 6 args])
		  have_libdb=yes
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB DB->close with 2 arguments)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB *bdb; bdb->close(bdb, 0);],
		  AC_DEFINE(HAVE_BDB_CLOSE_2_ARGS, 1, [BDB has close method with 2 args])
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB DB->fd with 2 arguments)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB *bdb; bdb->fd(bdb, 0);],
		  AC_DEFINE(HAVE_BDB_FD_2_ARGS, 1, [BDB has fd method with 2 args])
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB DB->set_flags)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB *bdb; bdb->set_flags(bdb, 0);],
		  AC_DEFINE(HAVE_BDB_SET_FLAGS, 1, [BDB has set_flags method])
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(for BDB V4.1+ DB->open with 7 arguments)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB *bdb; bdb->open(bdb, NULL, NULL, NULL, DB_BTREE, 0, 0);],
		  AC_DEFINE(HAVE_BDB_OPEN_7_ARGS, 1, [BDB has open method with 7 args])
		  have_libdb=yes
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))

      AC_MSG_CHECKING(whether DB_TXN defined in db.h)
      dnl In BDB, DB_TXN is a pointer to a structure never defined
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB_TXN* ptr=(DB_TXN*)NULL],
		  AC_DEFINE(HAVE_BDB_DB_TXN, 1, [BDB defines DB_TXN])
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))
      AC_MSG_CHECKING(whether DBC defined in db.h)
      AC_TRY_LINK([#include <db.h>], [size_t len=sizeof(DBC)],
		  AC_DEFINE(HAVE_BDB_CURSOR, 1, [BDB defines DBC])
		  AC_MSG_RESULT(yes),
		  AC_MSG_RESULT(no))
      AC_MSG_CHECKING(number of arguments to db_cursor)
      AC_TRY_LINK([#include <stdio.h>
		   #include <db.h>], [DB* db; db->cursor(db, NULL, NULL, 0);],
		  AC_DEFINE(HAVE_BDB_CURSOR_4_ARGS, 1, [BDB cursor method has 4 arguments])
		  AC_MSG_RESULT(4),
		  AC_MSG_RESULT(3))
    fi

    if test "$have_libdb" = yes; then
      bdb_available="Version $bdb_version (library $bdb_dbname in $bdb_lib_dir)"
      LIBRDF_LIBS="$LIBRDF_LIBS $nLDFLAGS -l$bdb_dbname"
      LIBRDF_CPPFLAGS="$LIBRDF_CPPFLAGS $nCPPFLAGS"
    fi

  fi

fi

CPPFLAGS="$LIBRDF_CPPFLAGS"
LDFLAGS="$LIBRDF_LDFLAGS"
LIBS="$LIBRDF_LIBS"


dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(errno.h stdlib.h unistd.h string.h fcntl.h dmalloc.h time.h sys/time.h sys/stat.h getopt.h)
AC_HEADER_TIME

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN

dnl Checks for library functions.
AC_CHECK_FUNCS(getopt getopt_long memcmp mkstemp mktemp tmpnam gettimeofday getenv)

AM_CONDITIONAL(MEMCMP, test $ac_cv_func_memcmp = no)
AM_CONDITIONAL(GETOPT, test $ac_cv_func_getopt = no -a $ac_cv_func_getopt_long = no)

dnl Checks for URI resolvers

CPPFLAGS="$LIBRDF_CPPFLAGS"
LDFLAGS="$LIBRDF_LDFLAGS"
LIBS="$LIBRDF_LIBS"


dnl Checks for XML parsers

# At present this is for passing on to raptor and not used here
AC_ARG_WITH(xml-parser, [  --with-xml-parser=NAME  Use XML parser - expat, libxml (default=libxml)], xml_parser="$withval", xml_parser="expat") 




dnl Checks for RDF parsers
parser_modules=raptor
rdf_parsers_available=

# raptor is REQUIRED despite the checking here
AC_CHECK_PROG(RAPTOR_CONFIG, raptor-config, raptor-config)
AC_ARG_WITH(raptor, [  --with-raptor=SOURCE    Pick raptor source - system/internal (default=internal)], with_raptor="$withval", with_raptor="internal")

RAPTOR_MIN_VERSION=1.4.17
AC_SUBST(RAPTOR_MIN_VERSION)
if test "X$RAPTOR_CONFIG" = "X"; then
  with_raptor=internal
else
  RAPTOR_VERSION=`$RAPTOR_CONFIG --version 2>/dev/null`

  # FIXME - could use raptor-config --decimal-version at some point
  RAPTOR_VERSION_DEC=`echo $RAPTOR_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
  RAPTOR_MIN_VERSION_DEC=`echo $RAPTOR_MIN_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

  if test "X$with_raptor" = Xauto; then
    if test "X$RAPTOR_VERSION" -a $RAPTOR_VERSION_DEC -ge $RAPTOR_MIN_VERSION_DEC; then
      with_raptor=system
    else
      with_raptor=internal
    fi
  elif test $with_raptor = system; then
    if test $RAPTOR_VERSION_DEC -lt $RAPTOR_MIN_VERSION_DEC; then
      AC_MSG_WARN(System raptor $RAPTOR_VERSION is too old - need $RAPTOR_MIN_VERSION)
      AC_MSG_WARN(Proceeding anyway since --with-raptor=system was given)
      AC_MSG_WARN(To use the internal raptor use --with-raptor=internal)
      AC_MSG_WARN(or omit any --with-raptor argument.)
    fi
  fi
fi

AC_MSG_CHECKING(raptor library source)
if test $with_raptor = system; then
  AC_MSG_RESULT(system $RAPTOR_VERSION)
  rdf_parsers_available="$parser_available raptor(system $RAPTOR_VERSION)"
else
  with_raptor=internal
  AC_MSG_RESULT(internal)
  rdf_parsers_available="$parser_available raptor(internal)"
fi
AC_SUBST(with_raptor)


# Always have these
raptor_dir=raptor

config_subdir_raptor=no
if test $with_raptor = internal; then
  LIBRDF_INTERNAL_CPPFLAGS="-I\$(top_srcdir)/raptor/src $LIBRDF_INTERNAL_CPPFLAGS"
  LIBRDF_INTERNAL_LIBS="\$(top_builddir)/raptor/src/libraptor.la $LIBRDF_INTERNAL_LIBS"
  config_subdir_raptor=yes
else
  LIBRDF_EXTERNAL_CPPFLAGS="`$RAPTOR_CONFIG --cflags` $LIBRDF_EXTERNAL_CPPFLAGS"
  LIBRDF_EXTERNAL_LIBS="`$RAPTOR_CONFIG --libs` $LIBRDF_EXTERNAL_LIBS"
  raptor_dir=
fi

if test "$config_subdir_raptor" = yes -o "$USE_MAINTAINER_MODE" = yes; then
  AC_CONFIG_SUBDIRS(raptor)
fi


# rasqal is REQUIRED despite the checking here
AC_CHECK_PROG(RASQAL_CONFIG, rasqal-config, rasqal-config)
AC_ARG_WITH(rasqal, [  --with-rasqal=SOURCE    Pick rasqal source - system/internal (default=internal)], with_rasqal="$withval", with_rasqal="internal")

RASQAL_MIN_VERSION=0.9.16
RASQAL_MAX_VERSION=0.9.99
AC_SUBST(RASQAL_MIN_VERSION)
AC_SUBST(RASQAL_MAX_VERSION)
if test "X$RASQAL_CONFIG" = "X"; then
  with_rasqal=internal
else
  RASQAL_VERSION=`$RASQAL_CONFIG --version 2>/dev/null`

  RASQAL_VERSION_DEC=`$RASQAL_CONFIG --version-decimal`
  RASQAL_MIN_VERSION_DEC=`echo $RASQAL_MIN_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
  RASQAL_MAX_VERSION_DEC=`echo $RASQAL_MAX_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

  if test "X$with_rasqal" = Xauto; then
    if test "X$RASQAL_VERSION" -a $RASQAL_VERSION_DEC -ge $RASQAL_MIN_VERSION_DEC -a $RASQAL_VERSION_DEC -le $RASQAL_MAX_VERSION_DEC; then
      with_rasqal=system
    else
      with_rasqal=internal
    fi
  elif test $with_rasqal = system; then
    if test $RASQAL_VERSION_DEC -lt $RASQAL_MIN_VERSION_DEC -o $RASQAL_VERSION_DEC -gt $RASQAL_MAX_VERSION_DEC; then
      AC_MSG_WARN(System rasqal $RASQAL_VERSION is not in range $RASQAL_MIN_VERSION to $RASQAL_MAX_VERSION)
      AC_MSG_WARN(Proceeding anyway since --with-rasqal=system was given)
      AC_MSG_WARN(To use the internal rasqal use --with-rasqal=internal)
      AC_MSG_WARN(or omit any --with-rasqal argument.)
    fi
  fi
fi

if test "X$with_rasqal" = Xinternal; then
  rdf_query="rasqal(internal)"
else
  rdf_query="rasqal(system $RASQAL_VERSION)"
fi
AC_SUBST(with_rasqal)



dnl Checks for modules
digest_modules="md5 sha1 ripemd160"

AC_MSG_CHECKING(digests wanted)
AC_ARG_ENABLE(digests, [  --enable-digests=LIST   Use digests (default=md5 sha1 ripemd160)], digest_modules="$enableval") 
AC_MSG_RESULT($digest_modules)

DIGEST_OBJS=
DIGEST_SRCS=

AC_ARG_WITH(openssl-digests, [  --with-openssl-digests  Use openssl digests (default=yes)], enable_openssl_digests="$withval", enable_openssl_digests="no") 

# This is needed because autoheader can't work out which computed
# symbols must be pulled from acconfig.h into config.h.in
if test "x" = "y"; then
  AC_DEFINE(HAVE_OPENSSL_CRYPTO_MD5_DIGEST, 1, [Have openssl MD5 digest])
  AC_DEFINE(HAVE_OPENSSL_CRYPTO_SHA1_DIGEST, 1, [Have openssl SHA1 digest])
  AC_DEFINE(HAVE_OPENSSL_CRYPTO_RIPEMD160_DIGEST, 1, [Have openssl RIPEMD160 digest])
fi

digest_modules_available=
AC_MSG_CHECKING(whether to use openssl digests)
if test "$enable_openssl_digests" = yes ; then
  AC_MSG_RESULT(yes)
  AC_CHECK_HEADERS(openssl/crypto.h)
  if test "$ac_cv_header_openssl_crypto_h" = yes ; then
    AC_DEFINE(HAVE_OPENSSL_DIGESTS, 1, [Have openssl digests])
    new_digest_modules=
    LIBS="$LIBRDF_LIBS -lcrypto"
    have_libcrypto=no

    for module in $digest_modules; do
      func=`echo $module | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
      found=
      AC_MSG_CHECKING(for openssl $func digest module)
      AC_CACHE_VAL(ac_cv_lib_crypto_$func,
                   [AC_TRY_LINK(, $func(),
                   [eval "ac_cv_lib_crypto_$func=yes"],
                   [eval "ac_cv_lib_crypto_$func=no"])])
      if eval "test \"`echo '$ac_cv_lib_crypto_'$func`\" = yes"; then
	AC_MSG_RESULT(yes)
	n=HAVE_OPENSSL_CRYPTO_${func}_DIGEST
	AC_DEFINE_UNQUOTED($n)
        have_libcrypto=yes
        digest_modules_available="$digest_modules_available $module(openssl)"
      else
	AC_MSG_RESULT(no)
	new_digest_modules="${new_digest_modules} $module"
      fi
    done
    if test "$have_libcrypto" = yes; then
      LIBRDF_LIBS="$LIBRDF_LIBS -lcrypto"
    fi
    DIGEST_OBJS="$DIGEST_OBJS rdf_digest_openssl.lo"
    DIGEST_SRCS="$DIGEST_SRCS rdf_digest_openssl.c"
    digest_modules=$new_digest_modules
  fi
else
  AC_MSG_RESULT(no)
fi


dnl hashes
HASH_OBJS=
HASH_SRCS=

AC_MSG_CHECKING(for bdb hash support)
if test "$have_libdb" = yes; then
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_BDB_HASH, 1, [Have BDB hash support])
  HASH_OBJS="$HASH_OBJS rdf_hash_bdb.lo"
  HASH_SRCS="$HASH_SRCS rdf_hash_bdb.c"
else
  AC_MSG_RESULT(no)
fi


AC_SUBST(HASH_OBJS)
AC_SUBST(HASH_SRCS)


dnl Storages
persistent_storages="/file/tstore/mysql/sqlite/"
persistent_store=no
all_storages="memory file hashes trees mysql sqlite tstore postgresql"
always_available_storages="memory file hashes trees"

dnl default availabilities and enablements
for storage in $all_storages; do
  eval $storage'_storage_available=no'
  eval $storage'_storage=no'
done
for storage in $always_available_storages; do
  eval $storage'_storage_available=yes'
  eval $storage'_storage=yes'
done


storages_available="file hashes(memory) trees"
if test "$have_libdb" = yes; then
  storages_available="$storages_available hashes(bdb $bdb_version)"
fi

AC_ARG_WITH(threestore, [  --with-threestore(=CONFIG|yes|no)  Location of 3store-config (default=no)], tstore_config="$withval", tstore_config="no")
if test "X$tstore_config" != "Xno"; then
  if test "X$tstore_config" != "Xyes" ; then
    TSTORE_CONFIG=$tstore_config
  fi
  tstore_config=yes
fi
if test $tstore_config = yes -a "X$TSTORE_CONFIG" = "X" ; then
  AC_CHECK_PROGS(TSTORE_CONFIG, 3store-config)
fi

AC_MSG_CHECKING(for 3store library)
if test "X$TSTORE_CONFIG" != X -a "X$PKG_CONFIG" != X ; then
  tstore_version=`$TSTORE_CONFIG --version`

  storages_available="$storages_available 3store($tstore_version)"
  tstore_storage_available=yes

  AC_MSG_RESULT(yes - version $tstore_version)
else
  AC_MSG_RESULT(no)
fi


AC_ARG_WITH(mysql, [  --with-mysql(=CONFIG|yes|no) Location of MySQL 3/4 mysql_config (default=no)], mysql_config="$withval", mysql_config="no")
if test "X$mysql_config" != "Xno"; then
  if test "X$mysql_config" != "Xyes" ; then
    MYSQL_CONFIG=$mysql_config
  fi
  mysql_config=yes
fi
if test $mysql_config = yes -a "X$MYSQL_CONFIG" = "X" ; then
  AC_CHECK_PROGS(MYSQL_CONFIG, mysql_config)
fi

libmysql_min_version=3.23.56
# Also tested on 4.0.14

mysql_cflags=
mysql_libs=
if test "X$MYSQL_CONFIG" != X; then
  dnl need to change quotes to allow square brackets
  changequote(<<, >>)dnl
  mysql_libs=`$MYSQL_CONFIG --libs | sed -e "s/'//g"`
  # Stop the MySQL config program adding -Os or -O<n> options to CFLAGS
  mysql_cflags=`$MYSQL_CONFIG --cflags | sed -e "s/'//g" -e "s/[ \t]-O[A-Za-z0-9]*/ /" `
  changequote([, ])dnl

  LIBS="$LIBRDF_LIBS $mysql_libs"
  CPPFLAGS="$LIBRDF_CPPFLAGS $mysql_cflags"
  AC_MSG_CHECKING(for mysql library)
  LIBMYSQL_VERSION=`$MYSQL_CONFIG --version`
  libmysql_version_dec=`echo $LIBMYSQL_VERSION | awk -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
  libmysql_min_version_dec=`echo $libmysql_min_version | awk -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
  AC_MSG_RESULT(yes - version $LIBMYSQL_VERSION)
  if test $libmysql_version_dec -lt $libmysql_min_version_dec; then
    AC_MSG_WARN(Using libmysql $LIBMYSQL_VERSION is unsupported - $libmysql_min_version or newer recommended.)
  else
    storages_available="$storages_available mysql($LIBMYSQL_VERSION)"
    mysql_storage_available=yes
  fi

AC_MSG_CHECKING(whether MYSQL_OPT_RECONNECT is declared)
AC_COMPILE_IFELSE([#include <mysql.h>
int x=MYSQL_OPT_RECONNECT;],
   AC_DEFINE(HAVE_MYSQL_OPT_RECONNECT, 1, [have MYSQL_OPT_RECONNECT declared])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))


  LIBS="$LIBRDF_LIBS"
  CPPFLAGS="$LIBRDF_CPPFLAGS"
fi



AC_ARG_WITH(sqlite, [  --with-sqlite(=yes|no|2|3) Enable SQLite store with version (default=no)], sqlite_api_version="$withval", sqlite_api_version="no")
AC_ARG_WITH(sqlite-version, [  --with-sqlite-version(=yes|no|2|3) SQLite version to use (default=no)], sqlite_api_version="no")
if test "x$sqlite_api_version" = "xyes" ; then
  sqlite_api_version="auto"
fi

AC_MSG_CHECKING(for sqlite library versions available)
sqlite_versions=
if test "X$PKG_CONFIG" != X ; then
  sqlite_version=`$PKG_CONFIG sqlite3 --modversion 2>/dev/null`
  if test "X$sqlite_version" != X; then
    if test "X$sqlite_api_version" = Xauto; then
      sqlite_api_version=3
    fi
    sqlite_versions=$sqlite_version
  fi
  sqlite_version=`$PKG_CONFIG sqlite --modversion 2>/dev/null`
  if test "X$sqlite_version" != X; then
    if test "X$sqlite_api_version" = Xauto; then
      sqlite_api_version=2
    fi
    sqlite_versions="$sqlite_version $sqlite_versions"
  fi
fi
AC_MSG_RESULT($sqlite_versions)

if test "X$sqlite_api_version" = Xauto; then
  sqlite_api_version=no
fi

AC_MSG_CHECKING(using sqlite library)
if test "X$sqlite_api_version" != Xno ; then
  if test "$sqlite_api_version" = 3; then
    SQLITE_CONFIG="$PKG_CONFIG sqlite3"
  else
    SQLITE_CONFIG="$PKG_CONFIG sqlite"
  fi
  sqlite_version=`$SQLITE_CONFIG --modversion 2>/dev/null`
  AC_DEFINE_UNQUOTED(SQLITE_API, $sqlite_api_version, [SQLite API version])

  storages_available="$storages_available sqlite($sqlite_version)"
  sqlite_storage_available=yes
  AC_MSG_RESULT(yes - version $sqlite_version)

  CPPFLAGS="$LIBRDF_CPPFLAGS `$SQLITE_CONFIG --cflags`"
  AC_CHECK_HEADERS(sqlite3.h sqlite.h)
  CPPFLAGS="$LIBRDF_CPPFLAGS"
else
  AC_MSG_RESULT(no)
fi

AC_ARG_WITH(postgresql, [  --with-postgresql(=CONFIG|yes|no)  Location of pg_config (default=no)], pg_config="$withval", pg_config="no")
if test "X$pg_config" != "Xno"; then
  if test "X$pg_config" != "Xyes" ; then
    PG_CONFIG=$pg_config
  fi
  pg_config=yes
fi
if test $pg_config = yes -a "X$PG_CONFIG" = "X" ; then
  AC_CHECK_PROGS(PG_CONFIG, pg_config)
fi

AC_MSG_CHECKING(for postgresql library)
if test "X$PG_CONFIG" != X ; then
  postgresql_version=`$PG_CONFIG --version | sed -e 's/^.* //'`
  storages_available="$storages_available postgresql($postgresql_version)"
  postgresql_storage_available=yes

  AC_MSG_RESULT(yes - version $postgresql_version)
  
  CPPFLAGS="$LIBRDF_CPPFLAGS -I`$PG_CONFIG --includedir`"
  AC_CHECK_HEADERS(libpq-fe.h)
  CPPFLAGS="$LIBRDF_CPPFLAGS"
  
  LIBS="$LIBRDF_LIBS -L`$PG_CONFIG --libdir` -lpq"
else
  AC_MSG_RESULT(no)
fi


# This is needed because autoheader can't work out which computed
# symbols must be pulled from acconfig.h into config.h.in
if test "x" = "y"; then
  AC_DEFINE(STORAGE_FILE,   1, [Building file storage])
  AC_DEFINE(STORAGE_HASHES, 1, [Building hashes storage])
  AC_DEFINE(STORAGE_TREES,  1, [Building trees storage])
  AC_DEFINE(STORAGE_MEMORY, 1, [Building memory storage])
  AC_DEFINE(STORAGE_MYSQL,  1, [Building MySQL storage])
  AC_DEFINE(STORAGE_SQLITE, 1, [Building SQLite storage])
  AC_DEFINE(STORAGE_TSTORE, 1, [Building 3store storage])
  AC_DEFINE(STORAGE_POSTGRESQL, 1, [Building PostgreSQL storage])
fi


AC_MSG_CHECKING(storages required)
AC_ARG_ENABLE(storages, [  --enable-storages=LIST   Use storages (default=all)], storages="$enableval") 
if test "X$storages" = Xall -o "X$storages" = X; then
  storages="$all_storages"
  AC_MSG_RESULT(all)
else
  AC_MSG_RESULT($storages)
fi

dnl absolutely ensuring memory is enabled
if echo $storages | grep memory >/dev/null; then
 :
else
  storages="memory $storages"
fi

for storage in $storages; do
  if eval test \$$storage'_storage_available' = yes; then
    eval $storage'_storage=yes'
    NAME=`echo $storage | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
    n=STORAGE_${NAME}
    AC_DEFINE_UNQUOTED($n)
    storages_enabled="$storages_enabled $storage"

    if echo $persistent_storages | grep "/$storage/" >/dev/null; then
      persistent_store=yes
    fi
  fi

done

AM_CONDITIONAL(STORAGE_FILE,   test $file_storage   = yes)
AM_CONDITIONAL(STORAGE_HASHES, test $hashes_storage = yes)
AM_CONDITIONAL(STORAGE_TREES,  test $trees_storage  = yes)
AM_CONDITIONAL(STORAGE_MEMORY, test $memory_storage = yes)
AM_CONDITIONAL(STORAGE_MYSQL,  test $mysql_storage  = yes)
AM_CONDITIONAL(STORAGE_SQLITE, test $sqlite_storage = yes)
AM_CONDITIONAL(STORAGE_TSTORE, test $tstore_storage = yes)
AM_CONDITIONAL(STORAGE_POSTGRESQL, test $postgresql_storage = yes)

if test $mysql_storage  = yes; then
  MYSQL_CPPFLAGS=`$MYSQL_CONFIG --cflags | sed -e "s/'//g" -e "s/-O[A-Za-z0-9]*//" `
  MYSQL_LIBS="$LIBRDF_LIBS $mysql_libs"
fi
if test $sqlite_storage  = yes; then
  SQLITE_CPPFLAGS="`$SQLITE_CONFIG --cflags`"
  SQLITE_LIBS="`$SQLITE_CONFIG --libs`"
fi
if test $tstore_storage  = yes; then
  TSTORE_CPPFLAGS="`$TSTORE_CONFIG --cflags` `$PKG_CONFIG glib --cflags`"
  TSTORE_LIBS="`$TSTORE_CONFIG --libs` `$PKG_CONFIG glib --libs`"
fi
if test $postgresql_storage  = yes; then
  POSTGRESQL_CPPFLAGS="-I`$PG_CONFIG --includedir`"
  POSTGRESQL_LIBS="-L`$PG_CONFIG --libdir` -lpq"
fi


if test "$persistent_store" = no; then
  AC_MSG_WARN(No persistant storage is available or enabled)
fi

AC_SUBST(MYSQL_CPPFLAGS)
AC_SUBST(MYSQL_LIBS)
AC_SUBST(SQLITE_CPPFLAGS)
AC_SUBST(SQLITE_LIBS)
AC_SUBST(TSTORE_CPPFLAGS)
AC_SUBST(TSTORE_LIBS)
AC_SUBST(POSTGRESQL_CPPFLAGS)
AC_SUBST(POSTGRESQL_LIBS)

if test "$modular" = yes; then
  LIBRDF_CPPFLAGS="$LIBRDF_CPPFLAGS $INCLTDL"
  LIBRDF_LIBS="$LIBRDF_LIBS $LIBLTDL"
  AC_DEFINE(MODULAR_LIBRDF, 1, [Build storage backends as modules])
else
  LIBRDF_CPPFLAGS="$LIBRDF_CPPFLAGS $MYSQL_CPPFLAGS $SQLITE_CPPFLAGS $TSTORE_CPPFLAGS $POSTGRESQL_CPPFLAGS"
  LIBRDF_LIBS="$LIBRDF_LIBS $MYSQL_LIBS $SQLITE_LIBS $TSTORE_LIBS $POSTGRESQL_LIBS"
fi

AM_CONDITIONAL(MODULAR_LIBRDF, test $modular = yes)

CPPFLAGS="$LIBRDF_CPPFLAGS"
LDFLAGS="$LIBRDF_LDFLAGS"
LIBS="$LIBRDF_LIBS"


dnl Check for use of threads

AC_ARG_WITH(threads, [  --with-threads          With POSIX thread support (default=no)], with_threads="$withval", with_threads="no")

AC_CHECK_HEADERS(pthread.h)

AC_MSG_CHECKING(if POSIX threads should be used)
if test $with_threads = "yes" ; then

  if test "$ac_cv_header_pthread_h" = yes ; then
    AC_DEFINE(WITH_THREADS, 1, [Use POSIX threads])
    LIBRDF_LIBS="$LIBRDF_LIBS -lpthread"
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
  fi
else
  AC_MSG_RESULT(no)
fi
  
LIBS=$LIBRDF_LIBS


# Maybe add some local digest modules
for module in $digest_modules; do
  module_u=`echo $module | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
  AC_MSG_CHECKING(for local $module digest module)
  base=rdf_digest_$module
  if test -r $srcdir/librdf/$base.c; then
    AC_MSG_RESULT(yes)

    n=HAVE_LOCAL_${module_u}_DIGEST
    AC_DEFINE_UNQUOTED($n)

    DIGEST_OBJS="$DIGEST_OBJS $base.lo"
    DIGEST_SRCS="$DIGEST_SRCS $base.c"
    digest_modules_available="$digest_modules_available $module"
  else
    AC_MSG_RESULT(no)
  fi
done

AC_SUBST(DIGEST_OBJS)
AC_SUBST(DIGEST_SRCS)

# This is needed because autoheader can't work out which computed
# symbols must be pulled from acconfig.h into config.h.in
if test "x" = "y"; then
  AC_DEFINE(HAVE_LOCAL_MD5_DIGEST, 1, [Have local MD5 digest])
  AC_DEFINE(HAVE_LOCAL_SHA1_DIGEST, 1, [Have local SHA1 digest])
  AC_DEFINE(HAVE_LOCAL_RIPEMD160_DIGEST, 1, [Have local RIPEMD160 digest])
fi


dnl RDF query
rasqal_dir=
config_subdir_rasqal=no

if test $with_rasqal = internal; then
  LIBRDF_INTERNAL_CPPFLAGS="-I\$(top_srcdir)/rasqal/src $LIBRDF_INTERNAL_CPPFLAGS"
  LIBRDF_INTERNAL_LIBS="\$(top_builddir)/rasqal/src/librasqal.la $LIBRDF_INTERNAL_LIBS"
  rasqal_dir=rasqal
  config_subdir_rasqal=yes
else
  LIBRDF_EXTERNAL_CPPFLAGS="`$RASQAL_CONFIG --cflags` $LIBRDF_EXTERNAL_CPPFLAGS"
  LIBRDF_EXTERNAL_LIBS="`$RASQAL_CONFIG --libs` $LIBRDF_EXTERNAL_LIBS"
fi

if test "$config_subdir_rasqal" = yes -o "$USE_MAINTAINER_MODE" = yes; then
  AC_CONFIG_SUBDIRS(rasqal)
fi



#LIBRDF_EXTERNAL_CPPFLAGS="$LIBRDF_EXTERNAL_CPPFLAGS $LIBRDF_CPPFLAGS"
LIBRDF_EXTERNAL_LIBS="$LIBRDF_EXTERNAL_LIBS $LIBRDF_LIBS"

# Everything except things with .pc files (raptor and rasqal)
# 3store uses it but is optional and thus tricky to specify this way
LIBRDF_PKGCONFIG_PRIVATE_LIBS=$LIBRDF_LIBS

dnl Local sub-packages in sub directories needed to build librdf
dnl Ordering constraints: rasqal needs raptor
if test "$USE_MAINTAINER_MODE" = yes; then 
  LOCAL_SUBDIRS="raptor rasqal"
else
  LOCAL_SUBDIRS="$raptor_dir $rasqal_dir"
fi

AC_SUBST(LOCAL_SUBDIRS)


# Make final changes to cflags
CFLAGS="-DLIBRDF_INTERNAL=1 $CFLAGS"
MEM=
MEM_LIBS=
AC_ARG_WITH(dmalloc, [  --with-dmalloc          Use dmalloc debugging library (default=no)], use_dmalloc="$withval", use_dmalloc="no") 
AC_MSG_CHECKING(using dmalloc library)
if test "$USE_MAINTAINER_MODE" = yes; then
  if test "$ac_cv_header_dmalloc_h" = yes; then
    if test "X$use_dmalloc" = Xauto; then
      use_dmalloc=yes
    fi
  else
    use_dmalloc=no
  fi
else
  use_dmalloc=no
fi
AC_MSG_RESULT($use_dmalloc)
if test $use_dmalloc = yes; then
  MEM=-DLIBRDF_MEMORY_DEBUG_DMALLOC=1
  MEM_LIBS=-ldmalloc
fi
AC_ARG_WITH(memory-signing, [  --with-memory-signing   Sign allocated memory (default=no)], use_memory_signing="$withval", use_memory_signing="no")
AC_MSG_CHECKING(using memory signing)
AC_MSG_RESULT($use_memory_signing)
if test $use_memory_signing = yes; then
  MEM=-DLIBRDF_MEMORY_SIGN=1
  MEM_LIBS=
fi

assert=yes
AC_ARG_ENABLE(assertions, [  --enable-assertions    Perform run time assertion checks (default yes).  ], assert=$enableval)
if test "$assert" = "yes"; then
  AC_DEFINE(LIBRDF_ASSERT, 1, [Run time assertion checks.])
fi

assert_messages=yes
AC_ARG_ENABLE(assert-messages, [  --enable-assert-messages  Print run time assertion check failure messages (default yes).  ], assert_messages=$enableval)	
if test "$assert_messages" = "yes"; then
  AC_DEFINE(LIBRDF_ASSERT_MESSAGES, 1, [Print run time assertion check failure messages.])
fi

if test "$USE_MAINTAINER_MODE" = yes; then
  LIBRDF_CPPFLAGS="-g -DLIBRDF_DEBUG=1 $LIBRDF_CPPFLAGS"
fi

STANDARD_CFLAGS="$STANDARD_CFLAGS $CFLAGS"
if test "$USE_MAINTAINER_MODE" = yes; then
  CFLAGS="$MAINTAINER_CFLAGS $CFLAGS"
fi

if test "$USE_MAINTAINER_MODE" = yes -a $release_version = no; then 
  dnl need to change quotes to allow square brackets
  changequote(<<, >>)dnl
  CFLAGS=`echo $CFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  CXXFLAGS=`echo $CXXFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  STANDARD_CFLAGS=`echo $STANDARD_CFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  LIBRDF_CPPFLAGS=`echo $LIBRDF_CPPFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  LIBRDF_EXTERNAL_CPPFLAGS=`echo $LIBRDF_EXTERNAL_CPPFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  LIBRDF_INTERNAL_CPPFLAGS=`echo $LIBRDF_INTERNAL_CPPFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  changequote([, ])dnl
fi


LIBS=
CPPFLAGS=
LDFLAGS=

LIBRDF_LIBTOOLLIBS=librdf.la

AC_SUBST(LIBRDF_EXTERNAL_CPPFLAGS)
AC_SUBST(LIBRDF_EXTERNAL_LIBS)

AC_SUBST(LIBRDF_INTERNAL_CPPFLAGS)
AC_SUBST(LIBRDF_INTERNAL_LIBS)

AC_SUBST(LIBRDF_CPPFLAGS)
AC_SUBST(LIBRDF_LDFLAGS)
AC_SUBST(LIBRDF_LIBTOOLLIBS)
AC_SUBST(LIBRDF_PKGCONFIG_PRIVATE_LIBS)

AC_SUBST(MEM)
AC_SUBST(MEM_LIBS)
AC_SUBST(STANDARD_CFLAGS)

abs_top_srcdir=`cd $srcdir; pwd`
AC_SUBST(abs_top_srcdir)
abs_top_builddir=`pwd`
AC_SUBST(abs_top_builddir)

AC_CONFIG_FILES([Makefile
redland.rdf
librdf/redland.spec
docs/Makefile
docs/version.xml
librdf/Makefile
librdf/win32/Makefile
examples/Makefile
utils/Makefile
demos/Makefile
data/Makefile
redland.pc])
AC_CONFIG_FILES([redland-config], [chmod +x redland-config])
AC_CONFIG_FILES([redland-src-config], [chmod +x redland-src-config])

dnl Check for gtk-doc and docbook
GTK_DOC_CHECK([1.3])

AC_OUTPUT

AC_MSG_RESULT([
Redland build summary:
])

if test "$modular" = yes; then
  AC_MSG_RESULT([  Using modular storage backends])
else
  AC_MSG_RESULT([  Using monolithic storage backends])
fi

AC_MSG_RESULT([
  Berkeley/Sleepycat DB   : $bdb_available
  Triple stores available : $storages_available
  Triple stores enabled   :$storages_enabled
  RDF parsers             :$rdf_parsers_available
  RDF query               : $rdf_query
  Content digests         :$digest_modules_available
])
