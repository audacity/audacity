dnl -*- Mode: autoconf -*-
dnl
dnl configure.in - autoconf file for Raptor
dnl (Process this file with autoconf to produce a configure script.)
dnl
dnl Copyright (C) 2000-2008 David Beckett http://www.dajobe.org/
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


AC_INIT(Raptor RDF Parser, 1.4.18, http://bugs.librdf.org/, raptor)
AC_PREREQ(2.50)
AC_CONFIG_SRCDIR(src/raptor_general.c)
AC_REVISION($Revision: 1.2 $)

libxml_min_version=2.6.8

libxslt_min_version=1.0.18

AM_INIT_AUTOMAKE([1.7 check-news std-options -Wall])
AM_CONFIG_HEADER(src/raptor_config.h)
AM_MAINTAINER_MODE

release_version=no
AC_ARG_ENABLE(release, [  --enable-release        Turn on optimizations (for maintainer).  ], \
 if test "$enableval" = "yes"; then \
	  release_version=yes
fi;)	

AM_CONDITIONAL(RELEASE_VERSION, test $release_version = yes)
if test "$USE_MAINTAINER_MODE" = yes -a $release_version = no; then 
  CFLAGS=`echo $CFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  CXXFLAGS=`echo $CXXFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
  CPPFLAGS=`echo $CPPFLAGS | sed -e "s/-O[A-Za-z0-9]*//"`
fi
RPM_RELEASE=SNAP
if test "$release_version" = "yes"; then
  RPM_RELEASE=1
fi
AC_SUBST(RPM_RELEASE)


dnl Checks for programs.
AC_CANONICAL_HOST
AM_SANITY_CHECK
AM_PROG_CC_STDC

m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])
AC_PROG_LIBTOOL
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_PROG_LEX
if test $LEX != : ; then
  if echo "$LEX" | grep flex >/dev/null 2>&1; then
    :
  else
    LEX="$SHELL $missing_dir/missing flex"
    AC_SUBST(LEX_OUTPUT_ROOT, lex.yy)
    AC_SUBST(LEXLIB, '')
  fi
fi

if test "$USE_MAINTAINER_MODE" = yes; then
  FLEX_MIN_VERSION=2.5.19
  FLEX_REC_VERSION=2.5.33
  AC_MSG_CHECKING(flex version)
  if test "X$LEX" != "X:"; then
    FLEX_VERSION=`$LEX -V 2>&1 | awk '{print $NF}'`
    FLEX_VERSION_DEC=`echo $FLEX_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

    FLEX_MIN_VERSION_DEC=`echo $FLEX_MIN_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

    if test $FLEX_VERSION_DEC -ge $FLEX_MIN_VERSION_DEC; then
      AC_MSG_RESULT($FLEX_VERSION - OK)
    else
      AC_MSG_RESULT(version $FLEX_VERSION - too old)
      AC_MSG_WARN(Please get flex from http://flex.sourceforge.net/)
      AC_MSG_WARN(version $FLEX_MIN_VERSION ($FLEX_REC_VERSION recommended))
      AC_MSG_FAILURE(flex too old)
    fi
  else
    AC_MSG_RESULT(not present)
    AC_MSG_WARN(Please get flex from http://flex.sourceforge.net/)
    AC_MSG_WARN(version $FLEX_MIN_VERSION ($FLEX_REC_VERSION recommended))
    AC_MSG_FAILURE(flex not present)
  fi
fi

AC_PROG_YACC
if test "$USE_MAINTAINER_MODE" = yes; then
  AC_MSG_CHECKING(for GNU bison)
  # Match these styles of versions
  # GNU Bison version 1.28
  # bison (GNU Bison) 1.875
dnl need to change quotes to allow square brackets
changequote(<<, >>)dnl
  YACC_VERSION=`$YACC --version 2>&1 | sed -ne 's/^.*GNU Bison[^0-9]*//p'`
changequote([, ])dnl
  if test "X$YACC_VERSION" != X; then
    AC_MSG_RESULT($YACC_VERSION - OK)
  else
    AC_MSG_FAILURE($YACC is not GNU bison)
  fi
fi


# Find a tar command for 'make dist'
AC_CHECK_PROGS(TAR, gnutar gtar tar)
AC_CHECK_PROGS(PERL, perl)

AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)


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


dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(errno.h stdlib.h unistd.h string.h limits.h math.h dmalloc.h getopt.h sys/stat.h sys/param.h sys/time.h setjmp.h)
AC_HEADER_TIME
dnl FreeBSD fetch.h needs stdio.h and sys/param.h first
AC_CHECK_HEADERS(fetch.h,,,
  [#include <stdio.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])



dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN

dnl for raptor_nfc.c
AC_CHECK_TYPES([u8])
AC_CHECK_TYPES([u16])

AC_CHECK_SIZEOF(unsigned char, 1)
AC_CHECK_SIZEOF(unsigned short, 2)
AC_CHECK_SIZEOF(unsigned int, 4)
AC_CHECK_SIZEOF(unsigned long, 4)
  

dnl need to change quotes to allow square brackets
changequote(<<, >>)dnl
version_major=`echo $VERSION | sed -e 's/^\([^\.]*\)\.\([^\.]*\)\.\(.*\)$/\1/'`
version_minor=`echo $VERSION | sed -e 's/^\([^\.]*\)\.\([^\.]*\)\.\(.*\)$/\2/'`
version_release=`echo $VERSION | sed -e 's/^\([^\.]*\)\.\([^\.]*\)\.\(.*\)$/\3/'`
changequote([, ])dnl

version_decimal=`expr $version_major \* 10000 + $version_minor \* 100 + $version_release`

AC_DEFINE_UNQUOTED(RAPTOR_VERSION_MAJOR, $version_major, [Major version number])
AC_DEFINE_UNQUOTED(RAPTOR_VERSION_MINOR, $version_minor, [Minor version number])
AC_DEFINE_UNQUOTED(RAPTOR_VERSION_RELEASE, $version_release, [Release version number])
AC_DEFINE_UNQUOTED(RAPTOR_VERSION_DECIMAL, $version_decimal, [Release version as a decimal])

# for raptor-config.in
RAPTOR_VERSION_DECIMAL=$version_decimal
AC_SUBST(RAPTOR_VERSION_DECIMAL)

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
RAPTOR_LIBTOOL_VERSION=2:0:1
AC_SUBST(RAPTOR_LIBTOOL_VERSION)


dnl Checks for library functions.
AC_CHECK_FUNCS(getopt getopt_long stricmp strcasecmp vsnprintf isascii setjmp)

AM_CONDITIONAL(STRCASECMP, test $ac_cv_func_stricmp = no -a $ac_cv_func_stricmp = no)
AM_CONDITIONAL(GETOPT, test $ac_cv_func_getopt = no -a $ac_cv_func_getopt_long = no)


AC_MSG_CHECKING(whether need to declare optind)
AC_TRY_LINK([#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif], [int x=optind;],
            AC_MSG_RESULT(no),
            AC_DEFINE(NEED_OPTIND_DECLARATION, 1, [need 'extern int optind' declaration?])
            AC_MSG_RESULT(yes))



if test $ac_cv_func_vsnprintf = yes; then
  AC_MSG_CHECKING(vsnprintf has C99 compatible return value)
  AC_TRY_RUN([#include <stdarg.h>
int is_c99(char *s, ...) {
  char buffer[32];
  va_list args;
  int r;
  va_start(args, s);
  r = vsnprintf(buffer, 5, s, args);
  va_end(args);

  return (r == 7);
}

int main(int argc, char* argv) {
  return is_c99("1234567");
}], AC_MSG_RESULT(no),
    AC_DEFINE(HAVE_C99_VSNPRINTF, 1, [vsnprint has C99 compatible return value])
    AC_MSG_RESULT(yes))
fi

need_libm=no

oLIBS="$LIBS"
LIBS="$LIBS -lm"
AC_MSG_CHECKING(whether have trunc in libm)
AC_TRY_LINK([#ifdef HAVE_MATH_H
#include <math.h>
#endif], [double d=trunc(1.0);],
            AC_MSG_RESULT(yes)
            need_libm=yes
            AC_DEFINE(HAVE_TRUNC, 1, [have trunc() in libm]),
            AC_MSG_RESULT(no))

AC_MSG_CHECKING(whether have round in libm)
AC_TRY_LINK([#ifdef HAVE_MATH_H
#include <math.h>
#endif], [double d=round(1.0);],
            AC_MSG_RESULT(yes)
            need_libm=yes
            AC_DEFINE(HAVE_ROUND, 1, [have round() in libm]),
            AC_MSG_RESULT(no))
LIBS="$oLIBS"

if test "X$need_libm" = Xyes; then
  LIBS="$LIBS -lm"
fi


dnl Checks for XML parsers
have_expat=0
have_expat_lib=0
have_expat_source=0
need_expat=0
need_expat_source=0
expat_source_dir=
expat_obj_dir=

AC_ARG_WITH(expat-source, [  --with-expat-source=PATH  Location of expat source tree (default=auto)], expat_source="$withval", expat_source="auto")


AC_MSG_CHECKING(for expat sources)
if test "$expat_source" != "auto"; then
  have_expat_source=1
  have_expat=1
  expat_source_dir=$expat_source
  expat_obj_dir=$expat_source
  AC_MSG_RESULT(yes - $expat_source)
elif test -d $srcdir/expat; then
  expat_source=local
  have_expat_source=1
  have_expat=1
  expat_source_dir="\$(top_srcdir)/expat"
  expat_obj_dir="\$(top_builddir)/expat"
  AC_MSG_RESULT(yes - local source)
else
  expat_source=
  AC_MSG_RESULT(no)
fi

if test "X$expat_source" = X; then
  expat_source=auto
else
  if test -d "$expat_source_dir/xmlparse"; then
    # old expat
    CPPFLAGS="-I$expat_source_dir/xmlparse $CPPFLAGS"
  else
    # new expat
    CPPFLAGS="-I$expat_source_dir/lib $CPPFLAGS"
  fi
fi

AC_CHECK_HEADERS(expat.h xmlparse.h)


oLIBS="$LIBS"
AC_CHECK_LIB(xmltok, main, xmlt=1, xmlt=0)

# Raptor needs XML_ParserCreate expat function (not namespace one)
AC_CHECK_LIB(xmlparse, XML_ParserCreate, xmlp=1, xmlp=0, -lxmltok)

LIBS="$oLIBS"
AC_CHECK_LIB(expat, XML_ParserCreate, libexpat=1, libexpat=0)

LIBS="$oLIBS"
AC_MSG_CHECKING(for working expat in libxmlparse and libxmltok)
if test $xmlp = 1 -a $xmlt = 1 -a  $ac_cv_header_xmlparse_h = yes; then
  LIBS="$LIBS -lxmlparse -lxmltok"
  AC_TRY_RUN([#include <stdio.h>
	     main() {XML_ParserCreate(NULL); return(0);}],
             worked=yes, worked=no, worked=no)
  if test $worked = yes; then
    # Old expat
    have_expat_lib=1
    have_expat=1
    expat_libs="-lxmlparse -lxmltok"

    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
  fi
else
  AC_MSG_RESULT(no)
fi

LIBS="$oLIBS"

AC_MSG_CHECKING(for working expat in libexpat)
if test $libexpat = 1 -a $ac_cv_header_expat_h = yes ; then
  LIBS="$LIBS -lexpat"
  AC_TRY_RUN([#include <stdio.h>
             main() {XML_ParserCreate(NULL); return(0);}],
             worked=yes, worked=no, worked=no)
  if test $worked = yes; then
    # New expat - expat-1.95.0 or later
    have_expat_lib=1
    have_expat=1
    expat_libs="-lexpat"

    AC_MSG_RESULT(yes)

    AC_MSG_CHECKING(for expat support of UTF-8 BOM)
    AC_TRY_RUN([
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#ifdef HAVE_EXPAT_H
#include <expat.h>
#else
#ifdef HAVE_XMLPARSE_H
#include <xmlparse.h>
#endif
#endif

int main(int argc, char **argv) {
  const char *xml_buffer=
     /* UTF-8 BOM */
     "\xef\xbb\xbf<?xml version='1.0' encoding='UTF-8'?><tag/>";
  XML_Parser xp=XML_ParserCreate(NULL);
  int  len=strlen(xml_buffer);
  
  /* This might cause an error on expat 1.95.1 */
  int rc=XML_Parse(xp, xml_buffer, len, 1);

  /* if expat gives an error ... */
  if(!rc) {
    /* then the next line will crash in normal_updatePosition  */
    int line=XML_GetCurrentLineNumber(xp);
  }
  XML_ParserFree(xp);

  return 0;
}
],
            AC_MSG_RESULT(yes),
            AC_DEFINE(EXPAT_UTF8_BOM_CRASH, 1, [does expat crash when it sees an initial UTF8 BOM?])
            AC_MSG_RESULT(no)
            AC_MSG_WARN(The available expat crashes on XML UTF-8 BOM in documents)
            AC_MSG_WARN(Fix either by using libxml or expat 1.95.2+))

  else
    AC_MSG_RESULT(no)
  fi
else
  AC_MSG_RESULT(no)
fi

LIBS="$oLIBS"

AC_ARG_WITH(xml2-config, [  --with-xml2-config=PATH   Location of libxml xml2-config []], xml2_config="$withval", xml2_config="")

if test "X$xml2_config" != "X" ; then
  AC_MSG_CHECKING(for $xml2_config)

  if test -f $xml2_config ; then
    XML_CONFIG=$xml2_config
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no - searching PATH)
  fi
fi
if test "X$XML_CONFIG" = "X"; then
  AC_CHECK_PROGS(XML_CONFIG, xml2-config)
fi

AC_ARG_WITH(xslt-config, [  --with-xslt-config=PATH   Location of libxslt xslt-config []], xslt_config="$withval", xslt_config="")

if test "X$xslt_config" != "X" ; then
  AC_MSG_CHECKING(for $xslt_config)

  if test -f $xslt_config ; then
    XSLT_CONFIG=$xslt_config
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no - searching PATH)
  fi
fi
if test "X$XSLT_CONFIG" = "X"; then
  AC_CHECK_PROGS(XSLT_CONFIG, xslt-config)
fi

AC_ARG_WITH(curl-config, [  --with-curl-config=PATH   Location of libcurl curl-config []], curl_config="$withval", curl_config="")

if test "X$curl_config" != "X" ; then
  AC_MSG_CHECKING(for $curl_config)

  if test -f $curl_config ; then
    CURL_CONFIG=$curl_config
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no - searching PATH)
  fi
fi
if test "X$CURL_CONFIG" = "X"; then
  AC_CHECK_PROGS(CURL_CONFIG, curl-config)
fi


oLIBS="$LIBS"

AC_ARG_ENABLE(nfc-check, [  --disable-nfc-check        Turn off Unicode NFC checking (default enabled).  ], nfc_check="no", nfc_check="yes")	
AC_MSG_CHECKING(using Unicode NFC checking)
AC_MSG_RESULT($nfc_check);


AC_ARG_WITH(www-config, [  --with-libwww-config=PATH Location of W3C libwww libwww-config []], libwww_config="$withval", libwww_config="")

if test "X$libwww_config" != "X" ; then
  AC_MSG_WARN(libwww is no longer supported)
fi


have_libxml=0
have_libxml_lib=0
have_libxml_source=0
need_libxml=0
need_libxml_source=0
AC_MSG_CHECKING(for libxml sources)
if test -d $srcdir/libxml -a -r $srcdir/libxml/libxml.spec ; then
  have_libxml_source=1
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi

oCPPFLAGS="$CPPFLAGS"
oLIBS="$LIBS"
if test "X$XML_CONFIG" != X; then
  LIBS="$LIBS `$XML_CONFIG --libs`"
  AC_CHECK_FUNC(xmlCreatePushParserCtxt, have_xmlCreatePushParserCtxt=yes, have_xmlCreatePushParserCtxt=no)
  AC_MSG_CHECKING(for system (GNOME) libxml library)
  if test $have_xmlCreatePushParserCtxt = yes; then
    have_libxml_lib=1
    have_libxml=1
    CPPFLAGS="`$XML_CONFIG --cflags` $CPPFLAGS"
    LIBXML_VERSION=`$XML_CONFIG --version`
    libxml_version_dec=`echo $LIBXML_VERSION | awk -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
    libxml_min_version_dec=`echo $libxml_min_version | awk -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
    AC_MSG_RESULT(yes - version $LIBXML_VERSION)
    if test $libxml_version_dec -lt $libxml_min_version_dec; then
       AC_MSG_WARN(Using libxml $LIBXML_VERSION is unsupported - $libxml_min_version or newer required.)
       have_libxml_lib=0
       have_libxml=0
    fi
  else
    AC_MSG_RESULT(no)
  fi

  AC_CHECK_HEADERS(libxml/nanohttp.h)
  AC_CHECK_HEADERS(libxml/parser.h)
  AC_CHECK_HEADERS(libxml/hash.h libxml/SAX2.h,,,
  [#ifdef HAVE_LIBXML_PARSER_H
#include <libxml/parser.h>
#endif
])

  if test "$ac_cv_header_libxml_parser_h" = no -a "$ac_cv_header_gnome_xml_parser_h" = no; then
    AC_MSG_WARN(libxml library found but not headers - disabling)
    have_libxml_lib=0
    have_libxml=0
  else
    AC_MSG_CHECKING(if libxml xmlEntity has name_length field)
    AC_TRY_LINK([
#ifdef HAVE_LIBXML_PARSER_H
#include <libxml/parser.h>
#endif
], [xmlEntity foo; foo.name_length=0],
                AC_MSG_RESULT(yes)
		AC_DEFINE(RAPTOR_LIBXML_ENTITY_NAME_LENGTH,1,[does libxml struct xmlEntity have a field name_length]),
		AC_MSG_RESULT(no))

    AC_MSG_CHECKING(if libxml xmlEntity has etype field)
    AC_TRY_LINK([
#ifdef HAVE_LIBXML_PARSER_H
#include <libxml/parser.h>
#endif
], [xmlEntity foo; foo.etype=0],
                AC_MSG_RESULT(yes)
		AC_DEFINE(RAPTOR_LIBXML_ENTITY_ETYPE, 1, [does libxml struct xmlEntity have a field etype]),
		AC_MSG_RESULT(no))

    AC_MSG_CHECKING(if libxml xmlSAXHandler has initialized field)
    AC_TRY_LINK([
#ifdef HAVE_LIBXML_PARSER_H
#include <libxml/parser.h>
#endif
], [xmlSAXHandler foo; foo.initialized=0],
                AC_MSG_RESULT(yes)
		AC_DEFINE(RAPTOR_LIBXML_XMLSAXHANDLER_INITIALIZED, 1, [does libxml xmlSAXHandler have initialized field]),
		AC_MSG_RESULT(no))

    AC_MSG_CHECKING(if libxml xmlSAXHandler has externalSubset field)
    AC_TRY_LINK([
#ifdef HAVE_LIBXML_PARSER_H
#include <libxml/parser.h>
#endif
], [xmlSAXHandler foo; foo.externalSubset=NULL],
                AC_MSG_RESULT(yes)
		AC_DEFINE(RAPTOR_LIBXML_XMLSAXHANDLER_EXTERNALSUBSET, 1, [does libxml xmlSAXHandler have externalSubset field]),
		AC_MSG_RESULT(no))

    AC_CHECK_FUNCS(xmlSAX2InternalSubset xmlCtxtUseOptions)

    AC_MSG_CHECKING(if libxml has parser option XML_PARSE_NONET)
    AC_TRY_LINK([
#ifdef HAVE_LIBXML_PARSER_H
#include <libxml/parser.h>
#endif
], [xmlParserOption foo; foo = XML_PARSE_NONET],
                AC_MSG_RESULT(yes)
		AC_DEFINE(RAPTOR_LIBXML_XML_PARSE_NONET, 1, [does libxml have XML_PARSE_NONET]),
		AC_MSG_RESULT(no))
    AC_CHECK_HEADERS(libxml/HTMLparser.h)
    AC_MSG_CHECKING(if libxml has parser option HTML_PARSE_NONET)
    AC_TRY_LINK([
#ifdef HAVE_LIBXML_HTMLPARSER_H
#include <libxml/HTMLparser.h>
#endif
], [htmlParserOption foo; foo = HTML_PARSE_NONET],
                AC_MSG_RESULT(yes)
		AC_DEFINE(RAPTOR_LIBXML_HTML_PARSE_NONET, 1, [does libxml have HTML_PARSE_NONET]),
		AC_MSG_RESULT(no))
  fi

fi
CPPFLAGS="$oCPPFLAGS"
LIBS="$oLIBS"


have_libxslt=0

oCPPFLAGS="$CPPFLAGS"
oLIBS="$LIBS"
if test "X$XSLT_CONFIG" != X; then
  LIBS="$LIBS `$XSLT_CONFIG --libs`"
  AC_CHECK_FUNC(xsltSaveResultToString, have_xsltSaveResultToString=yes, have_xsltSaveResultToString=no)
  AC_MSG_CHECKING(for system libxslt library)
  if test $have_xsltSaveResultToString = yes; then
    have_libxslt=1
    CPPFLAGS="`$XSLT_CONFIG --cflags` $CPPFLAGS"
    LIBXSLT_VERSION=`$XSLT_CONFIG --version`
    libxslt_version_dec=`echo $LIBXSLT_VERSION | awk -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
    libxslt_min_version_dec=`echo $libxslt_min_version | awk -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
    AC_MSG_RESULT(yes - version $LIBXSLT_VERSION)
    if test $libxslt_version_dec -lt $libxslt_min_version_dec; then
       AC_MSG_WARN(Using libxslt $LIBXSLT_VERSION is unsupported - $libxslt_min_version or newer required.)
       have_libxslt=0
    fi
  else
    AC_MSG_RESULT(no)
  fi

  AC_CHECK_FUNC(xsltInit)

  AC_CHECK_HEADERS(libxslt/xslt.h)
  if test "$ac_cv_header_libxslt_xslt_h" = no ; then
    AC_MSG_WARN(libxslt library found but not headers - disabling)
    have_libxslt_lib=0
    have_libxslt=0
  fi
fi
CPPFLAGS="$oCPPFLAGS"
LIBS="$oLIBS"


AC_ARG_WITH(xml-parser, [  --with-xml-parser=NAME    Use XML parser - libxml (default), expat], xml_parser="$withval", xml_parser="expat") 

for xml_parser_name in $xml_parser libxml expat; do
  case $xml_parser_name in
    expat)
      if test "$expat_source" != "auto"; then
	need_expat=1
	need_expat_source=1
      elif test $have_expat_lib = 1; then
	need_expat=1
      elif test $have_expat_source = 1; then
	need_expat=1
	need_expat_source=1
      fi
      if test $need_expat = 1; then
	AC_DEFINE(RAPTOR_XML_EXPAT, 1, [Use expat XML parser])
        break
      fi
      ;;

    libxml)
      if test $have_libxml_lib = 1; then
	need_libxml=1
      elif test $have_libxml_source = 1; then
	need_libxml=1
	need_libxml_source=1
      fi
      if test $need_libxml = 1; then
	AC_DEFINE(RAPTOR_XML_LIBXML, 1, [Use libxml XML parser])
        break
      fi
      ;;

    *)
      AC_MSG_ERROR(No such XML parser $xml_parser_name)
      ;;
esac
done

AM_CONDITIONAL(RAPTOR_XML_EXPAT, test $need_expat = 1)
AM_CONDITIONAL(RAPTOR_XML_LIBXML, test $need_libxml = 1)


AC_MSG_CHECKING(XML parser to use)
result=
if test $need_libxml = 1; then
  if test $need_libxml_source = 1; then
    result="$result libxml(source)"
  else
    result="$result libxml(system)"
  fi
elif test $need_expat = 1; then
  if test $need_expat_source = 1; then
    result="$result expat(source in $expat_source_dir)"
  else
    result="$result expat(system)"
  fi
else
  AC_MSG_ERROR(No XML parser available - please install expat or libxml)
fi
AC_MSG_RESULT($result)


dnl RDF Parsers
rdfxml_parser=no
ntriples_parser=no
turtle_parser=no
trig_parser=no
n3_parser=no
rss_parser=no
grddl_parser=no
guess_parser=yes
rdfa_parser=yes

rdf_parsers_available="rdfxml ntriples turtle trig guess rss-tag-soup rdfa"
if test "$USE_MAINTAINER_MODE" = yes; then
  rdf_parsers_available="$rdf_parsers_available n3"
fi
rdf_parsers_enabled=


grddl_parser_ok=no
AC_MSG_CHECKING(GRDDL parser requirements)
if test $need_libxml = 1 -a $have_libxslt = 1; then
  AC_MSG_RESULT(yes)
  grddl_parser_ok=yes
  rdf_parsers_available="$rdf_parsers_available grddl"
else
  AC_MSG_RESULT(no - libxml2 and libxslt are both not available)
fi


# This is needed because autoheader can't work out which computed
# symbols must be pulled from acconfig.h into config.h.in
if test "x" = "y"; then
  AC_DEFINE(RAPTOR_PARSER_RDFXML, 1, [Building RDF/XML parser])
  AC_DEFINE(RAPTOR_PARSER_NTRIPLES, 1, [Building N-Triples parser])
  AC_DEFINE(RAPTOR_PARSER_TURTLE, 1, [Building Turtle parser])
  AC_DEFINE(RAPTOR_PARSER_TRIG, 1, [Building TRiG parser])
  AC_DEFINE(RAPTOR_PARSER_N3, 1, [Building Notation 3 parser])
  AC_DEFINE(RAPTOR_PARSER_RSS, 1, [Building RSS Tag Soup parser])
  AC_DEFINE(RAPTOR_PARSER_GRDDL, 1, [Building GRDDL parser])
  AC_DEFINE(RAPTOR_PARSER_GUESS, 1, [Building guess parser])
  AC_DEFINE(RAPTOR_PARSER_RDFA, 1, [Building RDFA parser])
fi

AC_MSG_CHECKING(RDF parsers required)
AC_ARG_ENABLE(parsers, [  --enable-parsers=LIST      Use RDF parsers (default=all)], parsers="$enableval", parsers="rdfxml turtle") 
if test "X$parsers" = Xall -o "X$parsers" = X; then
  parsers="$rdf_parsers_available"
  AC_MSG_RESULT(all)
elif test "X$parsers" = Xnone; then
  parsers=
  AC_MSG_RESULT(none)
else
  AC_MSG_RESULT($parsers)
fi

for parser in $parsers; do
  p=$parser
  if test $p = rss-tag-soup; then
    p=rss
  fi

  if test $p = grddl; then
    if test $grddl_parser_ok != yes; then
      AC_MSG_WARN(GRDDL parser is not available)
      continue
    fi
  fi

  eval $p'_parser=yes'
  NAME=`echo $p | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
  n=RAPTOR_PARSER_${NAME}
  AC_DEFINE_UNQUOTED($n)
  rdf_parsers_enabled="$rdf_parsers_enabled $parser"
done

nfc_needed=no
if test $rdfxml_parser = yes; then
  nfc_needed=yes
fi

need_libxslt=0
if test $grddl_parser = yes; then
  need_libxslt=1
fi

need_librdfa=no
if test $rdfa_parser = yes; then
  need_librdfa=yes
fi

AM_CONDITIONAL(RAPTOR_PARSER_RDFXML, test $rdfxml_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_NTRIPLES, test $ntriples_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_TURTLE, test $turtle_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_TRIG, test $trig_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_N3, test $n3_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_RSS, test $rss_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_GRDDL, test $grddl_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_GUESS, test $guess_parser = yes)
AM_CONDITIONAL(RAPTOR_PARSER_RDFA, test $rdfa_parser = yes)

AM_CONDITIONAL(LIBRDFA, test $need_librdfa = yes)


dnl RDF Serializers
rdfxml_serializer=no
ntriples_serializer=no
rdfxml_abbrev_serializer=no
turtle_serializer=no
rss_1_0_serializer=no
atom_serializer=no
dot_serializer=no
json_serializer=no

rdf_serializers_available="rdfxml rdfxml-abbrev turtle ntriples rss-1.0 dot json atom"

# This is needed because autoheader can't work out which computed
# symbols must be pulled from acconfig.h into config.h.in
if test "x" = "y"; then
  AC_DEFINE(RAPTOR_SERIALIZER_RDFXML, 1, [Building RDF/XML serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_NTRIPLES, 1, [Building N-Triples serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_RDFXML_ABBREV, 1, [Building RDF/XML-abbreviated serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_TURTLE, 1, [Building Turtle serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_RSS_1_0, 1, [Building RSS 1.0 serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_ATOM, 1, [Building Atom 1.0 serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_DOT, 1, [Building GraphViz DOT serializer])
  AC_DEFINE(RAPTOR_SERIALIZER_JSON, 1, [Building JSON serializer])
fi

AC_MSG_CHECKING(RDF serializers required)
AC_ARG_ENABLE(serializers, [  --enable-serializers=LIST  Use RDF serializers (default=all)], serializers="$enableval", serializers="ntriples") 
if test "X$serializers" = Xall -o "X$serializers" = X; then
  serializers="$rdf_serializers_available"
  AC_MSG_RESULT(all)
elif test "X$serializers" = Xnone; then
  serializers=
  AC_MSG_RESULT(none)
else
  AC_MSG_RESULT($serializers)
fi

for serializer in $serializers; do
  s=`echo $serializer | tr '.-' '__'`
  eval $s'_serializer=yes'
  NAME=`echo $s | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
  n=RAPTOR_SERIALIZER_${NAME}
  AC_DEFINE_UNQUOTED($n)
  rdf_serializers_enabled="$rdf_serializers_enabled $serializer"
done

AM_CONDITIONAL(RAPTOR_SERIALIZER_RDFXML, test $rdfxml_serializer = yes)
AM_CONDITIONAL(RAPTOR_SERIALIZER_NTRIPLES, test $ntriples_serializer = yes)
AM_CONDITIONAL(RAPTOR_SERIALIZER_RDFXML_ABBREV, test $rdfxml_abbrev_serializer = yes)
AM_CONDITIONAL(RAPTOR_SERIALIZER_TURTLE, test $turtle_serializer = yes)
AM_CONDITIONAL(RAPTOR_SERIALIZER_RSS_1_0, test $rss_1_0_serializer = yes)
AM_CONDITIONAL(RAPTOR_SERIALIZER_DOT, test $dot_serializer = yes)
AM_CONDITIONAL(RAPTOR_SERIALIZER_JSON, test $json_serializer = yes)

AM_CONDITIONAL(RAPTOR_RSS_COMMON, test $rss_1_0_serializer = yes -o $rss_parser = yes)

dnl Enable NFC code only if needed and the check is enabled
if test $nfc_needed = yes; then
  if test $nfc_check != yes; then
    nfc_needed=no
  else
    AC_DEFINE([RAPTOR_NFC_CHECK], 1, [Provide a Unicode NFC check])
  fi
fi
AM_CONDITIONAL(RAPTOR_NFC_CHECK, test $nfc_needed = yes)



AC_MSG_CHECKING(XML names version)
AC_ARG_WITH(xml-names, [  --with-xml-names=1.1|1.0  Select XML version name checking (default=1.0)], xml_names="$withval", xml_names="1.0")
if test $xml_names = 1.1; then
  AC_DEFINE(RAPTOR_XML_1_1, 1, [Check XML 1.1 Names])
fi
AC_MSG_RESULT($xml_names)


have_libcurl=0
have_libfetch=0
need_libcurl=0
need_libxml_www=0
need_libfetch=0

oCPPFLAGS="$CPPFLAGS"
oLIBS="$LIBS"
if test "X$CURL_CONFIG" != X; then
  CPPFLAGS="$CPPFLAGS `$CURL_CONFIG --cflags`"
  LIBS="$LIBS `$CURL_CONFIG --libs`"
  AC_CHECK_HEADER(curl/curl.h)
  AC_CHECK_FUNC(curl_easy_init, have_curl_easy_init=yes, have_curl_easy_init=no)
  LIBS="$oLIBS"
  CPPFLAGS="$oCPPFLAGS"
  AC_MSG_CHECKING(for libcurl library)
  if test $have_curl_easy_init = yes -a "$ac_cv_header_curl_curl_h" = yes; then
    have_libcurl=1
    LIBCURL_VERSION=`$CURL_CONFIG --version | sed -e 's/^libcurl *//'`
    AC_MSG_RESULT(yes - version $LIBCURL_VERSION)
  else
    AC_MSG_RESULT(no)
  fi
fi


if test "X$ac_cv_header_curl_curl_h" = Xyes; then
  AC_DEFINE([HAVE_CURL_CURL_H], 1, [Have curl/curl.h])
fi

if test $ac_cv_header_fetch_h = yes; then
  oLIBS="$LIBS"
  AC_CHECK_LIB(fetch, fetchXGetURL, have_libfetch=1)
  LIBS="$oLIBS"
fi


AC_ARG_WITH(www, [  --with-www=NAME           Use WWW library - curl, xml, libfetch, none (default)], www="$withval", www="none") 

for www_name in $www curl xml libfetch none; do
  case $www_name in
    curl)
      if test $have_libcurl = 1; then
        need_libcurl=1
        AC_DEFINE([RAPTOR_WWW_LIBCURL], 1, [Have libcurl WWW library])
        break
      fi
      ;;

    xml)
      if test $have_libxml = 1; then
        need_libxml=1
        need_libxml_www=1
	AC_DEFINE([RAPTOR_WWW_LIBXML], 1, [Have libxml available as a WWW library])
        break
      fi
      ;;

    libfetch)
      if test $have_libfetch = 1; then
        need_libfetch=1
	AC_DEFINE([RAPTOR_WWW_LIBFETCH], 1, [Have libfetch WWW library])
        break
      fi
      ;;

    none)
      need_libcurl=0
      need_libxml_www=0
      AC_DEFINE([RAPTOR_WWW_NONE], 1, [No WWW library])
      break
      ;;

    *)
      AC_MSG_ERROR(No such WWW library $www_name)
      ;;
esac
done

AC_MSG_CHECKING(WWW libraries available)
www_libraries_available=
if test $have_libcurl = 1; then
  www_libraries_available="$www_libraries_available libcurl $LIBCURL_VERSION"
fi
if test $have_libxml = 1; then
  if test $need_libxml_source = 1; then
    www_libraries_available="$www_libraries_available libxml(source)"
  else
    www_libraries_available="$www_libraries_available libxml(system $LIBXML_VERSION)"
  fi
fi
if test $have_libfetch = 1; then
  www_libraries_available="$www_libraries_available libfetch"
fi
AC_MSG_RESULT($www_libraries_available)


AC_MSG_CHECKING(WWW library to use)
www_library=
RAPTOR_WWW_LIBRARY=none
if test $need_libcurl = 1; then
  www_library="libcurl $LIBCURL_VERSION"
  RAPTOR_WWW_LIBRARY=libcurl
elif test $need_libxml_www = 1; then
  if test $need_libxml_source = 1; then
    www_library="libxml(source)"
  else
    www_library="libxml(system $LIBXML_VERSION)"
  fi
  RAPTOR_WWW_LIBRARY=libxml
elif test $need_libfetch = 1; then
  www_library="libfetch"
  RAPTOR_WWW_LIBRARY=libfetch
else
  www_library=none
fi
AC_MSG_RESULT($www_library)
if test "X$www_library" = Xnone; then
  AC_MSG_WARN([No WWW library in use - only file: URLs will work])
  AC_MSG_WARN([Install libcurl, libxml2 or BSD libfetch for WWW access])
fi

if test $need_libcurl = 1; then
  CPPFLAGS="$CPPFLAGS `$CURL_CONFIG --cflags`"
  LIBS="$LIBS `$CURL_CONFIG --libs`"
  AC_LIBOBJ(raptor_www_curl)

fi


have_lininn=no
have_inn_parsedate=no

oCPPFLAGS="$CPPFLAGS"
if test -d /usr/include/inn; then
  CPPFLAGS="$CPPFLAGS -I/usr/include/inn"
fi
AC_CHECK_HEADER(libinn.h)
CPPFLAGS="$oCPPFLAGS"

oCPPFLAGS="$CPPFLAGS"
oLIBS="$LIBS"
if test $ac_cv_header_libinn_h = yes; then
  CPPFLAGS="$CPPFLAGS -I/usr/include/inn"
  LIBS="$LIBS -L/usr/lib/news -linn"
  AC_CHECK_LIB(inn, HashMessageID, have_libinn=yes)

  AC_MSG_CHECKING(parsedate in libinn)
  AC_TRY_LINK([
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#include <libinn.h>
], [ parsedate("Sun Jun 12 00:04:09 BST 2005", NULL) ],
    AC_DEFINE(HAVE_INN_PARSEDATE, 1, [INN parsedate function present])
    have_inn_parsedate=yes
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(no))
fi
CPPFLAGS="$oCPPFLAGS"
LIBS="$oLIBS"


AC_MSG_CHECKING(date parsing source)
raptor_parsedate_needed=no
if test $have_inn_parsedate = yes; then
  CPPFLAGS="$CPPFLAGS -I/usr/include/inn"
  LIBS="$LIBS -L/usr/lib/news -linn"
  AC_MSG_RESULT(INN parsedate)
else
  if test $need_libcurl = 1; then
    AC_MSG_RESULT(libcurl curl_getdate)
  else
    raptor_parsedate_needed=yes
    AC_MSG_RESULT(raptor parsedate)
  fi
fi

AM_CONDITIONAL(PARSEDATE, test $raptor_parsedate_needed = yes)
if test $raptor_parsedate_needed = yes; then
  AC_DEFINE([HAVE_RAPTOR_PARSE_DATE], 1, [Raptor raptor_parse_date available])
fi


if test $need_libfetch = 1; then
  LIBS="$LIBS -lfetch"
  AC_LIBOBJ(raptor_www_libfetch)
fi


RAPTOR_XML_PARSER=none
if test $need_libxml = 1; then

  if test $need_libxml_www = 1; then
    AC_LIBOBJ(raptor_www_libxml)
  fi
  if test $need_libxml_source = 1; then
    SD="$SD libxml"
    (cd libxml && ./configure --cache=../config.cache --enable-shared=no)
    CPPFLAGS="-I$srcdir/libxml $CPPFLAGS"
    LIBS="$LIBS -Llibxml -llibxml"
  else
    LIBS="$LIBS `$XML_CONFIG --libs`"
    CPPFLAGS="`$XML_CONFIG --cflags` $CPPFLAGS"
  fi
  RAPTOR_XML_PARSER=libxml
fi


if test $need_expat = 1; then
  if test $need_expat_source = 1; then
    # Only build local copy if it is needed
    if test "X$expat_source" = local; then
      SD="$SD expat"
    fi
    if test -d "$expat_source_dir/xmlparse"; then
      # old expat
      LIBS="$LIBS $expat_obj_dir/xmlparse/xmlparse.o $expat_obj_dir/xmlparse/hashtable.o $expat_obj_dir/xmltok/xmlrole.o $expat_obj_dir/xmltok/xmltok.o"
    else
      # new expat
      LIBS="$LIBS $expat_obj_dir/lib/xmlparse.o $expat_obj_dir/lib/xmlrole.o $expat_obj_dir/lib/xmltok.o"
    fi
  else
    LIBS="$LIBS $expat_libs"
  fi
  RAPTOR_XML_PARSER=expat
fi

if test $need_libxslt = 1; then
  LIBS="$LIBS `$XSLT_CONFIG --libs`"
  CPPFLAGS="`$XSLT_CONFIG --cflags` $CPPFLAGS"
fi

RAPTOR_LIBTOOLLIBS=libraptor.la
AC_SUBST(RAPTOR_LIBTOOLLIBS)


xml_parsers_available=
if test $need_libxml = 1; then
  if test $need_libxml_source = 1; then
    xml_parsers_available="$xml_parsers_available libxml(source)"
  else
    xml_parsers_available="$xml_parsers_available libxml(system $LIBXML_VERSION)"
  fi
fi
if test $need_expat = 1; then
  if test $need_expat_source = 1; then
    xml_parsers_available="$xml_parsers_available expat(source in $expat_source_dir)"
  else
    if test $libexpat = 1; then
      xml_parsers_available="$xml_parsers_available expat(system libexpat)"
    else
      xml_parsers_available="$xml_parsers_available expat(system libxmlparse,libxmltok)"
    fi
  fi
fi


# Make final changes to cflags
MEM=
MEM_LIBS=
CPPFLAGS="-DRAPTOR_INTERNAL=1 $CPPFLAGS"
AC_ARG_WITH(dmalloc, [  --with-dmalloc            Use dmalloc debugging library (default=no)], use_dmalloc="$withval", use_dmalloc="no") 
AC_MSG_CHECKING(using dmalloc library)
if test "$USE_MAINTAINER_MODE" = yes; then
  if test "$ac_cv_header_dmalloc_h" = yes; then
    if test "X$use_dmalloc" = Xauto; then
      use_dmalloc=yes
    fi
  else
    use_dmalloc=no
  fi
  CPPFLAGS="-g -DRAPTOR_DEBUG=1 $CPPFLAGS"
else
  use_dmalloc=no
fi
AC_MSG_RESULT($use_dmalloc);
if test $use_dmalloc = yes; then
  MEM=-DRAPTOR_MEMORY_DEBUG_DMALLOC=1
  MEM_LIBS=-ldmalloc
fi
AC_ARG_WITH(memory-signing, [  --with-memory-signing     Sign allocated memory (default=no)], use_memory_signing="$withval", use_memory_signing="no") 
AC_MSG_CHECKING(using memory signing)
AC_MSG_RESULT($use_memory_signing);
if test $use_memory_signing = yes; then
  MEM=-DRAPTOR_MEMORY_SIGN=1
  MEM_LIBS=
fi

STANDARD_CFLAGS="$STANDARD_CFLAGS $CFLAGS"
if test "$USE_MAINTAINER_MODE" = yes; then
  CFLAGS="$MAINTAINER_CFLAGS $CFLAGS"
fi

AC_SUBST(MEM)
AC_SUBST(MEM_LIBS)
AC_SUBST(STANDARD_CFLAGS)

ECHO_N="$ECHO_N"
ECHO_C="$ECHO_C"
AC_SUBST(ECHO_N)
AC_SUBST(ECHO_C)

# Features
# lists
RAPTOR_PARSERS=$rdf_parsers_enabled
AC_SUBST(RAPTOR_PARSERS)
RAPTOR_SERIALIZERS=$rdf_serializers_enabled
AC_SUBST(RAPTOR_SERIALIZERS)
# single values or none
AC_SUBST(RAPTOR_WWW_LIBRARY)
AC_SUBST(RAPTOR_XML_PARSER)

abs_top_srcdir=`cd $srcdir; pwd`
AC_SUBST(abs_top_srcdir)
abs_top_builddir=`pwd`
AC_SUBST(abs_top_builddir)

AC_CONFIG_FILES([Makefile
raptor.spec
raptor.rdf
data/Makefile
docs/Makefile
docs/version.xml
examples/Makefile
src/Makefile
tests/Makefile
tests/grddl/Makefile
tests/rdfa/Makefile
tests/turtle/Makefile
tests/trig/Makefile
utils/Makefile
win32/Makefile
librdfa/Makefile
raptor.pc])
AC_CONFIG_FILES([src/raptor-config], [chmod +x src/raptor-config])
AC_CONFIG_FILES([raptor-src-config], [chmod +x raptor-src-config])

dnl Check for gtk-doc and docbook
GTK_DOC_CHECK([1.3])

AC_OUTPUT

AC_MSG_RESULT([
Raptor build summary:
  RDF parsers available     : $rdf_parsers_available
  RDF parsers enabled       :$rdf_parsers_enabled
  RDF serializers available : $rdf_serializers_available
  RDF serializers enabled   :$rdf_serializers_enabled
  XML parser                :$xml_parsers_available
  WWW library               : $www_library
])
