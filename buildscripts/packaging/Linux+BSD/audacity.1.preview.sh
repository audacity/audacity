#!/bin/sh
#
# Audacity: A Digital Audio Editor
#

cd "$(dirname "$0")"

case $1 in
(portable|p)
	portable=''
	;;
(*)
	portable='.\\"'
	;;
esac

sed \
    -e 's!@MAN_AUDACITY_UPPER@!AUDACITY!g' \
    -e 's!@Variables_substituted_by_CMAKE_on_installation@!!g' \
    -e 's!@MUSE_APP_INSTALL_SUFFIX@!!g' \
    -e 's!@MUSE_APP_NAME_VERSION@!Audacity 4!g' \
    -e 's!@MAN_PORTABLE@!'"$portable"'!g' \
    -e 's!@PORTABLE_INSTALL_NAME@!AudacityPortable!g' \
    -e 's!@CMAKE_INSTALL_PREFIX@!/usr!g' \
    -e 's!@Audacity_SHARE_NAME@!share/!g' \
    -e 's!@Audacity_INSTALL_NAME@!audacity-4.0/!g' \
    <audacity.1.in | man -l -
