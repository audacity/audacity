#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-Studio-CLA-applies
#
# MuseScore Studio
# Music Composition & Notation
#
# Copyright (C) 2021 MuseScore Limited
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
    -e 's!@MAN_MSCORE_UPPER@!MSCORE!g' \
    -e 's!@Variables_substituted_by_CMAKE_on_installation@!!g' \
    -e 's!@MUSE_APP_INSTALL_SUFFIX@!!g' \
    -e 's!@MUSE_APP_NAME_VERSION@!MuseScore 4!g' \
    -e 's!@MAN_PORTABLE@!'"$portable"'!g' \
    -e 's!@PORTABLE_INSTALL_NAME@!MuseScorePortable!g' \
    -e 's!@CMAKE_INSTALL_PREFIX@!/usr!g' \
    -e 's!@Mscore_SHARE_NAME@!share/!g' \
    -e 's!@Mscore_INSTALL_NAME@!mscore-4.0/!g' \
    <mscore.1.in | man -l -
