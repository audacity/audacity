#!/usr/bin/env bash
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

echo "Setup ccache config"

export CCACHE_DIR=$GITHUB_WORKSPACE/.ccache
mkdir -p $CCACHE_DIR

echo "CCACHE_DIR=$CCACHE_DIR" | tee -a $GITHUB_ENV
echo "base_dir = $GITHUB_WORKSPACE" >$CCACHE_DIR/ccache.conf
echo "compression = true" >>$CCACHE_DIR/ccache.conf
echo "compression_level = 5" >>$CCACHE_DIR/ccache.conf
echo "max_size = 2G" >>$CCACHE_DIR/ccache.conf
echo "sloppiness=pch_defines,time_macros" >>$CCACHE_DIR/ccache.conf
cat $CCACHE_DIR/ccache.conf

ccache -sv
ccache -z
