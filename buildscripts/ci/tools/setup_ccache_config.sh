#!/usr/bin/env bash
# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
# **********************************************************************

echo "Setup ccache config"

export CCACHE_DIR=$GITHUB_WORKSPACE/.ccache
mkdir -p $CCACHE_DIR

echo "CCACHE_DIR=$CCACHE_DIR" | tee -a $GITHUB_ENV
echo "base_dir = $GITHUB_WORKSPACE" >$CCACHE_DIR/ccache.conf
echo "max_size = 1G" >>$CCACHE_DIR/ccache.conf
echo "sloppiness=pch_defines,time_macros" >>$CCACHE_DIR/ccache.conf
cat $CCACHE_DIR/ccache.conf

ccache -sv
ccache -z
