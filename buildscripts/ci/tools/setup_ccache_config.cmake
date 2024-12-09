# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
# **********************************************************************

message(STATUS "Setup ccache config")

set(CCACHE_DIR "$ENV{GITHUB_WORKSPACE}/.ccache")

file(MAKE_DIRECTORY ${CCACHE_DIR})

file(WRITE ${CCACHE_DIR}/ccache.conf "base_dir = $ENV{GITHUB_WORKSPACE}\n")
file(APPEND ${CCACHE_DIR}/ccache.conf "compression = true\n")
file(APPEND ${CCACHE_DIR}/ccache.conf "compression_level = 5\n")
file(APPEND ${CCACHE_DIR}/ccache.conf "max_size = 2G\n")
file(APPEND ${CCACHE_DIR}/ccache.conf "sloppiness=pch_defines,time_macros\n")

execute_process(COMMAND ccache -s)
execute_process(COMMAND ccache -z)
