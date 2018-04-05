#!/bin/sh
#
# The purpose of this script is to give the user more control over where libraries
# such as Lame and FFmpeg get loaded from.
#
# Since absolute pathnames are used when loading these libraries, the normal search
# path would be DYLD_LIBRARY_PATH, absolute path, DYLD_FALLBACK_LIBRARY_PATH.  This
# means that DYLD_LIBRARY_PATH can override what the user actually wants.
#
# So, we simply clear DYLD_LIBRARY_PATH to allow the users choice to be the first
# one tried.
#

DYLD_FALLBACK_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$DYLD_FALLBACK_LIBRARY_PATH:$HOME/lib:/usr/local/lib:/usr/lib"
export DYLD_FALLBACK_LIBRARY_PATH

DYLD_LIBRARY_PATH=""
export DYLD_LIBRARY_PATH

dir=$(dirname "$0")
exec "$dir/Audacity"
