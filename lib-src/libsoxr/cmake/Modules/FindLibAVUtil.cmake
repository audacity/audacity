# SoX Resampler Library       Copyright (c) 2007-18 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Find AVUTIL
# Find the installation of this package: includes and libraries.
#
#  AVUTIL_INCLUDE_DIRS - where to find headers for this package.
#  AVUTIL_LIBRARIES    - libraries to link to when using this package.
#  AVUTIL_FOUND        - true iff this package can be found.

if (AVUTIL_INCLUDE_DIRS)
  set (AVUTIL_FIND_QUIETLY TRUE)
endif ()

find_path (AVUTIL_INCLUDE_DIRS libavutil/cpu.h)

find_library (AVUTIL_LIBRARIES NAMES avutil)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
  AVUTIL DEFAULT_MSG AVUTIL_LIBRARIES AVUTIL_INCLUDE_DIRS)

mark_as_advanced (AVUTIL_LIBRARIES AVUTIL_INCLUDE_DIRS)
