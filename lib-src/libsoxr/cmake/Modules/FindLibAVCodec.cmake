# SoX Resampler Library       Copyright (c) 2007-18 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Find AVCODEC
# Find the installation of this package: include-dirs and libraries.
#
#  AVCODEC_INCLUDE_DIRS - where to find headers for this package.
#  AVCODEC_LIBRARIES    - libraries to link to when using this package.
#  AVCODEC_FOUND        - true iff this package can be found.

if (AVCODEC_INCLUDE_DIRS)
  set (AVCODEC_FIND_QUIETLY TRUE)
endif ()

find_path (AVCODEC_INCLUDE_DIRS libavcodec/avcodec.h)

find_library (AVCODEC_LIBRARIES NAMES avcodec)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
  AVCODEC DEFAULT_MSG AVCODEC_LIBRARIES AVCODEC_INCLUDE_DIRS)

mark_as_advanced (AVCODEC_LIBRARIES AVCODEC_INCLUDE_DIRS)
