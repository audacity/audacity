# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Find AVCODEC
# Find the native installation of this package: includes and libraries.
#
#  AVCODEC_INCLUDES    - where to find headers for this package.
#  AVCODEC_LIBRARIES   - List of libraries when using this package.
#  AVCODEC_FOUND       - True if this package can be found.

if (AVCODEC_INCLUDES)
  set (AVCODEC_FIND_QUIETLY TRUE)
endif (AVCODEC_INCLUDES)

find_path (AVCODEC_INCLUDES libavcodec/avcodec.h)

find_library (AVCODEC_LIBRARIES NAMES avcodec)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
  AVCODEC DEFAULT_MSG AVCODEC_LIBRARIES AVCODEC_INCLUDES)

mark_as_advanced (AVCODEC_LIBRARIES AVCODEC_INCLUDES)
