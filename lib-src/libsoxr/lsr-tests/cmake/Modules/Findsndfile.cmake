# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Find SNDFILE
# Find the native installation of this package: includes and libraries.
#
#  SNDFILE_INCLUDES    - where to find headers for this package.
#  SNDFILE_LIBRARIES   - List of libraries when using this package.
#  SNDFILE_FOUND       - True if this package can be found.

if (SNDFILE_INCLUDES)
  set (SNDFILE_FIND_QUIETLY TRUE)
endif (SNDFILE_INCLUDES)

find_path (SNDFILE_INCLUDES sndfile.h)

find_library (SNDFILE_LIBRARIES NAMES sndfile)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
  SNDFILE DEFAULT_MSG SNDFILE_LIBRARIES SNDFILE_INCLUDES)

mark_as_advanced (SNDFILE_LIBRARIES SNDFILE_INCLUDES)
