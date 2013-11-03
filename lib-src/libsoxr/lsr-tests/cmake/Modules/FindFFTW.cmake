# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Find FFTW
# Find the native installation of this package: includes and libraries.
#
#  FFTW_INCLUDES    - where to find headers for this package.
#  FFTW_LIBRARIES   - List of libraries when using this package.
#  FFTW_FOUND       - True if this package can be found.

if (FFTW_INCLUDES)
  set (FFTW_FIND_QUIETLY TRUE)
endif (FFTW_INCLUDES)

find_path (FFTW_INCLUDES fftw3.h)

find_library (FFTW_LIBRARIES NAMES fftw3)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
  FFTW DEFAULT_MSG FFTW_LIBRARIES FFTW_INCLUDES)

mark_as_advanced (FFTW_LIBRARIES FFTW_INCLUDES)
