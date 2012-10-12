# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

list (APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

find_package (FFTW)
if (FFTW_FOUND)
  include_directories (${FFTW_INCLUDE_DIRS})
  link_libraries (${FFTW_LIBRARIES})
  set (HAVE_FFTW3 1)
endif ()

find_package (sndfile)
if (SNDFILE_FOUND)
  include_directories (${SNDFILE_INCLUDE_DIRS})
  link_libraries (${SNDFILE_LIBRARIES})
  set (HAVE_SNDFILE 1)
endif ()

check_function_exists (lrintf HAVE_LRINTF)
check_function_exists (alarm HAVE_ALARM)
check_function_exists (signal HAVE_SIGNAL)
check_include_files (sys/times.h HAVE_SYS_TIMES_H)
make_exist (HAVE_LRINTF HAVE_ALARM HAVE_SIGNAL HAVE_SYS_TIMES_H)
make_exist (HAVE_FFTW HAVE_SNDFILE)

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (tests_lib SHARED util calc_snr)

link_libraries (tests_lib ${PROJECT_NAME}-lsr)

enable_testing ()

set (tests
  callback_hang_test callback_test downsample_test
  float_short_test misc_test multi_channel_test
  reset_test simple_test snr_bw_test termination_test varispeed_test)

foreach (test ${tests})
  add_executable (${test} ${test})
  add_test (lsr-${test} ${BIN}${test})
  set_property (TEST lsr-${test} PROPERTY ENVIRONMENT "SOXR_LSR_STRICT=1")
endforeach ()

add_executable (multichan_throughput_test multichan_throughput_test)
add_executable (throughput_test throughput_test )
add_executable (sndfile-resample sndfile-resample)
