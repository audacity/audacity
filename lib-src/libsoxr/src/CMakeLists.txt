# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

add_definitions (${PROJECT_DEFS} -DSOXR_LIB)

set (RDFT32 fft4g32)
if (WITH_AVFFT AND AVCODEC_FOUND)
  set (RDFT32S avfft32s)
elseif (WITH_PFFFT)
  set (RDFT32S pffft32s)
elseif (WITH_SIMD)
  set (RDFT32S fft4g32s)
endif ()

if (WITH_DOUBLE_PRECISION)
  set (DP_SOURCES rate64)
endif ()

if (WITH_SINGLE_PRECISION)
  set (SP_SOURCES rate32 ${RDFT32})
endif ()

if (HAVE_SIMD)
  set (SIMD_SOURCES rate32s ${RDFT32S} simd)
  foreach (source ${SIMD_SOURCES})
    set_property(SOURCE ${source} PROPERTY COMPILE_FLAGS ${SIMD_C_FLAGS})
  endforeach ()
endif ()

add_library(${PROJECT_NAME} ${LIB_TYPE} ${PROJECT_NAME} data-io dbesi0 filter fft4g64
  ${SP_SOURCES} ${DP_SOURCES} ${SIMD_SOURCES})
set_target_properties (${PROJECT_NAME} PROPERTIES SOVERSION "${VERSION}")
list (APPEND TARGET_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.h")


if (WITH_LSR_IF)
  set (LSR samplerate)
  set (LSR ${PROJECT_NAME}-lsr)
  add_library(${LSR} ${LIB_TYPE} lsr)
  target_link_libraries (${LSR} ${PROJECT_NAME})
  set_target_properties (${LSR} PROPERTIES SOVERSION ${VERSION})
  list (APPEND TARGET_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/${LSR}.h")
endif ()

install (TARGETS ${PROJECT_NAME} ${LSR} DESTINATION lib)
install (FILES ${TARGET_HEADERS} DESTINATION include)



# Packaging

get_property(LIB1 TARGET ${PROJECT_NAME} PROPERTY LOCATION)
if (BUILD_SHARED_LIBS)
  set (LIB1 ${LIB1}.${VERSION})
endif ()
if (WITH_LSR_IF)
  get_property(LIB2 TARGET ${LSR} PROPERTY LOCATION)
  if (BUILD_SHARED_LIBS)
    set (LIB2 ${LIB2}.${VERSION})
  endif ()
endif ()
set (TARGET_LIBS ${LIB1} ${LIB2})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/libsoxr.src ${CMAKE_CURRENT_BINARY_DIR}/libsoxr.src)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/libsoxr-dev.src ${CMAKE_CURRENT_BINARY_DIR}/libsoxr-dev.src)
