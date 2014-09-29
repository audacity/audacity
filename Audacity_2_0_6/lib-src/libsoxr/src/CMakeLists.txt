# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

add_definitions (${PROJECT_C_FLAGS} -DSOXR_LIB)



# Libsoxr configuration:

set (RDFT32 fft4g32)
if (WITH_AVFFT AND AVCODEC_FOUND)
  set (RDFT32 avfft32)
  set (RDFT32S avfft32s)
elseif (WITH_PFFFT)
  #set (RDFT32 pffft32)
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
    set_property (SOURCE ${source} PROPERTY COMPILE_FLAGS ${SIMD_C_FLAGS})
  endforeach ()
endif ()



# Libsoxr:

add_library (${PROJECT_NAME} ${LIB_TYPE} ${PROJECT_NAME}.c data-io dbesi0 filter fft4g64
  ${SP_SOURCES} vr32 ${DP_SOURCES} ${SIMD_SOURCES})
set_target_properties (${PROJECT_NAME} PROPERTIES
  VERSION "${SO_VERSION}"
  SOVERSION ${SO_VERSION_MAJOR}
  INSTALL_NAME_DIR ${LIB_INSTALL_DIR}
  LINK_INTERFACE_LIBRARIES ""
  PUBLIC_HEADER "${PROJECT_NAME}.h")
if (BUILD_FRAMEWORK)
  set_target_properties (${PROJECT_NAME} PROPERTIES FRAMEWORK TRUE)
elseif (NOT WIN32)
  set (TARGET_PCS ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc)
  configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.pc.in ${TARGET_PCS})
  install (FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc DESTINATION ${LIB_INSTALL_DIR}/pkgconfig)
endif ()



# LSR bindings:

if (WITH_LSR_BINDINGS)
  set (LSR ${PROJECT_NAME}-lsr)
  set (LSR_SO_VERSION 0.1.9)
  set (LSR_SO_VERSION_MAJOR 0)
  add_library (${LSR} ${LIB_TYPE} lsr)
  target_link_libraries (${LSR} ${PROJECT_NAME})
  set_target_properties (${LSR} PROPERTIES
    VERSION "${LSR_SO_VERSION}"
    SOVERSION ${LSR_SO_VERSION_MAJOR}
    INSTALL_NAME_DIR ${LIB_INSTALL_DIR}
    LINK_INTERFACE_LIBRARIES ""
    PUBLIC_HEADER "${LSR}.h")
  if (BUILD_FRAMEWORK)
    set_target_properties (${LSR} PROPERTIES FRAMEWORK TRUE)
  elseif (NOT WIN32)
    set (TARGET_PCS "${TARGET_PCS} ${CMAKE_CURRENT_BINARY_DIR}/${LSR}.pc")
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${LSR}.pc.in ${CMAKE_CURRENT_BINARY_DIR}/${LSR}.pc)
    install (FILES ${CMAKE_CURRENT_BINARY_DIR}/${LSR}.pc DESTINATION ${LIB_INSTALL_DIR}/pkgconfig)
  endif ()
endif ()



# Installation (from build from source):

install (TARGETS ${PROJECT_NAME} ${LSR}
  FRAMEWORK DESTINATION ${FRAMEWORK_INSTALL_DIR}
  LIBRARY DESTINATION ${LIB_INSTALL_DIR}
  RUNTIME DESTINATION ${BIN_INSTALL_DIR}
  ARCHIVE DESTINATION ${LIB_INSTALL_DIR}
  PUBLIC_HEADER DESTINATION ${INCLUDE_INSTALL_DIR})



# Packaging (for unix-like distributions):

get_property (LIB1 TARGET ${PROJECT_NAME} PROPERTY LOCATION)
if (BUILD_SHARED_LIBS)
  set (LIB1 ${LIB1}.${SO_VERSION_MAJOR} ${LIB1}.${SO_VERSION})
endif ()
list (APPEND TARGET_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.h")
if (WITH_LSR_BINDINGS)
  get_property (LIB2 TARGET ${LSR} PROPERTY LOCATION)
  if (BUILD_SHARED_LIBS)
    set (LIB2 ${LIB2}.${LSR_SO_VERSION_MAJOR} ${LIB2}.${LSR_SO_VERSION})
  endif ()
  list (APPEND TARGET_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/${LSR}.h")
endif ()
set (TARGET_LIBS ${LIB1} ${LIB2})
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/libsoxr.src.in ${CMAKE_CURRENT_BINARY_DIR}/libsoxr.src)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/libsoxr-dev.src.in ${CMAKE_CURRENT_BINARY_DIR}/libsoxr-dev.src)
