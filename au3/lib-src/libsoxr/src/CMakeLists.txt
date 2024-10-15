# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.



# Can generate vr-coefs.h but it complicates cross-compiling & non-cmake builds

if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/vr-coefs.h)
  include_directories(${CMAKE_CURRENT_BINARY_DIR})
  set_property(SOURCE vr32.c
      APPEND PROPERTY OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/vr-coefs.h)
  add_executable (vr-coefs vr-coefs.c)
  target_link_libraries (vr-coefs ${LIBM_LIBRARIES})
  ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/vr-coefs.h
    COMMAND vr-coefs > ${CMAKE_CURRENT_BINARY_DIR}/vr-coefs.h
    DEPENDS vr-coefs)
endif ()



add_definitions (${PROJECT_C_FLAGS} -DSOXR_LIB)



# Libsoxr configuration:

set (RDFT32 fft4g32)
if (AVCODEC_FOUND)
  set (RDFT32 avfft32)
  set (RDFT32S avfft32s)
elseif (WITH_PFFFT)
  #set (RDFT32 pffft32)
  set (RDFT32S pffft32s)
elseif (WITH_CR32S)
  set (RDFT32S fft4g32s)
  if (NOT WITH_CR32)
    list (APPEND RDFT32S fft4g32)
  endif ()
endif ()

set (SOURCES ${PROJECT_NAME}.c data-io)

if (WITH_CR32 OR WITH_CR32S OR WITH_CR64 OR WITH_CR64S)
  list (APPEND SOURCES dbesi0 filter fft4g64 cr)
endif ()

if (WITH_CR32)
  list (APPEND SOURCES cr32 ${RDFT32})
endif ()

if (WITH_CR64)
  list (APPEND SOURCES cr64)
endif ()

if (WITH_VR32)
  list (APPEND SOURCES vr32)
endif ()

if (WITH_CR32S)
  foreach (source cr32s ${RDFT32S} util32s)
    list (APPEND SOURCES ${source})
    set_property (SOURCE ${source}
        APPEND_STRING PROPERTY COMPILE_FLAGS ${SIMD32_C_FLAGS})
  endforeach ()
endif ()

if (WITH_CR64S)
  foreach (source cr64s pffft64s util64s)
    list (APPEND SOURCES ${source})
    set_property (SOURCE ${source}
        APPEND_STRING PROPERTY COMPILE_FLAGS ${SIMD64_C_FLAGS})
  endforeach ()
endif ()



# Libsoxr:

add_library (${PROJECT_NAME} ${LIB_TYPE} ${SOURCES})
target_link_libraries (${PROJECT_NAME} PRIVATE ${LIBS} ${LIBM_LIBRARIES})
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
  add_library (${LSR} ${LIB_TYPE} ${LSR})
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
