include(ExternalProject)

def_vars()

set( CURL_DIR "${_INTDIR}/libcurl" )
set( CURL_TAG "curl-7_76_0")

set(CURL_CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CURL_DIR} -DHTTP_ONLY=On -DBUILD_CURL_EXE=Off)

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set(CURL_CMAKE_ARGS ${CURL_CMAKE_ARGS} -DCMAKE_USE_SCHANNEL=On)
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set(CURL_CMAKE_ARGS ${CURL_CMAKE_ARGS} -DCMAKE_USE_SECTRANSP=On -DCMAKE_OSX_ARCHITECTURES=x86_64 -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9)
elseif( CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD" )

endif()

ExternalProject_Add(curl
   PREFIX "${CURL_DIR}"
   INSTALL_DIR "${CURL_DIR}"
   GIT_REPOSITORY https://github.com/curl/curl
   GIT_TAG ${CURL_TAG}
   GIT_SHALLOW Yes
   CMAKE_CACHE_ARGS -DCMAKE_OSX_ARCHITECTURES:STRING=x86_64 
   CMAKE_ARGS ${CURL_CMAKE_ARGS}
)

add_library(AUDACITY::libcurl SHARED IMPORTED GLOBAL)

file(MAKE_DIRECTORY "${CURL_DIR}/include")

set_target_properties(AUDACITY::libcurl PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${CURL_DIR}/include"
)

if (WIN32)
   set_target_properties(AUDACITY::libcurl PROPERTIES
      INTERFACE_LINK_LIBRARIES "winmm;ws2_32;advapi32;crypt32"
   )

   set_property(TARGET AUDACITY::libcurl APPEND PROPERTY IMPORTED_CONFIGURATIONS "Debug;Release")

   set_target_properties(AUDACITY::libcurl PROPERTIES
      IMPORTED_IMPLIB_DEBUG "${CURL_DIR}/lib/libcurl-d_imp.lib"
      IMPORTED_LOCATION_DEBUG "${CURL_DIR}/bin/libcurl-d.dll"

      IMPORTED_IMPLIB_RELEASE "${CURL_DIR}/lib/libcurl_imp.lib"
      IMPORTED_LOCATION_RELEASE "${CURL_DIR}/bin/libcurl.dll"
   )
elseif(APPLE)
   if(XCODE)
      set_property(TARGET AUDACITY::libcurl APPEND PROPERTY IMPORTED_CONFIGURATIONS "Debug;Release")

      set_target_properties(AUDACITY::libcurl PROPERTIES
         IMPORTED_LOCATION_RELEASE "${CURL_DIR}/lib/libcurl.dylib"
         IMPORTED_LOCATION_DEBUG "${CURL_DIR}/lib/libcurl-d.dylib"
      )
   else()
      set_property(TARGET AUDACITY::libcurl APPEND PROPERTY IMPORTED_CONFIGURATIONS NOCONFIG)

      set_target_properties(AUDACITY::libcurl PROPERTIES
         IMPORTED_LOCATION_NOCONFIG "${CURL_DIR}/lib/libcurl.dylib"
      )
   endif()
elseif(UNIX)
   set_property(TARGET AUDACITY::libcurl APPEND PROPERTY IMPORTED_CONFIGURATIONS NOCONFIG)

   set_target_properties(AUDACITY::libcurl PROPERTIES
     IMPORTED_LOCATION_NOCONFIG "${CURL_DIR}/lib/libcurl.so"
     IMPORTED_SONAME_NOCONFIG "libcurl.so"
  )
endif()