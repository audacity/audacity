# Code signing

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   # On windows, we expect WINDOWS_CERTIFICATE to be set
   # as a CMake variable or as an environment variable
   if( DEFINED WINDOWS_CERTIFICATE )
      install( CODE "set( WINDOWS_CERTIFICATE \"${WINDOWS_CERTIFICATE}\" )" )
   endif()

   if( DEFINED WINDOWS_CERTIFICATE_PASSWORD )
      # To simplify the helper script - we push password to the environment
      install( CODE "set( ENV{WINDOWS_CERTIFICATE_PASSWORD} \"${WINDOWS_CERTIFICATE_PASSWORD}\") " )
   endif()

   install( CODE "set( PFX_SIGN_PS_LOCATION \"${CMAKE_SOURCE_DIR}/scripts/build/windows/PfxSign.ps1\") " )
   install( SCRIPT "scripts/build/windows/PfxSign.cmake" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin")
   set_from_env( APPLE_CODESIGN_IDENTITY )
   set_from_env( APPLE_NOTARIZATION_USER_NAME )
   set_from_env( APPLE_NOTARIZATION_PASSWORD )

   # Pass arguments to cmake install script
   install( CODE "set( APPLE_CODESIGN_IDENTITY \"${APPLE_CODESIGN_IDENTITY}\" )" )
   install( CODE "set( APPLE_NOTARIZATION_USER_NAME \"${APPLE_NOTARIZATION_USER_NAME}\" )" )
   install( CODE "set( APPLE_NOTARIZATION_PASSWORD \"${APPLE_NOTARIZATION_PASSWORD}\" )" )

   install( CODE "set( APP_IDENTIFIER \"org.audacityteam.audacity\" )" )
   install( CODE "get_filename_component( APP_LOCATION \${CMAKE_INSTALL_PREFIX}/Audacity.app ABSOLUTE )" )
   install( CODE "set( APPLE_CODESIGN_ENTITLEMENTS ${CMAKE_SOURCE_DIR}/mac/Audacity.entitlements )")

   install( SCRIPT "scripts/build/macOS/SignMacos.cmake" )

   if( ${_OPT}perform_notarization )
      install( SCRIPT "scripts/build/macOS/NotarizeMacos.cmake" )
   endif()
endif()
