# CMake script to sign macOS build
# Arguments:
# APP_IDENTIFIER - app identifier
# APP_LOCATION - the path to Audacity.app
# DMG_LOCATION - the path to Audaicty dmg package
# APPLE_CODESIGN_IDENTITY - identity to use
# APPLE_CODESIGN_ENTITLEMENTS - path to the entitlements

function( codesign path deep is_dmg)
    message(STATUS "Signing ${path}")

    set ( args 
        --verbose
        --timestamp
        --identifier "${APP_IDENTIFIER}"
        --sign "${APPLE_CODESIGN_IDENTITY}"
    )

    if( NOT is_dmg )
        list( APPEND args
                --options runtime 
                --entitlements "${APPLE_CODESIGN_ENTITLEMENTS}"
        )
    endif()

    if( deep )
        list( APPEND args --deep)
    endif()

    execute_process( COMMAND xcrun codesign ${args} ${path} )
endfunction()

function( sign_modules path )
    message(STATUS "\tLooking for modules or plugins in: '${path}'")

    file( GLOB_RECURSE modules 
          LIST_DIRECTORIES Off
          "${path}/*.so" "${path}/*.dylib"
        )
    
    foreach( module ${modules} )
        codesign( ${module} Off Off )
    endforeach()
endfunction()

if( DEFINED APP_LOCATION )
    sign_modules( "${APP_LOCATION}/Contents/modules" )
    sign_modules( "${APP_LOCATION}/Contents/plug-ins" )
    codesign( "${APP_LOCATION}" On Off )
endif()

if (DEFINED DMG_LOCATION )
    codesign( "${DMG_LOCATION}" Off On)
endif()
