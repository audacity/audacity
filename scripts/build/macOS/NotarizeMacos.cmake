# CMake script to sign macOS build
# Arguments:
# APP_IDENTIFIER - app identifier
# APP_LOCATION - the path to Audacity.app
# DMG_LOCATION - the path to Audacity dmg package
# APPLE_NOTARIZATION_USER_NAME - notarization user name
# APPLE_NOTARIZATION_PASSWORD - notarization password

# https://cmake.org/cmake/help/latest/policy/CMP0054.html
cmake_policy( SET CMP0054 NEW )
# https://cmake.org/cmake/help/latest/policy/CMP0011.html
cmake_policy( SET CMP0011 NEW )

function( get_plist_value output path key )
    execute_process(
        COMMAND /usr/libexec/PlistBuddy -c "Print ${key}" "${path}"
        OUTPUT_VARIABLE result
    )

    string( STRIP ${result} result )

    set( ${output} ${result} PARENT_SCOPE )
endfunction()

if( APP_LOCATION )
    get_filename_component( temp_dir "${APP_LOCATION}/.." ABSOLUTE )
else()
    get_filename_component( temp_dir "${DMG_LOCATION}" DIRECTORY )
endif()

set( temp_plist "${temp_dir}/NotarizationResult.plist" )

function( notarize path )
    message( STATUS "Notarizing ${path}" )

    execute_process( 
        COMMAND 
            xcrun altool 
                --notarize-app
                --primary-bundle-id "${APP_IDENTIFIER}"
                --file "${path}"
                --username "${APPLE_NOTARIZATION_USER_NAME}"
                --password "${APPLE_NOTARIZATION_PASSWORD}"
                --output-format xml 
        OUTPUT_VARIABLE
            result 
    )

    file( WRITE ${temp_plist} ${result} )

    get_plist_value( req_id ${temp_plist} "notarization-upload:RequestUUID" )

    message( STATUS "\t Request ID: '${req_id}'" )

    set( success Off )

    while( NOT success )
        execute_process(COMMAND ${CMAKE_COMMAND} -E sleep 15)

        execute_process(
            COMMAND
                xcrun altool 
                    --notarization-info ${req_id}
                    --username ${APPLE_NOTARIZATION_USER_NAME}
                    --password ${APPLE_NOTARIZATION_PASSWORD}
                    --output-format xml
            OUTPUT_VARIABLE
                result
        )

        file( WRITE ${temp_plist} ${result} )

        get_plist_value( notarization_result ${temp_plist} "notarization-info:Status" )

        message( STATUS "\t Status: ${notarization_result}" )

        if( NOT "${notarization_result}" STREQUAL "in progress" )
            if ( NOT "${notarization_result}" STREQUAL "success" ) 
                message(FATAL_ERROR "Notarization failed:\n${result}\n")
            else()
                message(STATUS "Notarization successfull")
            endif()

            break()
        endif()
    endwhile()
endfunction()

if( DEFINED APP_LOCATION )
    get_filename_component( archive "${APP_LOCATION}/../notarization.zip" ABSOLUTE )
    
    execute_process(
        COMMAND
            xcrun ditto 
                -c -k --keepParent 
                ${APP_LOCATION}
                ${archive}    
    )

    notarize( ${archive} )
    execute_process( COMMAND stapler staple "${APP_LOCATION}" )

    file( REMOVE "${APP_LOCATION}/../notarization.zip" )
endif()

if( DEFINED DMG_LOCATION )
    notarize( ${DMG_LOCATION} )
    execute_process( COMMAND stapler staple "${DMG_LOCATION}" )
endif()

file( REMOVE ${temp_plist} )
