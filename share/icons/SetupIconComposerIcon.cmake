find_program(XCRUN xcrun)

function(get_actool_version output_var)
    execute_process(
        COMMAND ${XCRUN} actool --version
        OUTPUT_VARIABLE cmd_output
        ERROR_VARIABLE cmd_error
        RESULT_VARIABLE cmd_result
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    if(NOT cmd_result EQUAL 0)
        set(${output_var} "" PARENT_SCOPE)
        return()
    endif()

    string(REGEX REPLACE "[ \t\r\n]+" "" cmd_output "${cmd_output}")
    string(REGEX MATCH "<key>short-bundle-version</key>[^<]*<string>(.*)</string>" match_found "${cmd_output}")

    if(match_found)
        set(${output_var} "${CMAKE_MATCH_1}" PARENT_SCOPE)
    else()
        set(${output_var} "" PARENT_SCOPE)
    endif()
endfunction()

function(target_setup_iconcomposer_icon target icon_path)
    get_filename_component(icon_path_ext ${icon_path} EXT)
    if(NOT EXISTS ${icon_path} OR NOT IS_DIRECTORY ${icon_path} OR NOT icon_path_ext STREQUAL ".icon")
        message(WARNING "Icon path '${icon_path}' does not seem to name a valid Icon Composer bundle.")
        return()
    endif()
    
    if(NOT XCRUN)
        message(WARNING "xcrun not found. Cannot setup Icon Composer icon.")
        return()
    endif()

    get_actool_version(actool_version)

    if(NOT "${actool_version}" VERSION_GREATER_EQUAL "26.0")
        message(WARNING "Could not find actool with version 26.0 or greater."
            " Icon Composer assets will not be generated.")
        return()
    endif()

    file(GLOB icon_files "${icon_path}/*")

    get_filename_component(icon_name ${icon_path} NAME_WE)

    add_custom_command(OUTPUT Assets.car ${icon_name}.icns
        COMMAND ${XCRUN} actool --output-format human-readable-text
            --compile ${CMAKE_CURRENT_BINARY_DIR}
            --notices --warnings --errors
            --platform macosx
            --minimum-deployment-target ${CMAKE_OSX_DEPLOYMENT_TARGET}
            --output-partial-info-plist /dev/null
            --app-icon ${icon_name}
            ${icon_path}
        VERBATIM
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${icon_files}
        MAIN_DEPENDENCY ${icon_path}/icon.json
        COMMENT "Generating macOS app icon Assets.car and ${icon_name}.icns"
    )

    set(icon_resources
        ${CMAKE_CURRENT_BINARY_DIR}/Assets.car
        ${CMAKE_CURRENT_BINARY_DIR}/${icon_name}.icns
    )

    target_sources(${target} PRIVATE ${icon_resources})
    set_target_properties(${target} PROPERTIES
        MACOSX_BUNDLE_ICON_FILE ${icon_name}.icns
        MACOSX_BUNDLE_ICON_NAME ${icon_name}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        RESOURCE "${icon_resources}"
    )

    # Difference from Musescore: Audacity does ad-hoc signing and this is required
    set_property(TARGET ${target} APPEND PROPERTY
        LINK_DEPENDS "${icon_resources}"
    )

    # At the time of writing, CMake does not propagate this target properties to
    # the Info.plist template, so we create a normal variable for it.
    # https://cmake.org/cmake/help/v4.2/prop_tgt/MACOSX_BUNDLE_INFO_PLIST.html
    set(MACOSX_BUNDLE_ICON_NAME ${icon_name} PARENT_SCOPE)
endfunction()
