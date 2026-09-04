#
# Audacity: A Digital Audio Editor
#

include(GetPlatformInfo)

if (OS_IS_MAC)
    execute_process(
        COMMAND xcrun actool --version --output-format human-readable-text
        OUTPUT_VARIABLE AU4_ACTOOL_VERSION
        ERROR_QUIET
    )

    if (AU4_ACTOOL_VERSION MATCHES "short-bundle-version: ([0-9]+)" AND CMAKE_MATCH_1 GREATER_EQUAL 26)
        set(AU4_APPICON_DIR ${CMAKE_BINARY_DIR}/src/app/${AU4_SHARE_NAME}${AU4_INSTALL_NAME})
        set(AU4_APPICON_FILES ${AU4_APPICON_DIR}Assets.car ${AU4_APPICON_DIR}AppIcon.icns)
        file(GLOB_RECURSE AU4_APPICON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/AppIcon.icon/*)

        add_custom_command(
            OUTPUT ${AU4_APPICON_FILES}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${AU4_APPICON_DIR}
            COMMAND xcrun actool
                    --compile ${AU4_APPICON_DIR}
                    --enable-on-demand-resources NO
                    --app-icon AppIcon
                    --platform macosx
                    --include-all-app-icons
                    --minimum-deployment-target ${CMAKE_OSX_DEPLOYMENT_TARGET}
                    --output-partial-info-plist /dev/null
                    --target-device mac
                    ${CMAKE_CURRENT_SOURCE_DIR}/AppIcon.icon
            DEPENDS ${AU4_APPICON_SOURCES}
            COMMENT "Compiling AppIcon.icon"
            VERBATIM
        )

        add_custom_target(appicon DEPENDS ${AU4_APPICON_FILES})
        add_dependencies(audacity appicon)
        set_property(TARGET audacity APPEND PROPERTY LINK_DEPENDS "${AU4_APPICON_FILES}")

        install(FILES ${AU4_APPICON_FILES} DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME})
    else()
        message(WARNING "actool >= 26.0 not found, falling back to AppIcon/AU4_AppIcon.icns")
        install(FILES AppIcon/AU4_AppIcon.icns RENAME AppIcon.icns DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME})
    endif()

    install(FILES AupIcon/AU4_AupIcon.icns RENAME AupIcon.icns DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME})

elseif(OS_IS_WIN)
    # See also windows_icons.rc

    if (WIN_PORTABLE)
        install(FILES AppIcon/AU4_AppIcon.ico RENAME appicon.ico DESTINATION ${CMAKE_INSTALL_PREFIX}/../AppInfo)
        install(FILES AppIcon/AU4_AppIcon_16x16.png RENAME appicon_16.png DESTINATION ${CMAKE_INSTALL_PREFIX}/../AppInfo)
        install(FILES AppIcon/AU4_AppIcon_32x32.png RENAME appicon_32.png DESTINATION ${CMAKE_INSTALL_PREFIX}/../AppInfo)
        install(FILES AppIcon/AU4_AppIcon_128x128.png RENAME appicon_128.png DESTINATION ${CMAKE_INSTALL_PREFIX}/../AppInfo)
        install(FILES AupIcon/AU4_AupIcon.ico RENAME aup.ico DESTINATION ${CMAKE_INSTALL_PREFIX}/../AppInfo/FileTypeIcons)
        install(FILES AupIcon/AU4_AupIcon_512x512.png RENAME aup.png DESTINATION ${CMAKE_INSTALL_PREFIX}/../AppInfo/FileTypeIcons)
    endif()

elseif(OS_IS_LIN)
    foreach(SIZE 16 24 32 48 64 96 128 512)
        install(FILES AppIcon/AU4_AppIcon_${SIZE}x${SIZE}.png 
                RENAME audacity${MUSE_APP_INSTALL_SUFFIX}.png 
                DESTINATION share/icons/hicolor/${SIZE}x${SIZE}/apps)
    endforeach()

    install(FILES AupIcon/AU4_AupIcon_512x512.png 
            RENAME application-x-audacity${MUSE_APP_INSTALL_SUFFIX}.png
            DESTINATION share/icons/hicolor/512x512/mimetypes)
    install(FILES AupIcon/AU4_AupIcon.svg 
            RENAME application-x-audacity${MUSE_APP_INSTALL_SUFFIX}.svg
            DESTINATION share/icons/hicolor/scalable/mimetypes)

    # Note: Must now run "gtk-update-icon-cache" to set the new icons.
endif()
