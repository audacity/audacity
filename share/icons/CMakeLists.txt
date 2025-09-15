#
# Audacity: A Digital Audio Editor
#

include(GetPlatformInfo)

if (OS_IS_MAC)
    install(FILES AppIcon/AU4_AppIcon.icns RENAME AppIcon.icns DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME})

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
