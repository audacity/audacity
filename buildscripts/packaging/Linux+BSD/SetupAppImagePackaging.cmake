include(GetPlatformInfo)

if(NOT(OS_IS_LIN OR OS_IS_FBSD))
    return()
endif()

set(DESKTOP_LAUNCHER_NAME "${MUSE_APP_NAME_VERSION} Portable")

# Build portable AppImage as per https://github.com/probonopd/AppImageKit
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/portable)

execute_process(COMMAND echo ${CMAKE_INSTALL_PREFIX} OUTPUT_FILE PREFIX.txt)

# Prepare portable scripts:
configure_file(${CMAKE_CURRENT_LIST_DIR}/portable/AppRun.in AppRun @ONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/portable/portable-utils.in portable-utils @ONLY)
install(PROGRAMS ${PROJECT_BINARY_DIR}/AppRun DESTINATION . COMPONENT portable)
install(PROGRAMS ${PROJECT_BINARY_DIR}/portable-utils
    ${CMAKE_CURRENT_LIST_DIR}/portable/ldd-recursive
    ${CMAKE_CURRENT_LIST_DIR}/portable/rm-empty-dirs DESTINATION bin COMPONENT portable)

install(FILES ${CMAKE_CURRENT_LIST_DIR}/portable/qt.conf DESTINATION bin COMPONENT portable)


# Identify App's main window so that it receives the correct name
# and icon in the OS dock / taskbar. Run `xprop WM_CLASS` and click on
# App's main window to find out what string to use here.
set(WINDOW_MANAGER_CLASS ${MUSE_APP_NAME_VERSION})

# Install desktop file (perform variable substitution first)
configure_file(${CMAKE_CURRENT_LIST_DIR}/org.musescore.MuseScore.desktop.in org.musescore.MuseScore${MUSE_APP_INSTALL_SUFFIX}.desktop)
install(FILES ${PROJECT_BINARY_DIR}/org.musescore.MuseScore${MUSE_APP_INSTALL_SUFFIX}.desktop DESTINATION share/applications)

# Install appdata file (perform variable substitution first)
if("${MUSE_APP_INSTALL_SUFFIX}" MATCHES "-")
    message(FATAL_ERROR
        "MUSE_APP_INSTALL_SUFFIX='${MUSE_APP_INSTALL_SUFFIX}'\n"
        "MUSE_APP_INSTALL_SUFFIX must not contain hyphen characters. It will be used "
        "inside the <id> tag in *.appdata.xml and hyphens are discouraged there."
    )
endif()

configure_file(${CMAKE_CURRENT_LIST_DIR}/org.musescore.MuseScore.appdata.xml.in org.musescore.MuseScore${MUSE_APP_INSTALL_SUFFIX}.appdata.xml)
install(FILES ${PROJECT_BINARY_DIR}/org.musescore.MuseScore${MUSE_APP_INSTALL_SUFFIX}.appdata.xml DESTINATION share/metainfo)

# Add .MSCZ, .MSCX and .MSCS to MIME database (informs system that filetypes .MSCZ, .MSCX and .MSCS are MuseScore files)
#configure_file(${CMAKE_CURRENT_LIST_DIR}/musescore.xml.in musescore${MUSE_APP_INSTALL_SUFFIX}.xml)
#install(FILES ${PROJECT_BINARY_DIR}/musescore${MUSE_APP_INSTALL_SUFFIX}.xml DESTINATION share/mime/packages COMPONENT doc)

# Note: Must now run "update-mime-database" to apply changes.
