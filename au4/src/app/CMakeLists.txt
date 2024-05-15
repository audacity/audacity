
###########################################
# Setup main application
###########################################

###########################################
# Common
###########################################
set(EXECUTABLE_NAME audacity)

###########################################
# Platform specific
###########################################
include(GetPlatformInfo)

if (OS_IS_WIN)
    set(APP_OUTPUT_NAME ${MUSESCORE_NAME}${MUSESCORE_VERSION_MAJOR})

    include(GetCompilerInfo)

    if (CC_IS_MSVC)
        # MSVC recognizes a *.rc file and will invoke the resource compiler to link it
        set(WINDOWS_ICONS_RC ${PROJECT_SOURCE_DIR}/share/icons/windows_icons.rc)
    elseif(CC_IS_MINGW)
        set(WINDOWS_ICONS_RC ${PROJECT_BINARY_DIR}/windows_icons_rc.o)
        add_custom_command(
            OUTPUT ${PROJECT_BINARY_DIR}/windows_icons_rc.o
            COMMAND ${CMAKE_RC_COMPILER} -i windows_icons.rc -o ${PROJECT_BINARY_DIR}/windows_icons_rc.o
            DEPENDS ${PROJECT_SOURCE_DIR}/share/icons/windows_icons.rc
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/share/icons
            )
        set_source_files_properties(${PROJECT_BINARY_DIR}/windows_icons_rc.o PROPERTIES generated true)
    endif()

elseif(OS_IS_LIN)

    if (MUSE_APP_INSTALL_SUFFIX)
        set(APP_OUTPUT_NAME "${EXECUTABLE_NAME}${MUSE_APP_INSTALL_SUFFIX}")
    endif()

    set(CMAKE_INSTALL_RPATH "${QT_INSTALL_LIBS}")
    if (BUILD_SHARED_LIBS)
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH};${SHARED_LIBS_INSTALL_DESTINATION}")
    endif(BUILD_SHARED_LIBS)

elseif(OS_IS_MAC)
    set(EXECUTABLE_NAME audacity MACOSX_BUNDLE)
    set(MACOSX_BUNDLE_ICON_FILE AppIcon.icns)
    set(MACOSX_BUNDLE_GUI_IDENTIFIER ${MUSE_APP_GUI_IDENTIFIER})
    set(MACOSX_BUNDLE_BUNDLE_NAME ${MUSE_APP_TITLE})
    set(MACOSX_BUNDLE_LONG_VERSION_STRING ${MUSE_APP_VERSION})
    set(MACOSX_BUNDLE_SHORT_VERSION_STRING ${MUSE_APP_VERSION})
    set(MACOSX_BUNDLE_BUNDLE_VERSION ${CMAKE_BUILD_NUMBER})
    set(MACOSX_BUNDLE_COPYRIGHT "Copyright © 1999-2024. Published under the GNU General Public License version 3.")

    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH};${QT_INSTALL_LIBS};@loader_path/../Frameworks")

    message("MACOSX_BUNDLE_VERSION: ${MACOSX_BUNDLE_LONG_VERSION_STRING}.${MACOSX_BUNDLE_BUNDLE_VERSION}")
endif()

###########################################
# Dependency library declaration
###########################################

set(LINK_LIB
    ${QT_LIBRARIES}
    muse::global
    wxwidgets::wxwidgets

    muse::actions
    muse::ui
    muse::uicomponents
    muse::dockwindow
    muse::shortcuts
    muse::workspace
    muse::accessibility
    muse::cloud
    muse::network
    muse::multiinstances
    muse::learn
    muse::languages

    appshell
    au3wrap
    context
    project
    projectscene
    playback
)

###########################################
# Resources
###########################################
qt_add_resources(APP_RCC_SOURCES app.qrc)

###########################################
# Executable declaration
###########################################

add_executable(${EXECUTABLE_NAME}
    ${APP_RCC_SOURCES}
    main.cpp
    app.cpp
    app.h
    #commandlineparser.cpp
    #commandlineparser.h
)

target_include_directories(audacity PUBLIC
    ${PROJECT_SOURCE_DIR}/src
    ${PROJECT_SOURCE_DIR}/src/framework
)


###########################################
# Setting target properties
###########################################

if (APP_OUTPUT_NAME)
  set_target_properties(${EXECUTABLE_NAME} PROPERTIES OUTPUT_NAME ${APP_OUTPUT_NAME})
endif (APP_OUTPUT_NAME)

if (OS_IS_MAC)
    set_target_properties(audacity
        PROPERTIES MACOSX_BUNDLE_INFO_PLIST ${PROJECT_SOURCE_DIR}/buildscripts/packaging/MacOS/MacOSXBundleInfo.plist.in
        XCODE_ATTRIBUTE_PRODUCT_BUNDLE_IDENTIFIER ${MACOSX_BUNDLE_GUI_IDENTIFIER}
    )
endif (OS_IS_MAC)

if (CC_IS_MINGW)
    if (CMAKE_BUILD_TYPE MATCHES "DEBUG")
        set_target_properties(${EXECUTABLE_NAME} PROPERTIES LINK_FLAGS "-mwindows -mconsole")
    else (CMAKE_BUILD_TYPE MATCHES "DEBUG")
        set_target_properties(${EXECUTABLE_NAME} PROPERTIES LINK_FLAGS "-Wl,-S -mwindows")
    endif (CMAKE_BUILD_TYPE MATCHES "DEBUG")
endif(CC_IS_MINGW)

if (CC_IS_MSVC)
    target_link_options(${EXECUTABLE_NAME} PRIVATE /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup)
endif(CC_IS_MSVC)

###########################################
# Link
###########################################
target_link_libraries(audacity ${LINK_LIB} )

###########################################
# INSTALL
###########################################

###########################################
# Windows
###########################################
if (OS_IS_WIN)

    install(TARGETS ${EXECUTABLE_NAME} RUNTIME DESTINATION bin )

    include(GetCompilerInfo)

    if (CC_IS_MINGW)
        get_filename_component(COMPILER_DIR ${CMAKE_CXX_COMPILER} DIRECTORY)
        get_filename_component (MINGW_ROOT ${COMPILER_DIR} DIRECTORY)

        install( FILES
               ${MINGW_ROOT}/bin/libgcc_s_seh-1.dll
               ${MINGW_ROOT}/bin/libstdc++-6.dll
               ${MINGW_ROOT}/bin/libwinpthread-1.dll
               DESTINATION bin)

    endif(CC_IS_MINGW)

    # Install Qt
    foreach (QtLibrary ${QT_LIBRARIES})
        #message(STATUS "Library ${QtLibrary}")
        # always use release libs
        # set_target_properties(${QtLibrary} PROPERTIES MAP_IMPORTED_CONFIG_DEBUG "RELEASE")
        get_target_property(QtSharedLibrary ${QtLibrary} LOCATION_RELEASE)
        if (EXISTS ${QtSharedLibrary})
            list (APPEND QtInstallLibraries ${QtSharedLibrary})
        endif (EXISTS ${QtSharedLibrary})
    endforeach (QtLibrary ${QT_LIBRARIES})
    list(REMOVE_DUPLICATES QtInstallLibraries)

    if (MUE_COMPILE_QT5_COMPAT)
        install(FILES
                ${PROJECT_SOURCE_DIR}/build/qt.conf
                ${QtInstallLibraries}
                ${QT_INSTALL_BINS}/libEGL.dll
                ${QT_INSTALL_BINS}/libGLESv2.dll
                ${QT_INSTALL_BINS}/opengl32sw.dll
                ${QT_INSTALL_BINS}/d3dcompiler_47.dll
                ${QT_INSTALL_BINS}/Qt5QmlModels.dll
                ${QT_INSTALL_BINS}/Qt5QmlWorkerScript.dll
                DESTINATION bin)
    else()
        install(FILES
                #${PROJECT_SOURCE_DIR}/build/qt.conf
                ${QtInstallLibraries}
                #${QT_INSTALL_BINS}/libEGL.dll
                #${QT_INSTALL_BINS}/libGLESv2.dll
                ${QT_INSTALL_BINS}/opengl32sw.dll
                ${QT_INSTALL_BINS}/d3dcompiler_47.dll
                ${QT_INSTALL_BINS}/Qt6QuickControls2Impl.dll
                ${QT_INSTALL_BINS}/Qt6QuickLayouts.dll
                ${QT_INSTALL_BINS}/Qt6QmlModels.dll
                ${QT_INSTALL_BINS}/Qt6QmlWorkerScript.dll
                DESTINATION bin)
    endif()

    install(FILES
            ${QT_INSTALL_PLUGINS}/iconengines/qsvgicon.dll
            DESTINATION bin/iconengines)

    install(FILES
            ${QT_INSTALL_PLUGINS}/imageformats/qjpeg.dll
            ${QT_INSTALL_PLUGINS}/imageformats/qsvg.dll
            DESTINATION bin/imageformats)

    install(FILES
            ${QT_INSTALL_PLUGINS}/platforms/qwindows.dll
            ${QT_INSTALL_PLUGINS}/platforms/qoffscreen.dll
            DESTINATION bin/platforms)

    install(FILES
            ${QT_INSTALL_PLUGINS}/tls/qopensslbackend.dll
            DESTINATION bin/tls)

    install(DIRECTORY
            ${QT_INSTALL_QML}
            DESTINATION .
            REGEX ".*d\\.dll" EXCLUDE
            REGEX ".pdb" EXCLUDE
            REGEX ".*QtMultimedia.*" EXCLUDE
            REGEX ".*QtSensors.*" EXCLUDE
            REGEX ".*QtTest.*" EXCLUDE
            REGEX ".*QtWebkit.*" EXCLUDE
            )

    if (SNDFILE_DLL)
        install(FILES ${SNDFILE_DLL} DESTINATION bin)
    endif()

    # Install ssl
    install(FILES
            ${DEPENDENCIES_LIB_DIR}/libcrypto-1_1-x64.dll
            ${DEPENDENCIES_LIB_DIR}/libssl-1_1-x64.dll
            DESTINATION bin)

    if (WIN_PORTABLE)
        # deploy the files and directory structure needed for the PortableApps.com format
        install(DIRECTORY ${PROJECT_SOURCE_DIR}/build/PortableApps/App DESTINATION ${CMAKE_INSTALL_PREFIX}/../..)
        install(DIRECTORY ${PROJECT_SOURCE_DIR}/build/PortableApps/Other DESTINATION ${CMAKE_INSTALL_PREFIX}/../..)
        install(FILES ${PROJECT_SOURCE_DIR}/build/PortableApps/help.html DESTINATION ${CMAKE_INSTALL_PREFIX}/../..)
        configure_file(${PROJECT_SOURCE_DIR}/build/PortableApps/appinfo.ini.in     ${CMAKE_INSTALL_PREFIX}/../../App/AppInfo/appinfo.ini   @ONLY)
    endif (WIN_PORTABLE)

    # TODO
    # install( DIRECTORY "${_LIB_OUTPUT}/"
    #          TYPE LIB
    #          USE_SOURCE_PERMISSIONS
    #      )

    install( DIRECTORY "${_LIB_OUTPUT}/"
              TYPE BIN
              USE_SOURCE_PERMISSIONS
          )

###########################################
# Linux
###########################################
elseif(OS_IS_LIN)

    install(TARGETS ${EXECUTABLE_NAME} RUNTIME DESTINATION bin)

    # message("~~~~~~~ install DIRECTORY: ${_LIB_OUTPUT} DESTINATION ${_LIB_INSTALL}")
    # install( DIRECTORY "${_LIB_OUTPUT}"
    #          DESTINATION "${_LIB_INSTALL}"
    #          USE_SOURCE_PERMISSIONS
    #          FILES_MATCHING PATTERN "*.so*" )

elseif(OS_IS_FBSD)
###########################################
# FreeBSD
###########################################


###########################################
# MacOS
###########################################
elseif(OS_IS_MAC)

    message("~~~~~~CMAKE_INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
    install(TARGETS audacity BUNDLE DESTINATION ${CMAKE_INSTALL_PREFIX})

    if(MU_COMPILE_INSTALL_QTQML_FILES)
        install(DIRECTORY
                ${QT_INSTALL_QML}
                DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}${AU4_FRAMEWORKS_NAME}
                REGEX ".*QtWebkit.*" EXCLUDE
                REGEX ".*QtTest.*" EXCLUDE
                REGEX ".*QtSensors.*" EXCLUDE
                REGEX ".*QtMultimedia.*" EXCLUDE
                REGEX ".*QtAudioEngine.*" EXCLUDE
                REGEX ".*_debug\\.dylib" EXCLUDE)
    endif()

###########################################
# Wasm
###########################################
elseif(OS_IS_WASM)

else()
    message(FATAL_ERROR "Unsupported Platform: ${CMAKE_HOST_SYSTEM_NAME}")
endif()


#################################################
# Miscellaneous Microsoft Visual Studio settings
#################################################
if (MSVC)

   # Force the "install" and "package" targets not to depend on the "all" target.
   set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY true)
   set(CMAKE_SKIP_PACKAGE_ALL_DEPENDENCY true)

   # Set the startup project to "mscore".
   if (NOT ${CMAKE_VERSION} VERSION_LESS "3.6.0")
      set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT mscore)
   endif ()

   # Set the debugging properties for the "mscore" project.
   file(TO_NATIVE_PATH "${CMAKE_INSTALL_PREFIX}/bin" VS_DEBUGGER_WORKING_DIRECTORY)
   if (NOT ${CMAKE_VERSION} VERSION_LESS "3.12.0")
      set_target_properties(${EXECUTABLE_NAME} PROPERTIES VS_DEBUGGER_COMMAND "${VS_DEBUGGER_WORKING_DIRECTORY}\\${MUSESCORE_EXECUTABLE_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
   endif ()
   if (NOT ${CMAKE_VERSION} VERSION_LESS "3.8.0")
      set_target_properties(${EXECUTABLE_NAME} PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${VS_DEBUGGER_WORKING_DIRECTORY}")
   endif ()
   if (NOT ${CMAKE_VERSION} VERSION_LESS "3.13.0")
      set_target_properties(${EXECUTABLE_NAME} PROPERTIES VS_DEBUGGER_COMMAND_ARGUMENTS "--debug")
   endif ()

endif (MSVC)
