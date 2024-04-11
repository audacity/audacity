# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2021-2024 MuseScore BVBA and others
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

if(OS_IS_MAC)
    set(EXECUTABLE_NAME audacity MACOSX_BUNDLE)
    set(MACOSX_BUNDLE_ICON_FILE AppIcon.icns)
    set(MACOSX_BUNDLE_GUI_IDENTIFIER org.musescore.${MUSESCORE_NAME})
    set(MACOSX_BUNDLE_BUNDLE_NAME ${MUSESCORE_NAME})
    set(MACOSX_BUNDLE_LONG_VERSION_STRING ${MUSESCORE_VERSION})
    set(MACOSX_BUNDLE_SHORT_VERSION_STRING ${MUSESCORE_VERSION})
    set(MACOSX_BUNDLE_BUNDLE_VERSION ${CMAKE_BUILD_NUMBER})
    set(MACOSX_BUNDLE_COPYRIGHT "Copyright © 1999-2024 MuseScore BVBA and others. Published under the GNU General Public License version 3.")

    set(CMAKE_INSTALL_RPATH ${QT_INSTALL_LIBS})

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

    appshell
    au3wrap
    context
    project
    projectscene
)


###########################################
# Executable declaration
###########################################

add_executable(audacity
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


# If MSCORE_OUTPUT_NAME is set (e.g, when cmake is called by the user), the output executable will be
# called MSCORE_OUTPUT_NAME instead of 'mscore'. This can be used to have MuseScore stable and unstable
# both installed in the same prefix on a Linux system.
if (MSCORE_OUTPUT_NAME)
  set_target_properties(${EXECUTABLE_NAME} PROPERTIES OUTPUT_NAME ${MSCORE_OUTPUT_NAME})
endif (MSCORE_OUTPUT_NAME)

if (OS_IS_MAC)
    set_target_properties(audacity PROPERTIES MACOSX_BUNDLE_INFO_PLIST ${PROJECT_SOURCE_DIR}/../cmake-proxies/cmake-modules/MacOSXBundleInfo.plist.in
                                            XCODE_ATTRIBUTE_PRODUCT_BUNDLE_IDENTIFIER ${MACOSX_BUNDLE_GUI_IDENTIFIER})
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
                ${PROJECT_SOURCE_DIR}/build/qt.conf
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

###########################################
# Linux
###########################################
elseif(OS_IS_LIN)

    install(TARGETS ${EXECUTABLE_NAME} RUNTIME DESTINATION bin )

elseif(OS_IS_FBSD)
###########################################
# FreeBSD
###########################################


###########################################
# MacOS
###########################################
elseif(OS_IS_MAC)

    install(TARGETS audacity BUNDLE DESTINATION ${CMAKE_INSTALL_PREFIX})

    if(MU_COMPILE_INSTALL_QTQML_FILES)
        install(DIRECTORY
                ${QT_INSTALL_QML}
                DESTINATION ${Mscore_SHARE_NAME}${Mscore_INSTALL_NAME}
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
