
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
    set(APP_OUTPUT_NAME ${MUSE_APP_NAME}${MUSE_APP_VERSION_MAJOR})

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

macro(add_to_link_if_exists lib)

# NOTE There may be cases:
# 1. The module is enabled and the target exists.
# 2. The module is disabled, but there is a stub, the target exists
# 3. The module is disabled and the target does not exist.
# This macro also adds qml parts to the link

    if (TARGET ${lib})
        list(APPEND LINK_LIB ${lib})

        if (TARGET ${lib}_qml)
            list(APPEND LINK_LIB ${lib}_qml)
        endif()
    endif()
endmacro()

set(LINK_LIB
    ${QT_LIBRARIES}
    muse::global
    muse::draw
)

add_to_link_if_exists(muse::accessibility)
add_to_link_if_exists(muse::actions)
add_to_link_if_exists(muse::audioplugins)
add_to_link_if_exists(muse::autobot)
add_to_link_if_exists(muse::cloud_qml)
add_to_link_if_exists(muse::cloud)
add_to_link_if_exists(muse::diagnostics)
add_to_link_if_exists(muse::dockwindow)
add_to_link_if_exists(muse::extensions)
add_to_link_if_exists(muse::graphicaleffects_qml)
add_to_link_if_exists(muse::interactive)
add_to_link_if_exists(muse::languages)
add_to_link_if_exists(muse::learn)
add_to_link_if_exists(muse::legacytreeview_qml)
add_to_link_if_exists(muse::multiwindows)
add_to_link_if_exists(muse::muse::vst)
add_to_link_if_exists(muse::network)
add_to_link_if_exists(muse::shortcuts)
add_to_link_if_exists(muse::tours)
add_to_link_if_exists(muse::ui_dialogs_qml)
add_to_link_if_exists(muse::ui)
add_to_link_if_exists(muse::uicomponents)
add_to_link_if_exists(muse::workspace)

add_to_link_if_exists(appshell)
add_to_link_if_exists(au3audio)
add_to_link_if_exists(au3wrap)
add_to_link_if_exists(au3cloud)
add_to_link_if_exists(audio)
add_to_link_if_exists(context)
add_to_link_if_exists(effects_audio_unit)
add_to_link_if_exists(effects_base)
add_to_link_if_exists(effects_builtin)
add_to_link_if_exists(effects_lv2)
add_to_link_if_exists(effects_nyquist)
add_to_link_if_exists(effects_vst)
add_to_link_if_exists(exporter)
add_to_link_if_exists(iex_labels)
add_to_link_if_exists(importer)
add_to_link_if_exists(playback)
add_to_link_if_exists(preferences)
add_to_link_if_exists(project)
add_to_link_if_exists(projectscene)
add_to_link_if_exists(record)
add_to_link_if_exists(spectrogram)
add_to_link_if_exists(toast)
add_to_link_if_exists(automation)

if (AU_MODULE_EFFECTS_VST)
    add_definitions(-DAU_MODULE_EFFECTS_VST)
endif()

if (AU_MODULE_EFFECTS_LV2)
    add_definitions(-DAU_MODULE_EFFECTS_LV2)
endif()

if (AU_MODULE_EFFECTS_AUDIO_UNIT)
    add_definitions(-DAU_MODULE_EFFECTS_AUDIO_UNIT)
endif()

set (AUDACITY_APPEND_SRC)

if (OS_IS_WIN)
    list(APPEND AUDACITY_APPEND_SRC ${WINDOWS_ICONS_RC})
endif()

###########################################
# Resources
###########################################
qt_add_resources(APP_RCC_SOURCES app.qrc)

###########################################
# Executable declaration
###########################################

qt_add_executable(${EXECUTABLE_NAME}
    WIN32 MACOSX_BUNDLE
    ${APP_RCC_SOURCES}
    ${AUDACITY_APPEND_SRC}
    main.cpp
    appfactory.cpp
    appfactory.h
    guiapp.cpp
    guiapp.h
    pluginregistrationapp.cpp
    pluginregistrationapp.h
    commandlineparser.cpp
    commandlineparser.h
)

###########################################
# Includes
###########################################

target_include_directories(audacity PRIVATE
    ${PROJECT_BINARY_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}

    ${PROJECT_SOURCE_DIR}/src

    ${MUSE_FRAMEWORK_PATH}
    ${MUSE_FRAMEWORK_PATH}/framework
    ${MUSE_FRAMEWORK_PATH}/framework/global
)


###########################################
# Setting target properties
###########################################

if (APP_OUTPUT_NAME)
  set_target_properties(${EXECUTABLE_NAME} PROPERTIES OUTPUT_NAME ${APP_OUTPUT_NAME})
endif (APP_OUTPUT_NAME)

if (MUSE_ENABLE_UNIT_TESTS_CODE_COVERAGE)
    set(COVERAGE_FLAGS -fprofile-arcs -ftest-coverage --coverage)
    set(COVERAGE_LINK_FLAGS -lgcov --coverage)
    target_compile_options(audacity PRIVATE ${COVERAGE_FLAGS})
    target_link_options(audacity PRIVATE ${COVERAGE_LINK_FLAGS})
endif()

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
##########################################
target_link_libraries(audacity PRIVATE ${LINK_LIB}  ${COVERAGE_LINK_FLAGS})

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
    set(deploy_tool_options_arg "--plugindir bin --qmldir ${PROJECT_SOURCE_DIR}")

    qt_generate_deploy_qml_app_script(
        TARGET audacity
        OUTPUT_SCRIPT deploy_script
        NO_UNSUPPORTED_PLATFORM_ERROR
        ${NO_TRANSLATIONS_OPTION} # Even with NO_OVERWRITE set, the translations get re-generated each time ...
        DEPLOY_TOOL_OPTIONS ${deploy_tool_options_arg}
    )

    add_custom_command(
        TARGET audacity PRE_LINK
        COMMAND ${CMAKE_COMMAND} -DSCRIPT_PATH=${deploy_script} -P ${CMAKE_CURRENT_SOURCE_DIR}/add_NO_OVERWRITE_option.cmake
        COMMENT "Injecting NO_OVERWRITE into Qt deploy script"
    )

    install(SCRIPT ${deploy_script})

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

    if (MSVC)
        set_target_properties(${EXECUTABLE_NAME} PROPERTIES
            VS_DEBUGGER_COMMAND "${CMAKE_INSTALL_PREFIX}/bin/${EXECUTABLE_NAME}"
            VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/bin"
        )

        # Unlike VSCode, Visual Studio doesn't have pre-launch tasks we could use to run the `INSTALL` target.
        # Instead, we copy the executable to the install directory after the build.
        add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_INSTALL_PREFIX}/bin
            COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${EXECUTABLE_NAME}> ${CMAKE_INSTALL_PREFIX}/bin
        )
    endif()

###########################################
# Linux
###########################################
elseif(OS_IS_LIN)

    install(TARGETS ${EXECUTABLE_NAME} RUNTIME DESTINATION bin)

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

endif (MSVC)
