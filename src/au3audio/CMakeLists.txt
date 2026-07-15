#
# Audacity: A Digital Audio Editor
#

declare_module(au3audio)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/au3audiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audiomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iau3devicemanager.h
    ${CMAKE_CURRENT_LIST_DIR}/isystemaudiodeviceslistener.h

    # AU3
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioengine.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiodevicesprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiodevicesprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3devicemanager.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3devicemanager.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioiolistener.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.h
    )

if (OS_IS_MAC)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macossystemaudiodeviceslistener.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macossystemaudiodeviceslistener.h
    )
    # the CoreAudio system headers clash with the `using namespace muse`
    # of other unity-combined sources
    set_source_files_properties(${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macossystemaudiodeviceslistener.cpp
        PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON
    )
elseif (OS_IS_WIN)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/windows/windowssystemaudiodeviceslistener.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/windows/windowssystemaudiodeviceslistener.h
    )
    # the Windows system headers clash with the `using namespace muse`
    # of other unity-combined sources
    set_source_files_properties(${CMAKE_CURRENT_LIST_DIR}/internal/platform/windows/windowssystemaudiodeviceslistener.cpp
        PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON
    )
elseif (OS_IS_LIN)
    find_path(LIBUDEV_INCLUDE_DIR libudev.h)
    find_library(LIBUDEV_LIBRARY udev)
    if (LIBUDEV_INCLUDE_DIR AND LIBUDEV_LIBRARY)
        set(MODULE_SRC ${MODULE_SRC}
            ${CMAKE_CURRENT_LIST_DIR}/internal/platform/linux/linuxsystemaudiodeviceslistener.cpp
            ${CMAKE_CURRENT_LIST_DIR}/internal/platform/linux/linuxsystemaudiodeviceslistener.h
        )
    else()
        message(STATUS "libudev not found - system audio device change detection disabled")
        set(MODULE_SRC ${MODULE_SRC}
            ${CMAKE_CURRENT_LIST_DIR}/internal/platform/stub/stubsystemaudiodeviceslistener.h
        )
    endif()
else()
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/stub/stubsystemaudiodeviceslistener.h
    )
endif()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

if (OS_IS_MAC)
    set(MODULE_LINK ${MODULE_LINK} "-framework CoreAudio")
elseif (OS_IS_WIN)
    set(MODULE_LINK ${MODULE_LINK} ole32)
elseif (OS_IS_LIN AND LIBUDEV_INCLUDE_DIR AND LIBUDEV_LIBRARY)
    set(MODULE_INCLUDE ${MODULE_INCLUDE} ${LIBUDEV_INCLUDE_DIR})
    set(MODULE_LINK ${MODULE_LINK} ${LIBUDEV_LIBRARY})
    set(MODULE_DEF ${MODULE_DEF} AU_AU3AUDIO_HAS_UDEV)
endif()

setup_module()

if (AU_BUILD_AU3AUDIO_TESTS)
    add_subdirectory(tests)
endif()
