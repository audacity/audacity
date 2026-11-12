#
# Audacity: A Digital Audio Editor
#

declare_module(audio)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audioconfigurationtypes.h
    ${CMAKE_CURRENT_LIST_DIR}/driver/audiodrivercontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/driver/audiodrivercontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/driver/iaudiodevices.h
    ${CMAKE_CURRENT_LIST_DIR}/driver/iaudiodrivercontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiostreamsuspender.h
    ${CMAKE_CURRENT_LIST_DIR}/audiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audiomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/audiotypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiobackendregister.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudioengine.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/idevice.h
    ${CMAKE_CURRENT_LIST_DIR}/isystemaudiodeviceslistener.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audioengineswitcher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audioengineswitcher.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiometer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/auqttimer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/auqttimer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/itimer.h

    # for muse
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiothreadsecurer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiothreadsecurer.h
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

set(MODULE_LINK
        au3-utility # For LockFreeQueue
        au3-project-rate # The default sample rate and format of new projects
        wxBase
)

if (OS_IS_MAC)
    set(MODULE_LINK ${MODULE_LINK} "-framework CoreAudio")
elseif (OS_IS_WIN)
    set(MODULE_LINK ${MODULE_LINK} ole32)
elseif (OS_IS_LIN AND LIBUDEV_INCLUDE_DIR AND LIBUDEV_LIBRARY)
    set(MODULE_INCLUDE ${MODULE_INCLUDE} ${LIBUDEV_INCLUDE_DIR})
    set(MODULE_LINK ${MODULE_LINK} ${LIBUDEV_LIBRARY})
    set(MODULE_DEF ${MODULE_DEF} AU_AUDIO_HAS_UDEV)
endif()

setup_module()
