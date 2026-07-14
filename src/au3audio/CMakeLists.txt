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
endif()

setup_module()

if (AU_BUILD_AU3AUDIO_TESTS)
    add_subdirectory(tests)
endif()
