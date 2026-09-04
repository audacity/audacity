#
# Audacity: A Digital Audio Editor
#

declare_module(video)

set(MODULE_QRC video.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/videomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/videomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/videotypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ivideoservice.h
    ${CMAKE_CURRENT_LIST_DIR}/videoattachpolicy.h
    ${CMAKE_CURRENT_LIST_DIR}/ivideodecodebackend.h

    #internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/videodecodeworker.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videodecodeworker.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoframecache.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoframecache.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videosyncclock.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videosyncclock.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videopathresolve.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videopathresolve.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoerrors.cpp

    #internal/ffmpeg
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/ffmpegsoftwarebackend.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/ffmpegsoftwarebackend.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/pixelconvert.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/pixelconvert.h

    #view
    ${CMAKE_CURRENT_LIST_DIR}/view/videopanelmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/videopanelmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/videopanelmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/videopanelmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/videosurfaceitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/videosurfaceitem.h
    )

set(MODULE_LINK
        au3wrap
        context
        au3-ffmpeg-support
        au3-mod-ffmpeg
)

# au3 headers leak wxWidgets macros, which a unity build turns into
# cross-translation-unit collisions.
set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_VIDEO_TESTS)
    add_subdirectory(tests)
endif()
