#
# Audacity: A Digital Audio Editor
#

declare_module(video)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/videomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/videomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/videotypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ivideoservice.h
    ${CMAKE_CURRENT_LIST_DIR}/ivideodecodebackend.h

    #internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videoerrors.cpp

    #internal/ffmpeg
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/ffmpegsoftwarebackend.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/ffmpegsoftwarebackend.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/pixelconvert.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpeg/pixelconvert.h
    )

set(MODULE_LINK
        au3wrap
        au3-ffmpeg-support
        au3-mod-ffmpeg
)

# au3 headers leak wxWidgets macros, which a unity build turns into
# cross-translation-unit collisions.
set(MODULE_USE_UNITY OFF)

setup_module()
