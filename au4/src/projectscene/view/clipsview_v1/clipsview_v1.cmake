

set(CLIPSVIEW_V1_SRC
    ${CMAKE_CURRENT_LIST_DIR}/trackslistclipsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/trackslistclipsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/clipslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/clipslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/clipkey.cpp
    ${CMAKE_CURRENT_LIST_DIR}/clipkey.h

   ${CMAKE_CURRENT_LIST_DIR}/items/WaveClipItem.cpp
   ${CMAKE_CURRENT_LIST_DIR}/items/WaveClipItem.h
   ${CMAKE_CURRENT_LIST_DIR}/TimelineContext.cpp
   ${CMAKE_CURRENT_LIST_DIR}/TimelineContext.h
   ${CMAKE_CURRENT_LIST_DIR}/WaveView.cpp
   ${CMAKE_CURRENT_LIST_DIR}/WaveView.h

   ${CMAKE_CURRENT_LIST_DIR}/WaveformScale.cpp
   ${CMAKE_CURRENT_LIST_DIR}/WaveformScale.h

   ${CMAKE_CURRENT_LIST_DIR}/converter.cpp
   ${CMAKE_CURRENT_LIST_DIR}/converter.h
   ${CMAKE_CURRENT_LIST_DIR}/ui.h
)

# NOTE Temporary, proof of concept
set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
set(CLIPSVIEW_V1_INC
    ${wxwidgets_INCLUDE_DIRS}
    ${expat_INCLUDE_DIRS}
    ${PortAudio_INCLUDE_DIR}

    ${AUDACITY_ROOT}/lib-src/sqlite
    ${AUDACITY_ROOT}/lib-src/pffft
    ${AUDACITY_ROOT}/lib-src/libsoxr/src
    ${AUDACITY_ROOT}/lib-src/portmixer

    ${AUDACITY_ROOT}
    # compile lib-project
    ${AU3_LIBRARIES}/lib-project
    ${AU3_LIBRARIES}/lib-registries
    ${AU3_LIBRARIES}/lib-exceptions
    ${AU3_LIBRARIES}/lib-utility
    ${AU3_LIBRARIES}/lib-strings
    ${AU3_LIBRARIES}/lib-string-utils
    ${AU3_LIBRARIES}/lib-preferences
    ${AU3_LIBRARIES}/lib-xml
    ${AU3_LIBRARIES}/lib-components
    ${AU3_LIBRARIES}/lib-basic-ui
    ${AU3_LIBRARIES}/lib-files
    # compile lib-project-file-io
    ${AU3_LIBRARIES}/lib-sentry-reporting
    ${AU3_LIBRARIES}/lib-math
    ${AU3_LIBRARIES}/lib-project-history
    ${AU3_LIBRARIES}/lib-transactions
    ${AU3_LIBRARIES}/lib-stretching-sequence
    ${AU3_LIBRARIES}/lib-wave-track
    ${AU3_LIBRARIES}/lib-sample-track
    ${AU3_LIBRARIES}/lib-mixer
    ${AU3_LIBRARIES}/lib-audio-graph
    ${AU3_LIBRARIES}/lib-playable-track
    ${AU3_LIBRARIES}/lib-track
    ${AU3_LIBRARIES}/lib-channel
    ${AU3_LIBRARIES}/lib-time-and-pitch
    ${AU3_LIBRARIES}/lib-project-rate
    ${AU3_LIBRARIES}/lib-track-selection
    ${AU3_LIBRARIES}/lib-audio-devices

    # compile lib-audio-io
    ${AU3_LIBRARIES}/lib-audio-io
    ${AU3_LIBRARIES}/lib-realtime-effects
    ${AU3_LIBRARIES}/lib-module-manager

    ${AU3_LIBRARIES}/lib-screen-geometry

    # compile after update 04.26
    ${AU3_LIBRARIES}/lib-sqlite-helpers

    # track paint
    ${AU3_LIBRARIES}/lib-time-frequency-selection
    ${AU3_LIBRARIES}/lib-graphics
    ${AU3_LIBRARIES}/lib-wave-track-paint
)

set(CLIPSVIEW_V1_DEF
    -DAUDACITY_VERSION=4
    -DAUDACITY_RELEASE=0
    -DAUDACITY_REVISION=0
    -DAUDACITY_MODLEVEL=0

    # Version string for visual display
    -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

    # This value is used in the resource compiler for Windows
    -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"


    -DUTILITY_API=
    -DPROJECT_API=
    -DSTRINGS_API=
    -DEXCEPTIONS_API=
    -DREGISTRIES_API=
    -DPREFERENCES_API=
    -DXML_API=
    -DBASIC_UI_API=
    -DCOMPONENTS_API=
    -DSTRING_UTILS_API=
    -DFILES_API=
    -DPROJECT_FILE_IO_API=
    -DPROJECT_HISTORY_API=
    -DMATH_API=
    -DTRANSACTIONS_API=
    -DSTRETCHING_SEQUENCE_API=
    -DWAVE_TRACK_API=
    -DSAMPLE_TRACK_API=
    -DMIXER_API=
    -DAUDIO_GRAPH_API=
    -DPLAYABLE_TRACK_API=
    -DTRACK_API=
    -DCHANNEL_API=
    -DTIME_AND_PITCH_API=
    -DPROJECT_RATE_API=
    -DTRACK_SELECTION_API=
    -DAUDIO_DEVICES_API=
    -DAUDIO_IO_API=
    -DREALTIME_EFFECTS_API=
    -DMODULE_MANAGER_API=
    -DWX_INIT_API=

    -DTIME_FREQUENCY_SELECTION_API=
    -DSCREEN_GEOMETRY_API=
    -DSQLITE_HELPERS_API=

    -DGRAPHICS_API=
    -DWAVE_TRACK_PAINT_API=

    -DEXPERIMENTAL_SPECTRAL_EDITING
)

set(WXBASE_RESTRICTIONS
   "wxUSE_GUI=0"

   # Don't use app.h
   _WX_APP_H_BASE_

   # Don't use evtloop.h
   _WX_EVTLOOP_H_

   # Don't use image.h
   _WX_IMAGE_H

   # Don't use colour.h
   _WX_COLOUR_H_BASE_

   # Don't use brush.h
   _WX_BRUSH_H_BASE_

   # Don't use pen.h
   _WX_PEN_H_BASE_
)

set(WXBASE_DEFS
    ${WXBASE_RESTRICTIONS}
)

include(GetPlatformInfo)

if (OS_IS_LIN)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXGTK__)
elseif(OS_IS_MAC)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXMAC__)
    # find_library(CoreAudio NAMES CoreAudio)
    # find_library(CoreAudioKit NAMES CoreAudioKit)
    # set(MODULE_LINK ${MODULE_LINK} ${CoreAudio} ${CoreAudioKit})

elseif(OS_IS_WIN)
   set(WXBASE_DEFS ${WXBASE_DEFS} __WXMSW__ WXUSINGDLL)
   # set(MODULE_LINK ${MODULE_LINK} zlib::zlib winmm mmdevapi mfplat)
endif()

set(CLIPSVIEW_V1_DEF ${CLIPSVIEW_V1_DEF} ${WXBASE_DEFS})
