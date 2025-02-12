

set(AU3_DEF

    -DAUDACITY_VERSION=4
    -DAUDACITY_RELEASE=0
    -DAUDACITY_REVISION=0
    -DAUDACITY_MODLEVEL=0

    # Version string for visual display
    -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

    # This value is used in the resource compiler for Windows
    -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"

    -Dsafenew=new

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
    -DVIEWPORT_API=
    -DSNAPPING_API=
    -DNUMERIC_FORMATS_API=
    -DLABEL_TRACK_API=

    -DTIME_FREQUENCY_SELECTION_API=
    -DSCREEN_GEOMETRY_API=
    -DSQLITE_HELPERS_API=

    -DGRAPHICS_API=
    -DWAVE_TRACK_PAINT_API=

    -DEXPERIMENTAL_SPECTRAL_EDITING

    -DEFFECTS_API=
    -DVST3_API=

    -DCOMMAND_PARAMETERS_API=
    -DAUDACITY_APPLICATION_LOGIC_API=
    -DMENUS_API=
    -DDYNAMIC_RANGE_PROCESSOR_API=

    -DIMPORT_EXPORT_API=
    -DTAGS_API=

    -DFFMPEG_SUPPORT_API=
)

set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
set(AU3_MODULES ${AUDACITY_ROOT}/modules)
set(AU3_INCLUDE
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
    ${AU3_LIBRARIES}/lib-label-track
    ${AU3_LIBRARIES}/lib-mixer
    ${AU3_LIBRARIES}/lib-audio-graph
    ${AU3_LIBRARIES}/lib-playable-track
    ${AU3_LIBRARIES}/lib-track
    ${AU3_LIBRARIES}/lib-channel
    ${AU3_LIBRARIES}/lib-time-and-pitch
    ${AU3_LIBRARIES}/lib-project-rate
    ${AU3_LIBRARIES}/lib-track-selection
    ${AU3_LIBRARIES}/lib-audio-devices
    ${AU3_LIBRARIES}/lib-viewport
    ${AU3_LIBRARIES}/lib-snapping
    ${AU3_LIBRARIES}/lib-numeric-formats

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

    # effects
    ${AU3_LIBRARIES}/lib-effects
    ${AU3_LIBRARIES}/lib-audio-unit
    ${AU3_LIBRARIES}/lib-command-parameters
    ${AU3_LIBRARIES}/lib-menus

    ${AU3_LIBRARIES}/lib-vst3

    ${AU3_MODULES}/import-export/mod-ffmpeg
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

set(AU3_LINK
    wxwidgets::wxwidgets
    expat::expat
    portaudio::portaudio
    sqlite
    soxr
    pffft
    portmixer
)

set(WXBASE_DEFS
    ${WXBASE_RESTRICTIONS}
)

include(GetPlatformInfo)

if (OS_IS_LIN)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXGTK__)
elseif(OS_IS_MAC)
    set(WXBASE_DEFS ${WXBASE_DEFS} __WXMAC__)
    find_library(CoreAudio NAMES CoreAudio)
    find_library(CoreAudioKit NAMES CoreAudioKit)
    set(AU3_LINK zlib::zlib libjpeg-turbo::libjpeg-turbo libpng::libpng ${AU3_LINK} ${CoreAudio} ${CoreAudioKit})

elseif(OS_IS_WIN)
   set(WXBASE_DEFS ${WXBASE_DEFS} __WXMSW__ WXUSINGDLL)
   set(AU3_LINK ${AU3_LINK} zlib::zlib libjpeg-turbo::libjpeg-turbo libpng::libpng winmm mmdevapi mfplat)
endif()

set(AU3_DEF ${AU3_DEF} ${WXBASE_DEFS})
