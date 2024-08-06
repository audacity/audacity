#
# Audacity: A Digital Audio Editor
#
declare_module(effects)

set(MODULE_QRC effects.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/effectstypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsparametersservice.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsprocessing.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsparametersservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsparametersservice.h

    ${CMAKE_CURRENT_LIST_DIR}/view/effectbuilder.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectbuilder.h

    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/builtineffects/amplify/amplify.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffects/amplify/amplify.h

    # au3wrap
    ${CMAKE_CURRENT_LIST_DIR}/au3wrap/au3effectsprocessing.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3wrap/au3effectsprocessing.h

    # effects helpers
    ${CMAKE_CURRENT_LIST_DIR}/builtineffects/StatefulPerTrackEffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffects/StatefulPerTrackEffect.h
    )


# ==================================
# === AU3 ===
# ==================================
set(AU3_DEF

    -DAUDACITY_VERSION=4
    -DAUDACITY_RELEASE=0
    -DAUDACITY_REVISION=0
    -DAUDACITY_MODLEVEL=0

    # Version string for visual display
    -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

    # This value is used in the resource compiler for Windows
    -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"

    -DEFFECTS_API=

    -DCOMPONENTS_API=
    -DSTRINGS_API=
    -DUTILITY_API=
    -DMATH_API=

    -DAUDIO_GRAPH_API=
    -DMIXER_API=
    -DEXCEPTIONS_API=
    -DTRACK_SELECTION_API=
    -DPREFERENCES_API=
    -DTRACK_API=
    -DWAVE_TRACK_API=
    -DCHANNEL_API=
    -DREGISTRIES_API=
    -DXML_API=
    -DBASIC_UI_API=
    -DFILES_API=
    -DTIME_FREQUENCY_SELECTION_API=
    -DSCREEN_GEOMETRY_API=
    -DSTRETCHING_SEQUENCE_API=
    -DSAMPLE_TRACK_API=
    -DPLAYABLE_TRACK_API=
    -DPROJECT_API=
    -DTIME_AND_PITCH_API=
    -DPROJECT_RATE_API=
    -DPROJECT_HISTORY_API=
    -DAUDIO_DEVICES_API=
    -DTRANSACTIONS_API=
    -DMODULE_MANAGER_API=
    -DNUMERIC_FORMATS_API=
    -DCOMMAND_PARAMETERS_API=
)

set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
set(AU3_INCLUDE
    ${wxwidgets_INCLUDE_DIRS}
    ${PortAudio_INCLUDE_DIR}

    ${AUDACITY_ROOT}

    ${AU3_LIBRARIES}/lib-effects

    ${AU3_LIBRARIES}/lib-components
    ${AU3_LIBRARIES}/lib-strings
    ${AU3_LIBRARIES}/lib-utility
    ${AU3_LIBRARIES}/lib-math

    ${AU3_LIBRARIES}/lib-audio-graph
    ${AU3_LIBRARIES}/lib-mixer
    ${AU3_LIBRARIES}/lib-exceptions
    ${AU3_LIBRARIES}/lib-track-selection
    ${AU3_LIBRARIES}/lib-preferences
    ${AU3_LIBRARIES}/lib-track
    ${AU3_LIBRARIES}/lib-wave-track
    ${AU3_LIBRARIES}/lib-channel
    ${AU3_LIBRARIES}/lib-registries
    ${AU3_LIBRARIES}/lib-xml
    ${AU3_LIBRARIES}/lib-basic-ui
    ${AU3_LIBRARIES}/lib-files
    ${AU3_LIBRARIES}/lib-time-frequency-selection
    ${AU3_LIBRARIES}/lib-screen-geometry
    ${AU3_LIBRARIES}/lib-stretching-sequence
    ${AU3_LIBRARIES}/lib-sample-track
    ${AU3_LIBRARIES}/lib-playable-track
    ${AU3_LIBRARIES}/lib-project
    ${AU3_LIBRARIES}/lib-time-and-pitch
    ${AU3_LIBRARIES}/lib-project-rate
    ${AU3_LIBRARIES}/lib-project-history
    ${AU3_LIBRARIES}/lib-audio-devices
    ${AU3_LIBRARIES}/lib-transactions
    ${AU3_LIBRARIES}/lib-module-manager
    ${AU3_LIBRARIES}/lib-numeric-formats
    ${AU3_LIBRARIES}/lib-command-parameters
)

set(AU3_ADD_AS_SRC ON)
if (AU3_ADD_AS_SRC)
    set(AU3_SRC
        ${AU3_LIBRARIES}/lib-effects/StatefulEffectBase.cpp
        ${AU3_LIBRARIES}/lib-effects/StatefulEffectBase.h
        ${AU3_LIBRARIES}/lib-effects/PerTrackEffect.cpp
        ${AU3_LIBRARIES}/lib-effects/PerTrackEffect.h
        ${AU3_LIBRARIES}/lib-effects/EffectBase.cpp
        ${AU3_LIBRARIES}/lib-effects/EffectBase.h
        ${AU3_LIBRARIES}/lib-effects/EffectPlugin.cpp
        ${AU3_LIBRARIES}/lib-effects/EffectPlugin.h
        ${AU3_LIBRARIES}/lib-effects/EffectOutputTracks.cpp
        ${AU3_LIBRARIES}/lib-effects/EffectOutputTracks.h
        ${AU3_LIBRARIES}/lib-effects/Effect.cpp
        ${AU3_LIBRARIES}/lib-effects/Effect.h

        ${AU3_LIBRARIES}/lib-components/EffectInterface.cpp
        ${AU3_LIBRARIES}/lib-components/EffectInterface.h
        ${AU3_LIBRARIES}/lib-components/SettingsVisitor.cpp
        ${AU3_LIBRARIES}/lib-components/SettingsVisitor.h

        ${AU3_LIBRARIES}/lib-strings/Identifier.cpp
        ${AU3_LIBRARIES}/lib-strings/Identifier.h

        ${AU3_LIBRARIES}/lib-utility/TypedAny.h

        ${AU3_LIBRARIES}/lib-math/SampleCount.cpp
        ${AU3_LIBRARIES}/lib-math/SampleCount.h

        ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSink.cpp
        ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSink.h
        ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphTask.cpp
        ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphTask.h

        ${AU3_LIBRARIES}/lib-mixer/EffectStage.cpp
        ${AU3_LIBRARIES}/lib-mixer/EffectStage.h
        ${AU3_LIBRARIES}/lib-mixer/WideSampleSource.cpp
        ${AU3_LIBRARIES}/lib-mixer/WideSampleSource.h

        ${AU3_LIBRARIES}/lib-exceptions/AudacityException.cpp
        ${AU3_LIBRARIES}/lib-exceptions/AudacityException.h

        ${AU3_LIBRARIES}/lib-track-selection/SyncLock.cpp
        ${AU3_LIBRARIES}/lib-track-selection/SyncLock.h

        ${AU3_LIBRARIES}/lib-preferences/Prefs.cpp
        ${AU3_LIBRARIES}/lib-preferences/Prefs.h

        ${AU3_LIBRARIES}/lib-track/Track.cpp
        ${AU3_LIBRARIES}/lib-track/Track.h

        ${AU3_LIBRARIES}/lib-wave-track/WaveTrack.cpp
        ${AU3_LIBRARIES}/lib-wave-track/WaveTrack.h
        ${AU3_LIBRARIES}/lib-wave-track/WaveTrackSink.cpp
        ${AU3_LIBRARIES}/lib-wave-track/WaveTrackSink.h
        ${AU3_LIBRARIES}/lib-wave-track/TimeStretching.cpp
        ${AU3_LIBRARIES}/lib-wave-track/TimeStretching.h
        ${AU3_LIBRARIES}/lib-wave-track/WaveChannelUtilities.cpp
        ${AU3_LIBRARIES}/lib-wave-track/WaveChannelUtilities.h
        ${AU3_LIBRARIES}/lib-wave-track/WaveClipUtilities.cpp
        ${AU3_LIBRARIES}/lib-wave-track/WaveClipUtilities.h

        ${AU3_LIBRARIES}/lib-channel/Channel.cpp
        ${AU3_LIBRARIES}/lib-channel/Channel.h

        ${AU3_LIBRARIES}/lib-registries/ClientData.cpp
        ${AU3_LIBRARIES}/lib-registries/ClientData.h

        ${AU3_LIBRARIES}/lib-xml/XMLTagHandler.cpp
        ${AU3_LIBRARIES}/lib-xml/XMLTagHandler.h

        ${AU3_LIBRARIES}/lib-basic-ui/BasicUI.cpp
        ${AU3_LIBRARIES}/lib-basic-ui/BasicUI.h

        ${AU3_LIBRARIES}/lib-files/FileException.cpp
        ${AU3_LIBRARIES}/lib-files/FileException.h

        ${AU3_LIBRARIES}/lib-time-frequency-selection/ViewInfo.cpp
        ${AU3_LIBRARIES}/lib-time-frequency-selection/ViewInfo.h

        ${AU3_LIBRARIES}/lib-screen-geometry/ZoomInfo.cpp
        ${AU3_LIBRARIES}/lib-screen-geometry/ZoomInfo.h

        ${AU3_LIBRARIES}/lib-stretching-sequence/ClipInterface.cpp
        ${AU3_LIBRARIES}/lib-stretching-sequence/ClipInterface.h

        ${AU3_LIBRARIES}/lib-sample-track/SampleTrack.cpp
        ${AU3_LIBRARIES}/lib-sample-track/SampleTrack.h

        ${AU3_LIBRARIES}/lib-playable-track/PlayableTrack.cpp
        ${AU3_LIBRARIES}/lib-playable-track/PlayableTrack.h

        ${AU3_LIBRARIES}/lib-project/Project.cpp
        ${AU3_LIBRARIES}/lib-project/Project.h

        ${AU3_LIBRARIES}/lib-time-and-pitch/TimeAndPitchInterface.cpp
        ${AU3_LIBRARIES}/lib-time-and-pitch/TimeAndPitchInterface.h

        ${AU3_LIBRARIES}/lib-project-rate/ProjectRate.cpp
        ${AU3_LIBRARIES}/lib-project-rate/ProjectRate.h

        ${AU3_LIBRARIES}/lib-project-history/UndoManager.cpp
        ${AU3_LIBRARIES}/lib-project-history/UndoManager.h

        ${AU3_LIBRARIES}/lib-audio-devices/AudioIOBase.cpp
        ${AU3_LIBRARIES}/lib-audio-devices/AudioIOBase.h

        ${AU3_LIBRARIES}/lib-transactions/TransactionScope.cpp
        ${AU3_LIBRARIES}/lib-transactions/TransactionScope.h

        ${AU3_LIBRARIES}/lib-module-manager/ConfigInterface.cpp
        ${AU3_LIBRARIES}/lib-module-manager/ConfigInterface.h

        ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormats.cpp
        ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormats.h
        ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterRegistry.cpp
        ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterRegistry.h
        ${AU3_LIBRARIES}/lib-numeric-formats/ProjectNumericFormats.cpp
        ${AU3_LIBRARIES}/lib-numeric-formats/ProjectNumericFormats.h
        ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormatterContext.cpp
        ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormatterContext.h

        ${AU3_LIBRARIES}/lib-command-parameters/ShuttleAutomation.cpp
        ${AU3_LIBRARIES}/lib-command-parameters/ShuttleAutomation.h
    )

    set(MODULE_LINK
        wxwidgets::wxwidgets
        portaudio::portaudio
    )
endif()

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
    find_library(CoreAudio NAMES CoreAudio)
    find_library(CoreAudioKit NAMES CoreAudioKit)
    set(MODULE_LINK ${MODULE_LINK} ${CoreAudio} ${CoreAudioKit})

elseif(OS_IS_WIN)
   set(WXBASE_DEFS ${WXBASE_DEFS} __WXMSW__ WXUSINGDLL)
   set(MODULE_LINK ${MODULE_LINK} zlib::zlib libjpeg-turbo::libjpeg-turbo libpng::libpng winmm mmdevapi mfplat)
endif()

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF} ${WXBASE_DEFS})
set(MODULE_SRC ${MODULE_SRC} ${AU3_SRC})

install(DIRECTORY
      builtineffects
      DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}
      PATTERN ".*" EXCLUDE
      PATTERN "*.h" EXCLUDE
      PATTERN "*.cpp" EXCLUDE
      )

setup_module()
