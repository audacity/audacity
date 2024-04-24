# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2021 MuseScore BVBA and others
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

# This module stores global state
declare_module(au3wrap)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3wrapmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3wrapmodule.h

    ${CMAKE_CURRENT_LIST_DIR}/wxtypes_convert.h
    ${CMAKE_CURRENT_LIST_DIR}/audacity3project.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacity3project.h
    ${CMAKE_CURRENT_LIST_DIR}/audacity3playback.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacity3playback.h

    ${CMAKE_CURRENT_LIST_DIR}/mocks/au3settingsmock.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/au3settingsmock.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/wxlogwrap.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/wxlogwrap.h
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

    -DEXPERIMENTAL_SPECTRAL_EDITING
)

set(AU3_LIBRARIES ${AUDACITY_ROOT}/libraries)
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
)

set(AU3_ADD_AS_SRC ON)
if (AU3_ADD_AS_SRC)
    set(AU3_SRC
    # Create project
    ${AU3_LIBRARIES}/lib-project/Project.cpp
    ${AU3_LIBRARIES}/lib-project/Project.h
    # dependencies of project-file-io:
    ${AU3_LIBRARIES}/lib-project/ProjectFormatExtensionsRegistry.cpp
    ${AU3_LIBRARIES}/lib-project/ProjectFormatExtensionsRegistry.h
    ${AU3_LIBRARIES}/lib-project/ProjectFormatVersion.cpp
    ${AU3_LIBRARIES}/lib-project/ProjectFormatVersion.h
    # dependencies of project:
    # xml
    ${AU3_LIBRARIES}/lib-xml/XMLAttributeValueView.cpp
    ${AU3_LIBRARIES}/lib-xml/XMLAttributeValueView.h
    ${AU3_LIBRARIES}/lib-xml/XMLFileReader.cpp
    ${AU3_LIBRARIES}/lib-xml/XMLFileReader.h
    ${AU3_LIBRARIES}/lib-xml/XMLMethodRegistry.cpp
    ${AU3_LIBRARIES}/lib-xml/XMLMethodRegistry.h
    ${AU3_LIBRARIES}/lib-xml/XMLTagHandler.cpp
    ${AU3_LIBRARIES}/lib-xml/XMLTagHandler.h
    ${AU3_LIBRARIES}/lib-xml/XMLWriter.cpp
    ${AU3_LIBRARIES}/lib-xml/XMLWriter.h
    # string-utils
    ${AU3_LIBRARIES}/lib-string-utils/HexHelpers.h
    ${AU3_LIBRARIES}/lib-string-utils/UrlEncode.h
    ${AU3_LIBRARIES}/lib-string-utils/UrlEncode.cpp
    ${AU3_LIBRARIES}/lib-string-utils/UrlDecode.h
    ${AU3_LIBRARIES}/lib-string-utils/UrlDecode.cpp
    ${AU3_LIBRARIES}/lib-string-utils/CodeConversions.h
    ${AU3_LIBRARIES}/lib-string-utils/CodeConversions.cpp
    ${AU3_LIBRARIES}/lib-string-utils/DateTimeConversions.h
    ${AU3_LIBRARIES}/lib-string-utils/DateTimeConversions.cpp
    ${AU3_LIBRARIES}/lib-string-utils/ToChars.h
    ${AU3_LIBRARIES}/lib-string-utils/ToChars.cpp
    ${AU3_LIBRARIES}/lib-string-utils/FromChars.h
    ${AU3_LIBRARIES}/lib-string-utils/FromChars.cpp
    # basic-ui
    ${AU3_LIBRARIES}/lib-basic-ui/BasicUI.cpp
    ${AU3_LIBRARIES}/lib-basic-ui/BasicUI.h
    ${AU3_LIBRARIES}/lib-basic-ui/BasicUIPoint.h

    # Load project:
    # project-file-io
    ${AU3_LIBRARIES}/lib-project-file-io/ActiveProjects.cpp
    ${AU3_LIBRARIES}/lib-project-file-io/ActiveProjects.h
    ${AU3_LIBRARIES}/lib-project-file-io/DBConnection.cpp
    ${AU3_LIBRARIES}/lib-project-file-io/DBConnection.h
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectFileIO.cpp
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectFileIO.h
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectSerializer.cpp
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectSerializer.h
    ${AU3_LIBRARIES}/lib-project-file-io/SqliteSampleBlock.cpp

    ${AU3_LIBRARIES}/lib-project-history/ProjectHistory.cpp
    ${AU3_LIBRARIES}/lib-project-history/ProjectHistory.h
    ${AU3_LIBRARIES}/lib-project-history/UndoManager.cpp
    ${AU3_LIBRARIES}/lib-project-history/UndoManager.h

    # dependencies of project-file-io:
    ${AU3_LIBRARIES}/lib-strings/TranslatableString.cpp
    ${AU3_LIBRARIES}/lib-strings/TranslatableString.h
    ${AU3_LIBRARIES}/lib-strings/Internat.cpp
    ${AU3_LIBRARIES}/lib-strings/Internat.h
    ${AU3_LIBRARIES}/lib-strings/Base64.cpp
    ${AU3_LIBRARIES}/lib-strings/Base64.h

    ${AU3_LIBRARIES}/lib-exceptions/AudacityException.cpp
    ${AU3_LIBRARIES}/lib-exceptions/AudacityException.h
    ${AU3_LIBRARIES}/lib-exceptions/InconsistencyException.cpp
    ${AU3_LIBRARIES}/lib-exceptions/InconsistencyException.h
    ${AU3_LIBRARIES}/lib-exceptions/UserException.cpp
    ${AU3_LIBRARIES}/lib-exceptions/UserException.h

    ${AU3_LIBRARIES}/lib-registries/AttachedVirtualFunction.h
    ${AU3_LIBRARIES}/lib-registries/ClientData.cpp
    ${AU3_LIBRARIES}/lib-registries/ClientData.h
    ${AU3_LIBRARIES}/lib-registries/ClientDataHelpers.h
    ${AU3_LIBRARIES}/lib-registries/Registrar.h
    ${AU3_LIBRARIES}/lib-registries/Registry.cpp
    ${AU3_LIBRARIES}/lib-registries/Registry.h

    ${AU3_LIBRARIES}/lib-utility/Observer.cpp
    ${AU3_LIBRARIES}/lib-utility/Observer.h
    ${AU3_LIBRARIES}/lib-utility/MemoryStream.cpp
    ${AU3_LIBRARIES}/lib-utility/MemoryStream.h
    ${AU3_LIBRARIES}/lib-utility/BufferedStreamReader.cpp
    ${AU3_LIBRARIES}/lib-utility/BufferedStreamReader.h
    ${AU3_LIBRARIES}/lib-utility/MemoryX.cpp
    ${AU3_LIBRARIES}/lib-utility/MemoryX.h
    ${AU3_LIBRARIES}/lib-utility/ModuleConstants.cpp
    ${AU3_LIBRARIES}/lib-utility/ModuleConstants.h

    ${AU3_LIBRARIES}/lib-files/AudacityLogger.cpp
    ${AU3_LIBRARIES}/lib-files/AudacityLogger.h
    ${AU3_LIBRARIES}/lib-files/FileException.cpp
    ${AU3_LIBRARIES}/lib-files/FileException.h
    ${AU3_LIBRARIES}/lib-files/FileIO.cpp
    ${AU3_LIBRARIES}/lib-files/FileIO.h
    ${AU3_LIBRARIES}/lib-files/FileNames.cpp
    ${AU3_LIBRARIES}/lib-files/FileNames.h
    ${AU3_LIBRARIES}/lib-files/PathList.cpp
    ${AU3_LIBRARIES}/lib-files/PathList.h
    ${AU3_LIBRARIES}/lib-files/PlatformCompatibility.cpp
    ${AU3_LIBRARIES}/lib-files/PlatformCompatibility.h
    ${AU3_LIBRARIES}/lib-files/TempDirectory.cpp
    ${AU3_LIBRARIES}/lib-files/TempDirectory.h
    ${AU3_LIBRARIES}/lib-files/wxFileNameWrapper.h

    ${AU3_LIBRARIES}/lib-track/TimeWarper.cpp
    ${AU3_LIBRARIES}/lib-track/TimeWarper.h
    ${AU3_LIBRARIES}/lib-track/Track.cpp
    ${AU3_LIBRARIES}/lib-track/Track.h
    ${AU3_LIBRARIES}/lib-track/TrackAttachment.cpp
    ${AU3_LIBRARIES}/lib-track/TrackAttachment.h

    ${AU3_LIBRARIES}/lib-wave-track/WaveTrack.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveTrack.h
    ${AU3_LIBRARIES}/lib-wave-track/Sequence.cpp
    ${AU3_LIBRARIES}/lib-wave-track/Sequence.h
    ${AU3_LIBRARIES}/lib-wave-track/WaveClip.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveClip.h
    ${AU3_LIBRARIES}/lib-wave-track/SampleBlock.cpp
    ${AU3_LIBRARIES}/lib-wave-track/SampleBlock.h
    ${AU3_LIBRARIES}/lib-wave-track/WideClip.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WideClip.h

    ${AU3_LIBRARIES}/lib-sample-track/SampleTrack.cpp
    ${AU3_LIBRARIES}/lib-sample-track/SampleTrack.h

    ${AU3_LIBRARIES}/lib-preferences/Prefs.cpp
    ${AU3_LIBRARIES}/lib-preferences/Prefs.h
    ${AU3_LIBRARIES}/lib-preferences/BasicSettings.cpp
    ${AU3_LIBRARIES}/lib-preferences/BasicSettings.h

    ${AU3_LIBRARIES}/lib-transactions/TransactionScope.cpp
    ${AU3_LIBRARIES}/lib-transactions/TransactionScope.h

    ${AU3_LIBRARIES}/lib-channel/Channel.cpp
    ${AU3_LIBRARIES}/lib-channel/Channel.h

    ${AU3_LIBRARIES}/lib-mixer/WideSampleSequence.cpp
    ${AU3_LIBRARIES}/lib-mixer/WideSampleSequence.h
    ${AU3_LIBRARIES}/lib-mixer/Envelope.cpp
    ${AU3_LIBRARIES}/lib-mixer/Envelope.h
    ${AU3_LIBRARIES}/lib-mixer/AudioIOSequences.cpp
    ${AU3_LIBRARIES}/lib-mixer/AudioIOSequences.h

    ${AU3_LIBRARIES}/lib-track-selection/SyncLock.cpp
    ${AU3_LIBRARIES}/lib-track-selection/SyncLock.h

    ${AU3_LIBRARIES}/lib-project-rate/Decibels.cpp
    ${AU3_LIBRARIES}/lib-project-rate/Decibels.h
    ${AU3_LIBRARIES}/lib-project-rate/ProjectRate.cpp
    ${AU3_LIBRARIES}/lib-project-rate/ProjectRate.h
    ${AU3_LIBRARIES}/lib-project-rate/QualitySettings.cpp
    ${AU3_LIBRARIES}/lib-project-rate/QualitySettings.h

    ${AU3_LIBRARIES}/lib-time-and-pitch/StaffPadTimeAndPitch.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/StaffPadTimeAndPitch.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/AudioContainer.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/AudioContainer.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/TimeAndPitchExperimentalSettings.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/TimeAndPitchExperimentalSettings.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/TimeAndPitchInterface.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/TimeAndPitchInterface.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/DummyFormantShifterLogger.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/DummyFormantShifterLogger.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/FormantShifter.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/FormantShifter.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/FormantShifterLogger.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/FormantShifterLogger.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/StaffPad/TimeAndPitch.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/StaffPad/TimeAndPitch.h
    ${AU3_LIBRARIES}/lib-time-and-pitch/StaffPad/FourierTransform_pffft.cpp
    ${AU3_LIBRARIES}/lib-time-and-pitch/StaffPad/FourierTransform_pffft.h

    ${AU3_LIBRARIES}/lib-playable-track/PlayableTrack.cpp
    ${AU3_LIBRARIES}/lib-playable-track/PlayableTrack.h

    ${AU3_LIBRARIES}/lib-audio-devices/AudioIOBase.cpp
    ${AU3_LIBRARIES}/lib-audio-devices/AudioIOBase.h
    ${AU3_LIBRARIES}/lib-audio-devices/DeviceManager.cpp
    ${AU3_LIBRARIES}/lib-audio-devices/DeviceManager.h

    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphChannel.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphChannel.h
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphBuffers.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphBuffers.h
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSource.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSource.h

    ${AU3_LIBRARIES}/lib-math/SampleFormat.cpp
    ${AU3_LIBRARIES}/lib-math/SampleFormat.h
    ${AU3_LIBRARIES}/lib-math/SampleCount.cpp
    ${AU3_LIBRARIES}/lib-math/SampleCount.h
    ${AU3_LIBRARIES}/lib-math/Resample.cpp
    ${AU3_LIBRARIES}/lib-math/Resample.h
    ${AU3_LIBRARIES}/lib-math/Dither.cpp
    ${AU3_LIBRARIES}/lib-math/Dither.h

    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegmentSampleView.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegmentSampleView.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/ClipInterface.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/ClipInterface.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/ClipTimeAndPitchSource.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/ClipTimeAndPitchSource.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/StretchingSequence.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/StretchingSequence.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegmentFactory.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegmentFactory.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/ClipSegment.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/ClipSegment.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegment.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegment.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/SilenceSegment.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/SilenceSegment.h
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegmentFactoryInterface.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/AudioSegmentFactoryInterface.h

    # end dependencies of project-file-io

    # When creating a project, audio devices are accessed, so we need to initialize the audio
    ${AU3_LIBRARIES}/lib-audio-io/AudioIO.cpp
    ${AU3_LIBRARIES}/lib-audio-io/AudioIO.h
    ${AU3_LIBRARIES}/lib-audio-io/AudioIOExt.cpp
    ${AU3_LIBRARIES}/lib-audio-io/AudioIOExt.h
    ${AU3_LIBRARIES}/lib-audio-io/AudioIOListener.cpp
    ${AU3_LIBRARIES}/lib-audio-io/AudioIOListener.h
    ${AU3_LIBRARIES}/lib-audio-io/PlaybackSchedule.cpp
    ${AU3_LIBRARIES}/lib-audio-io/PlaybackSchedule.h
    ${AU3_LIBRARIES}/lib-audio-io/ProjectAudioIO.cpp
    ${AU3_LIBRARIES}/lib-audio-io/ProjectAudioIO.h
    ${AU3_LIBRARIES}/lib-audio-io/RingBuffer.cpp
    ${AU3_LIBRARIES}/lib-audio-io/RingBuffer.h

    # begin dependencies of lib-audio-io
    ${AU3_LIBRARIES}/lib-realtime-effects/RealtimeEffectList.cpp
    ${AU3_LIBRARIES}/lib-realtime-effects/RealtimeEffectList.h
    ${AU3_LIBRARIES}/lib-realtime-effects/RealtimeEffectManager.cpp
    ${AU3_LIBRARIES}/lib-realtime-effects/RealtimeEffectManager.h
    ${AU3_LIBRARIES}/lib-realtime-effects/RealtimeEffectState.cpp
    ${AU3_LIBRARIES}/lib-realtime-effects/RealtimeEffectState.h

    ${AU3_LIBRARIES}/lib-mixer/Mix.cpp
    ${AU3_LIBRARIES}/lib-mixer/Mix.h
    ${AU3_LIBRARIES}/lib-mixer/MixerSource.cpp
    ${AU3_LIBRARIES}/lib-mixer/MixerSource.h
    ${AU3_LIBRARIES}/lib-mixer/EffectStage.cpp
    ${AU3_LIBRARIES}/lib-mixer/EffectStage.h
    ${AU3_LIBRARIES}/lib-mixer/MixerOptions.cpp
    ${AU3_LIBRARIES}/lib-mixer/MixerOptions.h

    ${AU3_LIBRARIES}/lib-module-manager/PluginManager.cpp
    ${AU3_LIBRARIES}/lib-module-manager/PluginManager.h
    ${AU3_LIBRARIES}/lib-module-manager/ModuleManager.cpp
    ${AU3_LIBRARIES}/lib-module-manager/ModuleManager.h
    ${AU3_LIBRARIES}/lib-module-manager/PluginDescriptor.cpp
    ${AU3_LIBRARIES}/lib-module-manager/PluginDescriptor.cpp
    ${AU3_LIBRARIES}/lib-module-manager/PluginInterface.cpp
    ${AU3_LIBRARIES}/lib-module-manager/PluginInterface.h
    ${AU3_LIBRARIES}/lib-module-manager/ModuleSettings.cpp
    ${AU3_LIBRARIES}/lib-module-manager/ModuleSettings.h

    ${AU3_LIBRARIES}/lib-components/EffectAutomationParameters.cpp
    ${AU3_LIBRARIES}/lib-components/EffectAutomationParameters.h
    ${AU3_LIBRARIES}/lib-components/EffectInterface.cpp
    ${AU3_LIBRARIES}/lib-components/EffectInterface.h
    ${AU3_LIBRARIES}/lib-components/ComponentInterface.cpp
    ${AU3_LIBRARIES}/lib-components/ComponentInterface.h
    ${AU3_LIBRARIES}/lib-components/ComponentInterfaceSymbol.h

    ${AU3_LIBRARIES}/lib-time-frequency-selection/SelectedRegion.cpp
    ${AU3_LIBRARIES}/lib-time-frequency-selection/SelectedRegion.h
    ${AU3_LIBRARIES}/lib-time-frequency-selection/ViewInfo.cpp
    ${AU3_LIBRARIES}/lib-time-frequency-selection/ViewInfo.h

    ${AU3_LIBRARIES}/lib-screen-geometry/ZoomInfo.cpp
    ${AU3_LIBRARIES}/lib-screen-geometry/ZoomInfo.h

    # end dependencies of lib-audio-io

    # Requires wxApp, so we won't use it
    # ${AU3_LIBRARIES}/lib-wx-init/SettingsWX.cpp
    # ${AU3_LIBRARIES}/lib-wx-init/SettingsWX.h
    )

    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/sqlite au3-sqlite)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/soxr au3-soxr)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/pffft au3-pffft)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/portmixer au3-portmixer)

    set(MODULE_LINK
        wxwidgets::wxwidgets
        expat::expat
        portaudio::portaudio
        sqlite
        soxr
        pffft
        portmixer
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

endif()

# ==================================
set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF} ${WXBASE_DEFS})
set(MODULE_SRC ${MODULE_SRC} ${AU3_SRC})

set(MODULE_USE_PCH_NONE ON)
set(MODULE_USE_UNITY_NONE ON)
setup_module()
