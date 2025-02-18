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
    ${CMAKE_CURRENT_LIST_DIR}/au3types.h
    ${CMAKE_CURRENT_LIST_DIR}/iau3project.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/wxtypes_convert.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/wxlogwrap.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/wxlogwrap.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/domconverter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/domconverter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/progressdialog.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/progressdialog.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/domaccessor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/domaccessor.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3project.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3project.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiodevicesprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiodevicesprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3commonsettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3commonsettings.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3basicui.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3basicui.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.h
    )


if(OS_IS_WIN)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3platformcompatibilitywindows.cpp)
elseif(OS_IS_LIN)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3platformcompatibilitylinux.cpp)
elseif(OS_IS_MAC)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3platformcompatibilitymacos.cpp)
endif()

# ==================================
# === AU3 ===
# ==================================
include(${CMAKE_CURRENT_LIST_DIR}/au3defs.cmake)

set(AU3_SRC
    # Create project
    ${AU3_LIBRARIES}/lib-project/Project.cpp
    ${AU3_LIBRARIES}/lib-project/Project.h
    # dependencies of project-file-io:
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
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectFileIOExtension.cpp
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectFileIOExtension.h
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectSerializer.cpp
    ${AU3_LIBRARIES}/lib-project-file-io/ProjectSerializer.h
    ${AU3_LIBRARIES}/lib-project-file-io/SqliteSampleBlock.cpp

    ${AU3_LIBRARIES}/lib-sqlite-helpers/sqlite/SQLiteUtils.cpp
    ${AU3_LIBRARIES}/lib-sqlite-helpers/sqlite/SQLiteUtils.h
    ${AU3_LIBRARIES}/lib-sqlite-helpers/sqlite/Error.cpp
    ${AU3_LIBRARIES}/lib-sqlite-helpers/sqlite/Error.h

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
    ${AU3_LIBRARIES}/lib-strings/Identifier.cpp
    ${AU3_LIBRARIES}/lib-strings/Identifier.h

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
    ${AU3_LIBRARIES}/lib-utility/TypedAny.h

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
    ${AU3_LIBRARIES}/lib-track/UndoTracks.cpp
    ${AU3_LIBRARIES}/lib-track/UndoTracks.h
    ${AU3_LIBRARIES}/lib-track/PendingTracks.cpp
    ${AU3_LIBRARIES}/lib-track/PendingTracks.h
    ${AU3_LIBRARIES}/lib-track/ChannelAttachments.cpp
    ${AU3_LIBRARIES}/lib-track/ChannelAttachments.h

    ${AU3_LIBRARIES}/lib-wave-track/WaveTrack.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveTrack.h
    ${AU3_LIBRARIES}/lib-wave-track/Sequence.cpp
    ${AU3_LIBRARIES}/lib-wave-track/Sequence.h
    ${AU3_LIBRARIES}/lib-wave-track/WaveClip.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveClip.h
    ${AU3_LIBRARIES}/lib-wave-track/SampleBlock.cpp
    ${AU3_LIBRARIES}/lib-wave-track/SampleBlock.h
    ${AU3_LIBRARIES}/lib-wave-track/WaveTrackUtilities.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveTrackUtilities.h
    ${AU3_LIBRARIES}/lib-wave-track/WaveTrackSink.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveTrackSink.h
    ${AU3_LIBRARIES}/lib-wave-track/TimeStretching.cpp
    ${AU3_LIBRARIES}/lib-wave-track/TimeStretching.h
    ${AU3_LIBRARIES}/lib-wave-track/WaveChannelUtilities.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveChannelUtilities.h
    ${AU3_LIBRARIES}/lib-wave-track/WaveClipUtilities.cpp
    ${AU3_LIBRARIES}/lib-wave-track/WaveClipUtilities.h

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
    ${AU3_LIBRARIES}/lib-track-selection/TrackFocus.cpp
    ${AU3_LIBRARIES}/lib-track-selection/TrackFocus.h

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
    ${AU3_LIBRARIES}/lib-audio-devices/Meter.cpp
    ${AU3_LIBRARIES}/lib-audio-devices/Meter.h

    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphChannel.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphChannel.h
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphBuffers.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphBuffers.h
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSource.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSource.h
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSink.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphSink.h
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphTask.cpp
    ${AU3_LIBRARIES}/lib-audio-graph/AudioGraphTask.h

    ${AU3_LIBRARIES}/lib-math/SampleFormat.cpp
    ${AU3_LIBRARIES}/lib-math/SampleFormat.h
    ${AU3_LIBRARIES}/lib-math/SampleCount.cpp
    ${AU3_LIBRARIES}/lib-math/SampleCount.h
    ${AU3_LIBRARIES}/lib-math/Resample.cpp
    ${AU3_LIBRARIES}/lib-math/Resample.h
    ${AU3_LIBRARIES}/lib-math/Dither.cpp
    ${AU3_LIBRARIES}/lib-math/Dither.h
    ${AU3_LIBRARIES}/lib-math/InterpolateAudio.cpp
    ${AU3_LIBRARIES}/lib-math/InterpolateAudio.h
    ${AU3_LIBRARIES}/lib-math/Matrix.cpp
    ${AU3_LIBRARIES}/lib-math/Matrix.h

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
    ${AU3_LIBRARIES}/lib-stretching-sequence/TempoChange.cpp
    ${AU3_LIBRARIES}/lib-stretching-sequence/TempoChange.h

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
    ${AU3_LIBRARIES}/lib-mixer/DownmixSource.cpp
    ${AU3_LIBRARIES}/lib-mixer/DownmixSource.h
    ${AU3_LIBRARIES}/lib-mixer/DownmixStage.cpp
    ${AU3_LIBRARIES}/lib-mixer/DownmixStage.h
    ${AU3_LIBRARIES}/lib-mixer/MixerSource.cpp
    ${AU3_LIBRARIES}/lib-mixer/MixerSource.h
    ${AU3_LIBRARIES}/lib-mixer/MixerSource.cpp
    ${AU3_LIBRARIES}/lib-mixer/MixerSource.h
    ${AU3_LIBRARIES}/lib-mixer/EffectStage.cpp
    ${AU3_LIBRARIES}/lib-mixer/EffectStage.h
    ${AU3_LIBRARIES}/lib-mixer/MixerOptions.cpp
    ${AU3_LIBRARIES}/lib-mixer/MixerOptions.h
    ${AU3_LIBRARIES}/lib-mixer/WideSampleSource.cpp
    ${AU3_LIBRARIES}/lib-mixer/WideSampleSource.h

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
    ${AU3_LIBRARIES}/lib-module-manager/ConfigInterface.cpp
    ${AU3_LIBRARIES}/lib-module-manager/ConfigInterface.h

    ${AU3_LIBRARIES}/lib-components/EffectAutomationParameters.cpp
    ${AU3_LIBRARIES}/lib-components/EffectAutomationParameters.h
    ${AU3_LIBRARIES}/lib-components/EffectInterface.cpp
    ${AU3_LIBRARIES}/lib-components/EffectInterface.h
    ${AU3_LIBRARIES}/lib-components/ComponentInterface.cpp
    ${AU3_LIBRARIES}/lib-components/ComponentInterface.h
    ${AU3_LIBRARIES}/lib-components/ComponentInterfaceSymbol.h
    ${AU3_LIBRARIES}/lib-components/SettingsVisitor.cpp
    ${AU3_LIBRARIES}/lib-components/SettingsVisitor.h
    ${AU3_LIBRARIES}/lib-components/PluginProvider.cpp
    ${AU3_LIBRARIES}/lib-components/PluginProvider.h

    ${AU3_LIBRARIES}/lib-time-frequency-selection/SelectedRegion.cpp
    ${AU3_LIBRARIES}/lib-time-frequency-selection/SelectedRegion.h
    ${AU3_LIBRARIES}/lib-time-frequency-selection/ViewInfo.cpp
    ${AU3_LIBRARIES}/lib-time-frequency-selection/ViewInfo.h

    ${AU3_LIBRARIES}/lib-screen-geometry/ZoomInfo.cpp
    ${AU3_LIBRARIES}/lib-screen-geometry/ZoomInfo.h

    # end dependencies of lib-audio-io

    # track paint
    ${AU3_LIBRARIES}/lib-wave-track-paint/GraphicsDataCache.cpp
    ${AU3_LIBRARIES}/lib-wave-track-paint/GraphicsDataCache.h
    ${AU3_LIBRARIES}/lib-wave-track-paint/PixelSampleMapper.cpp
    ${AU3_LIBRARIES}/lib-wave-track-paint/PixelSampleMapper.h
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WaveBitmapCache.cpp
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WaveBitmapCache.h
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WaveDataCache.cpp
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WaveDataCache.h
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WaveData.cpp
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WaveData.h
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WavePaintParameters.cpp
    ${AU3_LIBRARIES}/lib-wave-track-paint/waveform/WavePaintParameters.h

    ${AU3_LIBRARIES}/lib-graphics/FrameStatistics.cpp
    ${AU3_LIBRARIES}/lib-graphics/FrameStatistics.h

    ${AU3_LIBRARIES}/lib-viewport/Viewport.cpp
    ${AU3_LIBRARIES}/lib-viewport/Viewport.h

    ${AU3_LIBRARIES}/lib-snapping/ProjectSnap.cpp
    ${AU3_LIBRARIES}/lib-snapping/ProjectSnap.h
    ${AU3_LIBRARIES}/lib-snapping/SnapUtils.cpp
    ${AU3_LIBRARIES}/lib-snapping/SnapUtils.h

    ${AU3_LIBRARIES}/lib-numeric-formats/ProjectTimeSignature.cpp
    ${AU3_LIBRARIES}/lib-numeric-formats/ProjectTimeSignature.h
    ${AU3_LIBRARIES}/lib-numeric-formats/Beats.cpp
    ${AU3_LIBRARIES}/lib-numeric-formats/Beats.h
    ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormats.cpp
    ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormats.h
    ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterRegistry.cpp
    ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterRegistry.h
    ${AU3_LIBRARIES}/lib-numeric-formats/ProjectNumericFormats.cpp
    ${AU3_LIBRARIES}/lib-numeric-formats/ProjectNumericFormats.h
    ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormatterContext.cpp
    ${AU3_LIBRARIES}/lib-numeric-formats/NumericConverterFormatterContext.h

    # Requires wxApp, so we won't use it
    # ${AU3_LIBRARIES}/lib-wx-init/SettingsWX.cpp
    # ${AU3_LIBRARIES}/lib-wx-init/SettingsWX.h

    ${AU3_LIBRARIES}/lib-effects/StatefulEffect.cpp
    ${AU3_LIBRARIES}/lib-effects/StatefulEffect.h
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
    ${AU3_LIBRARIES}/lib-effects/LoadEffects.cpp
    ${AU3_LIBRARIES}/lib-effects/LoadEffects.h

    ${AU3_LIBRARIES}/lib-audio-unit/AudioUnitEffectsModule.cpp
    ${AU3_LIBRARIES}/lib-audio-unit/AudioUnitEffectsModule.h

    ${AU3_LIBRARIES}/lib-command-parameters/ShuttleAutomation.cpp
    ${AU3_LIBRARIES}/lib-command-parameters/ShuttleAutomation.h
    ${AU3_LIBRARIES}/lib-command-parameters/ShuttlePrefs.cpp
    ${AU3_LIBRARIES}/lib-command-parameters/ShuttlePrefs.h
    ${AU3_LIBRARIES}/lib-command-parameters/WrappedType.cpp
    ${AU3_LIBRARIES}/lib-command-parameters/WrappedType.h

    ${AU3_LIBRARIES}/lib-menus/CommandContext.cpp
    ${AU3_LIBRARIES}/lib-menus/CommandContext.h
    ${AU3_LIBRARIES}/lib-menus/CommandFlag.cpp
    ${AU3_LIBRARIES}/lib-menus/CommandFlag.h
    ${AU3_LIBRARIES}/lib-menus/CommandFunctors.h
    ${AU3_LIBRARIES}/lib-menus/CommandManager.cpp
    ${AU3_LIBRARIES}/lib-menus/CommandManager.h
    ${AU3_LIBRARIES}/lib-menus/CommandTargets.cpp
    ${AU3_LIBRARIES}/lib-menus/CommandTargets.h
    ${AU3_LIBRARIES}/lib-menus/Keyboard.cpp
    ${AU3_LIBRARIES}/lib-menus/Keyboard.h
    ${AU3_LIBRARIES}/lib-menus/MenuRegistry.cpp
    ${AU3_LIBRARIES}/lib-menus/MenuRegistry.h

    ${AU3_LIBRARIES}/lib-audacity-application-logic/AudacityApplicationLogic.cpp
    ${AU3_LIBRARIES}/lib-audacity-application-logic/AudacityApplicationLogic.h
    ${AU3_LIBRARIES}/lib-audacity-application-logic/AudacityApplicationLogicTypes.h

    ${AU3_LIBRARIES}/lib-effects/EffectBase.cpp
    ${AU3_LIBRARIES}/lib-effects/EffectBase.h
    ${AU3_LIBRARIES}/lib-effects/Effect.cpp
    ${AU3_LIBRARIES}/lib-effects/Effect.h
    ${AU3_LIBRARIES}/lib-effects/EffectManager.cpp
    ${AU3_LIBRARIES}/lib-effects/EffectManager.h
    ${AU3_LIBRARIES}/lib-effects/EffectOutputTracks.cpp
    ${AU3_LIBRARIES}/lib-effects/EffectOutputTracks.h
    ${AU3_LIBRARIES}/lib-effects/EffectPlugin.cpp
    ${AU3_LIBRARIES}/lib-effects/EffectPlugin.h
    ${AU3_LIBRARIES}/lib-effects/LoadEffects.cpp
    ${AU3_LIBRARIES}/lib-effects/LoadEffects.h
    ${AU3_LIBRARIES}/lib-effects/MixAndRender.cpp
    ${AU3_LIBRARIES}/lib-effects/MixAndRender.h
    ${AU3_LIBRARIES}/lib-effects/PerTrackEffect.cpp
    ${AU3_LIBRARIES}/lib-effects/PerTrackEffect.h
    ${AU3_LIBRARIES}/lib-effects/StatefulEffectBase.cpp
    ${AU3_LIBRARIES}/lib-effects/StatefulEffectBase.h

    ${AU3_LIBRARIES}/lib-dynamic-range-processor/CompressorProcessor.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/CompressorProcessor.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DownwardMeterValueProvider.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DownwardMeterValueProvider.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorClock.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorClock.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorHistory.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorHistory.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorUtils.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/DynamicRangeProcessorUtils.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/MeterValueProvider.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/MeterValueProvider.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/UpwardMeterValueProvider.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/UpwardMeterValueProvider.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/SimpleCompressor/GainReductionComputer.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/SimpleCompressor/GainReductionComputer.h
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/SimpleCompressor/LookAheadGainReduction.cpp
    ${AU3_LIBRARIES}/lib-dynamic-range-processor/SimpleCompressor/LookAheadGainReduction.h

    ${AU3_LIBRARIES}/lib-label-track/LabelTrack.cpp
    ${AU3_LIBRARIES}/lib-label-track/LabelTrack.h

    ${AU3_LIBRARIES}/lib-import-export/Import.cpp
    ${AU3_LIBRARIES}/lib-import-export/Import.h
    ${AU3_LIBRARIES}/lib-import-export/ImportUtils.cpp
    ${AU3_LIBRARIES}/lib-import-export/ImportUtils.h
    ${AU3_LIBRARIES}/lib-import-export/ImportProgressListener.cpp
    ${AU3_LIBRARIES}/lib-import-export/ImportProgressListener.h
    ${AU3_LIBRARIES}/lib-import-export/ImportPlugin.cpp
    ${AU3_LIBRARIES}/lib-import-export/ImportPlugin.h
    ${AU3_LIBRARIES}/lib-import-export/ImportForwards.h

    ${AU3_LIBRARIES}/lib-file-formats/AcidizerTags.h

    ${AU3_LIBRARIES}/lib-tags/Tags.cpp
    ${AU3_LIBRARIES}/lib-tags/Tags.h
)

if(AU_MODULE_EFFECTS_VST)

    # VST3 SDK
    add_subdirectory(vst3)

    set(MODULE_SRC ${MODULE_SRC}
        ${AU3_LIBRARIES}/lib-vst3/AudacityVst3HostApplication.cpp
        ${AU3_LIBRARIES}/lib-vst3/AudacityVst3HostApplication.h
        ${AU3_LIBRARIES}/lib-vst3/VST3EffectBase.cpp
        ${AU3_LIBRARIES}/lib-vst3/VST3EffectBase.h
        ${AU3_LIBRARIES}/lib-vst3/VST3EffectsModule.cpp
        ${AU3_LIBRARIES}/lib-vst3/VST3EffectsModule.h
        ${AU3_LIBRARIES}/lib-vst3/VST3Instance.cpp
        ${AU3_LIBRARIES}/lib-vst3/VST3Instance.h
        ${AU3_LIBRARIES}/lib-vst3/VST3Utils.cpp
        ${AU3_LIBRARIES}/lib-vst3/VST3Utils.h
        ${AU3_LIBRARIES}/lib-vst3/VST3Wrapper.cpp
        ${AU3_LIBRARIES}/lib-vst3/VST3Wrapper.h
        ${AU3_LIBRARIES}/lib-vst3/internal/ConnectionProxy.cpp
        ${AU3_LIBRARIES}/lib-vst3/internal/ConnectionProxy.h
    )
endif()

if(AU_USE_FFMPEG)
    set(MODULE_SRC ${MODULE_SRC}
        ${AU3_MODULES}/import-export/mod-ffmpeg/ImportFFmpeg.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/FFmpegDefines.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/FFmpeg.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/FFmpeg.h

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegTypes.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/AVCodecFunctions.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/AVCodecID.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/AVFormatFunctions.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/AVUtilFunctions.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/FifoBuffer.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/FifoBuffer.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVChannelLayoutWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecContextWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecContextWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVDictionaryWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVDictionaryWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFormatContextWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFormatContextWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVInputFormatWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVInputFormatWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVIOContextWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVIOContextWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVOutputFormatWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVOutputFormatWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVPacketWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVPacketWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVStreamWrapper.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVStreamWrapper.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/DynamicLibraryHelpers.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/DynamicLibraryHelpers.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/FFmpegAPIResolver.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/FFmpegAPIResolver.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/FFmpegLog.h

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-2.3.6-single-header.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/52/avconfig.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/55/AVCodecIDLookup.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/55/AVCodecImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/55/AVFormatImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/52/AVUtilImpl.cpp

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-3.4.8-single-header.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/55/avconfig.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/57/AVCodecIDLookup.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/57/AVCodecImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/57/AVFormatImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/55/AVUtilImpl.cpp

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-4.2.4-single-header.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/56/avconfig.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/58/AVCodecIDLookup.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/58/AVCodecImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/58/AVFormatImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/56/AVUtilImpl.cpp

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-5.0.1-single-header.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/57/avconfig.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/59/AVCodecIDLookup.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/59/AVCodecImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/59/AVFormatImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/57/AVUtilImpl.cpp

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-6.0.0-single-header.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/58/avconfig.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/60/AVCodecIDLookup.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/60/AVCodecImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/60/AVFormatImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/58/AVUtilImpl.cpp

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-7.0.0-single-header.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/59/avconfig.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/61/AVCodecIDLookup.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/61/AVCodecImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/61/AVFormatImpl.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/59/AVUtilImpl.cpp

        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/AVCodecFunctionsLoader.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/AVCodecFunctionsLoader.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/AVFormatFunctionsLoader.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/AVFormatFunctionsLoader.h
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/AVUtilFunctionsLoader.cpp
        ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/AVUtilFunctionsLoader.h
    )
endif()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/sqlite au3-sqlite)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/soxr au3-soxr)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/pffft au3-pffft)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/portmixer au3-portmixer)


# ==================================
set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_SRC ${MODULE_SRC} ${AU3_SRC})
set(MODULE_LINK ${AU3_LINK})

if (AU_MODULE_EFFECTS_VST)
    set(MODULE_INCLUDE ${MODULE_INCLUDE} ${AU_MODULE_VST_VST3_SDK_PATH})
    set(MODULE_LINK ${MODULE_LINK} vst_sdk_3)
endif()

set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)

if (CC_IS_CLANG)
    set_property(TARGET ${MODULE} APPEND_STRING PROPERTY COMPILE_FLAGS "-fobjc-arc")
endif()
