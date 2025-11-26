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

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometerfactory.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometerfactory.h
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
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsnap.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsnap.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackrulertypeattachment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackrulertypeattachment.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackviewtypeattachment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackviewtypeattachment.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/waveformscale.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/waveformscale.h
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
include(${CMAKE_CURRENT_LIST_DIR}/au3wrapDefs.cmake)

# AU3 Libraries - using add_subdirectory instead of manual source listing
# Note: Libraries are renamed from lib-* to au3-* in the au3/libraries directory

# Third-party libraries
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/sqlite au3-sqlite)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/soxr au3-soxr)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/pffft au3-pffft)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/portmixer au3-portmixer)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/soundtouch au3-soundtouch)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/sbsms au3-sbsms)

# Core AU3 libraries (in dependency order)
add_subdirectory(${AU3_LIBRARIES}/au3-string-utils au3-string-utils)
add_subdirectory(${AU3_LIBRARIES}/au3-strings au3-strings)
add_subdirectory(${AU3_LIBRARIES}/au3-utility au3-utility)
add_subdirectory(${AU3_LIBRARIES}/au3-components au3-components)
add_subdirectory(${AU3_LIBRARIES}/au3-basic-ui au3-basic-ui)
add_subdirectory(${AU3_LIBRARIES}/au3-exceptions au3-exceptions)
add_subdirectory(${AU3_LIBRARIES}/au3-preferences au3-preferences)
add_subdirectory(${AU3_LIBRARIES}/au3-math au3-math)
add_subdirectory(${AU3_LIBRARIES}/au3-files au3-files)
add_subdirectory(${AU3_LIBRARIES}/au3-registries au3-registries)
add_subdirectory(${AU3_LIBRARIES}/au3-xml au3-xml)
add_subdirectory(${AU3_LIBRARIES}/au3-project au3-project)
add_subdirectory(${AU3_LIBRARIES}/au3-project-rate au3-project-rate)
add_subdirectory(${AU3_LIBRARIES}/au3-file-formats au3-file-formats)
add_subdirectory(${AU3_LIBRARIES}/au3-track au3-track)
add_subdirectory(${AU3_LIBRARIES}/au3-transactions au3-transactions)
add_subdirectory(${AU3_LIBRARIES}/au3-sample-track au3-sample-track)
add_subdirectory(${AU3_LIBRARIES}/au3-audio-graph au3-audio-graph)
add_subdirectory(${AU3_LIBRARIES}/au3-tags au3-tags)
add_subdirectory(${AU3_LIBRARIES}/au3-wave-track au3-wave-track)
add_subdirectory(${AU3_LIBRARIES}/au3-wave-track-settings au3-wave-track-settings)
add_subdirectory(${AU3_LIBRARIES}/au3-track-selection au3-track-selection)
add_subdirectory(${AU3_LIBRARIES}/au3-sqlite-helpers au3-sqlite-helpers)
add_subdirectory(${AU3_LIBRARIES}/au3-project-history au3-project-history)
add_subdirectory(${AU3_LIBRARIES}/au3-project-file-io au3-project-file-io)
add_subdirectory(${AU3_LIBRARIES}/au3-time-and-pitch au3-time-and-pitch)
add_subdirectory(${AU3_LIBRARIES}/au3-playable-track au3-playable-track)
add_subdirectory(${AU3_LIBRARIES}/au3-audio-devices au3-audio-devices)
add_subdirectory(${AU3_LIBRARIES}/au3-fft au3-fft)
add_subdirectory(${AU3_LIBRARIES}/au3-wave-track-fft au3-wave-track-fft)
add_subdirectory(${AU3_LIBRARIES}/au3-stretching-sequence au3-stretching-sequence)
add_subdirectory(${AU3_LIBRARIES}/au3-channel au3-channel)
add_subdirectory(${AU3_LIBRARIES}/au3-mixer au3-mixer)
add_subdirectory(${AU3_LIBRARIES}/au3-realtime-effects au3-realtime-effects)
add_subdirectory(${AU3_LIBRARIES}/au3-audio-io au3-audio-io)
add_subdirectory(${AU3_LIBRARIES}/au3-module-manager au3-module-manager)
add_subdirectory(${AU3_LIBRARIES}/au3-time-frequency-selection au3-time-frequency-selection)
add_subdirectory(${AU3_LIBRARIES}/au3-screen-geometry au3-screen-geometry)
add_subdirectory(${AU3_LIBRARIES}/au3-graphics au3-graphics)
add_subdirectory(${AU3_LIBRARIES}/au3-wave-track-paint au3-wave-track-paint)
add_subdirectory(${AU3_LIBRARIES}/au3-viewport au3-viewport)
add_subdirectory(${AU3_LIBRARIES}/au3-snapping au3-snapping)
add_subdirectory(${AU3_LIBRARIES}/au3-numeric-formats au3-numeric-formats)
add_subdirectory(${AU3_LIBRARIES}/au3-command-parameters au3-command-parameters)
add_subdirectory(${AU3_LIBRARIES}/au3-menus au3-menus)
add_subdirectory(${AU3_LIBRARIES}/au3-effects au3-effects)
add_subdirectory(${AU3_LIBRARIES}/au3-builtin-effects au3-builtin-effects)
add_subdirectory(${AU3_LIBRARIES}/au3-dynamic-range-processor au3-dynamic-range-processor)
add_subdirectory(${AU3_LIBRARIES}/au3-label-track au3-label-track)
add_subdirectory(${AU3_LIBRARIES}/au3-import-export au3-import-export)
add_subdirectory(${AU3_LIBRARIES}/au3-ipc au3-ipc)
add_subdirectory(${AU3_LIBRARIES}/au3-audacity-application-logic au3-audacity-application-logic)

# AU3 modules that are still compiled as sources (not yet converted to libraries)
set(AU3_SRC
    ${AU3_MODULES}/import-export/RegisterExportPlugins.cpp
    ${AU3_MODULES}/import-export/RegisterExportPlugins.h
    ${AU3_MODULES}/import-export/RegisterImportPlugins.cpp
    ${AU3_MODULES}/import-export/RegisterImportPlugins.h

    ${AU3_MODULES}/import-export/mod-mp3/ExportMP3.cpp
    ${AU3_MODULES}/import-export/mod-mp3/ExportMP3.h
    ${AU3_MODULES}/import-export/mod-mp3/MP3.cpp

    ${AU3_MODULES}/import-export/mod-mpg123/ImportMP3_MPG123.cpp
    ${AU3_MODULES}/import-export/mod-mpg123/ImportMP3_MPG123.h

    ${AU3_MODULES}/import-export/mod-wavpack/ExportWavPack.cpp
    ${AU3_MODULES}/import-export/mod-wavpack/ExportWavPack.h

    ${AU3_MODULES}/import-export/mod-wavpack/ImportWavPack.cpp
    ${AU3_MODULES}/import-export/mod-wavpack/ImportWavPack.h

    ${AU3_MODULES}/import-export/mod-pcm/ExportPCM.cpp
    ${AU3_MODULES}/import-export/mod-pcm/ExportPCM.h

    ${AU3_MODULES}/import-export/mod-pcm/ImportPCM.cpp
    ${AU3_MODULES}/import-export/mod-pcm/ImportPCM.h
)

if (AU_MODULE_EFFECTS_AUDIO_UNIT)
    add_subdirectory(${AU3_LIBRARIES}/au3-audio-unit au3-audio-unit)

    set(AU3_LINK ${AU3_LINK}
        "-framework AudioUnit"
        "-framework AudioToolbox"
    )

    set(AU3_LIBRARIES_LIST ${AU3_LIBRARIES_LIST} au3-audio-unit)
endif()

if (AU_MODULE_EFFECTS_LV2)
    add_subdirectory(lv2sdk)
    add_subdirectory(${AU3_LIBRARIES}/au3-lv2 au3-lv2)

    set(AU3_LINK ${AU3_LINK} lv2sdk)
    set(AU3_LIBRARIES_LIST ${AU3_LIBRARIES_LIST} au3-lv2)
endif()

if(AU_MODULE_EFFECTS_VST)
    # VST3 SDK
    add_subdirectory(vst3)
    add_subdirectory(${AU3_LIBRARIES}/au3-vst3 au3-vst3)

    set(AU3_INCLUDE ${AU3_INCLUDE} ${AU_MODULE_VST_VST3_SDK_PATH})
    set(AU3_LINK ${AU3_LINK} vst_sdk_3)
    set(AU3_LIBRARIES_LIST ${AU3_LIBRARIES_LIST} au3-vst3)
endif()

# FFmpeg support (still compiled as sources, not yet converted to library)
set(MODULE_SRC ${MODULE_SRC}
    ${AU3_MODULES}/import-export/mod-ffmpeg/ImportFFmpeg.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/ExportFFmpeg.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/ExportFFmpeg.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/FFmpegDefines.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/FFmpeg.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/FFmpeg.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/ExportFFmpegOptions.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/ExportFFmpegOptions.h

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

    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-8.0.0-single-header.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/60/avconfig.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/62/AVCodecIDLookup.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/62/AVCodecImpl.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/62/AVFormatImpl.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/60/AVUtilImpl.cpp

    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/AVCodecFunctionsLoader.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/AVCodecFunctionsLoader.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/AVFormatFunctionsLoader.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/AVFormatFunctionsLoader.h
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/AVUtilFunctionsLoader.cpp
    ${AU3_MODULES}/import-export/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/AVUtilFunctionsLoader.h
)

# ==================================
# List of all AU3 libraries to link publicly
set(AU3_LIBRARIES_LIST
    au3-string-utils
    au3-strings
    au3-utility
    au3-components
    au3-basic-ui
    au3-exceptions
    au3-preferences
    au3-math
    au3-files
    au3-registries
    au3-xml
    au3-project
    au3-project-rate
    au3-file-formats
    au3-track
    au3-transactions
    au3-sample-track
    au3-audio-graph
    au3-tags
    au3-wave-track
    au3-track-selection
    au3-sqlite-helpers
    au3-project-history
    au3-project-file-io
    au3-time-and-pitch
    au3-playable-track
    au3-audio-devices
    au3-fft
    au3-wave-track-fft
    au3-stretching-sequence
    au3-channel
    au3-mixer
    au3-realtime-effects
    au3-audio-io
    au3-module-manager
    au3-time-frequency-selection
    au3-screen-geometry
    au3-graphics
    au3-wave-track-paint
    au3-viewport
    au3-snapping
    au3-numeric-formats
    au3-command-parameters
    au3-menus
    au3-effects
    au3-builtin-effects
    au3-dynamic-range-processor
    au3-label-track
    au3-import-export
    au3-ipc
    au3-audacity-application-logic
    ${AU3_LIBRARIES_LIST}
)

# ==================================
set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_SRC ${MODULE_SRC} ${AU3_SRC})
set(MODULE_LINK ${AU3_LINK} project audio)
set(MODULE_LINK_PUBLIC ${AU3_LIBRARIES_LIST})

set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)

if (CC_IS_CLANG)
    set_property(TARGET ${MODULE} APPEND_STRING PROPERTY COMPILE_FLAGS "-fobjc-arc")
endif()
