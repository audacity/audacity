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
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3commonsettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3commonsettings.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3basicui.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3basicui.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsnap.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsnap.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackviewtypeattachment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackviewtypeattachment.h
    )

# ==================================
# === AU3 ===
# ==================================

# Include AU3 wrap definitions
include(${CMAKE_CURRENT_LIST_DIR}/au3wrapDefs.cmake)

# Third-party libraries (not part of au3/libraries)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/sqlite au3-sqlite)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/soxr au3-soxr)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/pffft au3-pffft)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/portmixer au3-portmixer)

# Conditional time-stretching and pitch-shifting libraries
if (AU_USE_SOUNDTOUCH)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/soundtouch au3-soundtouch)
endif()

if (AU_USE_SBSMS)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/thirdparty/sbsms au3-sbsms)
endif()

# Conditional third-party SDKs (must be set up before AU3 libraries)
if (AU_MODULE_EFFECTS_LV2)
    add_subdirectory(lv2sdk)
endif()

if(AU_MODULE_EFFECTS_VST)
    add_subdirectory(vst3)
endif()

# Include AU3 libraries - this will add all library subdirectories
# and set AU3_LIBRARIES_LIST in this scope
add_subdirectory(${AU3_LIBRARIES} au3-libraries)

# Additional linking setup for conditional libraries
if (AU_MODULE_EFFECTS_LV2)
    set(AU3_LINK ${AU3_LINK} lv2sdk)
endif()

if(AU_MODULE_EFFECTS_VST)
    set(AU3_INCLUDE ${AU3_INCLUDE} ${AU_MODULE_VST_VST3_SDK_PATH})
    set(AU3_LINK ${AU3_LINK} vst_sdk_3)
endif()

# We currently have some AU3 modules that are still compiled as sources (not yet converted to libraries)
set(MODULE_SRC ${MODULE_SRC}
    # AU3 modules that are still compiled as sources (not yet converted to libraries)
    ${IMPORT_EXPORT_MODULE}/RegisterExportPlugins.cpp
    ${IMPORT_EXPORT_MODULE}/RegisterExportPlugins.h
    ${IMPORT_EXPORT_MODULE}/RegisterImportPlugins.cpp
    ${IMPORT_EXPORT_MODULE}/RegisterImportPlugins.h

    ${IMPORT_EXPORT_MODULE}/mod-mp3/ExportMP3.cpp
    ${IMPORT_EXPORT_MODULE}/mod-mp3/ExportMP3.h
    ${IMPORT_EXPORT_MODULE}/mod-mp3/MP3.cpp

    ${IMPORT_EXPORT_MODULE}/mod-mpg123/ImportMP3_MPG123.cpp
    ${IMPORT_EXPORT_MODULE}/mod-mpg123/ImportMP3_MPG123.h

    ${IMPORT_EXPORT_MODULE}/mod-wavpack/ExportWavPack.cpp
    ${IMPORT_EXPORT_MODULE}/mod-wavpack/ExportWavPack.h
    ${IMPORT_EXPORT_MODULE}/mod-wavpack/ImportWavPack.cpp
    ${IMPORT_EXPORT_MODULE}/mod-wavpack/ImportWavPack.h

    ${IMPORT_EXPORT_MODULE}/mod-pcm/ExportPCM.cpp
    ${IMPORT_EXPORT_MODULE}/mod-pcm/ExportPCM.h
    ${IMPORT_EXPORT_MODULE}/mod-pcm/ImportPCM.cpp
    ${IMPORT_EXPORT_MODULE}/mod-pcm/ImportPCM.h

    ${IMPORT_EXPORT_MODULE}/mod-ogg/ExportOGG.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ogg/ExportOGG.h

    # FFmpeg support (still compiled as sources, not yet converted to library)
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/ImportFFmpeg.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/ExportFFmpeg.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/ExportFFmpeg.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/FFmpegDefines.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/FFmpeg.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/FFmpeg.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/ExportFFmpegOptions.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/ExportFFmpegOptions.h

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/FFmpegTypes.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/AVCodecFunctions.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/AVCodecID.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/AVFormatFunctions.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/AVUtilFunctions.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/FifoBuffer.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/FifoBuffer.h

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVChannelLayoutWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecContextWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecContextWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVDictionaryWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVDictionaryWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFormatContextWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFormatContextWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVInputFormatWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVInputFormatWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVIOContextWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVIOContextWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVOutputFormatWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVOutputFormatWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVPacketWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVPacketWrapper.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVStreamWrapper.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/wrappers/AVStreamWrapper.h

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/DynamicLibraryHelpers.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/DynamicLibraryHelpers.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/FFmpegAPIResolver.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/FFmpegAPIResolver.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/FFmpegLog.h

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-2.3.6-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/52/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/55/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/55/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/55/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/52/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-3.4.8-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/55/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/57/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/57/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/57/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/55/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-4.2.4-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/56/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/58/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/58/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/58/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/56/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-5.0.1-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/57/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/59/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/59/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/59/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/57/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-6.0.0-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/58/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/60/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/60/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/60/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/58/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-7.0.0-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/59/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/61/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/61/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/61/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/59/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/ffmpeg-8.0.0-single-header.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/60/avconfig.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/62/AVCodecIDLookup.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/62/AVCodecImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/62/AVFormatImpl.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/60/AVUtilImpl.cpp

    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/AVCodecFunctionsLoader.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avcodec/AVCodecFunctionsLoader.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/AVFormatFunctionsLoader.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avformat/AVFormatFunctionsLoader.h
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/AVUtilFunctionsLoader.cpp
    ${IMPORT_EXPORT_MODULE}/mod-ffmpeg/lib-ffmpeg-support/impl/avutil/AVUtilFunctionsLoader.h
)

# ==================================
# AU3_LIBRARIES_LIST is now set by au3/libraries/CMakeLists.txt
# ==================================
set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_SRC ${MODULE_SRC})
set(MODULE_LINK ${AU3_LINK} project audio)
set(MODULE_LINK_PUBLIC ${AU3_LIBRARIES_LIST})

set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)

if (CC_IS_CLANG)
    set_property(TARGET ${MODULE} APPEND_STRING PROPERTY COMPILE_FLAGS "-fobjc-arc")
endif()
