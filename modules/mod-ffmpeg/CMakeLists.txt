set( TARGET mod-ffmpeg )

add_subdirectory(lib-ffmpeg-support)

set( SOURCES
      ExportFFmpeg.cpp
      ExportFFmpegOptions.cpp
      ExportFFmpegOptions.h
      FFmpeg.cpp
      FFmpeg.h
      FFmpegDefines.h
      FFmpegPrefs.cpp
      FFmpegPresets.cpp
      FFmpegPresets.h
      ImportFFmpeg.cpp
)

set( LIBRARIES
   PRIVATE
      lib-ffmpeg-support
      Audacity
)

if( "${${_OPT}use_ffmpeg}" STREQUAL "linked" )
      set( DISABLE_DYNAMIC_LOADING_FFMPEG YES )
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
