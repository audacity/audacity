#[[
FFmpeg import and export
]]

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
      lib-export-ui-interface
      lib-ffmpeg-support-interface
      lib-import-export-interface
      lib-preference-pages-interface
      lib-wx-init-interface
)

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

if( "${${_OPT}use_ffmpeg}" STREQUAL "linked" )
      set( DISABLE_DYNAMIC_LOADING_FFMPEG YES )
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
