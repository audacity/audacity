#[[
Adds FFmpeg import and export, if those third party libraries are installed
]]

if (USE_FFMPEG)

   set( SOURCES
      ExportFFmpeg.cpp
      ExportFFmpegDialogs.cpp
      ExportFFmpegDialogs.h
      FFmpeg.cpp
      FFmpeg.h
      FFmpegPrefs.cpp
      ImportFFmpeg.cpp
   )
   set( LIBRARIES
      Audacity
      lib-ffmpeg-support-interface
      SndFile::sndfile
   )
   audacity_module( mod-ffmpeg "${SOURCES}" "${LIBRARIES}"
      "" "" )

endif()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
   if( "${${_OPT}use_ffmpeg}" STREQUAL "linked" )
      set( DISABLE_DYNAMIC_LOADING_FFMPEG YES )
   endif()
   if( USE_FFMPEG )
      list( APPEND MIMETYPES
         audio/aac
         audio/ac3
         audio/mp4
         audio/x-ms-wma
         video/mpeg
      )
      set( MIMETYPES "${MIMETYPES}" PARENT_SCOPE )
   endif()
endif()
