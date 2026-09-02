#[[
FFmpeg import/export (FFmpeg is loaded dynamically at runtime).
Note: FFmpeg.cpp does #include "ImportFFmpeg.cpp" and #include "ExportFFmpeg.cpp"
(as Audacity 3 does), so those two files must not be listed as sources here.
]]

set( TARGET au3-mod-ffmpeg )

set( SOURCES
   FFmpeg.cpp
   FFmpeg.h
   FFmpegDefines.h
   ImportFFmpeg.h
   ExportFFmpeg.h
   ExportFFmpegOptions.cpp
   ExportFFmpegOptions.h
)

set( LIBRARIES
PUBLIC
   au3-ffmpeg-support
   au3-import-export
   au3-math
   au3-mixer
   au3-preferences
   au3-strings
   au3-xml
PRIVATE
   au3-files
   au3-tags
   au3-track
   au3-utility
   au3-wave-track
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
