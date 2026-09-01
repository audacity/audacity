#[[
PCM (WAV/AIFF/...) import/export via libsndfile.
]]

set( TARGET au3-mod-pcm )

set( SOURCES
   ExportPCM.cpp
   ExportPCM.h
   ImportPCM.cpp
   ImportPCM.h
)

set( LIBRARIES
PUBLIC
   au3-import-export
PRIVATE
   au3-file-formats
   au3-files
   au3-math
   au3-mixer
   au3-preferences
   au3-tags
   au3-track
   au3-wave-track
   SndFile::sndfile
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
