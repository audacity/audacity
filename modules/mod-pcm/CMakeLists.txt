#[[
Adds PCM (.wav file) import and export
]]

set( SOURCES
   ExportPCM.cpp
   ImportPCM.cpp
)
set( LIBRARIES
   Audacity
   SndFile::sndfile
)
if (USE_LIBID3TAG)
   list( APPEND LIBRARIES libid3tag::libid3tag )
endif()

audacity_module( mod-pcm "${SOURCES}" "${LIBRARIES}"
   "" "" )
