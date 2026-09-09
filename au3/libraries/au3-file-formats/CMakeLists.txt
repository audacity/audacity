#[[
Provides audio file information via libsndfile
]]

set( TARGET au3-file-formats )

set( SOURCES
   FileFormats.cpp
   FileFormats.h
   AcidizerTags.h
)
set( LIBRARIES
PUBLIC
   SndFile::sndfile # FileFormats.h includes <sndfile.h>
   au3-math
   au3-strings
PRIVATE
   au3-basic-ui
   au3-utility
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
