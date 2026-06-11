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
      PRIVATE
      SndFile::sndfile
      au3-math-interface
      au3-strings-interface
      wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
