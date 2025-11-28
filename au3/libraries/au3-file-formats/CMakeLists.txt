#[[
Provides audio file information via libsndfile
]]

set( SOURCES
   FileFormats.cpp
   FileFormats.h
   AcidizerTags.h
)
set( LIBRARIES
   PUBLIC
      SndFile::sndfile
      au3-math-interface
)
audacity_library( au3-file-formats "${SOURCES}" "${LIBRARIES}"
   "" ""
)
