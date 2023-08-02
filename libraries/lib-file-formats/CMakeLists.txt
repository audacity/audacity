#[[
Provides audio file information via libsndfile
]]

set( SOURCES
   FileFormats.cpp
   FileFormats.h
)
set( LIBRARIES
   PUBLIC
      SndFile::sndfile
      lib-math-interface
)
audacity_library( lib-file-formats "${SOURCES}" "${LIBRARIES}"
   "" ""
)
