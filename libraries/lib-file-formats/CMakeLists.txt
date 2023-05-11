#[[
Provides audio file information via libsndfile
]]

set( SOURCES
   FileFormats.cpp
   FileFormats.h
)
set( LIBRARIES
   lib-utility-interface
   lib-basic-ui-interface
   lib-math-interface
   lib-strings-interface
   PUBLIC
      SndFile::sndfile
)
audacity_library( lib-file-formats "${SOURCES}" "${LIBRARIES}"
   "" ""
)
