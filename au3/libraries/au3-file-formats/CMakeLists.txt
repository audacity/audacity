#[[
Provides audio file information via libsndfile
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
