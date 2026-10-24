#[[
Provides audio file information via libsndfile
]]

set( TARGET au3-file-formats )

set( SOURCES
   FileFormats.cpp
   FileFormats.h
   AcidizerTags.h
   GetAcidizerTags.cpp
   GetAcidizerTags.h
)
set( LIBRARIES
PUBLIC
   # Intentionally PUBLIC: this library is the libsndfile facade. FileFormats.h
   # and GetAcidizerTags.h expose SNDFILE, which cannot be forward-declared
   # (its typedef differs between libsndfile versions).
   SndFile::sndfile
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
