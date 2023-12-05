#[[
Library for Music Information Retrieval (MIR) algorithms.
]]

set( SOURCES
   MusicInformationRetrieval.cpp
   MusicInformationRetrieval.h
)

set( LIBRARIES
)

audacity_library( lib-music-information-retrieval "${SOURCES}" ""
   "" ""
)
