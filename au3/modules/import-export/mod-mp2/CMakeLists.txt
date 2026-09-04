#[[
MP2 export via twolame.
]]

set( TARGET au3-mod-mp2 )

set( SOURCES
   ExportMP2.cpp
   ExportMP2.h
)

set( LIBRARIES
PUBLIC
   au3-import-export
PRIVATE
   au3-files
   au3-mixer
   au3-tags
   au3-track
   twolame
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
