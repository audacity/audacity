#[[
Import of older Audacity project files, in .aup format, saved before version 3.0
]]

set( TARGET mod-aup )

set( SOURCES
      ImportAUP.cpp
      AUP.cpp
)

set( LIBRARIES
   PRIVATE
      Audacity
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
