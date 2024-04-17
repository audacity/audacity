#[[
Interpret .lof (list-of-files) format, a simple language that can name other
files to import and then time-shift
]]

set( TARGET mod-lof )

set( SOURCES
      ImportLOF.cpp
      LOF.cpp
)

set( LIBRARIES
   PRIVATE
      Audacity
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
