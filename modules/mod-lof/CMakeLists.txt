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
