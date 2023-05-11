set( TARGET mod-lof )

set( SOURCES
      ImportLOF.cpp
      LOF.cpp
)

set( LIBRARIES
      Audacity
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
