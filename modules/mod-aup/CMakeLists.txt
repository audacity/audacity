set( TARGET mod-aup )

set( SOURCES
      ImportAUP.cpp
      AUP.cpp
)

set( LIBRARIES
      Audacity
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
