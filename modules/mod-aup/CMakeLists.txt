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
