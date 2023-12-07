set( TARGET mod-cloud-audiocom )

set( SOURCES
   menus/Menus.cpp

   AudioComModule.cpp
)

set( LIBRARIES
   PRIVATE
      lib-cloud-audiocom
      lib-menus
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
