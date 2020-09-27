#[[
Adds MP2 export
]]

addlib( twolame            twolame     LIBTWOLAME  NO    YES   "twolame >= 0.3.13" )

if ( USE_LIBTWOLAME )

   set( SOURCES
      ExportMP2.cpp
   )

   set( LIBS
      Audacity
      twolame
   )

   if ( USE_LIBID3TAG )
      list( APPEND LIBS
         libid3tag::libid3tag
      )
   endif()

   audacity_module( mod-mp2 "${SOURCES}" "${LIBS}"
      "" "" )

endif()
