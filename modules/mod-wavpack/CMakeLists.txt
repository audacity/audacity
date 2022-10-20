#[[
Adds WavPack import and export
]]

if ( USE_WAVPACK )

   set( SOURCES
      ExportWavPack.cpp
      ImportWavPack.cpp
   )

   set( LIBS
      Audacity
      wavpack::wavpack
   )

   audacity_module( mod-wavpack "${SOURCES}" "${LIBS}"
      "" "" )

endif()
