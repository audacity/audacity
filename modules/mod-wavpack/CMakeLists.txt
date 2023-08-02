set( TARGET mod-wavpack )

set( SOURCES
      ImportWavPack.cpp
      ExportWavPack.cpp
      WavPack.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      wavpack::wavpack
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
