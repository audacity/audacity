set( TARGET mod-wavpack )

set( SOURCES
      ImportWavPack.cpp
      ExportWavPack.cpp
      WavPack.cpp
)

set( LIBRARIES
      lib-utility-interface
      lib-tags-interface
      lib-import-export-interface
      wavpack::wavpack
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
