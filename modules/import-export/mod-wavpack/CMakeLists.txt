#[[
WavPack import and export
]]

set( TARGET mod-wavpack )

set( SOURCES
      ImportWavPack.cpp
      ExportWavPack.cpp
      WavPack.cpp
)

set( LIBRARIES
   PRIVATE
      wavpack::wavpack
)

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list(APPEND LIBRARIES
   lib-import-export-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
