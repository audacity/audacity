#[[
Implements audio import and export pipelines.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( TARGET au3-import-export )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
   Export.cpp
   Export.h
   ExportOptionsEditor.cpp
   ExportOptionsEditor.h
   ExportPlugin.cpp
   ExportPlugin.h
   ExportPluginHelpers.cpp
   ExportPluginHelpers.h
   ExportPluginRegistry.cpp
   ExportPluginRegistry.h
   ExportProgressUI.cpp
   ExportProgressUI.h
   ExportTypes.h
   ExportUtils.cpp
   ExportUtils.h
   GetAcidizerTags.cpp
   GetAcidizerTags.h
   Import.cpp
   Import.h
   ImportExport.cpp
   ImportExport.h
   ImportForwards.h
   ImportPlugin.cpp
   ImportPlugin.h
   ImportProgressListener.cpp
   ImportProgressListener.h
   ImportUtils.cpp
   ImportUtils.h
   LibsndfileTagger.cpp
   LibsndfileTagger.h
   PlainExportOptionsEditor.cpp
   PlainExportOptionsEditor.h
)
set( LIBRARIES
#   rapidjson::rapidjson
   au3-tags-interface
   au3-wave-track-interface
   au3-project-interface
   PRIVATE
      au3-effects-interface
)
audacity_library( au3-import-export "${SOURCES}" "${LIBRARIES}"
   "" ""
)
