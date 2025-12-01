#[[
Implements audio import and export pipelines.
]]

set( TARGET au3-import-export )

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
      wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
