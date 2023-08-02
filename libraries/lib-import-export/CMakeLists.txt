#[[
Implements audio import and export pipelines.
]]

set( TARGET lib-import-export )
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
   ExportTypes.h
   ExportUtils.cpp
   ExportUtils.h

   Import.cpp
   Import.h
   ImportForwards.h
   ImportPlugin.cpp
   ImportPlugin.h
   ImportProgressListener.cpp
   ImportProgressListener.h
   ImportUtils.cpp
   ImportUtils.h

   PlainExportOptionsEditor.cpp
   PlainExportOptionsEditor.h
)
set( LIBRARIES
   rapidjson::rapidjson
   lib-tags-interface
   lib-wave-track-interface
   PRIVATE
      lib-effects-interface      
)
audacity_library( lib-import-export "${SOURCES}" "${LIBRARIES}"
   "" ""
)
