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
   lib-math-interface
   lib-strings-interface
   lib-basic-ui-interface
   lib-registries-interface
   lib-project-interface
   lib-sample-track-interface
   lib-wave-track-interface
   lib-effects-interface
   PUBLIC
      rapidjson::rapidjson
      lib-tags
)
audacity_library( lib-import-export "${SOURCES}" "${LIBRARIES}"
   "" ""
)
