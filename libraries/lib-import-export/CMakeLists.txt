#[[
Implements audio import and export pipelines.
]]

set( SOURCES
   Export.cpp
   Export.h
   ExportOptionsEditor.cpp
   ExportOptionsEditor.h
   ExportPlugin.cpp
   ExportPlugin.h
   ExportProgressListener.cpp
   ExportProgressListener.h
   ExportTypes.h
   ExportUtils.cpp
   ExportUtils.h
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
   lib-stretching-sequence-interface
)
audacity_library( lib-import-export "${SOURCES}" "${LIBRARIES}"
   "" ""
)
