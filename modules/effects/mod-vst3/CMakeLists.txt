#[[
User interface for effects using the VST version 3 protocol
]]

if (${_OPT}has_vst3)

set(SOURCES
   VST3Editor.cpp
   VST3Editor.h
   VST3Effect.h
   VST3Effect.cpp
   VST3OptionsDialog.h
   VST3OptionsDialog.cpp
   VST3ParametersWindow.cpp
   VST3ParametersWindow.h
   internal/PlugFrame.h
   internal/PlugFrame.cpp

   $<$<BOOL:${wxIS_GTK}>:
      internal/x11/RunLoop.h
      internal/x11/RunLoop.cpp
      internal/x11/PlugFrame.h
      internal/x11/PlugFrame.cpp
      internal/x11/SocketWindow.h
      internal/x11/SocketWindow.cpp
   >
)

if(${wxIS_MAC})
   set_source_files_properties(
      module_mac.mm
      PROPERTIES
         COMPILE_FLAGS "-fobjc-arc"
   )
endif()

set(LIBRARIES
   PRIVATE
      Audacity
      lib-vst3
)

set(EXTRA
   vst3sdk::base
   vst3sdk::pluginterfaces
   vst3sdk::sdk_hosting
   vst3sdk::sdk_common
)

# Third party libraries used only by this module
set(EXTRA_CLUSTER_NODES "${LIBRARIES};${EXTRA}" PARENT_SCOPE)

audacity_module( mod-vst3 "${SOURCES}" "${LIBRARIES}"
   ""
   "" )

endif()
