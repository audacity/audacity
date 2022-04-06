#[[
Adds support for Steinberg VST version 3 plug-ins
]]

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( wxIS_MAC on )
elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set ( wxIS_WIN on )
else()
   set ( wxIS_GTK on)
endif()

if (${_OPT}has_vst3)

set(SOURCES
   AudacityVst3HostApplication.cpp
   AudacityVst3HostApplication.h
   VST3Effect.h
   VST3Effect.cpp
   VST3EffectsModule.h
   VST3EffectsModule.cpp
   VST3Instance.cpp
   VST3Instance.h
   VST3Utils.h
   VST3Utils.cpp
   VST3OptionsDialog.h
   VST3OptionsDialog.cpp
   VST3ParametersWindow.cpp
   VST3ParametersWindow.h
   VST3UIValidator.cpp
   VST3UIValidator.h
   VST3Wrapper.cpp
   VST3Wrapper.h
   internal/ConnectionProxy.h
   internal/ConnectionProxy.cpp
   internal/PlugFrame.h
   internal/PlugFrame.cpp
   memorystream.cpp
   memorystream.h

   $<$<BOOL:${wxIS_WIN}>:
      module_win32.cpp
   >

   $<$<BOOL:${wxIS_GTK}>:
      module_linux.cpp
      internal/x11/RunLoop.h
      internal/x11/RunLoop.cpp
      internal/x11/PlugFrame.h
      internal/x11/PlugFrame.cpp
      internal/x11/SocketWindow.h
      internal/x11/SocketWindow.cpp
   >

   $<$<BOOL:${wxIS_MAC}>:
      module_mac.mm
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
   Audacity
   vst3sdk::base
   vst3sdk::pluginterfaces
   vst3sdk::sdk_hosting
   vst3sdk::sdk_common
)

audacity_module( mod-vst3 "${SOURCES}" "${LIBRARIES}"
   ""
   "" )

endif()
