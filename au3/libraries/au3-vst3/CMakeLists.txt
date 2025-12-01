#[[
VST3 effect plugin discovery and processing logic, without user interface
]]

set( TARGET au3-vst3 )

set( SOURCES
   AudacityVst3HostApplication.cpp
   AudacityVst3HostApplication.h
   VST3EffectBase.cpp
   VST3EffectBase.h
   VST3EffectsModule.cpp
   VST3EffectsModule.h
   VST3Instance.cpp
   VST3Instance.h
   VST3Utils.cpp
   VST3Utils.h
   VST3Wrapper.cpp
   VST3Wrapper.h
   internal/ConnectionProxy.cpp
   internal/ConnectionProxy.h
   memorystream.cpp
   memorystream.h
   $<$<BOOL:${wxIS_GTK}>:
      module_linux.cpp
   >
   $<$<BOOL:${wxIS_WIN}>:
      module_win32.cpp
   >
   $<$<BOOL:${wxIS_MAC}>:
      module_mac.mm
   >
)
set( LIBRARIES
   vst_sdk_3
   au3-utility
   au3-effects-interface
   au3-files-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

if(${wxIS_MAC})
   set_source_files_properties(
      module_mac.mm
      PROPERTIES
         COMPILE_FLAGS "-fobjc-arc"
         SKIP_PRECOMPILE_HEADERS YES
   )
endif()

