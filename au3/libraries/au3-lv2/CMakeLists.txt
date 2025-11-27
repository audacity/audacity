#[[
LV2 effect plugin discovery and processing logic, without user interface
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   LV2EffectBase.cpp
   LV2EffectBase.h
   LV2FeaturesList.cpp
   LV2FeaturesList.h
   LV2Instance.cpp
   LV2Instance.h
   LV2InstanceFeaturesList.cpp
   LV2InstanceFeaturesList.h
   LV2Ports.cpp
   LV2Ports.h
   LV2Preferences.cpp
   LV2Preferences.h
   LV2Symbols.cpp
   LV2Symbols.h
   LV2Utils.h
   LV2Wrapper.cpp
   LV2Wrapper.h
   LoadLV2.cpp
   LoadLV2.h
   lv2_external_ui.h
   zix/common.h
   zix/ring.cpp
   zix/ring.h
)
set( LIBRARIES
   au3-effects-interface
   lv2
)
audacity_library( au3-lv2 "${SOURCES}" "${LIBRARIES}"
   "" ""
)
