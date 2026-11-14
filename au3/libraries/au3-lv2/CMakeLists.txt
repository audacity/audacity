#[[
LV2 effect plugin discovery and processing logic, without user interface
]]

set( TARGET au3-lv2 )

set( SOURCES
   LV2EffectBase.cpp
   LV2EffectBase.h
   LV2FeaturesList.cpp
   LV2FeaturesList.h
   LV2Instance.cpp
   LV2Instance.h
   LV2InstanceFeaturesList.cpp
   LV2InstanceFeaturesList.h
   LV2PluginId.cpp
   LV2PluginId.h
   LV2Ports.cpp
   LV2Ports.h
   LV2Preferences.cpp
   LV2Preferences.h
   LV2Symbols.cpp
   LV2Symbols.h
   LV2UIFeaturesList.cpp
   LV2UIFeaturesList.h
   LV2Utils.h
   LV2Wrapper.cpp
   LV2Wrapper.h
   LoadLV2.cpp
   LoadLV2.h
   lv2_external_ui.h
)
set( LIBRARIES
PUBLIC
   lv2sdk # LoadLV2.h includes lilv/lilv.h
   au3-components
   au3-effects
   au3-math
   au3-strings
   au3-utility
PRIVATE
   au3-basic-ui
   au3-exceptions
   au3-module-manager
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
