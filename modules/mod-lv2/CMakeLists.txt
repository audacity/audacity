#[[
Adds support for lv2 (Ladspa version 2) plug-ins
]]

addlib( lv2                lv2         LV2         NO    YES   "lilv-0 >= 0.24.6" "lv2 >= 1.16.0" "serd-0 >= 0.30.2" "sord-0 >= 0.16.4" "sratom-0 >= 0.6.4" "suil-0 >= 0.10.6" )

set( SOURCES
   LV2Effect.cpp
   LV2Effect.h
   LV2EffectMeter.cpp
   LV2EffectMeter.h
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
   LV2UIFeaturesList.cpp
   LV2UIFeaturesList.h
   LV2Utils.h
   LV2Validator.cpp
   LV2Validator.h
   LV2Wrapper.cpp
   LV2Wrapper.h
   LoadLV2.cpp
   LoadLV2.h
   NativeWindow.h
   lv2_external_ui.h
   zix/common.h
   zix/ring.cpp
   zix/ring.h
)
set( LIBRARIES
   Audacity
   lv2
)
audacity_module( mod-lv2 "${SOURCES}" "${LIBRARIES}"
   ""
   "$<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GTK>" )
