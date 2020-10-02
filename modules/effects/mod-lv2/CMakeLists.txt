#[[
User interface for effects using the lv2 protocol
]]

set( SOURCES
   LV2Editor.cpp
   LV2Editor.h
   LV2Effect.cpp
   LV2Effect.h
   LV2EffectMeter.cpp
   LV2EffectMeter.h
   LV2PreferencesDialog.cpp
   LV2PreferencesDialog.h
   LV2UIFeaturesList.cpp
   LV2UIFeaturesList.h
   NativeWindow.h
)
set( LIBRARIES
   PRIVATE
      Audacity
      lib-lv2-interface
)

# Third party libraries used only by this module
set(EXTRA_CLUSTER_NODES "${LIBRARIES};lv2" PARENT_SCOPE)

audacity_module( mod-lv2 "${SOURCES}" "${LIBRARIES}"
   ""
   "$<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GTK>" )

