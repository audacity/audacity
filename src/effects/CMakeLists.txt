#
# Audacity: A Digital Audio Editor
#

add_subdirectory(effects_base)
add_subdirectory(builtin)
add_subdirectory(builtin_collection)
add_subdirectory(audacity_plugin)

if (AU_MODULE_EFFECTS_LV2)
    add_subdirectory(lv2)
endif()

if (AU_MODULE_EFFECTS_VST)
    add_subdirectory(vst)
endif()

if (AU_MODULE_EFFECTS_AUDIO_UNIT)
    add_subdirectory(audio_unit)
endif()

if (AU_MODULE_EFFECTS_NYQUIST)
    add_subdirectory(nyquist)
endif()
