#
# Audacity: A Digital Audio Editor
#

add_subdirectory(effects_base)
add_subdirectory(builtin)

if (AU_MODULE_EFFECTS_LV2)
    add_subdirectory(lv2)
endif()

if (AU_MODULE_EFFECTS_VST)
    add_subdirectory(vst)
endif()

if (AU_MODULE_EFFECTS_AUDIO_UNIT)
    add_subdirectory(audio_unit)
endif()

add_subdirectory(nyquist)
