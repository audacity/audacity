if (NOT AU_MODULE_EFFECTS_AUDIO_UNIT)
    add_subdirectory(audio_unit)
endif()

if (NOT AU_MODULE_EFFECTS_LV2)
    add_subdirectory(lv2)
endif()

if (NOT AU_MODULE_EFFECTS_VST)
    add_subdirectory(vst)
endif()
