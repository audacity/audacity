#
# Audacity: A Digital Audio Editor
#

add_subdirectory(effects_base)
add_subdirectory(builtin)

if (AU_MODULE_EFFECTS_VST)
    add_subdirectory(vst)
endif()

add_subdirectory(nyquist)
