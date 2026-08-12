#
# Audacity: A Digital Audio Editor
#

add_subdirectory(locale)
if (MUSE_MODULE_TESTFLOW)
    add_subdirectory(testflowscripts)
endif()
add_subdirectory(icons)
add_subdirectory(workspaces)

IF (AU_MODULE_EFFECTS_NYQUIST)
    add_subdirectory(nyquist-runtime)
    add_subdirectory(nyquist-plug-ins)
ENDIF()
