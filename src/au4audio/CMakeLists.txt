#
# Audacity: A Digital Audio Editor
#

# The audio engine of graphs of nodes, over any audio library
declare_module(au4audio)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/internal/endoftracknode.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/endoftracknode.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/iendoftrackcontext.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playheadmapper.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playheadmapper.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/stereo.h
)
# Their `using namespace au::au4audio` would leak into other unity-combined sources
set_source_files_properties(${MODULE_SRC} PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)

set(MODULE_LINK
    au3-clip-processing
)

setup_module()

if (AU_BUILD_AU4AUDIO_TESTS)
    add_subdirectory(tests)
endif()
