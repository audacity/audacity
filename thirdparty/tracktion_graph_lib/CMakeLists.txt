#
# Audacity: A Digital Audio Editor
#
# tracktion_graph, with the modules it needs on top of the juce library,
# compiled as one static library from the modules' unity sources, except
# tracktion_graph's, which builds its tests too.

set(TARGET tracktion_graph)

set(JUCE_MODULES_DIR ${CMAKE_SOURCE_DIR}/thirdparty/JUCE/modules)
set(TRACKTION_MODULES_DIR ${CMAKE_SOURCE_DIR}/thirdparty/tracktion_engine/modules)

# Needed by tracktion_graph only, not by the juce library's clients
set(JUCE_MODULES
    juce_audio_formats
)
set(TRACKTION_MODULES
    tracktion_core
    tracktion_graph
)

# On Apple platforms, JUCE modules come with Objective-C++ unity sources
if (APPLE)
    set(JUCE_UNITY_EXT mm)
else()
    set(JUCE_UNITY_EXT cpp)
endif()

set(SOURCES)
foreach(module ${JUCE_MODULES})
    list(APPEND SOURCES ${JUCE_MODULES_DIR}/${module}/${module}.${JUCE_UNITY_EXT})
endforeach()
list(APPEND SOURCES ${TRACKTION_MODULES_DIR}/tracktion_core/tracktion_core.cpp)
# tracktion_graph.cpp also compiles tests that need tracktion_engine
list(APPEND SOURCES ${CMAKE_CURRENT_LIST_DIR}/tracktion_graph_runtime.cpp)

add_library(${TARGET} STATIC ${SOURCES})

# The sources are unity files already, and contain no Qt code
set_target_properties(${TARGET} PROPERTIES UNITY_BUILD OFF AUTOMOC OFF POSITION_INDEPENDENT_CODE ON)

target_include_directories(${TARGET} SYSTEM PUBLIC ${TRACKTION_MODULES_DIR})

set(MODULE_AVAILABILITY)
foreach(module ${JUCE_MODULES} ${TRACKTION_MODULES})
    list(APPEND MODULE_AVAILABILITY JUCE_MODULE_AVAILABLE_${module}=1)
endforeach()

# Must be the same for the library and its clients, hence PUBLIC
target_compile_definitions(${TARGET} PUBLIC ${MODULE_AVAILABILITY})

# juce_audio_formats bundles codecs whose C symbols clash with those of the libraries Audacity links, and they aren't used
target_compile_definitions(${TARGET} PUBLIC
    JUCE_USE_OGGVORBIS=0
    JUCE_USE_FLAC=0
)

target_link_libraries(${TARGET} PUBLIC juce)

# tracktion_CPU.h calls __rdtsc, which MSVC ARM64 lacks; every includer needs the stand-in, hence PUBLIC
if (MSVC AND CMAKE_CXX_COMPILER_ARCHITECTURE_ID STREQUAL "ARM64")
    target_compile_options(${TARGET} PUBLIC "$<$<COMPILE_LANGUAGE:CXX>:/FI${CMAKE_CURRENT_LIST_DIR}/msvc_arm64_rdtsc.h>")
endif()

# Third-party code: don't flood the build log
if (MSVC)
    target_compile_options(${TARGET} PRIVATE /W0)
else()
    target_compile_options(${TARGET} PRIVATE -w)
endif()
