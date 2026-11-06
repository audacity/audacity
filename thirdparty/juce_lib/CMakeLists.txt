#
# Audacity: A Digital Audio Editor
#
# The JUCE modules that play and record with audio devices, compiled as one
# static library from the modules' unity sources.
# JUCE's own CMake API is not used: it builds its juceaide helper at configure
# time, which we do not need.

set(TARGET juce)

set(JUCE_MODULES_DIR ${CMAKE_SOURCE_DIR}/thirdparty/JUCE/modules)

set(JUCE_MODULES
    juce_core
    juce_events
    juce_audio_basics
    juce_audio_devices
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
# Compiled separately by JUCE's own CMake API, so that only it rebuilds on every build
list(APPEND SOURCES ${JUCE_MODULES_DIR}/juce_core/juce_core_CompilationTime.cpp)

add_library(${TARGET} STATIC ${SOURCES})

# The sources are unity files already, and contain no Qt code
set_target_properties(${TARGET} PROPERTIES UNITY_BUILD OFF AUTOMOC OFF POSITION_INDEPENDENT_CODE ON)

target_include_directories(${TARGET} SYSTEM PUBLIC ${JUCE_MODULES_DIR})

set(MODULE_AVAILABILITY)
foreach(module ${JUCE_MODULES})
    list(APPEND MODULE_AVAILABILITY JUCE_MODULE_AVAILABLE_${module}=1)
endforeach()

# Must be the same for the library and its clients, hence PUBLIC
target_compile_definitions(${TARGET} PUBLIC
    JUCE_GLOBAL_MODULE_SETTINGS_INCLUDED=1
    JUCE_STANDALONE_APPLICATION=0
    JUCE_USE_CURL=0
    JUCE_WEB_BROWSER=0
    JUCE_JACK=0
    ${MODULE_AVAILABILITY}
)

# Third-party code: don't flood the build log
if (MSVC)
    target_compile_options(${TARGET} PRIVATE /W0)
else()
    target_compile_options(${TARGET} PRIVATE -w)
endif()

if (OS_IS_LIN)
    find_package(ALSA REQUIRED)
    find_package(Threads REQUIRED)
    target_link_libraries(${TARGET} PUBLIC ALSA::ALSA Threads::Threads ${CMAKE_DL_LIBS} rt)
elseif (OS_IS_MAC)
    target_link_libraries(${TARGET} PUBLIC
        "-framework Accelerate"
        "-framework AudioToolbox"
        "-framework Cocoa"
        "-framework CoreAudio"
        "-framework CoreMIDI"
        "-framework Foundation"
        "-framework IOKit"
        "-framework QuartzCore"
        "-framework Security"
    )
endif()
# On Windows, JUCE pulls in its system libraries with #pragma comment(lib, ...)
