cmake_minimum_required(VERSION 3.13)

set(TARGET portaudio-v19)
set(TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET})

add_subdirectory("${TARGET_SOURCE}" "${CMAKE_CURRENT_BINARY_DIR}/portaudio" EXCLUDE_FROM_ALL)
target_include_directories(portaudio_static INTERFACE "${TARGET_SOURCE}/include")
set_target_properties(portaudio_static PROPERTIES OSX_ARCHITECTURES "")

if(NOT UNIX)
    return()
endif()

include(CheckIncludeFile)

target_include_directories(portaudio_static PRIVATE
    "${TARGET_SOURCE}/src/os/unix/")
target_sources(portaudio_static PRIVATE
    "${TARGET_SOURCE}/src/os/unix/pa_unix_hostapis.c"
    "${TARGET_SOURCE}/src/os/unix/pa_unix_util.c")

if(APPLE)
    target_compile_definitions(portaudio_static PUBLIC PA_USE_COREAUDIO=1)
    target_sources(portaudio_static PRIVATE
        "${TARGET_SOURCE}/src/hostapi/coreaudio/pa_mac_core.c"
        "${TARGET_SOURCE}/src/hostapi/coreaudio/pa_mac_core_utilities.c"
        "${TARGET_SOURCE}/src/hostapi/coreaudio/pa_mac_core_blocking.c"
        "${TARGET_SOURCE}/src/common/pa_ringbuffer.c")
else()
    option(PA_WITH_ALSA "Enable support for ALSA" OFF)
    if(PA_WITH_ALSA)
        find_package(ALSA REQUIRED)
        target_compile_definitions(portaudio_static PUBLIC PA_USE_ALSA=1)
        target_sources(portaudio_static PRIVATE
            "${TARGET_SOURCE}/src/hostapi/alsa/pa_linux_alsa.c")
        target_link_libraries(portaudio_static PUBLIC ALSA::ALSA)
    endif()

    option(PA_WITH_OSS "Enable support for OSS" OFF)
    if(PA_WITH_OSS)
        check_include_file(sys/soundcard.h HAVE_SYS_SOUNDCARD_H)
        check_include_file(linux/soundcard.h HAVE_LINUX_SOUNDCARD_H)
        check_include_file(machine/soundcard.h HAVE_MACHINE_SOUNDCARD_H)
        if(NOT (HAVE_SYS_SOUNDCARD_H OR HAVE_LINUX_SOUNDCARD_H OR HAVE_MACHINE_SOUNDCARD_H))
            message(FATAL_ERROR "Couldn't find OSS")
        endif()
        target_compile_definitions(portaudio_static PUBLIC
            PA_USE_OSS=1
            $<$<BOOL:${HAVE_SYS_SOUNDCARD_H}>:HAVE_SYS_SOUNDCARD_H>
            $<$<BOOL:${HAVE_LINUX_SOUNDCARD_H}>:HAVE_LINUX_SOUNDCARD_H>
            $<$<BOOL:${HAVE_MACHINE_SOUNDCARD_H}>:HAVE_MACHINE_SOUNDCARD_H>)
        target_sources(portaudio_static PRIVATE
            "${TARGET_SOURCE}/src/hostapi/oss/pa_unix_oss.c")
    endif()
endif()
