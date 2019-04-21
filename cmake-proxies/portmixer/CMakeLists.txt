#directory cmake-proxies/portmixer
set(TARGET portmixer)
set(TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET})
project(${TARGET})

include(CheckIncludeFiles)

get_target_property(PA_DEFINITIONS portaudio_static INTERFACE_COMPILE_DEFINITIONS)

add_library(${TARGET} STATIC
    "${TARGET_SOURCE}/src/px_mixer.c"
    "$<$<BOOL:${WIN32}>:${TARGET_SOURCE}/src/px_win_common.c>"
    "$<$<BOOL:${WIN32}>:${TARGET_SOURCE}/src/px_win_ds.c>"
    "$<$<BOOL:${WIN32}>:${TARGET_SOURCE}/src/px_win_endpoint.c>"
    "$<$<BOOL:${WIN32}>:${TARGET_SOURCE}/src/px_win_wasapi.c>"
    "$<$<BOOL:${WIN32}>:${TARGET_SOURCE}/src/px_win_wmme.c>"
    "$<$<IN_LIST:PA_USE_ALSA=1,${PA_DEFINITIONS}>:${TARGET_SOURCE}/src/px_linux_alsa.c>"
    "$<$<IN_LIST:PA_USE_OSS=1,${PA_DEFINITIONS}>:${TARGET_SOURCE}/src/px_unix_oss.c>"
    "$<$<IN_LIST:PA_USE_COREAUDIO=1,${PA_DEFINITIONS}>:${TARGET_SOURCE}/src/px_mac_coreaudio.c>")


target_compile_definitions(${TARGET} PRIVATE
#   $<$<BOOL:${WIN32}>:PX_USE_WIN_DSOUND>
    $<$<BOOL:${WIN32}>:PX_USE_WIN_MME>
    $<$<BOOL:${WIN32}>:PX_USE_WIN_WASAPI>
    $<$<IN_LIST:PA_USE_ALSA=1,${PA_DEFINITIONS}>:PX_USE_LINUX_ALSA>
    $<$<IN_LIST:PA_USE_OSS=1,${PA_DEFINITIONS}>:PX_USE_UNIX_OSS>
    $<$<IN_LIST:PA_USE_COREAUDIO=1,${PA_DEFINITIONS}>:PX_USE_MAC_COREAUDIO>)

target_include_directories(${TARGET} PUBLIC "${TARGET_SOURCE}/include")
target_link_libraries(${TARGET} PUBLIC portaudio_static)
set_target_properties(${TARGET} PROPERTIES OSX_ARCHITECTURES "")
