# Source-built RtAudio dep, fetched at configure time.
# Produces target `rtaudio::rtaudio`.

include(FetchContent)

set(RTAUDIO_BUILD_TESTING     OFF CACHE BOOL "")
set(RTAUDIO_BUILD_STATIC_LIBS ON  CACHE BOOL "")
set(RTAUDIO_BUILD_SHARED_LIBS OFF CACHE BOOL "")
set(RTAUDIO_STATIC_MSVCRT     OFF CACHE BOOL "")

if (OS_IS_WIN)
	set(RTAUDIO_API_DS     ON CACHE BOOL "")
    set(RTAUDIO_API_ASIO  ON CACHE BOOL "")
elseif (OS_IS_LIN OR OS_IS_FBSD)
	set(RTAUDIO_API_ALSA  ON  CACHE BOOL "")
    set(RTAUDIO_API_PULSE ON CACHE BOOL "")
endif()

FetchContent_Declare(
    rtaudio
    GIT_REPOSITORY https://github.com/thestk/rtaudio.git
    GIT_TAG        6.0.1
    GIT_SHALLOW    TRUE
)

FetchContent_GetProperties(rtaudio)
if (NOT rtaudio_POPULATED)
    FetchContent_Populate(rtaudio)
    add_subdirectory(${rtaudio_SOURCE_DIR} ${rtaudio_BINARY_DIR} EXCLUDE_FROM_ALL)
endif()

if (MSVC AND RTAUDIO_API_ASIO)
    target_compile_options(rtaudio PRIVATE /UUNICODE /U_UNICODE)
endif()

if (TARGET rtaudio AND NOT TARGET rtaudio::rtaudio)
    add_library(rtaudio::rtaudio ALIAS rtaudio)
endif()
