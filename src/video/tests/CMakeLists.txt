#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au_video_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/pixelconvert_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/ffmpegsoftwarebackend_tests.cpp
    )

set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    )

set(MODULE_TEST_LINK
    video
    au3wrap
    au3-ffmpeg-support
    au3-mod-ffmpeg
    )

# The media fixtures are generated rather than committed; see
# tools/videosync/make_fixtures.py. Tests that need them skip when they are
# absent, which is also what happens on CI, since no job installs FFmpeg.
set(MODULE_TEST_DATA_ROOT ${CMAKE_SOURCE_DIR}/tools/videosync)

include(SetupGTest)
