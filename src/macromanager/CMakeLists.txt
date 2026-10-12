#
# Audacity: A Digital Audio Editor
#

muse_create_module(macromanager)

target_sources(macromanager PRIVATE
    macromanagermodule.cpp
    macromanagermodule.h
)

add_subdirectory(qml/Audacity/MacroManager)
