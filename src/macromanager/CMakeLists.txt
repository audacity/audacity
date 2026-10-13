#
# Audacity: A Digital Audio Editor
#

muse_create_module(macromanager)

target_sources(macromanager PRIVATE
    macromanagermodule.cpp
    macromanagermodule.h
    macromanagertypes.h
    imacrosrepository.h

    internal/macroserialization.cpp
    internal/macroserialization.h
    internal/macrosrepository.cpp
    internal/macrosrepository.h
)

add_subdirectory(qml/Audacity/MacroManager)
