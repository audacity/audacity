#
# Audacity: A Digital Audio Editor
#

muse_create_module(macromanager)

target_sources(macromanager PRIVATE
    macromanagermodule.cpp
    macromanagermodule.h
    macromanagertypes.h
    imacrosrepository.h
    imacrorunner.h
    irunonfilesrequests.h

    internal/macrooutputnames.cpp
    internal/macrooutputnames.h
    internal/macrorunner.cpp
    internal/macrorunner.h
    internal/macroserialization.cpp
    internal/macroserialization.h
    internal/macrosrepository.cpp
    internal/macrosrepository.h
    internal/runonfilesrequests.cpp
    internal/runonfilesrequests.h
    internal/runonfilesstarter.cpp
    internal/runonfilesstarter.h
)

add_subdirectory(qml/Audacity/MacroManager)

if (AU_BUILD_MACROMANAGER_TESTS)
    add_subdirectory(tests)
endif()
