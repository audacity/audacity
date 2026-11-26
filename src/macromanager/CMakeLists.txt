#
# Audacity: A Digital Audio Editor
#

muse_create_module(macromanager)

target_sources(macromanager PRIVATE
    macromanagermodule.cpp
    macromanagermodule.h
    macromanagercommands.cpp
    macromanagercommands.h
    macromanagertypes.h
    imacrosrepository.h
    imacrorunner.h
    imacrostephandler.h
    imacrostephandlersregister.h
    irunonfilesrequests.h

    internal/macrooutputnames.cpp
    internal/macrooutputnames.h
    internal/macroscommandscontroller.cpp
    internal/macroscommandscontroller.h
    internal/macroscommandsregister.cpp
    internal/macroscommandsregister.h
    internal/macroscommandsstate.cpp
    internal/macroscommandsstate.h
    internal/macrorunner.cpp
    internal/macrorunner.h
    internal/macroserialization.cpp
    internal/macroserialization.h
    internal/macrosrepository.cpp
    internal/macrosrepository.h
    internal/macrostephandlersregister.cpp
    internal/macrostephandlersregister.h
    internal/runonfilesrequests.cpp
    internal/runonfilesrequests.h
    internal/runonfilesstarter.cpp
    internal/runonfilesstarter.h
)

target_link_libraries(macromanager PRIVATE shared)

add_subdirectory(qml/Audacity/MacroManager)

if (AU_BUILD_MACROMANAGER_TESTS)
    add_subdirectory(tests)
endif()
