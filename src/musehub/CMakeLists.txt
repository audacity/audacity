#
# Audacity: A Digital Audio Editor
#

muse_create_module(musehub)

target_sources(musehub PRIVATE
    musehubmodule.cpp
    musehubmodule.h
    musehubcommands.h
    internal/musehubcommandscontroller.cpp
    internal/musehubcommandscontroller.h
    internal/musehubcommandsregister.cpp
    internal/musehubcommandsregister.h
    internal/musehubcommandsstate.cpp
    internal/musehubcommandsstate.h
)

target_link_libraries(musehub PRIVATE au3-musehub Qt::Quick)

add_subdirectory(qml/Audacity/MuseHub)
