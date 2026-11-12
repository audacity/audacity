#
# Audacity: A Digital Audio Editor
#

muse_create_module(projectrecovery)

target_sources(projectrecovery PRIVATE
    projectrecoverymodule.cpp
    projectrecoverymodule.h
    projectrecoverycommands.h
    iprojectrecoveryservice.h

    internal/binaryxml.cpp
    internal/binaryxml.h
    internal/database.cpp
    internal/database.h
    internal/documentedit.cpp
    internal/documentedit.h
    internal/projectdocument.cpp
    internal/projectdocument.h
    internal/projectinspector.cpp
    internal/projectinspector.h
    internal/projectrecoverycommandscontroller.cpp
    internal/projectrecoverycommandscontroller.h
    internal/projectrecoverycommandsregister.cpp
    internal/projectrecoverycommandsregister.h
    internal/projectrecoverycommandsstate.cpp
    internal/projectrecoverycommandsstate.h
    internal/projectrecoveryservice.cpp
    internal/projectrecoveryservice.h
    internal/projectrepairer.cpp
    internal/projectrepairer.h
    internal/workingcopy.cpp
    internal/workingcopy.h
)

target_link_libraries(projectrecovery PRIVATE sqlite au3-project au3-sqlite-helpers)

add_subdirectory(qml/Audacity/ProjectRecovery)

if (AU_BUILD_PROJECTRECOVERY_TESTS)
    add_subdirectory(tests)
endif()
