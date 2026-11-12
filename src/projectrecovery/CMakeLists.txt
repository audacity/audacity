#
# Audacity: A Digital Audio Editor
#

muse_create_module(projectrecovery)

target_sources(projectrecovery PRIVATE
    internal/binaryxml.cpp
    internal/binaryxml.h
    internal/database.cpp
    internal/database.h
    internal/projectdocument.cpp
    internal/projectdocument.h
    internal/projectinspector.cpp
    internal/projectinspector.h
    internal/workingcopy.cpp
    internal/workingcopy.h
)

target_link_libraries(projectrecovery PRIVATE sqlite au3-project au3-sqlite-helpers)

if (AU_BUILD_PROJECTRECOVERY_TESTS)
    add_subdirectory(tests)
endif()
