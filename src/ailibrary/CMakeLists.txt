# Persistent, project-scoped AI asset Library. It deliberately has no editor or provider dependency.
declare_module(ailibrary)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/projectasset.h
    ${CMAKE_CURRENT_LIST_DIR}/globalassetcatalogue.h
    ${CMAKE_CURRENT_LIST_DIR}/globalassetcatalogue.cpp
    ${CMAKE_CURRENT_LIST_DIR}/libraryassetstore.h
    ${CMAKE_CURRENT_LIST_DIR}/libraryassetstore.cpp
)

set(MODULE_LINK aicore aiproject)

setup_module()

if (MUSE_ENABLE_UNIT_TESTS)
    add_subdirectory(tests)
endif()
