message(STATUS "Setup development environment")

configure_file(
    ${CMAKE_SOURCE_DIR}/muse/tools/codestyle/uncrustify_muse.cfg
    ${CMAKE_SOURCE_DIR}/uncrustify.cfg
    COPYONLY
)
