# Project-adjacent AI workspace contracts; no .aup4 schema changes.
declare_module(aiproject)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/aiworkspace.cpp
    ${CMAKE_CURRENT_LIST_DIR}/aiworkspace.h
)

set(MODULE_LINK aicore)

setup_module()
