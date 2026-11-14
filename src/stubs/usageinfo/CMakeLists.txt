declare_module(usageinfo)

set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/usageinfostubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/usageinfostubmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/usageinfostub.cpp
    ${CMAKE_CURRENT_LIST_DIR}/usageinfostub.h
)

setup_module()
