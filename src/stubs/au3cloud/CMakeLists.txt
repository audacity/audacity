declare_module(au3cloud)

set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3cloudstubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3cloudstubmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/au3audiocomservicestub.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audiocomservicestub.h
    ${CMAKE_CURRENT_LIST_DIR}/authorizationstub.cpp
    ${CMAKE_CURRENT_LIST_DIR}/authorizationstub.h
    ${CMAKE_CURRENT_LIST_DIR}/usageinfostub.cpp
    ${CMAKE_CURRENT_LIST_DIR}/usageinfostub.h
)

setup_module()