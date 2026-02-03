#
# Audacity: A Digital Audio Editor
#

declare_module(au3cloud)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3cloudmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3cloudmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iauthorization.h
    ${CMAKE_CURRENT_LIST_DIR}/iuserdata.h
    ${CMAKE_CURRENT_LIST_DIR}/iusageinfo.h

    ${CMAKE_CURRENT_LIST_DIR}/dev/cloudtestsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dev/cloudtestsmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/oauthhttpserverreplyhandler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/oauthhttpserverreplyhandler.h
    )

set(MODULE_LINK au3wrap Qt6::Network Qt6::NetworkAuth)

setup_module()
