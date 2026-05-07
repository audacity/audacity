#
# Audacity: A Digital Audio Editor
#

declare_module(au3cloud)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3cloudmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3cloudmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/au3clouderrors.h
    ${CMAKE_CURRENT_LIST_DIR}/cloudtypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iauthorization.h
    ${CMAKE_CURRENT_LIST_DIR}/iau3audiocomservice.h
    ${CMAKE_CURRENT_LIST_DIR}/iau3cloudconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iusageinfo.h

    ${CMAKE_CURRENT_LIST_DIR}/dev/cloudtestsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dev/cloudtestsmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiocomservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiocomservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/clouduiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/clouduiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudprojectfileioextension.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudprojectfileioextension.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3cloudservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/oauthhttpserverreplyhandler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/oauthhttpserverreplyhandler.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiocomtypeconv.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiocomtypeconv.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/downloadmanager.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/downloadmanager.h

    ${CMAKE_CURRENT_LIST_DIR}/view/accountmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/accountmodel.h
    )

set(MODULE_LINK au3wrap Qt6::Network Qt6::NetworkAuth)

setup_module()
