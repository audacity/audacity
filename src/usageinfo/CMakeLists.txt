#
# Audacity: A Digital Audio Editor
#

declare_module(usageinfo)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/usageinfomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/usageinfomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iusageinfo.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/usageinfoservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/usageinfoservice.h
    )

set(MODULE_LINK Qt6::Network)

setup_module()
