#
# Audacity: A Digital Audio Editor
#
declare_module(effects)

set(MODULE_QRC effects.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/effectstypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsuiengine.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiengine.h

    ${CMAKE_CURRENT_LIST_DIR}/view/effectbuilder.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectbuilder.h

    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/builtineffects/amplify/amplify.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffects/amplify/amplify.h
    )

install(DIRECTORY
      builtineffects
      DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}
      PATTERN ".*" EXCLUDE
      PATTERN "*.h" EXCLUDE
      PATTERN "*.cpp" EXCLUDE
      )

setup_module()
