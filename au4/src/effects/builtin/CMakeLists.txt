#
# Audacity: A Digital Audio Editor
#
declare_module(effects_builtin)

set(MODULE_QRC effects_builtin.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    # common
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ibuiltineffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/general/generalviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/general/generalviewmodel.h

    # built-in effects
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/tonegen/chirpeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/chirpeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneeffect.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(AU3_SRC
    ${AU3_LIBRARIES}/lib-effects/StatefulPerTrackEffect.cpp
    ${AU3_LIBRARIES}/lib-effects/StatefulPerTrackEffect.h
    ${AU3_LIBRARIES}/lib-builtin-effects/AmplifyBase.cpp
    ${AU3_LIBRARIES}/lib-builtin-effects/AmplifyBase.h
    ${AU3_LIBRARIES}/lib-builtin-effects/ToneGenBase.cpp
    ${AU3_LIBRARIES}/lib-builtin-effects/ToneGenBase.h
)

set(AU3_DEF ${AU3_DEF}
    -DBUILTIN_EFFECTS_API=
)

set(MODULE_SRC ${MODULE_SRC}
    ${AU3_SRC}
)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

if(AU_MODULE_VST)
    set(MODULE_DEF ${MODULE_DEF} -DAU_MODULE_VST)
endif()

set(MODULE_LINK au3wrap)

setup_module()
