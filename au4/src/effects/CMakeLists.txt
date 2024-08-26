#
# Audacity: A Digital Audio Editor
#
declare_module(effects)

set(MODULE_QRC effects.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/effectsettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/effectsettings.h
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/effectstypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsviewregister.h

    # internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.h

    # view
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewregister.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectbuilder.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectbuilder.h

    # built-in
    ${CMAKE_CURRENT_LIST_DIR}/builtin/builtineffects.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/builtineffects.h
    ${CMAKE_CURRENT_LIST_DIR}/builtin/abstracteffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/abstracteffectmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/builtin/general/generalviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/general/generalviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/builtin/amplify/amplifyeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/amplify/amplifyeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/builtin/amplify/amplifyviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/amplify/amplifyviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/builtin/tonegen/chirpeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/tonegen/chirpeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/builtin/tonegen/toneeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtin/tonegen/toneeffect.h

    # temp_concept
    ${CMAKE_CURRENT_LIST_DIR}/tmpconcept/tempconceptexecutor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tmpconcept/tempconceptexecutor.h
    )

if (AU_MODULE_VST)
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3/vst3pluginsscanner.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3/vst3pluginsscanner.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3/vst3pluginsmetareader.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/au3/vst3pluginsmetareader.h
    )
endif()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

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

