#
# Audacity: A Digital Audio Editor
#
declare_module(effects_builtin)

set(MODULE_QRC effects_builtin.qrc)

set(MODULE_SRC
    # common
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ibuiltineffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtinviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtinviewlauncher.h

    ${CMAKE_CURRENT_LIST_DIR}/common/params.h
    ${CMAKE_CURRENT_LIST_DIR}/common/abstracteffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/abstracteffectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffect.h
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/common/effectmanagemenu.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/effectmanagemenu.h

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

    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfgenerator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfgenerator.h
    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noisegenerator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noisegenerator.h
    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noiseviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noiseviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/fade/fadeeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/fade/fadeeffect.h

    ${CMAKE_CURRENT_LIST_DIR}/invert/inverteffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/invert/inverteffect.h

    ${CMAKE_CURRENT_LIST_DIR}/reverse/reverseeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/reverse/reverseeffect.h

    ${CMAKE_CURRENT_LIST_DIR}/repair/repaireffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/repair/repaireffect.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(AU3_SRC
    ${AU3_LIBRARIES}/lib-effects/StatefulPerTrackEffect.cpp
    ${AU3_LIBRARIES}/lib-effects/StatefulPerTrackEffect.h
    ${AU3_LIBRARIES}/lib-effects/StatefulEffect.cpp
    ${AU3_LIBRARIES}/lib-effects/StatefulEffect.h
    ${AU3_LIBRARIES}/lib-builtin-effects/ToneGenBase.cpp
    ${AU3_LIBRARIES}/lib-builtin-effects/ToneGenBase.h
    ${AU3_LIBRARIES}/lib-builtin-effects/Generator.cpp
    ${AU3_LIBRARIES}/lib-builtin-effects/Generator.h
)

set(AU3_DEF ${AU3_DEF}
    -DBUILTIN_EFFECTS_API=
)

set(MODULE_SRC ${MODULE_SRC}
    ${AU3_SRC}
)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()
