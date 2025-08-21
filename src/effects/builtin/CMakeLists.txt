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
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsviewregister.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtinviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtinviewlauncher.h

    ${CMAKE_CURRENT_LIST_DIR}/common/params.h
    ${CMAKE_CURRENT_LIST_DIR}/common/builtineffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/builtineffectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/common/builtineffectsettingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/builtineffectsettingmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffect.h
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffectmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/valuewarper.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/valuewarper.h
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/valuewarpertypes.h
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/warpingtransformer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/warpingtransformer.h

    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectviewloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectviewloader.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewregister.h

    ${CMAKE_CURRENT_LIST_DIR}/general/generalviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/general/generalviewmodel.h

    # built-in effects
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/amplify/amplifyviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/loudness/normalizeloudnesseffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/loudness/normalizeloudnesseffect.h
    ${CMAKE_CURRENT_LIST_DIR}/loudness/normalizeloudnessviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/loudness/normalizeloudnessviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/clickremoval/clickremovaleffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/clickremoval/clickremovaleffect.h
    ${CMAKE_CURRENT_LIST_DIR}/clickremoval/clickremovalviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/clickremoval/clickremovalviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/compressor/compressoreffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/compressor/compressoreffect.h
    ${CMAKE_CURRENT_LIST_DIR}/compressor/compressorviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/compressor/compressorviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/limiter/limiterviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/limiter/limiterviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/limiter/limitereffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/limiter/limitereffect.h

    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeviewmodel.h

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

    ${CMAKE_CURRENT_LIST_DIR}/silencegen/silencegenerator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/silencegen/silencegenerator.h
    ${CMAKE_CURRENT_LIST_DIR}/silencegen/silenceviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noisegenerator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noisegenerator.h
    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noiseviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/noisegen/noiseviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/noisereduction/noisereductioneffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/noisereduction/noisereductioneffect.h
    ${CMAKE_CURRENT_LIST_DIR}/noisereduction/noisereductionviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/noisereduction/noisereductionviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/fade/fadeeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/fade/fadeeffect.h

    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/equalizationbandsliders.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/equalizationbandsliders.h
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceq.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceq.h
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqbandsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqbandsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqviewmodel.h

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
    ${AU3_LIBRARIES}/lib-builtin-effects/SilenceBase.cpp
    ${AU3_LIBRARIES}/lib-builtin-effects/SilenceBase.h
    ${AU3_LIBRARIES}/lib-builtin-effects/EqualizationBase.cpp
    ${AU3_LIBRARIES}/lib-builtin-effects/EqualizationBase.h
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
