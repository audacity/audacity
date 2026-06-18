#
# Audacity: A Digital Audio Editor
#
declare_module(effects_builtin_collection)

set(MODULE_QRC builtin_effects_collection.qrc)

set(MODULE_SRC
    # common
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectscollectionmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectscollectionmodule.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/builtincollectionloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtincollectionloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtinviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtinviewlauncher.h

    ${CMAKE_CURRENT_LIST_DIR}/common/params.h
    ${CMAKE_CURRENT_LIST_DIR}/common/builtineffectsettingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/builtineffectsettingmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffect.h
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/generatoreffectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/common/measureunits.h

    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/valuewarper.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/valuewarper.h
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/valuewarpertypes.h
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/warpingtransformer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/common/valuewarper/warpingtransformer.h

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

    ${CMAKE_CURRENT_LIST_DIR}/dynamics/compressor/compressoreffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/compressor/compressoreffect.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/compressor/compressorviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/compressor/compressorviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/compressor/compressorsettingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/compressor/compressorsettingmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/limiter/limiterviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/limiter/limiterviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/limiter/limitereffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/limiter/limitereffect.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/limiter/limitersettingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/limiter/limitersettingmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/dynamicseffectsmacros.h

    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/normalize/normalizeviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/removedcoffset/removedcoffseteffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/removedcoffset/removedcoffseteffect.h

    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/meters/abstractdbmetermodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/meters/abstractdbmetermodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/meters/compressiondbmetermodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/meters/compressiondbmetermodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/meters/outputdbmetermodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/meters/outputdbmetermodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/painters/abstractsequencepainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/painters/abstractsequencepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/painters/areasequencepainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/painters/areasequencepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/painters/linesequencepainter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/painters/linesequencepainter.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/abstractdynamicseffectinstancemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/abstractdynamicseffectinstancemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/dynamicstimeline.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/dynamicstimeline.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/timelinesourcemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/timelinesourcemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/stopwatch.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/stopwatch.h
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/dynamicsplaystatemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dynamics/timeline/dynamicsplaystatemodel.h

    ${CMAKE_CURRENT_LIST_DIR}/tonegen/chirpeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/chirpeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneeffect.h

    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/reverb/reverbviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/paulstretch/paulstretcheffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/paulstretch/paulstretcheffect.h
    ${CMAKE_CURRENT_LIST_DIR}/paulstretch/paulstretchviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/paulstretch/paulstretchviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tonegen/toneviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfgenerator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfgenerator.h
    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dtmfgen/dtmfviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/silencegen/silencegenerator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/silencegen/silencegenerator.h
    ${CMAKE_CURRENT_LIST_DIR}/silencegen/silenceviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/slidingstretch/slidingstretcheffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/slidingstretch/slidingstretcheffect.h
    ${CMAKE_CURRENT_LIST_DIR}/slidingstretch/slidingstretchviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/slidingstretch/slidingstretchviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/slidingstretch/slidingstretchsettingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/slidingstretch/slidingstretchsettingmodel.h

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

    ${CMAKE_CURRENT_LIST_DIR}/equalization_common/equalizationenvelopeutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/equalization_common/equalizationenvelopeutils.h

    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/equalizationbandsliders.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/equalizationbandsliders.h
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceq.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceq.h
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqbandsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqbandsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/graphiceq/graphiceqviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/filtercurveeq/filtercurveeq.cpp
    ${CMAKE_CURRENT_LIST_DIR}/filtercurveeq/filtercurveeq.h
    ${CMAKE_CURRENT_LIST_DIR}/filtercurveeq/filtercurvemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/filtercurveeq/filtercurvemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/filtercurveeq/filtercurveeqviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/filtercurveeq/filtercurveeqviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/invert/inverteffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/invert/inverteffect.h

    ${CMAKE_CURRENT_LIST_DIR}/reverse/reverseeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/reverse/reverseeffect.h

    ${CMAKE_CURRENT_LIST_DIR}/repair/repaireffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/repair/repaireffect.h

    ${CMAKE_CURRENT_LIST_DIR}/truncatesilence/truncatesilenceeffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/truncatesilence/truncatesilenceeffect.h
    ${CMAKE_CURRENT_LIST_DIR}/truncatesilence/truncatesilenceviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/truncatesilence/truncatesilenceviewmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/changepitch/changepitcheffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/changepitch/changepitcheffect.h
    ${CMAKE_CURRENT_LIST_DIR}/changepitch/changepitchviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/changepitch/changepitchviewmodel.h
    )

# AU3
set(MODULE_DEF ${MODULE_DEF}
    -DBUILTIN_EFFECTS_API=
    # Note: USE_SOUNDTOUCH and USE_SBSMS are now set as PUBLIC compile definitions
    # by the soundtouch and libsbsms targets, so they propagate automatically when linked
    # Note: NOTE_TRACK_API is only needed if USE_MIDI is defined
    # Currently MIDI support is not enabled in AU4
    # -DNOTE_TRACK_API=
)

set(MODULE_LINK au3wrap libsbsms shared)
set(MODULE_LINK_PUBLIC au3-effects au3-builtin-effects)

if (AUDACITY_USE_DEEPFILTERNET)
    find_package(Threads REQUIRED)
    find_program(CARGO_EXECUTABLE cargo)
    if (NOT CARGO_EXECUTABLE)
        message(FATAL_ERROR "AUDACITY_USE_DEEPFILTERNET requires Cargo/Rust, but cargo was not found.")
    endif()

    set(DEEPFILTERNET_RUST_MANIFEST ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/rust/Cargo.toml)
    set(DEEPFILTERNET_RUST_LOCK ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/rust/Cargo.lock)
    set(DEEPFILTERNET_RUST_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/deepfilternet-rust)
    set(DEEPFILTERNET_RUST_LIB
        ${DEEPFILTERNET_RUST_TARGET_DIR}/release/${CMAKE_STATIC_LIBRARY_PREFIX}audacity_deepfilternet${CMAKE_STATIC_LIBRARY_SUFFIX}
    )

    add_custom_command(
        OUTPUT ${DEEPFILTERNET_RUST_LIB}
        COMMAND ${CMAKE_COMMAND} -E env CARGO_TARGET_DIR=${DEEPFILTERNET_RUST_TARGET_DIR}
            ${CARGO_EXECUTABLE} build --manifest-path ${DEEPFILTERNET_RUST_MANIFEST} --release
        DEPENDS
            ${DEEPFILTERNET_RUST_MANIFEST}
            ${DEEPFILTERNET_RUST_LOCK}
            ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/rust/src/lib.rs
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/rust
        COMMENT "Building DeepFilterNet Rust backend"
        VERBATIM
    )

    add_custom_target(audacity_deepfilternet_rust DEPENDS ${DEEPFILTERNET_RUST_LIB})
    add_library(audacity_deepfilternet_backend STATIC IMPORTED GLOBAL)
    add_dependencies(audacity_deepfilternet_backend audacity_deepfilternet_rust)
    set_target_properties(audacity_deepfilternet_backend PROPERTIES
        IMPORTED_LOCATION ${DEEPFILTERNET_RUST_LIB}
    )

    list(APPEND MODULE_SRC
        ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/deepfilternet_backend.h
        ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/deepfilterneteffect.cpp
        ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/deepfilterneteffect.h
        ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/deepfilternetviewmodel.cpp
        ${CMAKE_CURRENT_LIST_DIR}/deepfilternet/deepfilternetviewmodel.h
    )

    list(APPEND MODULE_LINK audacity_deepfilternet_backend au3-math Threads::Threads ${CMAKE_DL_LIBS})
    if (WIN32)
        list(APPEND MODULE_LINK advapi32 bcrypt ntdll userenv ws2_32)
    elseif (UNIX AND NOT APPLE)
        list(APPEND MODULE_LINK m)
    endif()
    list(APPEND MODULE_DEF -DAUDACITY_USE_DEEPFILTERNET=1)
endif()

set(MODULE_USE_UNITY OFF)

setup_module()
