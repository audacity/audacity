#
# Audacity: A Digital Audio Editor
#
muse_create_module(effects_audio_unit)

target_sources(effects_audio_unit PRIVATE
    audiouniteffectsmodule.cpp
    audiouniteffectsmodule.h

    internal/audiouniteffectloader.h

    internal/audiounitpluginsscanner.h

    internal/audiounitpluginsmetareader.cpp
    internal/audiounitpluginsmetareader.h

    internal/audiounitviewlauncher.cpp
    internal/audiounitviewlauncher.h
    internal/audiounittypes.h
)

target_link_libraries(effects_audio_unit PRIVATE
    au3wrap
    au3-effects
    au3-realtime-effects
    au3-audio-unit
)

add_subdirectory(qml/Audacity/AudioUnit)
