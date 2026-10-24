muse_create_module(effects_audio_unit STUB)

target_sources(effects_audio_unit PRIVATE
    audiouniteffectsstubmodule.cpp
    audiouniteffectsstubmodule.h
)

add_subdirectory(qml/Audacity/AudioUnit)
