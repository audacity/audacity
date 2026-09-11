muse_create_module(effects_vst STUB)

target_sources(effects_vst PRIVATE
    vsteffectsstubmodule.cpp
    vsteffectsstubmodule.h
)

add_subdirectory(qml/Audacity/Vst)
