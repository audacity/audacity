muse_create_module(effects_audio_unit STUB)

target_sources(effects_audio_unit PRIVATE
    audiouniteffectsstubmodule.cpp
    audiouniteffectsstubmodule.h
)

qt_add_resources(effects_audio_unit_RCC_SOURCES audiounit.qrc)
target_sources(effects_audio_unit PRIVATE ${effects_audio_unit_RCC_SOURCES})

add_qml_import_path(${CMAKE_CURRENT_LIST_DIR}/qml)
