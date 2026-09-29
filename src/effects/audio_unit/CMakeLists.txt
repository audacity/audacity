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

    view/audiounitview.cpp
    view/audiounitview.h
    view/audiounitviewmodel.cpp
    view/audiounitviewmodel.h
    view/audiounitcontrol.mm
    view/audiounitcontrol.h
)

set_source_files_properties(view/audiounitcontrol.mm PROPERTIES
    SKIP_UNITY_BUILD_INCLUSION ON
    SKIP_PRECOMPILE_HEADERS ON
)

qt_add_resources(effects_audio_unit_RCC_SOURCES audiounit.qrc)
target_sources(effects_audio_unit PRIVATE ${effects_audio_unit_RCC_SOURCES})

add_qml_import_path(${CMAKE_CURRENT_LIST_DIR}/qml)

target_link_libraries(effects_audio_unit PRIVATE
    au3wrap
    au3-effects
    au3-realtime-effects
    au3-audio-unit
)
