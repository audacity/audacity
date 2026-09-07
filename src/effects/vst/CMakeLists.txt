#
# Audacity: A Digital Audio Editor
#
muse_create_module(effects_vst NO_UNITY)

target_sources(effects_vst PRIVATE
    vsteffectsmodule.cpp
    vsteffectsmodule.h

    internal/vst3effectloader.h
    internal/vst3pluginsscanner.h
    internal/vst3pluginsmetareader.cpp
    internal/vst3pluginsmetareader.h
    internal/vst3viewlauncher.cpp
    internal/vst3viewlauncher.h
    internal/vstparameterextractorservice.cpp
    internal/vstparameterextractorservice.h
    internal/vsttypes.h

    # for muse
    internal/musevstplugininstance.cpp
    internal/musevstplugininstance.h
    internal/musevstpluginsregister.cpp
    internal/musevstpluginsregister.h
)

target_link_libraries(effects_vst PRIVATE
    au3wrap
    au3-components
    au3-realtime-effects
    au3-vst3
)

if (MUSE_MODULE_VST_QML)
    target_link_libraries(effects_vst PRIVATE muse::vst_qml)
endif()

add_subdirectory(qml/Audacity/Vst)
