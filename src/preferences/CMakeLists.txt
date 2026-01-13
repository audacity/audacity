muse_create_module(preferences)

target_sources(preferences PRIVATE
    preferencesmodule.cpp
    preferencesmodule.h
)

add_subdirectory(qml/Audacity/Preferences)
