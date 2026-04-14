muse_create_module(preferences)

target_sources(preferences PRIVATE
    preferencesmodule.cpp
    preferencesmodule.h
)

target_link_libraries(preferences PRIVATE importer)

add_subdirectory(qml/Audacity/Preferences)
