muse_create_module(preferences)

target_sources(preferences PRIVATE
    preferencesmodule.cpp
    preferencesmodule.h
)

add_subdirectory(qml/Audacity/Preferences)

if (AU_BUILD_PREFERENCES_TESTS)
    add_subdirectory(tests)
endif()
