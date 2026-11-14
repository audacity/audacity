#
# Audacity: A Digital Audio Editor
#

muse_create_module(uicomponents)

target_sources(uicomponents PRIVATE
    uicomponentsmodule.cpp
    uicomponentsmodule.h
    types/numerictypes.h
    )

target_link_libraries(uicomponents PRIVATE
    muse::uicomponents_qml
    Qt::Quick
)

add_subdirectory(qml/Audacity/UiComponents)

if (AU_BUILD_UICOMPONENTS_TESTS)
    add_subdirectory(tests)
endif()
