add_library(extension_native STATIC
    bytebuffer.cpp
    bytebuffer.h
)

target_include_directories(extension_native PUBLIC ${PROJECT_SOURCE_DIR}/src)
target_link_libraries(extension_native PUBLIC Qt::Core Qt::Qml)
