cmake_minimum_required(VERSION 3.16)
project(QtQuickApplication4 LANGUAGES CXX)

include(qt.cmake)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Core)
find_package(Qt6 REQUIRED COMPONENTS Widgets Charts)
find_package(Qt${QT_VERSION_MAJOR}
    COMPONENTS
        Quick
)
qt_standard_project_setup()

set(PROJECT_SOURCES
    painters/abstractsequencepainter.cpp
    painters/abstractsequencepainter.h
    painters/areasequencepainter.cpp
    painters/areasequencepainter.h
    painters/linesequencepainter.cpp
    painters/linesequencepainter.h
    main.cpp

    meters/abstractdynamicsmetermodel.cpp
    meters/abstractdynamicsmetermodel.h
    meters/compressionmetermodel.h
    meters/dynamicseffectoutputmetermodel.h

    dynamicstimeline.cpp
    dynamicstimeline.h
    dynamicstimelinemodel.cpp
    dynamicstimelinemodel.h

    profiler.cpp
    profiler.h
    stopwatch.cpp
    stopwatch.h
)

if(QT_VERSION VERSION_LESS 5.15)
    qt5_add_resources(PROJECT_SOURCES qml.qrc)
elseif(QT_VERSION VERSION_LESS 6.2)
    qt_add_resources(PROJECT_SOURCES qml.qrc)
endif()

qt_add_executable(${PROJECT_NAME} ${PROJECT_SOURCES})

if(QT_VERSION VERSION_GREATER_EQUAL 6.2)
    qt_add_qml_module(${PROJECT_NAME}
        URI qtquickapplication4
        VERSION 1.0
        RESOURCE_PREFIX "/qt/qml/"
        QML_FILES
            main.qml
            DynamicsPanel.qml
            meters/DynamicsMeter.qml
    )
endif()

set_target_properties(${PROJECT_NAME}
    PROPERTIES
        WIN32_EXECUTABLE TRUE
)

target_link_libraries(${PROJECT_NAME}
    PUBLIC
        Qt::Quick
        Qt6::Widgets
        Qt6::Charts
)


if(Qt6_FOUND)
    qt_import_qml_plugins(${PROJECT_NAME})
endif()
