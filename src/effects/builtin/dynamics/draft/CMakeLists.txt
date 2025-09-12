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

qt_add_executable(dynamics_timeline_testapp ${PROJECT_SOURCES})

qt_add_qml_module(dynamics_timeline_testapp
    URI qtquickapplication4
    VERSION 1.0
    RESOURCE_PREFIX "/qt/qml/"
    QML_FILES
        main.qml
        DynamicsPanel.qml
        meters/DynamicsMeter.qml
)

set_target_properties(dynamics_timeline_testapp
    PROPERTIES
        WIN32_EXECUTABLE TRUE
)

target_link_libraries(dynamics_timeline_testapp
    PUBLIC
        Qt::Quick
        Qt6::Widgets
        Qt6::Graphs
)


qt_import_qml_plugins(dynamics_timeline_testapp)
