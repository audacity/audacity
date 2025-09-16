set(TEST_DIR ${CMAKE_CURRENT_LIST_DIR}/test)

set(PROJECT_SOURCES
    painters/abstractsequencepainter.cpp
    painters/abstractsequencepainter.h
    painters/areasequencepainter.cpp
    painters/areasequencepainter.h
    painters/linesequencepainter.cpp
    painters/linesequencepainter.h
    dynamicscolors.h
    dynamicstimeline.cpp
    dynamicstimeline.h
    dynamicstimelinetypes.h
    profiler.cpp
    profiler.h
    stopwatch.cpp
    stopwatch.h

    ${TEST_DIR}/timelinesourcetestmodel.cpp
    ${TEST_DIR}/timelinesourcetestmodel.h
    ${TEST_DIR}/meters/abstractdynamicsmetertestmodel.cpp
    ${TEST_DIR}/meters/abstractdynamicsmetertestmodel.h
    ${TEST_DIR}/meters/compressionmetertestmodel.h
    ${TEST_DIR}/meters/outputdbmetertestmodel.h
    ${TEST_DIR}/main.cpp
)

qt_add_executable(dynamics_timeline_testapp ${PROJECT_SOURCES})

qt_add_qml_module(dynamics_timeline_testapp
    URI dynamics_timeline_testapp
    VERSION 1.0
    RESOURCE_PREFIX "/qt/qml/"
    QML_FILES
        test/main.qml
        ClipIndicator.qml
        DynamicsPanel.qml
        meters/DynamicsMeter.qml
)

set_target_properties(dynamics_timeline_testapp
    PROPERTIES
        WIN32_EXECUTABLE TRUE
)

target_link_libraries(dynamics_timeline_testapp
    PRIVATE
        Qt::Quick
        Qt6::Widgets
        Qt6::Graphs

        muse::global
)

target_include_directories(dynamics_timeline_testapp
    PRIVATE
        ${MUSE_FRAMEWORK_SRC_PATH})

qt_import_qml_plugins(dynamics_timeline_testapp)
