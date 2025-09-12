#include "dynamicstimeline.h"
#include "dynamicstimelinetypes.h"
#include "timelinesourcetestmodel.h"
#include "meters/compressionmetermodel.h"
#include "meters/dynamicseffectoutputmetermodel.h"
#include "stopwatch.h"
#include <QApplication>
#include <QQmlApplicationEngine>
#include <QSurface>

int main(int argc, char* argv[])
{
    // Enabling anti-aliasing
    QSurfaceFormat fmt;
    fmt.setSamples(4); // Request 4x multisampling
    QSurfaceFormat::setDefaultFormat(fmt);

    QApplication app(argc, argv);

    qmlRegisterType<au::effects::DynamicsTimeline>("Audacity.BuiltinEffects", 1,
                                                   0, "DynamicsTimeline");
    qmlRegisterType<au::effects::DynamicsSample>("Audacity.BuiltinEffects", 1, 0,
                                                 "DynamicsSample");
    qmlRegisterType<au::effects::TimelineSourceTestModel>(
        "Audacity.BuiltinEffects", 1, 0, "TimelineSourceModel");
    qmlRegisterType<au::effects::Stopwatch>("Audacity.BuiltinEffects", 1, 0,
                                            "Stopwatch");
    qmlRegisterType<au::effects::DynamicsEffectOutputMeterModel>(
        "Audacity.BuiltinEffects", 1, 0, "DynamicsEffectOutputMeterModel");
    qmlRegisterType<au::effects::CompressionMeterModel>("Audacity.BuiltinEffects", 1, 0,
                                                        "CompressionMeterModel");
    QQmlApplicationEngine engine;
    engine.load(QUrl(QStringLiteral("qrc:/qt/qml/dynamics_timeline_testapp/test/main.qml")));
    if (engine.rootObjects().isEmpty()) {
        return -1;
    }

    return app.exec();
}
