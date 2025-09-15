#include "dynamicstimeline.h"
#include "dynamicstimelinetypes.h"
#include "timelinesourcetestmodel.h"
#include "meters/compressionmetertestmodel.h"
#include "meters/dynamicseffectoutputmetertestmodel.h"
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

    using namespace au::effects;
    qmlRegisterType<DynamicsTimeline>("Audacity.BuiltinEffects", 1, 0, "DynamicsTimeline");
    qmlRegisterType<DynamicsSample>("Audacity.BuiltinEffects", 1, 0, "DynamicsSample");
    qmlRegisterType<TimelineSourceTestModel>("Audacity.BuiltinEffects", 1, 0, "TimelineSourceModel");
    qmlRegisterType<Stopwatch>("Audacity.BuiltinEffects", 1, 0, "Stopwatch");
    qmlRegisterType<DynamicsEffectOutputMeterTestModel>("Audacity.BuiltinEffects", 1, 0, "DynamicsEffectOutputMeterModel");
    qmlRegisterType<CompressionMeterTestModel>("Audacity.BuiltinEffects", 1, 0, "CompressionMeterModel");

    QQmlApplicationEngine engine;
    engine.load(QUrl(QStringLiteral("qrc:/qt/qml/dynamics_timeline_testapp/test/main.qml")));
    if (engine.rootObjects().isEmpty()) {
        return -1;
    }

    return app.exec();
}
