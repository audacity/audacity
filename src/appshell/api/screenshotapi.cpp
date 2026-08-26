/*
 * Audacity: A Digital Audio Editor
 */
#include "screenshotapi.h"

#include <QDir>
#include <QImage>
#include <QQuickWindow>

#include "log.h"

using namespace au::appshell::api;

ScreenshotApi::ScreenshotApi(muse::api::IApiEngine* e)
    : ApiObject(e)
{
}

QString ScreenshotApi::outputDir() const
{
    const QByteArray fromEnv = qgetenv("AU_TESTFLOW_SCREENSHOTS_DIR");
    if (!fromEnv.isEmpty()) {
        return QString::fromLocal8Bit(fromEnv);
    }
    return testflowConfiguration()->dataPath().toQString() + "/screenshots";
}

bool ScreenshotApi::save(const QString& name)
{
    auto* window = qobject_cast<QQuickWindow*>(mainWindow()->qWindow());
    IF_ASSERT_FAILED(window) {
        return false;
    }

    // Renders a fresh frame synchronously, so the capture reflects the current scene
    const QImage image = window->grabWindow();
    if (image.isNull()) {
        LOGE() << "failed to grab the main window";
        return false;
    }

    const QString dir = outputDir();
    if (!QDir().mkpath(dir)) {
        LOGE() << "failed to create " << dir;
        return false;
    }

    // The file is closed before returning: testflow runs end with std::_Exit,
    // which flushes nothing
    const QString path = dir + "/" + name + ".png";
    if (!image.save(path, "PNG")) {
        LOGE() << "failed to save " << path;
        return false;
    }

    LOGI() << "saved screenshot " << path;
    return true;
}
