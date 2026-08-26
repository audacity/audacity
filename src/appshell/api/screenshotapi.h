/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QString>

#include "framework/global/api/apiobject.h"
#include "framework/global/modularity/ioc.h"
#include "framework/testflow/itestflowconfiguration.h"
#include "framework/ui/imainwindow.h"

namespace au::appshell::api {
//! Lets testflow scripts capture the main window, for screenshot comparison on CI.
//! Scripts obtain it with `require("Audacity.Screenshot")`.
class ScreenshotApi : public muse::api::ApiObject
{
    Q_OBJECT

    muse::ContextInject<muse::ui::IMainWindow> mainWindow = { this };
    muse::GlobalInject<muse::testflow::ITestflowConfiguration> testflowConfiguration;

public:
    explicit ScreenshotApi(muse::api::IApiEngine* e);

    //! Grabs the main window into <dir>/<name>.png, where <dir> is
    //! $AU_TESTFLOW_SCREENSHOTS_DIR if set, else <testflow data path>/screenshots.
    Q_INVOKABLE bool save(const QString& name);

private:
    QString outputDir() const;
};
}
