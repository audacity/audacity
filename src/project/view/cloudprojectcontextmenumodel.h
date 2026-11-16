/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"
#include "framework/global/io/path.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iplatforminteractive.h"

namespace au::project {
class CloudProjectContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;

public:
    CloudProjectContextMenuModel(QString projectId, muse::io::path_t localPath, QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
    void handleMenuItem(const QString& itemId) override;

private:
    QString m_projectId;
    muse::io::path_t m_localPath;
};
}
