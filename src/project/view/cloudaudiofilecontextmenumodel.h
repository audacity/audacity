/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

namespace au::project {
class CloudAudioFileContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    Q_PROPERTY(QString cloudItemId READ cloudItemId WRITE setCloudItemId NOTIFY cloudItemIdChanged)

public:
    CloudAudioFileContextMenuModel() = default;

    Q_INVOKABLE void load() override;
    void handleMenuItem(const QString& itemId) override;

    QString cloudItemId() const;
    void setCloudItemId(const QString& cloudItemId);

signals:
    void cloudItemIdChanged();

private:
    QString m_cloudItemId;
};
}
