/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

namespace au::project {
class CloudAudioFileContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    Q_PROPERTY(QString slug READ slug WRITE setSlug NOTIFY slugChanged)

public:
    CloudAudioFileContextMenuModel() = default;

    Q_INVOKABLE void load() override;
    void handleMenuItem(const QString& itemId) override;

    QString slug() const;
    void setSlug(const QString& slug);

signals:
    void slugChanged();

private:
    QString m_slug;
};
}
