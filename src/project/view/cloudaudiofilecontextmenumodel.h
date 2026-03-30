/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

namespace au::project {
class CloudAudioFileContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    Q_PROPERTY(QString audioId READ audioId WRITE setAudioId NOTIFY audioIdChanged)
    Q_PROPERTY(QString slug READ slug WRITE setSlug NOTIFY slugChanged)

public:
    CloudAudioFileContextMenuModel() = default;

    Q_INVOKABLE void load() override;
    void handleMenuItem(const QString& itemId) override;

    QString audioId() const;
    void setAudioId(const QString& audioId);

    QString slug() const;
    void setSlug(const QString& slug);

signals:
    void audioIdChanged();
    void slugChanged();

private:
    QString m_audioId;
    QString m_slug;
};
}
