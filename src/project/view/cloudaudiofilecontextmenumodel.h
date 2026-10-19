/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

namespace au::project {
class CloudAudioFileContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    CloudAudioFileContextMenuModel(QString audioId, QString slug, QObject* parent = nullptr);

    Q_INVOKABLE void load() override;

private:
    QString m_audioId;
    QString m_slug;
};
}
