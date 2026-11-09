/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QVariantList>

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

#include "audio/iaudiobackendselector.h"
#include "audio/iaudioengine.h"

namespace au::projectscene {
//! The audio engines, to switch between them from the playback toolbar: as in the audio preferences, at once.
//! Also shows the latency of the active engine's device.
class AudioEngineToolBarItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(QVariantList engines READ engines NOTIFY enginesChanged FINAL)
    Q_PROPERTY(QString currentEngine READ currentEngine NOTIFY enginesChanged FINAL)
    Q_PROPERTY(QString latency READ latency NOTIFY latencyChanged FINAL)

    muse::GlobalInject<audio::IAudioBackendSelector> audioBackendSelector;
    muse::GlobalInject<audio::IAudioEngine> audioEngine;
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    explicit AudioEngineToolBarItem(muse::uicomponents::ToolBarItemType::Type type, QObject* parent = nullptr);

    Q_INVOKABLE void selectEngine(const QString& id);

    QVariantList engines() const;
    QString currentEngine() const;
    //! Input, output and round-trip, as the device last opened reported them
    QString latency() const;

signals:
    void enginesChanged();
    void latencyChanged();

private:
    void updateEngines();

    muse::uicomponents::MenuItemList m_engines;
};
}
