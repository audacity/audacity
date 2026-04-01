/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "workspace/iworkspacemanager.h"
#include "importexport/import/iimporterconfiguration.h"

namespace au::appshell {
class MusicPreferencesModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    muse::ContextInject<au::importexport::IImporterConfiguration> importerConfiguration { this };
    muse::ContextInject<muse::workspace::IWorkspaceManager> workspacesManager { this };

    Q_PROPERTY(bool tempoDetectionEnabled READ tempoDetectionEnabled NOTIFY tempoDetectionEnabledChanged)

public:
    explicit MusicPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool tempoDetectionEnabled() const;
    Q_INVOKABLE void setTempoDetectionEnabled(bool enabled);

signals:
    void tempoDetectionEnabledChanged();
};
}
