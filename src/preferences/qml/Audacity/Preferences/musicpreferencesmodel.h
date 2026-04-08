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

    muse::GlobalInject<au::importexport::IImporterConfiguration> importerConfiguration;

    muse::ContextInject<muse::workspace::IWorkspaceManager> workspacesManager { this };

    Q_PROPERTY(importexport::TempoDetectionPref::TempoDetection tempoDetectionPref READ tempoDetectionPref NOTIFY tempoDetectionPrefChanged)
    Q_PROPERTY(QVariantList tempoDetectionWorkspaces READ tempoDetectionWorkspaces NOTIFY tempoDetectionWorkspacesChanged)
    Q_PROPERTY(
        bool askBeforeSubsequentImport READ askBeforeSubsequentImport WRITE setAskBeforeSubsequentImport NOTIFY askBeforeSubsequentImportChanged)

public:
    explicit MusicPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    importexport::TempoDetectionPref::TempoDetection tempoDetectionPref() const;
    Q_INVOKABLE void setTempoDetectionPref(importexport::TempoDetectionPref::TempoDetection pref);

    QVariantList tempoDetectionWorkspaces() const;
    Q_INVOKABLE void appendToTempoDetectionWorkspaces(const QString& workspaceName);
    Q_INVOKABLE void removeFromTempoDetectionWorkspaces(const QString& workspaceName);

    bool askBeforeSubsequentImport() const;
    void setAskBeforeSubsequentImport(bool ask);

signals:
    void tempoDetectionPrefChanged();
    void tempoDetectionWorkspacesChanged();
    void askBeforeSubsequentImportChanged();
};
}
