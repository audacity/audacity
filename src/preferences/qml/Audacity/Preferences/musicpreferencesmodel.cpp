/*
 * Audacity: A Digital Audio Editor
 */
#include "musicpreferencesmodel.h"

namespace au::appshell {
MusicPreferencesModel::MusicPreferencesModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void MusicPreferencesModel::init()
{
    importerConfiguration()->tempoDetectionPrefChanged().onNotify(this, [this] {
        emit tempoDetectionPrefChanged();
    });

    importerConfiguration()->tempoDetectionWorkspacesChanged().onNotify(this, [this] {
        emit tempoDetectionWorkspacesChanged();
    });

    importerConfiguration()->subsequentImportLoopActionChanged().onNotify(this, [this] {
        emit askBeforeSubsequentImportChanged();
    });
}

importexport::TempoDetectionPref::TempoDetection MusicPreferencesModel::tempoDetectionPref() const
{
    return importerConfiguration()->tempoDetectionPref();
}

void MusicPreferencesModel::setTempoDetectionPref(importexport::TempoDetectionPref::TempoDetection pref)
{
    importerConfiguration()->setTempoDetectionPref(pref);
}

QVariantList MusicPreferencesModel::tempoDetectionWorkspaces() const
{
    QVariantList result;
    for (const auto& workspaceName : importerConfiguration()->tempoDetectionWorkspaces()) {
        result << QString::fromStdString(workspaceName);
    }
    return result;
}

void MusicPreferencesModel::appendToTempoDetectionWorkspaces(const QString& workspaceName)
{
    auto workspaces = importerConfiguration()->tempoDetectionWorkspaces();
    for (const auto& ws : workspaces) {
        if (ws == workspaceName.toStdString()) {
            return;
        }
    }

    workspaces.push_back(workspaceName.toStdString());
    importerConfiguration()->setTempoDetectionWorkspaces(workspaces);
}

void MusicPreferencesModel::removeFromTempoDetectionWorkspaces(const QString& workspaceName)
{
    auto workspaces = importerConfiguration()->tempoDetectionWorkspaces();
    workspaces.erase(std::remove(workspaces.begin(), workspaces.end(), workspaceName.toStdString()), workspaces.end());
    importerConfiguration()->setTempoDetectionWorkspaces(workspaces);
}

bool MusicPreferencesModel::askBeforeSubsequentImport() const
{
    return importerConfiguration()->subsequentImportLoopAction() == importexport::LoopAction::Ask;
}

void MusicPreferencesModel::setAskBeforeSubsequentImport(bool ask)
{
    if (askBeforeSubsequentImport() == ask) {
        return;
    }

    importerConfiguration()->setSubsequentImportLoopAction(
        ask ? importexport::LoopAction::Ask : importerConfiguration()->subsequentImportLoopAction());
}
}
