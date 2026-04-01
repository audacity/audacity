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
    importerConfiguration()->tempoDetectionWorkspacesChanged().onNotify(this, [this] {
        emit tempoDetectionEnabledChanged();
    });

    workspacesManager()->currentWorkspaceChanged().onNotify(this, [this] {
        emit tempoDetectionEnabledChanged();
    });
}

bool MusicPreferencesModel::tempoDetectionEnabled() const
{
    auto currentWorkspace = workspacesManager()->currentWorkspace();
    if (!currentWorkspace) {
        return false;
    }

    const std::string wsName = currentWorkspace->name();
    const auto workspaces = importerConfiguration()->tempoDetectionWorkspaces();
    return std::find(workspaces.begin(), workspaces.end(), wsName) != workspaces.end();
}

void MusicPreferencesModel::setTempoDetectionEnabled(bool enabled)
{
    auto currentWorkspace = workspacesManager()->currentWorkspace();
    if (!currentWorkspace) {
        return;
    }

    const std::string wsName = currentWorkspace->name();
    auto workspaces = importerConfiguration()->tempoDetectionWorkspaces();
    auto it = std::find(workspaces.begin(), workspaces.end(), wsName);

    if (enabled && it == workspaces.end()) {
        workspaces.push_back(wsName);
    } else if (!enabled && it != workspaces.end()) {
        workspaces.erase(it);
    } else {
        return;
    }

    importerConfiguration()->setTempoDetectionWorkspaces(workspaces);
}
}
