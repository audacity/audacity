/*
 * Audacity: A Digital Audio Editor
 */
#include "editpreferencesmodel.h"

#include "settings.h"

namespace au::appshell {
EditPreferencesModel::EditPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void EditPreferencesModel::init()
{
    trackeditConfiguration()->applyEffectToAllAudioChanged().onNotify(this, [this]{
        emit applyEffectToAllAudioChanged();
    });

    projectsceneConfiguration()->stereoHeightsPrefChanged().onNotify(this, [this] {
        emit stereoHeightsPrefChanged();
        emit asymmetricStereoHeightsPossibleChanged();
    });

    projectsceneConfiguration()->asymmetricStereoHeightsWorkspacesChanged().onNotify(this, [this] {
        emit asymmetricWorkspacesChanged();
        emit asymmetricStereoHeightsPossibleChanged();
    });

    trackeditConfiguration()->pasteAsNewClipChanged().onNotify(this, [this] {
        emit pasteAsNewClipChanged();
    });

    trackeditConfiguration()->askBeforeConvertingToMonoOrStereoChanged().onNotify(this, [this]{
        emit askBeforeConvertingToMonoOrStereoChanged();
    });

    workspacesManager()->workspacesListChanged().onNotify(this, [this]() {
        projectsceneConfiguration()->asymmetricStereoHeightWorkspacesCleanUp();
    });

    workspacesManager()->currentWorkspaceChanged().onNotify(this, [this]() {
        emit asymmetricStereoHeightsPossibleChanged();
    });
}

bool EditPreferencesModel::askBeforeConvertingToMonoOrStereo() const
{
    return trackeditConfiguration()->askBeforeConvertingToMonoOrStereo();
}

void EditPreferencesModel::setAskBeforeConvertingToMonoOrStereo(bool value)
{
    trackeditConfiguration()->setAskBeforeConvertingToMonoOrStereo(value);
}

bool EditPreferencesModel::applyEffectToAllAudio() const
{
    return trackeditConfiguration()->applyEffectToAllAudio();
}

void EditPreferencesModel::setApplyEffectToAllAudio(bool value)
{
    trackeditConfiguration()->setApplyEffectToAllAudio(value);
}

projectscene::StereoHeightsPref::AsymmetricStereoHeights
EditPreferencesModel::stereoHeightsPref() const
{
    return projectsceneConfiguration()->stereoHeightsPref();
}

void EditPreferencesModel::setStereoHeightsPref(
    projectscene::StereoHeightsPref::AsymmetricStereoHeights pref)
{
    projectsceneConfiguration()->setStereoHeightsPref(pref);
}

QVariantList EditPreferencesModel::asymmetricWorkspaces() const
{
    QVariantList result;
    for (const auto& workspaceName : projectsceneConfiguration()->asymmetricStereoHeightsWorkspaces()) {
        result << QString::fromStdString(workspaceName);
    }

    return result;
}

void EditPreferencesModel::appendToAsymmetricWorkspaces(const QString& newWorkspaceName)
{
    auto workspaces = projectsceneConfiguration()->asymmetricStereoHeightsWorkspaces();
    for (const auto& workspaceName : workspaces) {
        if (workspaceName == newWorkspaceName.toStdString()) {
            return;
        }
    }

    workspaces.push_back(newWorkspaceName.toStdString());
    projectsceneConfiguration()->setAsymmetricStereoHeightsWorkspaces(workspaces);
}

void EditPreferencesModel::removeFromAsymmetricWorkspaces(const QString& workspaceName)
{
    auto workspaces = projectsceneConfiguration()->asymmetricStereoHeightsWorkspaces();
    workspaces.erase(std::remove(workspaces.begin(), workspaces.end(), workspaceName.toStdString()), workspaces.end());

    projectsceneConfiguration()->setAsymmetricStereoHeightsWorkspaces(workspaces);
}

bool EditPreferencesModel::asymmetricStereoHeightsPossible() const
{
    if (projectsceneConfiguration()->stereoHeightsPref()
        == projectscene::StereoHeightsPref::AsymmetricStereoHeights::ALWAYS) {
        return true;
    } else if (projectsceneConfiguration()->stereoHeightsPref()
               == projectscene::StereoHeightsPref::AsymmetricStereoHeights::WORKSPACE_DEPENDENT) {
        std::string currentWorkspace = workspacesManager()->currentWorkspace()->name();
        if (muse::contains(projectsceneConfiguration()->asymmetricStereoHeightsWorkspaces(), currentWorkspace)) {
            return true;
        }
    }

    return false;
}

bool EditPreferencesModel::pasteAsNewClip() const
{
    return trackeditConfiguration()->pasteAsNewClip();
}

void EditPreferencesModel::setPasteAsNewClip(bool value)
{
    trackeditConfiguration()->setPasteAsNewClip(value);
}
}
