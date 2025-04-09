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
    effectsConfiguration()->applyEffectToAllAudioChanged().onNotify(this, [this]{
        emit applyEffectToAllAudioChanged();
    });

    projectsceneConfiguration()->stereoHeightsPrefChanged().onNotify(this, [this] {
        emit stereoHeightsPrefChanged();
    });

    projectsceneConfiguration()->asymmetricStereoHeightsWorkspacesChanged().onNotify(this, [this] {
        emit asymmetricWorkspacesChanged();
    });

    trackeditConfiguration()->pasteAsNewClipChanged().onNotify(this, [this] {
        emit pasteAsNewClipChanged();
    });

    trackeditConfiguration()->askBeforeConvertingToMonoOrStereoChanged().onNotify(this, [this]{
        emit askBeforeConvertingToMonoOrStereoChanged();
    });

    workspacesManager()->workspacesListChanged().onNotify(this, [this]() {
        asymmetricStereoHeightWorkspacesCleanUp();
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

void EditPreferencesModel::asymmetricStereoHeightWorkspacesCleanUp()
{
    // cleanup after user removes workspace
    std::vector<std::string> asymmetricWorkspaces = projectsceneConfiguration()->asymmetricStereoHeightsWorkspaces();
    auto workspaces = workspacesManager()->workspaces();

    // gather workspace names into a set for quick lookup
    std::unordered_set<std::string> workspaceNames;
    for (const auto& workspace : workspaces) {
        workspaceNames.insert(workspace->name());
    }

    // remove asymmetric workspaces that are not in the set
    asymmetricWorkspaces.erase(
        std::remove_if(asymmetricWorkspaces.begin(), asymmetricWorkspaces.end(),
                       [&](const auto& asymmetricWorkspace) {
        return workspaceNames.find(asymmetricWorkspace) == workspaceNames.end();
    }),
        asymmetricWorkspaces.end());

    projectsceneConfiguration()->setAsymmetricStereoHeightsWorkspaces(asymmetricWorkspaces);
}

bool EditPreferencesModel::applyEffectToAllAudio() const
{
    return effectsConfiguration()->applyEffectToAllAudio();
}

void EditPreferencesModel::setApplyEffectToAllAudio(bool value)
{
    if (applyEffectToAllAudio() == value) {
        return;
    }
    effectsConfiguration()->setApplyEffectToAllAudio(value);
}

projectscene::StereoHeightsPref::AsymmetricStereoHeights
EditPreferencesModel::stereoHeightsPref() const
{
    return projectsceneConfiguration()->stereoHeightsPref();
}

void EditPreferencesModel::setStereoHeightsPref(
    projectscene::StereoHeightsPref::AsymmetricStereoHeights pref)
{
    if (stereoHeightsPref() == pref) {
        return;
    }
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

bool EditPreferencesModel::pasteAsNewClip() const
{
    return trackeditConfiguration()->pasteAsNewClip();
}

void EditPreferencesModel::setPasteAsNewClip(bool value)
{
    if (pasteAsNewClip() == value) {
        return;
    }
    trackeditConfiguration()->setPasteAsNewClip(value);
}
}
