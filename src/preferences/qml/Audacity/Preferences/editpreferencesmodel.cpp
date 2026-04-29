/*
 * Audacity: A Digital Audio Editor
 */
#include "editpreferencesmodel.h"

#include "settings.h"
#include "translation.h"

namespace au::appshell {
EditPreferencesModel::EditPreferencesModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
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

    trackeditConfiguration()->deleteBehaviorChanged().onNotify(this, [this] {
        emit deleteBehaviorPrefChanged();
    });

    trackeditConfiguration()->closeGapBehaviorChanged().onNotify(this, [this] {
        emit closeGapBehaviorChanged();
    });

    trackeditConfiguration()->pasteBehaviorChanged().onNotify(this, [this] {
        emit pasteBehaviorPrefChanged();
    });

    trackeditConfiguration()->pasteInsertBehaviorChanged().onNotify(this, [this] {
        emit pasteInsertBehaviorPrefChanged();
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

int EditPreferencesModel::deleteBehavior() const
{
    return static_cast<int>(trackeditConfiguration()->deleteBehavior());
}

void EditPreferencesModel::setDeleteBehavior(int pref)
{
    if (deleteBehavior() == pref) {
        return;
    }
    trackeditConfiguration()->setDeleteBehavior(static_cast<au::trackedit::DeleteBehavior>(pref));
}

int EditPreferencesModel::closeGapBehavior() const
{
    return static_cast<int>(trackeditConfiguration()->closeGapBehavior());
}

void EditPreferencesModel::setCloseGapBehavior(int pref)
{
    if (closeGapBehavior() == pref) {
        return;
    }
    trackeditConfiguration()->setCloseGapBehavior(static_cast<au::trackedit::CloseGapBehavior>(pref));
}

int EditPreferencesModel::pasteBehavior() const
{
    return static_cast<int>(trackeditConfiguration()->pasteBehavior());
}

void EditPreferencesModel::setPasteBehavior(int pref)
{
    if (pasteBehavior() == pref) {
        return;
    }
    trackeditConfiguration()->setPasteBehavior(static_cast<au::trackedit::PasteBehavior>(pref));
}

int EditPreferencesModel::pasteInsertBehavior() const
{
    return static_cast<int>(trackeditConfiguration()->pasteInsertBehavior());
}

void EditPreferencesModel::setPasteInsertBehavior(int pref)
{
    if (pasteInsertBehavior() == pref) {
        return;
    }
    trackeditConfiguration()->setPasteInsertBehavior(static_cast<au::trackedit::PasteInsertBehavior>(pref));
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

int EditPreferencesModel::zoomPreset1() const
{
    return static_cast<int>(projectsceneConfiguration()->zoomPreset1());
}

void EditPreferencesModel::setZoomPreset1(int preset)
{
    if (zoomPreset1() == preset) {
        return;
    }
    projectsceneConfiguration()->setZoomPreset1(static_cast<projectscene::ZoomPresets::Preset>(preset));
    emit zoomPreset1Changed();
}

int EditPreferencesModel::zoomPreset2() const
{
    return static_cast<int>(projectsceneConfiguration()->zoomPreset2());
}

void EditPreferencesModel::setZoomPreset2(int preset)
{
    if (zoomPreset2() == preset) {
        return;
    }
    projectsceneConfiguration()->setZoomPreset2(static_cast<projectscene::ZoomPresets::Preset>(preset));
    emit zoomPreset2Changed();
}

QVariantList EditPreferencesModel::zoomPresetList() const
{
    QVariantList list;
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Fit to width") }, { "value", 0 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Zoom to selection") }, { "value", 1 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Zoom default") }, { "value", 2 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Minutes") }, { "value", 3 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Seconds") }, { "value", 4 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "5ths of seconds") }, { "value", 5 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "10ths of seconds") }, { "value", 6 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "20ths of seconds") }, { "value", 7 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "50ths of seconds") }, { "value", 8 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "100ths of seconds") }, { "value", 9 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "500ths of seconds") }, { "value", 10 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Milliseconds") }, { "value", 11 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Samples") }, { "value", 12 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "4 pixels per sample") }, { "value", 13 } };
    list << QVariantMap{ { "title", muse::qtrc("appshell/preferences", "Max zoom") }, { "value", 14 } };
    return list;
}
}
