/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "workspace/iworkspacemanager.h"
#include "effects/effects_base/ieffectsconfiguration.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "trackedit/itrackeditconfiguration.h"

namespace au::appshell {
class EditPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<au::trackedit::ITrackeditConfiguration> trackeditConfiguration;
    muse::GlobalInject<au::projectscene::IProjectSceneConfiguration> projectsceneConfiguration;
    muse::GlobalInject<au::effects::IEffectsConfiguration> effectsConfiguration;

    muse::Inject<muse::workspace::IWorkspaceManager> workspacesManager;

    Q_PROPERTY(bool applyEffectToAllAudio READ applyEffectToAllAudio NOTIFY applyEffectToAllAudioChanged)
    Q_PROPERTY(
        projectscene::StereoHeightsPref::AsymmetricStereoHeights stereoHeightsPref READ stereoHeightsPref NOTIFY stereoHeightsPrefChanged)
    Q_PROPERTY(int deleteBehavior READ deleteBehavior NOTIFY deleteBehaviorPrefChanged)
    Q_PROPERTY(int closeGapBehavior READ closeGapBehavior NOTIFY closeGapBehaviorChanged)
    Q_PROPERTY(int pasteBehavior READ pasteBehavior NOTIFY pasteBehaviorPrefChanged)
    Q_PROPERTY(int pasteInsertBehavior READ pasteInsertBehavior NOTIFY pasteInsertBehaviorPrefChanged)
    Q_PROPERTY(QVariantList asymmetricWorkspaces READ asymmetricWorkspaces NOTIFY asymmetricWorkspacesChanged);
    Q_PROPERTY(bool pasteAsNewClip READ pasteAsNewClip NOTIFY pasteAsNewClipChanged)
    Q_PROPERTY(
        bool askBeforeConvertingToMonoOrStereo READ askBeforeConvertingToMonoOrStereo WRITE setAskBeforeConvertingToMonoOrStereo NOTIFY askBeforeConvertingToMonoOrStereoChanged)

public:
    explicit EditPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool applyEffectToAllAudio() const;
    Q_INVOKABLE void setApplyEffectToAllAudio(bool value);

    projectscene::StereoHeightsPref::AsymmetricStereoHeights stereoHeightsPref() const;
    Q_INVOKABLE void setStereoHeightsPref(projectscene::StereoHeightsPref::AsymmetricStereoHeights pref);

    int deleteBehavior() const;
    Q_INVOKABLE void setDeleteBehavior(int);

    int closeGapBehavior() const;
    Q_INVOKABLE void setCloseGapBehavior(int);

    int pasteBehavior() const;
    Q_INVOKABLE void setPasteBehavior(int);

    int pasteInsertBehavior() const;
    Q_INVOKABLE void setPasteInsertBehavior(int);

    QVariantList asymmetricWorkspaces() const;
    Q_INVOKABLE void appendToAsymmetricWorkspaces(const QString& newWorkspaceName);
    Q_INVOKABLE void removeFromAsymmetricWorkspaces(const QString& newWorkspaceName);

    bool pasteAsNewClip() const;
    Q_INVOKABLE void setPasteAsNewClip(bool value);

    bool askBeforeConvertingToMonoOrStereo() const;
    void setAskBeforeConvertingToMonoOrStereo(bool value);

    void asymmetricStereoHeightWorkspacesCleanUp();

signals:
    void applyEffectToAllAudioChanged();
    void stereoHeightsPrefChanged();
    void deleteBehaviorPrefChanged();
    void closeGapBehaviorChanged();
    void pasteBehaviorPrefChanged();
    void pasteInsertBehaviorPrefChanged();
    void asymmetricWorkspacesChanged();
    void pasteAsNewClipChanged();
    void askBeforeConvertingToMonoOrStereoChanged();
};
}
