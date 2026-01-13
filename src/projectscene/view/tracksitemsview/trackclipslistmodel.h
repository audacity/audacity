/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"
#include "workspace/iworkspacemanager.h"
#include "iprojectsceneconfiguration.h"

#include "trackitemslistmodel.h"
#include "trackclipitem.h"

namespace au::projectscene {
class TrackClipsListModel : public TrackItemsListModel
{
    Q_OBJECT

    Q_PROPERTY(bool isStereo READ isStereo NOTIFY isStereoChanged FINAL)
    Q_PROPERTY(ClipStyles::Style clipStyle READ clipStyle NOTIFY clipStyleChanged FINAL)
    Q_PROPERTY(
        bool asymmetricStereoHeightsPossible READ asymmetricStereoHeightsPossible NOTIFY asymmetricStereoHeightsPossibleChanged)
    Q_PROPERTY(bool isContrastFocusBorderEnabled READ isContrastFocusBorderEnabled NOTIFY isContrastFocusBorderEnabledChanged FINAL)

    muse::GlobalInject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;

    muse::Inject<muse::workspace::IWorkspaceManager> workspacesManager;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    explicit TrackClipsListModel(QObject* parent = nullptr);

    bool isStereo() const;
    ClipStyles::Style clipStyle() const;

    Q_INVOKABLE bool moveSelectedClips(const ClipKey& key, bool completed);
    Q_INVOKABLE bool trimLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);
    Q_INVOKABLE bool trimRightClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);
    Q_INVOKABLE bool stretchLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);
    Q_INVOKABLE bool stretchRightClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);

    Q_INVOKABLE void selectClip(const ClipKey& key);
    Q_INVOKABLE void resetSelectedClips();
    Q_INVOKABLE bool changeClipTitle(const ClipKey& key, const QString& newTitle);

    Q_INVOKABLE void openClipPitchEdit(const ClipKey& key);
    Q_INVOKABLE void resetClipPitch(const ClipKey& key);

    Q_INVOKABLE void openClipSpeedEdit(const ClipKey& key);
    Q_INVOKABLE void resetClipSpeed(const ClipKey& key);

    // update clip after moving to other track
    Q_INVOKABLE projectscene::ClipKey updateClipTrack(ClipKey clipKey) const;

    bool asymmetricStereoHeightsPossible() const;
    bool isContrastFocusBorderEnabled() const;

signals:
    void selectedClipIdxChanged();
    void isStereoChanged();
    void clipStyleChanged();
    void asymmetricStereoHeightsPossibleChanged();
    void isContrastFocusBorderEnabledChanged();

    void requestClipTitleEdit(int index);

    void contentXChanged();

private:
    void onInit() override;
    void onReload() override;

    void update();
    void updateItemMetrics(ViewTrackItem* item) override;
    trackedit::TrackItemKeyList getSelectedItemKeys() const override;

    TrackClipItem* clipItemByKey(const trackedit::ClipKey& k) const;

    bool isKeyboardTriggered() const;

    muse::async::NotifyList<au::trackedit::Clip> m_allClipList;
    ClipStyles::Style m_clipStyle = ClipStyles::Style::COLORFUL;
    bool m_isStereo = false;
};
}
