/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "types/projectscenetypes.h"
#include "iprojectsceneconfiguration.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/iselectioncontroller.h"
#include "playback/iaudiodevicesprovider.h"

namespace au::projectscene {
class TrackContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<trackedit::ISelectionController> selectionController;

    Q_PROPERTY(trackedit::TrackId trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

public:
    TrackContextMenuModel() = default;

    Q_INVOKABLE void load() override;

    trackedit::TrackId trackId() const;
    void setTrackId(const trackedit::TrackId& newTrackId);
    void handleMenuItem(const QString& itemId) override;

signals:
    void trackIdChanged();
    void trackRenameRequested();

private:
    void onActionsStateChanges(const muse::actions::ActionCodeList& codes) override;

    muse::uicomponents::MenuItemList makeMonoTrackItems();
    muse::uicomponents::MenuItemList makeStereoTrackItems();

    muse::uicomponents::MenuItemList makeTrackColorItems();
    muse::uicomponents::MenuItemList makeTrackFormatItems();
    muse::uicomponents::MenuItemList makeTrackRateItems();
    muse::uicomponents::MenuItemList makeTrackMoveItems();
    muse::uicomponents::MenuItemList makeTrackViewItems();
    muse::uicomponents::MenuItemList makeTrackRulerItems();

    muse::uicomponents::MenuItem* makeItemWithArg(const muse::actions::ActionCode& actionCode);

    void updateColorCheckedState();
    void updateTrackFormatState();
    void updateTrackRateState();
    void updateTrackMonoState();

    trackedit::TrackId m_trackId;
    muse::actions::ActionCodeList m_colorChangeActionCodeList;
};
}
