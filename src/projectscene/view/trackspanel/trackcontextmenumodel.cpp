/*
* Audacity: A Digital Audio Editor
*/
#include "trackcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

void TrackContextMenuModel::load()
{
    AbstractMenuModel::load();

    auto makeItemWithArg = [this](const ActionCode& actionCode) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<trackedit::TrackId>(m_trackId));
        return item;
    };

    MenuItemList moveTrackItems {
        makeItemWithArg("track-move-up"),
        makeItemWithArg("track-move-down"),
        makeItemWithArg("track-move-top"),
        makeItemWithArg("track-move-bottom"),
    };

    MenuItemList items {
        makeItemWithArg("track-rename"),
        makeItemWithArg("track-duplicate"),
        makeItemWithArg("track-delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("track menu", "Move track"), moveTrackItems),
        makeSeparator(),
        makeItemWithArg("track-make-stereo"),
        makeItemWithArg("track-swap-stereo"),
        makeItemWithArg("track-split-stereo"),
    };

    setItems(items);
}

au::trackedit::TrackId TrackContextMenuModel::trackId() const
{
    return m_trackId;
}

void TrackContextMenuModel::setTrackId(const trackedit::TrackId& newTrackId)
{
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

void TrackContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == "track-rename") {
        emit trackRenameRequested();
    } else {
        AbstractMenuModel::handleMenuItem(itemId);
    }
}
