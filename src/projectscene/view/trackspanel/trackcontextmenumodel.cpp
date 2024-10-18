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

    MenuItem* rename = new MenuItem(this);
    rename->setTitle(muse::TranslatableString("Rename track", "Rename"));
    rename->setId("track-rename");
    rename->setState({ true });

    MenuItemList items {
        makeItemWithArg("track-duplicate"),
        makeItemWithArg("track-delete"),
        makeSeparator(),
        makeItemWithArg("track-make-stereo"),
        makeItemWithArg("track-swap-stereo"),
        makeItemWithArg("track-split-stereo"),
    };

    items.prepend(rename);
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
