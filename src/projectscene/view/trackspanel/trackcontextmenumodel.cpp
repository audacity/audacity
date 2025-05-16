/*
* Audacity: A Digital Audio Editor
*/
#include "trackcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

namespace {
//! NOTE: can be moved to the framework
bool containsAny(const ActionCodeList& list, const ActionCodeList& actionCodes)
{
    return std::any_of(actionCodes.begin(), actionCodes.end(), [&](ActionCode code) {
        return std::find(list.begin(), list.end(), code) != list.end();
    });
}
}

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
        makeMenu(muse::TranslatableString("track color", "Track color"), makeTrackColorItems(), "trackColorMenu"),
        makeSeparator(),
        makeItemWithArg("track-make-stereo"),
        makeItemWithArg("track-swap-stereo"),
        makeItemWithArg("track-split-stereo"),
    };

    setItems(items);
    updateColorCheckedState();
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

void TrackContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (codes.empty()) {
        return;
    }

    for (const ActionCode& code : codes) {
        MenuItem& actionItem = findItem(code);
        if (actionItem.isValid()) {
            actionItem.setState(uiActionsRegister()->actionState(code));
        }
    }

    if (containsAny(codes, m_colorChangeActionCodeList)) {
        updateColorCheckedState();
    }
}

void TrackContextMenuModel::updateColorCheckedState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }
    auto track = project->trackeditProject()->track(m_trackId);
    if (!track.has_value()) {
        return;
    }

    for (const auto& action : m_colorChangeActionCodeList) {
        MenuItem& item = findItem(ActionCode(action));
        ActionQuery query(action);
        track.value().color.toString();

        if (query.param("color").toString() == track.value().color.toString()) {
            auto state = item.state();
            state.checked = true;
            item.setState(state);
        } else {
            auto state = item.state();
            state.checked = false;
            item.setState(state);
        }
    }
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackColorItems()
{
    m_colorChangeActionCodeList.clear();

    MenuItemList items;
    const auto& colors = projectSceneConfiguration()->clipColors();
    for (const auto& color : colors) {
        items << makeMenuItem(makeTrackColorChangeAction(color.second).toString(),
                              muse::TranslatableString("track", muse::String::fromStdString(color.first)));
        m_colorChangeActionCodeList.push_back(makeTrackColorChangeAction(color.second).toString());
    }

    return items;
}
