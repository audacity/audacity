/*
* Audacity: A Digital Audio Editor
*/
#include "clipcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

static const ActionCode ENABLE_STRETCH_CODE("stretch-clip-to-match-tempo");

void ClipContextMenuModel::load()
{
    AbstractMenuModel::load();

    auto makeItemWithArg = [this](const ActionCode& actionCode) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<trackedit::ClipKey>(m_clipKey.key));
        return item;
    };

    auto enableStretchItem = makeItemWithArg(ENABLE_STRETCH_CODE);
    updateStretchEnabledState(*enableStretchItem);

    MenuItemList items {
        makeItemWithArg("clip-properties"),
        makeItemWithArg("clip-rename"),
        makeSeparator(),
        makeItemWithArg("clip-cut"),
        makeItemWithArg("clip-copy"),
        makeItemWithArg("clip-delete"),
        makeSeparator(),
        makeItemWithArg("duplicate"),
        makeItemWithArg("clip-split-cut"),
        makeItemWithArg("clip-split-delete"),
        makeSeparator(),
        makeItemWithArg("track-split"),
        makeSeparator(),
        makeItemWithArg("clip-export"),
        makeSeparator(),
        enableStretchItem,
        makeItemWithArg("clip-pitch-speed"),
        makeItemWithArg("clip-render-pitch-speed"),
    };

    setItems(items);
}

ClipKey ClipContextMenuModel::clipKey() const
{
    return m_clipKey;
}

void ClipContextMenuModel::setClipKey(const ClipKey& newClipKey)
{
    m_clipKey = newClipKey;
    emit clipKeyChanged();
}

void ClipContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    AbstractMenuModel::onActionsStateChanges(codes);

    if (containsAction(codes, ENABLE_STRETCH_CODE)) {
        MenuItem& item = findItem(ActionCode(ENABLE_STRETCH_CODE));
        updateStretchEnabledState(item);
    }
}

void ClipContextMenuModel::updateStretchEnabledState(MenuItem& item)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }
    auto clip = project->trackeditProject()->clip(m_clipKey.key);
    if (!clip.isValid()) {
        return;
    }
    auto state = item.state();
    state.checked = clip.stretchToMatchTempo;
    item.setState(state);
}
