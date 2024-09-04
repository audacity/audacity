/*
* Audacity: A Digital Audio Editor
*/
#include "clipcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

void ClipContextMenuModel::load()
{
    AbstractMenuModel::load();

    auto makeItemWithArg = [this](const ActionCode& actionCode) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<trackedit::ClipKey>(m_clipKey.key));
        return item;
    };

    MenuItemList items {
        makeItemWithArg("clip-properties"),
        makeItemWithArg("clip-rename"),
        makeSeparator(),
        makeItemWithArg("clip-cut"),
        makeItemWithArg("clip-copy"),
        makeItemWithArg("clip-delete"),
        makeSeparator(),
        makeItemWithArg("clip-cut-close-gap"),
        makeItemWithArg("clip-delete-close-gap"),
        makeItemWithArg("clip-duplicate"),
        makeSeparator(),
        makeItemWithArg("track-split"),
        makeSeparator(),
        makeItemWithArg("clip-export"),
        makeSeparator(),
        makeItemWithArg("clip-enable-stretching"),
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
