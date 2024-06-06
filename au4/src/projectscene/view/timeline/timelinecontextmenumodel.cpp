/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "timelinecontextmenumodel.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;
using namespace muse::actions;

void TimelineContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeRulerItems();
    setItems(items);
}

MenuItemList TimelineContextMenuModel::makeRulerItems()
{
    MenuItemList items {
        makeMenuItem("minutes-seconds-ruler"),
        makeMenuItem("beats-measures-ruler"),
        makeSeparator(),
        makeMenuItem("update-display-while-playing"),
        makeMenuItem("pinned-play-head"),
        makeSeparator(),
        makeMenuItem("toggle-loop-region"),
        makeMenuItem("clear-loop-region"),
        makeMenuItem("set-loop-region-to-selection"),
        makeMenuItem("set-selection-to-loop"),
        makeSeparator(),
        makeMenuItem("toggle-vertical-rulers"),
    };

    return items;
}
