/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "timelinecontextmenumodel.h"

#include "playback/playbackcommands.h"
#include "projectscene/projectscenecommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;

void TimelineContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeRulerItems();
    setItems(items);
}

MenuItemList TimelineContextMenuModel::makeRulerItems()
{
    MenuItemList items {
        makeMenuItem(PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND),
        makeMenuItem(PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND),
        makeSeparator(),
        makeMenuItem(PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND),
        makeMenuItem(PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND),
        makeMenuItem(PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND),
        makeSeparator(),
        makeMenuItem(playback::PLAYBACK_TOGGLE_LOOP_REGION_COMMAND),
        makeMenuItem(playback::PLAYBACK_CLEAR_LOOP_REGION_COMMAND),
        makeMenuItem(playback::PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND),
        makeMenuItem(playback::PLAYBACK_SET_SELECTION_TO_LOOP_COMMAND),
        makeMenuItem(playback::PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND),
        makeSeparator(),
        makeMenuItem(PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND),
    };

    return items;
}
