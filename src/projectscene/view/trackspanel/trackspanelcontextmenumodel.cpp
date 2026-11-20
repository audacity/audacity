/*
* Audacity: A Digital Audio Editor
*/
#include "trackspanelcontextmenumodel.h"

#include "projectscene/projectscenecommands.h"
#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse::uicomponents;

void TracksPanelContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList addTrackItems {
        makeMenuItem(trackedit::TRACKEDIT_NEW_MONO_TRACK_COMMAND, muse::TranslatableString("trackspanelcontextmenu", "Mono")),
        makeMenuItem(trackedit::TRACKEDIT_NEW_STEREO_TRACK_COMMAND, muse::TranslatableString("trackspanelcontextmenu", "Stereo")),
        makeMenuItem(trackedit::TRACKEDIT_NEW_LABEL_TRACK_COMMAND, muse::TranslatableString("trackspanelcontextmenu", "Label")),
    };

    setItems({
        makeMenu(muse::TranslatableString("trackspanelcontextmenu", "Add track"), addTrackItems),
        makeSeparator(),
        makeMenuItem(PROJECTSCENE_COLLAPSE_ALL_TRACKS_COMMAND),
        makeMenuItem(PROJECTSCENE_EXPAND_ALL_TRACKS_COMMAND),
        makeMenuItem(PROJECTSCENE_FIT_ALL_TRACKS_TO_HEIGHT_COMMAND,
                     muse::TranslatableString("trackspanelcontextmenu", "Fit tracks to height")),
        makeSeparator(),
        makeMenuItem(PROJECTSCENE_RESET_ALL_TRACK_HEIGHTS_COMMAND),
    });
}
