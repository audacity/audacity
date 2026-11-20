/*
* Audacity: A Digital Audio Editor
*/
#include "trackspanelcontextmenumodel.h"

#include "projectscene/projectscenecommands.h"

using namespace au::projectscene;
using namespace muse::uicomponents;

void TracksPanelContextMenuModel::load()
{
    AbstractMenuModel::load();

    setItems({
        makeMenuItem(PROJECTSCENE_COLLAPSE_ALL_TRACKS_COMMAND),
        makeMenuItem(PROJECTSCENE_EXPAND_ALL_TRACKS_COMMAND),
        makeMenuItem(PROJECTSCENE_FIT_ALL_TRACKS_TO_HEIGHT_COMMAND,
                     muse::TranslatableString("trackspanelcontextmenu", "Fit tracks to height")),
        makeSeparator(),
        makeMenuItem(PROJECTSCENE_RESET_ALL_TRACK_HEIGHTS_COMMAND),
    });
}
