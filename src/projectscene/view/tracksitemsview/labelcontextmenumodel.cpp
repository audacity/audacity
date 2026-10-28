/*
* Audacity: A Digital Audio Editor
*/
#include "labelcontextmenumodel.h"

#include "translation.h"

#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse::uicomponents;

void LabelContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList cutAndItems {
        makeMenuItem(trackedit::TRACKEDIT_CUT_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("label", "Cut and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("label", "Cut and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("label", "Cut and close gap on all tracks")),
    };

    MenuItemList pasteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_PASTE_OVERLAP_COMMAND,
                     muse::TranslatableString("label", "Paste and overlap")),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_COMMAND,
                     muse::TranslatableString("label", "Paste and make room on this track")),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("label", "Paste and make room on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_DELETE_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("label", "Delete and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("label", "Delete and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("label", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeMenuItem(trackedit::TRACKEDIT_RENAME_ITEM_COMMAND, muse::TranslatableString("label", "Rename label")),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_CUT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_COPY_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("label", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("label", "Paste and…"), pasteAndItems, "menu-paste-and"),
        makeMenu(muse::TranslatableString("label", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeMenuItem("group-items"),
        makeMenuItem("ungroup-items"),
    };

    setItems(items);
}

void LabelContextMenuModel::handleMenuItem(const QString& itemId)
{
    AbstractMenuModel::handleMenuItem(itemId);
}

LabelKey LabelContextMenuModel::labelKey() const
{
    return m_labelKey;
}

void LabelContextMenuModel::setLabelKey(const LabelKey& key)
{
    if (m_labelKey == key) {
        return;
    }

    m_labelKey = key;
    emit labelKeyChanged();
}
