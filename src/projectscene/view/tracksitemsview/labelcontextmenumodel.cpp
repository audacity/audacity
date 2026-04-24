/*
* Audacity: A Digital Audio Editor
*/
#include "labelcontextmenumodel.h"

#include "translation.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

void LabelContextMenuModel::load()
{
    AbstractMenuModel::load();

    auto makeItemWithArg = [this](const ActionCode& actionCode, const muse::TranslatableString& title = {}) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<trackedit::LabelKey>(m_labelKey.key));
        if (!title.isEmpty()) {
            item->setTitle(title);
        }
        return item;
    };

    MenuItemList cutAndItems {
        makeMenuItem("cut-leave-gap",
                     muse::TranslatableString("label", "Cut and leave gap")),
        makeMenuItem("cut-per-track-ripple",
                     muse::TranslatableString("label", "Cut and close gap on this track")),
        makeMenuItem("cut-all-tracks-ripple",
                     muse::TranslatableString("label", "Cut and close gap on all tracks")),
    };

    MenuItemList pasteAndItems {
        makeMenuItem("action://trackedit/paste-overlap",
                     muse::TranslatableString("label", "Paste and overlap")),
        makeMenuItem("action://trackedit/paste-insert",
                     muse::TranslatableString("label", "Paste and make room on this track")),
        makeMenuItem("action://trackedit/paste-insert-all-tracks-ripple",
                     muse::TranslatableString("label", "Paste and make room on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem("delete-leave-gap",
                     muse::TranslatableString("label", "Delete and leave gap")),
        makeMenuItem("delete-per-track-ripple",
                     muse::TranslatableString("label", "Delete and close gap on this track")),
        makeMenuItem("delete-all-tracks-ripple",
                     muse::TranslatableString("label", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeItemWithArg("rename-item", muse::TranslatableString("label", "Rename label")),
        makeSeparator(),
        makeItemWithArg("action://trackedit/cut"),
        makeItemWithArg("action://trackedit/copy"),
        makeItemWithArg("action://trackedit/delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("label", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("label", "Paste and…"), pasteAndItems, "menu-paste-and"),
        makeMenu(muse::TranslatableString("label", "Delete and…"), deleteAndItems, "menu-delete-and"),
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
