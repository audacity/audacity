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

    MenuItemList items {
        makeItemWithArg("rename-item", muse::TranslatableString("label", "Rename label")),
        makeSeparator(),
        makeItemWithArg("action://trackedit/cut"),
        makeItemWithArg("action://trackedit/copy"),
        makeItemWithArg("label-delete"),
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
