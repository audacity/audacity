#include "effectmanagemenu.h"

#include "effects/effects_base/effectstypes.h"
#include "au3wrap/internal/wxtypes_convert.h"

using namespace muse;
using namespace muse::uicomponents;
using namespace muse::actions;
using namespace au::effects;

void EffectManageMenu::load()
{
    AbstractMenuModel::load();

    EffectId effectId = EffectId::fromQString(m_effectId);

    auto makeItemWithEffectArg = [this, effectId](const ActionCode& actionCode) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<EffectId>(effectId));
        return item;
    };

    auto makeMenuWithPresets = [this, effectId](const TranslatableString& title,
                                                const PresetIdList& presets,
                                                const ActionCode& actionCode)
    {
        MenuItem* menuItem = makeMenu(title, {});
        if (presets.empty()) {
            menuItem->setState(ui::UiActionState::make_disabled());
        } else {
            MenuItemList subitems;
            for (const PresetId& p : presets) {
                String name = au3::wxToString(p);
                MenuItem* item = makeMenuItem(actionCode, TranslatableString::untranslatable(name));
                item->setId(name);
                item->setArgs(ActionData::make_arg2<EffectId, PresetId>(effectId, p));
                subitems << item;
            }
            menuItem->setSubitems(subitems);
        }
        return menuItem;
    };

    MenuItemList items;

    // user
    PresetIdList userPresets = presetsController()->userPresets(effectId);
    items << makeMenuWithPresets(TranslatableString("effects", "User Presets"), userPresets, "action://effects/presets/apply");
    items << makeItemWithEffectArg("action://effects/presets/save");
    items << makeMenuWithPresets(TranslatableString("effects", "Delete Presets"), userPresets, "action://effects/presets/delete");

    items << makeSeparator();

    // factory
    PresetIdList factoryPresets = presetsController()->factoryPresets(effectId);
    items << makeMenuWithPresets(TranslatableString("effects", "Factory Presets"), factoryPresets, "action://effects/presets/apply");

    items << makeSeparator();

    // import / export
    items << makeItemWithEffectArg("action://effects/presets/import");
    items << makeItemWithEffectArg("action://effects/presets/export");

    setItems(items);
}

QString EffectManageMenu::effectId_prop() const
{
    return m_effectId;
}

void EffectManageMenu::setEffectId_prop(const QString& newEffectId)
{
    if (m_effectId == newEffectId) {
        return;
    }
    m_effectId = newEffectId;
    emit effectIdChanged();
}
