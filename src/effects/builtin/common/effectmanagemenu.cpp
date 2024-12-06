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

    const EffectInstanceId instanceId = m_instanceId.toULongLong();
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);

    // subscribe on user presets change
    presetsController()->userPresetsChanged().onReceive(this, [this, effectId, instanceId](const EffectId& eid) {
        if (effectId != eid) {
            return;
        }

        reload(effectId, instanceId);
    });

    reload(effectId, instanceId);
}

void EffectManageMenu::reload(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    MenuItemList items;

    // user
    PresetIdList userPresets = presetsController()->userPresets(effectId);
    {
        MenuItem* menuItem = makeMenu(TranslatableString("effects", "User Presets"), {});
        if (userPresets.empty()) {
            menuItem->setState(ui::UiActionState::make_disabled());
        } else {
            MenuItemList subitems;
            for (const PresetId& p : userPresets) {
                String name = au3::wxToString(p);
                MenuItem* item = makeMenuItem("action://effects/presets/apply", TranslatableString::untranslatable(name));
                item->setId("user_apply_" + name);
                item->setArgs(ActionData::make_arg2<EffectInstanceId, PresetId>(instanceId, p)); // apply for instance
                subitems << item;
            }
            menuItem->setSubitems(subitems);
        }
        items << menuItem;
    }

    {
        MenuItem* item = makeMenuItem("action://effects/presets/save");
        item->setArgs(ActionData::make_arg1<EffectInstanceId>(instanceId));
        items << item;  // apply from instance
    }

    {
        MenuItem* menuItem = makeMenu(TranslatableString("effects", "Delete Presets"), {});
        if (userPresets.empty()) {
            menuItem->setState(ui::UiActionState::make_disabled());
        } else {
            MenuItemList subitems;
            for (const PresetId& p : userPresets) {
                String name = au3::wxToString(p);
                MenuItem* item = makeMenuItem("action://effects/presets/delete", TranslatableString::untranslatable(name));
                item->setId("user_delete_" + name);
                item->setArgs(ActionData::make_arg2<EffectId, PresetId>(effectId, p)); // delete for effect
                subitems << item;
            }
            menuItem->setSubitems(subitems);
        }
        items << menuItem;
    }

    items << makeSeparator();

    // factory
    {
        PresetIdList factoryPresets = presetsController()->factoryPresets(effectId);
        MenuItem* menuItem = makeMenu(TranslatableString("effects", "Factory Presets"), {});

        MenuItemList subitems;
        MenuItem* defItem = makeMenuItem("action://effects/presets/apply", TranslatableString("effects", "Defaults"));
        defItem->setId("factory_apply_default");
        defItem->setArgs(ActionData::make_arg2<EffectInstanceId, PresetId>(instanceId, "default"));
        subitems << defItem;

        for (const PresetId& p : factoryPresets) {
            String name = au3::wxToString(p);
            MenuItem* item = makeMenuItem("action://effects/presets/apply", TranslatableString::untranslatable(name));
            item->setId("factory_apply_" + name);
            item->setArgs(ActionData::make_arg2<EffectInstanceId, PresetId>(instanceId, p));     // apply for instance
            subitems << item;
        }
        menuItem->setSubitems(subitems);

        items << menuItem;
    }

    items << makeSeparator();

    // import / export
    {
        MenuItem* item = makeMenuItem("action://effects/presets/import");
        item->setArgs(ActionData::make_arg1<EffectInstanceId>(instanceId));
        items << item;
    }

    {
        MenuItem* item = makeMenuItem("action://effects/presets/export");
        item->setArgs(ActionData::make_arg1<EffectInstanceId>(instanceId));
        items << item;
    }

    setItems(items);
}

QString EffectManageMenu::instanceId_prop() const
{
    return m_instanceId;
}

void EffectManageMenu::setInstanceId_prop(const QString& newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}
