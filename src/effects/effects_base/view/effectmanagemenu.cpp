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

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);

    // Subscribe on user presets change (replace subscription each time with new effectId)
    presetsController()->userPresetsChanged().onReceive(this, [this, effectId](const EffectId& eid) {
        if (effectId != eid) {
            return;
        }

        reload(effectId, m_instanceId);
    }, muse::async::Asyncable::Mode::SetReplace);

    reload(effectId, m_instanceId);
}

void EffectManageMenu::reload(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    assert(!effectId.empty());
    assert(instanceId != 0);

    MenuItemList items;
    QVariantList presets;

    presets << QVariantMap {
        { "id", "default" },
        { "name", muse::qtrc("effects", "Default preset") } };

    auto makeApplyAction = [](const EffectInstanceId& iid, const PresetId& p) {
        ActionQuery q("action://effects/presets/apply");
        q.addParam("instanceId", Val(iid));
        q.addParam("presetId", Val(au3::wxToStdSting(p)));
        return q;
    };

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
                MenuItem* item = makeMenuItem(makeApplyAction(instanceId, p).toString(), TranslatableString::untranslatable(name));
                item->setId("user_apply_" + name);
                subitems << item;

                presets << QVariantMap {
                    { "id", name.toQString() },
                    { "name", name.toQString() } };
            }
            menuItem->setSubitems(subitems);
        }
        items << menuItem;
    }

    {
        ActionQuery q("action://effects/presets/save");
        q.addParam("instanceId", Val(instanceId));
        MenuItem* item = makeMenuItem(q.toString());
        items << item;
    }

    {
        MenuItem* menuItem = makeMenu(TranslatableString("effects", "Delete Presets"), {});
        if (userPresets.empty()) {
            menuItem->setState(ui::UiActionState::make_disabled());
        } else {
            MenuItemList subitems;
            for (const PresetId& p : userPresets) {
                String name = au3::wxToString(p);
                ActionQuery q("action://effects/presets/delete");
                q.addParam("effectId", Val(effectId.toStdString()));
                q.addParam("presetId", Val(au3::wxToStdSting(p)));
                MenuItem* item = makeMenuItem(q.toString(), TranslatableString::untranslatable(name));
                item->setId("user_delete_" + name);
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
        MenuItem* defItem = makeMenuItem(makeApplyAction(instanceId, "default").toString(), TranslatableString("effects", "Defaults"));
        defItem->setId("factory_apply_default");
        subitems << defItem;

        for (const PresetId& p : factoryPresets) {
            String name = au3::wxToString(p);
            MenuItem* item = makeMenuItem(makeApplyAction(instanceId, p).toString(), TranslatableString::untranslatable(name));
            QString id = "factory_apply_" + name;
            item->setId(id);
            subitems << item;

            presets << QVariantMap {
                { "id", name.toQString() },
                { "name", name.toQString() } };
        }
        menuItem->setSubitems(subitems);

        items << menuItem;
    }

    items << makeSeparator();

    // import / export
    {
        ActionQuery q("action://effects/presets/import");
        q.addParam("instanceId", Val(instanceId));
        MenuItem* item = makeMenuItem(q.toString());
        items << item;
    }

    {
        ActionQuery q("action://effects/presets/export");
        q.addParam("instanceId", Val(instanceId));
        MenuItem* item = makeMenuItem(q.toString());
        items << item;
    }

    // UI Mode toggle - only for external plugins (VST3, LV2, Audio Units...)
    // Built-in effects don't have a "Vendor UI" concept
    {
        const EffectMeta effectMeta = effectsProvider()->meta(effectId);
        const bool isExternalPlugin = effectMeta.family != EffectFamily::Builtin
                                      && effectMeta.family != EffectFamily::Unknown;

        if (isExternalPlugin) {
            items << makeSeparator();
            ActionQuery q("action://effects/toggle_vendor_ui");
            q.addParam("effectId", Val(effectId.toStdString()));
            MenuItem* item = makeMenuItem(q.toString());
            items << item;
        }
    }

    setItems(items);
    m_presets = presets;
    emit presetsChanged();
}

int EffectManageMenu::instanceId_prop() const
{
    return m_instanceId;
}

void EffectManageMenu::setInstanceId_prop(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}

QVariantList EffectManageMenu::presets()
{
    return m_presets;
}

QString EffectManageMenu::preset() const
{
    return m_currentPreset;
}

void EffectManageMenu::setPreset(QString presetId)
{
    m_currentPreset = presetId;
    resetPreset();
    emit presetChanged();
}

bool EffectManageMenu::enabled() const
{
    return m_presets.size() > 1; // Do not take default preset into account
}

void EffectManageMenu::resetPreset()
{
    ActionQuery q("action://effects/presets/apply");
    q.addParam("instanceId", Val(m_instanceId));
    q.addParam("presetId", Val(m_currentPreset.toStdString()));
    dispatcher()->dispatch(q);
}

void EffectManageMenu::savePresetAs()
{
    ActionQuery q("action://effects/presets/save");
    q.addParam("instanceId", Val(m_instanceId));
    dispatcher()->dispatch(q);
    emit presetsChanged();
    emit presetChanged();
}

bool EffectManageMenu::useVendorUI() const
{
    const EffectInstanceId instanceId = m_instanceId.toULongLong();
    if (instanceId == 0) {
        return true; // Default to vendor UI
    }
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    if (effectId.empty()) {
        return true; // Default to vendor UI
    }
    return configuration()->effectUIMode(effectId) == EffectUIMode::VendorUI;
}

void EffectManageMenu::setUseVendorUI(const bool value)
{
    const EffectInstanceId instanceId = m_instanceId.toULongLong();
    if (instanceId == 0) {
        return;
    }
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    if (effectId.empty()) {
        return;
    }

    const EffectUIMode mode = value ? EffectUIMode::VendorUI : EffectUIMode::FallbackUI;
    configuration()->setEffectUIMode(effectId, mode);
    emit useVendorUIChanged();
}
