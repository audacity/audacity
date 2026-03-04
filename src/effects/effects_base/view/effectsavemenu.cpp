#include "effectsavemenu.h"

#include "au3wrap/internal/wxtypes_convert.h"

using namespace muse;
using namespace muse::actions;
using namespace muse::uicomponents;
using namespace au::effects;

int EffectSaveMenu::instanceId_prop() const
{
    return m_instanceId;
}

void EffectSaveMenu::setInstanceId_prop(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }

    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}

QString EffectSaveMenu::preset() const
{
    return m_preset;
}

void EffectSaveMenu::setPreset(QString newPreset)
{
    if (m_preset == newPreset) {
        return;
    }

    m_preset = newPreset;
    emit presetChanged();
}

bool EffectSaveMenu::canSave() const
{
    return m_canSave;
}

void EffectSaveMenu::load()
{
    AbstractMenuModel::load();
    reload();
}

void EffectSaveMenu::reload()
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty()) {
        setItems({});
        m_canSave = false;
        return;
    }

    const PresetIdList userPresets = presetsController()->userPresets(effectId);
    const PresetId currentPreset = m_preset.toStdString();

    const auto it = std::find(userPresets.begin(), userPresets.end(), currentPreset);
    m_canSave = !currentPreset.empty() && it != userPresets.end();

    MenuItemList items;

    {
        ActionQuery q("action://effects/presets/save");
        q.addParam("instanceId", Val(m_instanceId));
        q.addParam("presetId", Val(currentPreset.ToStdString()));
        MenuItem* item = makeMenuItem(q.toString(), TranslatableString("effects", "Save"));
        if (!m_canSave) {
            item->setState(ui::UiActionState::make_disabled());
        }
        items << item;
    }

    {
        ActionQuery q("action://effects/presets/save_as");
        q.addParam("instanceId", Val(m_instanceId));
        MenuItem* item = makeMenuItem(q.toString(), TranslatableString("effects", "Save as new"));
        items << item;
    }

    setItems(items);
}
