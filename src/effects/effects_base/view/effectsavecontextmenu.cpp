/*
* Audacity: A Digital Audio Editor
*/
#include "effectsavecontextmenu.h"

// needed for PresetIdList (TODO: remove wxWidgets types from interfaces)
#include "au3wrap/internal/wxtypes_convert.h"

#include "../effectscommands.h"

using namespace muse;
using namespace muse::rcommand;
using namespace muse::uicomponents;
using namespace au::effects;

EffectSaveContextMenu::EffectSaveContextMenu(QObject* parent)
    : AbstractMenuModel(parent)
{
}

int EffectSaveContextMenu::instanceId_prop() const
{
    return m_instanceId;
}

void EffectSaveContextMenu::setInstanceId_prop(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }

    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}

QString EffectSaveContextMenu::preset() const
{
    return m_preset;
}

void EffectSaveContextMenu::setPreset(QString newPreset)
{
    if (m_preset == newPreset) {
        return;
    }

    m_preset = newPreset;
    emit presetChanged();
}

void EffectSaveContextMenu::load()
{
    AbstractMenuModel::load();
    reload();
}

void EffectSaveContextMenu::reload()
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty()) {
        setItems({});
        m_canSave = false;
        return;
    }

    const PresetIdList userPresets = presetsController()->userPresets(effectId);
    const std::string currentPreset = m_preset.toStdString();

    const auto it = std::find(userPresets.begin(), userPresets.end(), au3::wxFromStdString(currentPreset));
    m_canSave = !currentPreset.empty() && it != userPresets.end();

    MenuItemList items;

    {
        MenuItem* item = makeMenuItem(make_query(EFFECTS_PRESET_SAVE_COMMAND, {
            { EFFECTS_INSTANCE_ID_PARAM, Val(m_instanceId) },
            { EFFECTS_PRESET_ID_PARAM, Val(currentPreset) },
        }), muse::TranslatableString("effects", "Save"));
        if (!m_canSave) {
            item->setEnabled(false);
        }
        items << item;
    }

    {
        MenuItem* item = makeMenuItem(make_query(EFFECTS_PRESET_SAVE_AS_COMMAND, {
            { EFFECTS_INSTANCE_ID_PARAM, Val(m_instanceId) },
        }), muse::TranslatableString("effects", "Save as…"));
        items << item;
    }

    setItems(items);
}
