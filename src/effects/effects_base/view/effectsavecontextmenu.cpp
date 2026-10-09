/*
* Audacity: A Digital Audio Editor
*/
#include "effectsavecontextmenu.h"

#include "effects/effects_base/effectscommands.h"

// needed for PresetIdList (TODO: remove wxWidgets types from interfaces)
#include "au3wrap/internal/wxtypes_convert.h"

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

    MenuItem* saveItem = makeMenuItem(SavePresetCommand { .instanceId = m_instanceId, .presetId = currentPreset },
                                      muse::TranslatableString("effects", "Save"));
    if (!m_canSave) {
        saveItem->setCommandState(CommandState(false));
    }
    items << saveItem;

    items << makeMenuItem(SavePresetAsCommand { .instanceId = m_instanceId },
                          muse::TranslatableString("effects", "Save as…"));

    setItems(items);
}
