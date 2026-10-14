/*
* Audacity: A Digital Audio Editor
*/
#include "presetscontextmenumodel.h"

#include "effects/effects_base/effectscommands.h"

using namespace muse;
using namespace muse::rcommand;
using namespace muse::uicomponents;
using namespace au::effects;

PresetsContextMenuModel::PresetsContextMenuModel(QObject* parent)
    : AbstractMenuModel(parent)
{
}

int PresetsContextMenuModel::instanceId_prop() const
{
    return m_instanceId;
}

void PresetsContextMenuModel::setInstanceId_prop(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }

    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}

bool PresetsContextMenuModel::useVendorUI() const
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty()) {
        return true;
    }

    return configuration()->effectUIMode(effectId) == EffectUIMode::VendorUI;
}

void PresetsContextMenuModel::load()
{
    AbstractMenuModel::load();

    configuration()->effectUIModeChanged().onNotify(this, [this] {
        emit useVendorUIChanged();
        reload();
    }, muse::async::Asyncable::Mode::SetReplace);

    reload();
}

void PresetsContextMenuModel::reload()
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty()) {
        setItems({});
        return;
    }

    MenuItemList items;

    items << makeMenuItem(ImportPresetCommand { .instanceId = m_instanceId });
    items << makeMenuItem(ExportPresetCommand { .instanceId = m_instanceId });

    const EffectMeta effectMeta = effectsProvider()->meta(effectId);
    const IEffectViewLauncherPtr launcher = viewLaunchRegister()->launcher(effectMeta.family);
    const bool hasVendorUI = effectMeta.family != EffectFamily::Builtin
                             && effectMeta.family != EffectFamily::Nyquist
                             && effectMeta.family != EffectFamily::Unknown
                             && (!launcher || launcher->vendorUiSupported(effectId));
    if (hasVendorUI) {
        items << makeSeparator();

        MenuItem* item = makeMenuItem(ToggleVendorUiCommand { .effectId = effectId });
        if (item) {
            item->setCommandState(CommandState(true, useVendorUI()));
        }

        items << item;
    }

    setItems(items);
}
