#include "presetscontextmenumodel.h"

using namespace muse;
using namespace muse::actions;
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

    {
        ActionQuery q("action://effects/presets/import");
        q.addParam("instanceId", Val(m_instanceId));
        items << makeMenuItem(q.toString());
    }

    {
        ActionQuery q("action://effects/presets/export");
        q.addParam("instanceId", Val(m_instanceId));
        items << makeMenuItem(q.toString());
    }

    const EffectMeta effectMeta = effectsProvider()->meta(effectId);
    const bool hasVendorUI = effectMeta.family != EffectFamily::Builtin
                             && effectMeta.family != EffectFamily::Nyquist
                             && effectMeta.family != EffectFamily::Unknown;
    if (hasVendorUI) {
        items << makeSeparator();

        ActionQuery q("action://effects/toggle_vendor_ui");
        q.addParam("effectId", Val(effectId.toStdString()));
        MenuItem* item = makeMenuItem(q.toString());

        if (item) {
            ui::UiActionState state = item->state();
            state.checked = useVendorUI();
            item->setState(state);
        }

        items << item;
    }

    setItems(items);
}
