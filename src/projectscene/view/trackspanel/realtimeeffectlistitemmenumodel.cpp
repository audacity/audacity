/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmenumodel.h"
#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/effectsutils.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "global/defer.h"
#include "log.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace au::effects;

RealtimeEffectListItemMenuModel::RealtimeEffectListItemMenuModel(QObject* parent)
    : RealtimeEffectMenuModelBase(parent)
{
}

bool RealtimeEffectListItemMenuModel::belongsWithMe(effects::TrackId trackId) const
{
    return isMasterTrack() == (trackId == IRealtimeEffectService::masterTrackId);
}

void RealtimeEffectListItemMenuModel::doLoad()
{
    doPopulateMenu();
}

void RealtimeEffectListItemMenuModel::doPopulateMenu()
{
    MenuItemList items;

    items << makeMenuItem("realtimeeffect-remove", muse::TranslatableString("projectscene", "No effect")) << makeSeparator();

    const auto effectMenus = effects::utils::realtimeEffectMenu(
        effectsConfiguration()->effectMenuOrganization(),
        effectsProvider()->effectMetaList(), m_effectFilter, *this);
    if (!effectMenus.empty()) {
        items << makeSeparator() << effectMenus;
    }

    setItems(items);
    updateEffectCheckmarks();
}

void RealtimeEffectListItemMenuModel::handleMenuItem(const QString& itemId)
{
    const MenuItem& menuItem = findItem(itemId);
    const auto tId = trackId();
    IF_ASSERT_FAILED(tId.has_value() && m_effectState) {
        return;
    }

    if (menuItem.id() == "realtimeeffect-remove") {
        realtimeEffectService()->removeRealtimeEffect(*tId, m_effectState);
    } else {
        const auto effectId = effects::effectIdFromAction(menuItem.id());
        if (const RealtimeEffectStatePtr newState = realtimeEffectService()->replaceRealtimeEffect(*tId, m_effectState, effectId)) {
            effectsProvider()->showEffect(newState);
        }
    }
}

QString RealtimeEffectListItemMenuModel::prop_effectState() const
{
    if (!m_effectState) {
        return {};
    }
    return QString::number(reinterpret_cast<uintptr_t>(m_effectState.get()));
}

void RealtimeEffectListItemMenuModel::prop_setEffectState(const QString& state)
{
    if (state == prop_effectState()) {
        return;
    }
    if (state.isEmpty()) {
        m_effectState.reset();
    } else {
        m_effectState = reinterpret_cast<RealtimeEffectState*>(state.toLongLong())->shared_from_this();
    }
    updateEffectCheckmarks();
    emit effectStateChanged();
}

namespace {
bool updateCheckmarks(MenuItem& item, const au::effects::EffectId& selectedEffectId)
{
    if (item.subitems().empty()) {
        const auto itemEffectId = au::effects::effectIdFromAction(item.id());
        const auto checked = itemEffectId == selectedEffectId;
        item.setChecked(checked);
        return checked;
    } else {
        auto checked = false;
        for (MenuItem* subItem : item.subitems()) {
            checked |= updateCheckmarks(*subItem, selectedEffectId);
        }
        item.setChecked(checked);
        return checked;
    }
}
}

void RealtimeEffectListItemMenuModel::updateEffectCheckmarks()
{
    if (m_effectState == nullptr) {
        return;
    }
    const MenuItemList& itemList = items();
    const auto myEffectId = muse::String::fromStdString(m_effectState->GetID().ToStdString());
    for (MenuItem* category : itemList) {
        updateCheckmarks(*category, myEffectId);
    }
}

muse::uicomponents::MenuItem* RealtimeEffectListItemMenuModel::makeMenuEffectItem(const effects::EffectId& effectId)
{
    auto item = makeMenuItem(effects::makeEffectAction(effects::REALTIME_EFFECT_REPLACE_ACTION, effectId));
    item->setCheckable(true);
    return item;
}

muse::uicomponents::MenuItem* RealtimeEffectListItemMenuModel::makeMenuEffect(const muse::String& title,
                                                                              const muse::uicomponents::MenuItemList& items)
{
    auto menu = makeMenu(muse::TranslatableString::untranslatable(title), items);
    menu->setCheckable(true);
    return menu;
}
