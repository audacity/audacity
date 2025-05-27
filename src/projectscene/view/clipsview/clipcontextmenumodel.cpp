/*
* Audacity: A Digital Audio Editor
*/
#include "clipcontextmenumodel.h"

#include "translation.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

static const ActionCode ENABLE_STRETCH_CODE("stretch-clip-to-match-tempo");

namespace {
//! NOTE: can be moved to the framework
bool containsAny(const ActionCodeList& list, const ActionCodeList& actionCodes)
{
    return std::any_of(actionCodes.begin(), actionCodes.end(), [&](ActionCode code) {
        return std::find(list.begin(), list.end(), code) != list.end();
    });
}
}

void ClipContextMenuModel::load()
{
    AbstractMenuModel::load();

    auto makeItemWithArg = [this](const ActionCode& actionCode) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<trackedit::ClipKey>(m_clipKey.key));
        return item;
    };

    auto enableStretchItem = makeItemWithArg(ENABLE_STRETCH_CODE);
    updateStretchEnabledState(*enableStretchItem);

    auto colorItems = makeClipColourItems();

    MenuItemList items {
        makeItemWithArg("clip-properties"),
        makeItemWithArg("rename-clip"),
        makeMenu(muse::TranslatableString("clip", "Clip color"), colorItems, "colorMenu"),
        makeSeparator(),
        makeItemWithArg("copy"),
        makeItemWithArg("duplicate"),
        makeItemWithArg("cut"),
        makeSeparator(),
        makeItemWithArg("split"),
        makeSeparator(),
        makeItemWithArg("clip-export"),
        makeSeparator(),
        enableStretchItem,
        makeItemWithArg("clip-pitch-speed-open"),
        makeItemWithArg("clip-render-pitch-speed"),
    };

    setItems(items);

    updateColorCheckedState();
    updateColorMenu();
}

void ClipContextMenuModel::handleMenuItem(const QString& itemId)
{
    AbstractMenuModel::handleMenuItem(itemId);
}

ClipKey ClipContextMenuModel::clipKey() const
{
    return m_clipKey;
}

void ClipContextMenuModel::setClipKey(const ClipKey& newClipKey)
{
    m_clipKey = newClipKey;
    emit clipKeyChanged();
}

void ClipContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    AbstractMenuModel::onActionsStateChanges(codes);

    if (containsAction(codes, ENABLE_STRETCH_CODE)) {
        MenuItem& item = findItem(ActionCode(ENABLE_STRETCH_CODE));
        updateStretchEnabledState(item);
    }

    if (containsAny(codes, m_colorChangeActionCodeList)) {
        updateColorCheckedState();
    }
}

void ClipContextMenuModel::updateStretchEnabledState(MenuItem& item)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }
    auto clip = project->trackeditProject()->clip(m_clipKey.key);
    if (!clip.isValid()) {
        return;
    }
    auto state = item.state();
    state.checked = clip.stretchToMatchTempo;
    item.setState(state);
}

void ClipContextMenuModel::updateColorCheckedState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }
    auto clip = project->trackeditProject()->clip(m_clipKey.key);
    if (!clip.isValid()) {
        return;
    }

    for (const auto& action : m_colorChangeActionCodeList) {
        MenuItem& item = findItem(ActionCode(action));
        ActionQuery query(action);

        if ((!clip.hasCustomColor && action == m_colorChangeActionCodeList.at(0))
            || (clip.hasCustomColor && query.param("color").toString() == clip.color.toString())) {
            auto state = item.state();
            state.checked = true;
            item.setState(state);
        } else {
            auto state = item.state();
            state.checked = false;
            item.setState(state);
        }
    }
}

void ClipContextMenuModel::updateColorMenu()
{
    ClipStyles::Style clipStyle = projectSceneConfiguration()->clipStyle();
    MenuItem& colorMenu = findMenu("colorMenu");

    if (clipStyle == ClipStyles::Style::CLASSIC) {
        colorMenu.setState(muse::ui::UiActionState::make_disabled());
    } else {
        colorMenu.setState(muse::ui::UiActionState::make_enabled());
    }
}

MenuItemList ClipContextMenuModel::makeClipColourItems()
{
    m_colorChangeActionCodeList.clear();
    MenuItemList items;
    items <<
        makeMenuItem("action://trackedit/clip/change-color-auto",
                     muse::TranslatableString("clip", muse::String::fromStdString("Same as track color")));
    items << makeSeparator();
    m_colorChangeActionCodeList.push_back("action://trackedit/clip/change-color-auto");

    const auto& colors = projectSceneConfiguration()->clipColors();
    for (const auto& color : colors) {
        items << makeMenuItem(makeClipColorChangeAction(color.second).toString(),
                              muse::TranslatableString("clip", muse::String::fromStdString(color.first)));
        m_colorChangeActionCodeList.push_back(makeClipColorChangeAction(color.second).toString());
    }

    return items;
}
