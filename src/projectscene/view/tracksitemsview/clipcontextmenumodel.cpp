/*
* Audacity: A Digital Audio Editor
*/
#include "clipcontextmenumodel.h"

#include "spectrogram/spectrogramtypes.h"
#include "trackedit/dom/track.h"
#include "framework/global/translation.h"

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

    auto makeItemWithArg = [this](const ActionCode& actionCode, const muse::TranslatableString& title = {}) {
        MenuItem* item = makeMenuItem(actionCode);
        item->setArgs(ActionData::make_arg1<trackedit::ClipKey>(m_clipKey.key));
        if (!title.isEmpty()) {
            item->setTitle(title);
        }
        return item;
    };

    auto enableStretchItem = makeItemWithArg(ENABLE_STRETCH_CODE);
    updateStretchEnabledState(*enableStretchItem);

    auto colorItems = makeClipColourItems();

    MenuItemList items {
        makeItemWithArg("clip-properties"),
        makeItemWithArg("rename-item", muse::TranslatableString("clip", "Rename clip")),
        makeMenu(muse::TranslatableString("clip", "Clip color"), colorItems, "colorMenu"),
        makeSeparator(),
        makeItemWithArg("action://trackedit/cut"),
        makeItemWithArg("action://trackedit/copy"),
        makeItemWithArg("duplicate"),
        makeSeparator(),
        makeItemWithArg("split"),
        makeSeparator(),
        makeItemWithArg("clip-export"),
        makeSeparator(),
        enableStretchItem,
        makeItemWithArg("clip-pitch-speed-open"),
        makeItemWithArg("clip-render-pitch-speed"),
    };

    const auto project = globalContext()->currentProject();
    if (project && project->viewState()) {
        muse::ValCh<trackedit::TrackViewType> valCh = project->viewState()->trackViewType(m_clipKey.trackId());

        if (valCh.val == trackedit::TrackViewType::Spectrogram || valCh.val == trackedit::TrackViewType::WaveformAndSpectrogram) {
            items.push_back(makeSeparator());
            items.push_back(makeMenuItem(spectrogram::TRACK_SPECTROGRAM_SETTINGS_ACTION));
        }

        valCh.ch.onReceive(this, [this](auto) { load(); }, Mode::SetReplace);
    }

    setItems(items);

    updateColorCheckedState();
    updateColorMenu();
}

void ClipContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == spectrogram::TRACK_SPECTROGRAM_SETTINGS_ACTION) {
        const auto project = globalContext()->currentProject();
        IF_ASSERT_FAILED(project) {
            return;
        }
        const auto trackId = m_clipKey.trackId();
        const auto track = project->trackeditProject()->track(trackId);
        IF_ASSERT_FAILED(track) {
            return;
        }
        const muse::String trackTitle = track->title;
        auto args = muse::actions::ActionData::make_arg2(trackId, trackTitle);
        dispatcher()->dispatch(spectrogram::TRACK_SPECTROGRAM_SETTINGS_ACTION, std::move(args));
    } else {
        AbstractMenuModel::handleMenuItem(itemId);
    }
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
