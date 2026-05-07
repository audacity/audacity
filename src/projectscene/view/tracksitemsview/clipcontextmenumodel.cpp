/*
* Audacity: A Digital Audio Editor
*/
#include "clipcontextmenumodel.h"

#include "spectrogram/spectrogramtypes.h"
#include "trackedit/dom/track.h"
#include "framework/global/translation.h"
#include "global/realfn.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

static const ActionCode ENABLE_STRETCH_CODE("stretch-clip-to-match-tempo");
static const ActionCode RENDER_PITCH_SPEED_CODE("clip-render-pitch-speed");
static const ActionCode RESET_PITCH_SPEED_CODE("clip-reset-pitch-speed");
static const ActionCodeList PITCH_SPEED_CODES { RENDER_PITCH_SPEED_CODE, RESET_PITCH_SPEED_CODE };

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

    auto renderPitchSpeedItem = makeItemWithArg(RENDER_PITCH_SPEED_CODE);
    updatePitchSpeedModifiedEnabledState(*renderPitchSpeedItem);

    auto resetPitchSpeedItem = makeItemWithArg(RESET_PITCH_SPEED_CODE);
    updatePitchSpeedModifiedEnabledState(*resetPitchSpeedItem);

    auto colorItems = makeClipColourItems();

    MenuItemList cutAndItems {
        makeMenuItem("cut-leave-gap",
                     muse::TranslatableString("clip", "Cut and leave gap")),
        makeMenuItem("cut-per-track-ripple",
                     muse::TranslatableString("clip", "Cut and close gap on this track")),
        makeMenuItem("cut-all-tracks-ripple",
                     muse::TranslatableString("clip", "Cut and close gap on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem("delete-leave-gap",
                     muse::TranslatableString("clip", "Delete and leave gap")),
        makeMenuItem("delete-per-track-ripple",
                     muse::TranslatableString("clip", "Delete and close gap on this track")),
        makeMenuItem("delete-all-tracks-ripple",
                     muse::TranslatableString("clip", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeItemWithArg("clip-properties"),
        makeItemWithArg("rename-item", muse::TranslatableString("clip", "Rename clip")),
        makeMenu(muse::TranslatableString("clip", "Clip color"), colorItems, "colorMenu"),
        makeSeparator(),
        makeItemWithArg("action://trackedit/cut"),
        makeItemWithArg("action://trackedit/copy"),
        makeItemWithArg("duplicate"),
        makeItemWithArg("action://trackedit/delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("clip", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("clip", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeItemWithArg("split"),
        makeItemWithArg("clip-export"),
        makeSeparator(),
        enableStretchItem,
        makeItemWithArg("clip-pitch-speed-open"),
        resetPitchSpeedItem,
        renderPitchSpeedItem,
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

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->clipList(m_clipKey.trackId()).onItemChanged(this, [this](const trackedit::Clip& clip) {
            if (clip.key == m_clipKey.key) {
                load();
            }
        }, muse::async::Asyncable::Mode::SetReplace);
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

    if (containsAny(codes, PITCH_SPEED_CODES)) {
        for (const auto& code : PITCH_SPEED_CODES) {
            MenuItem& item = findItem(ActionCode(code));
            updatePitchSpeedModifiedEnabledState(item);
        }
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

void ClipContextMenuModel::updatePitchSpeedModifiedEnabledState(MenuItem& item)
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }
    auto clip = project->trackeditProject()->clip(m_clipKey.key);
    if (!clip.isValid()) {
        return;
    }

    const bool hasPitchOrSpeed = clip.pitch != 0 || !muse::RealIsEqual(clip.speed, 1.0);
    auto state = item.state();
    state.enabled = state.enabled && hasPitchOrSpeed;
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

        bool checked = false;
        if (clip.colorIndex == trackedit::CLIP_COLOR_INDEX_NONE && action == m_colorChangeActionCodeList.at(0)) {
            checked = true;
        } else if (query.contains("colorindex") && query.param("colorindex").toInt() == clip.colorIndex) {
            checked = true;
        }

        auto state = item.state();
        state.checked = checked;
        item.setState(state);
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

    const auto& colorInfos = projectSceneConfiguration()->clipColorInfos();
    for (const auto& info : colorInfos) {
        items << makeMenuItem(makeClipColorChangeAction(info.index).toString(),
                              muse::TranslatableString("clip", muse::String::fromStdString(info.name)));
        m_colorChangeActionCodeList.push_back(makeClipColorChangeAction(info.index).toString());
    }

    return items;
}
