/*
* Audacity: A Digital Audio Editor
*/
#include "clipcontextmenumodel.h"

#include "framework/global/translation.h"
#include "framework/global/realfn.h"
#include "framework/ui/view/iconcodes.h"

#include "spectrogram/spectrogramcommands.h"
#include "trackedit/dom/track.h"
#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;
using namespace muse::rcommand;

static const QString COLOR_MENU_ID("colorMenu");

void ClipContextMenuModel::load()
{
    AbstractMenuModel::load();

    auto makeItemWithClipKey = [this](const Command& command) {
        return makeMenuItem(make_query(command, clipParams()));
    };

    auto enableStretchItem = makeItemWithClipKey(trackedit::TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND);
    updateStretchEnabledState(*enableStretchItem);

    auto renderPitchSpeedItem = makeItemWithClipKey(trackedit::TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND);
    updatePitchSpeedModifiedEnabledState(*renderPitchSpeedItem);

    auto resetPitchSpeedItem = makeItemWithClipKey(trackedit::TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND);
    updatePitchSpeedModifiedEnabledState(*resetPitchSpeedItem);

    auto colorItems = makeClipColourItems();

    MenuItemList cutAndItems {
        makeMenuItem(trackedit::TRACKEDIT_CUT_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("clip", "Cut and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("clip", "Cut and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("clip", "Cut and close gap on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_DELETE_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("clip", "Delete and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("clip", "Delete and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("clip", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        // makeMenuItem("clip-properties"),
        makeMenuItem(trackedit::TRACKEDIT_RENAME_ITEM_COMMAND, muse::TranslatableString("clip", "Rename clip")),
        makeMenu(muse::TranslatableString("clip", "Clip color"), colorItems, COLOR_MENU_ID),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_CUT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_COPY_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DUPLICATE_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("clip", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("clip", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_SPLIT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DISJOIN_COMMAND, muse::TranslatableString("clip", "Split at silences")),
        makeSeparator(),
        enableStretchItem,
        makeMenuItem(trackedit::TRACKEDIT_CLIP_PITCH_SPEED_OPEN_COMMAND),
        resetPitchSpeedItem,
        renderPitchSpeedItem,
    };

    const auto project = globalContext()->currentProject();
    if (project && project->viewState()) {
        muse::ValCh<trackedit::TrackViewType> valCh = project->viewState()->trackViewType(m_clipKey.trackId());

        if (valCh.val == trackedit::TrackViewType::Spectrogram || valCh.val == trackedit::TrackViewType::WaveformAndSpectrogram) {
            items.push_back(makeSeparator());
            items.push_back(makeMenuItem(make_query(spectrogram::TRACK_SPECTROGRAM_SETTINGS_COMMAND, {
                { spectrogram::TRACK_SPECTROGRAM_SETTINGS_TRACK_ID_PARAM, muse::Val(static_cast<int>(m_clipKey.trackId())) }
            })));
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

        prj->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
            if (track.id == m_clipKey.trackId()) {
                updateColorCheckedState();
            }
        }, muse::async::Asyncable::Mode::SetReplace);
    }

    setItems(items);

    updateColorCheckedState();
    updateColorMenu();
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

Params ClipContextMenuModel::clipParams() const
{
    return {
        { trackedit::TRACKEDIT_TRACK_ID_PARAM, Val(static_cast<int64_t>(m_clipKey.key.trackId)) },
        { trackedit::TRACKEDIT_CLIP_ID_PARAM, Val(static_cast<int64_t>(m_clipKey.key.itemId)) },
    };
}

void ClipContextMenuModel::onCommandStateChanged(const Command& command, const CommandState& state)
{
    AbstractMenuModel::onCommandStateChanged(command, state);

    if (command == trackedit::TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND) {
        updateStretchEnabledState(findItem(command));
    }

    if (command == trackedit::TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND
        || command == trackedit::TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND) {
        updatePitchSpeedModifiedEnabledState(findItem(command));
    }

    if (command == trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND || command == trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND) {
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
    item.setChecked(clip.stretchToMatchTempo);
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
    item.setEnabled(item.enabled() && hasPitchOrSpeed);
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

    const auto trackOpt = project->trackeditProject()->track(m_clipKey.trackId());
    if (trackOpt) {
        const muse::Color trackColor = projectSceneConfiguration()->clipColor(trackOpt->colorIndex);
        MenuItem& autoColorItem = findItem(trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND);
        autoColorItem.setIcon(muse::ui::IconCode::Code::FRETBOARD_MARKER_CIRCLE_FILLED);
        autoColorItem.setIconColor(QString::fromStdString(trackColor.toString()));
    }

    for (MenuItem* item : findMenu(COLOR_MENU_ID).subitems()) {
        if (!item || !item->isValid()) {
            continue;
        }

        if (item->command() == trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND) {
            item->setChecked(clip.isAutoColor);
        } else if (item->command() == trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND) {
            const CommandQuery query = item->commandQuery();
            item->setChecked(!clip.isAutoColor
                             && query.contains(trackedit::TRACKEDIT_COLOR_INDEX_PARAM)
                             && query.param(trackedit::TRACKEDIT_COLOR_INDEX_PARAM).toInt() == clip.colorIndex);
        }
    }
}

void ClipContextMenuModel::updateColorMenu()
{
    ClipStyles::Style clipStyle = projectSceneConfiguration()->clipStyle();
    findMenu(COLOR_MENU_ID).setEnabled(clipStyle != ClipStyles::Style::CLASSIC);
}

MenuItemList ClipContextMenuModel::makeClipColourItems()
{
    MenuItemList items;
    items << makeMenuItem(trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND,
                          muse::TranslatableString("clip", "Same as track color"));
    items << makeSeparator();

    const auto& colorInfos = projectSceneConfiguration()->clipColorInfos();
    for (const auto& info : colorInfos) {
        items << makeMenuItem(make_query(trackedit::TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND, {
            { trackedit::TRACKEDIT_COLOR_INDEX_PARAM, Val(info.index) }
        }), info.title);
    }

    return items;
}
