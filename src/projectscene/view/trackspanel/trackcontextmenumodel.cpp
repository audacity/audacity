/*
* Audacity: A Digital Audio Editor
*/
#include "trackcontextmenumodel.h"

#include "framework/global/async/async.h"

#include "project/projectcommands.h"
#include "projectscene/projectscenecommands.h"
#include "record/recordcommands.h"
#include "spectrogram/spectrogramcommands.h"
#include "trackedit/dom/track.h"
#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;
using namespace muse::rcommand;

namespace {
const QString TRACK_RENAME_ITEM_ID("track-rename");

const QString TRACK_COLOR_MENU_ID("trackColorMenu");
const QString TRACK_FORMAT_MENU_ID("trackFormatMenu");
const QString TRACK_RATE_MENU_ID("trackRateMenu");

const std::vector<Command> TRACK_VIEW_TYPE_COMMANDS = {
    au::trackedit::TRACKEDIT_TRACK_VIEW_WAVEFORM_COMMAND,
    au::trackedit::TRACKEDIT_TRACK_VIEW_SPECTROGRAM_COMMAND,
    au::trackedit::TRACKEDIT_TRACK_VIEW_MULTI_COMMAND,
};

Command trackViewTypeCommand(au::trackedit::TrackViewType viewType)
{
    switch (viewType) {
    case au::trackedit::TrackViewType::Waveform: return au::trackedit::TRACKEDIT_TRACK_VIEW_WAVEFORM_COMMAND;
    case au::trackedit::TrackViewType::Spectrogram: return au::trackedit::TRACKEDIT_TRACK_VIEW_SPECTROGRAM_COMMAND;
    case au::trackedit::TrackViewType::WaveformAndSpectrogram: return au::trackedit::TRACKEDIT_TRACK_VIEW_MULTI_COMMAND;
    default:
        assert(viewType == au::trackedit::TrackViewType::Undefined);
        return Command();
    }
}
}

MenuItem* TrackContextMenuModel::makeRenameItem()
{
    MenuItem* item = new MenuItem(this);
    item->setId(TRACK_RENAME_ITEM_ID);
    item->setTitle(muse::TranslatableString("action", "Rename"));
    item->setDescription(muse::TranslatableString("action_description", "Rename track"));
    item->setEnabled(true);
    return item;
}

MenuItemList TrackContextMenuModel::makeStereoTrackItems()
{
    MenuItemList items {
        makeRenameItem(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DUPLICATE_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Track visualization"), makeTrackVisualizationItems()),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeMenuItem(PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Meters && monitoring"), makeMeterMonitoringItems()),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_SWAP_CHANNELS_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_SPLIT_STEREO_TO_LR_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_SPLIT_STEREO_TO_CENTER_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Format:"), makeTrackFormatItems(), TRACK_FORMAT_MENU_ID),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Rate:"), makeTrackRateItems(), TRACK_RATE_MENU_ID),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_RESAMPLE_COMMAND),
    };

    return items;
}

MenuItemList TrackContextMenuModel::makeMonoTrackItems()
{
    MenuItemList items {
        makeRenameItem(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DUPLICATE_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Track visualization"), makeTrackVisualizationItems()),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeMenuItem(PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Meters && monitoring"), makeMeterMonitoringItems()),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_MAKE_STEREO_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Format:"), makeTrackFormatItems(), TRACK_FORMAT_MENU_ID),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Rate:"), makeTrackRateItems(), TRACK_RATE_MENU_ID),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_RESAMPLE_COMMAND),
    };

    return items;
}

MenuItemList TrackContextMenuModel::makeLabelTrackItems()
{
    return {
        makeRenameItem(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DUPLICATE_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString("trackcontextmenu", "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeSeparator(),
        makeMenuItem(make_query(project::PROJECT_EXPORT_LABELS_COMMAND, {
            { project::PROJECT_TRACK_ID_PARAM, Val(static_cast<int64_t>(m_trackId)) },
        }))
    };
}

void TrackContextMenuModel::load()
{
    AbstractMenuModel::load();

    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        updateColorCheckedState();
        updateTrackFormatState();
        updateTrackRateState();
        updateTrackMonoState();
        updateTrackViewCheckedState();
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->tracksSelected().onReceive(this, [this](const trackedit::TrackIdList&) {
        updateTrackMonoState();
    }, muse::async::Asyncable::Mode::SetReplace);

    trackNavigationController()->openContextMenuRequested().onReceive(this, [this](const trackedit::TrackItemKey& key) {
        if (key.trackId == m_trackId && key.itemId == trackedit::INVALID_TRACK_ITEM) {
            emit openRequested();
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    auto track = prj->track(m_trackId);
    if (!track.has_value()) {
        return;
    }

    prj->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
        if (track.id != m_trackId) {
            return;
        }
        load();
    }, muse::async::Asyncable::Mode::SetReplace);

    switch (track.value().type) {
    case trackedit::TrackType::Mono:
        setItems(makeMonoTrackItems());
        break;
    case trackedit::TrackType::Stereo:
        setItems(makeStereoTrackItems());
        break;
    case trackedit::TrackType::Label:
        setItems(makeLabelTrackItems());
        break;
    default:
        return;
    }

    updateColorCheckedState();
    updateTrackFormatState();
    updateTrackRateState();
    updateTrackMonoState();
    updateTrackViewCheckedState();
    updateTrackHalfWaveCheckedState();
    updateRecordingState();

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        updateRecordingState();
    }, muse::async::Asyncable::Mode::SetReplace);

    if (const auto project = globalContext()->currentProject(); project && project->viewState()) {
        project->viewState()->trackViewType(m_trackId).ch.onReceive(this, [this](trackedit::TrackViewType) {
            updateTrackViewCheckedState();
        }, muse::async::Asyncable::Mode::SetReplace);

        project->viewState()->isHalfWave(m_trackId).ch.onReceive(this, [this](bool) {
            updateTrackHalfWaveCheckedState();
        }, muse::async::Asyncable::Mode::SetReplace);
    }
}

au::trackedit::TrackId TrackContextMenuModel::trackId() const
{
    return m_trackId;
}

void TrackContextMenuModel::setTrackId(const trackedit::TrackId& newTrackId)
{
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

void TrackContextMenuModel::handleMenuItem(const QString& itemId)
{
    if (itemId == TRACK_RENAME_ITEM_ID) {
        emit trackRenameRequested();
    } else {
        //! Why an async call?
        //!
        //! On the one hand, `pushHistoryState` may be called as a result of a QML item triggering an action that will lead to its deletion
        //! (e.g. deleting a track from a context menu).
        //!
        //! On the other hand, when the QML-triggered action is called after an undo, this undo history item gets discarded.
        //! If this undo item involved the creation of audio blocks, such as when generating audio, `pushHistoryState` will delete these blocks.
        //! If there was many of them, it will open a progress dialog which, to be updated, leads to `QCoreApplication::processEvents()` calls.
        //!
        //! This crashes because QCoreApplication::processEvents() will now process an event that deletes the QML item that is at the origin of this very call...
        //! Qt then throws the message "Object 0x..... destroyed while one of its QML signal handlers is in progress."
        //!
        //! https://github.com/audacity/audacity/issues/9530
        muse::async::Async::call(this, [this, itemId]{ AbstractMenuModel::handleMenuItem(itemId); });
    }
}

void TrackContextMenuModel::onCommandStateChanged(const Command& command, const CommandState& state)
{
    AbstractMenuModel::onCommandStateChanged(command, state);

    if (command == trackedit::TRACKEDIT_TRACK_CHANGE_COLOR_COMMAND) {
        updateColorCheckedState();
    }

    if (muse::contains(TRACK_VIEW_TYPE_COMMANDS, command)) {
        updateTrackViewCheckedState();
    }

    if (command == PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND) {
        updateTrackHalfWaveCheckedState();
    }

    if (command == trackedit::TRACKEDIT_TRACK_CHANGE_FORMAT_COMMAND) {
        updateTrackFormatState();
    }

    if (command == trackedit::TRACKEDIT_TRACK_CHANGE_RATE_COMMAND || command == trackedit::TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND) {
        updateTrackRateState();
    }

    if (command == trackedit::TRACKEDIT_TRACK_MAKE_STEREO_COMMAND) {
        updateTrackMonoState();
    }
}

void TrackContextMenuModel::updateColorCheckedState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    auto track = project->trackeditProject()->track(m_trackId);
    if (!track.has_value()) {
        return;
    }

    for (MenuItem* item : findMenu(TRACK_COLOR_MENU_ID).subitems()) {
        if (!item || !item->isValid()) {
            continue;
        }

        const CommandQuery query = item->commandQuery();
        item->setChecked(query.contains(trackedit::TRACKEDIT_COLOR_INDEX_PARAM)
                         && query.param(trackedit::TRACKEDIT_COLOR_INDEX_PARAM).toInt() == track.value().colorIndex);
    }
}

void TrackContextMenuModel::updateTrackViewCheckedState()
{
    const auto prj = globalContext()->currentProject();
    if (!prj) {
        // Probably closing down.
        return;
    }
    const Command currentCommand = trackViewTypeCommand(prj->viewState()->trackViewType(m_trackId).val);
    for (const Command& command : TRACK_VIEW_TYPE_COMMANDS) {
        MenuItem& item = findItem(command);
        if (item.isValid()) {
            item.setChecked(command == currentCommand);
        }
    }
}

void TrackContextMenuModel::updateTrackHalfWaveCheckedState()
{
    const auto project = globalContext()->currentProject();
    if (!project || !project->viewState()) {
        return;
    }

    MenuItem& item = findItem(PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND);
    if (item.isValid()) {
        item.setChecked(project->viewState()->isHalfWave(m_trackId).val);
    }
}

void TrackContextMenuModel::updateTrackFormatState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    auto track = project->trackeditProject()->track(m_trackId);
    if (!track.has_value()) {
        return;
    }

    MenuItem& menu = findMenu(TRACK_FORMAT_MENU_ID);
    for (MenuItem* item : menu.subitems()) {
        if (!item || !item->isValid()) {
            continue;
        }

        const bool checked = item->commandQuery().param(trackedit::TRACKEDIT_FORMAT_PARAM).toInt()
                             == static_cast<int>(track.value().format);
        item->setChecked(checked);
        if (checked) {
            menu.setTitle(muse::TranslatableString("trackcontextmenu", "Format: %1")
                          .arg(muse::TranslatableString::untranslatable(item->translatedTitle())));
        }
    }
}

void TrackContextMenuModel::updateTrackRateState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const auto track = project->trackeditProject()->track(m_trackId);
    if (!track.has_value()) {
        return;
    }

    bool isOnAvailableRates = false;
    for (MenuItem* item : findMenu(TRACK_RATE_MENU_ID).subitems()) {
        if (!item || !item->isValid() || item->command() != trackedit::TRACKEDIT_TRACK_CHANGE_RATE_COMMAND) {
            continue;
        }

        const bool checked = item->commandQuery().param(trackedit::TRACKEDIT_RATE_PARAM).toInt() == static_cast<int>(track.value().rate);
        item->setChecked(checked);
        isOnAvailableRates |= checked;
    }

    MenuItem& customRateItem = findItem(trackedit::TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND);
    customRateItem.setChecked(!isOnAvailableRates);

    MenuItem& menu = findMenu(TRACK_RATE_MENU_ID);
    //: %1 is a sample rate in hertz, e.g. "44100 Hz"
    menu.setTitle(muse::TranslatableString("trackcontextmenu", "Rate: %1 Hz")
                  .arg(muse::String::number(static_cast<int>(track.value().rate))));
}

void TrackContextMenuModel::updateTrackMonoState()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr trackeditProject = project->trackeditProject();
    if (!trackeditProject) {
        return;
    }

    const au::trackedit::TrackList trackList = trackeditProject->trackList();
    const auto it = std::find_if(trackList.begin(), trackList.end(), [this](const au::trackedit::Track& t) {
        return t.id == m_trackId;
    });

    if (it == trackList.end()) {
        return;
    }

    MenuItem& makeStereoItem = findItem(trackedit::TRACKEDIT_TRACK_MAKE_STEREO_COMMAND);
    if (!makeStereoItem.isValid()) {
        return;
    }

    const auto selectedTracks = selectionController()->selectedTracks();
    bool canMakeStereo = (selectedTracks.size() == 1)
                         && (it + 1 != trackList.end())
                         && (it->type == au::trackedit::TrackType::Mono)
                         && (it + 1)->type == au::trackedit::TrackType::Mono;

    makeStereoItem.setEnabled(canMakeStereo);
}

void TrackContextMenuModel::updateRecordingState()
{
    bool recording = globalContext()->isRecording();

    MenuItem& formatMenu = findMenu(TRACK_FORMAT_MENU_ID);
    if (formatMenu.isValid()) {
        formatMenu.setEnabled(!recording);
    }

    MenuItem& rateMenu = findMenu(TRACK_RATE_MENU_ID);
    if (rateMenu.isValid()) {
        rateMenu.setEnabled(!recording);
    }
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackColorItems()
{
    MenuItemList items;
    const auto& colorInfos = projectSceneConfiguration()->clipColorInfos();
    for (const auto& info : colorInfos) {
        MenuItem* item = makeMenuItem(make_query(trackedit::TRACKEDIT_TRACK_CHANGE_COLOR_COMMAND, {
            { trackedit::TRACKEDIT_COLOR_INDEX_PARAM, Val(info.index) },
        }), info.title);
        item->setIconColor(QString::fromStdString(projectSceneConfiguration()->clipColor(info.index).toString()));
        items << item;
    }

    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackFormatItems()
{
    muse::uicomponents::MenuItemList items;
    for (const auto& formatInfo : trackedit::availableTrackFormats()) {
        items << makeMenuItem(make_query(trackedit::TRACKEDIT_TRACK_CHANGE_FORMAT_COMMAND, {
            { trackedit::TRACKEDIT_FORMAT_PARAM, Val(static_cast<int>(formatInfo.format)) },
        }), formatInfo.description);
    }
    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackRateItems()
{
    muse::uicomponents::MenuItemList items;
    for (const auto& rate : audioDriverController()->sampleRates()) {
        items << makeMenuItem(make_query(trackedit::TRACKEDIT_TRACK_CHANGE_RATE_COMMAND, {
            { trackedit::TRACKEDIT_RATE_PARAM, Val(static_cast<int>(rate)) },
        }),
                              //: %1 is a sample rate in hertz, e.g. "44100 Hz"
                              muse::TranslatableString("trackcontextmenu", "%1 Hz").arg(muse::String::number(static_cast<int>(rate))));
    }
    items << makeSeparator();
    items << makeMenuItem(trackedit::TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND);
    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackMoveItems()
{
    return {
        makeMenuItem(trackedit::TRACKEDIT_TRACK_MOVE_UP_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_MOVE_DOWN_COMMAND),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_MOVE_TOP_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_MOVE_BOTTOM_COMMAND),
    };
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackVisualizationItems()
{
    const Params trackParams { { trackedit::TRACKEDIT_TRACK_ID_PARAM, Val(static_cast<int64_t>(m_trackId)) } };

    muse::uicomponents::MenuItemList items;
    for (const Command& command : TRACK_VIEW_TYPE_COMMANDS) {
        items.push_back(makeMenuItem(make_query(command, trackParams)));
    }

    items.push_back(makeMenuItem(make_query(PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND, {
        { PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_TRACK_ID_PARAM, Val(static_cast<int64_t>(m_trackId)) },
    })));
    items.push_back(makeSeparator());

    items.push_back(makeMenuItem(muse::rcommand::make_query(spectrogram::TRACK_SPECTROGRAM_SETTINGS_COMMAND, {
        { spectrogram::TRACK_SPECTROGRAM_SETTINGS_TRACK_ID_PARAM, muse::Val(static_cast<int>(m_trackId)) }
    })));

    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeMeterMonitoringItems()
{
    return {
        makeMenuItem(record::RECORD_TOGGLE_MIC_METERING_COMMAND),
        makeSeparator(),
        makeMenuItem(record::RECORD_TOGGLE_INPUT_MONITORING_COMMAND),
    };
}
