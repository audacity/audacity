/*
* Audacity: A Digital Audio Editor
*/
#include "trackcontextmenumodel.h"
#include "trackedit/dom/track.h"

#include "global/async/async.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

namespace {
constexpr const char* TRACK_FORMAT_CHANGE_ACTION = "action://trackedit/track/change-format?format=%1";
constexpr const char* TRACK_RATE_CHANGE_ACTION = "action://trackedit/track/change-rate?rate=%1";

constexpr const char* TRACK_VIEW_WAVEFORM_ACTION = "action://trackedit/track-view-waveform";
constexpr const char* TRACK_VIEW_SPECTROGRAM_ACTION = "action://trackedit/track-view-spectrogram";
constexpr const char* TRACK_VIEW_MULTI_ACTION = "action://trackedit/track-view-multi";
constexpr const char* TRACK_VIEW_HALF_WAVE_ACTION = "action://projectscene/track-view-half-wave";

constexpr const char* TRACK_SPECTROGRAM_SETTINGS_ACTION = "action://trackedit/track-spectrogram-settings";

constexpr const char* TRACK_COLOR_MENU_ID = "trackColorMenu";
constexpr const char* TRACK_FORMAT_MENU_ID = "trackFormatMenu";
constexpr const char* TRACK_RATE_MENU_ID = "trackRateMenu";

constexpr const char* TRANSLATABLE_STRING_CONTEXT = "trackcontextmenu";

//! NOTE: can be moved to the framework
bool containsAny(const ActionCodeList& list, const ActionCodeList& actionCodes)
{
    return std::any_of(actionCodes.begin(), actionCodes.end(), [&](ActionCode code) {
        return std::find(list.begin(), list.end(), code) != list.end();
    });
}

muse::actions::ActionQuery makeTrackFormatChangeAction(const au::trackedit::TrackFormat& format)
{
    return muse::actions::ActionQuery(muse::String(TRACK_FORMAT_CHANGE_ACTION).arg(
                                          muse::String::number(static_cast<int>(format))));
}

muse::actions::ActionQuery makeTrackRateChangeAction(uint64_t rate)
{
    return muse::actions::ActionQuery(muse::String(TRACK_RATE_CHANGE_ACTION).arg(muse::String::number(static_cast<int>(rate))));
}
}

MenuItem* TrackContextMenuModel::makeItemWithArg(const ActionCode& actionCode)
{
    MenuItem* item = makeMenuItem(actionCode);
    item->setArgs(ActionData::make_arg1<trackedit::TrackId>(m_trackId));
    return item;
}

MenuItemList TrackContextMenuModel::makeStereoTrackItems()
{
    MenuItemList items {
        makeItemWithArg("track-rename"),
        makeItemWithArg("track-duplicate"),
        makeItemWithArg("track-delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Track view"), makeTrackViewItems()),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeItemWithArg("toggle-vertical-rulers"),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Meters && monitoring"), makeMeterMonitoringItems()),
        makeSeparator(),
        makeItemWithArg("track-swap-channels"),
        makeItemWithArg("track-split-stereo-to-lr"),
        makeItemWithArg("track-split-stereo-to-center"),
        makeSeparator(),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Format:"), makeTrackFormatItems(), TRACK_FORMAT_MENU_ID),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Rate:"), makeTrackRateItems(), TRACK_RATE_MENU_ID),
        makeItemWithArg("track-resample"),
    };

    return items;
}

MenuItemList TrackContextMenuModel::makeMonoTrackItems()
{
    MenuItemList items {
        makeItemWithArg("track-rename"),
        makeItemWithArg("track-duplicate"),
        makeItemWithArg("track-delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Track view"), makeTrackViewItems()),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeItemWithArg("toggle-vertical-rulers"),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Meters && monitoring"), makeMeterMonitoringItems()),
        makeSeparator(),
        makeItemWithArg("track-make-stereo"),
        makeSeparator(),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Format:"), makeTrackFormatItems(), TRACK_FORMAT_MENU_ID),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Rate:"), makeTrackRateItems(), TRACK_RATE_MENU_ID),
        makeItemWithArg("track-resample"),
    };

    return items;
}

MenuItemList TrackContextMenuModel::makeLabelTrackItems()
{
    return {
        makeItemWithArg("track-rename"),
        makeItemWithArg("track-duplicate"),
        makeItemWithArg("track-delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeSeparator(),
        makeItemWithArg("export-labels")
    };
}

void TrackContextMenuModel::load()
{
    AbstractMenuModel::load();

    projectHistory()->historyChanged().onNotify(this, [this]() {
        updateColorCheckedState();
        updateTrackFormatState();
        updateTrackRateState();
        updateTrackMonoState();
        updateTrackViewCheckedState();
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->tracksSelected().onReceive(this, [this](const trackedit::TrackIdList&) {
        updateTrackMonoState();
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
    if (itemId == "track-rename") {
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

void TrackContextMenuModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    if (codes.empty()) {
        return;
    }

    for (const ActionCode& code : codes) {
        MenuItem& actionItem = findItem(code);
        if (actionItem.isValid()) {
            actionItem.setState(uiActionsRegister()->actionState(code));
        }
    }

    if (containsAny(codes, m_colorChangeActionCodeList)) {
        updateColorCheckedState();
    }

    if (containsAny(codes, m_trackViewTypeChangeActionCodeList)) {
        updateTrackViewCheckedState();
    }

    if (containsAny(codes, { ActionCode(TRACK_VIEW_HALF_WAVE_ACTION) })) {
        const auto project = globalContext()->currentProject();
        assert(project);

        const auto viewState = project->viewState();
        assert(viewState);

        const bool isHalfWave = viewState->isHalfWave(m_trackId).val;

        MenuItem& item = findItem(ActionCode(TRACK_VIEW_HALF_WAVE_ACTION));
        auto state = item.state();
        state.checked = isHalfWave;
        item.setState(state);
    }

    for (const auto& formatInfo : au::trackedit::availableTrackFormats()) {
        if (std::find(codes.begin(), codes.end(), makeTrackFormatChangeAction(formatInfo.format).toString()) != codes.end()) {
            updateTrackFormatState();
        }
    }

    for (const auto& rate : audioDevicesProvider()->availableSampleRateList()) {
        if (std::find(codes.begin(), codes.end(), makeTrackRateChangeAction(rate).toString()) != codes.end()) {
            updateTrackRateState();
        }
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

    for (const auto& action : m_colorChangeActionCodeList) {
        MenuItem& item = findItem(ActionCode(action));
        ActionQuery query(action);
        track.value().color.toString();

        if (query.param("color").toString() == track.value().color.toString()) {
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

void TrackContextMenuModel::updateTrackViewCheckedState()
{
    const trackedit::ITrackeditProjectPtr trackeditProject = globalContext()->currentTrackeditProject();
    const std::optional<trackedit::Track> track = trackeditProject->track(m_trackId);
    if (track.has_value()) {
        const trackedit::TrackViewType viewType = track->viewType;
        for (const auto& viewTypeCode : m_trackViewTypeChangeActionCodeList) {
            MenuItem& item = findItem(viewTypeCode);
            auto state = item.state();
            switch (viewType) {
            case trackedit::TrackViewType::Waveform:
                state.checked = (viewTypeCode == TRACK_VIEW_WAVEFORM_ACTION);
                break;
            case trackedit::TrackViewType::Spectrogram:
                state.checked = (viewTypeCode == TRACK_VIEW_SPECTROGRAM_ACTION);
                break;
            case trackedit::TrackViewType::WaveformAndSpectrogram:
                state.checked = (viewTypeCode == TRACK_VIEW_MULTI_ACTION);
                break;
            default:
                assert(false);
                state.checked = false;
            }
            item.setState(state);
        }
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

    for (const auto& formatInfo : au::trackedit::availableTrackFormats()) {
        MenuItem& item = findItem(makeTrackFormatChangeAction(formatInfo.format).toString());
        if (track.value().format != formatInfo.format) {
            item.setChecked(false);
            continue;
        }

        item.setChecked(true);
        MenuItem& menu = findMenu(QString::fromUtf8(TRACK_FORMAT_MENU_ID));
        menu.setTitle(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Format: %1")
                      .arg(muse::String(formatInfo.description)));
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
    for (const auto& rate : audioDevicesProvider()->availableSampleRateList()) {
        MenuItem& item = findItem(makeTrackRateChangeAction(rate).toString());
        if (track.value().rate != rate) {
            item.setChecked(false);
            continue;
        }

        item.setChecked(true);
        isOnAvailableRates = true;
    }

    MenuItem& customRateItem = findItem(ActionCode("track-change-rate-custom"));
    customRateItem.setChecked(!isOnAvailableRates);

    MenuItem& menu = findMenu(QString::fromUtf8(TRACK_RATE_MENU_ID));
    menu.setTitle(muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, "Rate: %1 Hz")
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

    MenuItem& makeStereoItem = findItem(ActionCode("track-make-stereo"));
    if (!makeStereoItem.isValid()) {
        return;
    }

    const auto selectedTracks = selectionController()->selectedTracks();
    bool canMakeStereo = (selectedTracks.size() == 1)
                         && (it + 1 != trackList.end())
                         && (it->type == au::trackedit::TrackType::Mono)
                         && (it + 1)->type == au::trackedit::TrackType::Mono;

    auto state = makeStereoItem.state();
    state.enabled = canMakeStereo;
    makeStereoItem.setState(state);
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackColorItems()
{
    m_colorChangeActionCodeList.clear();

    MenuItemList items;
    const auto& colors = projectSceneConfiguration()->clipColors();
    for (const auto& color : colors) {
        items << makeMenuItem(makeTrackColorChangeAction(color.second).toString(),
                              muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, muse::String::fromStdString(color.first)));
        m_colorChangeActionCodeList.push_back(makeTrackColorChangeAction(color.second).toString());
    }

    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackFormatItems()
{
    muse::uicomponents::MenuItemList items;
    for (const auto& formatInfo : trackedit::availableTrackFormats()) {
        items << makeMenuItem(makeTrackFormatChangeAction(formatInfo.format).toString(),
                              muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, muse::String(formatInfo.description)));
    }
    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackRateItems()
{
    muse::uicomponents::MenuItemList items;
    for (const auto& rate : audioDevicesProvider()->availableSampleRateList()) {
        items << makeMenuItem(makeTrackRateChangeAction(rate).toString(),
                              muse::TranslatableString(TRANSLATABLE_STRING_CONTEXT, muse::String::number(static_cast<int>(rate)) + " Hz"));
    }
    items << makeSeparator();
    items << makeItemWithArg("track-change-rate-custom");
    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackMoveItems()
{
    return {
        makeItemWithArg("track-move-up"),
        makeItemWithArg("track-move-down"),
        makeSeparator(),
        makeItemWithArg("track-move-top"),
        makeItemWithArg("track-move-bottom"),
    };
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackViewItems()
{
    m_trackViewTypeChangeActionCodeList.clear();
    muse::uicomponents::MenuItemList items;
    items.reserve(5);
    for (const std::string action : { TRACK_VIEW_WAVEFORM_ACTION,
                                      TRACK_VIEW_SPECTROGRAM_ACTION,
                                      TRACK_VIEW_MULTI_ACTION,
                                      TRACK_VIEW_HALF_WAVE_ACTION,
                                      "separator",
                                      TRACK_SPECTROGRAM_SETTINGS_ACTION }) {
        if (action == "separator") {
            items.push_back(makeSeparator());
        } else {
            muse::actions::ActionQuery query(action);
            query.addParam("trackId", muse::Val { m_trackId });
            items.push_back(makeMenuItem(query.toString()));
            m_trackViewTypeChangeActionCodeList.push_back(action);
        }
    }
    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeMeterMonitoringItems()
{
    return {
        makeItemWithArg("action://record/toggle-mic-metering"),
        makeSeparator(),
        makeItemWithArg("action://record/toggle-input-monitoring"),
    };
}
