/*
* Audacity: A Digital Audio Editor
*/
#include "trackcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

namespace {
constexpr const char* TRACK_FORMAT_CHANGE_ACTION = "action://trackedit/track/change-format?format=%1";
constexpr const char* TRACK_FORMAT_MENU_ID = "trackFormatMenu";
constexpr const char* TRACK_COLOR_MENU_ID = "trackColorMenu";

struct TrackFormatInfo {
    au::trackedit::TrackFormat format;
    const char* description;
};

constexpr const TrackFormatInfo AVAILABLE_TRACK_FORMATS[] = {
    { au::trackedit::TrackFormat::Int16, "16-bit PCM" },
    { au::trackedit::TrackFormat::Int24, "24-bit PCM" },
    { au::trackedit::TrackFormat::Float32, "32-bit float" }
};

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
        makeMenu(muse::TranslatableString("track menu", "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString("track view", "Track view"), makeTrackViewItems()),
        makeMenu(muse::TranslatableString("track color", "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeMenu(muse::TranslatableString("track ruler", "Rulers"), makeTrackRulerItems()),
        makeSeparator(),
        makeItemWithArg("track-swap-stereo"),
        makeItemWithArg("track-split-stereo-to-lr"),
        makeItemWithArg("track-split-stereo-to-center"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("track format", "Format:"), makeTrackFormatItems(), QString::fromUtf8(TRACK_FORMAT_MENU_ID)),
        makeMenu(muse::TranslatableString("track rate", "Rate:"), makeTrackRateItems()),
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
        makeMenu(muse::TranslatableString("track menu", "Move track"), makeTrackMoveItems()),
        makeMenu(muse::TranslatableString("track view", "Track view"), makeTrackViewItems()),
        makeMenu(muse::TranslatableString("track color", "Track color"), makeTrackColorItems(), TRACK_COLOR_MENU_ID),
        makeMenu(muse::TranslatableString("track ruler", "Rulers"), makeTrackRulerItems()),
        makeSeparator(),
        makeItemWithArg("track-make-stereo"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("track format", "Format:"), makeTrackFormatItems(), QString::fromUtf8(TRACK_FORMAT_MENU_ID)),
        makeMenu(muse::TranslatableString("track rate", "Rate:"), makeTrackRateItems()),
        makeItemWithArg("track-resample"),
    };

    return items;
}

void TrackContextMenuModel::load()
{
    AbstractMenuModel::load();

    projectHistory()->historyChanged().onNotify(this, [this]() {
        updateColorCheckedState();
        updateTrackFormatState();
    });

    auto track = globalContext()->currentTrackeditProject()->track(m_trackId);
    if (!track.has_value()) {
        return;
    }

    switch (track.value().type) {
    case trackedit::TrackType::Mono:
        setItems(makeMonoTrackItems());
        break;
    case trackedit::TrackType::Stereo:
        setItems(makeStereoTrackItems());
        break;
    case trackedit::TrackType::Label:
        break;
    default:
        return;
    }

    updateColorCheckedState();
    updateTrackFormatState();

    MenuItem& waveformViewItem = findItem(ActionCode("track-view-waveform"));
    auto state = waveformViewItem.state();
    state.checked = true;
    waveformViewItem.setState(state);
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
        AbstractMenuModel::handleMenuItem(itemId);
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

    for (const auto& formatInfo : AVAILABLE_TRACK_FORMATS) {
        if (std::find(codes.begin(), codes.end(), makeTrackFormatChangeAction(formatInfo.format).toString()) != codes.end()) {
            updateTrackFormatState();
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

    for (const auto& formatInfo : AVAILABLE_TRACK_FORMATS) {
        MenuItem& item = findItem(makeTrackFormatChangeAction(formatInfo.format).toString());
        if (track.value().format != formatInfo.format) {
            item.setChecked(false);
            continue;
        }

        item.setChecked(true);
        MenuItem& menu = findMenu(QString::fromUtf8(TRACK_FORMAT_MENU_ID));
        menu.setTitle(muse::TranslatableString("track format", "Format: %1")
                      .arg(muse::String(formatInfo.description)));
    }
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackColorItems()
{
    m_colorChangeActionCodeList.clear();

    MenuItemList items;
    const auto& colors = projectSceneConfiguration()->clipColors();
    for (const auto& color : colors) {
        items << makeMenuItem(makeTrackColorChangeAction(color.second).toString(),
                              muse::TranslatableString("track", muse::String::fromStdString(color.first)));
        m_colorChangeActionCodeList.push_back(makeTrackColorChangeAction(color.second).toString());
    }

    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackFormatItems()
{
    muse::uicomponents::MenuItemList items;
    for (const auto& formatInfo : AVAILABLE_TRACK_FORMATS) {
        items << makeMenuItem(makeTrackFormatChangeAction(formatInfo.format).toString(),
                              muse::TranslatableString("track", muse::String(formatInfo.description)));
    }
    return items;
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackRateItems()
{
    return {
        makeItemWithArg("track-rate-8000"),
        makeItemWithArg("track-rate-44100"),
    };
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
    return {
        makeItemWithArg("track-view-waveform"),
        makeItemWithArg("track-view-spectrogram"),
        makeItemWithArg("track-view-multi"),
        makeSeparator(),
        makeItemWithArg("track-view-half-wave"),
    };
}

muse::uicomponents::MenuItemList TrackContextMenuModel::makeTrackRulerItems()
{
    return {
        makeItemWithArg("track-ruler-enable-meter"),
        makeItemWithArg("track-ruler-enable-vertical"),
    };
}
