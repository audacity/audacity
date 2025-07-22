/*
* Audacity: A Digital Audio Editor
*/
#include "trackedituiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"
#include "dom/track.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

namespace {
constexpr const char16_t* TRACK_FORMAT_CHANGE_ACTION = u"action://trackedit/track/change-format?format=%1";
constexpr const char16_t* TRACK_RATE_CHANGE_ACTION = u"action://trackedit/track/change-rate?rate=%1";

UiActionList STATIC_ACTIONS = {
    UiAction("toggle-loop-region",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Toggle loop region"),
             TranslatableString("action", "Toggle loop region")
             ),
    UiAction("clear-loop-region",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Clear loop region"),
             TranslatableString("action", "Clear loop region")
             ),
    UiAction("set-loop-region-to-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region to selection"),
             TranslatableString("action", "Set loop region to selection")
             ),
    UiAction("set-selection-to-loop",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set selection to loop"),
             TranslatableString("action", "Set selection to loop")
             ),
    UiAction("set-loop-region-in",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region in"),
             TranslatableString("action", "Set loop region in")
             ),
    UiAction("set-loop-region-out",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region out"),
             TranslatableString("action", "Set loop region out")
             ),
    UiAction("select-all",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Select all"),
             TranslatableString("action", "Select all")
             ),
    UiAction("clear-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Clear selection"),
             TranslatableString("action", "Clear selection")
             ),
    UiAction("cut",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut"),
             IconCode::Code::CUT
             ),
    UiAction("cut-per-clip-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut and close gap (per clip)"),
             TranslatableString("action", "Cut and close gap (per clip)"),
             IconCode::Code::CUT
             ),
    UiAction("cut-per-track-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut and close gap (per track)"),
             TranslatableString("action", "Cut and close gap (per track)"),
             IconCode::Code::CUT
             ),
    UiAction("cut-all-tracks-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut and close gap (all tracks)"),
             TranslatableString("action", "Cut and close gap (all tracks)"),
             IconCode::Code::CUT
             ),
    UiAction("copy",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy"),
             IconCode::Code::COPY
             ),
    UiAction("delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "De&lete"),
             TranslatableString("action", "Delete"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("delete-per-track-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete and close gap (per track)"),
             TranslatableString("action", "Delete and close gap (per track)"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("delete-all-tracks-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete and close gap (all tracks)"),
             TranslatableString("action", "Delete and close gap (all tracks)"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("split",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split"),
             TranslatableString("action", "Split")
             ),
    UiAction("join",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Merge selected clips"),
             TranslatableString("action", "Merge selected clips")
             ),
    UiAction("disjoin",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split clips at silences"),
             TranslatableString("action", "Split clips at silences")
             ),
    UiAction("undo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Undo"),
             TranslatableString("action", "Undo"),
             IconCode::Code::UNDO
             ),
    UiAction("redo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Redo"),
             TranslatableString("action", "Redo"),
             IconCode::Code::REDO
             ),
    UiAction("duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate"),
             TranslatableString("action", "Duplicate")
             ),
    // track actions
    UiAction("track-rename",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Rename"),
             TranslatableString("action", "Rename track")
             ),
    UiAction("track-duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate"),
             TranslatableString("action", "Duplicate track")
             ),
    UiAction("track-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete"),
             TranslatableString("action", "Delete track")
             ),
    UiAction("track-move-up",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move track up"),
             TranslatableString("action", "Move track up")
             ),
    UiAction("track-move-down",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move track down"),
             TranslatableString("action", "Move track down")
             ),
    UiAction("track-move-top",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move track to top"),
             TranslatableString("action", "Move track to top")
             ),
    UiAction("track-move-bottom",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move track to bottom"),
             TranslatableString("action", "Move track to bottom")
             ),
    UiAction("track-change-rate-custom",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Other..."),
             TranslatableString("action", "Other..."),
             Checkable::Yes
             ),
    UiAction("track-make-stereo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Make stereo track"),
             TranslatableString("action", "Make stereo track")
             ),
    UiAction("track-swap-channels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Swap stereo channels"),
             TranslatableString("action", "Swap stereo channels")
             ),
    UiAction("track-split-stereo-to-lr",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split stereo to L/R mono"),
             TranslatableString("action", "Split stereo to L/R mono")
             ),
    UiAction("track-split-stereo-to-center",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split stereo to center mono"),
             TranslatableString("action", "Split stereo to center mono")
             ),
    UiAction("track-resample",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Resample track..."),
             TranslatableString("action", "Resample track...")
             ),
    UiAction("track-view-waveform",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Waveform"),
             TranslatableString("action", "Waveform"),
             Checkable::Yes
             ),
    UiAction("track-view-spectrogram",
             //! TODO: Change context when spectrogram is implemented
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Spectogram"),
             TranslatableString("action", "Spectogram"),
             Checkable::Yes
             ),
    UiAction("track-view-multi",
             //! TODO: Change context when multi-view is implemented
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Multi-view"),
             TranslatableString("action", "Multi-view"),
             Checkable::Yes
             ),
    UiAction("track-view-half-wave",
             //! TODO: Change context when half-wave is implemented
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Half-wave"),
             TranslatableString("action", "Half-wave"),
             Checkable::Yes
             ),
    UiAction("track-ruler-enable-meter",
             //! TODO: Change context when metering rulers are implemented
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Enable metering rulers"),
             TranslatableString("action", "Enable metering rulers")
             ),
    UiAction("track-ruler-enable-vertical",
             //!TODO: Change context when vertical rulers are implemented
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Enable vertical rulers"),
             TranslatableString("action", "Enable vertical rulers")
             ),
    UiAction("paste",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Paste"),
             TranslatableString("action", "Paste"),
             IconCode::Code::PASTE
             ),
    UiAction("paste-insert",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Paste (Insert)"),
             TranslatableString("action", "Paste (Insert)"),
             IconCode::Code::PASTE
             ),
    UiAction("paste-insert-all-tracks-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Insert and Preserve Sync"),
             TranslatableString("action", "Insert and Preserve Sync"),
             IconCode::Code::PASTE
             ),
    UiAction("track-split",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split"),
             TranslatableString("action", "Split")
             ),
    UiAction("track-split-at",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split at"),
             TranslatableString("action", "Split at")
             ),
    UiAction("merge-selected-on-tracks",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Merge selected clips"),
             TranslatableString("action", "Merge selected clips")
             ),
    UiAction("duplicate-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate selected"),
             TranslatableString("action", "Duplicate selected")
             ),
    UiAction("duplicate-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate clip"),
             TranslatableString("action", "Duplicate clip")
             ),
    UiAction("clip-export",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Export clip"),
             TranslatableString("action", "Export clip")
             ),
    UiAction("stretch-clip-to-match-tempo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Stretch with tempo changes"),
             TranslatableString("action", "Stretch with tempo changes"),
             Checkable::Yes
             ),
    UiAction("clip-pitch-speed-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Open pitch and speed dialog"),
             TranslatableString("action", "Open pitch and speed dialog")
             ),
    UiAction("clip-render-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Render pitch and speed"),
             TranslatableString("action", "Render pitch and speed")
             ),
    UiAction("new-mono-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "New mono track"),
             TranslatableString("action", "New mono track")
             ),
    UiAction("new-stereo-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "New stereo track"),
             TranslatableString("action", "New stereo track")
             ),
    UiAction("new-label-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "New label track"),
             TranslatableString("action", "New label track")
             ),
    UiAction("trim-audio-outside-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Trim"),
             TranslatableString("action", "Trim"),
             IconCode::Code::TRIM_AUDIO_OUTSIDE_SELECTION
             ),
    UiAction("silence-audio-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Silence"),
             TranslatableString("action", "Silence"),
             IconCode::Code::SILENCE_AUDIO_SELECTION
             ),
    UiAction("group-clips",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Group clips"),
             TranslatableString("action", "Group clips")
             ),
    UiAction("ungroup-clips",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Ungroup clips"),
             TranslatableString("action", "Ungroup clips")
             ),
};
}

TrackeditUiActions::TrackeditUiActions(std::shared_ptr<TrackeditActionsController> controller)
    : m_actions(STATIC_ACTIONS), m_controller(controller)
{
}

void TrackeditUiActions::init()
{
    for (const auto& formatInfo : availableTrackFormats()) {
        UiAction formatAction;
        formatAction.code
            = muse::actions::ActionQuery(muse::String(TRACK_FORMAT_CHANGE_ACTION).arg(muse::String::number(static_cast<int>(formatInfo.
                                                                                                                            format))))
              .toString();
        formatAction.uiCtx = context::UiCtxAny;
        formatAction.scCtx = context::CTX_ANY;
        formatAction.description = muse::TranslatableString("action", "Change track format");
        formatAction.title = muse::TranslatableString("action", "Change track format");
        formatAction.checkable = Checkable::Yes;
        m_actions.push_back(std::move(formatAction));
    }

    for (const auto& sampleRate : audioDevicesProvider()->availableSampleRateList()) {
        UiAction rateAction;
        rateAction.code
            = muse::actions::ActionQuery(muse::String(TRACK_RATE_CHANGE_ACTION).arg(muse::String::number(static_cast<int>(sampleRate)))).
              toString();
        rateAction.uiCtx = context::UiCtxAny;
        rateAction.scCtx = context::CTX_ANY;
        rateAction.description = muse::TranslatableString("action", "Change track sample rate");
        rateAction.title = muse::TranslatableString("action", "Change track sample rate");
        rateAction.checkable = Checkable::Yes;
        m_actions.push_back(std::move(rateAction));
    }

    m_controller->actionEnabledChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionEnabledChanged.send({ code });
    });

    m_controller->actionCheckedChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionCheckedChanged.send({ code });
    });
}

const UiActionList& TrackeditUiActions::actionsList() const
{
    return m_actions;
}

bool TrackeditUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool TrackeditUiActions::actionChecked(const UiAction& act) const
{
    return m_controller->actionChecked(act.code);
}

muse::async::Channel<ActionCodeList> TrackeditUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> TrackeditUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
