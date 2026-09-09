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
    UiAction("rename-item",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Rename item (clip/label)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Rename item (clip/label)")
             ),
    UiAction("action://trackedit/copy",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Copy"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Copy"),
             IconCode::Code::COPY
             ),
    UiAction("action://trackedit/cut",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cut"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cut"),
             IconCode::Code::CUT
             ),
    UiAction("action://trackedit/undo",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Undo"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Undo"),
             IconCode::Code::UNDO
             ),
    UiAction("action://trackedit/redo",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Redo"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Redo"),
             IconCode::Code::REDO
             ),
    UiAction("action://trackedit/delete",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "De&lete"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Delete"),
             IconCode::Code::DELETE_TANK
             ),

    UiAction("select-all",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Select all"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Select all")
             ),
    UiAction("clear-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Clear selection"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Clear selection")
             ),
    UiAction("cut-leave-gap",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cut and leave gap"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cut and leave gap"),
             IconCode::Code::CUT
             ),
    UiAction("cut-per-clip-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cut and close gap (per clip)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cut and close gap (per clip)"),
             IconCode::Code::CUT
             ),
    UiAction("cut-per-track-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cut and close gap (per track)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cut and close gap (per track)"),
             IconCode::Code::CUT
             ),
    UiAction("cut-all-tracks-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cut and close gap (all tracks)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cut and close gap (all tracks)"),
             IconCode::Code::CUT
             ),
    UiAction("delete-leave-gap",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Delete and leave gap"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Delete and leave gap"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("delete-per-clip-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Delete and close gap (per clip)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Delete and close gap (per clip)"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("delete-per-track-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Delete and close gap (per track)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Delete and close gap (per track)"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("delete-all-tracks-ripple",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Delete and close gap (all tracks)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Delete and close gap (all tracks)"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("split",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Split"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Split"),
             IconCode::Code::SPLIT_TOOL
             ),
    UiAction("join",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Join selected clips"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Join selected clips")
             ),
    UiAction("disjoin",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Split clips at silences"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Split clips at silences")
             ),
    UiAction("duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Duplicate"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Duplicate")
             ),
    // track actions
    UiAction("track-rename",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Rename"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Rename track")
             ),
    UiAction("track-duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Duplicate"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Duplicate track")
             ),
    UiAction("track-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Delete"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Delete track")
             ),
    UiAction("track-move-up",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move track up"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move track up")
             ),
    UiAction("track-move-down",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move track down"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move track down")
             ),
    UiAction("track-move-top",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move track to top"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move track to top")
             ),
    UiAction("track-move-bottom",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move track to bottom"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move track to bottom")
             ),
    UiAction("track-change-rate-custom",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Other…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Other…"),
             Checkable::Yes
             ),
    UiAction("track-make-stereo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Make stereo track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Make stereo track")
             ),
    UiAction("track-swap-channels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Swap stereo channels"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Swap stereo channels")
             ),
    UiAction("track-split-stereo-to-lr",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Split stereo to L/R mono"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Split stereo to L/R mono")
             ),
    UiAction("track-split-stereo-to-center",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Split stereo to center mono"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Split stereo to center mono")
             ),
    UiAction("track-resample",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Resample track…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Resample track…")
             ),
    UiAction("action://trackedit/track-view-waveform",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Waveform"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Waveform"),
             IconCode::Code::WAVEFORM,
             Checkable::Yes
             ),
    UiAction("action://trackedit/track-view-spectrogram",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Spectrogram"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Spectrogram"),
             IconCode::Code::SPECTROGRAM,
             Checkable::Yes
             ),
    UiAction("action://trackedit/track-view-multi",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Multi-view"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Multi-view"),
             IconCode::Code::WAVEFORM_MULTIVIEW,
             Checkable::Yes
             ),
    UiAction("action://trackedit/paste-default",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Paste"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Paste"),
             IconCode::Code::PASTE
             ),
    UiAction("action://trackedit/paste-insert",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Paste (pushes clips on selected track)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Paste (pushes clips on selected track)"),
             IconCode::Code::PASTE
             ),
    UiAction("action://trackedit/paste-overlap",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Paste (overlaps other clips)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Paste (overlaps other clips)"),
             IconCode::Code::PASTE
             ),
    UiAction("action://trackedit/paste-insert-all-tracks-ripple",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Paste (preserves synchronization on all tracks)"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Paste (preserves synchronization on all tracks)"),
             IconCode::Code::PASTE
             ),
    UiAction("merge-selected-on-tracks",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Merge selected clips"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Merge selected clips")
             ),
    UiAction("duplicate-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Duplicate selected"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Duplicate selected")
             ),
    UiAction("duplicate-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Duplicate clip"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Duplicate clip")
             ),
    UiAction("clip-export",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Export clip"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Export clip")
             ),
    UiAction("stretch-clip-to-match-tempo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Stretch with tempo changes"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Stretch with tempo changes"),
             Checkable::Yes
             ),
    UiAction("clip-pitch-speed-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Open pitch and speed dialog"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Open pitch and speed dialog")
             ),
    UiAction("clip-render-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Render pitch and speed"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Render pitch and speed")
             ),
    UiAction("clip-reset-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Reset pitch and speed"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Reset pitch and speed")
             ),
    UiAction("new-mono-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "New mono track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "New mono track")
             ),
    UiAction("new-stereo-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "New stereo track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "New stereo track")
             ),
    UiAction("new-label-track",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "New label track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "New label track")
             ),
    UiAction("label-add",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Add label"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Add label")
             ),
    UiAction("trim-audio-outside-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Trim"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Trim"),
             IconCode::Code::TRIM_AUDIO_OUTSIDE_SELECTION
             ),
    UiAction("silence-audio-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Silence"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Silence"),
             IconCode::Code::SILENCE_AUDIO_SELECTION
             ),
    UiAction("group-clips",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Group clips"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Group clips")
             ),
    UiAction("ungroup-clips",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Ungroup clips"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Ungroup clips")
             ),
    UiAction("track-view-item-move-left",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move item left"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move item left")
             ),
    UiAction("track-view-item-move-right",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move item right"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move item right")
             ),
    UiAction("track-view-item-extend-left",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Extend item left"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Extend item left")
             ),
    UiAction("track-view-item-extend-right",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Extend item right"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Extend item right")
             ),
    UiAction("track-view-item-reduce-left",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Reduce item left"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Reduce item left")
             ),
    UiAction("track-view-item-reduce-right",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Reduce item right"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Reduce item right")
             ),
    UiAction("track-view-item-move-up",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move item up"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move item up")
             ),
    UiAction("track-view-item-move-down",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Move item down"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move item down")
             ),

    // track view navigation
    UiAction("track-view-next-panel",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Next panel"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Next panel")
             ),
    UiAction("track-view-prev-panel",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Previous panel"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Previous panel")
             ),

    UiAction("track-view-above-item",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Above item"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Above item")
             ),
    UiAction("track-view-below-item",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Below item"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Below item")
             ),

    UiAction("track-view-first-track",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "First track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "First track")
             ),
    UiAction("track-view-last-track",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Last track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Last track")
             ),

    UiAction("track-view-replace-selection",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Select track/track item"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Select track/track item")
             ),
    UiAction("track-view-toggle-selection",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Add track or track item to selection"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Add track or track item to selection")
             ),
    UiAction("track-view-range-selection",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Track range selection"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Track range selection")
             ),
    UiAction("track-view-extend-track-selection-prev",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Multi track selection previous"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Multi track selection previous")
             ),
    UiAction("track-view-extend-track-selection-next",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Multi track selection next"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Multi track selection next")
             ),

    UiAction("track-view-item-context-menu",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Open item’s context menu"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Open item’s context menu")
             )
};
}

TrackeditUiActions::TrackeditUiActions(const muse::modularity::ContextPtr& ctx, std::shared_ptr<TrackeditActionsController> controller)
    : muse::Contextable(ctx), m_actions(STATIC_ACTIONS), m_controller(controller)
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
        formatAction.scCtx = context::CTX_DISABLED;
        formatAction.description = muse::TranslatableString("action_description", "Change track format");
        formatAction.title = muse::TranslatableString("action", "Change track format");
        formatAction.checkable = Checkable::Yes;
        m_actions.push_back(std::move(formatAction));
    }

    for (const auto& sampleRate : audioDriverController()->sampleRates()) {
        UiAction rateAction;
        rateAction.code
            = muse::actions::ActionQuery(muse::String(TRACK_RATE_CHANGE_ACTION).arg(muse::String::number(static_cast<int>(sampleRate)))).
              toString();
        rateAction.uiCtx = context::UiCtxAny;
        rateAction.scCtx = context::CTX_DISABLED;
        rateAction.description = muse::TranslatableString("action_description", "Change track sample rate");
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
