#include "projectuiactions.h"

#include "context/uicontext.h"
#include "modularity/ioc.h"
#include "types/translatablestring.h"
#include "context/shortcutcontext.h"

using namespace muse;
using namespace muse::ui;
using namespace au::project;

namespace {
const muse::actions::ActionCode UPDATE_AUDIO_PREVIEW_ACTION_CODE("audacity://cloud/update-audio-preview");
const muse::actions::ActionCode UPDATE_AUDIO_PREVIEW_FOR_PROJECT_ACTION_CODE("audacity://cloud/update-audio-preview-for-project");
}

const UiActionList ProjectUiActions::m_actions = {
    //! TODO AU4
    //! Here are all of app menu UiActions - not all of them belong here,
    //! it's temporary state.
    //! Remember to move particular UiActions to appropriate modules when implementing
    //! functionality described by a particular UiAction.

    // file menu
    UiAction("file-new",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&New…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "New…")
             ),
    UiAction("file-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Open…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Open…")
             ),
    UiAction("project-show-in-folder",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
#if defined(Q_OS_MAC)
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Show in Finder"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Show in Finder")
#elif defined(Q_OS_WIN)
             TranslatableString("action", "Show in Explorer"),
             TranslatableString("action", "Show in Explorer")
#else
             TranslatableString("action", "Show in File Manager"),
             TranslatableString("action", "Show in File Manager")
#endif
             ),
    UiAction("file-open-recent",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Open recent"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Open recent project")
             ),
    UiAction("audacity://cloud/open-audio-file",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Open"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Open")
             ),
    UiAction("cloud-file-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Open"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Open")
             ),
    UiAction("clear-recent",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Clear recent files"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Clear recent files")
             ),
    UiAction("project-import",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Import…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Import…")
             ),
    UiAction("file-save",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Save"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Save")
             ),
    UiAction("file-save-as",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Save &as…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Save as…")
             ),
    UiAction("export-audio",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Export audio…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Export audio…")
             ),
    UiAction("export-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Export labels"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Export labels")
             ),
    UiAction("export-midi",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Export MIDI"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Export MIDI")
             ),
    UiAction("file-close",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Close project"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Close project")
             ),

    // edit menu
    UiAction("duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Duplicate"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Duplicate")
             ),
    UiAction("insert",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Insert"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Insert")
             ),
    UiAction("trim-clip",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Trim clip"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Trim clip")
             ),
    UiAction("split-into-new-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Split into new track"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Split into new track")
             ),
    UiAction("paste-new-label",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Paste new label"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Paste new label")
             ),
    // select menu
    UiAction("select-all",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Select all"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Select all")
             ),
    UiAction("select-all-tracks",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Select all tracks"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Select all tracks")
             ),
    UiAction("select-left-of-playback-position",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Left of playback position"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Left of playback position")
             ),
    UiAction("select-right-of-playback-position",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Right of playback position"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Right of playback position")
             ),
    UiAction("select-track-start-to-cursor",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Track start to cursor"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Track start to cursor")
             ),
    UiAction("select-cursor-to-track-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cursor to track end"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cursor to track end")
             ),
    UiAction("select-track-start-to-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Track start to end"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Track start to end")
             ),
    UiAction("select-previous-clip-boundary-to-cursor",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Previous clip boundary to cursor"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Previous clip boundary to cursor")
             ),
    UiAction("select-cursor-to-next-clip-boundary",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Cursor to next clip boundary"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Cursor to next clip boundary")
             ),
    UiAction("select-previous-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Previous clip"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Previous clip")
             ),
    UiAction("select-next-clip",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Next clip"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Next clip")
             ),
    UiAction("toggle-spectral-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Toggle spectral selection"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Toggle spectral selection")
             ),
    UiAction("cursor-to-track-start",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Track Start"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move cursor to track start")
             ),
    UiAction("cursor-to-track-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Track End"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move cursor to track end")
             ),

    UiAction("zero-cross",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Closest zero crossing"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Move cursor to closest zero crossing")
             ),

    // view menu (zoom actions are in projectsceneuiactions.cpp)
    UiAction("collapse-all-tracks",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Collapse all tracks"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Collapse all tracks")
             ),
    UiAction("expand-all-tracks",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Expand all tracks"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Expand all tracks")
             ),
    UiAction("skip-to-selection-start",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Skip to selection start"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Skip to selection start")
             ),
    UiAction("skip-to-selection-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Skip to selection end"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Skip to selection end")
             ),
    UiAction("toggle-effects",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Show effects panel"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Show effects panel"),
             Checkable::Yes
             ),
    UiAction("open-metadata-editor",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Show metadata editor"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Show metadata editor")
             ),
    UiAction("toggle-history",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Show history"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Show history"),
             Checkable::Yes
             ),

    // record menu
    UiAction("set-up-timed-recording",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Set up timed recording"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Set up timed recording")
             ),
    UiAction("toggle-sound-activated-recording",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Enable sound activating recording"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Enable sound activating recording")
             ),
    UiAction("set-sound-activation-level",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Set sound activation level"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Set sound activation level")
             ),

    // tracks menu
    UiAction("duplicate-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Duplicate"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Duplicate")
             ),
    UiAction("remove-tracks",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Remove tracks"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Remove tracks")
             ),
    UiAction("mixdown-to",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Mix-down to…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Mix-down to…")
             ),
    UiAction("align-end-to-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align end to end"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align end to end")
             ),
    UiAction("align-together",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align together"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align together")
             ),
    UiAction("align-start-to-zero",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align start to zero"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align start to zero")
             ),
    UiAction("align-start-to-playhead",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align start to playhead"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align start to playhead")
             ),
    UiAction("align-start-to-selection-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align start to selection end"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align start to selection end")
             ),
    UiAction("align-end-to-playhead",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align end to playhead"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align end to playhead")
             ),
    UiAction("align-end-to-selection-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Align end to selection end"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Align end to selection end")
             ),
    UiAction("sort-by-time",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Sort by time"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Sort by time")
             ),
    UiAction("sort-by-name",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Sort by name"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Sort by name")
             ),
    UiAction("keep-tracks-synchronised",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Keep tracks synchronized"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Keep tracks synchronized")
             ),

    UiAction("plugin-manager",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Plugin manager"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Plugin manager")
             ),

    // effects menu
    UiAction("add-realtime-effects",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Add track effects"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Add track effects")
             ),
    UiAction("favourite-effect-1",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Fav effect #1"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Fav effect #1")
             ),
    UiAction("favourite-effect-2",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Fav effect #2"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Fav effect #2")
             ),
    UiAction("favourite-effect-3",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Fav effect #3"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Fav effect #3")
             ),

    // analyze menu
    UiAction("contrast-analyzer",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Contrast analyzer"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Contrast analyzer")
             ),
    UiAction("plot-spectrum",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Plot spectrum"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Plot spectrum")
             ),

    // tools menu
    UiAction("manage-macros",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Manage macros"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Manage macros")
             ),
    UiAction("apply-macros-palette",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Apply macros palette"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Apply macros palette")
             ),
    UiAction("macro-fade-ends",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Macro fade ends"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Macro fade ends")
             ),
    UiAction("macro-mp3-conversion",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Macro MP3 conversion"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Macro MP3 conversion")
             ),
    UiAction("nyquist-plugin-installer",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Nyquist plugin installer"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Nyquist plugin installer")
             ),
    UiAction("nyquist-prompt",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Nyquist prompt"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Nyquist prompt")
             ),
    UiAction("sample-data-export",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Sample data export"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Sample data export")
             ),
    UiAction("sample-data-import",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Sample data import"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Sample data import")
             ),
    UiAction("raw-data-import",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Raw data import"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Raw data import")
             ),
    UiAction("reset-configuration",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Reset configuration"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Reset configuration")
             ),

    // extra menu
    UiAction("prev-window",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Previous window"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Previous window")
             ),
    UiAction("next-window",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Next window"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Next window")
             ),
    UiAction("benchmark",
             au::context::UiCtxUnknown,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Benchmark"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Benchmark")
             ),
    UiAction("regular-interval-labels",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Regular interval labels"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Regular interval labels")
             ),

    // help menu
    UiAction("tutorials",
             au::context::UiCtxUnknown,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Tutorials"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Tutorials")
             ),
    UiAction("device-info",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Device info"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Device info")
             ),
    UiAction("midi-device-info",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "MIDI device info"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "MIDI device info")
             ),
    UiAction("log",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Log"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Log")
             ),
    UiAction("crash-report",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Crash report"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Crash report")
             ),
    UiAction("raise-segfault",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Raise segfault"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Raise segfault")
             ),
    UiAction("throw-exception",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Throw exception"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Throw exception")
             ),
    UiAction("violate-assertion",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Violate assertion"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Violate assertion")
             ),
    UiAction("menu-tree",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Menu tree"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Menu tree")
             ),
    UiAction("frame-statistics",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Frame statistics"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Frame statistics")
             ),
    UiAction("link-account",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Link account"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Link account")
             ),
    UiAction("file-save-to-cloud",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Save to clo&ud…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Save to cloud…"),
             IconCode::Code::CLOUD_FILE
             ),
    UiAction("file-share-audio",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Share audio"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Share audio"),
             IconCode::Code::SHARE_AUDIO
             ),
    UiAction(UPDATE_AUDIO_PREVIEW_ACTION_CODE,
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Update cloud audio preview"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Update cloud audio preview")
             ),
    UiAction(UPDATE_AUDIO_PREVIEW_FOR_PROJECT_ACTION_CODE,
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Update audio preview"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Update audio preview")
             ),

    UiAction("project-properties",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Project propert&ies…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Project properties…")
             )
};

ProjectUiActions::ProjectUiActions(const std::shared_ptr<ProjectActionsController>& controller)
    : muse::Contextable(controller->iocContext()), m_controller(controller)
{
}

void ProjectUiActions::init()
{
    m_controller->actionEnabledChanged().onReceive(this, [this](const muse::actions::ActionCodeList& codes) {
        m_actionEnabledChanged.send(codes);
    });
}

const UiActionList& ProjectUiActions::actionsList() const
{
    return m_actions;
}

bool ProjectUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool ProjectUiActions::actionChecked(const UiAction&) const
{
    return false;
}

muse::async::Channel<muse::actions::ActionCodeList> ProjectUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<muse::actions::ActionCodeList> ProjectUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
