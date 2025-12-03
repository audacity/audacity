#include "projectuiactions.h"

#include "context/uicontext.h"
#include "types/translatablestring.h"
#include "context/shortcutcontext.h"

using namespace muse;
using namespace muse::ui;
using namespace au::project;

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
             TranslatableString("action", "&New…"),
             TranslatableString("action", "New…")
             ),
    UiAction("file-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Open…"),
             TranslatableString("action", "Open…")
             ),
    UiAction("clear-recent",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Clear recent files"),
             TranslatableString("action", "Clear recent files")
             ),
    UiAction("project-import",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Import"),
             TranslatableString("action", "Import")
             ),
    UiAction("file-save",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Save project"),
             TranslatableString("action", "Save project")
             ),
    UiAction("file-save-as",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Save &as…"),
             TranslatableString("action", "Save as…")
             ),
    UiAction("file-save-backup",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Save backup"),
             TranslatableString("action", "Save backup")
             ),
    UiAction("export-audio",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Export audio"),
             TranslatableString("action", "Export audio")
             ),
    UiAction("export-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Export labels"),
             TranslatableString("action", "Export labels")
             ),
    UiAction("export-midi",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Export MIDI"),
             TranslatableString("action", "Export MIDI")
             ),
    UiAction("file-close",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Close project"),
             TranslatableString("action", "Close project")
             ),

    // edit menu
    UiAction("duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate"),
             TranslatableString("action", "Duplicate")
             ),
    UiAction("insert",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Insert"),
             TranslatableString("action", "Insert")
             ),
    UiAction("rename-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Rename clip"),
             TranslatableString("action", "Rename clip")
             ),
    UiAction("trim-clip",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Trim clip"),
             TranslatableString("action", "Trim clip")
             ),
    UiAction("split-into-new-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split into new track"),
             TranslatableString("action", "Split into new track")
             ),
    UiAction("silence-audio",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Silence audio"),
             TranslatableString("action", "Silence audio")
             ),
    UiAction("paste-new-label",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Paste new label"),
             TranslatableString("action", "Paste new label")
             ),
    UiAction("cut-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut labels"),
             TranslatableString("action", "Cut labels")
             ),
    UiAction("split-cut-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut labels and leave gap"),
             TranslatableString("action", "Cut labels and leave gap")
             ),
    UiAction("copy-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy labels"),
             TranslatableString("action", "Copy labels")
             ),
    UiAction("delete-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete labels"),
             TranslatableString("action", "Delete labels")
             ),
    UiAction("split-delete-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete labels and leave gap"),
             TranslatableString("action", "Delete labels and leave gap")
             ),
    UiAction("split-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split labels"),
             TranslatableString("action", "Split labels")
             ),
    UiAction("join-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Merge labels"),
             TranslatableString("action", "Merge labels")
             ),
    UiAction("silence-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Silence labels"),
             TranslatableString("action", "Silence labels")
             ),
    UiAction("disjoin-labels",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Disjoin labels"),
             TranslatableString("action", "Disjoin labels")
             ),
    UiAction("manage-labels",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Manage labels"),
             TranslatableString("action", "Manage labels")
             ),
    UiAction("manage-metadata",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Metadata editor"),
             TranslatableString("action", "Metadata editor")
             ),

    // select menu
    UiAction("select-all",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Select all"),
             TranslatableString("action", "Select all")
             ),
    UiAction("select-all-tracks",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Select all tracks"),
             TranslatableString("action", "Select all tracks")
             ),
    UiAction("select-left-of-playback-position",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Left of playback position"),
             TranslatableString("action", "Left of playback position")
             ),
    UiAction("select-right-of-playback-position",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Right of playback position"),
             TranslatableString("action", "Right of playback position")
             ),
    UiAction("select-track-start-to-cursor",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Track start to cursor"),
             TranslatableString("action", "Track start to cursor")
             ),
    UiAction("select-cursor-to-track-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cursor to track end"),
             TranslatableString("action", "Cursor to track end")
             ),
    UiAction("select-track-start-to-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Track start to end"),
             TranslatableString("action", "Track start to end")
             ),
    UiAction("select-previous-clip-boundary-to-cursor",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Previous clip boundary to cursor"),
             TranslatableString("action", "Previous clip boundary to cursor")
             ),
    UiAction("select-cursor-to-next-clip-boundary",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cursor to next clip boundary"),
             TranslatableString("action", "Cursor to next clip boundary")
             ),
    UiAction("select-previous-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Previous clip"),
             TranslatableString("action", "Previous clip")
             ),
    UiAction("select-next-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Next clip"),
             TranslatableString("action", "Next clip")
             ),
    UiAction("toggle-spectral-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Toggle spectral selection"),
             TranslatableString("action", "Toggle spectral selection")
             ),
    UiAction("zero-cross",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move cursor to closest zero crossing"),
             TranslatableString("action", "Move cursor to closest zero crossing")
             ),

    // view menu
    UiAction("zoom-in",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom in"),
             TranslatableString("action", "Zoom in")
             ),
    UiAction("zoom-out",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom out"),
             TranslatableString("action", "Zoom out")
             ),
    UiAction("zoom-to-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom to selection"),
             TranslatableString("action", "Zoom to selection"),
             IconCode::Code::FIT_SELECTION
             ),
    UiAction("zoom-toggle",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom toggle"),
             TranslatableString("action", "Zoom toggle")
             ),
    UiAction("zoom-reset",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom reset"),
             TranslatableString("action", "Zoom reset")
             ),
    UiAction("fit-project-to-window",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Fit project to window"),
             TranslatableString("action", "Fit project to window")
             ),
    UiAction("fit-view-to-project",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Fit view to project"),
             TranslatableString("action", "Fit view to project")
             ),
    UiAction("collapse-all-tracks",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Collapse all tracks"),
             TranslatableString("action", "Collapse all tracks")
             ),
    UiAction("expand-all-tracks",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Expand all tracks"),
             TranslatableString("action", "Expand all tracks")
             ),
    UiAction("skip-to-selection-start",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Skip to selection start"),
             TranslatableString("action", "Skip to selection start")
             ),
    UiAction("skip-to-selection-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Skip to selection end"),
             TranslatableString("action", "Skip to selection end")
             ),
    UiAction("toggle-effects",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Show effects"),
             TranslatableString("action", "Show effects"),
             Checkable::Yes
             ),
    UiAction("toggle-metadata-editor",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Show metadata editor"),
             TranslatableString("action", "Show metadata editor"),
             Checkable::Yes
             ),
    UiAction("toggle-history",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Show history"),
             TranslatableString("action", "Show history"),
             Checkable::Yes
             ),

    // record menu
    UiAction("record-on-current-track",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Record on current track"),
             TranslatableString("action", "Record on current track")
             ),
    UiAction("record-on-new-track",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Record on new track"),
             TranslatableString("action", "Record on new track")
             ),
    UiAction("set-up-timed-recording",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Set up timed recording"),
             TranslatableString("action", "Set up timed recording")
             ),
    UiAction("punch-and-roll-record",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Punch and roll record"),
             TranslatableString("action", "Punch and roll record")
             ),
    UiAction("toggle-sound-activated-recording",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Enable sound activating recording"),
             TranslatableString("action", "Enable sound activating recording")
             ),
    UiAction("set-sound-activation-level",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Set sound activation level"),
             TranslatableString("action", "Set sound activation level")
             ),

    // tracks menu
    UiAction("duplicate-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate"),
             TranslatableString("action", "Duplicate")
             ),
    UiAction("remove-tracks",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Remove tracks"),
             TranslatableString("action", "Remove tracks")
             ),
    UiAction("mixdown-to",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Mix-down to…"),
             TranslatableString("action", "Mix-down to…")
             ),
    UiAction("align-end-to-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align end to end"),
             TranslatableString("action", "Align end to end")
             ),
    UiAction("align-together",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align together"),
             TranslatableString("action", "Align together")
             ),
    UiAction("align-start-to-zero",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align start to zero"),
             TranslatableString("action", "Align start to zero")
             ),
    UiAction("align-start-to-playhead",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align start to playhead"),
             TranslatableString("action", "Align start to playhead")
             ),
    UiAction("align-start-to-selection-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align start to selection end"),
             TranslatableString("action", "Align start to selection end")
             ),
    UiAction("align-end-to-playhead",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align end to playhead"),
             TranslatableString("action", "Align end to playhead")
             ),
    UiAction("align-end-to-selection-end",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Align end to selection end"),
             TranslatableString("action", "Align end to selection end")
             ),
    UiAction("sort-by-time",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Sort by time"),
             TranslatableString("action", "Sort by time")
             ),
    UiAction("sort-by-name",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Sort by name"),
             TranslatableString("action", "Sort by name")
             ),
    UiAction("keep-tracks-synchronised",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Keep tracks synchronised"),
             TranslatableString("action", "Keep tracks synchronised")
             ),

    // generate menu
    UiAction("generate-plugin-manager",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Plugin manager"),
             TranslatableString("action", "Plugin manager")
             ),
    UiAction("generate-omitted",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "(Omitted)"),
             TranslatableString("action", "(Omitted)")
             ),

    // effects menu
    UiAction("effect-plugin-manager",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Plugin manager"),
             TranslatableString("action", "Plugin manager")
             ),
    UiAction("add-realtime-effects",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Add realtime effects"),
             TranslatableString("action", "Add realtime effects")
             ),
    UiAction("favourite-effect-1",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Fav effect #1"),
             TranslatableString("action", "Fav effect #1")
             ),
    UiAction("favourite-effect-2",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Fav effect #2"),
             TranslatableString("action", "Fav effect #2")
             ),
    UiAction("favourite-effect-3",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Fav effect #3"),
             TranslatableString("action", "Fav effect #3")
             ),
    UiAction("plugins-omitted",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "(Plugins omitted)"),
             TranslatableString("action", "(Plugins omitted)")
             ),

    // analyze menu
    UiAction("analyze-plugin-manager",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Plugin manager"),
             TranslatableString("action", "Plugin manager")
             ),
    UiAction("contrast-analyzer",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Contrast analyzer"),
             TranslatableString("action", "Contrast analyzer")
             ),
    UiAction("plot-spectrum",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Plot spectrum"),
             TranslatableString("action", "Plot spectrum")
             ),
    UiAction("analyzer-omitted",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "(Omitted)"),
             TranslatableString("action", "(Omitted)")
             ),

    // tools menu
    UiAction("tools-plugin-manager",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Plugin manager"),
             TranslatableString("action", "Plugin manager")
             ),
    UiAction("manage-macros",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Manage macros"),
             TranslatableString("action", "Manage macros")
             ),
    UiAction("apply-macros-palette",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Apply macros palette"),
             TranslatableString("action", "Apply macros palette")
             ),
    UiAction("macro-fade-ends",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Macro fade ends"),
             TranslatableString("action", "Macro fade ends")
             ),
    UiAction("macro-mp3-conversion",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Macro MP3 conversion"),
             TranslatableString("action", "Macro MP3 conversion")
             ),
    UiAction("nyquist-plugin-installer",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Nyquist plugin installer"),
             TranslatableString("action", "Nyquist plugin installer")
             ),
    UiAction("nyquist-prompt",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Nyquist prompt"),
             TranslatableString("action", "Nyquist prompt")
             ),
    UiAction("sample-data-export",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Sample data export"),
             TranslatableString("action", "Sample data export")
             ),
    UiAction("sample-data-import",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Sample data import"),
             TranslatableString("action", "Sample data import")
             ),
    UiAction("raw-data-import",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Raw data import"),
             TranslatableString("action", "Raw data import")
             ),
    UiAction("reset-configuration",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Reset configuration"),
             TranslatableString("action", "Reset configuration")
             ),

    // extra menu
    UiAction("prev-window",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Previous window"),
             TranslatableString("action", "Previous window")
             ),
    UiAction("next-window",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Next window"),
             TranslatableString("action", "Next window")
             ),
    UiAction("benchmark",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Benchmark"),
             TranslatableString("action", "Benchmark")
             ),
    UiAction("regular-interval-labels",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Regular interval labels"),
             TranslatableString("action", "Regular interval labels")
             ),

    // help menu
    UiAction("tutorials",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Tutorials"),
             TranslatableString("action", "Tutorials")
             ),
    UiAction("device-info",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Device info"),
             TranslatableString("action", "Device info")
             ),
    UiAction("midi-device-info",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "MIDI device info"),
             TranslatableString("action", "MIDI device info")
             ),
    UiAction("log",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Log"),
             TranslatableString("action", "Log")
             ),
    UiAction("crash-report",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Crash report"),
             TranslatableString("action", "Crash report")
             ),
    UiAction("raise-segfault",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Raise segfault"),
             TranslatableString("action", "Raise segfault")
             ),
    UiAction("throw-exception",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Throw exception"),
             TranslatableString("action", "Throw exception")
             ),
    UiAction("violate-assertion",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Violate assertion"),
             TranslatableString("action", "Violate assertion")
             ),
    UiAction("menu-tree",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Menu tree"),
             TranslatableString("action", "Menu tree")
             ),
    UiAction("frame-statistics",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Frame statistics"),
             TranslatableString("action", "Frame statistics")
             ),
    UiAction("link-account",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Link account"),
             TranslatableString("action", "Link account")
             ),
    UiAction("updates",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Updates"),
             TranslatableString("action", "Updates")
             ),
    UiAction("about-audacity",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "About Audacity"),
             TranslatableString("action", "About Audacity")
             ),

    UiAction("file-save-to-cloud",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Save to clo&ud…"),
             TranslatableString("action", "Save to cloud…"),
             IconCode::Code::CLOUD_FILE
             ),
    UiAction("file-share-audio",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Share on Audio.com…"),
             TranslatableString("action", "Share on Audio.com…"),
             IconCode::Code::SHARE_AUDIO
             ),

    UiAction("project-properties",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Project propert&ies…"),
             TranslatableString("action", "Project properties…")
             )
};

ProjectUiActions::ProjectUiActions(const std::shared_ptr<ProjectActionsController>& controller)
    : m_controller(controller)
{
}

void ProjectUiActions::init()
{
    recordController()->isRecordingChanged().onNotify(this, [this]() {
        m_actionEnabledChanged.send(m_controller->prohibitedActionsWhileRecording());
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
