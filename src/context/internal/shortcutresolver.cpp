/*
* Audacity: A Digital Audio Editor
*/
#include "shortcutresolver.h"

#include <algorithm>
#include <map>
#include <set>
#include <utility>

#include "framework/global/stringutils.h"
#include "framework/global/log.h"

using namespace au::context;
using namespace muse;
using namespace muse::shortcuts;

static const std::string NAVIGATION_SCOPE = "NAVIGATION";
static const std::string APP_SCOPE = "APP";
static const std::string PLAYBACK_SCOPE = "PLAYBACK";
static const std::string RECORD_SCOPE = "RECORD";
static const std::string TIMELINE_SCOPE = "TIMELINE";
static const std::string TRACK_NAVIGATION_SECTION_SCOPE = "TRACK_NAVIGATION_SECTION";
static const std::string PROJECT_SCOPE = "PROJECT";
static const std::string PROJECTSCENE_SCOPE = "PROJECTSCENE";
static const std::string TRACKEDIT_SCOPE = "TRACKEDIT";
static const std::string TRACK_NAVIGATION_SCOPE = "TRACK-NAVIGATION";
static const std::string EFFECTS_SCOPE = "EFFECTS";
static const std::string SPECTROGRAM_SCOPE = "SPECTROGRAM";
static const std::string CLOUD_SCOPE = "CLOUD";
static const std::string AUDIO_SCOPE = "AUDIO";
static const std::string VST_SCOPE = "VST";
static const std::string DOCK_SCOPE = "DOCK";
static const std::string WORKSPACE_SCOPE = "WORKSPACE";
static const std::string EXTENSIONS_SCOPE = "EXTENSIONS";
static const std::string SHORTCUTS_SCOPE = "SHORTCUTS";
static const std::string MULTIWINDOWS_SCOPE = "MULTIWINDOWS";
static const std::string UPDATE_SCOPE = "UPDATE";
static const std::string DIAGNOSTICS_SCOPE = "DIAGNOSTICS";
static const std::string TESTFLOW_SCOPE = "TESTFLOW";

static const QString TRACK_VIEW_SECTION_NAME("TrackViewSection");

//! NOTE The priority is used only when one key sequence matches enabled shortcuts in different scopes,
//! so only the scopes that clash with another scope are ranked here; all other scopes get 0.
int ShortcutResolver::scopePriority(const std::string& scope) const
{
    if (scope == NAVIGATION_SCOPE) {
        return 1;
    }

    static const std::set<std::string> PROJECT_PAGE_SCOPES = {
        TRACKEDIT_SCOPE, PLAYBACK_SCOPE, RECORD_SCOPE, PROJECTSCENE_SCOPE, EFFECTS_SCOPE, SPECTROGRAM_SCOPE, TRACK_NAVIGATION_SCOPE
    };
    if (PROJECT_PAGE_SCOPES.contains(scope) && !isProjectPageShown()) {
        return -1;
    }

    if (scope == PLAYBACK_SCOPE || scope == RECORD_SCOPE) {
        return 2;
    }

    if (scope == TIMELINE_SCOPE) {
        return isProjectFocused() ? 3 : -1;
    }

    if (scope == TRACK_NAVIGATION_SECTION_SCOPE) {
        return isTrackViewFocused() ? 4 : -1;
    }

    return 0;
}

bool ShortcutResolver::isProjectPageShown() const
{
    return uiContextResolver()->matchWithCurrent(UiCtxProjectOpened);
}

bool ShortcutResolver::isProjectFocused() const
{
    return uiContextResolver()->matchWithCurrent(UiCtxProjectFocused);
}

bool ShortcutResolver::isTrackViewFocused() const
{
    const muse::ui::INavigationSection* section = navigationController()->activeSection();
    return section && section->name() == TRACK_VIEW_SECTION_NAME && isProjectFocused();
}

Shortcut ShortcutResolver::selectActive(const ShortcutList& list) const
{
    const auto rank = [this](const Shortcut& sc) {
        return std::pair { scopePriority(sc.scope), sc.command != rcommand::NONE_COMMAND };
    };

    const auto active = std::ranges::max_element(list, std::less<> {}, rank);
    if (active == list.end() || rank(*active).first < 0) {
        return Shortcut();
    }

    return *active;
}

TranslatableString ShortcutResolver::scopeTitle(const std::string& scopeCode) const
{
    static const std::map<std::string, TranslatableString> scopeTitles = {
        //: Group heading in Preferences > Shortcuts for shortcuts that move the keyboard focus between panels and controls
        { NAVIGATION_SCOPE, TranslatableString("shortcuts", "Navigation") },
        //: Group heading in Preferences > Shortcuts for shortcuts that act on the whole application
        { APP_SCOPE, TranslatableString("shortcuts", "App") },
        //: Group heading in Preferences > Shortcuts for shortcuts that control playback
        { PLAYBACK_SCOPE, TranslatableString("shortcuts", "Playback") },
        //: Group heading in Preferences > Shortcuts for shortcuts that control recording
        { RECORD_SCOPE, TranslatableString("shortcuts", "Recording") },
        //: Group heading in Preferences > Shortcuts for shortcuts that work only while the timeline has the keyboard focus
        { TIMELINE_SCOPE, TranslatableString("shortcuts", "Timeline") },
        //: Group heading in Preferences > Shortcuts for shortcuts that work only while a track or a track item has the keyboard focus
        { TRACK_NAVIGATION_SECTION_SCOPE, TranslatableString("shortcuts", "Track view") },
        //: Group heading in Preferences > Shortcuts for shortcuts that manage the project file
        { PROJECT_SCOPE, TranslatableString("shortcuts", "Project") },
        //: Group heading in Preferences > Shortcuts for shortcuts that change how the project is shown
        { PROJECTSCENE_SCOPE, TranslatableString("shortcuts", "Project view") },
        //: Group heading in Preferences > Shortcuts for shortcuts that edit audio, clips, labels and tracks
        { TRACKEDIT_SCOPE, TranslatableString("shortcuts", "Editing") },
        //: Group heading in Preferences > Shortcuts for shortcuts that move the keyboard focus between tracks and track items
        { TRACK_NAVIGATION_SCOPE, TranslatableString("shortcuts", "Track navigation") },
        //: Group heading in Preferences > Shortcuts for shortcuts that open or apply effects
        { EFFECTS_SCOPE, TranslatableString("shortcuts", "Effects") },
        //: Group heading in Preferences > Shortcuts for shortcuts of the spectrogram view
        { SPECTROGRAM_SCOPE, TranslatableString("shortcuts", "Spectrogram") },
        //: Group heading in Preferences > Shortcuts for shortcuts of the cloud service
        { CLOUD_SCOPE, TranslatableString("shortcuts", "Cloud") },
        //: Group heading in Preferences > Shortcuts for shortcuts of the audio engine and audio devices
        { AUDIO_SCOPE, TranslatableString("shortcuts", "Audio") },
        //: Group heading in Preferences > Shortcuts for shortcuts of VST plugins; "VST" is a product name, do not translate it
        { VST_SCOPE, TranslatableString("shortcuts", "VST") },
        //: Group heading in Preferences > Shortcuts for shortcuts that show, hide or move panels and toolbars
        { DOCK_SCOPE, TranslatableString("shortcuts", "Dock") },
        //: Group heading in Preferences > Shortcuts for shortcuts that change the workspace (the layout of panels and toolbars)
        { WORKSPACE_SCOPE, TranslatableString("shortcuts", "Workspace") },
        //: Group heading in Preferences > Shortcuts for shortcuts of extensions (add-ons that users install)
        { EXTENSIONS_SCOPE, TranslatableString("shortcuts", "Extensions") },
        //: Group heading in Preferences > Shortcuts for shortcuts that manage the shortcuts themselves
        { SHORTCUTS_SCOPE, TranslatableString("shortcuts", "Shortcuts") },
        //: Group heading in Preferences > Shortcuts for shortcuts that manage application windows
        { MULTIWINDOWS_SCOPE, TranslatableString("shortcuts", "Windows") },
        //: Group heading in Preferences > Shortcuts for shortcuts that check for or install application updates
        { UPDATE_SCOPE, TranslatableString("shortcuts", "Update") },
        //: Group heading in Preferences > Shortcuts for shortcuts of diagnostic tools for developers
        { DIAGNOSTICS_SCOPE, TranslatableString("shortcuts", "Diagnostics") },
        //: Group heading in Preferences > Shortcuts for shortcuts of the automated test tool for developers; "Testflow" is a tool name, do not translate it
        { TESTFLOW_SCOPE, TranslatableString("shortcuts", "Testflow") },
    };

    const std::string upperScopeCode = strings::toUpper(scopeCode);

    const auto it = scopeTitles.find(upperScopeCode);
    if (it != scopeTitles.end()) {
        return it->second;
    }

    LOGW() << "Unknown scope: " << scopeCode;
    return TranslatableString::untranslatable(String::fromStdString(upperScopeCode));
}
