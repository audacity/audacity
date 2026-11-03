/*
* Audacity: A Digital Audio Editor
*/
#include "shortcutresolver.h"

#include <algorithm>
#include <map>
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

//! NOTE The priority is used only when one key sequence matches enabled shortcuts in different scopes,
//! so only the scopes that clash with another scope are ranked here; all other scopes get 0.
int ShortcutResolver::scopePriority(const std::string& scope) const
{
    if (scope == NAVIGATION_SCOPE) {
        return 1;
    }

    if (scope == PLAYBACK_SCOPE || scope == RECORD_SCOPE) {
        return 2;
    }

    if (scope == TIMELINE_SCOPE) {
        return isProjectFocused() ? 3 : -1;
    }

    return 0;
}

bool ShortcutResolver::isProjectFocused() const
{
    return uiContextResolver()->matchWithCurrent(UiCtxProjectFocused);
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
        { NAVIGATION_SCOPE, TranslatableString("shortcuts", "NAVIGATION") },
        { APP_SCOPE, TranslatableString("shortcuts", "APP") },
        { PLAYBACK_SCOPE, TranslatableString("shortcuts", "PLAYBACK") },
        { RECORD_SCOPE, TranslatableString("shortcuts", "RECORDING") },
        { TIMELINE_SCOPE, TranslatableString("shortcuts", "TIMELINE") },
        { PROJECT_SCOPE, TranslatableString("shortcuts", "PROJECT") },
        { PROJECTSCENE_SCOPE, TranslatableString("shortcuts", "PROJECT VIEW") },
        { TRACKEDIT_SCOPE, TranslatableString("shortcuts", "EDITING") },
        { TRACK_NAVIGATION_SCOPE, TranslatableString("shortcuts", "TRACK NAVIGATION") },
        { EFFECTS_SCOPE, TranslatableString("shortcuts", "EFFECTS") },
        { SPECTROGRAM_SCOPE, TranslatableString("shortcuts", "SPECTROGRAM") },
        { CLOUD_SCOPE, TranslatableString("shortcuts", "CLOUD") },
        { AUDIO_SCOPE, TranslatableString("shortcuts", "AUDIO") },
        { VST_SCOPE, TranslatableString("shortcuts", "VST") },
        { DOCK_SCOPE, TranslatableString("shortcuts", "DOCK") },
        { WORKSPACE_SCOPE, TranslatableString("shortcuts", "WORKSPACE") },
        { EXTENSIONS_SCOPE, TranslatableString("shortcuts", "EXTENSIONS") },
        { SHORTCUTS_SCOPE, TranslatableString("shortcuts", "SHORTCUTS") },
        { MULTIWINDOWS_SCOPE, TranslatableString("shortcuts", "WINDOWS") },
        { UPDATE_SCOPE, TranslatableString("shortcuts", "UPDATE") },
        { DIAGNOSTICS_SCOPE, TranslatableString("shortcuts", "DIAGNOSTICS") },
        { TESTFLOW_SCOPE, TranslatableString("shortcuts", "TESTFLOW") },
    };

    const std::string upperScopeCode = strings::toUpper(scopeCode);

    const auto it = scopeTitles.find(upperScopeCode);
    if (it != scopeTitles.end()) {
        return it->second;
    }

    LOGW() << "Unknown scope: " << scopeCode;
    return TranslatableString::untranslatable(String::fromStdString(upperScopeCode));
}
