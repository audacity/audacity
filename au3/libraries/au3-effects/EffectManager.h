/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_EFFECTMANAGER__
#define __AUDACITY_EFFECTMANAGER__

#include "au3-components/EffectInterface.h"
#include "au3-strings/Identifier.h"

#include <functional>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

class AudacityProject;
class CommandContext;
class ComponentInterfaceSymbol;
class wxString;
typedef wxString PluginID;
class Effect;
class EffectPlugin;
struct EffectSettings;
class EffectInstance;

struct EffectAndDefaultSettings {
    EffectPlugin* effect{};
    EffectSettings settings{};
};

using EffectMap = std::unordered_map<wxString, EffectAndDefaultSettings>;
using EffectOwnerMap = std::unordered_map< wxString, std::shared_ptr<EffectPlugin> >;

EFFECTS_API
RegistryPaths GetUserPresets(EffectPlugin& host);

EFFECTS_API
bool HasCurrentSettings(EffectPlugin& host);

EFFECTS_API
bool HasFactoryDefaults(EffectPlugin& host);

class EFFECTS_API EffectManager
{
public:

    enum : unsigned {
        // No flags specified
        kNone = 0x00,
        // Flag used to disable prompting for configuration parameteres.
        kConfigured = 0x01,
        // Flag used to disable saving the state after processing.
        kSkipState = 0x02,
        // Flag used to disable "Repeat Last Effect"
        kDontRepeatLast = 0x04,
        // Flag used to disable "Select All during Repeat Generator Effect"
        kRepeatGen = 0x08,
        // Flag used for repeating Nyquist Prompt
        kRepeatNyquistPrompt = 0x10,
    };

    using EffectLoader = std::function<EffectSettingsManager* (const PluginID&)>;

    /*! Find the singleton EffectInstanceFactory for ID. */
    static const EffectInstanceFactory*
    GetInstanceFactory(const PluginID& ID, const EffectLoader& effectLoader);

    /** Get the singleton instance of the EffectManager. Probably not safe
        for multi-thread use. */
    static EffectManager& Get();

    //
    // public methods
    //
    // Used by the outside program to register the list of effects and retrieve
    // them by index number, usually when the user selects one from a menu.
    //
public:
    using EffectPresetDialog = std::function<std::optional<wxString>(
                                                 EffectPlugin&, const wxString& preset)>;

    /** Allow effects to disable saving the state at run time */
    void SetSkipStateFlag(bool flag);
    bool GetSkipStateFlag();

    /*! Return an effect by its ID. */
    EffectPlugin* GetEffect(const PluginID& ID, const EffectLoader& effectLoader);

    /*! Get default settings by effect ID.  May return nullptr */
    EffectSettings* GetDefaultSettings(const PluginID& ID, const EffectLoader& effectLoader);

    /*! Get effect and default settings by effect ID. */
    /*!
     @post `result: !result.first || result.second`
     (if first member is not null, then the second is not null)
     */
    std::pair<EffectPlugin*, EffectSettings*>
    GetEffectAndDefaultSettings(const PluginID& ID, const EffectLoader& effectLoader);

private:
    EffectAndDefaultSettings& DoGetEffect(const PluginID& ID, const EffectLoader& effectLoader);

    EffectMap mEffects;
    EffectOwnerMap mHostEffects;

    int mNumEffects;

    // Set true if we want to skip pushing state
    // after processing at effect run time.
    bool mSkipStateFlag;
};

#endif
