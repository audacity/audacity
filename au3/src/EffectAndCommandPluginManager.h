/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectAndCommandManager.h

   Split from EffectManager.h

   Yet another "manager".
   This one extends the functionality of `EffectManager` from plugins of type
   `PluginTypeEffect` to also include plugins of type
   `PluginTypeAudacityCommand`.

   Motivation for this split was remove references to audacity commands from
   `EffectManager` and be able to make `DoEffect` UI-framework-agnostic.

**********************************************************************/
#pragma once

#include "Identifier.h"
#include "TranslatableString.h"
#include <functional>
#include <memory>
#include <wx/string.h>

class Effect;
class EffectSettings;
class EffectInstance;
class AudacityProject;
class CommandContext;
class AudacityCommand;

using PluginID = wxString;

class EffectAndCommandPluginManager
{
public:
    static EffectAndCommandPluginManager& Get();

    using DialogInvoker = std::function<bool (
                                            Effect&, EffectSettings&, std::shared_ptr<EffectInstance>&)>;

    /** Run a command given the plugin ID */
    // Returns true on success.
    bool DoAudacityCommand(
        const PluginID& ID, const CommandContext&, bool shouldPrompt = true);

    // flags control which commands are included.
    void GetCommandDefinition(
        const PluginID& ID, const CommandContext& context, int flags);

    /** Support for batch commands */
    wxString GetEffectParameters(const PluginID& ID);
    bool SetEffectParameters(const PluginID& ID, const wxString& params);
    bool PromptUser(
        const PluginID& ID, AudacityProject& project, DialogInvoker dialogInvoker);

private:
    void BatchProcessingOn(const PluginID& ID);
    void BatchProcessingOff(const PluginID& ID);
    //! A custom deleter for std::unique_ptr
    struct UnsetBatchProcessing
    {
        PluginID mID;
        void operator()(EffectAndCommandPluginManager* p) const
        {
            if (p) {
                p->BatchProcessingOff(mID);
            }
        }
    };
    using BatchProcessingScope
        =std::unique_ptr<EffectAndCommandPluginManager, UnsetBatchProcessing>;

public:
    //! Begin a scope that ends when the returned object is destroyed
    /*!
     Within this scope, "batch" (i.e. macro) processing happens, and
     Effects that are not yet stateless may change their state temporarily,
     but it is restored afterward
     */
    BatchProcessingScope SetBatchProcessing(const PluginID& ID)
    {
        BatchProcessingOn(ID);
        return BatchProcessingScope { this, { ID } };
    }

private:
    using AudacityCommandMap = std::unordered_map<wxString, AudacityCommand*>;

    // Used by GetCommandDefinition
    ManualPageID GetCommandUrl(const PluginID& ID);
    TranslatableString GetCommandTip(const PluginID& ID);

    AudacityCommand* GetAudacityCommand(const PluginID& ID);
    AudacityCommandMap mCommands;
};
