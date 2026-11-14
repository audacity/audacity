/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectAndCommandManager.cpp

   Split from EffectManager.cpp

**********************************************************************/
#include "EffectAndCommandPluginManager.h"
#include "BasicUI.h"
#include "CommandContext.h"
#include "Effect.h"
#include "EffectManager.h"
#include "PluginManager.h"
#include "ShuttleGetDefinition.h"
#include "Track.h"
#include "commands/AudacityCommand.h"

EffectAndCommandPluginManager& EffectAndCommandPluginManager::Get()
{
    static EffectAndCommandPluginManager ecm;
    return ecm;
}

bool EffectAndCommandPluginManager::DoAudacityCommand(
    const PluginID& ID, const CommandContext& context,
    bool shouldPrompt /* = true */)
{
    EffectManager::Get().SetSkipStateFlag(false);
    AudacityCommand* command = GetAudacityCommand(ID);

    if (!command) {
        return false;
    }

    bool res = command->DoAudacityCommand(context, shouldPrompt);

    return res;
}

AudacityCommand*
EffectAndCommandPluginManager::GetAudacityCommand(const PluginID& ID)
{
    // Must have a "valid" ID
    if (ID.empty()) {
        return NULL;
    }

    if (mCommands.find(ID) == mCommands.end()) {
        // This will instantiate the command if it hasn't already been done
        auto command
            =dynamic_cast<AudacityCommand*>(PluginManager::Get().Load(ID));
        if (command) {
            command->Init();
            mCommands[ID] = command;
            return command;
        }

        BasicUI::ShowMessageBox(
            XO(
                "Attempting to initialize the following command failed:\n\n%s\n\nMore information may be available in 'Help > Diagnostics > Show Log'")
            .Format(PluginManager::Get().GetName(ID)),
            BasicUI::MessageBoxOptions {}.Caption(
                XO("Command failed to initialize")));

        return NULL;
    }

    return mCommands[ID];
}

ManualPageID EffectAndCommandPluginManager::GetCommandUrl(const PluginID& ID)
{
    if (auto pEff = EffectManager::Get().GetEffect(ID)) {
        return pEff->GetDefinition().ManualPage();
    }
    AudacityCommand* pCom = GetAudacityCommand(ID);
    if (pCom) {
        return pCom->ManualPage();
    }

    return wxEmptyString;
}

TranslatableString
EffectAndCommandPluginManager::GetCommandTip(const PluginID& ID)
{
    if (auto pEff = EffectManager::Get().GetEffect(ID)) {
        return pEff->GetDefinition().GetDescription();
    }
    AudacityCommand* pCom = GetAudacityCommand(ID);
    if (pCom) {
        return pCom->GetDescription();
    }

    return {};
}

void EffectAndCommandPluginManager::GetCommandDefinition(
    const PluginID& ID, const CommandContext& context, int flags)
{
    const EffectSettingsManager* effect = nullptr;
    const EffectSettings* settings;
    AudacityCommand* command = nullptr;

    if (auto [edi, pSettings]
            =EffectManager::Get().GetEffectAndDefaultSettings(ID);
        edi) {
        effect = &edi->GetDefinition();
        assert(pSettings); // postcondition
        settings = pSettings;
    } else {
        command = GetAudacityCommand(ID);
    }
    if (!effect && !command) {
        return;
    }

    ConstSettingsVisitor NullShuttle;

    // Test if it defines any parameters at all.
    bool bHasParams = command ? command->VisitSettings(NullShuttle)
                      : effect->VisitSettings(NullShuttle, *settings);
    if ((flags == 0) && !bHasParams) {
        return;
    }

    // This is capturing the output context into the shuttle.
    ShuttleGetDefinition S(*context.pOutput.get()->mStatusTarget.get());
    S.StartStruct();
    // using GET to expose a CommandID to the user!
    // Macro command details are one place that we do expose Identifier
    // to (more sophisticated) users
    const auto& pm = PluginManager::Get();
    S.AddItem(pm.GetCommandIdentifier(ID).GET(), "id");
    S.AddItem(pm.GetName(ID).Translation(), "name");
    if (bHasParams) {
        S.StartField("params");
        S.StartArray();
        command ? command->VisitSettings(S) : effect->VisitSettings(S, *settings);
        S.EndArray();
        S.EndField();
    }
    // use GET() to expose some details to macro programming users
    S.AddItem(GetCommandUrl(ID).GET(), "url");
    // The tip is a translated string!
    S.AddItem(GetCommandTip(ID).Translation(), "tip");
    S.EndStruct();
}

void EffectAndCommandPluginManager::BatchProcessingOn(const PluginID& ID)
{
    if (auto effect = EffectManager::Get().GetEffect(ID)) {
        effect->SetBatchProcessing();
    } else if (auto command = GetAudacityCommand(ID)) {
        command->SetBatchProcessing(true);
    }
}

void EffectAndCommandPluginManager::BatchProcessingOff(const PluginID& ID)
{
    if (auto effect = EffectManager::Get().GetEffect(ID)) {
        effect->UnsetBatchProcessing();
    } else if (auto command = GetAudacityCommand(ID)) {
        command->SetBatchProcessing(false);
    }
}

// This function is used only in the macro programming user interface
wxString EffectAndCommandPluginManager::GetEffectParameters(const PluginID& ID)
{
    auto pair = EffectManager::Get().GetEffectAndDefaultSettings(ID);
    if (auto effect = pair.first) {
        assert(pair.second); // postcondition
        wxString parms;

        effect->SaveSettingsAsString(*pair.second, parms);

        // Some effects don't have automatable parameters and will not return
        // anything, so try to get the active preset (current or factory).
        if (parms.empty()) {
            parms = EffectManager::Get().GetDefaultPreset(ID);
        }

        return parms;
    }

    AudacityCommand* command = GetAudacityCommand(ID);

    if (command) {
        wxString parms;

        command->SaveSettingsAsString(parms);

        // Some effects don't have automatable parameters and will not return
        // anything, so try to get the active preset (current or factory).
        if (parms.empty()) {
            parms = EffectManager::Get().GetDefaultPreset(ID);
        }

        return parms;
    }
    return wxEmptyString;
}

// This function is used only in the macro programming user interface
bool EffectAndCommandPluginManager::SetEffectParameters(
    const PluginID& ID, const wxString& params)
{
    auto pair = EffectManager::Get().GetEffectAndDefaultSettings(ID);
    if (auto effect = pair.first) {
        assert(pair.second); // postcondition
        auto& settings = *pair.second;
        CommandParameters eap(params);

        // Check first for what GetDefaultPreset() might have written
        if (eap.HasEntry(wxT("Use Preset"))) {
            return effect
                   ->LoadSettingsFromString(eap.Read(wxT("Use Preset")), settings)
                   .has_value();
        }

        return effect->LoadSettingsFromString(params, settings).has_value();
    }
    AudacityCommand* command = GetAudacityCommand(ID);

    if (command) {
        // Set defaults (if not initialised) before setting values.
        command->Init();
        CommandParameters eap(params);

        // Check first for what GetDefaultPreset() might have written
        if (eap.HasEntry(wxT("Use Preset"))) {
            return command->LoadSettingsFromString(eap.Read(wxT("Use Preset")));
        }

        return command->LoadSettingsFromString(params);
    }
    return false;
}

//! Shows an effect or command dialog so the user can specify settings for later
/*!
 It is used when defining a macro.  It does not invoke the effect or command.
 */
bool EffectAndCommandPluginManager::PromptUser(
    const PluginID& ID, AudacityProject& project, DialogInvoker dialogInvoker)
{
    bool result = false;
    if (auto effect = dynamic_cast<Effect*>(EffectManager::Get().GetEffect(ID))) {
        auto empty = TrackList::Create(nullptr);
        auto pEffectBase = dynamic_cast<EffectBase*>(effect);
        if (pEffectBase) {
            // This allows effects to call Init() safely
            pEffectBase->SetTracks(empty.get());
        }
        Finally Do([&] {
            // reverse the side-effect
            if (pEffectBase) {
                pEffectBase->SetTracks(nullptr);
            }
        });

        std::shared_ptr<EffectInstance> pInstance;
        //! Show the effect dialog, only so that the user can choose settings,
        //! for instance to define a macro.
        if (const auto pSettings = EffectManager::Get().GetDefaultSettings(ID)) {
            result = dialogInvoker(*effect, *pSettings, pInstance);
        }
        return result;
    }

    AudacityCommand* command = GetAudacityCommand(ID);

    if (command) {
        result = command->PromptUser(project);
        return result;
    }

    return result;
}
