/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

******************************************************************//**

\class EffectManager
\brief EffectManager is the class that handles effects and effect categories.

It maintains a graph of effect categories and subcategories,
registers and unregisters effects and can return filtered lists of
effects.

*//*******************************************************************/


#include "BasicUI.h"
#include "EffectManager.h"
#include "Effect.h"
#include "IAudacityCommand.h"
#include "ShuttleGetDefinition.h"

#include <algorithm>

#include "ConfigInterface.h"
#include "CommandContext.h"
#include "PluginManager.h"
#include "Track.h"

/*******************************************************************************
Creates a singleton and returns reference

 (Thread-safe...no active threading during construction or after destruction)
*******************************************************************************/
EffectManager & EffectManager::Get()
{
   static EffectManager em;
   return em;
}

EffectManager::EffectManager()
{
   mSkipStateFlag = false;
}

EffectManager::~EffectManager()
{
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
const PluginID & EffectManager::RegisterEffect(
   std::unique_ptr<EffectPlugin> uEffect)
{
   auto pEffect = uEffect.get();
   const PluginID & ID =
      PluginManager::Get().RegisterPlugin(std::move(uEffect), PluginTypeEffect);
   mEffects[ID] = { pEffect, {} };
   return ID;
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
void EffectManager::UnregisterEffect(const PluginID & ID)
{
   PluginID id = ID;
   PluginManager::Get().UnregisterPlugin(id);
   mEffects.erase(id);
}

bool EffectManager::DoAudacityCommand(
   const PluginID& ID, const CommandContext& context,
   bool shouldPrompt /* = true */)

{
   this->SetSkipStateFlag(false);
   IAudacityCommand* command = GetAudacityCommand(ID);

   if (!command)
   {
      return false;
   }

   bool res =
      command->DoAudacityCommand(context, shouldPrompt);

   return res;
}

ComponentInterfaceSymbol EffectManager::GetCommandSymbol(const PluginID & ID)
{
   return PluginManager::Get().GetSymbol(ID);
}

TranslatableString EffectManager::GetCommandName(const PluginID & ID)
{
   return GetCommandSymbol(ID).Msgid();
}

TranslatableString EffectManager::GetEffectFamilyName(const PluginID & ID)
{
   if(auto description = PluginManager::Get().GetPlugin(ID))
      return TranslatableString { description->GetEffectFamily(), {} };

   auto effect = GetEffect(ID);
   if (effect)
      return effect->GetDefinition().GetFamily().Msgid();
   return {};
}

TranslatableString EffectManager::GetVendorName(const PluginID & ID)
{
   if(auto description = PluginManager::Get().GetPlugin(ID))
      return TranslatableString { description->GetVendor(), {} };

   auto effect = GetEffect(ID);
   if (effect)
      return effect->GetDefinition().GetVendor().Msgid();
   return {};
}

CommandID EffectManager::GetCommandIdentifier(const PluginID & ID)
{
   auto name = PluginManager::Get().GetSymbol(ID).Internal();
   return EffectDefinitionInterface::GetSquashedName(name);
}

TranslatableString EffectManager::GetCommandDescription(const PluginID & ID)
{
   if (GetEffect(ID))
      return XO("Applied effect: %s").Format( GetCommandName(ID) );
   if (GetAudacityCommand(ID))
      return XO("Applied command: %s").Format( GetCommandName(ID) );

   return {};
}

ManualPageID EffectManager::GetCommandUrl(const PluginID & ID)
{
   if (auto pEff = GetEffect(ID))
      return pEff->GetDefinition().ManualPage();
   IAudacityCommand* pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->ManualPage();

   return wxEmptyString;
}

TranslatableString EffectManager::GetCommandTip(const PluginID & ID)
{
   if (auto pEff = GetEffect(ID))
      return pEff->GetDefinition().GetDescription();
   IAudacityCommand* pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->GetDescription();

   return {};
}

void EffectManager::GetCommandDefinition(
   const PluginID& ID, const CommandContext& context, int flags)
{
   const EffectSettingsManager *effect = nullptr;
   const EffectSettings *settings;
   IAudacityCommand* command = nullptr;

   if (auto [edi, pSettings] = GetEffectAndDefaultSettings(ID); edi) {
      effect = &edi->GetDefinition();
      assert(pSettings); // postcondition
      settings = pSettings;
   }
   else
      command = GetAudacityCommand( ID );
   if ( !effect && !command )
      return;

   ConstSettingsVisitor NullShuttle;

   // Test if it defines any parameters at all.
   bool bHasParams = command
      ? command->VisitSettings( NullShuttle )
      : effect->VisitSettings( NullShuttle, *settings );
   if ( (flags == 0) && !bHasParams )
      return;

   // This is capturing the output context into the shuttle.
   ShuttleGetDefinition S(  *context.pOutput.get()->mStatusTarget.get() );
   S.StartStruct();
   // using GET to expose a CommandID to the user!
   // Macro command details are one place that we do expose Identifier
   // to (more sophisticated) users
   S.AddItem( GetCommandIdentifier( ID ).GET(), "id" );
   S.AddItem( GetCommandName( ID ).Translation(), "name" );
   if ( bHasParams ) {
      S.StartField( "params" );
      S.StartArray();
      command
         ? command->VisitSettings( S )
         : effect->VisitSettings( S, *settings );
      S.EndArray();
      S.EndField();
   }
   // use GET() to expose some details to macro programming users
   S.AddItem( GetCommandUrl( ID ).GET(), "url" );
   // The tip is a translated string!
   S.AddItem( GetCommandTip( ID ).Translation(), "tip" );
   S.EndStruct();
}



bool EffectManager::IsHidden(const PluginID & ID)
{
   if(auto effect = GetEffect(ID))
      return effect->GetDefinition().IsHiddenFromMenus();
   return false;
}

void EffectManager::SetSkipStateFlag(bool flag)
{
   mSkipStateFlag = flag;
}

bool EffectManager::GetSkipStateFlag()
{
   return mSkipStateFlag;
}

bool EffectManager::SupportsAutomation(const PluginID & ID)
{
   const PluginDescriptor *plug =  PluginManager::Get().GetPlugin(ID);
   if (plug)
   {
      return plug->IsEffectAutomatable();
   }

   return false;
}

// This function is used only in the macro programming user interface
wxString EffectManager::GetEffectParameters(const PluginID & ID)
{
   auto pair = GetEffectAndDefaultSettings(ID);
   if (auto effect = pair.first) {
      assert(pair.second); // postcondition
      wxString parms;

      effect->SaveSettingsAsString(*pair.second, parms);

      // Some effects don't have automatable parameters and will not return
      // anything, so try to get the active preset (current or factory).
      if (parms.empty())
      {
         parms = GetDefaultPreset(ID);
      }

      return parms;
   }

   IAudacityCommand* command = GetAudacityCommand(ID);

   if (command)
   {
      wxString parms;

      command->SaveSettingsAsString(parms);

      // Some effects don't have automatable parameters and will not return
      // anything, so try to get the active preset (current or factory).
      if (parms.empty())
      {
         parms = GetDefaultPreset(ID);
      }

      return parms;
   }
   return wxEmptyString;
}

// This function is used only in the macro programming user interface
bool EffectManager::SetEffectParameters(
   const PluginID & ID, const wxString & params)
{
   auto pair = GetEffectAndDefaultSettings(ID);
   if (auto effect = pair.first) {
      assert(pair.second); // postcondition
      auto &settings = *pair.second;
      CommandParameters eap(params);

      // Check first for what GetDefaultPreset() might have written
      if (eap.HasEntry(wxT("Use Preset")))
      {
         return effect->LoadSettingsFromString(
            eap.Read(wxT("Use Preset")), settings).has_value();
      }

      return effect->LoadSettingsFromString(params, settings).has_value();
   }
   IAudacityCommand* command = GetAudacityCommand(ID);

   if (command)
   {
      // Set defaults (if not initialised) before setting values.
      command->Init();
      CommandParameters eap(params);

      // Check first for what GetDefaultPreset() might have written
      if (eap.HasEntry(wxT("Use Preset")))
      {
         return command
            ->LoadSettingsFromString(eap.Read(wxT("Use Preset")));
      }

      return command->LoadSettingsFromString(params);
   }
   return false;
}

//! Shows an effect or command dialog so the user can specify settings for later
/*!
 It is used when defining a macro.  It does not invoke the effect or command.
 */
bool EffectManager::PromptUser(
   const PluginID& ID, AudacityProject& project, DialogInvoker dialogInvoker)
{
   bool result = false;
   if (auto effect = dynamic_cast<Effect*>(GetEffect(ID))) {

      auto empty = TrackList::Create(nullptr);
      auto pEffectBase = dynamic_cast<EffectBase*>(effect);
      if (pEffectBase)
         // This allows effects to call Init() safely
         pEffectBase->SetTracks(empty.get());
      Finally Do([&]{
         // reverse the side-effect
         if (pEffectBase)
            pEffectBase->SetTracks(nullptr);
      });

      std::shared_ptr<EffectInstance> pInstance;
      //! Show the effect dialog, only so that the user can choose settings,
      //! for instance to define a macro.
      if (const auto pSettings = GetDefaultSettings(ID))
         result = dialogInvoker(*effect, *pSettings, pInstance);
      return result;
   }

   IAudacityCommand* command = GetAudacityCommand(ID);

   if (command)
   {
      result = command->PromptUser(project);
      return result;
   }

   return result;
}

bool HasCurrentSettings(EffectPlugin &host)
{
   return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
      CurrentSettingsGroup());
}

bool HasFactoryDefaults(EffectPlugin &host)
{
   return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
      FactoryDefaultsGroup());
}

RegistryPaths GetUserPresets(EffectPlugin &host)
{
   RegistryPaths presets;
   GetConfigSubgroups(host.GetDefinition(), PluginSettings::Private,
      UserPresetsGroup({}), presets);
   std::sort( presets.begin(), presets.end() );
   return presets;
}

bool EffectManager::HasPresets(const PluginID & ID)
{
   auto effect = GetEffect(ID);

   if (!effect)
   {
      return false;
   }

   return GetUserPresets(*effect).size() > 0 ||
          effect->GetDefinition().GetFactoryPresets().size() > 0 ||
          HasCurrentSettings(*effect) ||
          HasFactoryDefaults(*effect);
}

// This function is used only in the macro programming user interface
wxString EffectManager::GetPreset(
   const PluginID& ID, const wxString& params, EffectPresetDialog dialog)
{
   auto effect = GetEffect(ID);

   if (!effect)
   {
      return wxEmptyString;
   }

   CommandParameters eap(params);

   wxString preset;
   if (eap.HasEntry(wxT("Use Preset")))
   {
      preset = eap.Read(wxT("Use Preset"));
   }

   if (const auto answer = dialog(*effect, preset))
      preset = *answer;
   else
      preset = wxEmptyString;

   if (preset.empty())
   {
      return preset;
   }

   // This cleans a config "file" backed by a string in memory.
   eap.DeleteAll();

   eap.Write(wxT("Use Preset"), preset);
   eap.GetParameters(preset);

   return preset;
}

// This function is used only in the macro programming user interface
wxString EffectManager::GetDefaultPreset(const PluginID & ID)
{
   auto effect = GetEffect(ID);

   if (!effect)
   {
      return wxEmptyString;
   }

   wxString preset;
   if (HasCurrentSettings(*effect))
   {
      preset = EffectPlugin::kCurrentSettingsIdent;
   }
   else if (HasFactoryDefaults(*effect))
   {
      preset = EffectPlugin::kFactoryDefaultsIdent;
   }

   if (!preset.empty())
   {
      CommandParameters eap;

      eap.Write(wxT("Use Preset"), preset);
      eap.GetParameters(preset);
   }

   return preset;
}

void EffectManager::BatchProcessingOn(const PluginID & ID)
{
   if (auto effect = GetEffect(ID))
      effect->SetBatchProcessing();
   else if (auto command = GetAudacityCommand(ID))
      command->SetBatchProcessing(true);
}

void EffectManager::BatchProcessingOff(const PluginID & ID)
{
   if (auto effect = GetEffect(ID))
      effect->UnsetBatchProcessing();
   else if (auto command = GetAudacityCommand(ID))
      command->SetBatchProcessing(false);
}

EffectPlugin* EffectManager::GetEffect(const PluginID& ID)
{
   return DoGetEffect(ID).effect;
}

EffectSettings* EffectManager::GetDefaultSettings(const PluginID& ID)
{
   return GetEffectAndDefaultSettings(ID).second;
}

std::pair<EffectPlugin*, EffectSettings*>
EffectManager::GetEffectAndDefaultSettings(const PluginID& ID)
{
   auto &results = DoGetEffect(ID);
   if (results.effect)
      return {results.effect, &results.settings};
   else
      return {nullptr, nullptr};
}

namespace {
// Before: settings are as defaulted by `manager.MakeSettings()`
// Do as needed (once, persistently, when the plug-in is first used): store
// those default values into the config under "FactoryDefaults" preset
// After: settings are loaded for the "CurrentSettings" preset
void InitializePreset(
   EffectSettingsManager &manager, EffectSettings &settings) {
   // Config key remembering whether we already stored FactoryDefaults
   constexpr auto InitializedKey = L"Initialized";
   if (bool haveDefaults{};
      GetConfig(manager, PluginSettings::Private, FactoryDefaultsGroup(),
         InitializedKey, haveDefaults, false),
      !haveDefaults
   ) {
      manager.SaveUserPreset(FactoryDefaultsGroup(), settings);
      // Also initialize the "current" settings --
      if (bool haveCurrent{};
         GetConfig(manager, PluginSettings::Private, CurrentSettingsGroup(),
            InitializedKey, haveCurrent, false),
         !haveCurrent
      ) {
         manager.SaveUserPreset(CurrentSettingsGroup(), settings);
      }
      SetConfig(manager, PluginSettings::Private, FactoryDefaultsGroup(),
         InitializedKey, true);
   }
   // ignore failure
   (void) manager.LoadUserPreset(CurrentSettingsGroup(), settings);
}

std::pair<ComponentInterface *, EffectSettings>
LoadComponent(const PluginID &ID)
{
   if (auto result = dynamic_cast<EffectSettingsManager*>(
      PluginManager::Get().Load(ID))) {
      auto settings = result->MakeSettings();
      InitializePreset(*result, settings);
      return { result, std::move(settings) };
   }
   return { nullptr, {} };
}
}

EffectAndDefaultSettings &EffectManager::DoGetEffect(const PluginID & ID)
{
   static EffectAndDefaultSettings empty;

   // Must have a "valid" ID
   if (ID.empty())
      return empty;

   // If it is actually a command then refuse it (as an effect).
   if( mCommands.find( ID ) != mCommands.end() )
      return empty;

   if (auto iter = mEffects.find(ID); iter != mEffects.end())
      return iter->second;
   else {
      // This will instantiate the effect client if it hasn't already been done
      auto [component, settings] = LoadComponent(ID);
      if (!component)
         return empty;

      if (auto effect = dynamic_cast<EffectPlugin*>(component))
         return (mEffects[ID] = { effect, std::move(settings) });
      else
      {
         if (!dynamic_cast<IAudacityCommand*>(component))
            BasicUI::ShowMessageBox(
               XO("Attempting to initialize the following effect failed:\n\n%s\n\nMore information may be available in 'Help > Diagnostics > Show Log'")
                  .Format(GetCommandName(ID)),
               BasicUI::MessageBoxOptions {}.Caption(
                  XO("Effect failed to initialize")));

         return empty;
      }
   }
}

IAudacityCommand* EffectManager::GetAudacityCommand(const PluginID& ID)
{
   // Must have a "valid" ID
   if (ID.empty())
   {
      return NULL;
   }

   if (mCommands.find(ID) == mCommands.end()) {
      // This will instantiate the command if it hasn't already been done
      auto command =
         dynamic_cast<IAudacityCommand *>(PluginManager::Get().Load(ID));
      if (command)
      {
         command->Init();
         mCommands[ID] = command;
         return command;
      }

      BasicUI::ShowMessageBox(
         XO("Attempting to initialize the following command failed:\n\n%s\n\nMore information may be available in 'Help > Diagnostics > Show Log'")
            .Format(GetCommandName(ID)),
         BasicUI::MessageBoxOptions {}.Caption(
            XO("Command failed to initialize")));

      return NULL;
   }

   return mCommands[ID];
}

const PluginID & EffectManager::GetEffectByIdentifier(const CommandID & strTarget)
{
   static PluginID empty;
   if (strTarget.empty()) // set GetCommandIdentifier to wxT("") to not show an effect in Batch mode
   {
      return empty;
   }

   PluginManager & pm = PluginManager::Get();
   // Effects OR Generic commands...
   for (auto &plug
        : pm.PluginsOfType(PluginTypeEffect | PluginTypeAudacityCommand)) {
      auto &ID = plug.GetID();
      if (GetCommandIdentifier(ID) == strTarget)
         return ID;
   }
   return empty;
}

const EffectInstanceFactory*
EffectManager::GetInstanceFactory(const PluginID& ID)
{
   return Get().GetEffect(ID);
}
