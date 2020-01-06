/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class EffectManager
\brief EffectManager is the class that handles effects and effect categories.

It maintains a graph of effect categories and subcategories,
registers and unregisters effects and can return filtered lists of
effects.

*//*******************************************************************/

#include "../Audacity.h"
#include "EffectManager.h"

#include "Effect.h"

#include <algorithm>
#include <wx/tokenzr.h>

#include "../widgets/AudacityMessageBox.h"

#include "../ShuttleGetDefinition.h"
#include "../commands/CommandContext.h"
#include "../commands/Command.h"
#include "../PluginManager.h"


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
const PluginID & EffectManager::RegisterEffect(Effect *f)
{
   const PluginID & ID = PluginManager::Get().RegisterPlugin(f, PluginTypeEffect);

   mEffects[ID] = f;

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

bool EffectManager::DoAudacityCommand(const PluginID & ID,
                             const CommandContext &context,
                             wxWindow *parent,
                             bool shouldPrompt /* = true */)

{
   this->SetSkipStateFlag(false);
   AudacityCommand *command = GetAudacityCommand(ID);
   
   if (!command)
   {
      return false;
   }

   bool res = command->DoAudacityCommand(parent, context, shouldPrompt);

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
   auto effect = GetEffect(ID);
   if (effect)
      return effect->GetFamily().Msgid();
   return {};
}

TranslatableString EffectManager::GetVendorName(const PluginID & ID)
{
   auto effect = GetEffect(ID);
   if (effect)
      return effect->GetVendor().Msgid();
   return {};
}

CommandID EffectManager::GetCommandIdentifier(const PluginID & ID)
{
   wxString name = PluginManager::Get().GetSymbol(ID).Internal();

   // Get rid of leading and trailing white space
   name.Trim(true).Trim(false);

   if (name.empty())
   {
      return name;
   }

   wxStringTokenizer st(name, wxT(" "));
   wxString id;

   // CamelCase the name
   while (st.HasMoreTokens())
   {
      wxString tok = st.GetNextToken();

      id += tok.Left(1).MakeUpper() + tok.Mid(1).MakeLower();
   }

   return id;
}

TranslatableString EffectManager::GetCommandDescription(const PluginID & ID)
{
   if (GetEffect(ID))
      return XO("Applied effect: %s").Format( GetCommandName(ID) );
   if (GetAudacityCommand(ID))
      return XO("Applied command: %s").Format( GetCommandName(ID) );

   return {};
}

wxString EffectManager::GetCommandUrl(const PluginID & ID)
{
   Effect* pEff = GetEffect(ID);
   if( pEff )
      return pEff->ManualPage();
   AudacityCommand * pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->ManualPage();

   return wxEmptyString;
}

wxString EffectManager::GetCommandTip(const PluginID & ID)
{
   Effect* pEff = GetEffect(ID);
   if( pEff )
      return pEff->GetDescription().Translation();
   AudacityCommand * pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->GetDescription().Translation();

   return wxEmptyString;
}


void EffectManager::GetCommandDefinition(const PluginID & ID, const CommandContext & context, int flags)
{
   ComponentInterface *command;
   command = GetEffect(ID);
   if( !command )
      command = GetAudacityCommand( ID );
   if( !command )
      return;

   ShuttleParams NullShuttle;
   // Test if it defines any parameters at all.
   bool bHasParams = command->DefineParams( NullShuttle );
   if( (flags ==0) && !bHasParams )
      return;

   // This is capturing the output context into the shuttle.
   ShuttleGetDefinition S(  *context.pOutput.get()->mStatusTarget.get() );
   S.StartStruct();
   // using GET to expose a CommandID to the user!
   // Macro command details are one place that we do expose Identifier
   // to (more sophisticated) users
   S.AddItem( GetCommandIdentifier( ID ).GET(), "id" );
   S.AddItem( GetCommandName( ID ).Translation(), "name" );
   if( bHasParams ){
      S.StartField( "params" );
      S.StartArray();
      command->DefineParams( S );
      S.EndArray();
      S.EndField();
   }
   S.AddItem( GetCommandUrl( ID ), "url" );
   // The tip is a translated string!
   S.AddItem( GetCommandTip( ID ), "tip" );
   S.EndStruct();
}



bool EffectManager::IsHidden(const PluginID & ID)
{
   Effect *effect = GetEffect(ID);

   if (effect)
   {
      return effect->IsHidden();
   }

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

wxString EffectManager::GetEffectParameters(const PluginID & ID)
{
   Effect *effect = GetEffect(ID);
   
   if (effect)
   {
      wxString parms;

      effect->GetAutomationParameters(parms);

      // Some effects don't have automatable parameters and will not return
      // anything, so try to get the active preset (current or factory).
      if (parms.empty())
      {
         parms = GetDefaultPreset(ID);
      }

      return parms;
   }

   AudacityCommand *command = GetAudacityCommand(ID);
   
   if (command)
   {
      wxString parms;

      command->GetAutomationParameters(parms);

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

bool EffectManager::SetEffectParameters(const PluginID & ID, const wxString & params)
{
   Effect *effect = GetEffect(ID);
   
   if (effect)
   {
      CommandParameters eap(params);

      if (eap.HasEntry(wxT("Use Preset")))
      {
         return effect->SetAutomationParameters(eap.Read(wxT("Use Preset")));
      }

      return effect->SetAutomationParameters(params);
   }
   AudacityCommand *command = GetAudacityCommand(ID);
   
   if (command)
   {
      // Set defaults (if not initialised) before setting values.
      command->Init(); 
      CommandParameters eap(params);

      if (eap.HasEntry(wxT("Use Preset")))
      {
         return command->SetAutomationParameters(eap.Read(wxT("Use Preset")));
      }

      return command->SetAutomationParameters(params);
   }
   return false;
}

bool EffectManager::PromptUser(
   const PluginID & ID,
   const EffectClientInterface::EffectDialogFactory &factory, wxWindow &parent)
{
   bool result = false;
   Effect *effect = GetEffect(ID);

   if (effect)
   {
      result = effect->ShowInterface(
         parent, factory, effect->IsBatchProcessing() );
      return result;
   }

   AudacityCommand *command = GetAudacityCommand(ID);

   if (command)
   {
      result = command->PromptUser(&parent);
      return result;
   }

   return result;
}

bool EffectManager::HasPresets(const PluginID & ID)
{
   Effect *effect = GetEffect(ID);

   if (!effect)
   {
      return false;
   }

   return effect->GetUserPresets().size() > 0 ||
          effect->GetFactoryPresets().size() > 0 ||
          effect->HasCurrentSettings() ||
          effect->HasFactoryDefaults();
}

wxString EffectManager::GetPreset(const PluginID & ID, const wxString & params, wxWindow * parent)
{
   Effect *effect = GetEffect(ID);

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

   preset = effect->GetPreset(parent, preset);
   if (preset.empty())
   {
      return preset;
   }

   eap.DeleteAll();
   
   eap.Write(wxT("Use Preset"), preset);
   eap.GetParameters(preset);

   return preset;
}

wxString EffectManager::GetDefaultPreset(const PluginID & ID)
{
   Effect *effect = GetEffect(ID);

   if (!effect)
   {
      return wxEmptyString;
   }

   wxString preset;
   if (effect->HasCurrentSettings())
   {
      preset = Effect::kCurrentSettingsIdent;
   }
   else if (effect->HasFactoryDefaults())
   {
      preset = Effect::kFactoryDefaultsIdent;
   }

   if (!preset.empty())
   {
      CommandParameters eap;

      eap.Write(wxT("Use Preset"), preset);
      eap.GetParameters(preset);
   }

   return preset;
}

void EffectManager::SetBatchProcessing(const PluginID & ID, bool start)
{
   Effect *effect = GetEffect(ID);
   if (effect)
   {
      effect->SetBatchProcessing(start);
      return;
   }

   AudacityCommand *command = GetAudacityCommand(ID);
   if (command)
   {
      command->SetBatchProcessing(start);
      return;
   }

}

Effect *EffectManager::GetEffect(const PluginID & ID)
{
   // Must have a "valid" ID
   if (ID.empty())
   {
      return NULL;
   }

   // If it is actually a command then refuse it (as an effect).
   if( mCommands.find( ID ) != mCommands.end() )
      return NULL;

   // TODO: This is temporary and should be redone when all effects are converted
   if (mEffects.find(ID) == mEffects.end())
   {
      // This will instantiate the effect client if it hasn't already been done
      EffectDefinitionInterface *ident = dynamic_cast<EffectDefinitionInterface *>(PluginManager::Get().GetInstance(ID));
      if (ident && ident->IsLegacy())
      {
         auto effect = dynamic_cast<Effect *>(ident);
         if (effect && effect->Startup(NULL))
         {
            mEffects[ID] = effect;
            return effect;
         }
      }

      auto effect = std::make_shared<Effect>(); // TODO: use make_unique and store in std::unordered_map
      if (effect)
      {
         EffectClientInterface *client = dynamic_cast<EffectClientInterface *>(ident);
         if (client && effect->Startup(client))
         {
            auto pEffect = effect.get();
            mEffects[ID] = pEffect;
            mHostEffects[ID] = std::move(effect);
            return pEffect;
         }
      }

      auto command = dynamic_cast<AudacityCommand *>(PluginManager::Get().GetInstance(ID));
      if( !command )
         AudacityMessageBox(
            XO(
"Attempting to initialize the following effect failed:\n\n%s\n\nMore information may be available in 'Help > Diagnostics > Show Log'")
               .Format( GetCommandName(ID) ),
            XO("Effect failed to initialize"));

      return NULL;
   }

   return mEffects[ID];
}

AudacityCommand *EffectManager::GetAudacityCommand(const PluginID & ID)
{
   // Must have a "valid" ID
   if (ID.empty())
   {
      return NULL;
   }

   // TODO: This is temporary and should be redone when all effects are converted
   if (mCommands.find(ID) == mCommands.end())
   {

      // This will instantiate the effect client if it hasn't already been done
      auto command = dynamic_cast<AudacityCommand *>(PluginManager::Get().GetInstance(ID));
      if (command )//&& command->Startup(NULL))
      {
         command->Init();
         mCommands[ID] = command;
         return command;
      }

         /*
      if (ident && ident->IsLegacy())
      {
         auto command = dynamic_cast<AudacityCommand *>(ident);
         if (commandt && command->Startup(NULL))
         {
            mCommands[ID] = command;
            return command;
         }
      }


      auto command = std::make_shared<AudacityCommand>(); // TODO: use make_unique and store in std::unordered_map
      if (command)
      {
         AudacityCommand *client = dynamic_cast<AudacityCommand *>(ident);
         if (client && command->Startup(client))
         {
            auto pCommand = command.get();
            mEffects[ID] = pCommand;
            mHostEffects[ID] = std::move(effect);
            return pEffect;
         }
      }
*/
      AudacityMessageBox(
         XO(
"Attempting to initialize the following command failed:\n\n%s\n\nMore information may be available in 'Help > Diagnostics > Show Log'")
            .Format( GetCommandName(ID) ),
         XO("Command failed to initialize"));

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
   const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect | PluginTypeAudacityCommand);
   while (plug)
   {
      if (GetCommandIdentifier(plug->GetID()) == strTarget)
      {
         return plug->GetID();
      }
      plug = pm.GetNextPlugin(PluginTypeEffect | PluginTypeAudacityCommand);
   }
   return empty;;
}

