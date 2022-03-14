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


#include "EffectManager.h"

#include "Effect.h"

#include <algorithm>
#include <wx/tokenzr.h>

#include "../widgets/AudacityMessageBox.h"

#include "../ShuttleGetDefinition.h"
#include "../commands/CommandContext.h"
#include "../commands/AudacityCommand.h"
#include "PluginManager.h"


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
   std::unique_ptr<EffectUIHostInterface> uEffect)
{
   auto pEffect = uEffect.get();
   // Change the "grip" on the object
   // uEffect is expected to be a self-hosting Nyquist effect
   auto uDefinition = std::unique_ptr<EffectDefinitionInterface>(
      dynamic_cast<EffectUIClientInterface*>(uEffect.release()));
   wxASSERT(uDefinition);
   const PluginID & ID =
      PluginManager::Get().RegisterPlugin(std::move(uDefinition), PluginTypeEffect);
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
      return effect->GetDefinition().GetFamily().Msgid();
   return {};
}

TranslatableString EffectManager::GetVendorName(const PluginID & ID)
{
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
   AudacityCommand * pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->ManualPage();

   return wxEmptyString;
}

TranslatableString EffectManager::GetCommandTip(const PluginID & ID)
{
   if (auto pEff = GetEffect(ID))
      return pEff->GetDefinition().GetDescription();
   AudacityCommand * pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->GetDescription();

   return {};
}


void EffectManager::GetCommandDefinition(const PluginID & ID, const CommandContext & context, int flags)
{
   ComponentInterface *command = nullptr;
   if (auto effect = GetEffect(ID))
      // Fixing this will be a large and difficult thing, so for the time being we only cast the constness away
      command = const_cast<EffectDefinitionInterface*>(&effect->GetDefinition());
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

wxString EffectManager::GetEffectParameters(const PluginID & ID)
{
   if (auto effect = GetEffect(ID)) {
      wxString parms;

      effect->GetAutomationParametersAsString(parms);

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

      command->GetAutomationParametersAsString(parms);

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
   if (auto effect = GetEffect(ID)) {
      CommandParameters eap(params);

      if (eap.HasEntry(wxT("Use Preset")))
      {
         return effect
            ->SetAutomationParametersFromString(eap.Read(wxT("Use Preset")));
      }

      return effect->SetAutomationParametersFromString(params);
   }
   AudacityCommand *command = GetAudacityCommand(ID);
   
   if (command)
   {
      // Set defaults (if not initialised) before setting values.
      command->Init(); 
      CommandParameters eap(params);

      if (eap.HasEntry(wxT("Use Preset")))
      {
         return command
            ->SetAutomationParametersFromString(eap.Read(wxT("Use Preset")));
      }

      return command->SetAutomationParametersFromString(params);
   }
   return false;
}

//! Shows an effect or command dialog so the user can specify settings for later
/*!
 It is used when defining a macro.  It does not invoke the effect or command.
 */
bool EffectManager::PromptUser(
   const PluginID & ID, const EffectDialogFactory &factory, wxWindow &parent)
{
   bool result = false;
   if (auto effect = GetEffect(ID)) {
      //! Show the effect dialog, only so that the user can choose settings,
      //! for instance to define a macro.
      auto pSettings = GetDefaultSettings(ID);
      if (pSettings)
         result = effect->ShowHostInterface(
            parent, factory,
            *std::make_shared<SimpleEffectSettingsAccess>(*pSettings),
            effect->IsBatchProcessing() ) != 0;
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

static bool HasCurrentSettings(EffectHostInterface &host)
{
   return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
      CurrentSettingsGroup());
}

static bool HasFactoryDefaults(EffectHostInterface &host)
{
   return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
      FactoryDefaultsGroup());
}

static RegistryPaths GetUserPresets(EffectHostInterface &host)
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

#include <wx/choice.h>
#include <wx/listbox.h>
#include "../ShuttleGui.h"

namespace {

///////////////////////////////////////////////////////////////////////////////
//
// EffectPresetsDialog
//
///////////////////////////////////////////////////////////////////////////////

class EffectPresetsDialog final : public wxDialogWrapper
{
public:
   EffectPresetsDialog(wxWindow *parent, EffectUIHostInterface *effect);
   virtual ~EffectPresetsDialog();

   wxString GetSelected() const;
   void SetSelected(const wxString & parms);

private:
   void SetPrefix(const TranslatableString & type, const wxString & prefix);
   void UpdateUI();

   void OnType(wxCommandEvent & evt);
   void OnOk(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);

private:
   wxChoice *mType;
   wxListBox *mPresets;

   RegistryPaths mFactoryPresets;
   RegistryPaths mUserPresets;
   wxString mSelection;

   DECLARE_EVENT_TABLE()
};

enum
{
   ID_Type = 10000
};

BEGIN_EVENT_TABLE(EffectPresetsDialog, wxDialogWrapper)
   EVT_CHOICE(ID_Type, EffectPresetsDialog::OnType)
   EVT_LISTBOX_DCLICK(wxID_ANY, EffectPresetsDialog::OnOk)
   EVT_BUTTON(wxID_OK, EffectPresetsDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, EffectPresetsDialog::OnCancel)
END_EVENT_TABLE()

EffectPresetsDialog::EffectPresetsDialog(
   wxWindow *parent, EffectUIHostInterface *effect)
:  wxDialogWrapper(parent, wxID_ANY, XO("Select Preset"))
{
   ShuttleGui S(this, eIsCreating);
   S.StartVerticalLay();
   {
      S.StartTwoColumn();
      S.SetStretchyCol(1);
      {
         S.AddPrompt(XXO("Type:"));
         mType = S.Id(ID_Type).AddChoice( {}, {}, 0 );

         S.AddPrompt(XXO("&Preset:"));
         mPresets = S
            .Style( wxLB_SINGLE | wxLB_NEEDED_SB )
            .AddListBox( {} );
      }
      S.EndTwoColumn();

      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   mUserPresets = GetUserPresets(*effect);
   mFactoryPresets = effect->GetDefinition().GetFactoryPresets();

   if (mUserPresets.size() > 0)
   {
      mType->Append(_("User Presets"));
   }

   if (mFactoryPresets.size() > 0)
   {
      mType->Append(_("Factory Presets"));
   }

   if (HasCurrentSettings(*effect))
   {
      mType->Append(_("Current Settings"));
   }

   if (HasFactoryDefaults(*effect))
   {
      mType->Append(_("Factory Defaults"));
   }

   UpdateUI();
}

EffectPresetsDialog::~EffectPresetsDialog()
{
}

wxString EffectPresetsDialog::GetSelected() const
{
   return mSelection;
}

void EffectPresetsDialog::SetSelected(const wxString & parms)
{
   wxString preset = parms;
   if (preset.StartsWith(EffectUIHostInterface::kUserPresetIdent))
   {
      preset.Replace(EffectUIHostInterface::kUserPresetIdent, wxEmptyString, false);
      SetPrefix(XO("User Presets"), preset);
   }
   else if (preset.StartsWith(EffectUIHostInterface::kFactoryPresetIdent))
   {
      preset.Replace(EffectUIHostInterface::kFactoryPresetIdent, wxEmptyString, false);
      SetPrefix(XO("Factory Presets"), preset);
   }
   else if (preset.StartsWith(EffectUIHostInterface::kCurrentSettingsIdent))
   {
      SetPrefix(XO("Current Settings"), wxEmptyString);
   }
   else if (preset.StartsWith(EffectUIHostInterface::kFactoryDefaultsIdent))
   {
      SetPrefix(XO("Factory Defaults"), wxEmptyString);
   }
}

void EffectPresetsDialog::SetPrefix(
   const TranslatableString & type, const wxString & prefix)
{
   mType->SetStringSelection(type.Translation());

   if (type == XO("User Presets"))
   {
      mPresets->Clear();
      for (const auto &preset : mUserPresets)
         mPresets->Append(preset);
      mPresets->Enable(true);
      mPresets->SetStringSelection(prefix);
      if (mPresets->GetSelection() == wxNOT_FOUND)
      {
         mPresets->SetSelection(0);
      }
      mSelection = EffectUIHostInterface::kUserPresetIdent
         + mPresets->GetStringSelection();
   }
   else if (type == XO("Factory Presets"))
   {
      mPresets->Clear();
      for (size_t i = 0, cnt = mFactoryPresets.size(); i < cnt; i++)
      {
         auto label = mFactoryPresets[i];
         if (label.empty())
         {
            label = _("None");
         }
         mPresets->Append(label);
      }
      mPresets->Enable(true);
      mPresets->SetStringSelection(prefix);
      if (mPresets->GetSelection() == wxNOT_FOUND)
      {
         mPresets->SetSelection(0);
      }
      mSelection = EffectUIHostInterface::kFactoryPresetIdent
         + mPresets->GetStringSelection();
   }
   else if (type == XO("Current Settings"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectUIHostInterface::kCurrentSettingsIdent;
   }
   else if (type == XO("Factory Defaults"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectUIHostInterface::kFactoryDefaultsIdent;
   }
}

void EffectPresetsDialog::UpdateUI()
{
   int selected = mType->GetSelection();
   if (selected == wxNOT_FOUND)
   {
      selected = 0;
      mType->SetSelection(selected);
   }
   wxString type = mType->GetString(selected);

   if (type == _("User Presets"))
   {
      selected = mPresets->GetSelection();
      if (selected == wxNOT_FOUND)
      {
         selected = 0;
      }

      mPresets->Clear();
      for (const auto &preset : mUserPresets)
         mPresets->Append(preset);
      mPresets->Enable(true);
      mPresets->SetSelection(selected);
      mSelection = EffectUIHostInterface::kUserPresetIdent
         + mPresets->GetString(selected);
   }
   else if (type == _("Factory Presets"))
   {
      selected = mPresets->GetSelection();
      if (selected == wxNOT_FOUND)
      {
         selected = 0;
      }

      mPresets->Clear();
      for (size_t i = 0, cnt = mFactoryPresets.size(); i < cnt; i++)
      {
         auto label = mFactoryPresets[i];
         if (label.empty())
         {
            label = _("None");
         }
         mPresets->Append(label);
      }
      mPresets->Enable(true);
      mPresets->SetSelection(selected);
      mSelection = EffectUIHostInterface::kFactoryPresetIdent
         + mPresets->GetString(selected);
   }
   else if (type == _("Current Settings"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectUIHostInterface::kCurrentSettingsIdent;
   }
   else if (type == _("Factory Defaults"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectUIHostInterface::kFactoryDefaultsIdent;
   }
}

void EffectPresetsDialog::OnType(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();
}

void EffectPresetsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();

   EndModal(true);
}

void EffectPresetsDialog::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   mSelection = wxEmptyString;

   EndModal(false);
}

}

wxString EffectManager::GetPreset(const PluginID & ID, const wxString & params, wxWindow * parent)
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

   {
      EffectPresetsDialog dlg(parent, effect);
      dlg.Layout();
      dlg.Fit();
      dlg.SetSize(dlg.GetMinSize());
      dlg.CenterOnParent();
      dlg.SetSelected(preset);
      
      if (dlg.ShowModal())
         preset = dlg.GetSelected();
      else
         preset = wxEmptyString;
   }

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
      preset = EffectUIHostInterface::kCurrentSettingsIdent;
   }
   else if (HasFactoryDefaults(*effect))
   {
      preset = EffectUIHostInterface::kFactoryDefaultsIdent;
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
   auto effect = GetEffect(ID);
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

EffectUIHostInterface *EffectManager::GetEffect(const PluginID & ID)
{
   return DoGetEffect(ID).effect;
}

EffectSettings *EffectManager::GetDefaultSettings(const PluginID & ID)
{
   return GetEffectAndDefaultSettings(ID).second;
}

std::pair<EffectUIHostInterface *, EffectSettings *>
EffectManager::GetEffectAndDefaultSettings(const PluginID & ID)
{
   auto &results = DoGetEffect(ID);
   if (results.effect) {
      auto &settings = results.settings;
      if (!settings.has_value())
         // Create on demand
         settings = results.effect->GetDefinition().MakeSettings();
      return {results.effect, &settings};
   }
   return {nullptr, nullptr};
}

namespace {
void InitializePreset(EffectDefinitionInterfaceEx &definition) {
   bool haveDefaults;
   GetConfig(definition, PluginSettings::Private, FactoryDefaultsGroup(),
      wxT("Initialized"), haveDefaults, false);
   if (!haveDefaults)
   {
      definition.SaveUserPreset(FactoryDefaultsGroup());
      SetConfig(definition, PluginSettings::Private, FactoryDefaultsGroup(),
         wxT("Initialized"), true);
   }
   definition.LoadUserPreset(CurrentSettingsGroup());
}

ComponentInterface *LoadComponent(const PluginID &ID)
{
   if (auto result = dynamic_cast<EffectDefinitionInterfaceEx*>(
      PluginManager::Get().Load(ID))) {
      InitializePreset(*result);
      return result;
   }
   return nullptr;
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
      std::shared_ptr<Effect> hostEffect;
      // This will instantiate the effect client if it hasn't already been done
      const auto component = LoadComponent(ID);
      if (!component)
         return empty;

      if (auto effect = dynamic_cast<EffectUIHostInterface *>(component);
          effect && effect->Startup(nullptr))
         // Self-hosting or "legacy" effect objects
         return (mEffects[ID] = { effect, {} });
      else if (auto client = dynamic_cast<EffectUIClientInterface *>(component);
          client && (hostEffect = std::make_shared<Effect>())->Startup(client))
         // plugin that inherits only EffectUIClientInterface needs a host
         return (mEffects[ID] =
            { (mHostEffects[ID] = move(hostEffect)).get(), {} });
      else {
         if ( !dynamic_cast<AudacityCommand *>(component) )
            AudacityMessageBox(
               XO(
"Attempting to initialize the following effect failed:\n\n%s\n\nMore information may be available in 'Help > Diagnostics > Show Log'")
                  .Format( GetCommandName(ID) ),
               XO("Effect failed to initialize"));

         return empty;
      }
   }
}

AudacityCommand *EffectManager::GetAudacityCommand(const PluginID & ID)
{
   // Must have a "valid" ID
   if (ID.empty())
   {
      return NULL;
   }

   if (mCommands.find(ID) == mCommands.end()) {
      // This will instantiate the command if it hasn't already been done
      auto command =
         dynamic_cast<AudacityCommand *>(PluginManager::Get().Load(ID));
      if (command)
      {
         command->Init();
         mCommands[ID] = command;
         return command;
      }

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
   for (auto &plug
        : pm.PluginsOfType(PluginTypeEffect | PluginTypeAudacityCommand)) {
      auto &ID = plug.GetID();
      if (GetCommandIdentifier(ID) == strTarget)
         return ID;
   }
   return empty;
}

/* TODO:  fix the effect management so that repeated calls with the same ID
 give Effect objects with independent state for effect settings.
 */
std::unique_ptr<EffectProcessor>
EffectManager::NewEffect(const PluginID & ID)
{
   // Must have a "valid" ID
   if (ID.empty())
      return nullptr;

   // This will instantiate the effect client if it hasn't already been done
   // But it only makes a unique object for a given ID
   auto component = LoadComponent(ID);
   if (!component)
      return nullptr;

   auto effect = std::make_unique<Effect>();
   auto client = dynamic_cast<EffectUIClientInterface *>(component);
   if (client && effect->Startup(client))
      return effect;
   else
      return nullptr;
}
