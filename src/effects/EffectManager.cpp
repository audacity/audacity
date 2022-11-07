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

#include "ConfigInterface.h"
#include "../ShuttleGetDefinition.h"
#include "../commands/CommandContext.h"
#include "../commands/AudacityCommand.h"
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
   const EffectSettingsManager *effect = nullptr;
   const EffectSettings *settings;
   AudacityCommand *command = nullptr;

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

   AudacityCommand *command = GetAudacityCommand(ID);
   
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
   AudacityCommand *command = GetAudacityCommand(ID);
   
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
   const PluginID & ID, const EffectDialogFactory &factory, wxWindow &parent)
{
   bool result = false;
   if (auto effect = GetEffect(ID)) {

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
         result = effect->ShowHostInterface(
            parent, factory,
            pInstance,
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

static bool HasCurrentSettings(EffectPlugin &host)
{
   return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
      CurrentSettingsGroup());
}

static bool HasFactoryDefaults(EffectPlugin &host)
{
   return HasConfigGroup(host.GetDefinition(), PluginSettings::Private,
      FactoryDefaultsGroup());
}

static RegistryPaths GetUserPresets(EffectPlugin &host)
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
   EffectPresetsDialog(wxWindow *parent, EffectPlugin *effect);
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
   wxWindow *parent, EffectPlugin *effect)
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
   if (preset.StartsWith(EffectPlugin::kUserPresetIdent))
   {
      preset.Replace(EffectPlugin::kUserPresetIdent, wxEmptyString, false);
      SetPrefix(XO("User Presets"), preset);
   }
   else if (preset.StartsWith(EffectPlugin::kFactoryPresetIdent))
   {
      preset.Replace(EffectPlugin::kFactoryPresetIdent, wxEmptyString, false);
      SetPrefix(XO("Factory Presets"), preset);
   }
   else if (preset.StartsWith(EffectPlugin::kCurrentSettingsIdent))
   {
      SetPrefix(XO("Current Settings"), wxEmptyString);
   }
   else if (preset.StartsWith(EffectPlugin::kFactoryDefaultsIdent))
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
      mSelection = EffectPlugin::kUserPresetIdent
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
      mSelection = EffectPlugin::kFactoryPresetIdent
         + mPresets->GetStringSelection();
   }
   else if (type == XO("Current Settings"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectPlugin::kCurrentSettingsIdent;
   }
   else if (type == XO("Factory Defaults"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectPlugin::kFactoryDefaultsIdent;
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
      mSelection = EffectPlugin::kUserPresetIdent
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
      mSelection = EffectPlugin::kFactoryPresetIdent
         + mPresets->GetString(selected);
   }
   else if (type == _("Current Settings"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectPlugin::kCurrentSettingsIdent;
   }
   else if (type == _("Factory Defaults"))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = EffectPlugin::kFactoryDefaultsIdent;
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

// This function is used only in the macro programming user interface
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

EffectPlugin *EffectManager::GetEffect(const PluginID & ID)
{
   return DoGetEffect(ID).effect;
}

EffectSettings *EffectManager::GetDefaultSettings(const PluginID & ID)
{
   return GetEffectAndDefaultSettings(ID).second;
}

std::pair<EffectPlugin *, EffectSettings *>
EffectManager::GetEffectAndDefaultSettings(const PluginID & ID)
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

      if (auto effect = dynamic_cast<EffectPlugin *>(component))
         return (mEffects[ID] = { effect, std::move(settings) });
      else if (auto client = dynamic_cast<EffectUIClientInterface *>(component)) {
         // Nothing inherits EffectUIClientInterface now that does not also
         // inherit EffectPlugin
         wxASSERT(false);
         return empty;
      }
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

const EffectInstanceFactory *
EffectManager::GetInstanceFactory(const PluginID &ID)
{
   return Get().GetEffect(ID);
}
