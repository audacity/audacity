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

#include "../Experimental.h"

#include "Effect.h"

#include <algorithm>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>

#include "../widgets/AudacityMessageBox.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)
#include "EffectRack.h"
#endif

#include "../ShuttleGetDefinition.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/Command.h"
#include "../Menus.h"
#include "../MissingAliasFileDialog.h"
#include "../PluginManager.h"
#include "../ProjectHistory.h"
#include "../ProjectAudioManager.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "../TrackPanel.h"
#include "../ViewInfo.h"
#include "../WaveTrack.h"


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
#if defined(EXPERIMENTAL_EFFECTS_RACK)
   mRack = NULL;
#endif
}

EffectManager::~EffectManager()
{
#if defined(EXPERIMENTAL_EFFECTS_RACK)
   // wxWidgets has already destroyed the rack since it was derived from wxFrame. So
   // no need to DELETE it here.
#endif
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

/// DoEffect() takes a PluginID and has the EffectManager execute the associated
/// effect.
///
/// At the moment flags are used only to indicate whether to prompt for
//  parameters, whether to save the state to history and whether to allow
/// 'Repeat Last Effect'.

/* static */ bool EffectManager::DoEffect(
   const PluginID & ID, const CommandContext &context, unsigned flags )
{
   AudacityProject &project = context.project;
   const auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &trackFactory = TrackFactory::Get( project );
   auto rate = settings.GetRate();
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &commandManager = CommandManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   EffectType type = plug->GetEffectType();

   // Make sure there's no activity since the effect is about to be applied
   // to the project's tracks.  Mainly for Apply during RTP, but also used
   // for batch commands
   if (flags & EffectManager::kConfigured)
   {
      TransportActions::DoStop(project);
      SelectUtilities::SelectAllIfNone( project );
   }

   MissingAliasFilesDialog::SetShouldShow(true);

   auto nTracksOriginally = tracks.size();
   wxWindow *focus = wxWindow::FindFocus();
   wxWindow *parent = nullptr;
   if (focus != nullptr) {
      parent = focus->GetParent();
   }

   bool success = false;
   auto cleanup = finally( [&] {

      if (!success) {
         // For now, we're limiting realtime preview to a single effect, so
         // make sure the menus reflect that fact that one may have just been
         // opened.
         MenuManager::Get(project).UpdateMenus( false );
      }

   } );

   int count = 0;
   bool clean = true;
   for (auto t : tracks.Selected< const WaveTrack >()) {
      if (t->GetEndTime() != 0.0)
         clean = false;
      count++;
   }

   EffectManager & em = EffectManager::Get();

   success = em.DoEffect(ID, &window, rate,
      &tracks, &trackFactory, &selectedRegion,
      (flags & EffectManager::kConfigured) == 0);

   if (!success)
      return false;

   if (em.GetSkipStateFlag())
      flags = flags | EffectManager::kSkipState;

   if (!(flags & EffectManager::kSkipState))
   {
      wxString shortDesc = em.GetCommandName(ID);
      wxString longDesc = em.GetCommandDescription(ID);
      ProjectHistory::Get( project ).PushState(longDesc, shortDesc);
   }

   if (!(flags & EffectManager::kDontRepeatLast))
   {
      // Only remember a successful effect, don't remember insert,
      // or analyze effects.
      if (type == EffectTypeProcess) {
         wxString shortDesc = em.GetCommandName(ID);
         MenuManager::Get(project).mLastEffect = ID;
         wxString lastEffectDesc;
         /* i18n-hint: %s will be the name of the effect which will be
          * repeated if this menu item is chosen */
         lastEffectDesc.Printf(_("Repeat %s"), shortDesc);
         commandManager.Modify(wxT("RepeatLastEffect"), lastEffectDesc);
      }
   }

   //STM:
   //The following automatically re-zooms after sound was generated.
   // IMO, it was disorienting, removing to try out without re-fitting
   //mchinen:12/14/08 reapplying for generate effects
   if (type == EffectTypeGenerate)
   {
      if (count == 0 || (clean && selectedRegion.t0() == 0.0))
         window.DoZoomFit();
         //  trackPanel->Refresh(false);
   }
   window.RedrawProject();
   if (focus != nullptr && focus->GetParent()==parent) {
      focus->SetFocus();
   }

   // A fix for Bug 63
   // New tracks added?  Scroll them into view so that user sees them.
   // Don't care what track type.  An analyser might just have added a
   // Label track and we want to see it.
   if( tracks.size() > nTracksOriginally ){
      // 0.0 is min scroll position, 1.0 is max scroll position.
      trackPanel.VerticalScroll( 1.0 );
   }  else {
      trackPanel.EnsureVisible(trackPanel.GetFirstSelectedTrack());
      trackPanel.Refresh(false);
   }

   return true;
}

bool EffectManager::DoEffect(const PluginID & ID,
                             wxWindow *parent,
                             double projectRate,
                             TrackList *list,
                             TrackFactory *factory,
                             SelectedRegion *selectedRegion,
                             bool shouldPrompt /* = true */)

{
   this->SetSkipStateFlag(false);
   Effect *effect = GetEffect(ID);
   
   if (!effect)
   {
      return false;
   }

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   if (effect->SupportsRealtime())
   {
      GetRack()->Add(effect);
   }
#endif

   bool res = effect->DoEffect(parent,
                               projectRate,
                               list,
                               factory,
                               selectedRegion,
                               shouldPrompt);

   return res;
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

wxString EffectManager::GetCommandName(const PluginID & ID)
{
   return GetCommandSymbol(ID).Translation();
}

wxString EffectManager::GetEffectFamilyName(const PluginID & ID)
{
   auto effect = GetEffect(ID);
   if (effect)
      return effect->GetFamily().Translation();
   return {};
}

wxString EffectManager::GetVendorName(const PluginID & ID)
{
   auto effect = GetEffect(ID);
   if (effect)
      return effect->GetVendor().Translation();
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

wxString EffectManager::GetCommandDescription(const PluginID & ID)
{
   if (GetEffect(ID))
      return wxString::Format(_("Applied effect: %s"), GetCommandName(ID));
   if (GetAudacityCommand(ID))
      return wxString::Format(_("Applied command: %s"), GetCommandName(ID));

   return wxEmptyString;
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
      return pEff->GetDescription();
   AudacityCommand * pCom = GetAudacityCommand(ID);
   if( pCom )
      return pCom->GetDescription();

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
   S.AddItem( GetCommandName( ID ), "name" );
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

bool EffectManager::PromptUser(const PluginID & ID, wxWindow *parent)
{
   bool result = false;
   Effect *effect = GetEffect(ID);

   if (effect)
   {
      result = effect->PromptUser(parent);
      return result;
   }

   AudacityCommand *command = GetAudacityCommand(ID);

   if (command)
   {
      result = command->PromptUser(parent);
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

#if defined(EXPERIMENTAL_EFFECTS_RACK)
EffectRack *EffectManager::GetRack()
{
   if (!mRack)
   {
      // EffectRack is constructed with the current project as owner, so safenew is OK
      mRack = safenew EffectRack();
      // Make sure what I just commented remains true:
      wxASSERT(mRack->GetParent());
      mRack->CenterOnParent();
   }

   return mRack;
}

void EffectManager::ShowRack()
{
   GetRack()->Show(!GetRack()->IsShown());
}
#endif

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
         AudacityMessageBox(wxString::Format(_("Attempting to initialize the following effect failed:\n\n%s\n\nMore information may be available in Help->Show Log"),
                                    GetCommandName(ID)),
                   _("Effect failed to initialize"));

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
      AudacityMessageBox(wxString::Format(_("Attempting to initialize the following command failed:\n\n%s\n\nMore information may be available in Help->Show Log"),
                                    GetCommandName(ID)),
                   _("Command failed to initialize"));

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

