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

#include "../Experimental.h"

#include <algorithm>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>

#include "../widgets/ErrorDialog.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)
#include "EffectRack.h"
#endif

#include "EffectManager.h"
#include "../Shuttle.h"
#include "../commands/Command.h"
#include "../commands/CommandContext.h"
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
   mRealtimeLock.Enter();
   mRealtimeActive = false;
   mRealtimeSuspended = true;
   mRealtimeLatency = 0;
   mRealtimeLock.Leave();
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
   S.AddItem( GetCommandIdentifier( ID ), "id" );
   S.AddItem( GetCommandName( ID ), "name" );
   if( bHasParams ){
      S.StartField( "params" );
      S.StartArray();
      command->DefineParams( S );
      S.EndArray();
      S.EndField();
   }
   S.AddItem( GetCommandUrl( ID ), "url" );
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

void EffectManager::RealtimeSetEffects(const EffectArray & effects)
{
   // Block RealtimeProcess()
   RealtimeSuspend();

   // Tell any effects no longer in the chain to clean up
   for (auto e: mRealtimeEffects)
   {
      // Scan the NEW chain for the effect
      for (auto e1: effects)
      {
         // Found it so we're done
         if (e == e1)
         {
            e = NULL;
            break;
         }
      }

      // Must not have been in the NEW chain, so tell it to cleanup
      if (e && mRealtimeActive)
      {
         e->RealtimeFinalize();
      }
   }
      
   // Tell any NEW effects to get ready
   for (auto e : effects)
   {
      // Scan the old chain for the effect
      for (auto e1 : mRealtimeEffects)
      {
         // Found it so tell effect to get ready
         if (e == e1)
         {
            e = NULL;
            break;
         }
      }

      // Must not have been in the old chain, so tell it to initialize
      if (e && mRealtimeActive)
      {
         e->RealtimeInitialize();
      }
   }

   // Get rid of the old chain
   // And install the NEW one
   mRealtimeEffects = effects;

   // Allow RealtimeProcess() to, well, process 
   RealtimeResume();
}
#endif

bool EffectManager::RealtimeIsActive()
{
   return mRealtimeEffects.size() != 0;
}

bool EffectManager::RealtimeIsSuspended()
{
   return mRealtimeSuspended;
}

void EffectManager::RealtimeAddEffect(Effect *effect)
{
   // Block RealtimeProcess()
   RealtimeSuspend();

   // Initialize effect if realtime is already active
   if (mRealtimeActive)
   {
      // Initialize realtime processing
      effect->RealtimeInitialize();

      // Add the required processors
      for (size_t i = 0, cnt = mRealtimeChans.size(); i < cnt; i++)
      {
         effect->RealtimeAddProcessor(i, mRealtimeChans[i], mRealtimeRates[i]);
      }
   }
   
   // Add to list of active effects
   mRealtimeEffects.push_back(effect);

   // Allow RealtimeProcess() to, well, process 
   RealtimeResume();
}

void EffectManager::RealtimeRemoveEffect(Effect *effect)
{
   // Block RealtimeProcess()
   RealtimeSuspend();

   if (mRealtimeActive)
   {
      // Cleanup realtime processing
      effect->RealtimeFinalize();
   }
      
   // Remove from list of active effects
   auto end = mRealtimeEffects.end();
   auto found = std::find(mRealtimeEffects.begin(), end, effect);
   if (found != end)
      mRealtimeEffects.erase(found);

   // Allow RealtimeProcess() to, well, process 
   RealtimeResume();
}

void EffectManager::RealtimeInitialize(double rate)
{
   // The audio thread should not be running yet, but protect anyway
   RealtimeSuspend();

   // (Re)Set processor parameters
   mRealtimeChans.clear();
   mRealtimeRates.clear();

   // RealtimeAdd/RemoveEffect() needs to know when we're active so it can
   // initialize newly added effects
   mRealtimeActive = true;

   // Tell each effect to get ready for action
   for (auto e : mRealtimeEffects) {
      e->SetSampleRate(rate);
      e->RealtimeInitialize();
   }

   // Get things moving
   RealtimeResume();
}

void EffectManager::RealtimeAddProcessor(int group, unsigned chans, float rate)
{
   for (auto e : mRealtimeEffects)
      e->RealtimeAddProcessor(group, chans, rate);

   mRealtimeChans.push_back(chans);
   mRealtimeRates.push_back(rate);
}

void EffectManager::RealtimeFinalize()
{
   // Make sure nothing is going on
   RealtimeSuspend();

   // It is now safe to clean up
   mRealtimeLatency = 0;

   // Tell each effect to clean up as well
   for (auto e : mRealtimeEffects)
      e->RealtimeFinalize();

   // Reset processor parameters
   mRealtimeChans.clear();
   mRealtimeRates.clear();

   // No longer active
   mRealtimeActive = false;
}

void EffectManager::RealtimeSuspend()
{
   mRealtimeLock.Enter();

   // Already suspended...bail
   if (mRealtimeSuspended)
   {
      mRealtimeLock.Leave();
      return;
   }

   // Show that we aren't going to be doing anything
   mRealtimeSuspended = true;

   // And make sure the effects don't either
   for (auto e : mRealtimeEffects)
      e->RealtimeSuspend();

   mRealtimeLock.Leave();
}

void EffectManager::RealtimeResume()
{
   mRealtimeLock.Enter();

   // Already running...bail
   if (!mRealtimeSuspended)
   {
      mRealtimeLock.Leave();
      return;
   }

   // Tell the effects to get ready for more action
   for (auto e : mRealtimeEffects)
      e->RealtimeResume();

   // And we should too
   mRealtimeSuspended = false;

   mRealtimeLock.Leave();
}

//
// This will be called in a different thread than the main GUI thread.
//
void EffectManager::RealtimeProcessStart()
{
   // Protect ourselves from the main thread
   mRealtimeLock.Enter();

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended.
   if (!mRealtimeSuspended)
   {
      for (auto e : mRealtimeEffects)
      {
         if (e->IsRealtimeActive())
            e->RealtimeProcessStart();
      }
   }

   mRealtimeLock.Leave();
}

//
// This will be called in a different thread than the main GUI thread.
//
size_t EffectManager::RealtimeProcess(int group, unsigned chans, float **buffers, size_t numSamples)
{
   // Protect ourselves from the main thread
   mRealtimeLock.Enter();

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended, so allow the samples to pass as-is.
   if (mRealtimeSuspended || mRealtimeEffects.empty())
   {
      mRealtimeLock.Leave();
      return numSamples;
   }

   // Remember when we started so we can calculate the amount of latency we
   // are introducing
   wxMilliClock_t start = wxGetUTCTimeMillis();

   // Allocate the in/out buffer arrays
   float **ibuf = (float **) alloca(chans * sizeof(float *));
   float **obuf = (float **) alloca(chans * sizeof(float *));

   // And populate the input with the buffers we've been given while allocating
   // NEW output buffers
   for (unsigned int i = 0; i < chans; i++)
   {
      ibuf[i] = buffers[i];
      obuf[i] = (float *) alloca(numSamples * sizeof(float));
   }

   // Now call each effect in the chain while swapping buffer pointers to feed the
   // output of one effect as the input to the next effect
   size_t called = 0;
   for (auto e : mRealtimeEffects)
   {
      if (e->IsRealtimeActive())
      {
         e->RealtimeProcess(group, chans, ibuf, obuf, numSamples);
         called++;
      }

      for (unsigned int j = 0; j < chans; j++)
      {
         float *temp;
         temp = ibuf[j];
         ibuf[j] = obuf[j];
         obuf[j] = temp;
      }
   }

   // Once we're done, we might wind up with the last effect storing its results
   // in the temporary buffers.  If that's the case, we need to copy it over to
   // the caller's buffers.  This happens when the number of effects proccessed
   // is odd.
   if (called & 1)
   {
      for (unsigned int i = 0; i < chans; i++)
      {
         memcpy(buffers[i], ibuf[i], numSamples * sizeof(float));
      }
   }

   // Remember the latency
   mRealtimeLatency = (int) (wxGetUTCTimeMillis() - start).GetValue();

   mRealtimeLock.Leave();

   //
   // This is wrong...needs to handle tails
   //
   return numSamples;
}

//
// This will be called in a different thread than the main GUI thread.
//
void EffectManager::RealtimeProcessEnd()
{
   // Protect ourselves from the main thread
   mRealtimeLock.Enter();

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended.
   if (!mRealtimeSuspended)
   {
      for (auto e : mRealtimeEffects)
      {
         if (e->IsRealtimeActive())
            e->RealtimeProcessEnd();
      }
   }

   mRealtimeLock.Leave();
}

int EffectManager::GetRealtimeLatency()
{
   return mRealtimeLatency;
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
      if (GetCommandIdentifier(plug->GetID()).IsSameAs(strTarget, false))
      {
         return plug->GetID();
      }
      plug = pm.GetNextPlugin(PluginTypeEffect | PluginTypeAudacityCommand);
   }
   return empty;;
}

