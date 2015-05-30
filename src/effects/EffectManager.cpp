/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include "../Audacity.h"

#include <wx/msgdlg.h>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>

#include "../Experimental.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)
#include "EffectRack.h"
#endif

#include "EffectManager.h"

// ============================================================================
//
// Create singleton and return reference
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================
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

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   mRack = NULL;
#endif
}

EffectManager::~EffectManager()
{
#if defined(EXPERIMENTAL_EFFECTS_RACK)
   // wxWidgets has already destroyed the rack since it was derived from wxFrame. So
   // no need to delete it here.
#endif

   EffectMap::iterator iter = mHostEffects.begin();
   while (iter != mHostEffects.end())
   {
      delete iter->second;
      ++iter;
   }
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
const PluginID & EffectManager::RegisterEffect(Effect *f)
{
   const PluginID & ID = PluginManager::Get().RegisterPlugin(f);

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

wxString EffectManager::GetEffectName(const PluginID & ID)
{
   return PluginManager::Get().GetName(ID);
}

wxString EffectManager::GetEffectIdentifier(const PluginID & ID)
{
   wxString name = (PluginManager::Get().GetSymbol(ID));

   // Get rid of leading and trailing white space
   name.Trim(true).Trim(false);

   if (name == wxEmptyString)
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

wxString EffectManager::GetEffectDescription(const PluginID & ID)
{
   Effect *effect = GetEffect(ID);

   if (effect)
   {
      return wxString::Format(_("Applied effect: %s"), effect->GetName().c_str());
   }

   return wxEmptyString;
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
      if (parms.IsEmpty())
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
   
   if (!effect)
   {
      return false;
   }

   EffectAutomationParameters eap(params);

   if (eap.HasEntry(wxT("Use Preset")))
   {
      return effect->SetAutomationParameters(eap.Read(wxT("Use Preset")));
   }

   return effect->SetAutomationParameters(params);
}

bool EffectManager::PromptUser(const PluginID & ID, wxWindow *parent)
{
   Effect *effect = GetEffect(ID);
   bool result = false;

   if (effect)
   {
      result = effect->PromptUser(parent);
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

   return effect->GetUserPresets().GetCount() > 0 ||
          effect->GetFactoryPresets().GetCount() > 0 ||
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

   EffectAutomationParameters eap(params);

   wxString preset;
   if (eap.HasEntry(wxT("Use Preset")))
   {
      preset = eap.Read(wxT("Use Preset"));
   }

   preset = effect->GetPreset(parent, preset);
   if (preset.IsEmpty())
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

   if (!preset.IsEmpty())
   {
      EffectAutomationParameters eap;

      eap.Write(wxT("Use Preset"), preset);
      eap.GetParameters(preset);
   }

   return preset;
}

void EffectManager::SetBatchProcessing(const PluginID & ID, bool start)
{
   Effect *effect = GetEffect(ID);

   if (!effect)
   {
      return;
   }

   effect->SetBatchProcessing(start);
}

#if defined(EXPERIMENTAL_EFFECTS_RACK)
EffectRack *EffectManager::GetRack()
{
   if (!mRack)
   {
      mRack = new EffectRack();
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
   int newCount = (int) effects.GetCount();
   Effect **newEffects = new Effect *[newCount];
   for (int i = 0; i < newCount; i++)
   {
      newEffects[i] = effects[i];
   }

   // Block RealtimeProcess()
   RealtimeSuspend();

   // Tell any effects no longer in the chain to clean up
   for (int i = 0; i < mRealtimeCount; i++)
   {
      Effect *e = mRealtimeEffects[i];

      // Scan the new chain for the effect
      for (int j = 0; j < newCount; j++)
      {
         // Found it so we're done
         if (e == newEffects[j])
         {
            e = NULL;
            break;
         }
      }

      // Must not have been in the new chain, so tell it to cleanup
      if (e && mRealtimeActive)
      {
         e->RealtimeFinalize();
      }
   }
      
   // Tell any new effects to get ready
   for (int i = 0; i < newCount; i++)
   {
      Effect *e = newEffects[i];

      // Scan the old chain for the effect
      for (int j = 0; j < mRealtimeCount; j++)
      {
         // Found it so tell effect to get ready
         if (e == mRealtimeEffects[j])
         {
            e = NULL;
         }
      }

      // Must not have been in the old chain, so tell it to initialize
      if (e && mRealtimeActive)
      {
         e->RealtimeInitialize();
      }
   }

   // Get rid of the old chain
   if (mRealtimeEffects)
   {
      delete [] mRealtimeEffects;
   }

   // And install the new one
   mRealtimeEffects = newEffects;
   mRealtimeCount = newCount;

   // Allow RealtimeProcess() to, well, process 
   RealtimeResume();
}
#endif

bool EffectManager::RealtimeIsActive()
{
   return mRealtimeEffects.GetCount() != 0;
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
      for (size_t i = 0, cnt = mRealtimeChans.GetCount(); i < cnt; i++)
      {
         effect->RealtimeAddProcessor(i, mRealtimeChans[i], mRealtimeRates[i]);
      }
   }
   
   // Add to list of active effects
   mRealtimeEffects.Add(effect);

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
   mRealtimeEffects.Remove(effect);

   // Allow RealtimeProcess() to, well, process 
   RealtimeResume();
}

void EffectManager::RealtimeInitialize()
{
   // The audio thread should not be running yet, but protect anyway
   RealtimeSuspend();

   // (Re)Set processor parameters
   mRealtimeChans.Clear();
   mRealtimeRates.Clear();

   // RealtimeAdd/RemoveEffect() needs to know when we're active so it can
   // initialize newly added effects
   mRealtimeActive = true;

   // Tell each effect to get ready for action
   for (int i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
   {
      mRealtimeEffects[i]->RealtimeInitialize();
   }

   // Get things moving
   RealtimeResume();
}

void EffectManager::RealtimeAddProcessor(int group, int chans, float rate)
{
   for (size_t i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
   {
      mRealtimeEffects[i]->RealtimeAddProcessor(group, chans, rate);
   }

   mRealtimeChans.Add(chans);
   mRealtimeRates.Add(rate);
}

void EffectManager::RealtimeFinalize()
{
   // Make sure nothing is going on
   RealtimeSuspend();

   // It is now safe to clean up
   mRealtimeLatency = 0;

   // Tell each effect to clean up as well
   for (int i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
   {
      mRealtimeEffects[i]->RealtimeFinalize();
   }

   // Reset processor parameters
   mRealtimeChans.Clear();
   mRealtimeRates.Clear();

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
   for (int i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
   {
      mRealtimeEffects[i]->RealtimeSuspend();
   }

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
   for (int i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
   {
      mRealtimeEffects[i]->RealtimeResume();
   }

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
      for (size_t i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
      {
         if (mRealtimeEffects[i]->IsRealtimeActive())
         {
            mRealtimeEffects[i]->RealtimeProcessStart();
         }
      }
   }

   mRealtimeLock.Leave();
}

//
// This will be called in a different thread than the main GUI thread.
//
sampleCount EffectManager::RealtimeProcess(int group, int chans, float **buffers, sampleCount numSamples)
{
   // Protect ourselves from the main thread
   mRealtimeLock.Enter();

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended, so allow the samples to pass as-is.
   if (mRealtimeSuspended || mRealtimeEffects.IsEmpty())
   {
      mRealtimeLock.Leave();
      return numSamples;
   }

   // Remember when we started so we can calculate the amount of latency we
   // are introducing
   wxMilliClock_t start = wxGetLocalTimeMillis();

   // Allocate the in/out buffer arrays
   float **ibuf = (float **) alloca(chans * sizeof(float *));
   float **obuf = (float **) alloca(chans * sizeof(float *));

   // And populate the input with the buffers we've been given while allocating
   // new output buffers
   for (int i = 0; i < chans; i++)
   {
      ibuf[i] = buffers[i];
      obuf[i] = (float *) alloca(numSamples * sizeof(float));
   }

   // Now call each effect in the chain while swapping buffer pointers to feed the
   // output of one effect as the input to the next effect
   size_t called = 0;
   for (size_t i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
   {
      if (mRealtimeEffects[i]->IsRealtimeActive())
      {
         mRealtimeEffects[i]->RealtimeProcess(group, chans, ibuf, obuf, numSamples);
         called++;
      }

      for (int j = 0; j < chans; j++)
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
      for (int i = 0; i < chans; i++)
      {
         memcpy(buffers[i], ibuf[i], numSamples * sizeof(float));
      }
   }

   // Remember the latency
   mRealtimeLatency = (int) (wxGetLocalTimeMillis() - start).GetValue();

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
      for (size_t i = 0, cnt = mRealtimeEffects.GetCount(); i < cnt; i++)
      {
         if (mRealtimeEffects[i]->IsRealtimeActive())
         {
            mRealtimeEffects[i]->RealtimeProcessEnd();
         }
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
   if (ID.IsEmpty())
   {
      return NULL;
   }

   // TODO: This is temporary and should be redone when all effects are converted
   if (mEffects.find(ID) == mEffects.end())
   {
      Effect *effect;

      // This will instantiate the effect client if it hasn't already been done
      EffectIdentInterface *ident = dynamic_cast<EffectIdentInterface *>(PluginManager::Get().GetInstance(ID));
      if (ident && ident->IsLegacy())
      {
         effect = dynamic_cast<Effect *>(ident);
         if (effect && effect->Startup(NULL))
         {
            mEffects[ID] = effect;
            return effect;
         }
      }

      effect = new Effect();
      if (effect)
      {
         EffectClientInterface *client = dynamic_cast<EffectClientInterface *>(ident);
         if (client && effect->Startup(client))
         {
            mEffects[ID] = effect;
            mHostEffects[ID] = effect;
            return effect;
         }

         delete effect;
      }

      wxMessageBox(wxString::Format(_("Attempting to initialize the following effect failed:\n\n%s\n\nMore information may be available in Help->Show Log"),
                                    PluginManager::Get().GetName(ID).c_str()),
                   _("Effect failed to initialize"));

      return NULL;
   }

   return mEffects[ID];
}

const PluginID & EffectManager::GetEffectByIdentifier(const wxString & strTarget)
{
   static PluginID empty;
   if (strTarget == wxEmptyString) // set GetEffectIdentifier to wxT("") to not show an effect in Batch mode
   {
      return empty;
   }

   PluginManager & pm = PluginManager::Get();
   const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect);
   while (plug)
   {
      if (GetEffectIdentifier(plug->GetID()).IsSameAs(strTarget))
      {
         return plug->GetID();
      }
      plug = pm.GetNextPlugin(PluginTypeEffect);
   }

   return empty;;
}

