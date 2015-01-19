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
#ifdef EFFECT_CATEGORIES
   mCategories = new CategoryMap();
   mRootCategories = new CategorySet();
   mUnsorted = new EffectSet();
   
   // Create effect category graph. These categories and relationships
   // are taken from revision 2 of lv2.ttl, loaders for other plugin systems 
   // (such as LADSPA/LRDF) should map their categories to these ones when 
   // applicable. Individual LADSPA/LRDF and LV2 plugins can add new 
   // categories and make them subcategories of the existing ones, but not 
   // add subcategory relationships between these categories.
   //
   // We need some persistent, global identifiers for categories - LRDF
   // and LV2 uses URI strings so we do that too. The URIs here are the
   // same ones as in lv2.ttl. Category identifiers in other plugin systems
   // must be mapped to URIs by their loaders.

#define LV2PREFIX "http://lv2plug.in/ns/lv2core#"
   
   typedef EffectCategory* CatPtr;
   
   CatPtr gen = AddCategory(wxT(LV2PREFIX) wxT("GeneratorPlugin"),
                            _("Generator"));
   CatPtr inst = AddCategory(wxT(LV2PREFIX) wxT("InstrumentPlugin"),
   /* i18n-hint: (noun).*/
                             _("Instrument"));
   CatPtr osc = AddCategory(wxT(LV2PREFIX) wxT("OscillatorPlugin"),
                            _("Oscillator"));
   CatPtr util = AddCategory(wxT(LV2PREFIX) wxT("UtilityPlugin"),
                             _("Utility"));
   CatPtr conv = AddCategory(wxT(LV2PREFIX) wxT("ConverterPlugin"),
                             _("Converter"));
   CatPtr anal = AddCategory(wxT(LV2PREFIX) wxT("AnalyserPlugin"),
                             _("Analyser"));
   CatPtr mix = AddCategory(wxT(LV2PREFIX) wxT("MixerPlugin"),
                            _("Mixer"));
   CatPtr sim = AddCategory(wxT(LV2PREFIX) wxT("SimulatorPlugin"),
                            _("Simulator"));
   CatPtr del = AddCategory(wxT(LV2PREFIX) wxT("DelayPlugin"),
                            _("Delay"));
   CatPtr mod = AddCategory(wxT(LV2PREFIX) wxT("ModulatorPlugin"),
                            _("Modulator"));
   CatPtr rev = AddCategory(wxT(LV2PREFIX) wxT("ReverbPlugin"),
                            _("Reverb"));
   CatPtr phas = AddCategory(wxT(LV2PREFIX) wxT("PhaserPlugin"),
                             _("Phaser"));
   CatPtr flng = AddCategory(wxT(LV2PREFIX) wxT("FlangerPlugin"),
                             _("Flanger"));
   CatPtr chor = AddCategory(wxT(LV2PREFIX) wxT("ChorusPlugin"),
                             _("Chorus"));
   CatPtr flt = AddCategory(wxT(LV2PREFIX) wxT("FilterPlugin"),
                            _("Filter"));
   CatPtr lp = AddCategory(wxT(LV2PREFIX) wxT("LowpassPlugin"),
                           _("Lowpass"));
   CatPtr bp = AddCategory(wxT(LV2PREFIX) wxT("BandpassPlugin"),
                           _("Bandpass"));
   CatPtr hp = AddCategory(wxT(LV2PREFIX) wxT("HighpassPlugin"),
                           _("Highpass"));
   CatPtr comb = AddCategory(wxT(LV2PREFIX) wxT("CombPlugin"),
                             _("Comb"));
   CatPtr alp = AddCategory(wxT(LV2PREFIX) wxT("AllpassPlugin"),
                            _("Allpass"));
   CatPtr eq = AddCategory(wxT(LV2PREFIX) wxT("EQPlugin"),
                           _("Equaliser"));
   CatPtr peq = AddCategory(wxT(LV2PREFIX) wxT("ParaEQPlugin"),
                            _("Parametric"));
   CatPtr meq = AddCategory(wxT(LV2PREFIX) wxT("MultiEQPlugin"),
                            _("Multiband"));
   CatPtr spec = AddCategory(wxT(LV2PREFIX) wxT("SpectralPlugin"),
                             _("Spectral Processor"));
   CatPtr ptch = AddCategory(wxT(LV2PREFIX) wxT("PitchPlugin"),
                             _("Pitch Shifter"));
   CatPtr amp = AddCategory(wxT(LV2PREFIX) wxT("AmplifierPlugin"),
                            _("Amplifier"));
   CatPtr dist = AddCategory(wxT(LV2PREFIX) wxT("DistortionPlugin"),
                             _("Distortion"));
   CatPtr shp = AddCategory(wxT(LV2PREFIX) wxT("WaveshaperPlugin"),
                            _("Waveshaper"));
   CatPtr dyn = AddCategory(wxT(LV2PREFIX) wxT("DynamicsPlugin"),
                            _("Dynamics Processor"));
   CatPtr cmp = AddCategory(wxT(LV2PREFIX) wxT("CompressorPlugin"),
                            _("Compressor"));
   CatPtr exp = AddCategory(wxT(LV2PREFIX) wxT("ExpanderPlugin"),
                            _("Expander"));
   CatPtr lim = AddCategory(wxT(LV2PREFIX) wxT("LimiterPlugin"),
                            _("Limiter"));
   CatPtr gate = AddCategory(wxT(LV2PREFIX) wxT("GatePlugin"),
                             _("Gate"));
   
   AddCategoryParent(inst, gen);
   AddCategoryParent(osc, gen);
   AddCategoryParent(conv, util);
   AddCategoryParent(anal, util);
   AddCategoryParent(mix, util);
   AddCategoryParent(rev, sim);
   AddCategoryParent(rev, del);
   AddCategoryParent(phas, mod);
   AddCategoryParent(flng, mod);
   AddCategoryParent(chor, mod);
   AddCategoryParent(lp, flt);
   AddCategoryParent(bp, flt);
   AddCategoryParent(hp, flt);
   AddCategoryParent(comb, flt);
   AddCategoryParent(alp, flt);
   AddCategoryParent(eq, flt);
   AddCategoryParent(peq, eq);
   AddCategoryParent(meq, eq);
   AddCategoryParent(ptch, spec);
   AddCategoryParent(shp, dist);
   AddCategoryParent(cmp, dyn);
   AddCategoryParent(exp, dyn);
   AddCategoryParent(lim, dyn);
   AddCategoryParent(gate, dyn);
   // We also add a couple of categories for internal use. These are not
   // in lv2.ttl.

#define ATEAM "http://audacityteam.org/namespace#"
   
   CatPtr nrm = AddCategory(wxT(ATEAM) wxT("NoiseRemoval"),
                            _("Noise Removal"));
   CatPtr pnt = AddCategory(wxT(ATEAM) wxT("PitchAndTempo"),
                            _("Pitch and Tempo"));
   CatPtr tim = AddCategory(wxT(ATEAM) wxT("TimelineChanger"),
                            _("Timeline Changer"));
   CatPtr aTim = AddCategory(wxT(ATEAM) wxT("TimeAnalyser"),
                             _("Time"));
   CatPtr onst = AddCategory(wxT(ATEAM) wxT("OnsetDetector"),
                             _("Onsets"));
   AddCategoryParent(nrm, util);
   AddCategoryParent(tim, util);
   AddCategoryParent(aTim, anal);
   AddCategoryParent(onst, aTim);
   
   // We freeze the internal subcategory relations between the categories
   // added so far so LADSPA/LRDF or other category systems don't ruin
   // our hierarchy.
   FreezeCategories();
   
#endif

#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   mRealtimeLock.Enter();
   mRealtimeActive = false;
   mRealtimeSuspended = true;
   mRealtimeLatency = 0;
   mRealtimeLock.Leave();
#endif

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   mRack = NULL;
#endif
}

EffectManager::~EffectManager()
{
#ifdef EFFECT_CATEGORIES
   CategoryMap::iterator i;
   for (i = mCategories->begin(); i != mCategories->end(); ++i)
      delete i->second;

   delete mUnsorted;
   delete mRootCategories;
   delete mCategories;
#endif

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   // wxWidgets has already destroyed the rack since it was derived from wxFrame. So
   // no need to delete it here.
#endif

   EffectMap::iterator iter = mEffects.begin();
   while (iter != mEffects.end())
   {
      delete iter->second;
      iter++;
   }
}

void EffectManager::RegisterEffect(ModuleInterface *p, Effect *f, int NewFlags)
{
   f->SetEffectID(mNumEffects++);

   if( NewFlags != 0)
   {
      f->SetEffectFlags( NewFlags );
   }

   mEffects[PluginManager::Get().RegisterEffectPlugin(p, f)] = f;
}

void EffectManager::RegisterEffect(Effect *f, int NewFlags)
{
   f->SetEffectID(mNumEffects++);

   if( NewFlags != 0)
   {
      f->SetEffectFlags( NewFlags );
   }

   // This will go away after all effects have been converted
   mEffects[PluginManager::Get().RegisterLegacyEffectPlugin(f)] = f;

#ifdef EFFECT_CATEGORIES
   // Add the effect in the right categories
   std::set<wxString> catUris = f->GetEffectCategories();
   bool oneValid = false;
   std::set<wxString>::const_iterator iter;
   for (iter = catUris.begin(); iter != catUris.end(); ++iter) {
      EffectCategory* cat = LookupCategory(*iter);
      if (cat != 0) {
         cat->AddEffect(f);
         oneValid = true;
      }
   }
   if (!oneValid)
      mUnsorted->insert(f);

#endif
}

void EffectManager::UnregisterEffects()
{
#ifdef EFFECT_CATEGORIES
   mUnsorted->clear();

   CategoryMap::iterator iter;
   for (iter = mCategories->begin(); iter != mCategories->end(); ++iter)
      iter->second->mEffects.clear();
#endif
}

bool EffectManager::DoEffect(const PluginID & ID,
                             wxWindow *parent,
                             int flags,
                             double projectRate,
                             TrackList *list,
                             TrackFactory *factory,
                             SelectedRegion *selectedRegion,
                             wxString params)
{
   Effect *effect = GetEffect(ID);
   
   if (!effect)
   {
      return false;
   }

#if defined(EXPERIMENTAL_REALTIME_EFFECTS) && defined(EXPERIMENTAL_EFFECTS_RACK)
   if (effect->SupportsRealtime())
   {
      GetRack()->Add(effect);
   }
#endif

   return effect->DoEffect(parent,
                           flags,
                           projectRate,
                           list,
                           factory,
                           selectedRegion,
                           params);
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
      return effect->GetEffectDescription();
   }

   return wxEmptyString;
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

      return parms;
   }

   return wxEmptyString;
}

bool EffectManager::SetEffectParameters(const PluginID & ID, const wxString & params)
{
   Effect *effect = GetEffect(ID);
   
   if (effect)
   {
      return effect->SetAutomationParameters(params);
   }

   return false;
}

bool EffectManager::PromptUser(const PluginID & ID, wxWindow *parent)
{
   Effect *effect = GetEffect(ID);
   bool result = false;

   if (effect)
   {
      result = effect->PromptUser(parent, true);
   }

   return result;
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

#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
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

#endif

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
   Effect *effect;

   // TODO: This is temporary and should be redone when all effects are converted
   if (mEffects.find(ID) == mEffects.end())
   {
      EffectIdentInterface *ident = dynamic_cast<EffectIdentInterface *>(PluginManager::Get().GetInstance(ID));
      if (ident && ident->IsLegacy())
      {
         effect = dynamic_cast<Effect *>(ident);
         effect->SetEffectID(mNumEffects++);
         mEffects[ID] = effect;
         return effect;
      }

      effect = new Effect();
      if (effect)
      {
         // This will instantiate the effect client if it hasn't already been done
         EffectClientInterface *client = dynamic_cast<EffectClientInterface *>(ident);
         if (client && effect->Startup(client))
         {
            effect->SetEffectID(mNumEffects++);
            mEffects[ID] = effect;
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

#ifdef EFFECT_CATEGORIES

EffectCategory* EffectManager::AddCategory(const wxString& URI,
                                           const wxString& name) {

   CategoryMap::const_iterator iter = mCategories->find(URI);
   if (iter != mCategories->end())
      return iter->second;
   EffectCategory* cat = new EffectCategory(URI, name);
   mCategories->insert(std::make_pair(URI, cat));
   mRootCategories->insert(cat);
   return cat;
}

EffectCategory* EffectManager::LookupCategory(const wxString& URI) {
   CategoryMap::const_iterator iter = mCategories->find(URI);
   if (iter != mCategories->end())
      return iter->second;
   return 0;
}

bool EffectManager::AddCategoryParent(EffectCategory* child,
                                      EffectCategory* parent) {
   bool result = child->AddParent(parent);
   if (!result)
      return false;
   CategorySet::iterator iter = mRootCategories->find(child);
   if (iter != mRootCategories->end())
      mRootCategories->erase(iter);
   return true;
}

void EffectManager::FreezeCategories() {
   CategoryMap::iterator iter;
   for (iter = mCategories->begin(); iter != mCategories->end(); ++iter)
      iter->second->FreezeParents();
}

const CategorySet& EffectManager::GetRootCategories() const {
   return *mRootCategories;
}

EffectSet EffectManager::GetUnsortedEffects(int flags) const {

   if (flags == ALL_EFFECTS)
      return *mUnsorted;

   EffectSet result;
   EffectSet::const_iterator iter;
   for (iter = mUnsorted->begin(); iter != mUnsorted->end(); ++iter) {
      int g = (*iter)->GetEffectFlags();
      if ((flags & g) == g)
         result.insert(*iter);
   }

   return result;
}

#endif
