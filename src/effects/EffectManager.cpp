/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectManager.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

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
   mRealtimeMutex.Lock();
   mRealtimeEffects = NULL;
   mRealtimeCount = 0;
   mRealtimeActive = false;
   mRealtimeSuspended = false;
   mRealtimeMutex.Unlock();
   mRealtimeLatency = 0;
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

#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
   if (mRealtimeEffects)
   {
      delete [] mRealtimeEffects;
   }
#endif

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   // wxWidgets has already destroyed the rack since it was derived from wxFrame. So
   // no need to delete it here.
#endif
}

void EffectManager::RegisterEffect(Effect *f, int NewFlags)
{
   f->SetEffectID(mNumEffects++);

   if( NewFlags != 0)
   {
      f->SetEffectFlags( NewFlags );
   }

   // This will go away after all effects have been converted
   mEffectPlugins.Add(PluginManager::Get().RegisterLegacyEffectPlugin(f));

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
   if (effect->IsRealtimeCapable())
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
   wxString name = (PluginManager::Get().GetName(ID));

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

wxString EffectManager::GetEffectParameters(const PluginID & ID)
{
   Effect *effect = GetEffect(ID);
   
   if (effect)
   {
      ShuttleCli shuttle;
      shuttle.mbStoreInClient = false;
      effect->TransferParameters(shuttle);
      return shuttle.mParams;
   }

   return wxEmptyString;
}

bool EffectManager::SetEffectParameters(const PluginID & ID, const wxString & params)
{
   Effect *effect = GetEffect(ID);
   
   if (effect)
   {
      ShuttleCli shuttle;
      shuttle.mParams = params;
      shuttle.mbStoreInClient=true;
      return effect->TransferParameters(shuttle);
   }

   return false;
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
   GetRack()->Show();
}
#endif

#if defined(EXPERIMENTAL_REALTIME_EFFECTS)
void EffectManager::RealtimeInitialize(int numChannels, float sampleRate)
{
   mRealtimeMutex.Lock();
   for (int i = 0; i < mRealtimeCount; i++)
   {
      mRealtimeEffects[i]->RealtimeInitialize(numChannels, sampleRate);
   }
   mRealtimeMutex.Unlock();

   mRealtimeActive = true;

   RealtimeResume();
}

void EffectManager::RealtimeFinalize()
{
   RealtimeSuspend();

   mRealtimeActive = false;
   mRealtimeLatency = 0;

   mRealtimeMutex.Lock();
   for (int i = 0; i < mRealtimeCount; i++)
   {
      mRealtimeEffects[i]->RealtimeFinalize();
   }
   mRealtimeMutex.Unlock();
}

void EffectManager::RealtimeSuspend()
{
   mRealtimeSuspended = true;

   mRealtimeMutex.Lock();
   for (int i = 0; i < mRealtimeCount; i++)
   {
      mRealtimeEffects[i]->RealtimeSuspend();
   }
   mRealtimeMutex.Unlock();
}

void EffectManager::RealtimeResume()
{
   mRealtimeMutex.Lock();
   for (int i = 0; i < mRealtimeCount; i++)
   {
      mRealtimeEffects[i]->RealtimeResume();
   }
   mRealtimeMutex.Unlock();

   mRealtimeSuspended = false;
}

void EffectManager::RealtimeProcessMono(float *buffer, sampleCount numSamples)
{
   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended.
   if (mRealtimeSuspended)
   {
      return;
   }

   // We only ever have a single input channel

   wxMilliClock_t start = wxGetLocalTimeMillis();

   float *ib = (float *) alloca(sizeof(float) * numSamples);
   float *ob = (float *) alloca(sizeof(float) * numSamples);

   memcpy(ib, buffer, sizeof(float) * numSamples);

   float *ibuf = ib;
   float *obuf = ob;

   mRealtimeMutex.Lock();
   for (int i = 0; i < mRealtimeCount; i++)
   {
      mRealtimeEffects[i]->RealtimeProcess(&ibuf, &obuf, numSamples);

      float *tbuf = ibuf;
      ibuf = obuf;
      obuf = tbuf;
   }
   mRealtimeMutex.Unlock();

   memcpy(buffer, ibuf, sizeof(float) * numSamples);

   mRealtimeLatency = (int) (wxGetLocalTimeMillis() - start).GetValue();
}

void EffectManager::RealtimeProcessStereo(float *buffer, sampleCount numSamples)
{
   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended.
   if (mRealtimeSuspended)
   {
      return;
   }

   wxMilliClock_t start = wxGetLocalTimeMillis();

   float *ilc = (float *) alloca(sizeof(float) * numSamples);
   float *irc = (float *) alloca(sizeof(float) * numSamples);
   float *olc = (float *) alloca(sizeof(float) * numSamples);
   float *orc = (float *) alloca(sizeof(float) * numSamples);

   for (int opos = 0, ipos = 0; opos < numSamples; opos++, ipos += 2)
   {
      ilc[opos] = buffer[ipos];
      irc[opos] = buffer[ipos+1];
   }

   float *ibuf[2] = {ilc, irc};
   float *obuf[2] = {olc, orc};

   mRealtimeMutex.Lock();
   for (int i = 0; i < mRealtimeCount; i++)
   {
      mRealtimeEffects[i]->RealtimeProcess(ibuf, obuf, numSamples);

      float *tbuf[2] = {ibuf[0], ibuf[1]};
      ibuf[0] = obuf[0];
      ibuf[1] = obuf[1];
      obuf[0] = tbuf[0];
      obuf[1] = tbuf[1];
   }
   mRealtimeMutex.Unlock();

   for (int opos = 0, ipos = 0; ipos < numSamples; ipos++, opos += 2)
   {
      buffer[opos] = ibuf[0][ipos] > 1.0 ? 1.0 : ibuf[0][ipos];
      buffer[opos+1] = ibuf[1][ipos] > 1.0 ? 1.0 : ibuf[1][ipos];
   }

   mRealtimeLatency = (int) (wxGetLocalTimeMillis() - start).GetValue();
}

int EffectManager::GetRealtimeLatency()
{
   return mRealtimeLatency;
}

void EffectManager::SetRealtime(const EffectArray & effects)
{
   Effect **rteffects = new Effect *[effects.GetCount()];
   if (rteffects)
   {
      for (int i = 0, cnt = effects.GetCount(); i < cnt; i++)
      {
         rteffects[i] = effects[i];
      }

      mRealtimeMutex.Lock();
      Effect **rtold = mRealtimeEffects;
      mRealtimeEffects = rteffects;
      mRealtimeCount = effects.GetCount();
      mRealtimeMutex.Unlock();
      if (rtold)
      {
         delete [] rtold;
      }
   }
}
#endif

Effect *EffectManager::GetEffect(const PluginID & ID)
{
   Effect *effect;

   // TODO: This is temporary and should be redone when all effects are converted
   if (mEffectPlugins.Index(wxString(ID)) == wxNOT_FOUND)
   {
      // This will instantiate the effect client if it hasn't already been done
      EffectClientInterface *client = static_cast<EffectClientInterface *>(PluginManager::Get().GetInstance(ID));
      if (client)
      {
         effect = new Effect(client);
         if (effect)
         {
            effect->SetEffectID(mNumEffects++);
            PluginManager::Get().SetInstance(ID, effect);
            mEffectPlugins.Add(ID);
         }

         return effect;
      }
      return NULL;
   }

   return static_cast<Effect *>(PluginManager::Get().GetInstance(ID));
}

const PluginID & EffectManager::GetEffectByIdentifier(const wxString & strTarget)
{
   if (strTarget == wxEmptyString) // set GetEffectIdentifier to wxT("") to not show an effect in Batch mode
   {
      return PluginID(wxEmptyString);
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

   return PluginID(wxEmptyString);
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
