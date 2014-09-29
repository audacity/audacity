/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"
#include "../Prefs.h"

#include "LoadEffects.h"

#include "EffectManager.h"

#include "Amplify.h"
// #include "AvcCompressor.h"
#include "AutoDuck.h"
#include "BassTreble.h"
#include "ChangeSpeed.h"
#include "ClickRemoval.h"
#include "Compressor.h"
#include "DtmfGen.h"
#include "Echo.h"
#include "Paulstretch.h"
#include "Equalization.h"
#include "Fade.h"
#include "Invert.h"
#include "Leveller.h"
#include "Noise.h"
#include "NoiseRemoval.h"
#include "Normalize.h"
#include "Phaser.h"
#include "Repair.h"
#include "Repeat.h"
#include "Reverb.h"
#include "Reverse.h"
#include "Silence.h"
#include "ScienFilter.h"
#include "StereoToMono.h"
#ifdef USE_SBSMS
#include "TimeScale.h"
#endif
#include "ToneGen.h"
#include "TruncSilence.h"
#include "Wahwah.h"

#include "FindClipping.h"

#ifdef USE_SOUNDTOUCH
#include "ChangePitch.h"
#include "ChangeTempo.h"
#endif

#ifdef USE_NYQUIST
#include "nyquist/LoadNyquist.h"
#endif

#ifdef USE_AUDIO_UNITS
#include "audiounits/LoadAudioUnits.h"
#endif

#ifdef USE_VST
#include "VST/VSTEffect.h"
#endif

#ifdef USE_LADSPA
#include "ladspa/LoadLadspa.h"
#endif

#ifdef USE_LV2
#include "lv2/LoadLV2.h"
#endif

#ifdef USE_VAMP
#include "vamp/LoadVamp.h"
#endif


void LoadEffects()
{

   EffectManager& em = EffectManager::Get();

#ifdef EFFECT_CATEGORIES

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

   CatPtr gen = em.AddCategory(wxT(LV2PREFIX) wxT("GeneratorPlugin"),
                               _("Generator"));
   CatPtr inst = em.AddCategory(wxT(LV2PREFIX) wxT("InstrumentPlugin"),
   /* i18n-hint: (noun).*/
                                _("Instrument"));
   CatPtr osc = em.AddCategory(wxT(LV2PREFIX) wxT("OscillatorPlugin"),
                               _("Oscillator"));
   CatPtr util = em.AddCategory(wxT(LV2PREFIX) wxT("UtilityPlugin"),
                                _("Utility"));
   CatPtr conv = em.AddCategory(wxT(LV2PREFIX) wxT("ConverterPlugin"),
                                _("Converter"));
   CatPtr anal = em.AddCategory(wxT(LV2PREFIX) wxT("AnalyserPlugin"),
                                _("Analyser"));
   CatPtr mix = em.AddCategory(wxT(LV2PREFIX) wxT("MixerPlugin"),
                               _("Mixer"));
   CatPtr sim = em.AddCategory(wxT(LV2PREFIX) wxT("SimulatorPlugin"),
                               _("Simulator"));
   CatPtr del = em.AddCategory(wxT(LV2PREFIX) wxT("DelayPlugin"),
                               _("Delay"));
   CatPtr mod = em.AddCategory(wxT(LV2PREFIX) wxT("ModulatorPlugin"),
                               _("Modulator"));
   CatPtr rev = em.AddCategory(wxT(LV2PREFIX) wxT("ReverbPlugin"),
                               _("Reverb"));
   CatPtr phas = em.AddCategory(wxT(LV2PREFIX) wxT("PhaserPlugin"),
                                _("Phaser"));
   CatPtr flng = em.AddCategory(wxT(LV2PREFIX) wxT("FlangerPlugin"),
                                _("Flanger"));
   CatPtr chor = em.AddCategory(wxT(LV2PREFIX) wxT("ChorusPlugin"),
                                _("Chorus"));
   CatPtr flt = em.AddCategory(wxT(LV2PREFIX) wxT("FilterPlugin"),
                               _("Filter"));
   CatPtr lp = em.AddCategory(wxT(LV2PREFIX) wxT("LowpassPlugin"),
                              _("Lowpass"));
   CatPtr bp = em.AddCategory(wxT(LV2PREFIX) wxT("BandpassPlugin"),
                              _("Bandpass"));
   CatPtr hp = em.AddCategory(wxT(LV2PREFIX) wxT("HighpassPlugin"),
                              _("Highpass"));
   CatPtr comb = em.AddCategory(wxT(LV2PREFIX) wxT("CombPlugin"),
                                _("Comb"));
   CatPtr alp = em.AddCategory(wxT(LV2PREFIX) wxT("AllpassPlugin"),
                               _("Allpass"));
   CatPtr eq = em.AddCategory(wxT(LV2PREFIX) wxT("EQPlugin"),
                              _("Equaliser"));
   CatPtr peq = em.AddCategory(wxT(LV2PREFIX) wxT("ParaEQPlugin"),
                               _("Parametric"));
   CatPtr meq = em.AddCategory(wxT(LV2PREFIX) wxT("MultiEQPlugin"),
                               _("Multiband"));
   CatPtr spec = em.AddCategory(wxT(LV2PREFIX) wxT("SpectralPlugin"),
                                _("Spectral Processor"));
   CatPtr ptch = em.AddCategory(wxT(LV2PREFIX) wxT("PitchPlugin"),
                                _("Pitch Shifter"));
   CatPtr amp = em.AddCategory(wxT(LV2PREFIX) wxT("AmplifierPlugin"),
                               _("Amplifier"));
   CatPtr dist = em.AddCategory(wxT(LV2PREFIX) wxT("DistortionPlugin"),
                                _("Distortion"));
   CatPtr shp = em.AddCategory(wxT(LV2PREFIX) wxT("WaveshaperPlugin"),
                               _("Waveshaper"));
   CatPtr dyn = em.AddCategory(wxT(LV2PREFIX) wxT("DynamicsPlugin"),
                               _("Dynamics Processor"));
   CatPtr cmp = em.AddCategory(wxT(LV2PREFIX) wxT("CompressorPlugin"),
                               _("Compressor"));
   CatPtr exp = em.AddCategory(wxT(LV2PREFIX) wxT("ExpanderPlugin"),
                               _("Expander"));
   CatPtr lim = em.AddCategory(wxT(LV2PREFIX) wxT("LimiterPlugin"),
                               _("Limiter"));
   CatPtr gate = em.AddCategory(wxT(LV2PREFIX) wxT("GatePlugin"),
                                _("Gate"));

   em.AddCategoryParent(inst, gen);
   em.AddCategoryParent(osc, gen);
   em.AddCategoryParent(conv, util);
   em.AddCategoryParent(anal, util);
   em.AddCategoryParent(mix, util);
   em.AddCategoryParent(rev, sim);
   em.AddCategoryParent(rev, del);
   em.AddCategoryParent(phas, mod);
   em.AddCategoryParent(flng, mod);
   em.AddCategoryParent(chor, mod);
   em.AddCategoryParent(lp, flt);
   em.AddCategoryParent(bp, flt);
   em.AddCategoryParent(hp, flt);
   em.AddCategoryParent(comb, flt);
   em.AddCategoryParent(alp, flt);
   em.AddCategoryParent(eq, flt);
   em.AddCategoryParent(peq, eq);
   em.AddCategoryParent(meq, eq);
   em.AddCategoryParent(ptch, spec);
   em.AddCategoryParent(shp, dist);
   em.AddCategoryParent(cmp, dyn);
   em.AddCategoryParent(exp, dyn);
   em.AddCategoryParent(lim, dyn);
   em.AddCategoryParent(gate, dyn);

   // We also add a couple of categories for internal use. These are not
   // in lv2.ttl.

#define ATEAM "http://audacityteam.org/namespace#"

   CatPtr nrm = em.AddCategory(wxT(ATEAM) wxT("NoiseRemoval"),
                               _("Noise Removal"));
   CatPtr pnt = em.AddCategory(wxT(ATEAM) wxT("PitchAndTempo"),
                               _("Pitch and Tempo"));
   CatPtr tim = em.AddCategory(wxT(ATEAM) wxT("TimelineChanger"),
                               _("Timeline Changer"));
   CatPtr aTim = em.AddCategory(wxT(ATEAM) wxT("TimeAnalyser"),
                                _("Time"));
   CatPtr onst = em.AddCategory(wxT(ATEAM) wxT("OnsetDetector"),
                                _("Onsets"));
   em.AddCategoryParent(nrm, util);
   em.AddCategoryParent(tim, util);
   em.AddCategoryParent(aTim, anal);
   em.AddCategoryParent(onst, aTim);

   // We freeze the internal subcategory relations between the categories
   // added so far so LADSPA/LRDF or other category systems don't ruin
   // our hierarchy.
   em.FreezeCategories();

#endif

   // Generate menu
   em.RegisterEffect(new EffectNoise());
   em.RegisterEffect(new EffectSilence());
   em.RegisterEffect(new EffectToneGen());
   em.RegisterEffect(new EffectDtmf());
   // A little magic to convert 'Tone' to chirps.
   em.RegisterEffect(&((new EffectToneGen())->EnableForChirps()));

   // Effect menu

   em.RegisterEffect(new EffectAmplify());

   //Commented out now that the Compressor effect works better
   //em.RegisterEffect(new EffectAvcCompressor());

   const int SIMPLE_EFFECT = BUILTIN_EFFECT | PROCESS_EFFECT;
   // In this list, designating an effect as 'SIMPLE_EFFECT' just means
   // that it should be included in even the most basic of menus.

   em.RegisterEffect(new EffectAutoDuck());
   em.RegisterEffect(new EffectBassTreble());
   em.RegisterEffect(new EffectChangeSpeed());
   #ifdef USE_SOUNDTOUCH
      em.RegisterEffect(new EffectChangePitch());
      em.RegisterEffect(new EffectChangeTempo());
   #endif
   em.RegisterEffect(new EffectClickRemoval());
   em.RegisterEffect(new EffectCompressor());
   em.RegisterEffect(new EffectEcho());
   em.RegisterEffect(new EffectPaulstretch());
   em.RegisterEffect(new EffectEqualization());
   em.RegisterEffect(new EffectFadeIn(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectFadeOut(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectInvert());
   em.RegisterEffect(new EffectLeveller(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectNoiseRemoval(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectNormalize(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectPhaser());
   em.RegisterEffect(new EffectRepair());
   em.RegisterEffect(new EffectRepeat());
   em.RegisterEffect(new EffectReverb());
   em.RegisterEffect(new EffectReverse());
#ifdef EXPERIMENTAL_SCIENCE_FILTERS
   em.RegisterEffect(new EffectScienFilter());
#endif
   em.RegisterEffect(new EffectStereoToMono(), HIDDEN_EFFECT);// NOT in normal effects list.
   em.RegisterEffect(new EffectTruncSilence(), SIMPLE_EFFECT);
#ifdef USE_SBSMS
   em.RegisterEffect(new EffectTimeScale());
#endif
   em.RegisterEffect(new EffectWahwah());

   // Analyze menu
   em.RegisterEffect(new EffectFindClipping());

#ifdef USE_NYQUIST
   if (gPrefs->Read(wxT("/Nyquist/Enable"), true)) {
      LoadNyquistPlugins();
   }
#endif

#ifdef USE_LADSPA
   if (gPrefs->Read(wxT("/Ladspa/Enable"), true)) {
      LoadLadspaPlugins();
   }
#endif

#ifdef USE_VST
   if (gPrefs->Read(wxT("/VST/Enable"), true)) {
      RegisterVSTEffects();
   }
#endif

#ifdef USE_LV2
   if (gPrefs->Read(wxT("/LV2/Enable"), true)) {
      LoadLV2Plugins();
   }
#endif

#ifdef USE_AUDIO_UNITS
   if (gPrefs->Read(wxT("/AudioUnits/Enable"), true)) {
      LoadAudioUnits();
   }
#endif

#ifdef USE_VAMP
   if (gPrefs->Read(wxT("/VAMP/Enable"), true)) {
      LoadVampPlugins();
   }
#endif

}

void UnloadEffects()
{
   EffectManager::Get().UnregisterEffects();

#ifdef USE_LADSPA
   UnloadLadspaPlugins();
#endif

#ifdef USE_LV2
   UnloadLV2Plugins();
#endif

#ifdef USE_VAMP
   UnloadVampPlugins();
#endif
}

