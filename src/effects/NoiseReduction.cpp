/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReduction.cpp

  Dominic Mazzoni

  detailed rewriting by
  Paul Licameli

*******************************************************************//**

\class EffectNoiseReduction
\brief A two-pass effect to reduce background noise.

  The first pass is done over just noise.  For each windowed sample
  of the sound, we take a FFT and then statistics are tabulated for
  each frequency band.

  During the noise reduction phase, we start by setting a gain control
  for each frequency band such that if the sound has exceeded the
  previously-determined threshold, the gain is set to 0 dB, otherwise
  the gain is set lower (e.g. -18 dB), to suppress the noise.
  Then time-smoothing is applied so that the gain for each frequency
  band moves slowly, and then frequency-smoothing is applied so that a
  single frequency is never suppressed or boosted in isolation.
  Lookahead is employed; this effect is not designed for real-time
  but if it were, there would be a significant delay.

  The gain controls are applied to the complex FFT of the signal,
  and then the inverse FFT is applied.  A Hann window may be
  applied (depending on the advanced window types setting), and then
  the output signal is then pieced together using overlap/add.

*//****************************************************************//**
*/


#include "NoiseReduction.h"

#include "LoadEffects.h"
#include "EffectManager.h"
#include "EffectUI.h"

#include "ShuttleGui.h"
#include "HelpSystem.h"
#include "FFT.h"
#include "Prefs.h"
#include "RealFFTf.h"
#include "../SpectrumTransformer.h"

#include "WaveTrack.h"
#include "AudacityMessageBox.h"
#include "valnum.h"

#include <algorithm>
#include <vector>
#include <math.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/valtext.h>
#include <wx/textctrl.h>

// SPECTRAL_SELECTION not to affect this effect for now, as there might be no indication that it does.
// [Discussed and agreed for v2.1 by Steve, Paul, Bill].
#undef EXPERIMENTAL_SPECTRAL_EDITING

typedef std::vector<float> FloatVector;

// Define both of these to make the radio button three-way
#define RESIDUE_CHOICE
//#define ISOLATE_CHOICE

// Define for Attack and release controls.
// #define ATTACK_AND_RELEASE

// Define to expose other advanced, experimental dialog controls
//#define ADVANCED_SETTINGS

// Define to make the old statistical methods an available choice
//#define OLD_METHOD_AVAILABLE

namespace {

enum DiscriminationMethod : size_t {
   DM_MEDIAN,
   DM_SECOND_GREATEST,
   DM_OLD_METHOD,

   DM_N_METHODS,
   DM_DEFAULT_METHOD = DM_SECOND_GREATEST,
};

const struct DiscriminationMethodInfo {
   const TranslatableString name;
} discriminationMethodInfo[DM_N_METHODS] = {
   // Experimental only, don't need translations
      { XO("Median") },
      { XO("Second greatest") },
      { XO("Old") },
};

// magic number used only in the old statistics
// and the old discrimination
const float minSignalTime = 0.05f;

enum WindowTypes : unsigned {
   WT_RECTANGULAR_HANN = 0, // 2.0.6 behavior, requires 1/2 step
   WT_HANN_RECTANGULAR, // requires 1/2 step
   WT_HANN_HANN,        // requires 1/4 step
   WT_BLACKMAN_HANN,     // requires 1/4 step
   WT_HAMMING_RECTANGULAR, // requires 1/2 step
   WT_HAMMING_HANN, // requires 1/4 step
   // WT_HAMMING_INV_HAMMING, // requires 1/2 step

   WT_N_WINDOW_TYPES,
   WT_DEFAULT_WINDOW_TYPES = WT_HANN_HANN
};

const struct WindowTypesInfo {
   const TranslatableString name;
   unsigned minSteps;
} windowTypesInfo [WT_N_WINDOW_TYPES] = {

   // Experimental only, don't need translations
   { Verbatim("none, Hann (2.0.6 behavior)"),    2 },
   /* i18n-hint: Hann is a proper name */
   { Verbatim("Hann, none"),                     2 },
   /* i18n-hint: Hann is a proper name */
   { Verbatim("Hann, Hann (default)"),           4 },
   /* i18n-hint: Hann and Blackman are proper names */
   { Verbatim("Blackman, Hann"),                 4 },
   /* i18n-hint: Hamming is a proper name */
   { Verbatim("Hamming, none"),                  2 },
   /* i18n-hint: Hamming and Hann area proper names */
   { Verbatim("Hamming, Hann"),                  4 },
   /* i18n-hint: Hamming is a proper name */
   // { XO("Hamming, Reciprocal Hamming"),    2, }, // output window is special
};

enum {
   DEFAULT_WINDOW_SIZE_CHOICE = 8, // corresponds to 2048
   DEFAULT_STEPS_PER_WINDOW_CHOICE = 1 // corresponds to 4, minimum for WT_HANN_HANN
};

enum  NoiseReductionChoice {
   NRC_REDUCE_NOISE,
   NRC_ISOLATE_NOISE,
   NRC_LEAVE_RESIDUE,
};

} // namespace

//----------------------------------------------------------------------------
// EffectNoiseReduction::Statistics
//----------------------------------------------------------------------------

class EffectNoiseReduction::Statistics
{
public:
   Statistics(size_t spectrumSize, double rate, int windowTypes)
      : mRate{ rate }
      , mWindowSize{ (spectrumSize - 1) * 2 }
      , mWindowTypes{ windowTypes }
      , mTotalWindows{ 0 }
      , mTrackWindows{ 0 }
      , mSums( spectrumSize )
      , mMeans (spectrumSize )
#ifdef OLD_METHOD_AVAILABLE
      , mNoiseThreshold( spectrumSize )
#endif
   {}

   // Noise profile statistics follow

   double mRate; // Rate of profile track(s) -- processed tracks must match
   size_t mWindowSize;
   int mWindowTypes;

   unsigned mTotalWindows;
   unsigned mTrackWindows;
   FloatVector mSums;
   FloatVector mMeans;

#ifdef OLD_METHOD_AVAILABLE
   // Old statistics:
   FloatVector mNoiseThreshold;
#endif
};

//----------------------------------------------------------------------------
// EffectNoiseReduction::Settings
//----------------------------------------------------------------------------

// This object is the memory of the effect between uses
// (other than noise profile statistics)
class EffectNoiseReduction::Settings
{
public:
   Settings();
   ~Settings() {}

   int PromptUser(EffectNoiseReduction *effect, EffectSettingsAccess &access,
      wxWindow &parent, bool bHasProfile, bool bAllowTwiddleSettings);
   bool PrefsIO(bool read);
   bool Validate(EffectNoiseReduction *effect) const;

   size_t WindowSize() const { return 1u << (3 + mWindowSizeChoice); }
   unsigned StepsPerWindow() const { return 1u << (1 + mStepsPerWindowChoice); }

   bool      mDoProfile;

   // Stored in preferences:

   // Basic:
   double     mNewSensitivity;   // - log10 of a probability... yeah.
   double     mFreqSmoothingBands; // really an integer
   double     mNoiseGain;         // in dB, positive
   double     mAttackTime;        // in secs
   double     mReleaseTime;       // in secs

   // Advanced:
   double     mOldSensitivity;    // in dB, plus or minus

   // Basic:
   int        mNoiseReductionChoice;

   // Advanced:
   int        mWindowTypes;
   int        mWindowSizeChoice;
   int        mStepsPerWindowChoice;
   int        mMethod;
};

EffectNoiseReduction::Settings::Settings()
   : mDoProfile{ true }
{
   PrefsIO(true);
}

//----------------------------------------------------------------------------
// EffectNoiseReduction::Worker
//----------------------------------------------------------------------------

// This object holds information needed only during effect calculation
class EffectNoiseReduction::Worker final
   : public TrackSpectrumTransformer
{
public:
   typedef EffectNoiseReduction::Settings Settings;
   typedef  EffectNoiseReduction::Statistics Statistics;

   Worker(eWindowFunctions inWindowType, eWindowFunctions outWindowType,
      EffectNoiseReduction &effect, const Settings &settings,
      Statistics &statistics
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      , double f0, double f1
#endif
      );
   ~Worker();

   struct MyWindow : public Window
   {
      explicit MyWindow(size_t windowSize)
         : Window{ windowSize }
         , mSpectrums(windowSize / 2 + 1)
         , mGains(windowSize / 2 + 1)
      {}
      ~MyWindow() override;

      FloatVector mSpectrums;
      FloatVector mGains;
   };

   bool Process(TrackList &tracks, double mT0, double mT1);

protected:
   MyWindow &NthWindow(int nn) { return static_cast<MyWindow&>(Nth(nn)); }
   std::unique_ptr<Window> NewWindow(size_t windowSize) override;
   bool DoStart() override;
   static bool Processor(SpectrumTransformer &transformer);
   bool DoFinish() override;

private:
   void ApplyFreqSmoothing(FloatVector &gains);
   void GatherStatistics();
   inline bool Classify(unsigned nWindows, int band);
   void ReduceNoise();
   void FinishTrackStatistics();

private:

   const bool mDoProfile;

   EffectNoiseReduction &mEffect;
   Statistics &mStatistics;

   FloatVector mFreqSmoothingScratch;
   const size_t mFreqSmoothingBins;
   // When spectral selection limits the affected band:
   size_t mBinLow;  // inclusive lower bound
   size_t mBinHigh; // exclusive upper bound

   const int mNoiseReductionChoice;
   const int mMethod;
   const double mNewSensitivity;

   float     mOneBlockAttack;
   float     mOneBlockRelease;
   float     mNoiseAttenFactor;
   float     mOldSensitivityFactor;

   unsigned  mNWindowsToExamine;
   unsigned  mCenter;
   unsigned  mHistoryLen;

   // Following are for progress indicator only:
   unsigned  mProgressTrackCount = 0;
   sampleCount mLen = 0;
   sampleCount mProgressWindowCount = 0;
};

/****************************************************************//**

\class EffectNoiseReduction::Dialog
\brief Dialog used with EffectNoiseReduction

**//*****************************************************************/

//----------------------------------------------------------------------------
// EffectNoiseReduction::Dialog
//----------------------------------------------------------------------------

class EffectNoiseReduction::Dialog final : public EffectDialog
{
public:
   // constructors and destructors
   Dialog(EffectNoiseReduction *effect, EffectSettingsAccess &access,
       Settings *settings,
       wxWindow *parent, bool bHasProfile,
       bool bAllowTwiddleSettings);

   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   const Settings &GetTempSettings() const
   { return mTempSettings; }

private:
   void DisableControlsIfIsolating();

#ifdef ADVANCED_SETTINGS
   void EnableDisableSensitivityControls();
#endif

   // handlers
   void OnGetProfile( wxCommandEvent &event );
   void OnNoiseReductionChoice( wxCommandEvent &event );
#ifdef ADVANCED_SETTINGS
   void OnMethodChoice(wxCommandEvent &);
#endif
   void OnPreview(wxCommandEvent &event) override;
   void OnReduceNoise( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnHelp( wxCommandEvent &event );

   void OnText(wxCommandEvent &event);
   void OnSlider(wxCommandEvent &event);

   // data members

   EffectNoiseReduction *m_pEffect;
   //! This dialog is modal, so mAccess will live long enough for it
   EffectSettingsAccess &mAccess;
   EffectNoiseReduction::Settings *m_pSettings;
   EffectNoiseReduction::Settings mTempSettings;

   bool mbHasProfile;
   bool mbAllowTwiddleSettings;


   wxRadioButton *mKeepSignal;
#ifdef ISOLATE_CHOICE
   wxRadioButton *mKeepNoise;
#endif
#ifdef RESIDUE_CHOICE
   wxRadioButton *mResidue;
#endif

private:
    DECLARE_EVENT_TABLE()
};

const ComponentInterfaceSymbol EffectNoiseReduction::Symbol
{ XO("Noise Reduction") };

namespace{ BuiltinEffectsModule::Registration< EffectNoiseReduction > reg; }

EffectNoiseReduction::EffectNoiseReduction()
: mSettings(std::make_unique<EffectNoiseReduction::Settings>())
{
}

EffectNoiseReduction::~EffectNoiseReduction()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectNoiseReduction::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectNoiseReduction::GetDescription() const
{
   return XO("Removes background noise such as fans, tape noise, or hums");
}

// EffectDefinitionInterface implementation

EffectType EffectNoiseReduction::GetType() const
{
   return EffectTypeProcess;
}

//! An override still here for historical reasons, ignoring the factory
//! and the access
/*! We would like to make this effect behave more like others, but it does have
 its unusual two-pass nature.  First choose and analyze an example of noise,
 then apply noise reduction to another selection.  That is difficult to fit into
 the framework for managing settings of other effects. */
int EffectNoiseReduction::ShowHostInterface(EffectPlugin &,
   wxWindow &parent, const EffectDialogFactory &,
   std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
   bool forceModal)
{
   // Assign the out parameter
   pInstance = MakeInstance();

   // to do: use forceModal correctly

   // Doesn't use the factory but substitutes its own dialog

   // We may want to twiddle the levels if we are setting
   // from a macro editing dialog
   return mSettings->PromptUser(this, access, parent,
      bool(mStatistics), IsBatchProcessing());
}

int EffectNoiseReduction::Settings::PromptUser(EffectNoiseReduction *effect,
   EffectSettingsAccess &access, wxWindow &parent,
   bool bHasProfile, bool bAllowTwiddleSettings)
{
   EffectNoiseReduction::Dialog dlog(effect, access,
      this, &parent, bHasProfile, bAllowTwiddleSettings);

   dlog.CentreOnParent();
   dlog.ShowModal();

   const auto returnCode = dlog.GetReturnCode();
   if (!returnCode)
      return 0;

   *this = dlog.GetTempSettings();
   mDoProfile = (returnCode == 1);

   if (!PrefsIO(false))
      return 0;
   return returnCode;
}

namespace {
   template <typename StructureType, typename FieldType>
   struct PrefsTableEntry {
      typedef FieldType (StructureType::*MemberPointer);

      MemberPointer field;
      const wxChar *name;
      FieldType defaultValue;
   };

   template <typename StructureType, typename FieldType>
   void readPrefs(
      StructureType *structure, const wxString &prefix,
      const PrefsTableEntry<StructureType, FieldType> *fields, size_t numFields)
   {
      for (size_t ii = 0; ii < numFields; ++ii) {
         const PrefsTableEntry<StructureType, FieldType> &entry = fields[ii];
         gPrefs->Read(prefix + entry.name, &(structure->*(entry.field)),
            entry.defaultValue);
      }
   }

   template <typename StructureType, typename FieldType>
   void writePrefs(
      const StructureType *structure, const wxString &prefix,
      const PrefsTableEntry<StructureType, FieldType> *fields, size_t numFields)
   {
      for (size_t ii = 0; ii < numFields; ++ii) {
         const PrefsTableEntry<StructureType, FieldType> &entry = fields[ii];
         gPrefs->Write(prefix + entry.name, structure->*(entry.field));
      }
   }
}

bool EffectNoiseReduction::Settings::PrefsIO(bool read)
{
   static const double DEFAULT_OLD_SENSITIVITY = 0.0;

   static const PrefsTableEntry<Settings, double> doubleTable[] = {
         { &Settings::mNewSensitivity, wxT("Sensitivity"), 6.0 },
         { &Settings::mNoiseGain, wxT("Gain"), 6.0 },
         { &Settings::mAttackTime, wxT("AttackTime"), 0.02 },
         { &Settings::mReleaseTime, wxT("ReleaseTime"), 0.10 },
         { &Settings::mFreqSmoothingBands, wxT("FreqSmoothing"), 6.0 },

         // Advanced settings
         { &Settings::mOldSensitivity, wxT("OldSensitivity"), DEFAULT_OLD_SENSITIVITY },
   };
   static auto doubleTableSize = sizeof(doubleTable) / sizeof(doubleTable[0]);

   static const PrefsTableEntry<Settings, int> intTable[] = {
         { &Settings::mNoiseReductionChoice, wxT("ReductionChoice"), NRC_REDUCE_NOISE },

         // Advanced settings
         { &Settings::mWindowTypes, wxT("WindowTypes"), WT_DEFAULT_WINDOW_TYPES },
         { &Settings::mWindowSizeChoice, wxT("WindowSize"), DEFAULT_WINDOW_SIZE_CHOICE },
         { &Settings::mStepsPerWindowChoice, wxT("StepsPerWindow"), DEFAULT_STEPS_PER_WINDOW_CHOICE },
         { &Settings::mMethod, wxT("Method"), DM_DEFAULT_METHOD },
   };
   static auto intTableSize = sizeof(intTable) / sizeof(intTable[0]);

   static const wxString prefix(wxT("/Effects/NoiseReduction/"));

   if (read) {
      readPrefs(this, prefix, doubleTable, doubleTableSize);
      readPrefs(this, prefix, intTable, intTableSize);

      // Ignore preferences for unavailable options.
#if !(defined(RESIDUE_CHOICE) || defined (ISOLATE_CHOICE))
      mNoiseReductionChoice == NRC_REDUCE_NOISE;
#elif !(defined(RESIDUE_CHOICE))
      if (mNoiseReductionChoice == NRC_LEAVE_RESIDUE)
         mNoiseReductionChoice = NRC_ISOLATE_NOISE;
#elif !(defined(ISOLATE_CHOICE))
      if (mNoiseReductionChoice == NRC_ISOLATE_NOISE)
         mNoiseReductionChoice = NRC_LEAVE_RESIDUE;
#endif

#ifndef ADVANCED_SETTINGS
      // Initialize all hidden advanced settings to defaults.
      mWindowTypes = WT_DEFAULT_WINDOW_TYPES;
      mWindowSizeChoice = DEFAULT_WINDOW_SIZE_CHOICE;
      mStepsPerWindowChoice = DEFAULT_STEPS_PER_WINDOW_CHOICE;
      mMethod = DM_DEFAULT_METHOD;
      mOldSensitivity = DEFAULT_OLD_SENSITIVITY;
#endif

#ifndef OLD_METHOD_AVAILABLE
      if (mMethod == DM_OLD_METHOD)
         mMethod = DM_DEFAULT_METHOD;
#endif

      return true;
   }
   else {
      writePrefs(this, prefix, doubleTable, doubleTableSize);
      writePrefs(this, prefix, intTable, intTableSize);
      return gPrefs->Flush();
   }
}

bool EffectNoiseReduction::Settings::Validate(EffectNoiseReduction *effect) const
{
   if (StepsPerWindow() < windowTypesInfo[mWindowTypes].minSteps) {
      EffectUIServices::DoMessageBox(*effect,
         XO("Steps per block are too few for the window types.") );
      return false;
   }

   if (StepsPerWindow() > WindowSize()) {
      EffectUIServices::DoMessageBox(*effect,
         XO("Steps per block cannot exceed the window size.") );
      return false;
   }

   if (mMethod == DM_MEDIAN && StepsPerWindow() > 4) {
      EffectUIServices::DoMessageBox(*effect,
         XO(
"Median method is not implemented for more than four steps per window.") );
      return false;
   }

   return true;
}

auto EffectNoiseReduction::Worker::NewWindow(size_t windowSize)
   -> std::unique_ptr<Window>
{
   return std::make_unique<MyWindow>(windowSize);
}

EffectNoiseReduction::Worker::MyWindow::~MyWindow()
{
}

bool EffectNoiseReduction::Process(EffectInstance &, EffectSettings &)
{
   // This same code will either reduce noise or profile it

   this->CopyInputTracks(); // Set up mOutputTracks.

   auto track = * (mOutputTracks->Selected< const WaveTrack >()).begin();
   if (!track)
      return false;

   // Initialize statistics if gathering them, or check for mismatched (advanced)
   // settings if reducing noise.
   if (mSettings->mDoProfile) {
      size_t spectrumSize = 1 + mSettings->WindowSize() / 2;
      mStatistics = std::make_unique<Statistics>
         (spectrumSize, track->GetRate(), mSettings->mWindowTypes);
   }
   else if (mStatistics->mWindowSize != mSettings->WindowSize()) {
      // possible only with advanced settings
      EffectUIServices::DoMessageBox(*this,
         XO("You must specify the same window size for steps 1 and 2.") );
      return false;
   }
   else if (mStatistics->mWindowTypes != mSettings->mWindowTypes) {
      // A warning only
      EffectUIServices::DoMessageBox(*this,
         XO("Warning: window types are not the same as for profiling.") );
   }

   eWindowFunctions inWindowType, outWindowType;
   switch (mSettings->mWindowTypes) {
   case WT_RECTANGULAR_HANN:
      inWindowType = eWinFuncRectangular;
      outWindowType = eWinFuncHann;
      break;
   case WT_HANN_RECTANGULAR:
      inWindowType = eWinFuncHann;
      outWindowType = eWinFuncRectangular;
      break;
   case WT_BLACKMAN_HANN:
      inWindowType = eWinFuncBlackman;
      outWindowType = eWinFuncHann;
      break;
   case WT_HAMMING_RECTANGULAR:
      inWindowType = eWinFuncHamming;
      outWindowType = eWinFuncRectangular;
      break;
   case WT_HAMMING_HANN:
      inWindowType = eWinFuncHamming;
      outWindowType = eWinFuncHann;
      break;
   default:
      wxASSERT(false);
      [[fallthrough]] ;
   case WT_HANN_HANN:
      inWindowType = outWindowType = eWinFuncHann;
      break;
   }
   Worker worker{ inWindowType, outWindowType,
      *this, *mSettings, *mStatistics
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      , mF0, mF1
#endif
   };
   bool bGoodResult = worker.Process(*mOutputTracks, mT0, mT1);
   if (mSettings->mDoProfile) {
      if (bGoodResult)
         mSettings->mDoProfile = false; // So that "repeat last effect" will reduce noise
      else
         mStatistics.reset(); // So that profiling must be done again before noise reduction
   }
   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

EffectNoiseReduction::Worker::~Worker()
{
}

bool EffectNoiseReduction::Worker::Process(
   TrackList &tracks, double inT0, double inT1)
{
   mProgressTrackCount = 0;
   for ( auto track : tracks.Selected< WaveTrack >() ) {
      mProgressWindowCount = 0;
      if (track->GetRate() != mStatistics.mRate) {
         if (mDoProfile)
            EffectUIServices::DoMessageBox(mEffect,
               XO("All noise profile data must have the same sample rate.") );
         else
            EffectUIServices::DoMessageBox(mEffect,
               XO(
"The sample rate of the noise profile must match that of the sound to be processed.") );
         return false;
      }

      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = std::max(trackStart, inT0);
      double t1 = std::min(trackEnd, inT1);

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         const auto len = end - start;
         mLen = len;
         const auto extra = (mStepsPerWindow - 1) * mStepSize;
         // Adjust denominator for presence or absence of padding,
         // which makes the number of windows visited either more or less
         // than the number of window steps in the data.
         if (mDoProfile)
            mLen -= extra;
         else
            mLen += extra;

         if (!TrackSpectrumTransformer::Process(
            Processor, track, mHistoryLen, start, len ))
            return false;
      }
      ++mProgressTrackCount;
   }

   if (mDoProfile) {
      if (mStatistics.mTotalWindows == 0) {
         EffectUIServices::DoMessageBox(mEffect,
            XO("Selected noise profile is too short."));
         return false;
      }
   }

   return true;
}

void EffectNoiseReduction::Worker::ApplyFreqSmoothing(FloatVector &gains)
{
   // Given an array of gain mutipliers, average them
   // GEOMETRICALLY.  Don't multiply and take nth root --
   // that may quickly cause underflows.  Instead, average the logs.

   if (mFreqSmoothingBins == 0)
      return;

   {
      auto pScratch = mFreqSmoothingScratch.data();
      std::fill(pScratch, pScratch + mSpectrumSize, 0.0f);
   }

   for (size_t ii = 0; ii < mSpectrumSize; ++ii)
      gains[ii] = log(gains[ii]);

   // ii must be signed
   for (int ii = 0; ii < (int)mSpectrumSize; ++ii) {
      const int j0 = std::max(0, ii - (int)mFreqSmoothingBins);
      const int j1 = std::min(mSpectrumSize - 1, ii + mFreqSmoothingBins);
      for(int jj = j0; jj <= j1; ++jj) {
         mFreqSmoothingScratch[ii] += gains[jj];
      }
      mFreqSmoothingScratch[ii] /= (j1 - j0 + 1);
   }

   for (size_t ii = 0; ii < mSpectrumSize; ++ii)
      gains[ii] = exp(mFreqSmoothingScratch[ii]);
}

EffectNoiseReduction::Worker::Worker(eWindowFunctions inWindowType,
   eWindowFunctions outWindowType,
   EffectNoiseReduction &effect,
   const Settings &settings, Statistics &statistics
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   , double f0, double f1
#endif
)
: TrackSpectrumTransformer{ !settings.mDoProfile, inWindowType, outWindowType,
   settings.WindowSize(), settings.StepsPerWindow(),
   !settings.mDoProfile, !settings.mDoProfile
}
, mDoProfile{ settings.mDoProfile }

, mEffect{ effect }
, mStatistics{ statistics }

, mFreqSmoothingScratch( mSpectrumSize )
, mFreqSmoothingBins{ size_t(std::max(0.0, settings.mFreqSmoothingBands)) }
, mBinLow{ 0 }
, mBinHigh{ mSpectrumSize }

, mNoiseReductionChoice{ settings.mNoiseReductionChoice }
, mMethod{ settings.mMethod }

// Sensitivity setting is a base 10 log, turn it into a natural log
, mNewSensitivity{ settings.mNewSensitivity * log(10.0) }
{
   const auto sampleRate = mStatistics.mRate;

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   {
      // mBinLow is inclusive, mBinHigh is exclusive, of
      // the range of frequencies to affect.  Include any
      // bin that partly overlaps the selected range of frequencies.
      const double bin = sampleRate / mWindowSize;
      if (f0 >= 0.0 )
         mBinLow = floor(f0 / bin);
      if (f1 >= 0.0)
         mBinHigh = ceil(f1 / bin);
   }
#endif

   const double noiseGain = -settings.mNoiseGain;
   const unsigned nAttackBlocks = 1 + (int)(settings.mAttackTime * sampleRate / mStepSize);
   const unsigned nReleaseBlocks = 1 + (int)(settings.mReleaseTime * sampleRate / mStepSize);
   // Applies to amplitudes, divide by 20:
   mNoiseAttenFactor = DB_TO_LINEAR(noiseGain);
   // Apply to gain factors which apply to amplitudes, divide by 20:
   mOneBlockAttack = DB_TO_LINEAR(noiseGain / nAttackBlocks);
   mOneBlockRelease = DB_TO_LINEAR(noiseGain / nReleaseBlocks);
   // Applies to power, divide by 10:
   mOldSensitivityFactor = pow(10.0, settings.mOldSensitivity / 10.0);

   mNWindowsToExamine = (mMethod == DM_OLD_METHOD)
      ? std::max(2, (int)(minSignalTime * sampleRate / mStepSize))
      : 1 + mStepsPerWindow;

   mCenter = mNWindowsToExamine / 2;
   wxASSERT(mCenter >= 1); // release depends on this assumption

   if (mDoProfile)
#ifdef OLD_METHOD_AVAILABLE
      mHistoryLen = mNWindowsToExamine;
#else
      mHistoryLen = 1;
#endif
   else {
      // Allow long enough queue for sufficient inspection of the middle
      // and for attack processing
      // See ReduceNoise()
      mHistoryLen = std::max(mNWindowsToExamine, mCenter + nAttackBlocks);
   }
}

bool EffectNoiseReduction::Worker::DoStart()
{
   for (size_t ii = 0, nn = TotalQueueSize(); ii < nn; ++ii) {
      MyWindow &record = NthWindow(ii);
      std::fill(record.mSpectrums.begin(), record.mSpectrums.end(), 0.0);
      std::fill(record.mGains.begin(), record.mGains.end(), mNoiseAttenFactor);
   }
   return TrackSpectrumTransformer::DoStart();
}

bool EffectNoiseReduction::Worker::Processor(SpectrumTransformer &transformer)
{
   auto &worker = static_cast<Worker &>(transformer);
   // Compute power spectrum in the newest window
   {
      MyWindow &record = worker.NthWindow(0);
      float *pSpectrum = &record.mSpectrums[0];
      const double dc = record.mRealFFTs[0];
      *pSpectrum++ = dc * dc;
      float *pReal = &record.mRealFFTs[1], *pImag = &record.mImagFFTs[1];
      for (size_t nn = worker.mSpectrumSize - 2; nn--;) {
         const double re = *pReal++, im = *pImag++;
         *pSpectrum++ = re * re + im * im;
      }
      const double nyquist = record.mImagFFTs[0];
      *pSpectrum = nyquist * nyquist;
   }

   if (worker.mDoProfile)
      worker.GatherStatistics();
   else
      worker.ReduceNoise();

   // Update the Progress meter, let user cancel
   return !worker.mEffect.TrackProgress(worker.mProgressTrackCount,
      std::min(1.0,
         ((++worker.mProgressWindowCount).as_double() * worker.mStepSize)
            / worker.mLen.as_double()));
}

void EffectNoiseReduction::Worker::FinishTrackStatistics()
{
   const auto windows = mStatistics.mTrackWindows;

   // Combine averages in case of multiple profile tracks.
   if (windows) {
      const auto multiplier = mStatistics.mTotalWindows;
      const auto denom = windows + multiplier;
      for (size_t ii = 0, nn = mStatistics.mMeans.size(); ii < nn; ++ii) {
         auto &mean = mStatistics.mMeans[ii];
         auto &sum = mStatistics.mSums[ii];
         mean = (mean * multiplier + sum) / denom;
         // Reset for next track
         sum = 0;
      }
      // Reset for next track
      mStatistics.mTrackWindows = 0;
      mStatistics.mTotalWindows = denom;
   }
}

void EffectNoiseReduction::Worker::GatherStatistics()
{
   ++mStatistics.mTrackWindows;

   {
      // NEW statistics
      auto pPower = NthWindow(0).mSpectrums.data();
      auto pSum = mStatistics.mSums.data();
      for (size_t jj = 0; jj < mSpectrumSize; ++jj) {
         *pSum++ += *pPower++;
      }
   }

#ifdef OLD_METHOD_AVAILABLE
   // The noise threshold for each frequency is the maximum
   // level achieved at that frequency for a minimum of
   // mMinSignalBlocks blocks in a row - the max of a min.

   auto finish = mHistoryLen;

   {
      // old statistics
      auto pPower = NthWindow(0).mSpectrums.data();
      auto pThreshold = mStatistics.mNoiseThreshold.data();
      for (size_t jj = 0; jj < mSpectrumSize; ++jj) {
         float min = *pPower++;
         for (unsigned ii = 1; ii < finish; ++ii)
            min = std::min(min, NthWindow(ii).mSpectrums[jj]);
         *pThreshold = std::max(*pThreshold, min);
         ++pThreshold;
      }
   }
#endif
}

// Return true iff the given band of the "center" window looks like noise.
// Examine the band in a few neighboring windows to decide.
inline
bool EffectNoiseReduction::Worker::Classify(unsigned nWindows, int band)
{
   switch (mMethod) {
#ifdef OLD_METHOD_AVAILABLE
   case DM_OLD_METHOD:
      {
         float min = NthWindow(0).mSpectrums[band];
         for (unsigned ii = 1; ii < nWindows; ++ii)
            min = std::min(min, NthWindow(ii).mSpectrums[band]);
         return
            min <= mOldSensitivityFactor * mStatistics.mNoiseThreshold[band];
      }
#endif
   // New methods suppose an exponential distribution of power values
   // in the noise; NEW sensitivity (which is nonnegative) is meant to be
   // the negative of a log of probability (so the log is nonpositive)
   // that noise strays above the threshold.  Call that probability
   // 1 - F.  The quantile function of an exponential distribution is
   // - log (1 - F) * mean.  Thus simply multiply mean by sensitivity
   // to get the threshold.
   case DM_MEDIAN:
      // This method examines the window and all other windows
      // whose centers lie on or between its boundaries, and takes a median, to
      // avoid being fooled by up and down excursions into
      // either the mistake of classifying noise as not noise
      // (leaving a musical noise chime), or the opposite
      // (distorting the signal with a drop out).
      if (nWindows <= 3)
         // No different from second greatest.
         goto secondGreatest;
      else if (nWindows <= 5)
      {
         float greatest = 0.0, second = 0.0, third = 0.0;
         for (unsigned ii = 0; ii < nWindows; ++ii) {
            const float power = NthWindow(ii).mSpectrums[band];
            if (power >= greatest)
               third = second, second = greatest, greatest = power;
            else if (power >= second)
               third = second, second = power;
            else if (power >= third)
               third = power;
         }
         return third <= mNewSensitivity * mStatistics.mMeans[band];
      }
      else {
         // not implemented
         wxASSERT(false);
         return true;
      }
   secondGreatest:
   case DM_SECOND_GREATEST:
      {
         // This method just throws out the high outlier.  It
         // should be less prone to distortions and more prone to
         // chimes.
         float greatest = 0.0, second = 0.0;
         for (unsigned ii = 0; ii < nWindows; ++ii) {
            const float power = NthWindow(ii).mSpectrums[band];
            if (power >= greatest)
               second = greatest, greatest = power;
            else if (power >= second)
               second = power;
         }
         return second <= mNewSensitivity * mStatistics.mMeans[band];
      }
   default:
      wxASSERT(false);
      return true;
   }
}

void EffectNoiseReduction::Worker::ReduceNoise()
{
   auto historyLen = CurrentQueueSize();
   auto nWindows = std::min<unsigned>(mNWindowsToExamine, historyLen);

   if (mNoiseReductionChoice != NRC_ISOLATE_NOISE)
   {
      MyWindow &record = NthWindow(0);
      // Default all gains to the reduction factor,
      // until we decide to raise some of them later
      float *pGain = &record.mGains[0];
      std::fill(pGain, pGain + mSpectrumSize, mNoiseAttenFactor);
   }

   // Raise the gain for elements in the center of the sliding history
   // or, if isolating noise, zero out the non-noise
   if (nWindows > mCenter)
   {
      auto pGain = NthWindow(mCenter).mGains.data();
      if (mNoiseReductionChoice == NRC_ISOLATE_NOISE) {
         // All above or below the selected frequency range is non-noise
         std::fill(pGain, pGain + mBinLow, 0.0f);
         std::fill(pGain + mBinHigh, pGain + mSpectrumSize, 0.0f);
         pGain += mBinLow;
         for (size_t jj = mBinLow; jj < mBinHigh; ++jj) {
               const bool isNoise = Classify(nWindows, jj);
            *pGain++ = isNoise ? 1.0 : 0.0;
         }
      }
      else {
         // All above or below the selected frequency range is non-noise
         std::fill(pGain, pGain + mBinLow, 1.0f);
         std::fill(pGain + mBinHigh, pGain + mSpectrumSize, 1.0f);
         pGain += mBinLow;
         for (size_t jj = mBinLow; jj < mBinHigh; ++jj) {
            const bool isNoise = Classify(nWindows, jj);
            if (!isNoise)
               *pGain = 1.0;
            ++pGain;
         }
      }
   }

   if (mNoiseReductionChoice != NRC_ISOLATE_NOISE)
   {
      // In each direction, define an exponential decay of gain from the
      // center; make actual gains the maximum of mNoiseAttenFactor, and
      // the decay curve, and their prior values.

      // First, the attack, which goes backward in time, which is,
      // toward higher indices in the queue.
      for (size_t jj = 0; jj < mSpectrumSize; ++jj) {
         for (unsigned ii = mCenter + 1; ii < historyLen; ++ii) {
            const float minimum =
               std::max(mNoiseAttenFactor,
                        NthWindow(ii - 1).mGains[jj] * mOneBlockAttack);
            float &gain = NthWindow(ii).mGains[jj];
            if (gain < minimum)
               gain = minimum;
            else
               // We can stop now, our attack curve is intersecting
               // the release curve of some window previously processed.
               break;
         }
      }

      // Now, release.  We need only look one window ahead.  This part will
      // be visited again when we examine the next window, and
      // carry the decay further.
      {
         auto pNextGain = NthWindow(mCenter - 1).mGains.data();
         auto pThisGain = NthWindow(mCenter).mGains.data();
         for (auto nn = mSpectrumSize; nn--;) {
            *pNextGain =
               std::max(*pNextGain,
                        std::max(mNoiseAttenFactor,
                                 *pThisGain++ * mOneBlockRelease));
            ++pNextGain;
         }
      }
   }


   if (QueueIsFull()) {
      auto &record = NthWindow(historyLen - 1);  // end of the queue
      const auto last = mSpectrumSize - 1;

      if (mNoiseReductionChoice != NRC_ISOLATE_NOISE)
         // Apply frequency smoothing to output gain
         // Gains are not less than mNoiseAttenFactor
         ApplyFreqSmoothing(record.mGains);

      // Apply gain to FFT
      {
         const float *pGain = &record.mGains[1];
         float *pReal = &record.mRealFFTs[1];
         float *pImag = &record.mImagFFTs[1];
         auto nn = mSpectrumSize - 2;
         if (mNoiseReductionChoice == NRC_LEAVE_RESIDUE) {
            for (; nn--;) {
               // Subtract the gain we would otherwise apply from 1, and
               // negate that to flip the phase.
               const double gain = *pGain++ - 1.0;
               *pReal++ *= gain;
               *pImag++ *= gain;
            }
            record.mRealFFTs[0] *= (record.mGains[0] - 1.0);
            // The Fs/2 component is stored as the imaginary part of the DC component
            record.mImagFFTs[0] *= (record.mGains[last] - 1.0);
         }
         else {
            for (; nn--;) {
               const double gain = *pGain++;
               *pReal++ *= gain;
               *pImag++ *= gain;
            }
            record.mRealFFTs[0] *= record.mGains[0];
            // The Fs/2 component is stored as the imaginary part of the DC component
            record.mImagFFTs[0] *= record.mGains[last];
         }
      }
   }
}

bool EffectNoiseReduction::Worker::DoFinish()
{
   if (mDoProfile)
      FinishTrackStatistics();
   return TrackSpectrumTransformer::DoFinish();
}

//----------------------------------------------------------------------------
// EffectNoiseReduction::Dialog
//----------------------------------------------------------------------------

enum {
   ID_BUTTON_GETPROFILE = 10001,
   ID_RADIOBUTTON_KEEPSIGNAL,
#ifdef ISOLATE_CHOICE
   ID_RADIOBUTTON_KEEPNOISE,
#endif
#ifdef RESIDUE_CHOICE
   ID_RADIOBUTTON_RESIDUE,
#endif

#ifdef ADVANCED_SETTINGS
   ID_CHOICE_METHOD,
#endif

   // Slider/text pairs
   ID_GAIN_SLIDER,
   ID_GAIN_TEXT,

   ID_NEW_SENSITIVITY_SLIDER,
   ID_NEW_SENSITIVITY_TEXT,

#ifdef ATTACK_AND_RELEASE
   ID_ATTACK_TIME_SLIDER,
   ID_ATTACK_TIME_TEXT,

   ID_RELEASE_TIME_SLIDER,
   ID_RELEASE_TIME_TEXT,
#endif

   ID_FREQ_SLIDER,
   ID_FREQ_TEXT,

   END_OF_BASIC_SLIDERS,

#ifdef ADVANCED_SETTINGS
   ID_OLD_SENSITIVITY_SLIDER = END_OF_BASIC_SLIDERS,
   ID_OLD_SENSITIVITY_TEXT,

   END_OF_ADVANCED_SLIDERS,
   END_OF_SLIDERS = END_OF_ADVANCED_SLIDERS,
#else
   END_OF_SLIDERS = END_OF_BASIC_SLIDERS,
#endif

   FIRST_SLIDER = ID_GAIN_SLIDER,
};

namespace {

struct ControlInfo {
   typedef double (EffectNoiseReduction::Settings::*MemberPointer);

   double Value(long sliderSetting) const
   {
      return
         valueMin +
         (double(sliderSetting) / sliderMax) * (valueMax - valueMin);
   }

   long SliderSetting(double value) const
   {
      return std::clamp<long>(
         0.5 + sliderMax * (value - valueMin) / (valueMax - valueMin),
         0, sliderMax);
   }

   wxString Text(double value) const
   {
      if (formatAsInt)
         return wxString::Format(format, (int)(value));
      else
         return wxString::Format(format, value);
   }

   void CreateControls(int id, ShuttleGui &S) const
   {
      wxTextCtrl *const text = S.Id(id + 1)
         .Validator<FloatingPointValidator<double>>(
            formatAsInt ? 0 : 2,
            nullptr,
            NumValidatorStyle::DEFAULT,
            valueMin, valueMax
         )
         .AddTextBox(textBoxCaption, wxT(""), 0);

      wxSlider *const slider =
         S.Id(id)
            .Name( sliderName )
            .Style(wxSL_HORIZONTAL)
            .MinSize( { 150, -1 } )
            .AddSlider( {}, 0, sliderMax);
   }

   MemberPointer field;
   double valueMin;
   double valueMax;
   long sliderMax;
   // (valueMin - valueMax) / sliderMax is the value increment of the slider
   const wxChar* format;
   bool formatAsInt;
   const TranslatableString textBoxCaption;
   const TranslatableString sliderName;

   ControlInfo(MemberPointer f, double vMin, double vMax, long sMax, const wxChar* fmt, bool fAsInt,
      const TranslatableString &caption, const TranslatableString &name)
      : field(f), valueMin(vMin), valueMax(vMax), sliderMax(sMax), format(fmt), formatAsInt(fAsInt)
      , textBoxCaption(caption), sliderName(name)
   {
   }
};

const ControlInfo *controlInfo() {
   static const ControlInfo table[] = {
         ControlInfo(&EffectNoiseReduction::Settings::mNoiseGain,
         0.0, 48.0, 48, wxT("%d"), true,
         XXO("&Noise reduction (dB):"), XO("Noise reduction")),
         ControlInfo(&EffectNoiseReduction::Settings::mNewSensitivity,
         0.0, 24.0, 48, wxT("%.2f"), false,
         XXO("&Sensitivity:"), XO("Sensitivity")),
#ifdef ATTACK_AND_RELEASE
         ControlInfo(&EffectNoiseReduction::Settings::mAttackTime,
         0, 1.0, 100, wxT("%.2f"), false,
         XXO("Attac&k time (secs):"), XO("Attack time")),
         ControlInfo(&EffectNoiseReduction::Settings::mReleaseTime,
         0, 1.0, 100, wxT("%.2f"), false,
         XXO("R&elease time (secs):"), XO("Release time")),
#endif
         ControlInfo(&EffectNoiseReduction::Settings::mFreqSmoothingBands,
         0, 12, 12, wxT("%d"), true,
         XXO("&Frequency smoothing (bands):"), XO("Frequency smoothing")),

#ifdef ADVANCED_SETTINGS
         ControlInfo(&EffectNoiseReduction::Settings::mOldSensitivity,
         -20.0, 20.0, 4000, wxT("%.2f"), false,
         XXO("Sensiti&vity (dB):"), XO("Old Sensitivity")),
         // add here
#endif
   };

return table;
}

} // namespace


BEGIN_EVENT_TABLE(EffectNoiseReduction::Dialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, EffectNoiseReduction::Dialog::OnReduceNoise)
   EVT_BUTTON(wxID_CANCEL, EffectNoiseReduction::Dialog::OnCancel)
   EVT_BUTTON(ID_EFFECT_PREVIEW, EffectNoiseReduction::Dialog::OnPreview)
   EVT_BUTTON(ID_BUTTON_GETPROFILE, EffectNoiseReduction::Dialog::OnGetProfile)
   EVT_BUTTON(wxID_HELP, EffectNoiseReduction::Dialog::OnHelp)

   EVT_RADIOBUTTON(ID_RADIOBUTTON_KEEPSIGNAL, EffectNoiseReduction::Dialog::OnNoiseReductionChoice)
#ifdef ISOLATE_CHOICE
   EVT_RADIOBUTTON(ID_RADIOBUTTON_KEEPNOISE, EffectNoiseReduction::Dialog::OnNoiseReductionChoice)
#endif
#ifdef RESIDUE_CHOICE
   EVT_RADIOBUTTON(ID_RADIOBUTTON_RESIDUE, EffectNoiseReduction::Dialog::OnNoiseReductionChoice)
#endif

#ifdef ADVANCED_SETTINGS
   EVT_CHOICE(ID_CHOICE_METHOD, EffectNoiseReduction::Dialog::OnMethodChoice)
#endif

   EVT_SLIDER(ID_GAIN_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
   EVT_TEXT(ID_GAIN_TEXT, EffectNoiseReduction::Dialog::OnText)

   EVT_SLIDER(ID_NEW_SENSITIVITY_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
   EVT_TEXT(ID_NEW_SENSITIVITY_TEXT, EffectNoiseReduction::Dialog::OnText)

   EVT_SLIDER(ID_FREQ_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
   EVT_TEXT(ID_FREQ_TEXT, EffectNoiseReduction::Dialog::OnText)

#ifdef ATTACK_AND_RELEASE
   EVT_SLIDER(ID_ATTACK_TIME_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
   EVT_TEXT(ID_ATTACK_TIME_TEXT, EffectNoiseReduction::Dialog::OnText)

   EVT_SLIDER(ID_RELEASE_TIME_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
   EVT_TEXT(ID_RELEASE_TIME_TEXT, EffectNoiseReduction::Dialog::OnText)
#endif


#ifdef ADVANCED_SETTINGS
   EVT_SLIDER(ID_OLD_SENSITIVITY_SLIDER, EffectNoiseReduction::Dialog::OnSlider)
   EVT_TEXT(ID_OLD_SENSITIVITY_TEXT, EffectNoiseReduction::Dialog::OnText)
#endif
END_EVENT_TABLE()

EffectNoiseReduction::Dialog::Dialog(EffectNoiseReduction *effect,
    EffectSettingsAccess &access,
    EffectNoiseReduction::Settings *settings,
    wxWindow *parent, bool bHasProfile, bool bAllowTwiddleSettings)
   : EffectDialog( parent, XO("Noise Reduction"), EffectTypeProcess,wxDEFAULT_DIALOG_STYLE, eHelpButton )
   , m_pEffect(effect)
   , mAccess{access}
   , m_pSettings(settings) // point to
   , mTempSettings(*settings)  // copy
   , mbHasProfile(bHasProfile)
   , mbAllowTwiddleSettings(bAllowTwiddleSettings)
   // NULL out the control members until the controls are created.
   , mKeepSignal(NULL)
#ifdef ISOLATE_CHOICE
   , mKeepNoise(NULL)
#endif
#ifdef RESIDUE_CHOICE
   , mResidue(NULL)
#endif
{
   EffectDialog::Init();

   wxButton *const pButtonPreview =
      (wxButton *)wxWindow::FindWindowById(ID_EFFECT_PREVIEW, this);
   wxButton *const pButtonReduceNoise =
      (wxButton *)wxWindow::FindWindowById(wxID_OK, this);

   if (mbHasProfile || mbAllowTwiddleSettings) {
      pButtonPreview->Enable(!mbAllowTwiddleSettings);
      pButtonReduceNoise->SetFocus();
   }
   else {
      pButtonPreview->Enable(false);
      pButtonReduceNoise->Enable(false);
   }
}

void EffectNoiseReduction::Dialog::DisableControlsIfIsolating()
{
   // If Isolate is chosen, disable controls that define
   // "what to do with noise" rather than "what is noise."
   // Else, enable them.
   // This does NOT include sensitivity, NEW or old, nor
   // the choice of window functions, size, or step.
   // The method choice is not included, because it affects
   // which sensitivity slider is operative, and that is part
   // of what defines noise.

   static const int toDisable[] = {
      ID_GAIN_SLIDER,
      ID_GAIN_TEXT,

      ID_FREQ_SLIDER,
      ID_FREQ_TEXT,

#ifdef ATTACK_AND_RELEASE
      ID_ATTACK_TIME_SLIDER,
      ID_ATTACK_TIME_TEXT,

      ID_RELEASE_TIME_SLIDER,
      ID_RELEASE_TIME_TEXT,
#endif
   };
   static const auto nToDisable = sizeof(toDisable) / sizeof(toDisable[0]);

   bool bIsolating =
#ifdef ISOLATE_CHOICE
      mKeepNoise->GetValue();
#else
      false;
#endif
   for (auto ii = nToDisable; ii--;)
      wxWindow::FindWindowById(toDisable[ii], this)->Enable(!bIsolating);
}

#ifdef ADVANCED_SETTINGS
void EffectNoiseReduction::Dialog::EnableDisableSensitivityControls()
{
   wxChoice *const pChoice =
      static_cast<wxChoice*>(wxWindow::FindWindowById(ID_CHOICE_METHOD, this));
   const bool bOldMethod =
      pChoice->GetSelection() == DM_OLD_METHOD;
   wxWindow::FindWindowById(ID_OLD_SENSITIVITY_SLIDER, this)->Enable(bOldMethod);
   wxWindow::FindWindowById(ID_OLD_SENSITIVITY_TEXT, this)->Enable(bOldMethod);
   wxWindow::FindWindowById(ID_NEW_SENSITIVITY_SLIDER, this)->Enable(!bOldMethod);
   wxWindow::FindWindowById(ID_NEW_SENSITIVITY_TEXT, this)->Enable(!bOldMethod);
}
#endif

void EffectNoiseReduction::Dialog::OnGetProfile(wxCommandEvent & WXUNUSED(event))
{
   // Project has not be changed so skip pushing state
   EffectManager::Get().SetSkipStateFlag(true);

   if (!TransferDataFromWindow())
      return;

   // Return code distinguishes this first step from the actual effect
   EndModal(1);
}

// This handles the whole radio group
void EffectNoiseReduction::Dialog::OnNoiseReductionChoice( wxCommandEvent & WXUNUSED(event))
{
   if (mKeepSignal->GetValue())
      mTempSettings.mNoiseReductionChoice = NRC_REDUCE_NOISE;
#ifdef ISOLATE_CHOICE
   else if (mKeepNoise->GetValue())
      mTempSettings.mNoiseReductionChoice = NRC_ISOLATE_NOISE;
#endif
#ifdef RESIDUE_CHOICE
   else
      mTempSettings.mNoiseReductionChoice = NRC_LEAVE_RESIDUE;
#endif
   DisableControlsIfIsolating();
}

#ifdef ADVANCED_SETTINGS
void EffectNoiseReduction::Dialog::OnMethodChoice(wxCommandEvent &)
{
   EnableDisableSensitivityControls();
}
#endif

void EffectNoiseReduction::Dialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   if (!TransferDataFromWindow())
      return;

   // Save & restore parameters around Preview, because we didn't do OK.
   auto cleanup = valueRestorer( *m_pSettings );
   *m_pSettings = mTempSettings;
   m_pSettings->mDoProfile = false;

   m_pEffect->Preview(mAccess,
      // Don't need any UI updates for preview
      {},
      false);
}

void EffectNoiseReduction::Dialog::OnReduceNoise( wxCommandEvent & WXUNUSED(event))
{
   if (!TransferDataFromWindow())
      return;

   EndModal(2);
}

void EffectNoiseReduction::Dialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(0);
}

void EffectNoiseReduction::Dialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   HelpSystem::ShowHelp(this, "Noise_Reduction", true);
}

void EffectNoiseReduction::Dialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartStatic(XO("Step 1"));
   {
      S.AddVariableText(XO(
"Select a few seconds of just noise so Audacity knows what to filter out,\nthen click Get Noise Profile:"));
      //m_pButton_GetProfile =
      S.Id(ID_BUTTON_GETPROFILE).AddButton(XXO("&Get Noise Profile"));
   }
   S.EndStatic();

   S.StartStatic(XO("Step 2"));
   {
      S.AddVariableText(XO(
"Select all of the audio you want filtered, choose how much noise you want\nfiltered out, and then click 'OK' to reduce noise.\n"));

      S.StartMultiColumn(3, wxEXPAND);
      S.SetStretchyCol(2);
      {
         for (int id = FIRST_SLIDER; id < END_OF_BASIC_SLIDERS; id += 2) {
            const ControlInfo &info = controlInfo()[(id - FIRST_SLIDER) / 2];
            info.CreateControls(id, S);
         }
      }
      S.EndMultiColumn();

      S.StartMultiColumn(
         2
#ifdef RESIDUE_CHOICE
         +1
#endif
#ifdef ISOLATE_CHOICE
         +1
#endif
         ,
         wxALIGN_CENTER_HORIZONTAL);
      {
         S.AddPrompt(XXO("Noise:"));
         mKeepSignal = S.Id(ID_RADIOBUTTON_KEEPSIGNAL)
               /* i18n-hint: Translate differently from "Residue" ! */
               .AddRadioButton(XXO("Re&duce"));
#ifdef ISOLATE_CHOICE
         mKeepNoise = S.Id(ID_RADIOBUTTON_KEEPNOISE)
               .AddRadioButtonToGroup(XXO("&Isolate"));
#endif
#ifdef RESIDUE_CHOICE
         mResidue = S.Id(ID_RADIOBUTTON_RESIDUE)
               /* i18n-hint: Means the difference between effect and original sound.  Translate differently from "Reduce" ! */
               .AddRadioButtonToGroup(XXO("Resid&ue"));
#endif
      }
      S.EndMultiColumn();
   }
   S.EndStatic();


#ifdef ADVANCED_SETTINGS
   S.StartStatic(XO("Advanced Settings"));
   {
      S.StartMultiColumn(2);
      {
         S.TieChoice(XXO("&Window types:"),
            mTempSettings.mWindowTypes,
            []{
               TranslatableStrings windowTypeChoices;
               for (size_t ii = 0; ii < WT_N_WINDOW_TYPES; ++ii)
                  windowTypeChoices.push_back(windowTypesInfo[ii].name);
               return windowTypeChoices;
            }()
         );

         S.TieChoice(XXO("Window si&ze:"),
            mTempSettings.mWindowSizeChoice,
            {
               XO("8") ,
               XO("16") ,
               XO("32") ,
               XO("64") ,
               XO("128") ,
               XO("256") ,
               XO("512") ,
               XO("1024") ,
               XO("2048 (default)") ,
               XO("4096") ,
               XO("8192") ,
               XO("16384") ,
            }
         );

         S.TieChoice(XXO("S&teps per window:"),
            mTempSettings.mStepsPerWindowChoice,
            {
               XO("2") ,
               XO("4 (default)") ,
               XO("8") ,
               XO("16") ,
               XO("32") ,
               XO("64") ,
            }
         );

         S.Id(ID_CHOICE_METHOD)
         .TieChoice(XXO("Discrimination &method:"),
            mTempSettings.mMethod,
            []{
               TranslatableStrings methodChoices;
               auto nn = DM_N_METHODS;
#ifndef OLD_METHOD_AVAILABLE
               --nn;
#endif
               for (auto ii = 0; ii < nn; ++ii)
                  methodChoices.push_back(discriminationMethodInfo[ii].name);
               return methodChoices;
            }());
      }
      S.EndMultiColumn();

      S.StartMultiColumn(3, wxEXPAND);
      S.SetStretchyCol(2);
      {
         for (int id = END_OF_BASIC_SLIDERS; id < END_OF_ADVANCED_SLIDERS; id += 2) {
            const ControlInfo &info = controlInfo()[(id - FIRST_SLIDER) / 2];
            info.CreateControls(id, S);
         }
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#endif
}

bool EffectNoiseReduction::Dialog::TransferDataToWindow()
{
   // Do the choice controls:
   if (!EffectDialog::TransferDataToWindow())
      return false;

   for (int id = FIRST_SLIDER; id < END_OF_SLIDERS; id += 2) {
      wxSlider* slider =
         static_cast<wxSlider*>(wxWindow::FindWindowById(id, this));
      wxTextCtrl* text =
         static_cast<wxTextCtrl*>(wxWindow::FindWindowById(id + 1, this));
      const ControlInfo &info = controlInfo()[(id - FIRST_SLIDER) / 2];
      const double field = mTempSettings.*(info.field);
      text->SetValue(info.Text(field));
      slider->SetValue(info.SliderSetting(field));
   }

   mKeepSignal->SetValue(mTempSettings.mNoiseReductionChoice == NRC_REDUCE_NOISE);
#ifdef ISOLATE_CHOICE
   mKeepNoise->SetValue(mTempSettings.mNoiseReductionChoice == NRC_ISOLATE_NOISE);
#endif
#ifdef RESIDUE_CHOICE
   mResidue->SetValue(mTempSettings.mNoiseReductionChoice == NRC_LEAVE_RESIDUE);
#endif

   // Set the enabled states of controls
   DisableControlsIfIsolating();
#ifdef ADVANCED_SETTINGS
   EnableDisableSensitivityControls();
#endif

   return true;
}

bool EffectNoiseReduction::Dialog::TransferDataFromWindow()
{
   if( !wxWindow::Validate() )
      return false;
   // Do the choice controls:
   if (!EffectDialog::TransferDataFromWindow())
      return false;

   wxCommandEvent dummy;
   OnNoiseReductionChoice(dummy);

   return mTempSettings.Validate(m_pEffect);
}

void EffectNoiseReduction::Dialog::OnText(wxCommandEvent &event)
{
   int id = event.GetId();
   int idx = (id - FIRST_SLIDER - 1) / 2;
   const ControlInfo &info = controlInfo()[idx];
   wxTextCtrl* text =
      static_cast<wxTextCtrl*>(wxWindow::FindWindowById(id, this));
   wxSlider* slider =
      static_cast<wxSlider*>(wxWindow::FindWindowById(id - 1, this));
   double &field = mTempSettings.*(info.field);

   text->GetValue().ToDouble(&field);
   slider->SetValue(info.SliderSetting(field));
}

void EffectNoiseReduction::Dialog::OnSlider(wxCommandEvent &event)
{
   int id = event.GetId();
   int idx = (id - FIRST_SLIDER) / 2;
   const ControlInfo &info = controlInfo()[idx];
   wxSlider* slider =
      static_cast<wxSlider*>(wxWindow::FindWindowById(id, this));
   wxTextCtrl* text =
      static_cast<wxTextCtrl*>(wxWindow::FindWindowById(id + 1, this));
   double &field = mTempSettings.*(info.field);

   field = info.Value(slider->GetValue());
   text->SetValue(info.Text(field));
}
