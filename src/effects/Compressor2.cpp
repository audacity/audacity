/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor2.cpp

  Max Maisel

*******************************************************************//**

\class EffectCompressor2
\brief An Effect which reduces the dynamic level.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h
#include "Compressor2.h"

#include <math.h>
#include <numeric>

#include <wx/intl.h>
#include <wx/valgen.h>

#include "../AColor.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../ProjectFileManager.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"
#include "../widgets/Plot.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/Ruler.h"
#include "../widgets/SliderTextCtrl.h"

#include "LoadEffects.h"

enum kAlgorithms
{
   kExpFit,
   kEnvPT1,
   nAlgos
};

static const ComponentInterfaceSymbol kAlgorithmStrings[nAlgos] =
{
   { XO("Exponential-Fit") },
   { XO("Analog Model (PT1)") }
};

enum kCompressBy
{
   kAmplitude,
   kRMS,
   nBy
};

static const ComponentInterfaceSymbol kCompressByStrings[nBy] =
{
   { XO("peak amplitude") },
   { XO("RMS") }
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name         Type     Key                        Def         Min      Max       Scale
Param( Algorithm,      int,     wxT("Algorithm"),         kEnvPT1,    0,    nAlgos-1,  1   );
Param( CompressBy,     int,     wxT("CompressBy"),   kAmplitude,      0,   nBy-1,      1   );
Param( StereoInd,      bool,    wxT("StereoIndependent"), false,   false,   true,      1   );

Param( Threshold,      double,  wxT("Threshold"),        -12.0,   -60.0,     -1.0,     1.0 );
Param( Ratio,          double,  wxT("Ratio"),              2.0,     1.1,    100.0,    10.0 );
Param( KneeWidth,      double,  wxT("KneeWidth"),         10.0,     0.0,     20.0,    10.0 );
Param( AttackTime,     double,  wxT("AttackTime"),         0.2,     0.00001, 30.0, 20000.0 );
Param( ReleaseTime,    double,  wxT("ReleaseTime"),        1.0,     0.00001, 30.0, 20000.0 );
Param( LookaheadTime,  double,  wxT("LookaheadTime"),      0.0,     0.0,     10.0,   200.0 );
Param( LookbehindTime, double,  wxT("LookbehindTime"),     0.1,     0.0,     10.0,   200.0 );
Param( MakeupGain,     double,  wxT("MakeupGain"),         0.0,     0.0,    100.0,     1.0 );
Param( DryWet,         double,  wxT("DryWet"),           100.0,     0.0,    100.0,     1.0 );

inline int ScaleToPrecision(double scale)
{
   return ceil(log10(scale));
}

BEGIN_EVENT_TABLE(EffectCompressor2, wxEvtHandler)
   EVT_CHECKBOX(wxID_ANY, EffectCompressor2::OnUpdateUI)
   EVT_CHOICE(wxID_ANY, EffectCompressor2::OnUpdateUI)
   EVT_SLIDERTEXT(wxID_ANY, EffectCompressor2::OnUpdateUI)
END_EVENT_TABLE()

const ComponentInterfaceSymbol EffectCompressor2::Symbol
{ XO("Compressor v2") };

namespace{ BuiltinEffectsModule::Registration< EffectCompressor2 > reg; }

SlidingRmsPreprocessor::SlidingRmsPreprocessor(size_t windowSize, float gain)
   : mSum(0),
   mGain(gain),
   mWindow(windowSize, 0),
   mPos(0),
   mInsertCount(0)
{
}

float SlidingRmsPreprocessor::ProcessSample(float value)
{
   return DoProcessSample(value * value);
}

float SlidingRmsPreprocessor::ProcessSample(float valueL, float valueR)
{
   return DoProcessSample((valueL * valueL + valueR * valueR) / 2.0);
}

float SlidingRmsPreprocessor::DoProcessSample(float value)
{
   if(mInsertCount > REFRESH_WINDOW_EVERY)
   {
      // Update RMS sum directly from the circle buffer every
      // REFRESH_WINDOW_EVERY samples to avoid accumulation of rounding errors.
      mWindow[mPos] = value;
      Refresh();
   }
   else
   {
      // Calculate current level from root-mean-squared of
      // circular buffer ("RMS").
      mSum -= mWindow[mPos];
      mWindow[mPos] = value;
      mSum += mWindow[mPos];
      ++mInsertCount;
   }

   // Also refresh if there are severe rounding errors that
   // caused mRMSSum to be negative.
   if(mSum < 0)
      Refresh();

   mPos = (mPos + 1) % mWindow.size();

   // Multiply by gain (usually two) to approximately correct peak level
   // of standard audio (avoid clipping).
   return mGain * sqrt(mSum/float(mWindow.size()));
}

void SlidingRmsPreprocessor::Refresh()
{
   // Recompute the RMS sum periodically to prevent accumulation
   // of rounding errors during long waveforms.
   mSum = 0;
   for(const auto& sample : mWindow)
      mSum += sample;
   mInsertCount = 0;
}

SlidingMaxPreprocessor::SlidingMaxPreprocessor(size_t windowSize)
   : mWindow(windowSize, 0),
   mMaxes(windowSize, 0),
   mPos(0)
{
}

float SlidingMaxPreprocessor::ProcessSample(float value)
{
   return DoProcessSample(value);
}

float SlidingMaxPreprocessor::ProcessSample(float valueL, float valueR)
{
   return DoProcessSample((fabs(valueL) + fabs(valueR)) / 2.0);
}

float SlidingMaxPreprocessor::DoProcessSample(float value)
{
   size_t oldHead     = (mPos-1) % mWindow.size();
   size_t currentHead = mPos;
   size_t nextHead    = (mPos+1) % mWindow.size();
   mWindow[mPos] = value;
   mMaxes[mPos]  = std::max(value, mMaxes[oldHead]);

   if(mPos % ((mWindow.size()+1)/2) == 0)
   {
      mMaxes[mPos] = mWindow[mPos];
      for(size_t i = 1; i < mWindow.size(); ++i)
      {
         size_t pos1 = (mPos-i+mWindow.size()) % mWindow.size();
         size_t pos2 = (mPos-i+mWindow.size()+1) % mWindow.size();
         mMaxes[pos1] = std::max(mWindow[pos1], mMaxes[pos2]);
      }
   }
   mPos = nextHead;
   return std::max(mMaxes[currentHead], mMaxes[nextHead]);
}

EnvelopeDetector::EnvelopeDetector(size_t buffer_size)
   : mPos(0),
   mLookaheadBuffer(buffer_size, 0),
   mProcessingBuffer(buffer_size, 0),
   mProcessedBuffer(buffer_size, 0)
{
}

float EnvelopeDetector::ProcessSample(float value)
{
   float retval = mProcessedBuffer[mPos];
   mLookaheadBuffer[mPos++] = value;
   if(mPos == mProcessingBuffer.size())
   {
      Follow();
      mPos = 0;
      mProcessedBuffer.swap(mProcessingBuffer);
      mLookaheadBuffer.swap(mProcessingBuffer);
   }
   return retval;
}

size_t EnvelopeDetector::GetBlockSize() const
{
   wxASSERT(mProcessedBuffer.size() == mProcessingBuffer.size());
   wxASSERT(mProcessedBuffer.size() == mLookaheadBuffer.size());
   return mLookaheadBuffer.size();
}

ExpFitEnvelopeDetector::ExpFitEnvelopeDetector(
   float rate, float attackTime, float releaseTime)
   : EnvelopeDetector(TAU_FACTOR * (attackTime + 1.0) * rate)
{
   mAttackFactor = exp(-1.0 / (rate * attackTime));
   mReleaseFactor = exp(-1.0 / (rate * releaseTime));
}

void ExpFitEnvelopeDetector::Follow()
{
   /*
   "Follow"ing algorithm by Roger B. Dannenberg, taken from
   Nyquist.  His description follows.  -DMM

   Description: this is a sophisticated envelope follower.
   The input is an envelope, e.g. something produced with
   the AVG function. The purpose of this function is to
   generate a smooth envelope that is generally not less
   than the input signal. In other words, we want to "ride"
   the peaks of the signal with a smooth function. The
   algorithm is as follows: keep a current output value
   (called the "value"). The value is allowed to increase
   by at most rise_factor and decrease by at most fall_factor.
   Therefore, the next value should be between
   value * rise_factor and value * fall_factor. If the input
   is in this range, then the next value is simply the input.
   If the input is less than value * fall_factor, then the
   next value is just value * fall_factor, which will be greater
   than the input signal. If the input is greater than value *
   rise_factor, then we compute a rising envelope that meets
   the input value by working backwards in time, changing the
   previous values to input / rise_factor, input / rise_factor^2,
   input / rise_factor^3, etc. until this NEW envelope intersects
   the previously computed values. There is only a limited buffer
   in which we can work backwards, so if the NEW envelope does not
   intersect the old one, then make yet another pass, this time
   from the oldest buffered value forward, increasing on each
   sample by rise_factor to produce a maximal envelope. This will
   still be less than the input.

   The value has a lower limit of floor to make sure value has a
   reasonable positive value from which to begin an attack.
   */
   wxASSERT(mProcessedBuffer.size() == mProcessingBuffer.size());
   wxASSERT(mProcessedBuffer.size() == mLookaheadBuffer.size());

   // First apply a peak detect with the requested release rate.
   size_t buffer_size = mProcessingBuffer.size();
   double env = mProcessedBuffer[buffer_size-1];
   for(size_t i = 0; i < buffer_size; ++i)
   {
      env *= mReleaseFactor;
      if(mProcessingBuffer[i] > env)
         env = mProcessingBuffer[i];
      mProcessingBuffer[i] = env;
   }
   // Preprocess lookahead buffer as well.
   for(size_t i = 0; i < buffer_size; ++i)
   {
      env *= mReleaseFactor;
      if(mLookaheadBuffer[i] > env)
         env = mLookaheadBuffer[i];
      mLookaheadBuffer[i] = env;
   }

   // Next do the same process in reverse direction to get the
   // requested attack rate and preprocess lookahead buffer.
   for(ssize_t i = buffer_size - 1; i >= 0; --i)
   {
      env *= mAttackFactor;
      if(mLookaheadBuffer[i] < env)
         mLookaheadBuffer[i] = env;
      else
         env = mLookaheadBuffer[i];
   }
   for(ssize_t i = buffer_size - 1; i >= 0; --i)
   {
      if(mProcessingBuffer[i] < env * mAttackFactor)
      {
         env *= mAttackFactor;
         mProcessingBuffer[i] = env;
      }
      else if(mProcessingBuffer[i] > env)
         // Intersected the previous envelope buffer, so we are finished
         return;
      else
         ; // Do nothing if we are on a plateau from peak look-around
   }
}

Pt1EnvelopeDetector::Pt1EnvelopeDetector(
   float rate, float attackTime, float releaseTime, bool correctGain)
   : EnvelopeDetector(TAU_FACTOR * (attackTime + 1.0) * rate)
{
   // Approximate peak amplitude correction factor.
   if(correctGain)
      mGainCorrection = 1.0 + exp(attackTime / 30.0);
   else
      mGainCorrection = 1.0;

   mAttackFactor = 1.0 / (attackTime * rate);
   mReleaseFactor  = 1.0 / (releaseTime  * rate);
}

void Pt1EnvelopeDetector::Follow()
{
   wxASSERT(mProcessedBuffer.size() == mProcessingBuffer.size());
   wxASSERT(mProcessedBuffer.size() == mLookaheadBuffer.size());

   // Simulate analog compressor with PT1 characteristic.
   size_t buffer_size = mProcessingBuffer.size();
   float level = mProcessedBuffer[buffer_size-1] / mGainCorrection;
   for(size_t i = 0; i < buffer_size; ++i)
   {
      if(mProcessingBuffer[i] >= level)
         level += mAttackFactor * (mProcessingBuffer[i] - level);
      else
         level += mReleaseFactor * (mProcessingBuffer[i] - level);
      mProcessingBuffer[i] = level * mGainCorrection;
   }
}

EffectCompressor2::EffectCompressor2()
   : mIgnoreGuiEvents(false)
{
   mAlgorithm = DEF_Algorithm;
   mCompressBy = DEF_CompressBy;
   mStereoInd = DEF_StereoInd;

   mThresholdDB = DEF_Threshold;
   mRatio = DEF_Ratio;                    // positive number > 1.0
   mKneeWidthDB = DEF_KneeWidth;
   mAttackTime = DEF_AttackTime;          // seconds
   mReleaseTime = DEF_ReleaseTime;          // seconds
   mLookaheadTime = DEF_LookaheadTime;
   mLookbehindTime = DEF_LookbehindTime;
   mMakeupGainPct = DEF_MakeupGain;
   mDryWetPct = DEF_DryWet;

   SetLinearEffectFlag(false);
}

EffectCompressor2::~EffectCompressor2()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectCompressor2::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectCompressor2::GetDescription()
{
   return XO("Reduces the dynamic of one or more tracks");
}

wxString EffectCompressor2::ManualPage()
{
   return wxT("Compressor2");
}

// EffectDefinitionInterface implementation

EffectType EffectCompressor2::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectCompressor2::DefineParams( ShuttleParams & S )
{
   S.SHUTTLE_PARAM(mAlgorithm, Algorithm);
   S.SHUTTLE_PARAM(mCompressBy, CompressBy);
   S.SHUTTLE_PARAM(mStereoInd, StereoInd);

   S.SHUTTLE_PARAM(mThresholdDB, Threshold);
   S.SHUTTLE_PARAM(mRatio, Ratio);
   S.SHUTTLE_PARAM(mKneeWidthDB, KneeWidth);
   S.SHUTTLE_PARAM(mAttackTime, AttackTime);
   S.SHUTTLE_PARAM(mReleaseTime, ReleaseTime);
   S.SHUTTLE_PARAM(mLookaheadTime, LookaheadTime);
   S.SHUTTLE_PARAM(mLookbehindTime, LookbehindTime);
   S.SHUTTLE_PARAM(mMakeupGainPct, MakeupGain);
   S.SHUTTLE_PARAM(mDryWetPct, DryWet);

   return true;
}

bool EffectCompressor2::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_Algorithm, mAlgorithm);
   parms.Write(KEY_CompressBy, mCompressBy);
   parms.Write(KEY_StereoInd, mStereoInd);

   parms.Write(KEY_Threshold, mThresholdDB);
   parms.Write(KEY_Ratio, mRatio);
   parms.Write(KEY_KneeWidth, mKneeWidthDB);
   parms.Write(KEY_AttackTime, mAttackTime);
   parms.Write(KEY_ReleaseTime, mReleaseTime);
   parms.Write(KEY_LookaheadTime, mLookaheadTime);
   parms.Write(KEY_LookbehindTime, mLookbehindTime);
   parms.Write(KEY_MakeupGain, mMakeupGainPct);
   parms.Write(KEY_DryWet, mDryWetPct);

   return true;
}

bool EffectCompressor2::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyInt(Algorithm);
   ReadAndVerifyInt(CompressBy);
   ReadAndVerifyBool(StereoInd);

   ReadAndVerifyDouble(Threshold);
   ReadAndVerifyDouble(Ratio);
   ReadAndVerifyDouble(KneeWidth);
   ReadAndVerifyDouble(AttackTime);
   ReadAndVerifyDouble(ReleaseTime);
   ReadAndVerifyDouble(LookaheadTime);
   ReadAndVerifyDouble(LookbehindTime);
   ReadAndVerifyDouble(MakeupGain);
   ReadAndVerifyDouble(DryWet);

   mAlgorithm = Algorithm;
   mCompressBy = CompressBy;
   mStereoInd = StereoInd;

   mThresholdDB = Threshold;
   mRatio = Ratio;
   mKneeWidthDB = KneeWidth;
   mAttackTime = AttackTime;
   mReleaseTime = ReleaseTime;
   mLookaheadTime = LookaheadTime;
   mLookbehindTime = LookbehindTime;
   mMakeupGainPct = MakeupGain;
   mDryWetPct = DryWet;

   return true;
}

// Effect implementation

bool EffectCompressor2::CheckWhetherSkipEffect()
{
   return false;
}

bool EffectCompressor2::Startup()
{
   wxString base = wxT("/Effects/Compressor2/");
   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      mAlgorithm = DEF_Algorithm;
      mCompressBy = DEF_CompressBy;
      mStereoInd = DEF_StereoInd;

      mThresholdDB = DEF_Threshold;
      mRatio = DEF_Ratio;                    // positive number > 1.0
      mKneeWidthDB = DEF_KneeWidth;
      mAttackTime = DEF_AttackTime;          // seconds
      mReleaseTime = DEF_ReleaseTime;          // seconds
      mLookaheadTime = DEF_LookaheadTime;
      mLookbehindTime = DEF_LookbehindTime;
      mMakeupGainPct = DEF_MakeupGain;
      mDryWetPct = DEF_DryWet;

      SaveUserPreset(GetCurrentSettingsGroup());

      gPrefs->Flush();
   }
   return true;
}

bool EffectCompressor2::Process()
{
   return false;
}

void EffectCompressor2::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(10);

   S.StartHorizontalLay(wxEXPAND, true);
   {
      PlotData* plot;

      mGainPlot = S.MinSize( { 200, 200 } )
         .AddPlot({}, -60, 0, -60, 0, XO("dB"), XO("dB"),
            Ruler::LinearDBFormat, Ruler::LinearDBFormat);

      plot = mGainPlot->GetPlotData(0);
      plot->pen = std::unique_ptr<wxPen>(
         safenew wxPen(AColor::WideEnvelopePen));
      plot->xdata.resize(61);
      plot->ydata.resize(61);
      std::iota(plot->xdata.begin(), plot->xdata.end(), -60);

      mResponsePlot = S.MinSize( { 200, 200 } )
         .AddPlot({}, 0, 5, -0.2, 1.2, XO("s"), XO(""),
            Ruler::IntFormat, Ruler::RealFormat, 2);

      plot = mResponsePlot->GetPlotData(0);
      plot->pen = std::unique_ptr<wxPen>(
         safenew wxPen(AColor::WideEnvelopePen));
      plot->xdata = {0, RESPONSE_PLOT_STEP_START, RESPONSE_PLOT_STEP_START,
         RESPONSE_PLOT_STEP_STOP, RESPONSE_PLOT_STEP_STOP, 5};
      plot->ydata = {0, 0, 1, 1, 0, 0};

      plot = mResponsePlot->GetPlotData(1);
      plot->pen = std::unique_ptr<wxPen>(
         safenew wxPen(AColor::WideEnvelopePen));
      plot->pen->SetColour(wxColor( 230,80,80 )); // Same color as TrackArtist RMS red.
      plot->pen->SetWidth(2);
      plot->xdata.resize(RESPONSE_PLOT_SAMPLES+1);
      plot->ydata.resize(RESPONSE_PLOT_SAMPLES+1);
      for(size_t x = 0; x < plot->xdata.size(); ++x)
         plot->xdata[x] = x * float(RESPONSE_PLOT_TIME) / float(RESPONSE_PLOT_SAMPLES);
   }
   S.EndHorizontalLay();

   S.SetBorder(5);

   S.StartStatic(XO("Algorithm"));
   {
      S.StartMultiColumn(2, wxALIGN_LEFT);
      {
         S.SetStretchyCol(1);

         wxChoice* ctrl = nullptr;

         ctrl = S.Validator<wxGenericValidator>(&mAlgorithm)
            .AddChoice(XO("Envelope Algorithm:"),
               Msgids(kAlgorithmStrings, nAlgos),
               mAlgorithm);

         wxSize box_size = ctrl->GetMinSize();
         int width = S.GetParent()->GetTextExtent(wxString::Format(
            "%sxxxx",  kAlgorithmStrings[nAlgos-1].Translation())).GetWidth();
         box_size.SetWidth(width);
         ctrl->SetMinSize(box_size);

         ctrl = S.Validator<wxGenericValidator>(&mCompressBy)
            .AddChoice(XO("Compress based on:"),
               Msgids(kCompressByStrings, nBy),
               mCompressBy);
         ctrl->SetMinSize(box_size);

         S.Validator<wxGenericValidator>(&mStereoInd)
            .AddCheckBox(XO("Compress stereo channels independently"),
               DEF_StereoInd);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Compressor"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);
         int textbox_width = S.GetParent()->GetTextExtent("0.000001").GetWidth();
         SliderTextCtrl* ctrl = nullptr;

         S.AddVariableText(XO("Threshold:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Threshold"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_Threshold, MAX_Threshold,
               MIN_Threshold, ScaleToPrecision(SCL_Threshold), &mThresholdDB);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("dB"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Ratio:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Ratio"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_Ratio, MAX_Ratio, MIN_Ratio,
               ScaleToPrecision(SCL_Ratio), &mRatio);
         /* i18n-hint: Unless your language has a different convention for ratios,
          * like 8:1, leave as is.*/
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO(":1"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Knee Width:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Knee Width"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_KneeWidth, MAX_KneeWidth,
               MIN_KneeWidth, ScaleToPrecision(SCL_KneeWidth),
               &mKneeWidthDB);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("dB"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Attack Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Attack Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_AttackTime, MAX_AttackTime,
               MIN_AttackTime, ScaleToPrecision(SCL_AttackTime),
               &mAttackTime, SCL_AttackTime / 1000);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Release Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Release Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_ReleaseTime, MAX_ReleaseTime,
               MIN_ReleaseTime, ScaleToPrecision(SCL_ReleaseTime),
               &mReleaseTime, SCL_ReleaseTime / 1000);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Lookahead Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Lookahead Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_LookaheadTime, MAX_LookaheadTime,
               MIN_LookaheadTime, ScaleToPrecision(SCL_LookaheadTime),
               &mLookaheadTime);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Hold Time:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Hold Time"))
            .Style(SliderTextCtrl::HORIZONTAL | SliderTextCtrl::LOG)
            .AddSliderTextCtrl({}, DEF_LookbehindTime, MAX_LookbehindTime,
               MIN_LookbehindTime, ScaleToPrecision(SCL_LookbehindTime),
               &mLookbehindTime);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("s"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         /* i18n-hint: Make-up, i.e. correct for any reduction, rather than fabricate it.*/
         S.AddVariableText(XO("Make-up Gain:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Make-up Gain"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_MakeupGain, MAX_MakeupGain,
               MIN_MakeupGain, ScaleToPrecision(SCL_MakeupGain),
               &mMakeupGainPct);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("%"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         S.AddVariableText(XO("Dry/Wet:"), true,
            wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         ctrl = S.Name(XO("Dry/Wet"))
            .Style(SliderTextCtrl::HORIZONTAL)
            .AddSliderTextCtrl({}, DEF_DryWet, MAX_DryWet,
               MIN_DryWet, ScaleToPrecision(SCL_DryWet),
               &mDryWetPct);
         ctrl->SetMinTextboxWidth(textbox_width);
         S.AddVariableText(XO("%"), true,
            wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
      }
      S.EndMultiColumn();
   }
   S.EndVerticalLay();
}

bool EffectCompressor2::TransferDataToWindow()
{
   // Transferring data to window causes spurious UpdateUI events
   // which would reset the UI values to the previous value.
   // This guard lets the program ignore them.
   mIgnoreGuiEvents = true;
   if (!mUIParent->TransferDataToWindow())
   {
      mIgnoreGuiEvents = false;
      return false;
   }

   UpdateUI();
   mIgnoreGuiEvents = false;
   return true;
}

bool EffectCompressor2::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }
   return true;
}

// EffectCompressor2 implementation

void EffectCompressor2::InitGainCalculation()
{
   mMakeupGainDB = mMakeupGainPct / 100.0 *
      -(mThresholdDB * (1.0 - 1.0 / mRatio));
   mMakeupGain = DB_TO_LINEAR(mMakeupGainDB);
}

double EffectCompressor2::CompressorGain(double env)
{
   double kneeCond;
   double envDB = LINEAR_TO_DB(env);

   // envDB can become NaN is env is exactly zero.
   // As solution, use a very low dB value to prevent NaN propagation.
   if(isnan(envDB))
      envDB = -200;

   kneeCond = 2.0 * (envDB - mThresholdDB);
   if(kneeCond < -mKneeWidthDB)
   {
      // Below threshold: only apply make-up gain
      return mMakeupGain;
   }
   else if(kneeCond >= mKneeWidthDB)
   {
      // Above threshold: apply compression and make-up gain
      return DB_TO_LINEAR(mThresholdDB +
         (envDB - mThresholdDB) / mRatio + mMakeupGainDB - envDB);
   }
   else
   {
      // Within knee: apply interpolated compression and make-up gain
      return DB_TO_LINEAR(
         (1.0 / mRatio - 1.0)
         * pow(envDB - mThresholdDB + mKneeWidthDB / 2.0, 2)
         / (2.0 * mKneeWidthDB) + mMakeupGainDB);
   }
}

void EffectCompressor2::OnUpdateUI(wxCommandEvent & WXUNUSED(evt))
{
   if(!mIgnoreGuiEvents)
      TransferDataFromWindow();
   UpdateUI();
}

void EffectCompressor2::UpdateUI()
{
   UpdateCompressorPlot();
   UpdateResponsePlot();
}

void EffectCompressor2::UpdateCompressorPlot()
{
   PlotData* plot;
   plot = mGainPlot->GetPlotData(0);
   wxASSERT(plot->xdata.size() == plot->ydata.size());

   InitGainCalculation();
   size_t xsize = plot->xdata.size();
   for(size_t i = 0; i < xsize; ++i)
      plot->ydata[i] = plot->xdata[i] +
         LINEAR_TO_DB(CompressorGain(DB_TO_LINEAR(plot->xdata[i])));

// XXX: accessibility but fails with TranslatableString required
//   mGainPlot->SetName(wxString::Format(
//      "Compressor gain reduction: %.1f dB", plot->ydata[xsize-1]));
   mGainPlot->Refresh(false);
}

void EffectCompressor2::UpdateResponsePlot()
{
   PlotData* plot;
   plot = mResponsePlot->GetPlotData(1);
   wxASSERT(plot->xdata.size() == plot->ydata.size());

   std::unique_ptr<SamplePreprocessor> preproc;
   std::unique_ptr<EnvelopeDetector> envelope;
   float plot_rate = RESPONSE_PLOT_SAMPLES / RESPONSE_PLOT_TIME;

   size_t window_size =
      std::max(1, int(round((mLookaheadTime + mLookbehindTime) * plot_rate)));
   size_t lookahead_size =
      std::max(0, int(round(mLookaheadTime * plot_rate)));

   if(mCompressBy == kRMS)
      preproc = std::unique_ptr<SamplePreprocessor>(
         safenew SlidingRmsPreprocessor(window_size, 1.0));
   else
      preproc = std::unique_ptr<SamplePreprocessor>(
         safenew SlidingMaxPreprocessor(window_size));

   if(mAlgorithm == kExpFit)
      envelope = std::unique_ptr<EnvelopeDetector>(
         safenew ExpFitEnvelopeDetector(plot_rate, mAttackTime, mReleaseTime));
   else
      envelope = std::unique_ptr<EnvelopeDetector>(
         safenew Pt1EnvelopeDetector(plot_rate, mAttackTime, mReleaseTime, false));

   ssize_t step_start = RESPONSE_PLOT_STEP_START * plot_rate - lookahead_size;
   ssize_t step_stop = RESPONSE_PLOT_STEP_STOP * plot_rate - lookahead_size;

   ssize_t xsize = plot->xdata.size();
   ssize_t block_size = envelope->GetBlockSize();
   for(ssize_t i = -lookahead_size; i < 2*block_size; ++i)
   {
      if(i < step_start || i > step_stop)
         envelope->ProcessSample(preproc->ProcessSample(0));
      else
         envelope->ProcessSample(preproc->ProcessSample(1));
   }
   for(ssize_t i = 0; i < xsize; ++i)
      plot->ydata[i] = envelope->ProcessSample(preproc->ProcessSample(0));

   mResponsePlot->Refresh(false);
}
