/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)
  Max Maisel (Loudness)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"
#include "Biquad.h"

class ShuttleGui;

#define NORMALIZE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Normalize") }
#define LOUDNESS_PLUGIN_SYMBOL  ComponentInterfaceSymbol{ XO("Loudness") }

class EffectNormalize final : public Effect
{
public:
   EffectNormalize(bool isLoudness);
   virtual ~EffectNormalize();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   wxString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   bool Startup() override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectNormalize implementation

   enum AnalyseOperation
   {
      ANALYSE_NONE     = 0,
      ANALYSE_DC       = (1 << 0),
      ANALYSE_LOUDNESS = (1 << 1),
      ANALYSE_LOUDNESS_DC = ANALYSE_DC | ANALYSE_LOUDNESS
   };

   bool GetTrackMinMax(WaveTrack* track, float& min, float& max);
   bool GetTrackRMS(WaveTrack* track, float& rms);
   void AllocBuffers();
   bool ProcessOne(TrackIterRange<WaveTrack> range, bool analyse);
   bool LoadBufferBlock(TrackIterRange<WaveTrack> range,
                        sampleCount pos, size_t len);
   bool AnalyseBufferBlock();
   bool ProcessBufferBlock();
   void StoreBufferBlock(TrackIterRange<WaveTrack> range,
                         sampleCount pos, size_t len);
   void InitTrackAnalysis(bool dc);

   void CalcEBUR128HPF(float fs);
   void CalcEBUR128HSF(float fs);

   bool UpdateProgress();
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   bool   mIsLoudness;

   double mPeakLevel;
   double mLUFSLevel;
   double mRMSLevel;
   bool   mGain;
   bool   mDC;
   bool   mStereoInd;
   bool   mDualMono;
   int    mNormalizeTo;
   int    mGUINormalizeTo;

   double mCurT0;
   double mCurT1;
   double mProgressVal;
   int    mSteps;
   wxString mProgressMsg;
   double mTrackLen;
   double mCurRate;

   int    mOp;
   float  mMult;
   float  mOffset[2];   // MM: all those 2's must be increased once surround channels are supported
   float  mMin[2];
   float  mMax[2];
   float  mRMS[2];
   double mSum[2];
   sampleCount    mCount;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxChoice   *mNormalizeToCtl;
   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;
   wxCheckBox *mDualMonoCheckBox;

   Floats mTrackBuffer[2];    // MM: must be increased once surround channels are supported
   size_t mTrackBufferLen;
   size_t mTrackBufferCapacity;
   bool   mProcStereo;

   static const size_t HIST_BIN_COUNT = 65536;
   static constexpr double GAMMA_A = (-70.0 + 0.691) / 10.0;   // EBU R128 absolute threshold
   ArrayOf<long int> mLoudnessHist;
   Doubles mBlockRingBuffer;
   size_t mBlockRingPos;
   size_t mBlockRingSize;
   size_t mBlockSize;
   size_t mBlockOverlap;

   Biquad mR128HSF[2];
   Biquad mR128HPF[2];

   DECLARE_EVENT_TABLE()
};

#endif
