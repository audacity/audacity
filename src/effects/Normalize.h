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
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"
#include "Biquad.h"

class ShuttleGui;

#define NORMALIZE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Normalize") }

class EffectNormalize final : public Effect
{
public:
   EffectNormalize();
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
   void AllocBuffers(SelectedTrackListOfKindIterator iter);
   bool ProcessOne(SelectedTrackListOfKindIterator iter, bool analyse);
   bool LoadBufferBlock(WaveTrack* track1, WaveTrack* track2,
                        sampleCount pos, size_t len);
   bool AnalyseBufferBlock();
   bool ProcessBufferBlock();
   void StoreBufferBlock(WaveTrack* track1, WaveTrack* track2,
                         sampleCount pos, size_t len);
   void InitTrackAnalysis();

   void CalcEBUR128HPF(float fs);
   void CalcEBUR128HSF(float fs);

   bool UpdateProgress();
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   double mPeakLevel;
   double mLUFSLevel;
   bool   mGain;
   bool   mDC;
   bool   mStereoInd;
   bool   mUseLoudness;
   bool   mGUIUseLoudness;

   double mCurT0;
   double mCurT1;
   double mProgressVal;
   int    mSteps;
   wxString mProgressMsg;
   double mTrackLen;
   double mCurRate;

   int    mOp;
   float  mMult;
   float  mOffset[2];
   float  mMin[2];
   float  mMax[2];
   double mSum[2];
   sampleCount    mCount;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mUseLoudnessCheckBox;
   wxCheckBox *mStereoIndCheckBox;

   Floats mTrackBuffer[2];
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

   bool mCreating;
   Biquad mR128HSF[2];
   Biquad mR128HPF[2];

   DECLARE_EVENT_TABLE()
};

#endif
