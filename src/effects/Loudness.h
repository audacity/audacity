/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.h

  Max Maisel (based on Normalize effect)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LOUDNESS__
#define __AUDACITY_EFFECT_LOUDNESS__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"
#include "Biquad.h"

class ShuttleGui;

#define LOUDNESS_PLUGIN_SYMBOL  ComponentInterfaceSymbol{ XO("Loudness") }

class EffectLoudness final : public Effect
{
public:
   EffectLoudness();
   virtual ~EffectLoudness();

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
   // EffectLoudness implementation

   void AllocBuffers();
   void FreeBuffers();
   void InitTrackAnalysis();
   bool ProcessOne(TrackIterRange<WaveTrack> range, bool analyse);
   bool LoadBufferBlock(TrackIterRange<WaveTrack> range,
                        sampleCount pos, size_t len);
   bool AnalyseBufferBlock();
   bool ProcessBufferBlock();
   void StoreBufferBlock(TrackIterRange<WaveTrack> range,
                         sampleCount pos, size_t len);

   bool UpdateProgress();
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   bool   mStereoInd;
   double mLUFSLevel;
   double mRMSLevel;
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

   float  mMult;
   float  mRatio;
   sampleCount    mCount;

   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;
   wxChoice   *mNormalizeToCtl;
   wxCheckBox *mDualMonoCheckBox;

   Floats mTrackBuffer[2];    // MM: must be increased once surround channels are supported
   size_t mTrackBufferLen;
   size_t mTrackBufferCapacity;
   bool   mProcStereo;

   DECLARE_EVENT_TABLE()
};

#endif
