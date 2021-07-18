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
#include "EBUR128.h"

class wxChoice;
class wxSimplebook;
class ShuttleGui;

class EffectLoudness final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectLoudness();
   virtual ~EffectLoudness();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectProcessor implementation

   bool DefineParams( ShuttleParams & S ) override;

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
   bool GetTrackRMS(WaveTrack* track, float& rms);
   bool ProcessOne(TrackIterRange<WaveTrack> range, bool analyse);
   void LoadBufferBlock(TrackIterRange<WaveTrack> range,
                        sampleCount pos, size_t len);
   bool AnalyseBufferBlock();
   bool ProcessBufferBlock();
   void StoreBufferBlock(TrackIterRange<WaveTrack> range,
                         sampleCount pos, size_t len);

   bool UpdateProgress();
   void OnChoice(wxCommandEvent & evt);
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   bool   mStereoInd;
   double mLUFSLevel;
   double mRMSLevel;
   bool   mDualMono;
   int    mNormalizeTo;

   double mCurT0;
   double mCurT1;
   double mProgressVal;
   int    mSteps;
   TranslatableString mProgressMsg;
   double mTrackLen;
   double mCurRate;

   float  mMult;
   float  mRatio;
   float  mRMS[2];
   std::unique_ptr<EBUR128> mLoudnessProcessor;

   wxSimplebook *mBook;
   wxChoice *mChoice;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;
   wxCheckBox *mDualMonoCheckBox;

   Floats mTrackBuffer[2];    // MM: must be increased once surround channels are supported
   size_t mTrackBufferLen;
   size_t mTrackBufferCapacity;
   bool   mProcStereo;

   DECLARE_EVENT_TABLE()
};

#endif
