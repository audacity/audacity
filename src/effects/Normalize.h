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

#define NORMALIZE_PLUGIN_SYMBOL IdentInterfaceSymbol{ XO("Normalize") }

class EffectNormalize final : public Effect
{
public:
   EffectNormalize();
   virtual ~EffectNormalize();

   // IdentInterface implementation

   IdentInterfaceSymbol GetSymbol() override;
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
      ANALYSE_DC, ANALYSE_LOUDNESS, ANALYSE_LOUDNESS_DC
   };

   bool ProcessOne(
      WaveTrack * t, const wxString &msg, double& progress, float offset);
   bool AnalyseTrack(const WaveTrack * track, const wxString &msg,
                     double &progress, float &offset, float &extent);
   bool AnalyseTrackData(const WaveTrack * track, const wxString &msg, double &progress,
                     AnalyseOperation op, float &offset);
   void AnalyseDataDC(float *buffer, size_t len);
   void AnalyseDataLoudness(float *buffer, size_t len);
   void AnalyseDataLoudnessDC(float *buffer, size_t len);
   void ProcessData(float *buffer, size_t len, float offset);

   void CalcEBUR128HPF(float fs);
   void CalcEBUR128HSF(float fs);

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
   float  mMult;
   double mSum;
   double mSqSum;
   sampleCount    mCount;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mUseLoudnessCheckBox;
   wxCheckBox *mStereoIndCheckBox;

   bool mCreating;
   Biquad mR128HSF;
   Biquad mR128HPF;

   DECLARE_EVENT_TABLE()
};

#endif
