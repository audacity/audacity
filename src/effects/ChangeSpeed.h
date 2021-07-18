/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni

  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include "Effect.h"

class wxSlider;
class wxChoice;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

class EffectChangeSpeed final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectChangeSpeed();
   virtual ~EffectChangeSpeed();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;
   bool LoadFactoryDefaults() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   double CalcPreviewInputLength(double previewLength) override;
   bool Startup() override;
   bool Init() override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataFromWindow() override;
   bool TransferDataToWindow() override;

private:
   // EffectChangeSpeed implementation

   bool ProcessOne(WaveTrack *t, sampleCount start, sampleCount end);
   bool ProcessLabelTrack(LabelTrack *t);

   // handlers
   void OnText_PercentChange(wxCommandEvent & evt);
   void OnText_Multiplier(wxCommandEvent & evt);
   void OnSlider_PercentChange(wxCommandEvent & evt);
   void OnChoice_Vinyl(wxCommandEvent & evt);
   void OnTimeCtrl_ToLength(wxCommandEvent & evt);
   void OnTimeCtrlUpdate(wxCommandEvent & evt);

   // helper functions
   void Update_Text_PercentChange();   // Update control per current m_PercentChange.
   void Update_Text_Multiplier();      // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
   void Update_Vinyl();                // Update Vinyl controls for NEW percent change.
   void Update_TimeCtrl_ToLength();    // Update target length controls for NEW percent change.
   void UpdateUI();                    // Enable / disable OK / preview.

private:
   // track related
   int    mCurTrackNum;
   double mMaxNewLength;
   double mCurT0;
   double mCurT1;

   // control values
   double   m_PercentChange;  // percent change to apply to tempo
                              // -100% is meaningless, but sky's the upper limit.
                              // Slider is (-100, 200], but textCtrls can set higher.
   int      mFromVinyl;       // from standard vinyl speed (RPM) enum
   double   mFactor;          // scale factor calculated from percent change
   double   mFromLength;      // current selection length
   int      mTimeCtrlFormat;  // time control format index number
   double   mMultiplier;

   bool mbLoopDetect;

   // controls
   wxTextCtrl *      mpTextCtrl_PercentChange;
   wxTextCtrl *      mpTextCtrl_Multiplier;
   wxSlider *        mpSlider_PercentChange;
   wxChoice *        mpChoice_FromVinyl;
   wxChoice *        mpChoice_ToVinyl;
   NumericTextCtrl * mpFromLengthCtrl;
   NumericTextCtrl * mpToLengthCtrl;
   double mRate;

   // private effect parameters
   int      mToVinyl;         // to standard vinyl speed (rpm)
   double   mToLength;        // target length of selection
   NumericFormatSymbol mFormat;          // time control format

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_CHANGESPEED__
