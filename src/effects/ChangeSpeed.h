/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni

  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include <wx/choice.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"
#include "../Track.h"
#include "../WaveTrack.h"
#include "../widgets/NumericTextCtrl.h"

#include "Effect.h"

#define CHANGESPEED_PLUGIN_SYMBOL XO("Change Speed")

class EffectChangeSpeed : public Effect
{
public:
   EffectChangeSpeed();
   virtual ~EffectChangeSpeed();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool LoadFactoryDefaults();

   // Effect implementation

   virtual bool CheckWhetherSkipEffect();
   virtual double CalcPreviewInputLength(double previewLength);
   virtual bool Startup();
   virtual bool Init();
   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataFromWindow();
   virtual bool TransferDataToWindow();

private:
   // EffectChangeSpeed implementation

   bool ProcessOne(WaveTrack *t, sampleCount start, sampleCount end);
   bool ProcessLabelTrack(Track *t);

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
   void Update_Vinyl();                // Update Vinyl controls for new percent change.
   void Update_TimeCtrl_ToLength();    // Update target length controls for new percent change.
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
   wxString mFormat;          // time control format

   DECLARE_EVENT_TABLE();
};

#endif // __AUDACITY_EFFECT_CHANGESPEED__
