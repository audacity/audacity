/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include <wx/checkbox.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"

#include "Effect.h"

#define BASSTREBLE_PLUGIN_SYMBOL XO("Bass and Treble")

class EffectBassTreble : public Effect
{
public:
   EffectBassTreble();
   virtual ~EffectBassTreble();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);
   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect Implementation

   virtual bool Startup();
   virtual bool InitPass1();
   virtual bool InitPass2();

   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectBassTreble implementation

   void Coefficents(double hz, float slope, double gain, int type,
                    float& a0, float& a1, float& a2, float& b0, float& b1, float& b2);
   float DoFilter(float in);
   void UpdateUI();

   void OnBassText(wxCommandEvent & evt);
   void OnTrebleText(wxCommandEvent & evt);
   void OnLevelText(wxCommandEvent & evt);
   void OnBassSlider(wxCommandEvent & evt);
   void OnTrebleSlider(wxCommandEvent & evt);
   void OnLevelSlider(wxCommandEvent & evt);
   void OnNormalize(wxCommandEvent & evt);

private:
   float xn1Bass, xn2Bass, yn1Bass, yn2Bass,
         wBass, swBass, cwBass, aBass, bBass,
         a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
   // High shelf
   float xn1Treble, xn2Treble, yn1Treble, yn2Treble,
         wTreble, swTreble, cwTreble, aTreble, bTreble,
         b0Treble, b1Treble, b2Treble, a0Treble, a1Treble, a2Treble;

   double dB_bass, dB_treble, dB_level;
   double mMax;
   bool   mbNormalize;
   double mPreGain;

   wxSlider *mBassS;
   wxSlider *mTrebleS;
   wxSlider *mLevelS;
   wxTextCtrl *mBassT;
   wxTextCtrl *mTrebleT;
   wxTextCtrl *mLevelT;
   wxCheckBox *mNormalizeCheckBox;
   wxStaticText *mWarning;

   DECLARE_EVENT_TABLE();
};

#endif
