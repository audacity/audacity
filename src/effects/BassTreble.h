/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include <wx/event.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/checkbox.h>

#include "Effect.h"

class ShuttleGui;

#define BASSTREBLE_PLUGIN_SYMBOL XO("Bass and Treble")

class EffectBassTrebleState
{
public:
   float samplerate;
   double treble;
   double bass;
   double gain;
   double slope, hzBass, hzTreble;
   double a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
   double a0Treble, a1Treble, a2Treble, b0Treble, b1Treble, b2Treble;
   double xn1Bass, xn2Bass, yn1Bass, yn2Bass;
   double xn1Treble, xn2Treble, yn1Treble, yn2Treble;
};

WX_DECLARE_OBJARRAY(EffectBassTrebleState, EffectBassTrebleStateArray);

class EffectBassTreble final : public Effect
{
public:
   EffectBassTreble();
   virtual ~EffectBassTreble();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   bool SupportsRealtime() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool RealtimeInitialize() override;
   bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize() override;
   size_t RealtimeProcess(int group,
                               float **inbuf,
                               float **outbuf,
                               size_t numSamples) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;


   // Effect Implementation

   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   bool CheckWhetherSkipEffect() override;

private:
   // EffectBassTreble implementation

   void InstanceInit(EffectBassTrebleState & data, float sampleRate);
   size_t InstanceProcess(EffectBassTrebleState & data, float **inBlock, float **outBlock, size_t blockLen);

   void Coefficents(double hz, double slope, double gain, double samplerate, int type,
                    double& a0, double& a1, double& a2, double& b0, double& b1, double& b2);
   float DoFilter(EffectBassTrebleState & data, float in);

   void OnBassText(wxCommandEvent & evt);
   void OnTrebleText(wxCommandEvent & evt);
   void OnGainText(wxCommandEvent & evt);
   void OnBassSlider(wxCommandEvent & evt);
   void OnTrebleSlider(wxCommandEvent & evt);
   void OnGainSlider(wxCommandEvent & evt);
   void OnLinkCheckbox(wxCommandEvent & evt);

   // Auto-adjust gain to reduce variation in peak level
   void UpdateGain(double oldVal, int control );

private:
   EffectBassTrebleState mMaster;
   EffectBassTrebleStateArray mSlaves;

   double      mBass;
   double      mTreble;
   double      mGain;
   bool        mLink;

   wxSlider    *mBassS;
   wxSlider    *mTrebleS;
   wxSlider    *mGainS;

   wxTextCtrl  *mBassT;
   wxTextCtrl  *mTrebleT;
   wxTextCtrl  *mGainT;

   wxCheckBox  *mLinkCheckBox;

   DECLARE_EVENT_TABLE()
};

#endif
