/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   Reverb.h
   Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERB__
#define __AUDACITY_EFFECT_REVERB__

#include <wx/checkbox.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/string.h>

#include "../ShuttleGui.h"

#include "Effect.h"

#define REVERB_PLUGIN_SYMBOL XO("Reverb")

struct Reverb_priv_t;

class EffectReverb : public Effect
{
public:
   EffectReverb();
   virtual ~EffectReverb();

   struct Params
   {
      double mRoomSize;
      double mPreDelay;
      double mReverberance;
      double mHfDamping;
      double mToneLow;
      double mToneHigh;
      double mWetGain;
      double mDryGain;
      double mStereoWidth;
      bool mWetOnly;
   };

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual bool ProcessFinalize();
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);
   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);
   virtual wxArrayString GetFactoryPresets();
   virtual bool LoadFactoryPreset(int id);

   // Effect implementation

   bool Startup();
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

private:
   // EffectReverb implementation

   void SetTitle(const wxString & name = wxT(""));

#define SpinSliderHandlers(n) \
   void On ## n ## Slider(wxCommandEvent & evt); \
   void On ## n ## Text(wxCommandEvent & evt);

   SpinSliderHandlers(RoomSize);
   SpinSliderHandlers(PreDelay);
   SpinSliderHandlers(Reverberance);
   SpinSliderHandlers(HfDamping);
   SpinSliderHandlers(ToneLow);
   SpinSliderHandlers(ToneHigh);
   SpinSliderHandlers(WetGain);
   SpinSliderHandlers(DryGain);
   SpinSliderHandlers(StereoWidth);

#undef SpinSliderHandlers

private:
   int mNumChans;
   Reverb_priv_t *mP;

   Params mParams;

   bool mProcessingEvent;

#define SpinSlider(n) \
   wxSpinCtrl  *m ## n ## T; \
   wxSlider    *m ## n ## S;

   SpinSlider(RoomSize);
   SpinSlider(PreDelay);
   SpinSlider(Reverberance);
   SpinSlider(HfDamping);
   SpinSlider(ToneLow);
   SpinSlider(ToneHigh);
   SpinSlider(WetGain);
   SpinSlider(DryGain);
   SpinSlider(StereoWidth);

#undef SpinSlider

   wxCheckBox  *mWetOnlyC;

   DECLARE_EVENT_TABLE();
};

#endif
