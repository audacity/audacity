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

#include "Effect.h"

class ShuttleGui;

#define REVERB_PLUGIN_SYMBOL XO("Reverb")

struct Reverb_priv_t;

class EffectReverb final : public Effect
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

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;
   wxArrayString GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;

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

   SpinSliderHandlers(RoomSize)
   SpinSliderHandlers(PreDelay)
   SpinSliderHandlers(Reverberance)
   SpinSliderHandlers(HfDamping)
   SpinSliderHandlers(ToneLow)
   SpinSliderHandlers(ToneHigh)
   SpinSliderHandlers(WetGain)
   SpinSliderHandlers(DryGain)
   SpinSliderHandlers(StereoWidth)

#undef SpinSliderHandlers

private:
   unsigned mNumChans {};
   Reverb_priv_t *mP;

   Params mParams;

   bool mProcessingEvent;

#define SpinSlider(n) \
   wxSpinCtrl  *m ## n ## T; \
   wxSlider    *m ## n ## S;

   SpinSlider(RoomSize)
   SpinSlider(PreDelay)
   SpinSlider(Reverberance)
   SpinSlider(HfDamping)
   SpinSlider(ToneLow)
   SpinSlider(ToneHigh)
   SpinSlider(WetGain)
   SpinSlider(DryGain)
   SpinSlider(StereoWidth)

#undef SpinSlider

   wxCheckBox  *mWetOnlyC;

   DECLARE_EVENT_TABLE()
};

#endif
