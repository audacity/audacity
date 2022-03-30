/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Reverb.cpp
   Rob Sykes, Vaughan Johnson

******************************************************************//**

\class EffectReverb
\brief A reverberation effect

*//*******************************************************************/


#include "Reverb.h"
#include "LoadEffects.h"

#include <wx/arrstr.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>

#include "Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

#include "Reverb_libSoX.h"

enum 
{
   ID_RoomSize = 10000,
   ID_PreDelay,
   ID_Reverberance,
   ID_HfDamping,
   ID_ToneLow,
   ID_ToneHigh,
   ID_WetGain,
   ID_DryGain,
   ID_StereoWidth,
   ID_WetOnly
};

const EffectParameterMethods& EffectReverb::Parameters() const
{
   static CapturedParameters<EffectReverb,
      RoomSize, PreDelay, Reverberance, HfDamping, ToneLow, ToneHigh,
      WetGain, DryGain, StereoWidth, WetOnly
   > parameters;
   return parameters;
}

static const struct
{
   const TranslatableString name;
   EffectReverb::Params params;
}
FactoryPresets[] =
{
   //                         Room  Pre            Hf       Tone Tone  Wet   Dry   Stereo Wet
   // Name                    Size, Delay, Reverb, Damping, Low, High, Gain, Gain, Width, Only
   { XO("Vocal I" ),          { 70,   20,    40,     99,      100, 50,   -12,  0,    70,    false } },
   { XO("Vocal II"),          { 50,   0,     50,     99,      50,  100,  -1,   -1,   70,    false } },
   { XO("Bathroom"),          { 16,   8,     80,     0,       0,   100,  -6,   0,    100,   false } },
   { XO("Small Room Bright"), { 30,   10,    50,     50,      50,  100,  -1,   -1,   100,   false } },
   { XO("Small Room Dark"),   { 30,   10,    50,     50,      100, 0,    -1,   -1,   100,   false } },
   { XO("Medium Room"),       { 75,   10,    40,     50,      100, 70,   -1,   -1,   70,    false } },
   { XO("Large Room"),        { 85,   10,    40,     50,      100, 80,    0,   -6,   90,    false } },
   { XO("Church Hall"),       { 90,   32,    60,     50,      100, 50,    0,   -12,  100,   false } },
   { XO("Cathedral"),         { 90,   16,    90,     50,      100, 0,     0,   -20,  100,   false } },
};

struct Reverb_priv_t
{
   reverb_t reverb;
   float *dry;
   float *wet[2];
};

//
// EffectReverb
//

const ComponentInterfaceSymbol EffectReverb::Symbol
{ XO("Reverb") };

namespace{ BuiltinEffectsModule::Registration< EffectReverb > reg; }

BEGIN_EVENT_TABLE(EffectReverb, wxEvtHandler)

#define SpinSliderEvent(n) \
   EVT_SLIDER(ID_ ## n, EffectReverb::On ## n ## Slider) \
   EVT_TEXT(ID_ ## n, EffectReverb::On ## n ## Text)

   SpinSliderEvent(RoomSize)
   SpinSliderEvent(PreDelay)
   SpinSliderEvent(Reverberance)
   SpinSliderEvent(HfDamping)
   SpinSliderEvent(ToneLow)
   SpinSliderEvent(ToneHigh)
   SpinSliderEvent(WetGain)
   SpinSliderEvent(DryGain)
   SpinSliderEvent(StereoWidth)

#undef SpinSliderEvent 

END_EVENT_TABLE()

EffectReverb::EffectReverb()
{
   Parameters().Reset(*this);
   mProcessingEvent = false;

   SetLinearEffectFlag(true);
}

EffectReverb::~EffectReverb()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectReverb::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectReverb::GetDescription() const
{
   return XO("Adds ambience or a \"hall effect\"");
}

ManualPageID EffectReverb::ManualPage() const
{
   return L"Reverb";
}

// EffectDefinitionInterface implementation

EffectType EffectReverb::GetType() const
{
   return EffectTypeProcess;
}

// EffectProcessor implementation

unsigned EffectReverb::GetAudioInCount() const
{
   return 2;
}

unsigned EffectReverb::GetAudioOutCount() const
{
   return 2;
}

static size_t BLOCK = 16384;

bool EffectReverb::ProcessInitialize(
   EffectSettings &, sampleCount, ChannelNames chanMap)
{
   bool isStereo = false;
   mNumChans = 1;
   if (chanMap && chanMap[0] != ChannelNameEOL && chanMap[1] == ChannelNameFrontRight)
   {
      isStereo = true;
      mNumChans = 2;
   }

   mP = (Reverb_priv_t *) calloc(sizeof(*mP), mNumChans);

   for (unsigned int i = 0; i < mNumChans; i++)
   {
      reverb_create(&mP[i].reverb,
                    mSampleRate,
                    mParams.mWetGain,
                    mParams.mRoomSize,
                    mParams.mReverberance,
                    mParams.mHfDamping,
                    mParams.mPreDelay,
                    mParams.mStereoWidth * (isStereo ? 1 : 0),
                    mParams.mToneLow,
                    mParams.mToneHigh,
                    BLOCK,
                    mP[i].wet);
   }

   return true;
}

bool EffectReverb::ProcessFinalize()
{
   for (unsigned int i = 0; i < mNumChans; i++)
   {
      reverb_delete(&mP[i].reverb);
   }

   free(mP);

   return true;
}

size_t EffectReverb::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   const float *ichans[2] = {NULL, NULL};
   float *ochans[2] = {NULL, NULL};

   for (unsigned int c = 0; c < mNumChans; c++)
   {
      ichans[c] = inBlock[c];
      ochans[c] = outBlock[c];
   }
   
   float const dryMult = mParams.mWetOnly ? 0 : dB_to_linear(mParams.mDryGain);

   auto remaining = blockLen;

   while (remaining)
   {
      auto len = std::min(remaining, decltype(remaining)(BLOCK));
      for (unsigned int c = 0; c < mNumChans; c++)
      {
         // Write the input samples to the reverb fifo.  Returned value is the address of the
         // fifo buffer which contains a copy of the input samples.
         mP[c].dry = (float *) fifo_write(&mP[c].reverb.input_fifo, len, ichans[c]);
         reverb_process(&mP[c].reverb, len);
      }

      if (mNumChans == 2)
      {
         for (decltype(len) i = 0; i < len; i++)
         {
            for (int w = 0; w < 2; w++)
            {
               ochans[w][i] = dryMult *
                              mP[w].dry[i] +
                              0.5 *
                              (mP[0].wet[w][i] + mP[1].wet[w][i]);
            }
         }
      }
      else
      {
         for (decltype(len) i = 0; i < len; i++)
         {
            ochans[0][i] = dryMult * 
                           mP[0].dry[i] +
                           mP[0].wet[0][i];
         }
      }

      remaining -= len;

      for (unsigned int c = 0; c < mNumChans; c++)
      {
         ichans[c] += len;
         ochans[c] += len;
      }
   }

   return blockLen;
}

RegistryPaths EffectReverb::GetFactoryPresets() const
{
   RegistryPaths names;

   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      names.push_back( FactoryPresets[i].name.Translation() );
   }

   return names;
}

bool EffectReverb::LoadFactoryPreset(int id, EffectSettings &) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<EffectReverb*>(this)->DoLoadFactoryPreset(id);
}

bool EffectReverb::DoLoadFactoryPreset(int id)
{
   if (id < 0 || id >= (int) WXSIZEOF(FactoryPresets))
   {
      return false;
   }

   mParams = FactoryPresets[id].params;
   return true;
}

// Effect implementation

std::unique_ptr<EffectUIValidator>
EffectReverb::PopulateOrExchange(ShuttleGui & S, EffectSettingsAccess &)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

#define SpinSlider(n, p) \
      m ## n ## T = S.Id(ID_ ## n). \
         AddSpinCtrl( p, n.def, n.max, n.min); \
      S; \
      m ## n ## S = S.Id(ID_ ## n) \
         .Style(wxSL_HORIZONTAL) \
         .AddSlider( {}, n.def, n.max, n.min);

      SpinSlider(RoomSize,       XXO("&Room Size (%):"))
      SpinSlider(PreDelay,       XXO("&Pre-delay (ms):"))
      SpinSlider(Reverberance,   XXO("Rever&berance (%):"))
      SpinSlider(HfDamping,      XXO("Da&mping (%):"))
      SpinSlider(ToneLow,        XXO("Tone &Low (%):"))
      SpinSlider(ToneHigh,       XXO("Tone &High (%):"))
      SpinSlider(WetGain,        XXO("Wet &Gain (dB):"))
      SpinSlider(DryGain,        XXO("Dr&y Gain (dB):"))
      SpinSlider(StereoWidth,    XXO("Stereo Wid&th (%):"))

#undef SpinSlider

   }
   S.EndMultiColumn();

   S.StartHorizontalLay(wxCENTER, false);
   {
      mWetOnlyC =
      S
         .Id(ID_WetOnly).
            AddCheckBox(XXO("Wet O&nly"), WetOnly.def);
   }
   S.EndHorizontalLay();

   return nullptr;
}

bool EffectReverb::TransferDataToWindow(const EffectSettings &)
{
#define SetSpinSlider(n) \
   m ## n ## S->SetValue((int) mParams.m ## n); \
   m ## n ## T->SetValue(wxString::Format(wxT("%d"), (int) mParams.m ## n));

   SetSpinSlider(RoomSize);
   SetSpinSlider(PreDelay);
   SetSpinSlider(Reverberance);
   SetSpinSlider(HfDamping);
   SetSpinSlider(ToneLow);
   SetSpinSlider(ToneHigh);
   SetSpinSlider(WetGain);
   SetSpinSlider(DryGain);
   SetSpinSlider(StereoWidth);

#undef SetSpinSlider

   mWetOnlyC->SetValue((int) mParams.mWetOnly);

   return true;
}

bool EffectReverb::TransferDataFromWindow(EffectSettings &)
{
   mParams.mRoomSize = mRoomSizeS->GetValue();
   mParams.mPreDelay = mPreDelayS->GetValue();
   mParams.mReverberance = mReverberanceS->GetValue();
   mParams.mHfDamping = mHfDampingS->GetValue();
   mParams.mToneLow = mToneLowS->GetValue();
   mParams.mToneHigh = mToneHighS->GetValue();
   mParams.mWetGain = mWetGainS->GetValue();
   mParams.mDryGain = mDryGainS->GetValue();
   mParams.mStereoWidth = mStereoWidthS->GetValue();
   mParams.mWetOnly = mWetOnlyC->GetValue();

   return true;
}

#define SpinSliderHandlers(n) \
   void EffectReverb::On ## n ## Slider(wxCommandEvent & evt) \
   { \
      if (mProcessingEvent) return; \
      mProcessingEvent = true; \
      m ## n ## T->SetValue(wxString::Format(wxT("%d"), evt.GetInt())); \
      mProcessingEvent = false; \
   } \
   void EffectReverb::On ## n ## Text(wxCommandEvent & evt) \
   { \
      if (mProcessingEvent) return; \
      mProcessingEvent = true; \
      m ## n ## S->SetValue(std::clamp<long>(evt.GetInt(), n.min, n.max)); \
      mProcessingEvent = false; \
   }

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
