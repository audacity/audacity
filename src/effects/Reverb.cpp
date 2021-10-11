/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

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
#include "../Shuttle.h"
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

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name          Type     Key                  Def      Min      Max   Scale
Param( RoomSize,     double,  wxT("RoomSize"),      75,      0,       100,  1  );
Param( PreDelay,     double,  wxT("Delay"),         10,      0,       200,  1  );
Param( Reverberance, double,  wxT("Reverberance"),  50,      0,       100,  1  );
Param( HfDamping,    double,  wxT("HfDamping"),     50,      0,       100,  1  );
Param( ToneLow,      double,  wxT("ToneLow"),       100,     0,       100,  1  );
Param( ToneHigh,     double,  wxT("ToneHigh"),      100,     0,       100,  1  );
Param( WetGain,      double,  wxT("WetGain"),       -1,      -20,     10,   1  );
Param( DryGain,      double,  wxT("DryGain"),       -1,      -20,     10,   1  );
Param( StereoWidth,  double,  wxT("StereoWidth"),   100,     0,       100,  1  );
Param( WetOnly,      bool,    wxT("WetOnly"),       false,   false,   true, 1  );

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
   mParams.mRoomSize = DEF_RoomSize;
   mParams.mPreDelay = DEF_PreDelay;
   mParams.mReverberance = DEF_Reverberance;
   mParams.mHfDamping = DEF_HfDamping;
   mParams.mToneLow = DEF_ToneLow;
   mParams.mToneHigh = DEF_ToneHigh;
   mParams.mWetGain = DEF_WetGain;
   mParams.mDryGain = DEF_DryGain;
   mParams.mStereoWidth = DEF_StereoWidth;
   mParams.mWetOnly = DEF_WetOnly;

   mProcessingEvent = false;

   SetLinearEffectFlag(true);
}

EffectReverb::~EffectReverb()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectReverb::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectReverb::GetDescription()
{
   return XO("Adds ambience or a \"hall effect\"");
}

ManualPageID EffectReverb::ManualPage()
{
   return L"Reverb";
}

// EffectDefinitionInterface implementation

EffectType EffectReverb::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

unsigned EffectReverb::GetAudioInCount()
{
   return mParams.mStereoWidth ? 2 : 1;
}

unsigned EffectReverb::GetAudioOutCount()
{
   return mParams.mStereoWidth ? 2 : 1;
}

static size_t BLOCK = 16384;

bool EffectReverb::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames chanMap)
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

size_t EffectReverb::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   float *ichans[2] = {NULL, NULL};
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
bool EffectReverb::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mParams.mRoomSize,       RoomSize );
   S.SHUTTLE_PARAM( mParams.mPreDelay,       PreDelay );
   S.SHUTTLE_PARAM( mParams.mReverberance,   Reverberance );
   S.SHUTTLE_PARAM( mParams.mHfDamping,      HfDamping );
   S.SHUTTLE_PARAM( mParams.mToneLow,        ToneLow );
   S.SHUTTLE_PARAM( mParams.mToneHigh,       ToneHigh );
   S.SHUTTLE_PARAM( mParams.mWetGain,        WetGain );
   S.SHUTTLE_PARAM( mParams.mDryGain,        DryGain );
   S.SHUTTLE_PARAM( mParams.mStereoWidth,    StereoWidth );
   S.SHUTTLE_PARAM( mParams.mWetOnly,        WetOnly );
   return true;
}

bool EffectReverb::GetAutomationParameters(CommandParameters & parms)
{
   parms.Write(KEY_RoomSize, mParams.mRoomSize);
   parms.Write(KEY_PreDelay, mParams.mPreDelay);
   parms.Write(KEY_Reverberance, mParams.mReverberance);
   parms.Write(KEY_HfDamping, mParams.mHfDamping);
   parms.Write(KEY_ToneLow, mParams.mToneLow);
   parms.Write(KEY_ToneHigh, mParams.mToneHigh);
   parms.Write(KEY_WetGain, mParams.mWetGain);
   parms.Write(KEY_DryGain, mParams.mDryGain);
   parms.Write(KEY_StereoWidth, mParams.mStereoWidth);
   parms.Write(KEY_WetOnly, mParams.mWetOnly);

   return true;
}

bool EffectReverb::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyDouble(RoomSize);
   ReadAndVerifyDouble(PreDelay);
   ReadAndVerifyDouble(Reverberance);
   ReadAndVerifyDouble(HfDamping);
   ReadAndVerifyDouble(ToneLow);
   ReadAndVerifyDouble(ToneHigh);
   ReadAndVerifyDouble(WetGain);
   ReadAndVerifyDouble(DryGain);
   ReadAndVerifyDouble(StereoWidth);
   ReadAndVerifyBool(WetOnly);

   mParams.mRoomSize = RoomSize;
   mParams.mPreDelay = PreDelay;
   mParams.mReverberance = Reverberance;
   mParams.mHfDamping = HfDamping;
   mParams.mToneLow = ToneLow;
   mParams.mToneHigh = ToneHigh;
   mParams.mWetGain = WetGain;
   mParams.mDryGain = DryGain;
   mParams.mStereoWidth = StereoWidth;
   mParams.mWetOnly = WetOnly;

   return true;
}

RegistryPaths EffectReverb::GetFactoryPresets()
{
   RegistryPaths names;

   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      names.push_back( FactoryPresets[i].name.Translation() );
   }

   return names;
}

bool EffectReverb::LoadFactoryPreset(int id)
{
   if (id < 0 || id >= (int) WXSIZEOF(FactoryPresets))
   {
      return false;
   }

   mParams = FactoryPresets[id].params;

   if (mUIDialog)
   {
      TransferDataToWindow();
   }

   return true;
}

// Effect implementation

bool EffectReverb::Startup()
{
   wxString base = wxT("/Effects/Reverb/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      gPrefs->Read(base + wxT("RoomSize"), &mParams.mRoomSize, DEF_RoomSize);
      gPrefs->Read(base + wxT("Delay"), &mParams.mPreDelay, DEF_PreDelay);
      gPrefs->Read(base + wxT("Reverberance"), &mParams.mReverberance, DEF_Reverberance);
      gPrefs->Read(base + wxT("HfDamping"), &mParams.mHfDamping, DEF_HfDamping);
      gPrefs->Read(base + wxT("ToneLow"), &mParams.mToneLow, DEF_ToneLow);
      gPrefs->Read(base + wxT("ToneHigh"), &mParams.mToneHigh, DEF_ToneHigh);
      gPrefs->Read(base + wxT("WetGain"), &mParams.mWetGain, DEF_WetGain);
      gPrefs->Read(base + wxT("DryGain"), &mParams.mDryGain, DEF_DryGain);
      gPrefs->Read(base + wxT("StereoWidth"), &mParams.mStereoWidth, DEF_StereoWidth);
      gPrefs->Read(base + wxT("WetOnly"), &mParams.mWetOnly, DEF_WetOnly);

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
   }

   // Load the previous user presets
   for (int i = 0; i < 10; i++)
   {
      wxString path = base + wxString::Format(wxT("%d/"), i);
      if (gPrefs->Exists(path))
      {
         Params save = mParams;
         wxString name;

         gPrefs->Read(path + wxT("RoomSize"), &mParams.mRoomSize, DEF_RoomSize);
         gPrefs->Read(path + wxT("Delay"), &mParams.mPreDelay, DEF_PreDelay);
         gPrefs->Read(path + wxT("Reverberance"), &mParams.mReverberance, DEF_Reverberance);
         gPrefs->Read(path + wxT("HfDamping"), &mParams.mHfDamping, DEF_HfDamping);
         gPrefs->Read(path + wxT("ToneLow"), &mParams.mToneLow, DEF_ToneLow);
         gPrefs->Read(path + wxT("ToneHigh"), &mParams.mToneHigh, DEF_ToneHigh);
         gPrefs->Read(path + wxT("WetGain"), &mParams.mWetGain, DEF_WetGain);
         gPrefs->Read(path + wxT("DryGain"), &mParams.mDryGain, DEF_DryGain);
         gPrefs->Read(path + wxT("StereoWidth"), &mParams.mStereoWidth, DEF_StereoWidth);
         gPrefs->Read(path + wxT("WetOnly"), &mParams.mWetOnly, DEF_WetOnly);
         gPrefs->Read(path + wxT("name"), &name, wxEmptyString);
      
         if (!name.empty())
         {
            name.Prepend(wxT(" - "));
         }
         name.Prepend(wxString::Format(wxT("Settings%d"), i));

         SaveUserPreset(GetUserPresetsGroup(name));

         mParams = save;
      }
   }

   return true;
}

void EffectReverb::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(2);

#define SpinSlider(n, p) \
      m ## n ## T = S.Id(ID_ ## n). \
         AddSpinCtrl( p, DEF_ ## n, MAX_ ## n, MIN_ ## n); \
      S; \
      m ## n ## S = S.Id(ID_ ## n) \
         .Style(wxSL_HORIZONTAL) \
         .AddSlider( {}, DEF_ ## n, MAX_ ## n, MIN_ ## n);

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
      mWetOnlyC = S.Id(ID_WetOnly).
         AddCheckBox(XXO("Wet O&nly"), DEF_WetOnly);
   }
   S.EndHorizontalLay();

   return;
}

bool EffectReverb::TransferDataToWindow()
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

bool EffectReverb::TransferDataFromWindow()
{
   if (!mUIParent->Validate())
   {
      return false;
   }

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
      m ## n ## S->SetValue(TrapLong(evt.GetInt(), MIN_ ## n, MAX_ ## n)); \
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

void EffectReverb::SetTitle(const wxString & name)
{
   mUIDialog->SetTitle(
      name.empty()
         ? _("Reverb")
         : wxString::Format( _("Reverb: %s"), name )
   );
}
