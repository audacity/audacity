/**********************************************************************

  Audacity: A Digital Audio Editor

  Distortion.h

  Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DISTORTION__
#define __AUDACITY_EFFECT_DISTORTION__

#include <queue>

#include "Effect.h"

class wxSlider;
class wxStaticText;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

#define STEPS 1024      // number of +ve or -ve steps in lookup tabe
#define TABLESIZE 2049  // size of lookup table (steps * 2 + 1)

class EffectDistortionState
{
public:
   float       samplerate;
   sampleCount skipcount;
   int         tablechoiceindx;
   bool        dcblock;
   double      threshold;
   double      noisefloor;
   double      param1;
   double      param2;
   int         repeats;

   // DC block filter variables
   std::queue<float> queuesamples;
   double queuetotal;
};

class EffectDistortion final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectDistortion();
   virtual ~EffectDistortion();

   struct Params
   {
      int    mTableChoiceIndx;
      bool   mDCBlock;
      double mThreshold_dB;
      double mNoiseFloor;
      double mParam1;
      double mParam2;
      int    mRepeats;
   };

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool SupportsRealtime() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;
   RegistryPaths GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;

   // EffectProcessor implementation

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
   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:

   enum control
   {
      ID_Type = 10000,
      ID_DCBlock,
      ID_Threshold,
      ID_NoiseFloor,
      ID_Param1,
      ID_Param2,
      ID_Repeats,
   };
   // EffectDistortion implementation

   void InstanceInit(EffectDistortionState & data, float sampleRate);
   size_t InstanceProcess(EffectDistortionState & data,
                               float **inBlock,
                               float **outBlock,
                               size_t blockLen);

   // Control Handlers

   void OnTypeChoice(wxCommandEvent & evt);
   void OnDCBlockCheckbox(wxCommandEvent & evt);
   void OnThresholdText(wxCommandEvent & evt);
   void OnThresholdSlider(wxCommandEvent & evt);
   void OnNoiseFloorText(wxCommandEvent & evt);
   void OnNoiseFloorSlider(wxCommandEvent & evt);
   void OnParam1Text(wxCommandEvent & evt);
   void OnParam1Slider(wxCommandEvent & evt);
   void OnParam2Text(wxCommandEvent & evt);
   void OnParam2Slider(wxCommandEvent & evt);
   void OnRepeatsText(wxCommandEvent & evt);
   void OnRepeatsSlider(wxCommandEvent & evt);
   void UpdateUI();
   void UpdateControl(control id, bool enable, TranslatableString name);
   void UpdateControlText(wxTextCtrl *textCtrl, wxString &string, bool enabled);

   void MakeTable();
   float WaveShaper(float sample);
   float DCFilter(EffectDistortionState & data, float sample);

   // Preset tables for gain lookup

   void HardClip();           // hard clipping
   void SoftClip();           // soft clipping
   void ExponentialTable();   // exponential mapping
   void LogarithmicTable();   // logarithmic mapping
   void HalfSinTable();
   void CubicTable();
   void EvenHarmonicTable();
   void SineTable();
   void Leveller();           // 'Leveller' wavetable is modeled on the legacy effect of the same name.
   void Rectifier();          // 0% = Dry, 50% = half-wave rectified, 100% = full-wave rectified (abs value).
   void HardLimiter();        // Same effect as the LADSPA "hardLimiter 1413"

   // Wavetable helper functions

   void CopyHalfTable();   // for symmetric tables

   // Used by Soft Clipping but could be used for other tables.
   // Log curve formula: y = T + (((e^(RT - Rx)) - 1) / -R)
   // where R is the ratio, T is the threshold, and x is from T to 1. 
   inline float LogCurve(double threshold, float value, double ratio);

   // Used by Cubic curve but could be used for other tables
   // Cubic formula: y = x - (x^3 / 3.0)
   inline double Cubic(double x);


private:
   EffectDistortionState mMaster;
   std::vector<EffectDistortionState> mSlaves;

   double mTable[TABLESIZE];
   double mThreshold;
   bool mbSavedFilterState;

   // mMakeupGain is used by some distortion types to pass the
   // amount of gain required to bring overall effect gain to unity
   double mMakeupGain;

   int mTypChoiceIndex;

   wxChoice *mTypeChoiceCtrl;
   wxTextCtrl *mThresholdT;
   wxTextCtrl *mNoiseFloorT;
   wxTextCtrl *mParam1T;
   wxTextCtrl *mParam2T;
   wxTextCtrl *mRepeatsT;

   wxSlider *mThresholdS;
   wxSlider *mNoiseFloorS;
   wxSlider *mParam1S;
   wxSlider *mParam2S;
   wxSlider *mRepeatsS;

   wxCheckBox *mDCBlockCheckBox;

   wxStaticText *mThresholdTxt;
   wxStaticText *mNoiseFloorTxt;
   wxStaticText *mParam1Txt;
   wxStaticText *mParam2Txt;
   wxStaticText *mRepeatsTxt;
   
   wxString mOldThresholdTxt;
   wxString mOldmNoiseFloorTxt;
   wxString mOldParam1Txt;
   wxString mOldParam2Txt;
   wxString mOldRepeatsTxt;

   Params mParams;

   DECLARE_EVENT_TABLE()
};

#endif
