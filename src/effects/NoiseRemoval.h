/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REMOVAL__
#define __AUDACITY_EFFECT_NOISE_REMOVAL__



#if !defined(EXPERIMENTAL_NOISE_REDUCTION)

#include "StatefulEffect.h"
#include "EffectUI.h"

class wxButton;
class wxSizer;
class wxSlider;

class Envelope;
class EffectSettingsAccess;
class WaveTrack;

class wxRadioButton;
class wxTextCtrl;

#include "RealFFTf.h"
#include "SampleFormat.h"

class EffectNoiseRemoval final : public StatefulEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectNoiseRemoval();
   virtual ~EffectNoiseRemoval();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool SupportsAutomation() const override;

   // Effect implementation

   int ShowHostInterface(const std::shared_ptr<EffectContext> &pContext,
      EffectPlugin &plugin, wxWindow &parent, const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) override;
   bool Init() override;
   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   void End() override;

private:

   bool      mDoProfile;
   bool      mHasProfile;
   int       mLevel;

   // Parameters chosen before the first phase
   double    mSampleRate;
   size_t    mWindowSize;
   size_t    mSpectrumSize;
   float     mMinSignalTime;    // in secs

   // The frequency-indexed noise threshold derived during the first
   // phase of analysis
   Floats mNoiseThreshold;  // length is mSpectrumSize

   // Parameters that affect the noise removal, regardless of how the
   // noise profile was extracted
   double     mSensitivity;
   double     mFreqSmoothingHz;
   double     mNoiseGain;              // in dB, should be negative
   double     mAttackDecayTime;        // in secs
   bool       mbLeaveNoise;

   bool ProcessOne(int count, WaveTrack * track,
                   sampleCount start, sampleCount len);

   void Initialize();
   void StartNewTrack();
   void ProcessSamples(size_t len, float *buffer);
   void FillFirstHistoryWindow();
   void ApplyFreqSmoothing(float *spec);
   void GetProfile();
   void RemoveNoise();
   void RotateHistoryWindows();
   void FinishTrack();

   // Variables that only exist during processing
   std::shared_ptr<WaveTrack> mOutputTrack;
   sampleCount       mInSampleCount;
   sampleCount       mOutSampleCount;
   int                   mInputPos;

   HFFT     hFFT;
   Floats mFFTBuffer;         // mWindowSize
   Floats mWindow;            // mWindowSize

   int       mFreqSmoothingBins;
   int       mAttackDecayBlocks;
   float     mOneBlockAttackDecay;
   float     mNoiseAttenFactor;
   float     mSensitivityFactor;
   size_t    mMinSignalBlocks;
   size_t    mHistoryLen;
   Floats mInWaveBuffer;     // mWindowSize
   Floats mOutOverlapBuffer; // mWindowSize
   ArraysOf<float> mSpectrums;        // mHistoryLen x mSpectrumSize
   ArraysOf<float> mGains;            // mHistoryLen x mSpectrumSize
   ArraysOf<float> mRealFFTs;         // mHistoryLen x mWindowSize
   ArraysOf<float> mImagFFTs;         // mHistoryLen x mWindowSize

   friend class NoiseRemovalDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

class NoiseRemovalDialog final : public EffectDialog
{
public:
   // constructors and destructors
   NoiseRemovalDialog(EffectNoiseRemoval * effect, EffectSettingsAccess &access,
                      wxWindow *parent);

   wxSizer *MakeNoiseRemovalDialog(bool call_fit = true,
                                   bool set_sizer = true);

   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // handlers
   void OnGetProfile( wxCommandEvent &event );
   void OnKeepNoise( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event) override;
   void OnRemoveNoise( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );

   void OnSensitivityText(wxCommandEvent & event);
   void OnGainText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnTimeText(wxCommandEvent & event);
   void OnSensitivitySlider(wxCommandEvent & event);
   void OnGainSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnTimeSlider(wxCommandEvent & event);

 public:

   EffectNoiseRemoval * m_pEffect;
   EffectSettingsAccess &mAccess;

   wxButton * m_pButton_GetProfile;
   wxButton * m_pButton_Preview;
   wxButton * m_pButton_RemoveNoise;

   wxRadioButton *mKeepSignal;
   wxRadioButton *mKeepNoise;

   wxSlider   *mSensitivityS;
   wxSlider   *mGainS;
   wxSlider   *mFreqS;
   wxSlider   *mTimeS;

   wxTextCtrl *mSensitivityT;
   wxTextCtrl *mGainT;
   wxTextCtrl *mFreqT;
   wxTextCtrl *mTimeT;

   double      mSensitivity;
   double      mGain;
   double      mFreq;
   double      mTime;

   bool        mbLeaveNoise;

private:
   DECLARE_EVENT_TABLE()

};

#endif

#endif
