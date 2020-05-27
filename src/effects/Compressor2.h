/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor2.h

  Max Maisel (based on Compressor effect)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR2__
#define __AUDACITY_EFFECT_COMPRESSOR2__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

class Plot;
class ShuttleGui;
class SliderTextCtrl;

class SamplePreprocessor
{
   public:
      virtual float ProcessSample(float value) = 0;
      virtual float ProcessSample(float valueL, float valueR) = 0;
      virtual void Reset() = 0;
      virtual void SetWindowSize(size_t windowSize) = 0;
};

class SlidingRmsPreprocessor : public SamplePreprocessor
{
   public:
      SlidingRmsPreprocessor(size_t windowSize, float gain = 2.0);

      virtual float ProcessSample(float value);
      virtual float ProcessSample(float valueL, float valueR);
      virtual void Reset();
      virtual void SetWindowSize(size_t windowSize);

      static const size_t REFRESH_WINDOW_EVERY = 1048576; // 1 MB

   private:
      float mSum;
      float mGain;
      std::vector<float> mWindow;
      size_t mPos;
      size_t mInsertCount;

      inline float DoProcessSample(float value);
      void Refresh();
};

class SlidingMaxPreprocessor : public SamplePreprocessor
{
   public:
      SlidingMaxPreprocessor(size_t windowSize);

      virtual float ProcessSample(float value);
      virtual float ProcessSample(float valueL, float valueR);
      virtual void Reset();
      virtual void SetWindowSize(size_t windowSize);

   private:
      std::vector<float> mWindow;
      std::vector<float> mMaxes;
      size_t mPos;

      inline float DoProcessSample(float value);
};

class EnvelopeDetector
{
   public:
      EnvelopeDetector(size_t buffer_size);

      float ProcessSample(float value);
      size_t GetBlockSize() const;
      const float* GetBuffer(int idx) const;

      virtual void CalcInitialCondition(float value);
      inline float InitialCondition() const { return mInitialCondition; }
      inline size_t InitialConditionSize() const { return mInitialBlockSize; }

      virtual void SetParams(float sampleRate, float attackTime,
         float releaseTime) = 0;

      virtual float AttackFactor();
      virtual float DecayFactor();

   protected:
      size_t mPos;
      float mInitialCondition;
      size_t mInitialBlockSize;
      std::vector<float> mLookaheadBuffer;
      std::vector<float> mProcessingBuffer;
      std::vector<float> mProcessedBuffer;

      virtual void Follow() = 0;
};

class ExpFitEnvelopeDetector : public EnvelopeDetector
{
   public:
      ExpFitEnvelopeDetector(float rate, float attackTime, float releaseTime,
         size_t buffer_size);

      virtual void SetParams(float sampleRate, float attackTime,
         float releaseTime);

   private:
      double mAttackFactor;
      double mReleaseFactor;

      virtual void Follow();
};

class Pt1EnvelopeDetector : public EnvelopeDetector
{
   public:
      Pt1EnvelopeDetector(float rate, float attackTime, float releaseTime,
         size_t buffer_size, bool correctGain = true);
      virtual void CalcInitialCondition(float value);

      virtual void SetParams(float sampleRate, float attackTime,
         float releaseTime);
      virtual float AttackFactor();
      virtual float DecayFactor();

   private:
      bool mCorrectGain;
      double mGainCorrection;
      double mAttackFactor;
      double mReleaseFactor;

      virtual void Follow();
};

struct PipelineBuffer
{
   public:
      sampleCount trackPos;
      size_t trackSize;
      size_t size;

      inline float* operator[](size_t idx)
         { return mBlockBuffer[idx].get(); }

      void pad_to(size_t len, float value, bool stereo);
      void swap(PipelineBuffer& other);
      void init(size_t size, bool stereo);
      void fill(float value, bool stereo);
      inline size_t capacity() const { return mCapacity; }
      void free();

   private:
      size_t mCapacity;
      Floats mBlockBuffer[2];
};

class EffectCompressor2 final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectCompressor2();
   virtual ~EffectCompressor2();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool SupportsRealtime() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   bool RealtimeInitialize() override;
   bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize() override;
   size_t RealtimeProcess(int group, float **inbuf, float **outbuf,
      size_t numSamples) override;
   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   bool Startup() override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectCompressor2 implementation
   void InitGainCalculation();
   double CompressorGain(double env);
   std::unique_ptr<SamplePreprocessor> InitPreprocessor(
      double rate, bool preview = false);
   std::unique_ptr<EnvelopeDetector> InitEnvelope(
      double rate, size_t blockSize = 0, bool preview = false);
   size_t CalcBufferSize(size_t sampleRate);

   void AllocPipeline();
   void AllocRealtimePipeline();
   void FreePipeline();
   void SwapPipeline();
   bool ProcessOne(TrackIterRange<WaveTrack> range);
   bool LoadPipeline(TrackIterRange<WaveTrack> range, size_t len);
   void FillPipeline();
   void ProcessPipeline();
   inline float PreprocSample(PipelineBuffer& pbuf, size_t rp);
   inline float EnvelopeSample(PipelineBuffer& pbuf, size_t rp);
   inline void CompressSample(float env, size_t wp);
   bool PipelineHasData();
   void DrainPipeline();
   void StorePipeline(TrackIterRange<WaveTrack> range);

   bool UpdateProgress();
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();
   void UpdateCompressorPlot();
   void UpdateResponsePlot();
   void UpdateRealtimeParams();

   static const int TAU_FACTOR = 5;
   static const size_t MIN_BUFFER_CAPACITY = 1048576; // 1MB

   static const size_t PIPELINE_DEPTH = 4;
   PipelineBuffer mPipeline[PIPELINE_DEPTH];

   double mCurT0;
   double mCurT1;
   double mProgressVal;
   double mTrackLen;
   bool mProcStereo;

   std::mutex mRealtimeMutex;
   std::unique_ptr<SamplePreprocessor> mPreproc;
   std::unique_ptr<EnvelopeDetector> mEnvelope;

   int    mAlgorithm;
   int    mCompressBy;
   bool   mStereoInd;

   double    mThresholdDB;
   double    mRatio;
   double    mKneeWidthDB;
   double    mAttackTime;
   double    mReleaseTime;
   double    mLookaheadTime;
   double    mLookbehindTime;
   double    mMakeupGainPct;
   double    mDryWetPct;

   // cached intermediate values
   double mDryWet;
   double mMakeupGain;
   double mMakeupGainDB;
   size_t mLookaheadLength;

   static const size_t RESPONSE_PLOT_SAMPLES = 200;
   static const size_t RESPONSE_PLOT_TIME = 5;
   static const size_t RESPONSE_PLOT_STEP_START = 2;
   static const size_t RESPONSE_PLOT_STEP_STOP = 3;

   bool mIgnoreGuiEvents;
   Plot* mGainPlot;
   Plot* mResponsePlot;
   wxChoice* mAlgorithmCtrl;
   wxChoice* mPreprocCtrl;
   SliderTextCtrl* mAttackTimeCtrl;
   SliderTextCtrl* mLookaheadTimeCtrl;

   DECLARE_EVENT_TABLE()
};

#endif
