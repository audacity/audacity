/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepLearningEffectBase.h
   Hugo Flores Garcia

******************************************************************/
/**

\file DeepLearningEffectBase.h
\brief provides an abstract effect for using deep learning models in Audacity Effects. 

*/
/*******************************************************************/

#pragma once

#include "ActiveModel.h"
#include "ModelManagerPanel.h"
#include "effects/Effect.h"
#include "WaveTrack.h"

// BlockIndex.first corresponds to the starting sample of a block
// BlockIndex.second corresponds to the length of the block
using BlockIndex = std::pair<sampleCount, size_t>;
using ClipTimestamps = std::pair<double, double>;

class DeepLearningEffectBase /* not final */ : public StatefulEffect
{
public:
   DeepLearningEffectBase();

   // Effect implementation

   bool Init() override;

   // To be removed once we figure out a better
   // way to do the 'cleanup'
   void End() override;

   bool Process(EffectInstance &instance, EffectSettings &settings) override;

   // void PopulateOrExchange(ShuttleGui & S) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;

   // DeepLearningEffect implementation

   // TODO: write desc and instructions
   virtual bool ProcessOne(WaveTrack * track, double tStart, double tEnd) = 0;

   // TODO: write instructions
   virtual std::string GetDeepEffectID() = 0;
   
protected:

   //! gets the number of channels in a (possibly multichannel) track
   size_t GetNumChannels(WaveTrack *leader);

   //! builds a mono tensor with shape (1, samples) from a track
   torch::Tensor BuildMonoTensor(WaveTrack *track, float *buffer, 
                                 sampleCount start, size_t len);

   //! builds a multichannel tensor with shape (channels, samples) given a leader track.
   torch::Tensor BuildMultichannelTensor(WaveTrack *leader, float *buffer, 
                                         sampleCount start, size_t len);

   //! performs a forward pass on a helper thread, and sends updates to a progress dialog to keep the main thread alive. 
   torch::jit::IValue ForwardPassInThread(torch::Tensor input);

   //! writes an output tensor to a track tensor should be shape (1, samples)
   void TensorToTrack(torch::Tensor waveform, WaveTrack::Holder track,
                      double tStart, double tEnd);

   //! returns a list of block indices. Use these to process the audio in blocks
   std::vector<BlockIndex> GetBlockIndices(WaveTrack *track, 
                                           double tStart, double tEnd);

   //! returns a list of start and end times for all clips in the track
   std::vector<ClipTimestamps> GetClipTimestamps(WaveTrack *track, 
                                                 double tStart, double tEnd) const;

   //! use this to update the progress bar. 
   int mCurrentTrackNum {0};
   
   //! populate this with the current progress in ProcessOne
   double mCurrentProgress {0.0};

   std::shared_ptr<ActiveModel> mActiveModel;

private:
   ModelManagerPanel *mManagerPanel {nullptr};
};
