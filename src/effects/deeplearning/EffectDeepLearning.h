/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   EffectDeepLearning.h
   Hugo Flores Garcia

******************************************************************/
/**

\class EffectDeepLearning
\brief EffectDeepLearning provides methods for using deep learning
                            models in Audacity Effects. 

TODO: add more desc

*/
/*******************************************************************/

#ifndef __AUDACITY_EFFECT_SOURCESEP__
#define __AUDACITY_EFFECT_SOURCESEP__

#include "DeepModel.h"
#include "../Effect.h"

// BlockIndex.first corresponds to the starting sample of a block
// BlockIndex.second corresponds to the length of the block
using BlockIndex = std::pair<sampleCount, size_t>;

class EffectDeepLearning /* not final */ : public Effect
{
public:
   EffectDeepLearning();

   // Effect implementation
   bool Process() override;

   // TODO: write desc and instructions
   virtual bool ProcessOne(WaveTrack * track, double tStart, double tEnd) = 0;

protected:
   // the deep model itself
   std::unique_ptr<DeepModel> mModel;

   // gets the number of channels in a (possibly multichannel) track
   size_t GetNumChannels(WaveTrack *leader){return TrackList::Channels(leader).size();}

   // builds a mono tensor with shape (1, samples)
   // from a track
   torch::Tensor BuildMonoTensor(WaveTrack *track, float *buffer, 
                                 sampleCount start, size_t len);

   // builds a multichannel tensor with shape (channels, samples)
   // given a leader track.
   torch::Tensor BuildMultichannelTensor(WaveTrack *leader, float *buffer, 
                                         sampleCount start, size_t len);

   // wraps the forward pass in an exception
   torch::Tensor ForwardPass(torch::Tensor input); 

   // writes an output tensor to a track
   // tensor should be shape (1, samples)
   void TensorToTrack(torch::Tensor output, WaveTrack::Holder track, 
                      double tStart, double tEnd);

   // returns a list of block indices. Use these to 
   // to process the audio in blocks
   std::vector<BlockIndex> GetBlockIndices(WaveTrack *track, 
                                           double tStart, double tEnd);

   // use this to update the progress ba
   int mCurrentTrackNum;
};

#endif