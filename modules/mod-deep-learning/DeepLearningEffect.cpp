/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepLearningEffect.cpp
   Hugo Flores Garcia

******************************************************************/
/**

*/
/*******************************************************************/

#include "DeepLearningEffect.h"

#include <wx/stattext.h>

#include "FileNames.h"
#include "Shuttle.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"

#include "effects/LoadEffects.h"

#include <torch/script.h>

// DeepLearningEffectBase implementation

std::string DeepLearningEffect::GetDeepEffectID()
{ return "waveform-to-waveform";}

const ComponentInterfaceSymbol DeepLearningEffect::Symbol
{ XO("Deep Learning Effect") };

// register source separation
namespace{ BuiltinEffectsModule::Registration< DeepLearningEffect > reg; }

DeepLearningEffect::DeepLearningEffect()
{
   SetLinearEffectFlag(false);
}

DeepLearningEffect::~DeepLearningEffect()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol DeepLearningEffect::GetSymbol() const
{
   return Symbol;
}

TranslatableString DeepLearningEffect::GetDescription() const
{
   return XO("A generic audio-to-audio processor for hosting deep learning models.");
}

ManualPageID DeepLearningEffect::ManualPage() const
{
   return L"Deep_Learning_Effect"; 
}

// EffectDefinitionInterface implementation

EffectType DeepLearningEffect::GetType() const
{
   return EffectTypeProcess;
}

// Effect implementation

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// performs a forward pass through the deep model, and writes 
// the output to new tracks. 
bool DeepLearningEffect::ProcessOne(WaveTrack *leader,
                           double tStart, double tEnd)
{
   if (leader == NULL)
      return false;

   // keep track of the sample format and rate,
   // we want to convert all output tracks to this
   sampleFormat origFmt = leader->GetSampleFormat();
   int origRate = leader->GetRate();

   // initialize source tracks, one for each source that we will separate
   // even though the input track may have any arbitrary number of channels, 
   // each output source track will be mono for now. TODO: support stereo output sources. 
   std::vector<WaveTrack::Holder> sourceTracks;
   std::vector<std::string> sourceLabels = mActiveModel->GetModel()->GetCard()->labels();
   sourceTracks = CreateSourceTracks(leader, sourceLabels);
   
   // Initiate processing buffer, most likely shorter than
   // the length of the selection being processed.
   Floats buffer{ leader->GetMaxBlockSize() };

   // get each of the blocks we will process
   const auto &pModel = mActiveModel->GetModel();
   std::vector<BlockIndex> indices = GetBlockIndices(leader, tStart, tEnd);
   for (BlockIndex block : indices)
   {
      //Get a blockSize of samples (smaller than the size of the buffer)
      sampleCount samplePos = block.first;
      size_t blockSize = block.second;
      double tPos = leader->LongSamplesToTime(samplePos); 
   
      // get a torch tensor from the leader track
      torch::Tensor input = BuildMultichannelTensor(leader, buffer.get(), 
                                            samplePos, blockSize);

      // if we're not doing a multichannel forward pass, downmix
      if (!pModel->GetCard()->multichannel())
         input = input.sum(0, true, torch::kFloat);

      // resample!
      input = pModel->Resample(input, origRate, pModel->GetCard()->sample_rate());

      // forward pass!
      torch::Tensor output = pModel->ToTensor(ForwardPassInThread(input));

      // resample back
      output = pModel->Resample(output, pModel->GetCard()->sample_rate(), origRate);

      // write each source output to the source tracks
      for (size_t idx = 0; idx < std::min(sourceTracks.size(), (size_t)output.size(0)) ; idx++)
         TensorToTrack(output[idx].unsqueeze(0), sourceTracks[idx], 
                       tPos, tEnd); 

      // Update the Progress meter
      mCurrentProgress = (tPos - tStart) / (tEnd - tStart);
      if (TrackProgress(mCurrentTrackNum, mCurrentProgress)) 
         return false;
   }

   // postprocess the source tracks to the user's sample rate and format
   PostProcessSources(leader, sourceTracks, origFmt, origRate);

   return true;
}

std::vector<WaveTrack::Holder> DeepLearningEffect::CreateSourceTracks
(WaveTrack *leader, std::vector<std::string> &labels)
{
   std::vector<WaveTrack::Holder> sources;
   for (auto &label : labels)
   {
      WaveTrack::Holder srcTrack = leader->EmptyCopy();

      // append the source name to the track's name
      srcTrack->SetName(srcTrack->GetName() + wxString("-" + label));
      sources.emplace_back(srcTrack);
   }
   return sources;
}

void DeepLearningEffect::PostProcessSources
(WaveTrack *leader, std::vector<WaveTrack::Holder> &sourceTracks, sampleFormat fmt, int sampleRate)
{
   // flush all output track buffers
   // convert to the original rate and format
   for (std::shared_ptr<WaveTrack> track : sourceTracks)
   {
      track->Flush();
      track->ConvertToSampleFormat(fmt);
      track->Resample(sampleRate);
      AddToOutputTracks(track);

      // if the parent track used to be stereo,
      // make the source mono anyway
      mOutputTracks->UnlinkChannels(*track);

      // join the clips!
      for (const auto &timestamp : GetClipTimestamps(leader, 
                                                    track->GetStartTime(),
                                                    track->GetEndTime()))
         track->Join(timestamp.first, timestamp.second);
   }
}

// Define necessary symbols for a plug-in
#include "ModuleConstants.h"
DEFINE_MODULE_ENTRIES
