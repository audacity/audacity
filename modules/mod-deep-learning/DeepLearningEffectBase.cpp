/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepLearningEffectBase.cpp
   Hugo Flores Garcia

******************************************************************/

#include "DeepLearningEffectBase.h"

#include "DeepModelManager.h"
#include "ModelManagerPanel.h"

#include "FileNames.h"
#include "Shuttle.h"
#include "ShuttleGui.h"
#include <wx/range.h>

#include <torch/script.h>

#include <wx/log.h>
#include <wx/stattext.h>

#include <WaveClip.h>

// ModelCardPanel

DeepLearningEffectBase::DeepLearningEffectBase()
{
   EnablePreview(false);
}

bool DeepLearningEffectBase::Init()
{
   // Catch any errors while setting up the DeepModelManager
   try 
   {
      DeepModelManager &manager = DeepModelManager::Get();
      
      // try loading the model (if available)
      mActiveModel = std::make_shared<ActiveModel>();
      return true;
   }
   catch (const InvalidModelCardDocument &e)
   {
      Effect::MessageBox(XO("Error initalizing the Model Manager %s.").Format(e.what()),
      wxICON_ERROR);
      return false;
   }
}

void DeepLearningEffectBase::End()
{
   GuardedCall<void>([&]{
      DeepModelManager &manager = DeepModelManager::Get();

      // clean up in-progress installs
      for (auto card : manager.GetCards(GetDeepEffectID()))
      {
         if (manager.IsInstalling(card))
            manager.CancelInstall(card);
      }

      // release model (may still be active in thread)
      mActiveModel->GetModel()->Offload();
   });
}

bool DeepLearningEffectBase::Process()
{
   DeepModelManager &manager = DeepModelManager::Get();

   // if no card is selected, bail
   if (!mActiveModel->GetModel()->GetCard()) 
   {
      Effect::MessageBox(
         XO("No model is selected. Please select a model before applying the effect."),
         wxICON_ERROR);
      return false;
   }

   // if it's not installed, bail
   if (!manager.IsInstalled(mActiveModel->GetModel()->GetCard()))
   {
      Effect::MessageBox(
          XO("Please install the selected model (%s) before applying the effect.")
            .Format(mActiveModel->GetModel()->GetCard()->GetRepoID()),
          wxICON_ERROR);
      return false;
   }

   // try reloading the model if it's not loaded yet. 
   if (!mActiveModel->GetModel()->IsLoaded())
      mActiveModel->SetModel(*this);

   // Iterate over each track.
   // All needed because this effect needs to introduce
   // silence in the sync-lock group tracks to keep sync
   CopyInputTracks(true); // Set up mOutputTracks.
   bool bGoodResult = true;

   mCurrentTrackNum = 0;

   // NOTE: because we will append the separated tracks to mOutputTracks in ProcessOne(),
   // we need to collect the track pointers before calling ProcessOne()
   std::vector<WaveTrack *> pOutLeaders;
   for (WaveTrack *track : mOutputTracks->SelectedLeaders<WaveTrack>())
      pOutLeaders.emplace_back(track);

   // now that we have all the tracks we want to process,
   // go ahead and process each!
   for (WaveTrack *pOutLeader : pOutLeaders)
   {

      //Get start and end times from track
      double tStart = pOutLeader->GetStartTime();
      double tEnd = pOutLeader->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      tStart = std::max(mT0, tStart);
      tEnd = std::min(mT1, tEnd);

      // Process only if the right marker is to the right of the left marker
      if (tEnd > tStart)
      {
         //ProcessOne() (overridden in subclasses) processes a single track
         if (!ProcessOne(pOutLeader, tStart, tEnd))
            bGoodResult = false;
      }
      // increment current track
      mCurrentTrackNum++;
   }

   ReplaceProcessedTracks(bGoodResult);

   return bGoodResult;
}

size_t DeepLearningEffectBase::GetNumChannels(WaveTrack *leader)
{
   return TrackList::Channels(leader).size();
}

// gets a list of starting samples and block lengths
// dictated by the track, so we can process the audio
// audio in blocks
std::vector<BlockIndex> DeepLearningEffectBase::GetBlockIndices(WaveTrack *track, double tStart, double tEnd)
{
   std::vector<BlockIndex> blockIndices;

   const WaveClipHolders &clips = track->GetClips();

   sampleCount start = track->TimeToLongSamples(tStart);
   sampleCount end = track->TimeToLongSamples(tEnd);

   for (const auto &clip : clips)
   {
      sampleCount clipStart = clip->GetPlayStartSample();
      sampleCount clipEnd = clip->GetPlayEndSample();

      // skip if clip is out of bounds
      if (start > clipEnd || end < clipStart)
         continue;

      // trim around the edges
      start > clipStart ? 
         clipStart = start : clipStart;

      end < clipEnd ?
         clipEnd = end : clipEnd;

      sampleCount samplePos = clipStart;

      while (samplePos < clipEnd)
      {
         //Get a blockSize of samples (smaller than the size of the buffer)
         size_t blockSize = limitSampleBufferSize(
            track->GetBestBlockSize(samplePos),
            clipEnd - samplePos);
         
         blockIndices.emplace_back(BlockIndex(samplePos, blockSize));

         samplePos += blockSize;
      }
   }

   return blockIndices;
}

std::vector<ClipTimestamps> DeepLearningEffectBase::GetClipTimestamps(WaveTrack *track, double tStart, double tEnd) const
{
   std::vector<ClipTimestamps> timestamps;

   for (const auto &clip : track->GetClips())
      timestamps.emplace_back(
         std::pair<double, double>(clip->GetPlayStartTime(), 
                                   clip->GetPlayEndTime())
      );

   return timestamps;
}


torch::Tensor DeepLearningEffectBase::BuildMonoTensor(WaveTrack *track, float *buffer,
                                                  sampleCount start, size_t len)
{
   //Get the samples from the track and put them in the buffer
   if (!track->GetFloats(buffer, start, len))
      throw std::runtime_error("An error occurred while copying samples to tensor buffer.");

   // get tensor input from buffer
   torch::Tensor audio = torch::from_blob(buffer, len,
                                          torch::TensorOptions().dtype(torch::kFloat));
   audio = audio.unsqueeze(0); // add channel dimension

   return audio;
}

torch::Tensor DeepLearningEffectBase::BuildMultichannelTensor(WaveTrack *leader, float *buffer,
                                                          sampleCount start, size_t len)
{
   auto channels = TrackList::Channels(leader);
   std::vector<torch::Tensor> channelStack;

   // because we're reusing the same buffer, it's important that
   // we clone the tensor.
   for (WaveTrack *channel : channels)
      channelStack.emplace_back(
          BuildMonoTensor(channel, buffer, start, len).clone());

   return torch::cat(channelStack);
}

struct ForwardPassStatus
{
   torch::Tensor input;
   torch::jit::IValue output;
   std::atomic<bool> done {false};
   std::atomic<bool> success {true};
   DeepModelHolder model;
};

torch::jit::IValue DeepLearningEffectBase::ForwardPassInThread(torch::Tensor input)
{
   auto status = std::make_shared<ForwardPassStatus>();

   status->input = input;
   status->model = this->mActiveModel->GetModel();
   status->done = false;
   status->success = true;

   auto thread = std::thread(
      [status]()
      {
         try
         {
            torch::jit::IValue tempOut = status->model->Forward(status->input);

            // only write to output tensor if abort was not requested
            if (status->success)
               status->output = tempOut;
         }
         catch (const ModelException &e)
         {
            wxLogError(e.what());
            wxLogDebug(e.what());
            status->success = false;
            status->output = torch::jit::IValue(
                              torch::zeros_like(status->input));
         }
         status->done = true;
      }
   );

   // wait for the thread to finish
   while (!status->done)
   {
      if (TrackProgress(mCurrentTrackNum, mCurrentProgress))
      {
         // abort if requested
         status->success = false;

         // tensor output will be destroyed once the thread is destroyed
         thread.detach();

         status->output = torch::jit::IValue(
                           torch::zeros_like(status->input));
         return status->output;
      }

      ::wxSafeYield();
      using namespace std::chrono;
      std::this_thread::sleep_for(50ms);
   }

   if (thread.joinable())
      thread.join();

   if (!status->success)
   {
      Effect::MessageBox(XO("An internal error occurred within the neural network model. "
                        "This model may be broken. Please check the error log for more details"),
                        wxICON_ERROR);
   }

   return status->output;
}

void DeepLearningEffectBase::TensorToTrack(torch::Tensor waveform, WaveTrack::Holder track,
                                       double tStart, double tEnd)
{
   if (waveform.size(0) != 1)
      throw ModelException(XO("Internal error: input waveform is not mono."), "");

   // get the data pointer
   const void *data = waveform.contiguous().data_ptr<float>();
   size_t outputLen = waveform.size(-1);

   // add the data to a temporary track, then
   // paste on our output track
   WaveTrack::Holder tmp = track->EmptyCopy();
   tmp->Append(static_cast<constSamplePtr>(data), floatSample, outputLen);
   tmp->Flush();

   track->ClearAndPaste(tStart, tEnd, tmp.get());
}

// UI stuff
void DeepLearningEffectBase::PopulateOrExchange(ShuttleGui &S)
{
   DeepModelManager &manager = DeepModelManager::Get();

   S.StartVerticalLay(wxCENTER, true);
   {
      mManagerPanel = safenew ModelManagerPanel(S.GetParent(), this,
         mActiveModel, GetDeepEffectID());
      S.AddWindow(mManagerPanel);
      // mManagerPanel->PopulateOrExchange(S);
   }
   S.EndVerticalLay();

   mActiveModel->SetModel(*this);
}
