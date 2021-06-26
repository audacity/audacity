/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   EffectDeepLearning.cpp
   Hugo Flores Garcia

******************************************************************/
/**

*/
/*******************************************************************/

#include "EffectDeepLearning.h"

#include <torch/script.h>

EffectDeepLearning::EffectDeepLearning()
{ 
   // create an empty deep model
   mModel = std::make_unique<DeepModel>();
}

bool EffectDeepLearning::Process()
{
   // throw an error if there isn't a model loaded
   if (!mModel->IsLoaded())
   {
      Effect::MessageBox(
         // TODO: i18n-hint:  */
         XO("Please load a model before applying the effect."),
         wxICON_ERROR );
      return false;
   }

   // Iterate over each track.
   // All needed because this effect needs to introduce
   // silence in the sync-lock group tracks to keep sync
   CopyInputTracks(true); // Set up mOutputTracks.
   bool bGoodResult = true;

   mCurrentTrackNum = 0;

   // NOTE: because we will append the separated tracks to mOutputTracks in ProcessOne(), 
   // we need to collect the track pointers before calling ProcessOne()
   std::vector< WaveTrack* > pOutWaveTracks;
   for ( WaveTrack *track : mOutputTracks->SelectedLeaders< WaveTrack >())
      pOutWaveTracks.emplace_back(track);

   // now that we have all the tracks we want to process, 
   // go ahead and process each!
   for ( WaveTrack* pOutWaveTrack : pOutWaveTracks) {

      // if the track is multichannel, bail
      bool isMono = GetNumChannels(pOutWaveTrack) == 1;
      if (!isMono)
      {
         Effect::MessageBox(XO("The Source Separation effect can only process mono tracks. Please select a mono track and try again "));
         bGoodResult = false;
         return bGoodResult;
      }

      //Get start and end times from track
      double tStart = pOutWaveTrack->GetStartTime();
      double tEnd = pOutWaveTrack->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      tStart = wxMax(mT0, tStart);
      tEnd = wxMin(mT1, tEnd);

      // Process only if the right marker is to the right of the left marker
      if (tEnd > tStart) {
         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(pOutWaveTrack, tStart, tEnd))
            bGoodResult = false;
      }
      // increment current track
      mCurrentTrackNum++;
   }

   ReplaceProcessedTracks(bGoodResult);

   return bGoodResult;
}

// gets a list of starting samples and block lengths 
// dictated by the track, so we can process the audio
// audio in blocks
std::vector<BlockIndex> EffectDeepLearning::GetBlockIndices
(WaveTrack *track, double tStart, double tEnd)
{
   std::vector<BlockIndex> blockIndices;

   sampleCount start = track->TimeToLongSamples(tStart);
   sampleCount end = track->TimeToLongSamples(tEnd);

   //Get the length of the selection (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (end - start).as_double();

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
   bool bGoodResult = true;
   sampleCount samplePos = start;
   while (samplePos < end) 
   {
      //Get a blockSize of samples (smaller than the size of the buffer)
      size_t blockSize = limitSampleBufferSize(
         /*bufferSize*/ track->GetBestBlockSize(samplePos),
         /*limit*/ end - samplePos
      );

      blockIndices.emplace_back(BlockIndex(samplePos, blockSize));

      // Increment the sample pointer
      samplePos += blockSize;
   }

   return blockIndices;
}

// TODO: get rid of the Floats entirely and simply pass the data_ptr
// to empty torch contiguous zeros
torch::Tensor EffectDeepLearning::BuildMonoTensor(WaveTrack *track, float *buffer, 
                              sampleCount start, size_t len)
{
   //Get the samples from the track and put them in the buffer
   if (!track->GetFloats(buffer, start, len))
      throw std::exception();

   // get tensor input from buffer
   torch::Tensor audio = torch::from_blob(buffer, len, 
                                          torch::TensorOptions().dtype(torch::kFloat));
   audio = audio.unsqueeze(0); // add channel dimension

   return audio;
}

torch::Tensor EffectDeepLearning::ForwardPass(torch::Tensor input)
{
   torch::Tensor output;
   try
   {
      output = mModel->Forward(input);
   }
   catch (const std::exception &e)
   {
      std::cerr<<e.what();
      Effect::MessageBox(XO("An error occurred during the forward pass"),
         wxOK | wxICON_ERROR
      );

      output = torch::zeros_like(input);
   }
   return output;
}

void EffectDeepLearning::TensorToTrack(torch::Tensor output, WaveTrack::Holder track, 
                                   double tStart, double tEnd)
{
   // TODO: exception: input audio should be shape (1, samples)
   if (!(output.size(0) == 1))
      throw std::exception(); 

   // get the data pointer
   float *data = output.contiguous().data_ptr<float>();
   size_t outputLen = output.size(-1);

   // add the data to a temporary track, then 
   // paste on our output track
   WaveTrack::Holder tmp = track->EmptyCopy();
   tmp->Append((samplePtr)data, floatSample, outputLen);
   tmp->Flush();

   try {
      track->ClearAndPaste(tStart, tEnd, tmp.get());
   }
   catch (const std::exception &e)
   { 
      Effect::MessageBox(XO("Error copying tensor data to output track"),
      wxOK | wxICON_ERROR 
      ); 
   }
}
