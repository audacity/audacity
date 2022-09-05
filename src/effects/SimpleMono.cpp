/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleMono.cpp

  Dominic Mazzoni

*******************************************************************//**
\class EffectSimpleMono
\brief An abstract Effect class that simplifies the implementation of a basic
  monaural effect.  Inherit from it if your effect just modifies
  a single track in place and doesn't care how many samples
  it gets at a time.

  Your derived class only needs to implement
  GetSymbol, GetEffectAction, and ProcessSimpleMono.

*//*******************************************************************/



#include "SimpleMono.h"

#include "../WaveTrack.h"

#include <math.h>

bool EffectSimpleMono::Process(EffectContext &context,
   EffectInstance &, EffectSettings &)
{
   //Iterate over each track
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   mCurTrackNum = 0;
   for( auto pOutWaveTrack : mOutputTracks->Selected< WaveTrack >() )
   {
      //Get start and end times from track
      double trackStart = pOutWaveTrack->GetStartTime();
      double trackEnd = pOutWaveTrack->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         auto start = pOutWaveTrack->TimeToLongSamples(mCurT0);
         auto end = pOutWaveTrack->TimeToLongSamples(mCurT1);

         //Get the track rate and samples
         mCurRate = pOutWaveTrack->GetRate();
         mCurChannel = pOutWaveTrack->GetChannel();

         //NewTrackSimpleMono() will returns true by default
         //ProcessOne() processes a single track
         if (!NewTrackSimpleMono() ||
            !ProcessOne(context, pOutWaveTrack, start, end)) {
            bGoodResult = false;
            break;
         }
      }

      mCurTrackNum++;
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}


//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSimpleMono on these blocks
bool EffectSimpleMono::ProcessOne(EffectContext &context,
   WaveTrack * track, sampleCount start, sampleCount end)
{
   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   Floats buffer{ track->GetMaxBlockSize() };

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      const auto block =
         limitSampleBufferSize( track->GetBestBlockSize(s), end - s );

      //Get the samples from the track and put them in the buffer
      track->GetFloats(buffer.get(), s, block);

      //Process the buffer.  If it fails, clean up and exit.
      if (!ProcessSimpleMono(buffer.get(), block))
         //Return false because the effect failed.
         return false;

      //Processing succeeded. copy the newly-changed samples back
      //onto the track.
      track->Set((samplePtr) buffer.get(), floatSample, s, block);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (context.TrackProgress(mCurTrackNum,
                        (s - start).as_double() /
                        len))
         return false;
   }

   //Return true because the effect processing succeeded.
   return true;
}

//null implementation of NewTrackSimpleMono
bool EffectSimpleMono::NewTrackSimpleMono()
{
   return true;
}
