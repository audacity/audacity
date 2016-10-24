/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.cpp

  Dominic Mazzoni

*******************************************************************//**
\class EffectTwoPassSimpleMono
\brief An Effect base class that implements a two pass process by using
EffectSimpleMono.

Inherit from it if your effect needs to pass twice over the data.
It does the first pass on all selected tracks before going back and
doing the second pass over all selected tracks.

*//*******************************************************************/


#include "../Audacity.h"

#include "TwoPassSimpleMono.h"

#include "../WaveTrack.h"

bool EffectTwoPassSimpleMono::Process()
{
    mPass = 0;
    mSecondPassDisabled = false;

    InitPass1();
    this->CopyInputTracks(); // Set up mOutputTracks.
    bool bGoodResult = ProcessPass();

    if (bGoodResult && !mSecondPassDisabled)
    {
        mPass = 1;
        if (InitPass2())
            bGoodResult = ProcessPass();
    }

    this->ReplaceProcessedTracks(bGoodResult);
    return bGoodResult;
}

bool EffectTwoPassSimpleMono::ProcessPass()
{
   //Iterate over each track
   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         auto start = track->TimeToLongSamples(mCurT0);
         auto end = track->TimeToLongSamples(mCurT1);

         //Get the track rate and samples
         mCurRate = track->GetRate();
         mCurChannel = track->GetChannel();

         //NewTrackPass1/2() returns true by default
         bool ret;
         if (mPass == 0)
            ret = NewTrackPass1();
         else
            ret = NewTrackPass2();
         if (!ret)
            return false;

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(track, start, end))
            return false;
      }

      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

   return true;
}


//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes TwoBufferProcessPass1 or TwoBufferProcessPass2 on these blocks
bool EffectTwoPassSimpleMono::ProcessOne(WaveTrack * track,
                                         sampleCount start, sampleCount end)
{
   bool ret;
   float *tmpfloat;

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();
   auto maxblock = track->GetMaxBlockSize();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer1 = new float[maxblock];
   float *buffer2 = new float[maxblock];
   auto samples1 =  limitSampleBufferSize(
      std::min( maxblock, track->GetBestBlockSize(start) ), end - start );

   //Get the samples from the track and put them in the buffer
   track->Get((samplePtr) buffer1, floatSample, start, samples1);

   // Process the first buffer with a NULL previous buffer
   if (mPass == 0)
      ret = TwoBufferProcessPass1(NULL, 0, buffer1, samples1);
   else
      ret = TwoBufferProcessPass2(NULL, 0, buffer1, samples1);
   if (!ret) {
      delete[]buffer1;
      delete[]buffer2;

      //Return false because the effect failed.
      return false;
   }

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start + samples1;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      auto samples2 = limitSampleBufferSize(
         std::min( track->GetBestBlockSize(s), maxblock ), end - s
      );

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) buffer2, floatSample, s, samples2);

      //Process the buffer.  If it fails, clean up and exit.
      if (mPass == 0)
         ret = TwoBufferProcessPass1(buffer1, samples1, buffer2, samples2);
      else
         ret = TwoBufferProcessPass2(buffer1, samples1, buffer2, samples2);
      if (!ret) {
         delete[]buffer1;
         delete[]buffer2;
         //Return false because the effect failed.
         return false;
      }

      //Processing succeeded. copy the newly-changed samples back
      //onto the track.
      track->Set((samplePtr) buffer1, floatSample, s-samples1, samples1);

      //Increment s one blockfull of samples
      s += samples2;

      //Update the Progress meter
      if (mSecondPassDisabled)
         ret = TotalProgress(
            (mCurTrackNum + (s-start).as_double()/len) /
            GetNumWaveTracks());
      else
         ret = TotalProgress(
            (mCurTrackNum + (s-start).as_double()/len + GetNumWaveTracks()*mPass) /
            (GetNumWaveTracks()*2));
      if (ret) {
         delete[]buffer1;
         delete[]buffer2;
         //Return false because the effect failed.
         return false;
      }

      // Rotate the buffers
      tmpfloat = buffer1;
      buffer1 = buffer2;
      buffer2 = tmpfloat;

      std::swap(samples1, samples2);
   }

   // Send the last buffer with a NULL pointer for the current buffer
   if (mPass == 0)
      ret = TwoBufferProcessPass1(buffer1, samples1, NULL, 0);
   else
      ret = TwoBufferProcessPass2(buffer1, samples1, NULL, 0);

   if (!ret) {
      delete[]buffer1;
      delete[]buffer2;
      //Return false because the effect failed.
      return false;
   }

   //Processing succeeded. copy the newly-changed samples back
   //onto the track.
   track->Set((samplePtr) buffer1, floatSample, s-samples1, samples1);

   //Clean up the buffer
   delete[]buffer1;
   delete[]buffer2;

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectTwoPassSimpleMono::NewTrackPass1()
{
   return true;
}

bool EffectTwoPassSimpleMono::NewTrackPass2()
{
   return true;
}

//Initialisations before the first pass
bool EffectTwoPassSimpleMono::InitPass1()
{
   return true;
}

//Initialisations before the second pass.
//Return true if you actually want the second pass to go ahead
bool EffectTwoPassSimpleMono::InitPass2()
{
   return true;
}

