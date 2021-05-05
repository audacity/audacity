/**********************************************************************

  Audacity: A Digital Audio Editor

  @file RealtimeEffectState.cpp

  Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#include "RealtimeEffectState.h"

#include "EffectInterface.h"

RealtimeEffectState::RealtimeEffectState( EffectProcessor &effect )
   : mEffect{ effect }
{
}

bool RealtimeEffectState::RealtimeSuspend()
{
   auto result = mEffect.RealtimeSuspend();
   if ( result ) {
      mRealtimeSuspendCount++;
   }
   return result;
}

bool RealtimeEffectState::RealtimeResume()
{
   auto result = mEffect.RealtimeResume();
   if ( result ) {
      mRealtimeSuspendCount--;
   }
   return result;
}

// RealtimeAddProcessor and RealtimeProcess use the same method of
// determining the current processor index, so updates to one should
// be reflected in the other.
bool RealtimeEffectState::RealtimeAddProcessor(int group, unsigned chans, float rate)
{
   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;

   // Reset processor index
   if (group == 0)
   {
      mCurrentProcessor = 0;
      mGroupProcessor.clear();
   }

   // Remember the processor starting index
   mGroupProcessor.push_back(mCurrentProcessor);

   const auto numAudioIn = mEffect.GetAudioInCount();
   const auto numAudioOut = mEffect.GetAudioOutCount();

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accommodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < numAudioIn)
      {
         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fulfill the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= numAudioIn)
      {
         gchans = numAudioIn;
         ichans -= gchans;
      }

      // If we don't have enough output channels to accommodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < numAudioOut)
      {
         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fulfill the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= numAudioOut)
      {
         ochans -= numAudioOut;
      }

      // Add a NEW processor
      mEffect.RealtimeAddProcessor(gchans, rate);

      // Bump to next processor
      mCurrentProcessor++;
   }

   return true;
}

// RealtimeAddProcessor and RealtimeProcess use the same method of
// determining the current processor group, so updates to one should
// be reflected in the other.
size_t RealtimeEffectState::RealtimeProcess(int group,
                                    unsigned chans,
                                    float **inbuf,
                                    float **outbuf,
                                    size_t numSamples)
{
   //
   // The caller passes the number of channels to process and specifies
   // the number of input and output buffers.  There will always be the
   // same number of output buffers as there are input buffers.
   //
   // Effects always require a certain number of input and output buffers,
   // so if the number of channels we're currently processing are different
   // than what the effect expects, then we use a few methods of satisfying
   // the effects requirements.
   const auto numAudioIn = mEffect.GetAudioInCount();
   const auto numAudioOut = mEffect.GetAudioOutCount();

   float **clientIn = (float **) alloca(numAudioIn * sizeof(float *));
   float **clientOut = (float **) alloca(numAudioOut * sizeof(float *));
   float *dummybuf = (float *) alloca(numSamples * sizeof(float));
   decltype(numSamples) len = 0;
   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;
   unsigned indx = 0;
   unsigned ondx = 0;

   int processor = mGroupProcessor[group];

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accommodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < numAudioIn)
      {
         for (size_t i = 0; i < numAudioIn; i++)
         {
            if (indx == ichans)
            {
               indx = 0;
            }
            clientIn[i] = inbuf[indx++];
         }

         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fulfill the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= numAudioIn)
      {
         gchans = 0;
         for (size_t i = 0; i < numAudioIn; i++, ichans--, gchans++)
         {
            clientIn[i] = inbuf[indx++];
         }
      }

      // If we don't have enough output channels to accommodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < numAudioOut)
      {
         for (size_t i = 0; i < numAudioOut; i++)
         {
            if (i < ochans)
            {
               clientOut[i] = outbuf[i];
            }
            else
            {
               clientOut[i] = dummybuf;
            }
         }

         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fulfill the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= numAudioOut)
      {
         for (size_t i = 0; i < numAudioOut; i++, ochans--)
         {
            clientOut[i] = outbuf[ondx++];
         }
      }

      // Finally call the plugin to process the block
      len = 0;
      const auto blockSize = mEffect.GetBlockSize();
      for (decltype(numSamples) block = 0; block < numSamples; block += blockSize)
      {
         auto cnt = std::min(numSamples - block, blockSize);
         len += mEffect.RealtimeProcess(processor, clientIn, clientOut, cnt);

         for (size_t i = 0 ; i < numAudioIn; i++)
         {
            clientIn[i] += cnt;
         }

         for (size_t i = 0 ; i < numAudioOut; i++)
         {
            clientOut[i] += cnt;
         }
      }

      // Bump to next processor
      processor++;
   }

   return len;
}

bool RealtimeEffectState::IsRealtimeActive()
{
   return mRealtimeSuspendCount == 0;
}

