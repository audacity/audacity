/**********************************************************************

Audacity: A Digital Audio Editor

MixAndRender.cpp

Paul Licameli split from Mix.cpp

**********************************************************************/

#include "MixAndRender.h"

#include "BasicUI.h"
#include "Mix.h"
#include "WaveTrack.h"

using WaveTrackConstArray = std::vector < std::shared_ptr < const WaveTrack > >;

//TODO-MB: wouldn't it make more sense to DELETE the time track after 'mix and render'?
void MixAndRender(const TrackIterRange<const WaveTrack> &trackRange,
   const Mixer::WarpOptions &warpOptions,
   const wxString &newTrackName,
   WaveTrackFactory *trackFactory,
   double rate, sampleFormat format,
   double startTime, double endTime,
   WaveTrack::Holder &uLeft, WaveTrack::Holder &uRight)
{
   uLeft.reset(), uRight.reset();
   if (trackRange.empty())
      return;

   // This function was formerly known as "Quick Mix".
   bool mono = false;   /* flag if output can be mono without losing anything*/
   bool oneinput = false;  /* flag set to true if there is only one input track
                              (mono or stereo) */

   auto first = *trackRange.begin();
   assert(first); // because the range is known to be nonempty

   // this only iterates tracks which are relevant to this function, i.e.
   // selected WaveTracks. The tracklist is (confusingly) the list of all
   // tracks in the project

   int numWaves = 0; /* number of wave tracks in the selection */
   int numMono = 0;  /* number of mono, centre-panned wave tracks in selection*/
   for(auto wt : trackRange) {
      numWaves++;
      float pan = wt->GetPan();
      if (wt->GetChannel() == Track::MonoChannel && pan == 0)
         numMono++;
   }

   if (numMono == numWaves)
      mono = true;

   /* the next loop will do two things at once:
    * 1. build an array of all the wave tracks were are trying to process
    * 2. determine when the set of WaveTracks starts and ends, in case we
    *    need to work out for ourselves when to start and stop rendering.
    */

   double mixStartTime = 0.0;    /* start time of first track to start */
   bool gotstart = false;  // flag indicates we have found a start time
   double mixEndTime = 0.0;   /* end time of last track to end */
   double tstart, tend;    // start and end times for one track.

   SampleTrackConstArray waveArray;

   for(auto wt : trackRange) {
      waveArray.push_back( wt->SharedPointer< const SampleTrack >() );
      tstart = wt->GetStartTime();
      tend = wt->GetEndTime();
      if (tend > mixEndTime)
         mixEndTime = tend;
      // try and get the start time. If the track is empty we will get 0,
      // which is ambiguous because it could just mean the track starts at
      // the beginning of the project, as well as empty track. The give-away
      // is that an empty track also ends at zero.

      if (tstart != tend) {
         // we don't get empty tracks here
         if (!gotstart) {
            // no previous start, use this one unconditionally
            mixStartTime = tstart;
            gotstart = true;
         } else if (tstart < mixStartTime)
            mixStartTime = tstart;  // have a start, only make it smaller
      }  // end if start and end are different
   }

   /* create the destination track (NEW track) */
   if (numWaves == (int)TrackList::Channels(first).size())
      oneinput = true;
   // only one input track (either 1 mono or one linked stereo pair)

   // EmptyCopy carries over any interesting channel group information
   // But make sure the left is unlinked before we re-link
   auto mixLeft =
      first->EmptyCopy(trackFactory->GetSampleBlockFactory(), false);
   mixLeft->SetRate(rate);
   mixLeft->ConvertToSampleFormat(format);
   if (oneinput)
      mixLeft->SetName(first->GetName()); /* set name of output track to be the same as the sole input track */
   else
      /* i18n-hint: noun, means a track, made by mixing other tracks */
      mixLeft->SetName(newTrackName);
   mixLeft->SetOffset(mixStartTime);

   // TODO: more-than-two-channels
   decltype(mixLeft) mixRight{};
   if ( !mono ) {
      mixRight = trackFactory->Create(format, rate);
      if (oneinput) {
         auto channels = TrackList::Channels(first);
         if (channels.size() > 1)
            mixRight->SetName((*channels.begin().advance(1))->GetName()); /* set name to match input track's right channel!*/
         else
            mixRight->SetName(first->GetName());   /* set name to that of sole input channel */
      }
      else
         mixRight->SetName(newTrackName);
      mixRight->SetOffset(mixStartTime);
   }


   auto maxBlockLen = mixLeft->GetIdealBlockSize();

   // If the caller didn't specify a time range, use the whole range in which
   // any input track had clips in it.
   if (startTime == endTime) {
      startTime = mixStartTime;
      endTime = mixEndTime;
   }

   Mixer mixer(waveArray,
      // Throw to abort mix-and-render if read fails:
      true, warpOptions,
      startTime, endTime, mono ? 1 : 2, maxBlockLen, false,
      rate, format);

   using namespace BasicUI;
   auto updateResult = ProgressResult::Success;
   {
      auto pProgress = MakeProgress(XO("Mix and Render"),
         XO("Mixing and rendering tracks"));

      while (updateResult == ProgressResult::Success) {
         auto blockLen = mixer.Process(maxBlockLen);

         if (blockLen == 0)
            break;

         if (mono) {
            auto buffer = mixer.GetBuffer();
            mixLeft->Append(buffer, format, blockLen);
         }
         else {
            auto buffer = mixer.GetBuffer(0);
            mixLeft->Append(buffer, format, blockLen);
            buffer = mixer.GetBuffer(1);
            mixRight->Append(buffer, format, blockLen);
         }

         updateResult = pProgress->Poll(
            mixer.MixGetCurrentTime() - startTime, endTime - startTime);
      }
   }

   mixLeft->Flush();
   if (!mono)
      mixRight->Flush();
   if (updateResult == ProgressResult::Cancelled || updateResult == ProgressResult::Failed)
   {
      return;
   }
   else {
      uLeft = mixLeft, uRight = mixRight;
#if 0
   int elapsedMS = wxGetElapsedTime();
   double elapsedTime = elapsedMS * 0.001;
   double maxTracks = totalTime / (elapsedTime / numWaves);

   // Note: these shouldn't be translated - they're for debugging
   // and profiling only.
   wxPrintf("      Tracks: %d\n", numWaves);
   wxPrintf("  Mix length: %f sec\n", totalTime);
   wxPrintf("Elapsed time: %f sec\n", elapsedTime);
   wxPrintf("Max number of tracks to mix in real time: %f\n", maxTracks);
#endif
   }
}
